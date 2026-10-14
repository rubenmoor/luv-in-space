(in-package #:luv-in-space)

;;; The ship's state, the gravity models and the coarse step rule
;;; (docs/architecture.md "Integrator", "Gravity model"; decision 69: Ship
;;; state, step rule and kinematic attitude in physics.lisp, decision 17:
;;; Gravity model). Earth's frame only in milestone 2.

(defstruct (ship-state (:constructor %make-ship-state (r v q w m-p dv))
                       (:copier nil))
  (r (make-vec3) :type vec3)          ; m, Earth-centred ICRF
  (v (make-vec3) :type vec3)          ; m/s
  (q (quat 1 0 0 0) :type quat)       ; body -> inertial
  (w (make-vec3) :type vec3)          ; rad/s, body axes
  (m-p 0d0 :type double-float)        ; propellant, kg
  (dv 0d0 :type double-float))        ; odometer, m/s

(defun make-ship-state (r v q w m-p)
  "A ship state from copies of r, v, q, w and m-p; the odometer at 0."
  (%make-ship-state (vec3-copy r) (vec3-copy v) (quat-copy q) (vec3-copy w)
                    (float m-p 1d0) 0d0))

(defun copy-ship-state (state)
  "A deep copy: equalp to STATE, sharing no array."
  (%make-ship-state (vec3-copy (ship-state-r state)) (vec3-copy (ship-state-v state))
                    (quat-copy (ship-state-q state)) (vec3-copy (ship-state-w state))
                    (ship-state-m-p state) (ship-state-dv state)))

;;; Gravity

(declaim (inline third-body-add! gravity-into!))

(defun third-body-add! (a r d mu)
  "a += μ ((d − r)/|d − r|³ − d/|d|³): a third body at d, direct minus
indirect term."
  (declare (type vec3 a r d) (type double-float mu))
  (with-vec3 (((rx ry rz) r) ((dx dy dz) d))
    (let* ((sx (- dx rx)) (sy (- dy ry)) (sz (- dz rz))
           (s2 (+ (* sx sx) (* sy sy) (* sz sz)))
           (d2 (+ (* dx dx) (* dy dy) (* dz dz)))
           (ks (/ mu (* s2 (sqrt (the (double-float 0d0) s2)))))
           (kd (/ mu (* d2 (sqrt (the (double-float 0d0) d2))))))
      (vec3-set! a
                 (+ (aref a 0) (- (* ks sx) (* kd dx)))
                 (+ (aref a 1) (- (* ks sy) (* kd dy)))
                 (+ (aref a 2) (- (* ks sz) (* kd dz)))))))

(defun gravity-into! (a r time model &optional (central :earth))
  "Write the gravitational acceleration at r (m, CENTRAL's frame) at TIME
into a. MODEL: :none, :point (μ⊕), :j2 (with J2, pole along ICRF z),
:full (:j2 plus the Sun and Moon as third bodies)."
  (declare (type vec3 a r) (type double-float time) (type keyword model central))
  (ecase central
    (:earth
     (if (eq model :none)
         (vec3-set! a 0d0 0d0 0d0)
         (with-vec3 (((x y z) r))
           (let* ((r2 (+ (* x x) (* y y) (* z z)))
                  (k (/ +mu-earth+ (* r2 (sqrt (the (double-float 0d0) r2))))))
             (if (eq model :point)
                 (vec3-set! a (- (* k x)) (- (* k y)) (- (* k z)))
                 ;; a = −μ r/r³ − 1.5 J2 μ R²/r⁵ ((1 − 5 z²/r²) x, … y, (3 − 5 z²/r²) z)
                 (let* ((c (/ (* 1.5d0 +j2-earth+ +radius-earth+ +radius-earth+) r2))
                        (z2 (* 5 (/ (* z z) r2)))
                        (kxy (* k (+ 1 (* c (- 1 z2)))))
                        (kz (* k (+ 1 (* c (- 3 z2))))))
                   (vec3-set! a (- (* kxy x)) (- (* kxy y)) (- (* kz z)))
                   (ecase model
                     (:j2)
                     (:full
                      (let ((d (make-vec3)) (dv (make-vec3)))
                        (declare (dynamic-extent d dv))
                        (body-state-into d dv :moon time :earth)
                        (third-body-add! a r d +mu-moon+)
                        (body-state-into d dv :sun time :earth)
                        (third-body-add! a r d +mu-sun+))))
                   a))))))))

;;; The coarse step rule (docs/architecture.md "Integrator"); the fine step
;;; is +fine-step+.

(defparameter *coarse-eta* (/ 1d0 256) "η, docs/architecture.md \"Integrator\".")
(defparameter *coarse-step-min* (/ 1d0 64) "s, docs/architecture.md \"Integrator\".")
(defparameter *coarse-step-max* 16384d0 "s, docs/architecture.md \"Integrator\".")
(defparameter *thrust-step-max* 1d0 "s, under thrust; docs/architecture.md \"Integrator\".")
(declaim (type double-float *coarse-eta* *coarse-step-min* *coarse-step-max*
               *thrust-step-max*))

(declaim (inline coarse-step))

(defun coarse-step (r time thrusting &optional (central :earth))
  "h = η min over the frame's bodies of √(d³/μ), down to a power of two,
clamped to [*coarse-step-min*, *coarse-step-max*], ≤ *thrust-step-max*
when THRUSTING."
  (declare (type vec3 r) (type double-float time))
  (let ((tau 0d0))
    (declare (type double-float tau))
    (flet ((dynamical-time (d2 mu)
             (declare (type (double-float 0d0) d2) (type double-float mu))
             (sqrt (the (double-float 0d0) (/ (* d2 (sqrt d2)) mu)))))
      (ecase central
        (:earth
         (let ((d (make-vec3)) (dv (make-vec3)))
           (declare (dynamic-extent d dv))
           (setf tau (dynamical-time (vec3-dot r r) +mu-earth+))
           (body-state-into d dv :moon time :earth)
           (vec3-sub! d d r)
           (setf tau (min tau (dynamical-time (vec3-dot d d) +mu-moon+)))
           (body-state-into d dv :sun time :earth)
           (vec3-sub! d d r)
           (setf tau (min tau (dynamical-time (vec3-dot d d) +mu-sun+)))))))
    ;; Halving from the upper clamp, a power of two, rounds down exactly.
    (let ((h (* *coarse-eta* tau))
          (p *coarse-step-max*))
      (declare (type double-float h p))
      (loop while (and (> p h) (> p *coarse-step-min*))
            do (setf p (* 0.5d0 p)))
      (if thrusting (min p *thrust-step-max*) p))))
