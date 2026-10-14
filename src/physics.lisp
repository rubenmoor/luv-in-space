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

(defconstant +moon-distance-min+ 3.5d8
  "m, below the Moon's least distance from Earth (perigee ≥ 356,000 km).")
(defconstant +sun-distance-min+ 1.4d11
  "m, below the Sun's least distance from Earth (perihelion 1.471×10¹¹ m).")

(declaim (inline coarse-step))

(defun coarse-step (r time thrusting &optional (central :earth))
  "h = η min over the frame's bodies of √(d³/μ), down to a power of two,
clamped to [*coarse-step-min*, *coarse-step-max*], ≤ *thrust-step-max*
when THRUSTING."
  (declare (optimize speed) (type vec3 r) (type double-float time))
  (let ((tau 0d0))
    (declare (type double-float tau))
    (flet ((dynamical-time (d2 mu)
             (declare (type (double-float 0d0) d2) (type double-float mu))
             (sqrt (the (double-float 0d0) (/ (* d2 (sqrt d2)) mu)))))
      (ecase central
        (:earth
         (let* ((d (make-vec3)) (dv (make-vec3))
                (r2 (vec3-dot r r))
                (rn (sqrt (the (double-float 0d0) r2))))
           (declare (dynamic-extent d dv))
           (setf tau (dynamical-time r2 +mu-earth+))
           ;; A body is read only if its least possible distance could
           ;; lower tau; skipping it leaves the result unchanged.
           (flet ((third-body (key distance-min mu)
                    (let ((low (- distance-min rn)))
                      (unless (and (> low 0d0) (>= (dynamical-time (* low low) mu) tau))
                        (body-state-into d dv key time :earth)
                        (vec3-sub! d d r)
                        (setf tau (min tau (dynamical-time (vec3-dot d d) mu)))))))
             (declare (inline third-body))
             (third-body :moon +moon-distance-min+ +mu-moon+)
             (third-body :sun +sun-distance-min+ +mu-sun+))))))
    ;; Halving from the upper clamp, a power of two, rounds down exactly.
    (let ((h (* *coarse-eta* tau))
          (p *coarse-step-max*))
      (declare (type double-float h p))
      (loop while (and (> p h) (> p *coarse-step-min*))
            do (setf p (* 0.5d0 p)))
      (if thrusting (min p *thrust-step-max*) p))))

;;; The RK4 step (docs/architecture.md "Integrator"; decision 69). The state
;;; vector y: r 0–2, v 3–5, q 6–9, ω 10–12, m_p 13.

(deftype state-vector () '(simple-array double-float (14)))

(declaim (inline state-vector-axpy!))

(defun state-vector-axpy! (out y k s)
  "out = y + s k."
  (declare (type state-vector out y k) (type double-float s))
  (dotimes (i 14 out)
    (setf (aref out i) (+ (aref y i) (* s (aref k i))))))

;; Hot callers declare it inline: a full call boxes the double-float time.
(declaim (sb-ext:maybe-inline rk4-step!))

(defun rk4-step! (state h time model force-body torque-body mdot m-dry
                  inertia inertia-inverse &optional attitude-fn)
  "Advance STATE by h from TIME: classical RK4 on r, v, q, ω and m_p, the
body force (N), torque (N m) and mass flow (kg/s) held over the step, the
mass m-dry + m_p, the inertia tensor about the CoM and its inverse (body
axes) held over the step; conses nothing. When ṁ h > m_p the inputs are
scaled so the tanks empty exactly at the step's end. With ATTITUDE-FN,
called as (attitude-fn q-out w-out time-cell r v) to write the attitude and
the frame's rate (inertial axes) at the time in time-cell (a one-element
double array, so nothing is boxed), attitude is kinematic: q comes from it
at each stage, torque is ignored, q and ω are set from it at t + h."
  (declare (optimize speed) (type ship-state state) (type double-float h time mdot m-dry)
           (type vec3 force-body torque-body) (type mat3 inertia inertia-inverse)
           (type keyword model) (type (or null function) attitude-fn))
  (let* ((m-p0 (ship-state-m-p state))
         (scale (cond ((<= m-p0 0d0) 0d0)
                      ((> (* mdot h) m-p0) (/ m-p0 (* mdot h)))
                      (t 1d0)))
         (exhausted (< scale 1d0))
         (mdot (* scale mdot))
         (y (make-array 14 :element-type 'double-float))
         (ys (make-array 14 :element-type 'double-float))
         (k1 (make-array 14 :element-type 'double-float))
         (k2 (make-array 14 :element-type 'double-float))
         (k3 (make-array 14 :element-type 'double-float))
         (k4 (make-array 14 :element-type 'double-float))
         (f (make-vec3)) (tau (make-vec3))
         (r (make-vec3)) (v (make-vec3)) (w (make-vec3)) (a (make-vec3))
         (iw (make-vec3)) (tmp (make-vec3)) (q (make-quat)) (dq (make-quat))
         (time-cell (make-array 1 :element-type 'double-float)))
    (declare (dynamic-extent y ys k1 k2 k3 k4 f tau r v w a iw tmp q dq time-cell)
             (type double-float scale mdot))
    (vec3-scale! f force-body scale)
    (vec3-scale! tau torque-body scale)
    (replace y (ship-state-r state))
    (replace y (ship-state-v state) :start1 3)
    (replace y (ship-state-q state) :start1 6)
    (replace y (ship-state-w state) :start1 10)
    (setf (aref y 13) m-p0)
    (flet ((derivative! (k y time)
             (declare (type state-vector k y) (type double-float time))
             (vec3-set! r (aref y 0) (aref y 1) (aref y 2))
             (vec3-set! v (aref y 3) (aref y 4) (aref y 5))
             (gravity-into! a r time model)
             (if attitude-fn
                 (progn (setf (aref time-cell 0) time)
                        (funcall attitude-fn q tmp time-cell r v))
                 (quat-set! q (aref y 6) (aref y 7) (aref y 8) (aref y 9)))
             ;; v̇ = gravity + q (F / m) q*
             (quat-rotate! tmp q f)
             (vec3-add-scaled! a a tmp (/ (+ m-dry (aref y 13))))
             (setf (aref k 0) (aref y 3) (aref k 1) (aref y 4) (aref k 2) (aref y 5)
                   (aref k 3) (aref a 0) (aref k 4) (aref a 1) (aref k 5) (aref a 2)
                   (aref k 13) (- mdot))
             (if attitude-fn
                 (fill k 0d0 :start 6 :end 13)
                 (progn
                   (vec3-set! w (aref y 10) (aref y 11) (aref y 12))
                   (quat-derivative! dq q w)
                   (replace k dq :start1 6)
                   ;; I ω̇ = τ − ω × I ω
                   (mat3*vec3! iw inertia w)
                   (vec3-cross! tmp w iw)
                   (vec3-sub! tmp tau tmp)
                   (mat3*vec3! tmp inertia-inverse tmp)
                   (replace k tmp :start1 10)))
             k))
      (declare (inline derivative!))
      (let ((h/2 (* 0.5d0 h)))
        (derivative! k1 y time)
        (derivative! k2 (state-vector-axpy! ys y k1 h/2) (+ time h/2))
        (derivative! k3 (state-vector-axpy! ys y k2 h/2) (+ time h/2))
        (derivative! k4 (state-vector-axpy! ys y k3 h) (+ time h))
        (dotimes (i 14)
          (setf (aref y i)
                (+ (aref y i)
                   (* (/ h 6) (+ (aref k1 i) (* 2 (aref k2 i)) (* 2 (aref k3 i))
                                 (aref k4 i))))))))
    (let* ((m-p1 (if exhausted 0d0 (max 0d0 (aref y 13))))
           (m0 (+ m-dry m-p0))
           (m1 (+ m-dry m-p1))
           (fn (vec3-norm f)))
      (vec3-set! (ship-state-r state) (aref y 0) (aref y 1) (aref y 2))
      (vec3-set! (ship-state-v state) (aref y 3) (aref y 4) (aref y 5))
      (if attitude-fn
          (let ((q (ship-state-q state)) (w (ship-state-w state)))
            ;; ω in body axes: q* ω_t q
            (setf (aref time-cell 0) (+ time h))
            (funcall attitude-fn q tmp time-cell (ship-state-r state) (ship-state-v state))
            (quat-conjugate! dq q)
            (quat-rotate! w dq tmp))
          (progn
            (quat-set! (ship-state-q state) (aref y 6) (aref y 7) (aref y 8) (aref y 9))
            (quat-normalize! (ship-state-q state) (ship-state-q state))
            (vec3-set! (ship-state-w state) (aref y 10) (aref y 11) (aref y 12))))
      (setf (ship-state-m-p state) m-p1)
      (incf (ship-state-dv state)
            (if (> mdot 0d0)
                (* (/ fn mdot) (log (the (double-float (0d0)) (/ m0 m1))))
                (/ (* fn h) m0)))
      state)))
