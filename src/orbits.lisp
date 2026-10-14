(in-package #:luv-in-space)

;;; Osculating conics: state <-> elements, Kepler propagation, apsides,
;;; radius crossings and J2's secular rates (docs/architecture.md "Rails",
;;; decision 68: Orbit elements: one struct for every conic). Ellipses only
;;; so far; hyperbolas are PLACEHOLDER(97).

(defstruct (orbit (:constructor make-orbit (mu q e i raan argp tp)))
  (mu 0d0 :type double-float :read-only t)
  (q 0d0 :type double-float :read-only t)     ; periapsis radius
  (e 0d0 :type double-float :read-only t)
  (i 0d0 :type double-float :read-only t)
  (raan 0d0 :type double-float :read-only t)  ; Ω
  (argp 0d0 :type double-float :read-only t)  ; ω
  (tp 0d0 :type double-float :read-only t))   ; a periapsis passage

(defparameter *circular-e* 1d-11 "Below this e, ω = 0 (decision 68).")
(defparameter *equatorial-i* 1d-11 "Within this of 0 or π, Ω = 0 (decision 68).")

(defconstant +two-pi+ (* 2 pi))

(defun wrap-angle (x)
  "x in [0, 2π)."
  (declare (type double-float x))
  (let ((y (mod x +two-pi+)))
    (if (>= y +two-pi+) 0d0 y)))

(defun wrap-signed (x)
  "x in (−π, π]."
  (declare (type double-float x))
  (- x (* +two-pi+ (fround x +two-pi+))))

(defun elliptic-p (orbit) (< (orbit-e orbit) 1d0))

(defun require-elliptic (orbit)
  (unless (elliptic-p orbit)
    (error "Hyperbolic orbits are PLACEHOLDER(97): e = ~F." (orbit-e orbit))))

;;; Derived quantities

(defun orbit-a (orbit)
  "Semi-major axis; negative for a hyperbola."
  (/ (orbit-q orbit) (- 1 (orbit-e orbit))))

(defun orbit-semi-latus (orbit)
  "Semi-latus rectum."
  (* (orbit-q orbit) (+ 1 (orbit-e orbit))))

(defun orbit-n (orbit)
  "Mean motion, rad/s."
  (sqrt (/ (orbit-mu orbit) (expt (abs (orbit-a orbit)) 3))))

(defun orbit-period (orbit)
  (and (elliptic-p orbit) (/ +two-pi+ (orbit-n orbit))))

(defun orbit-periapsis-radius (orbit) (orbit-q orbit))

(defun orbit-apoapsis-radius (orbit)
  (and (elliptic-p orbit) (* (orbit-a orbit) (+ 1 (orbit-e orbit)))))

(defun orbit-energy (orbit)
  "Specific orbital energy −μ (1 − e) / (2q)."
  (/ (* (- (orbit-mu orbit)) (- 1 (orbit-e orbit))) (* 2 (orbit-q orbit))))

;;; Kepler's equation

(defun solve-kepler-elliptic (mean-anomaly e)
  "E with E − e sin E = M, M in (−π, π], by Newton to 1e-15 relative."
  (declare (type double-float mean-anomaly e))
  (let ((ea (if (> e 0.8d0)
                (if (minusp mean-anomaly) (- pi) pi)
                (+ mean-anomaly (* e (sin mean-anomaly))))))
    (declare (type double-float ea))
    (loop repeat 60
          for step of-type double-float
            = (/ (- ea (* e (sin ea)) mean-anomaly) (- 1 (* e (cos ea))))
          for next of-type double-float = (- ea step)
          do (when (or (= next ea) (<= (abs step) (* 1d-15 (abs next))))
               (setf ea next)
               (loop-finish))
             (setf ea next))
    ea))

(defun true->eccentric (theta e)
  (declare (type double-float theta e))
  (* 2 (atan (* (sqrt (- 1 e)) (sin (/ theta 2))) (* (sqrt (+ 1 e)) (cos (/ theta 2))))))

(defun eccentric->true (ea e)
  (declare (type double-float ea e))
  (* 2 (atan (* (sqrt (+ 1 e)) (sin (/ ea 2))) (* (sqrt (- 1 e)) (cos (/ ea 2))))))

(defun orbit-mean-anomaly (orbit time)
  "M at time, in (−π, π] for an ellipse."
  (require-elliptic orbit)
  (let* ((period (orbit-period orbit))
         (dt (- time (orbit-tp orbit))))
    (* (orbit-n orbit) (- dt (* period (fround dt period))))))

(defun orbit-true-anomaly (orbit time)
  (let ((e (orbit-e orbit)))
    (eccentric->true (solve-kepler-elliptic (orbit-mean-anomaly orbit time) e) e)))

;;; State <-> orbit

(defun perifocal-axes (i raan argp)
  "The unit vectors P (to periapsis) and Q (90° ahead of it) in ICRF."
  (let ((co (cos raan)) (so (sin raan))
        (cw (cos argp)) (sw (sin argp))
        (ci (cos i)) (si (sin i)))
    (values (vec3 (- (* co cw) (* so sw ci)) (+ (* so cw) (* co sw ci)) (* sw si))
            (vec3 (- (- (* co sw)) (* so cw ci)) (- (* co cw ci) (* so sw)) (* cw si)))))

(defun state->orbit (r v mu time)
  "The osculating orbit of the state (r, v) at time about a body of μ."
  (declare (type vec3 r v) (type double-float mu time))
  (let* ((h (vec3-cross r v))
         (hn (vec3-norm h))
         (rn (vec3-norm r))
         (evec (vec3-scale (vec3-sub (vec3-cross v h) (vec3-scale r (/ mu rn))) (/ mu)))
         (e (vec3-norm evec))
         (p (/ (* hn hn) mu))
         (q (/ p (+ 1 e)))
         (i (atan (sqrt (+ (expt (aref h 0) 2) (expt (aref h 1) 2))) (aref h 2)))
         (raan (if (or (< i *equatorial-i*) (> i (- pi *equatorial-i*)))
                   0d0
                   (wrap-angle (atan (aref h 0) (- (aref h 1))))))
         (node (vec3 (cos raan) (sin raan) 0))
         (normal (vec3-scale h (/ hn)))
         (ahead (vec3-cross normal node))
         (argp (if (< e *circular-e*)
                   0d0
                   (wrap-angle (atan (vec3-dot evec ahead) (vec3-dot evec node)))))
         (theta (- (atan (vec3-dot r ahead) (vec3-dot r node)) argp)))
    (when (>= e 1d0)
      (error "Hyperbolic orbits are PLACEHOLDER(97): e = ~F." e))
    (let* ((ea (true->eccentric (wrap-signed theta) e))
           (m (- ea (* e (sin ea))))
           (a (/ q (- 1 e)))
           (n (sqrt (/ mu (* a a a)))))
      (make-orbit mu q e i raan argp (- time (/ m n))))))

(defun make-elliptic-orbit (mu a e i raan argp mean-anomaly time)
  "The orbit with these classical elements and mean anomaly M at time."
  (let ((n (sqrt (/ mu (* a a a)))))
    (make-orbit mu (* a (- 1 e)) e i (wrap-angle raan) (wrap-angle argp)
                (- time (/ (wrap-signed mean-anomaly) n)))))

(defun orbit-state (orbit time)
  "(values r v) on the orbit at time, ICRF about its body."
  (require-elliptic orbit)
  (let* ((e (orbit-e orbit))
         (a (orbit-a orbit))
         (ea (solve-kepler-elliptic (orbit-mean-anomaly orbit time) e))
         (ce (cos ea)) (se (sin ea))
         (b (sqrt (- 1 (* e e))))
         (rn (* a (- 1 (* e ce))))
         (k (/ (sqrt (* (orbit-mu orbit) a)) rn)))
    (multiple-value-bind (pv qv)
        (perifocal-axes (orbit-i orbit) (orbit-raan orbit) (orbit-argp orbit))
      (let ((x (* a (- ce e))) (y (* a b se))
            (vx (* k (- se))) (vy (* k b ce)))
        (values (vec3-add (vec3-scale pv x) (vec3-scale qv y))
                (vec3-add (vec3-scale pv vx) (vec3-scale qv vy)))))))

;;; Events

(defun orbit-next-apsis (orbit time kind)
  "The first passage of :periapsis or :apoapsis strictly after time; nil for
a hyperbola's apoapsis or a periapsis already passed."
  (if (elliptic-p orbit)
      (let* ((period (orbit-period orbit))
             (t0 (ecase kind
                   (:periapsis (orbit-tp orbit))
                   (:apoapsis (+ (orbit-tp orbit) (/ period 2))))))
        (+ t0 (* period (1+ (ffloor (- time t0) period)))))
      (and (eq kind :periapsis) (> (orbit-tp orbit) time) (orbit-tp orbit))))

(defun orbit-radius-crossings (orbit radius time)
  "(values inbound outbound): the next times after time at which the orbit
crosses radius, falling and rising; nil outside [q, Q] (\"Removal of
objects\")."
  (require-elliptic orbit)
  (let ((e (orbit-e orbit)))
    (when (and (>= e *circular-e*)
               (<= (orbit-q orbit) radius (orbit-apoapsis-radius orbit)))
      (let* ((c (max -1d0 (min 1d0 (/ (- (/ (orbit-semi-latus orbit) radius) 1) e))))
             (theta (acos c))
             (ea (true->eccentric theta e))
             (dt (/ (- ea (* e (sin ea))) (orbit-n orbit)))
             (period (orbit-period orbit)))
        (flet ((next (t0) (+ t0 (* period (1+ (ffloor (- time t0) period))))))
          (values (next (- (orbit-tp orbit) dt))
                  (next (+ (orbit-tp orbit) dt))))))))

(defun orbit-j2-rates (orbit body)
  "(values Ω̇ ω̇ Ṁ−n) in rad/s, J2's secular rates."
  (let* ((body (body body))
         (n (orbit-n orbit))
         (e (orbit-e orbit))
         (ci (cos (orbit-i orbit)))
         (k (* n (body-j2 body) (expt (/ (body-radius body) (orbit-semi-latus orbit)) 2))))
    (values (* -3/2 k ci)
            (* 3/4 k (- (* 5 ci ci) 1))
            (* 3/4 k (sqrt (- 1 (* e e))) (- (* 3 ci ci) 1)))))
