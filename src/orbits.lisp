(in-package #:luv-in-space)

;;; Osculating conics: state <-> elements, Kepler propagation, apsides,
;;; radius crossings and J2's secular rates (docs/architecture.md "Rails",
;;; decision 68: Orbit elements: one struct for every conic), ellipses and
;;; hyperbolas ("Hyperbolic rails"); near-parabolic forms are milestone 4's.

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
(defparameter *parabolic-e* 1d-6
  "|e − 1| below this shows as e = 1 ± this, for display only (decision 46).")

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

(defun solve-kepler-hyperbolic (mean-anomaly e)
  "H with e sinh H − H = M, by Newton from H₀ = sign(M) ln(2|M|/e + 1.8) to
1e-14 relative (\"Hyperbolic rails\")."
  (declare (type double-float mean-anomaly e))
  (let ((h (* (signum mean-anomaly) (log (+ (/ (* 2 (abs mean-anomaly)) e) 1.8d0)))))
    (declare (type double-float h))
    (loop repeat 100
          for step of-type double-float
            = (/ (- (* e (sinh h)) h mean-anomaly) (- (* e (cosh h)) 1))
          for next of-type double-float = (- h step)
          do (when (or (= next h) (<= (abs step) (* 1d-14 (abs next))))
               (setf h next)
               (loop-finish))
             (setf h next))
    h))

(defun true->hyperbolic (theta e)
  (declare (type double-float theta e))
  (asinh (/ (* (sin theta) (sqrt (- (* e e) 1))) (+ 1 (* e (cos theta))))))

(defun hyperbolic->true (h e)
  (declare (type double-float h e))
  (* 2 (atan (* (sqrt (/ (+ e 1) (- e 1))) (tanh (/ h 2))))))

(defun orbit-mean-anomaly (orbit time)
  "M at time, in (−π, π] for an ellipse."
  (let ((dt (- time (orbit-tp orbit))))
    (if (elliptic-p orbit)
        (let ((period (orbit-period orbit)))
          (* (orbit-n orbit) (- dt (* period (fround dt period)))))
        (* (orbit-n orbit) dt))))

(defun orbit-anomaly (orbit time)
  "E for an ellipse, H for a hyperbola, at time."
  (let ((m (orbit-mean-anomaly orbit time)) (e (orbit-e orbit)))
    (if (elliptic-p orbit)
        (solve-kepler-elliptic m e)
        (solve-kepler-hyperbolic m e))))

(defun orbit-true-anomaly (orbit time)
  (let ((e (orbit-e orbit)) (x (orbit-anomaly orbit time)))
    (if (elliptic-p orbit) (eccentric->true x e) (hyperbolic->true x e))))

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
         (e (let ((e (vec3-norm evec)))
              (if (< (abs (- e 1)) *parabolic-e*)
                  (if (< e 1) (- 1 *parabolic-e*) (+ 1 *parabolic-e*))
                  e)))
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
    (let* ((theta (wrap-signed theta))
           (m (if (< e 1)
                  (let ((ea (true->eccentric theta e))) (- ea (* e (sin ea))))
                  (let ((h (true->hyperbolic theta e))) (- (* e (sinh h)) h))))
           (a (abs (/ q (- 1 e))))
           (n (sqrt (/ mu (* a a a)))))
      (make-orbit mu q e i raan argp (- time (/ m n))))))

(defun make-elliptic-orbit (mu a e i raan argp mean-anomaly time)
  "The orbit with these classical elements and mean anomaly M at time."
  (let ((n (sqrt (/ mu (* a a a)))))
    (make-orbit mu (* a (- 1 e)) e i (wrap-angle raan) (wrap-angle argp)
                (- time (/ (wrap-signed mean-anomaly) n)))))

(defun make-hyperbolic-orbit (mu q e i raan argp mean-anomaly time)
  "The hyperbola (e > 1) with periapsis radius q and mean anomaly M at time."
  (let* ((a (/ q (- e 1)))
         (n (sqrt (/ mu (* a a a)))))
    (make-orbit mu q e i (wrap-angle raan) (wrap-angle argp) (- time (/ mean-anomaly n)))))

(defun perifocal-state (orbit time)
  "(values x y vx vy) in the perifocal plane at time."
  (let* ((e (orbit-e orbit))
         (q (orbit-q orbit))
         (a (abs (orbit-a orbit)))
         (anomaly (orbit-anomaly orbit time)))
    (if (elliptic-p orbit)
        (let* ((ce (cos anomaly)) (se (sin anomaly))
               (b (sqrt (- 1 (* e e))))
               (k (/ (sqrt (* (orbit-mu orbit) a)) (* a (- 1 (* e ce))))))
          (values (* a (- ce e)) (* a b se) (* k (- se)) (* k b ce)))
        (let* ((ch (cosh anomaly)) (sh (sinh anomaly))
               (b (sqrt (- (* e e) 1)))
               (k (/ (sqrt (* (orbit-mu orbit) a)) (* a (- (* e ch) 1)))))
          (values (* q (- 1 (/ (- ch 1) (- e 1))))
                  (* q (sqrt (/ (+ e 1) (- e 1))) sh)
                  (* k (- sh)) (* k b ch))))))

(defun orbit-state (orbit time)
  "(values r v) on the orbit at time, ICRF about its body."
  (multiple-value-bind (x y vx vy) (perifocal-state orbit time)
    (multiple-value-bind (pv qv)
        (perifocal-axes (orbit-i orbit) (orbit-raan orbit) (orbit-argp orbit))
      (values (vec3-add (vec3-scale pv x) (vec3-scale qv y))
              (vec3-add (vec3-scale pv vx) (vec3-scale qv vy))))))

(defun orbit-points (orbit t-start t-end n)
  "A simple-vector of n ≥ 2 positions (vec3, ICRF about the body) at even
times from t-start to t-end: the map's ribbon."
  (multiple-value-bind (pv qv)
      (perifocal-axes (orbit-i orbit) (orbit-raan orbit) (orbit-argp orbit))
    (let ((points (make-array n)))
      (dotimes (k n points)
        (let ((time (+ t-start (* (- t-end t-start) (/ k (float (1- n) 1d0))))))
          (multiple-value-bind (x y) (perifocal-state orbit time)
            (setf (svref points k)
                  (vec3-add (vec3-scale pv x) (vec3-scale qv y)))))))))

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

(defun elliptic-radius-crossings (orbit radius time)
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

(defun orbit-radius-crossings (orbit radius time)
  "(values inbound outbound): the next times after time at which the orbit
crosses radius, falling and rising; nil outside [q, Q] (\"Removal of
objects\"). A hyperbola crosses each way once: a crossing already passed is
nil."
  (if (elliptic-p orbit)
      (elliptic-radius-crossings orbit radius time)
      (when (>= radius (orbit-q orbit))
        (let* ((e (orbit-e orbit))
               (c (max -1d0 (min 1d0 (/ (- (/ (orbit-semi-latus orbit) radius) 1) e))))
               (h (true->hyperbolic (acos c) e))
               (dt (/ (- (* e (sinh h)) h) (orbit-n orbit)))
               (inbound (- (orbit-tp orbit) dt))
               (outbound (+ (orbit-tp orbit) dt)))
          (values (and (> inbound time) inbound)
                  (and (> outbound time) outbound))))))

(defun orbit-j2-rates (orbit body)
  "(values Ω̇ ω̇ Ṁ−n) in rad/s, J2's secular rates; ellipses only."
  (let* ((body (body body))
         (n (orbit-n orbit))
         (e (orbit-e orbit))
         (ci (cos (orbit-i orbit)))
         (k (* n (body-j2 body) (expt (/ (body-radius body) (orbit-semi-latus orbit)) 2))))
    (values (* -3/2 k ci)
            (* 3/4 k (- (* 5 ci ci) 1))
            (* 3/4 k (sqrt (- 1 (* e e))) (- (* 3 ci ci) 1)))))
