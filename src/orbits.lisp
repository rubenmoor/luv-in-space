(in-package #:luv-in-space)

;;; Osculating conics: state <-> elements, Kepler propagation, apsides,
;;; radius crossings and J2's secular rates (docs/architecture.md "Rails",
;;; decision 68: Orbit elements: one struct for every conic), ellipses and
;;; hyperbolas ("Hyperbolic rails"); near-parabolic forms are milestone 4's.

(defstruct (orbit (:constructor make-orbit (mu q e i raan argp tp)))
  (mu 0d0 :type double-float)
  (q 0d0 :type double-float)     ; periapsis radius
  (e 0d0 :type double-float)
  (i 0d0 :type double-float)
  (raan 0d0 :type double-float)  ; Ω
  (argp 0d0 :type double-float)  ; ω
  (tp 0d0 :type double-float))   ; a periapsis passage

(defparameter *circular-e* 1d-11 "Below this e, ω = 0 (decision 68).")
(defparameter *equatorial-i* 1d-11 "Within this of 0 or π, Ω = 0 (decision 68).")
(defparameter *parabolic-e* 1d-6
  "|e − 1| below this shows as e = 1 ± this, for display only (decision 46).")

(declaim (type double-float *circular-e* *equatorial-i* *parabolic-e*))

(defconstant +two-pi+ (* 2 pi))

(declaim (inline wrap-angle wrap-signed))

(defun wrap-angle (x)
  "x in [0, 2π)."
  (declare (type double-float x))
  (let ((y (mod x +two-pi+)))
    (if (>= y +two-pi+) 0d0 y)))

(defun wrap-signed (x)
  "x in (−π, π]."
  (declare (type double-float x))
  ;; fround's ties-to-even inline: SBCL's fround boxes (task 147)
  (let* ((q (/ x +two-pi+)) (m (abs q)) (big 4503599627370496d0))
    (- x (* +two-pi+ (float-sign q (if (< m big) (- (+ m big) big) m))))))

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

;; Rails (physics.lisp) inline them: a full call boxes the double result.
(declaim (sb-ext:maybe-inline solve-kepler-elliptic solve-kepler-hyperbolic))

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

(declaim (inline true->eccentric true->hyperbolic))
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

(declaim (inline perifocal-axes!))
(defun perifocal-axes! (p-out q-out i raan argp)
  "Write the unit vectors P (to periapsis) and Q (90° ahead of it) in ICRF."
  (declare (type vec3 p-out q-out) (type double-float i raan argp))
  (let ((co (cos raan)) (so (sin raan))
        (cw (cos argp)) (sw (sin argp))
        (ci (cos i)) (si (sin i)))
    (vec3-set! p-out (- (* co cw) (* so sw ci)) (+ (* so cw) (* co sw ci)) (* sw si))
    (vec3-set! q-out (- (- (* co sw)) (* so cw ci)) (- (* co cw ci) (* so sw)) (* cw si))
    (values p-out q-out)))

(defun perifocal-axes (i raan argp)
  "The unit vectors P (to periapsis) and Q (90° ahead of it) in ICRF."
  (perifocal-axes! (make-vec3) (make-vec3) i raan argp))

(defun state->orbit-into! (out r v mu time)
  "Write the osculating orbit of the state (r, v) at time about a body of μ
into OUT and return it. Conses nothing."
  (declare (optimize speed) (type orbit out) (type vec3 r v) (type double-float mu time))
  (let ((h (make-vec3)) (evec (make-vec3)) (node (make-vec3)) (ahead (make-vec3)))
    (declare (dynamic-extent h evec node ahead))
    (vec3-cross! h r v)
    (let* ((hn (vec3-norm h))
           (rn (vec3-norm r))
           (e (let ((s (/ mu rn)))
                (vec3-cross! evec v h)
                (dotimes (k 3) (setf (aref evec k) (- (aref evec k) (* s (aref r k)))))
                (vec3-scale! evec evec (/ mu))
                (let ((e (vec3-norm evec)))
                  (if (< (abs (- e 1)) *parabolic-e*)
                      (if (< e 1) (- 1 *parabolic-e*) (+ 1 *parabolic-e*))
                      e))))
           (p (/ (* hn hn) mu))
           (q (/ p (+ 1 e)))
           (i (atan (sqrt (+ (expt (aref h 0) 2) (expt (aref h 1) 2))) (aref h 2)))
           (raan (if (or (< i *equatorial-i*) (> i (- pi *equatorial-i*)))
                     0d0
                     (wrap-angle (atan (aref h 0) (- (aref h 1))))))
           (argp (progn
                   (vec3-set! node (cos raan) (sin raan) 0d0)
                   ;; ahead = (h / hn) × node, h scaled in place
                   (vec3-cross! ahead (vec3-scale! h h (/ hn)) node)
                   (if (< e *circular-e*)
                       0d0
                       (wrap-angle (atan (vec3-dot evec ahead) (vec3-dot evec node))))))
           (theta (wrap-signed (- (atan (vec3-dot r ahead) (vec3-dot r node)) argp)))
           (m (if (< e 1)
                  (let ((ea (true->eccentric theta e))) (- ea (* e (sin ea))))
                  (let ((x (true->hyperbolic theta e))) (- (* e (sinh x)) x))))
           (a (abs (/ q (- 1 e))))
           (n (sqrt (/ mu (* a a a)))))
      (declare (type double-float hn rn e p q i raan argp theta m a n))
      (setf (orbit-mu out) mu (orbit-q out) q (orbit-e out) e (orbit-i out) i
            (orbit-raan out) raan (orbit-argp out) argp (orbit-tp out) (- time (/ m n)))
      out)))

(defun state->orbit (r v mu time)
  "The osculating orbit of the state (r, v) at time about a body of μ."
  (state->orbit-into! (make-orbit 0d0 0d0 0d0 0d0 0d0 0d0 0d0) r v mu time))

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

;;; Lambert's problem (decision 119: Rendezvous planning in milestone 3)

(declaim (inline stumpff-cs))
(defun stumpff-cs (z)
  "(values C S dC/dz dS/dz) of the Stumpff functions at z; series near 0."
  (declare (type double-float z))
  (if (< (abs z) 0.1d0)
      (let ((c 0d0) (s 0d0) (dc 0d0) (ds 0d0) (zk 1d0) (zk1 0d0)
            (fc 2d0) (fs 6d0))
        (declare (type double-float c s dc ds zk zk1 fc fs))
        ;; C = Σ (−z)^k / (2k+2)!, S = Σ (−z)^k / (2k+3)!
        (loop for k of-type fixnum from 0 below 8
              for sign of-type double-float = (if (evenp k) 1d0 -1d0)
              do (incf c (/ (* sign zk) fc))
                 (incf s (/ (* sign zk) fs))
                 (incf dc (/ (* sign k zk1) fc))
                 (incf ds (/ (* sign k zk1) fs))
                 (setf zk1 zk zk (* zk z)
                       fc (* fc (+ 2 (* 2 k) 1) (+ 2 (* 2 k) 2))
                       fs (* fs (+ 3 (* 2 k) 1) (+ 3 (* 2 k) 2))))
        (values c s dc ds))
      (multiple-value-bind (c s)
          (if (> z 0)
              (let ((sz (sqrt (the (double-float 0d0) z))))
                (values (/ (* 2 (expt (sin (/ sz 2)) 2)) z) (/ (- sz (sin sz)) (* sz z))))
              (let ((sz (sqrt (the (double-float 0d0) (- z)))))
                (values (/ (* 2 (expt (sinh (/ sz 2)) 2)) (- z))
                        (/ (- (sinh sz) sz) (* sz (- z))))))
        (declare (type double-float c s))
        (values c s
                (/ (- 1 (* z s) (* 2 c)) (* 2 z))
                (/ (- c (* 3 s)) (* 2 z))))))

(defconstant +lambert-z-max+ (* 4 pi pi) "Single revolution: z < 4π².")

(defparameter *lambert-skip-angle* (* 0.5d0 (/ pi 180))
  "Within this of 180°, `lambert-into!' returns nil: the plane is undefined.")

(declaim (type double-float *lambert-skip-angle*))

(defun lambert-into! (v1 v2 r1 r2 tof mu normal)
  "Write into V1 and V2 the velocities at R1 and R2 of the single-revolution
conic from R1 to R2 in TOF seconds about a body of MU, prograde about NORMAL;
return t, or nil without a solution or within `*lambert-skip-angle*' of 180°.
Universal variables (Vallado, \"Lambert's problem, universal variables\"),
Newton on z inside a bisection bracket. Conses nothing."
  (declare (optimize speed) (type vec3 v1 v2 r1 r2 normal) (type double-float tof mu))
  (let* ((n1 (vec3-norm r1)) (n2 (vec3-norm r2))
         (cos-dnu (max -1d0 (min 1d0 (/ (vec3-dot r1 r2) (* n1 n2)))))
         (long-way (with-vec3 (((ax ay az) r1) ((bx by bz) r2) ((nx ny nz) normal))
                     (minusp (+ (* nx (- (* ay bz) (* az by)))
                                (* ny (- (* az bx) (* ax bz)))
                                (* nz (- (* ax by) (* ay bx)))))))
         (dnu (if long-way (- +two-pi+ (acos cos-dnu)) (acos cos-dnu)))
         (a (* (if long-way -1d0 1d0) (sqrt (the (double-float 0d0) (* n1 n2 (+ 1 cos-dnu))))))
         (sqrt-mu (sqrt (the (double-float 0d0) mu)))
         (target (* sqrt-mu tof)))
    (declare (type double-float n1 n2 cos-dnu dnu a sqrt-mu target))
    (when (or (< (abs (- dnu pi)) *lambert-skip-angle*) (< dnu 1d-9) (> dnu (- +two-pi+ 1d-9))
              (<= tof 0))
      (return-from lambert-into! nil))
    ;; F(z) = √μ t(z); y < 0 counts as below the root (t → 0 there for A > 0)
    (flet ((ty (z)
             (declare (type double-float z))
             (multiple-value-bind (c s dc ds) (stumpff-cs z)
               (declare (type double-float c s dc ds))
               (let ((y (+ n1 n2 (/ (* a (- (* z s) 1)) (sqrt (the (double-float 0d0) c))))))
                 (if (<= y 0)
                     (values -1d0 0d0 y)
                     (let* ((x (sqrt (the (double-float 0d0) (/ y c)))) (x3 (* x x x)) (sy (sqrt (the (double-float 0d0) y)))
                            (f (+ (* x3 s) (* a sy)))
                            (df (+ (* x3 (- ds (/ (* 3 s dc) (* 2 c))))
                                   (* (/ a 8) (+ (/ (* 3 s sy) c) (/ a x))))))
                       (values f df y)))))))
      (declare (inline ty))
      (let ((lo (* -1d0 +lambert-z-max+))
            (hi (* +lambert-z-max+ (- 1 1d-9))))
        (declare (type double-float lo hi))
        (when (< (the double-float (ty hi)) target)
          (return-from lambert-into! nil))
        ;; down to −4π² 4⁶: cosh √−z overflows past ~−5e5
        (loop repeat 6
              while (>= (the double-float (ty lo)) target)
              do (setf hi lo lo (* 4 lo))
              finally (when (>= (the double-float (ty lo)) target)
                        (return-from lambert-into! nil)))
        (let ((z (if (< lo 0d0 hi) 0d0 (* 0.5d0 (+ lo hi)))))
          (declare (type double-float z))
          (loop repeat 50
                do (multiple-value-bind (f df y) (ty z)
                     (declare (type double-float f df y))
                     (when (and (> y 0) (<= (abs (- f target)) (* 1d-9 target)))
                       (let* ((f-l (- 1 (/ y n1)))
                              (g (* a (sqrt (the (double-float 0d0) (/ y mu)))))
                              (gdot (- 1 (/ y n2))))
                         (dotimes (k 3)
                           (setf (aref v1 k) (/ (- (aref r2 k) (* f-l (aref r1 k))) g)
                                 (aref v2 k) (/ (- (* gdot (aref r2 k)) (aref r1 k)) g))))
                       (return-from lambert-into! t))
                     (if (< f target) (setf lo z) (setf hi z))
                     (let ((next (if (and (> y 0) (> df 0)) (- z (/ (- f target) df)) lo)))
                       (declare (type double-float next))
                       (setf z (if (< lo next hi) next (* 0.5d0 (+ lo hi)))))))
          nil)))))

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
