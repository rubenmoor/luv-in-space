(in-package #:luv-in-space.tests)

(defun rel-diff (a b)
  "|a − b| / |b| of two vec3s."
  (/ (lis::vec3-norm (lis::vec3-sub a b)) (lis::vec3-norm b)))

(defun angle-diff (a b)
  (abs (lis::wrap-signed (- a b))))

(defparameter *mu* lis::+mu-earth+)

(defun deg (x) (lis::deg->rad (float x 1d0)))

;; (a e i Ω ω) of docs/architecture.md "Rails, orbits and frames": circular
;; equatorial, the polar start orbit (decision 38: Start orbit: polar),
;; retrograde, e = 0.5, e = 0.9.
(defparameter *test-orbits*
  (list (list 7d6 0d0 0d0 0d0 0d0)
        (list 6778136.6d0 0d0 (deg 90) (deg 270) 0d0)
        (list 7.5d6 0.01d0 (deg 150) (deg 40) (deg 70))
        (list 1.4d7 0.5d0 (deg 30) (deg 120) (deg 250))
        (list 7d7 0.9d0 (deg 63.4) (deg 300) (deg 10))))

(defun anomalies () (loop for k below 8 collect (+ 0.1d0 (* k (/ pi 4)))))

(defun test-orbit (elements m time)
  (destructuring-bind (a e i raan argp) elements
    (lis::make-elliptic-orbit *mu* a e i raan argp m time)))

(defun nonsingular (orbit time)
  "e cos ϖ, e sin ϖ, tan(i/2) cos Ω, tan(i/2) sin Ω, mean longitude at time."
  (let ((lon (+ (lis::orbit-raan orbit) (lis::orbit-argp orbit)))
        (tan-half (tan (/ (lis::orbit-i orbit) 2))))
    (list (* (lis::orbit-e orbit) (cos lon)) (* (lis::orbit-e orbit) (sin lon))
          (* tan-half (cos (lis::orbit-raan orbit))) (* tan-half (sin (lis::orbit-raan orbit)))
          (+ lon (lis::orbit-mean-anomaly orbit time)))))

(define-test kepler-round-trip
  (let ((time (lis::start-t)) (worst-r 0d0) (worst-v 0d0) (worst-elements 0d0))
    (dolist (elements *test-orbits*)
      (dolist (m (anomalies))
        (let ((given (test-orbit elements m time)))
          (multiple-value-bind (r v) (lis::orbit-state given time)
            (let ((orbit (lis::state->orbit r v *mu* time)))
              (multiple-value-bind (r2 v2) (lis::orbit-state orbit time)
                (setf worst-r (max worst-r (rel-diff r2 r))
                      worst-v (max worst-v (rel-diff v2 v))))
              (setf worst-elements
                    (max worst-elements
                         (abs (- 1 (/ (lis::orbit-q orbit) (lis::orbit-q given))))
                         (abs (- (lis::orbit-e orbit) (lis::orbit-e given)))
                         (abs (- (lis::orbit-i orbit) (lis::orbit-i given)))
                         (reduce #'max (mapcar #'angle-diff
                                               (nonsingular orbit time)
                                               (nonsingular given time))))))))))
    (true (< worst-r 1d-9))
    (true (< worst-v 1d-9))
    (true (< worst-elements 1d-9))))

(define-test kepler-propagation
  (let ((t0 (lis::start-t)))
    (dolist (elements *test-orbits*)
      (dolist (m (anomalies))
        (multiple-value-bind (r v) (lis::orbit-state (test-orbit elements m t0) t0)
          (let* ((orbit (lis::state->orbit r v *mu* t0))
                 (t1 (+ t0 (* 3.7d0 (lis::orbit-period orbit)))))
            (multiple-value-bind (r1 v1) (lis::orbit-state orbit t1)
              (multiple-value-bind (r0 v0)
                  (lis::orbit-state (lis::state->orbit r1 v1 *mu* t1) t0)
                (true (< (rel-diff r0 r) 1d-9))
                (true (< (rel-diff v0 v) 1d-9))))))))))

(define-test kepler-residual
  (let ((worst 0d0))
    (dolist (e '(0d0 0.1d0 0.5d0 0.8d0 0.81d0 0.9d0 0.99d0))
      (loop for k from -100 to 100
            for m = (* pi (/ k 100d0))
            for ea = (lis::solve-kepler-elliptic m e)
            do (setf worst (max worst (abs (- ea (* e (sin ea)) m))))))
    (true (< worst 1d-14))))

;; docs/design.md "The start": 92.6 min, 7,669 m/s.
(define-test start-orbit
  (let* ((orbit (lis::make-elliptic-orbit *mu* 6778136.6d0 0d0 (deg 90) (deg 270) 0d0
                                          0d0 (lis::start-t)))
         (v (nth-value 1 (lis::orbit-state orbit (lis::start-t)))))
    (true (< (abs (- (/ (lis::orbit-period orbit) 60) 92.6d0)) 0.1d0))
    (true (< (abs (- (lis::vec3-norm v) 7669)) 1))))

(defun k-17 ()
  "Derelict K-17's 800 × 90 km orbit, at apogee at t = 0."
  (let* ((rp (+ lis::+radius-earth+ 90d3))
         (ra (+ lis::+radius-earth+ 800d3))
         (a (/ (+ rp ra) 2)))
    (lis::make-elliptic-orbit *mu* a (/ (- ra rp) (+ ra rp)) (deg 90) (deg 270) 0d0 pi 0d0)))

(define-test apsides
  (let ((orbit (k-17)))
    (true (< (abs (- (lis::orbit-periapsis-radius orbit) (+ lis::+radius-earth+ 90d3))) 1d-6))
    (true (< (abs (- (lis::orbit-apoapsis-radius orbit) (+ lis::+radius-earth+ 800d3))) 1d-6))
    (true (< (abs (- (lis::orbit-a orbit) 6823136.6d0)) 1d-6))
    (true (< (abs (- (lis::orbit-e orbit) 0.052029d0)) 1d-6))
    (let ((period (lis::orbit-period orbit)))
      (true (< (abs (- (/ period 60) 93.48d0)) 0.01d0))
      (true (< (abs (- (lis::orbit-next-apsis orbit 0d0 :apoapsis) period)) 1d-6))
      (true (< (abs (- (lis::orbit-next-apsis orbit 0d0 :periapsis) (/ period 2))) 1d-6))
      (true (< (abs (- (lis::orbit-next-apsis orbit 1d0 :apoapsis) period)) 1d-6)))
    (multiple-value-bind (r v) (lis::orbit-state orbit 0d0)
      (true (< (abs (- (lis::vec3-norm r) (+ lis::+radius-earth+ 800d3))) 1d-6))
      (true (< (abs (- (lis::orbit-energy orbit)
                       (- (/ (lis::vec3-dot v v) 2) (/ *mu* (lis::vec3-norm r)))))
               1d-6)))))

;; docs/architecture.md "Removal of objects": θ = −14.35°, 2,603.07 s after apogee.
(define-test radius-crossings
  (let ((orbit (k-17)) (rc (+ lis::+radius-earth+ 100d3)))
    (multiple-value-bind (inbound outbound) (lis::orbit-radius-crossings orbit rc 0d0)
      (true (< (abs (- inbound 2603.07d0)) 0.01d0))
      (true (< (abs (- (lis::rad->deg (lis::orbit-true-anomaly orbit inbound)) -14.35d0))
               0.01d0))
      (true (< (abs (- (+ outbound inbound) (lis::orbit-period orbit))) 1d-6))
      (true (< (abs (- (lis::vec3-norm (lis::orbit-state orbit inbound)) rc)) 1d-3)))
    (let ((high (lis::make-elliptic-orbit *mu* (+ rc 1 200d3) (/ 200d3 (+ rc 1 200d3))
                                         0d0 0d0 0d0 0d0 0d0)))
      (true (< (abs (- (lis::orbit-periapsis-radius high) (+ rc 1))) 1d-6))
      (false (lis::orbit-radius-crossings high rc 0d0)))))

(define-test j2-rates
  (let* ((a (+ lis::+radius-earth+ 400d3))
         (orbit (lis::make-elliptic-orbit *mu* a 0d0 (deg 51.6) 0d0 0d0 0d0 0d0))
         (node-rate (lis::orbit-j2-rates orbit :earth))
         (closed (* -3/2 (sqrt (/ *mu* (expt a 3))) lis::+j2-earth+
                    (expt (/ lis::+radius-earth+ a) 2) (cos (deg 51.6)))))
    (true (< (abs (- (lis::rad->deg (* node-rate 86400)) -5.0d0)) 0.1d0))
    (true (< (abs (- 1 (/ node-rate closed))) 1d-12))
    (true (< (abs (lis::orbit-j2-rates
                   (lis::make-elliptic-orbit *mu* a 0d0 (deg 90) (deg 270) 0d0 0d0 0d0)
                   :earth))
             1d-15))))

;; docs/design.md "The start": β −10°, the ship 25° from the Sun.
(define-test start-lighting
  (let* ((time (lis::start-t))
         (orbit (lis::make-elliptic-orbit *mu* 6778136.6d0 0d0 (deg 90) (deg 270) 0d0
                                          0d0 time))
         (sun (lis::vec3-normalize (lis::body-state :sun time :earth))))
    (multiple-value-bind (r v) (lis::orbit-state orbit time)
      (let ((normal (lis::vec3-normalize (lis::vec3-cross r v))))
        (true (< (abs (- (lis::rad->deg (asin (lis::vec3-dot sun normal))) -10)) 1))
        (true (< (abs (- (lis::rad->deg (acos (lis::vec3-dot sun (lis::vec3-normalize r))))
                         25))
                 1))))))

;;; Hyperbolas (docs/architecture.md "Hyperbolic rails")

;; (q e i Ω ω): e = 1.5 and e = 3.
(defparameter *test-hyperbolas*
  (list (list 7.378d6 1.5d0 (deg 40) (deg 120) (deg 250))
        (list 1d7 3d0 (deg 150) (deg 10) (deg 80))))

(defun hyperbolic-anomalies () '(0d0 0.5d0 -0.5d0 2d0 -2d0))

(defun test-hyperbola (elements h time)
  (destructuring-bind (q e i raan argp) elements
    (lis::make-hyperbolic-orbit *mu* q e i raan argp (- (* e (sinh h)) h) time)))

(define-test hyperbolic-round-trip
  (let ((time (lis::start-t)) (worst 0d0))
    (dolist (elements *test-hyperbolas*)
      (dolist (h (hyperbolic-anomalies))
        (let ((given (test-hyperbola elements h time)))
          (multiple-value-bind (r v) (lis::orbit-state given time)
            (let ((orbit (lis::state->orbit r v *mu* time)))
              (multiple-value-bind (r2 v2) (lis::orbit-state orbit time)
                (setf worst (max worst (rel-diff r2 r) (rel-diff v2 v)
                                 (abs (- 1 (/ (lis::orbit-q orbit) (lis::orbit-q given))))
                                 (abs (- (lis::orbit-e orbit) (lis::orbit-e given)))
                                 (abs (- (lis::orbit-i orbit) (lis::orbit-i given)))
                                 (angle-diff (lis::orbit-raan orbit) (lis::orbit-raan given))
                                 (angle-diff (lis::orbit-argp orbit) (lis::orbit-argp given))
                                 (/ (abs (- (lis::orbit-tp orbit) (lis::orbit-tp given)))
                                    (/ (lis::orbit-n given)))))))))))
    (true (< worst 1d-9))))

(define-test hyperbolic-propagation
  (let ((t0 (lis::start-t)))
    (dolist (elements *test-hyperbolas*)
      (dolist (h (hyperbolic-anomalies))
        (multiple-value-bind (r v) (lis::orbit-state (test-hyperbola elements h t0) t0)
          (let* ((orbit (lis::state->orbit r v *mu* t0))
                 (t1 (+ t0 (/ 1.7d0 (lis::orbit-n orbit)))))
            (multiple-value-bind (r1 v1) (lis::orbit-state orbit t1)
              (multiple-value-bind (r0 v0)
                  (lis::orbit-state (lis::state->orbit r1 v1 *mu* t1) t0)
                (true (< (rel-diff r0 r) 1d-9))
                (true (< (rel-diff v0 v) 1d-9))))))))))

(define-test hyperbolic-kepler-residual
  (let ((worst 0d0))
    (dolist (e '(1.5d0 3d0))
      (loop for k from -6 to 3
            do (dolist (m (list (expt 10d0 k) (* 3 (expt 10d0 k))
                                (- (expt 10d0 k)) (* -3 (expt 10d0 k))))
                 (when (<= (abs m) 1d3)
                   (let ((h (lis::solve-kepler-hyperbolic m e)))
                     (setf worst (max worst (/ (abs (- (* e (sinh h)) h m)) (abs m)))))))))
    (true (< worst 1d-14))))

;; √(v∞² + 2μ/q) at periapsis, and no apoapsis or period.
(define-test hyperbolic-periapsis
  (let* ((q 7.378d6) (v-inf 500d0)
         (orbit (lis::make-hyperbolic-orbit *mu* q (+ 1 (/ (* q v-inf v-inf) *mu*))
                                            (deg 30) 0d0 0d0 0d0 0d0))
         (speed (lis::vec3-norm (nth-value 1 (lis::orbit-state orbit 0d0)))))
    (true (< (abs (- 1 (/ speed (sqrt (+ (* v-inf v-inf) (/ (* 2 *mu*) q)))))) 1d-12))
    (false (lis::orbit-period orbit))
    (false (lis::orbit-apoapsis-radius orbit))
    (false (lis::orbit-next-apsis orbit 1d0 :periapsis))
    (true (< (abs (lis::orbit-next-apsis orbit -1d0 :periapsis)) 1d-9))))

(define-test hyperbolic-radius-crossings
  (let* ((orbit (test-hyperbola (first *test-hyperbolas*) 0d0 0d0))
         (radius 2d7))
    (multiple-value-bind (inbound outbound) (lis::orbit-radius-crossings orbit radius -1d6)
      (true (< (abs (+ inbound outbound)) 1d-6))
      (true (< inbound 0 outbound))
      (dolist (time (list inbound outbound))
        (true (< (abs (- (lis::vec3-norm (lis::orbit-state orbit time)) radius)) 1d-3))))
    (multiple-value-bind (inbound outbound) (lis::orbit-radius-crossings orbit radius 0d0)
      (false inbound)
      (true outbound))
    (false (lis::orbit-radius-crossings orbit 7d6 -1d6))))

;;; The non-consing variant (task 147)

(defun state->orbit-test-states ()
  "(r v) of the test ellipses (circular, equatorial, polar among them) and
hyperbolas at their anomalies."
  (let ((time (lis::start-t)))
    (append
     (loop for elements in *test-orbits*
           append (loop for m in (anomalies)
                        collect (multiple-value-list
                                 (lis::orbit-state (test-orbit elements m time) time))))
     (loop for elements in *test-hyperbolas*
           append (loop for h in (hyperbolic-anomalies)
                        collect (multiple-value-list
                                 (lis::orbit-state (test-hyperbola elements h time) time)))))))

;; One orbit reused across all states: each equals a fresh state->orbit bit for bit.
(define-test state->orbit-into
  (let ((out (lis::state->orbit (lis::vec3 7d6 0 0) (lis::vec3 0 7.5d3 0) *mu* 0d0))
        (time (lis::start-t)))
    (loop for (r v) in (state->orbit-test-states)
          do (true (equalp (lis::state->orbit-into! out r v *mu* time)
                           (lis::state->orbit r v *mu* time)))
             (true (eq out (lis::state->orbit-into! out r v *mu* time))))))

(define-test state->orbit-into-conses-nothing
  (destructuring-bind (r v) (first (state->orbit-test-states))
    (let ((out (lis::state->orbit r v *mu* 0d0)) (time (lis::start-t)))
      (is = 0 (least-bytes-consed
               (lambda () (dotimes (i 1000) (lis::state->orbit-into! out r v *mu* time))))))))

;; |e − 1| < 1e-6 shows as 1 ± 1e-6 on its side.
(define-test near-parabolic-display
  (let* ((r (lis::vec3 7d6 0 0))
         (escape (sqrt (/ (* 2 *mu*) 7d6))))
    (dolist (side '(1 -1))
      (let ((orbit (lis::state->orbit r (lis::vec3 0 (* escape (+ 1 (* side 1d-8))) 0)
                                      *mu* 0d0)))
        (true (= (lis::orbit-e orbit) (+ 1 (* side 1d-6))))))))

