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


;;; Lambert's problem (decision 119)

(defun lambert (r1 r2 tof mu &optional (normal (lis::vec3 0 0 1)))
  "(values v1 v2) of `lambert-into!', nil without a solution."
  (let ((v1 (lis::make-vec3)) (v2 (lis::make-vec3)))
    (and (lis::lambert-into! v1 v2 r1 r2 (float tof 1d0) (float mu 1d0) normal)
         (values v1 v2))))

(defun km-vec3 (x y z) (lis::vec3 (* 1000 x) (* 1000 y) (* 1000 z)))

(defun within-km/s (v x y z tolerance)
  "Every component of v within tolerance km/s of (x y z)."
  (every (lambda (a b) (< (abs (- a b)) (* 1000 tolerance))) v (km-vec3 x y z)))

;; Vallado, 4th ed., example 7-5 (μ 398,600.4418 km³/s²); Curtis, 3rd ed.,
;; example 5.2 (μ 398,600), printed to 1e-4 km/s, so within half its last digit.
(define-test lambert-textbook
  (multiple-value-bind (v1 v2) (lambert (km-vec3 15945.34d0 0 0)
                                        (km-vec3 12214.83899d0 10249.46731d0 0)
                                        (* 76 60) 398600.4418d9)
    (true (within-km/s v1 2.058913d0 2.915965d0 0 1d-5))
    (true (within-km/s v2 -3.451565d0 0.910315d0 0 1d-5)))
  (multiple-value-bind (v1 v2) (lambert (km-vec3 5000 10000 2100) (km-vec3 -14600 2500 7000)
                                        3600 398600d9)
    (true (within-km/s v1 -5.9925d0 1.9254d0 3.2456d0 5d-5))
    (true (within-km/s v2 -3.3125d0 -4.1966d0 -0.38529d0 5d-5))))

;; (r1 r2 tof normal) in m, s: two points of a conic and the time between
;; them, transfer angles 5°–355° in 63 steps, 7 hyperbolas, 6,778–70,000 km.
(defparameter *lambert-cases*
  '(
    (6778000 0 0 6752208 590742 0 77.131d0 0.000000d0 -0.000000d0 1.000000d0) ; 5.0°, e 0.00, 6778–6778 km
    (-5483367 3743197 2785886 -6157773 2609377 2667018 178.274d0 0.285763d0 -0.266478d0 0.920505d0) ; 10.6°, e 0.00, 7200–7200 km
    (2580616 -9455636 -1982769 4274401 -8191977 -3823743 445.383d0 0.717588d0 0.050179d0 0.694658d0) ; 16.1°, e 0.00, 10000–10000 km
    (5070244 -4536914 872799 5546101 -3734842 -1531161 194.518d0 0.587521d0 0.725528d0 0.358368d0) ; 21.7°, hyperbolic, 6859–6859 km
    (-35379490 -4187885 22243051 -21457915 -1744001 36062672 6477.479d0 -0.139088d0 0.989665d0 -0.034899d0) ; 27.2°, e 0.00, 42000–42000 km
    (775152 5334606 5200079 3300000 6329353 1988314 597.684d0 -0.742404d0 0.519837d0 -0.422618d0) ; 32.8°, e 0.05, 7410–7490 km
    (3149147 6040592 -3904368 5429218 1652477 -5091015 730.915d0 -0.654508d0 -0.139120d0 -0.743145d0) ; 38.3°, e 0.05, 7624–7852 km
    (2215128 -7961266 1956904 -4000732 -6962085 2764331 404.162d0 -0.167680d0 -0.279066d0 -0.945519d0) ; 43.9°, hyperbolic, 8492–8492 km
    (4655039 26252382 1674908 -16527817 20053059 1666490 5728.696d0 0.019227d0 -0.067054d0 0.997564d0) ; 49.4°, e 0.05, 26040–26714 km
    (-28586440 -30498992 5922954 4865671 -40336037 -11539489 12818.189d0 0.404508d0 -0.206107d0 0.891007d0) ; 55.0°, e 0.05, 42219–42235 km
    (4450023 362745 -5131357 958408 6932554 -3899039 939.385d0 0.719846d0 0.262003d0 0.642788d0) ; 60.6°, e 0.30, 6802–8011 km
    (-154470 3515913 -10388420 -9363790 4931705 -2881407 1311.905d0 0.373658d0 0.880283d0 0.292372d0) ; 66.1°, hyperbolic, 10968–10968 km
    (670683 1732949 12467012 15803947 7473611 3800412 4099.973d0 -0.404508d0 0.908541d0 -0.104528d0) ; 71.7°, e 0.30, 12605–17890 km
    (17061876 35149598 -8458695 21335105 -8412636 -41333665 24482.116d0 -0.826969d0 0.284748d0 -0.484810d0) ; 77.2°, e 0.30, 39977–47270 km
    (56106493 837345 -39011312 16532539 -56695120 9390570 45053.768d0 -0.543597d0 -0.289036d0 -0.788011d0) ; 82.8°, e 0.25, 59798–68341 km
    (-8454753 -2003158 1104796 -2456070 8178924 -1946532 727.724d0 -0.066987d0 -0.250000d0 -0.965926d0) ; 88.3°, hyperbolic, 8759–8759 km
    (-17092277 6129293 2003474 -2162307 -7351876 -715198 3332.574d0 0.073751d0 -0.118025d0 0.990268d0) ; 93.9°, e 0.60, 7697–18268 km
    (7643541 -12928133 -5990528 6369925 7185973 -2933247 3042.094d0 0.505575d0 -0.098274d0 0.857167d0) ; 99.4°, e 0.60, 10041–16169 km
    (-5855055 25615771 -14203895 -19263447 -959905 22226743 11037.614d0 0.654508d0 0.475528d0 0.587785d0) ; 105.0°, e 0.46, 29428–29870 km
    (-9614939 227363 4098000 7038941 -2560653 7293059 1778.505d0 0.118746d0 0.967107d0 0.224951d0) ; 110.6°, hyperbolic, 10454–10454 km
    (4747614 4523690 2345871 848043 -5782397 -28212840 5660.706d0 -0.633022d0 0.754407d0 -0.173648d0) ; 116.1°, e 0.80, 6965–28812 km
    (4900592 1622541 -7405147 -15038613 -60249484 18270172 31789.491d0 -0.837521d0 0.043893d0 -0.544639d0) ; 121.7°, e 0.80, 9027–64730 km
    (13564570 -12509210 -720313 -35309936 -3519067 18781279 66010.513d0 -0.402250d0 -0.388448d0 -0.829038d0) ; 127.2°, e 0.75, 18466–40149 km
    (-4134082 15836742 -3091906 14651149 -7769433 1559697 3065.021d0 0.003330d0 -0.190780d0 -0.981627d0) ; 132.8°, hyperbolic, 16657–16657 km
    (-34981537 -59972126 -3004027 43038388 14714097 -4705599 60889.481d0 0.154508d0 -0.139120d0 0.978148d0) ; 138.3°, e 0.25, 45727–69494 km
    (4995474 2764115 -3653244 -5477785 1408842 3735013 2219.668d0 0.571394d0 0.049990d0 0.819152d0) ; 143.9°, e 0.00, 6778–6778 km
    (-6112351 2502388 2866568 5563297 -4562412 272252 2523.986d0 0.522111d0 0.668271d0 0.529919d0) ; 149.4°, e 0.00, 7200–7200 km
    (7660184 -233405 9021393 -3207749 1307047 -11319028 2433.435d0 -0.154508d0 0.975528d0 0.156434d0) ; 155.0°, hyperbolic, 11837–11837 km
    (14080873 13062027 -17524625 -15391157 -18143812 10484006 18607.749d0 -0.804412d0 0.542582d0 -0.241922d0) ; 160.6°, e 0.00, 26000–26000 km
    (22607841 -28747593 -20650943 -25209168 20783774 26391904 39525.839d0 -0.778167d0 -0.179654d0 -0.601815d0) ; 166.1°, e 0.00, 42000–42000 km
    (395735 -6136087 2953805 -1473918 6705626 -2927329 2942.694d0 -0.250000d0 -0.433013d0 -0.866025d0) ; 171.7°, e 0.05, 6822–7464 km
    (-5768695 -4646695 338524 5770123 5125667 -394713 3422.917d0 0.035631d0 -0.116544d0 -0.992546d0) ; 177.2°, e 0.05, 7415–7728 km
    (7782882 -6656779 -2842607 -7888648 6088756 2798464 5786.295d0 0.247741d0 -0.120831d0 0.961262d0) ; 182.8°, e 0.05, 10351–10629 km
    (-7969122 27198743 -1868449 10145406 -24118778 -670627 24391.756d0 0.587521d0 0.225528d0 0.777146d0) ; 188.3°, e 0.05, 26174–28404 km
    (-6656647 -20800369 40961075 -3560600 21913336 -35703461 49586.136d0 0.330758d0 0.818655d0 0.469472d0) ; 193.9°, e 0.05, 42043–46420 km
    (10352156 5022372 -2020885 -5785517 -3052501 3674085 3978.090d0 -0.421010d0 0.902859d0 0.087156d0) ; 199.4°, e 0.30, 7503–11682 km
    (1898389 -3968820 -9331254 -40710 6979951 6757489 3917.148d0 -0.904508d0 0.293893d0 -0.309017d0) ; 205.0°, e 0.30, 9715–10316 km
    (2181596 -10961330 3918260 3564648 12591263 -10608786 7270.768d0 -0.660084d0 -0.365890d0 -0.656059d0) ; 210.6°, e 0.30, 11843–16846 km
    (-19565948 -15908333 9837185 47614858 3340032 -7198882 39911.381d0 -0.106052d0 -0.425350d0 -0.898794d0) ; 216.1°, e 0.30, 27068–48272 km
    (-41473301 6539739 -1471226 40424793 -49315727 3321425 90118.618d0 0.028504d0 -0.043893d0 -0.998630d0) ; 221.7°, e 0.25, 42012–63853 km
    (5407950 4825826 -1633424 -624703 -11141753 -480271 19780.554d0 0.336824d0 -0.059391d0 0.939693d0) ; 227.2°, e 0.60, 7430–11170 km
    (-8264627 507023 5870454 2768752 5783994 -5307986 21972.718d0 0.544668d0 0.410437d0 0.731354d0) ; 232.8°, e 0.60, 8324–10150 km
    (18863446 -5503373 7864403 -8480685 -1423060 5169786 34586.511d0 0.095492d0 0.908541d0 0.406737d0) ; 238.3°, e 0.60, 10034–21165 km
    (12468587 12245551 -60825076 15639939 13168279 18475857 74671.862d0 -0.655959d0 0.754595d0 0.017452d0) ; 243.9°, e 0.46, 27557–63286 km
    (1224077 -69179485 -9003526 17204735 23215709 -40551917 79850.065d0 -0.926619d0 0.032358d0 -0.374607d0) ; 249.4°, e 0.25, 49794–69774 km
    (-14045580 -14045580 19863449 16519794 -11256967 -3721380 9171.710d0 -0.500000d0 -0.500000d0 -0.707107d0) ; 255.0°, e 0.80, 20334–28091 km
    (-14498681 1376713 -760325 3149248 -50650729 20496162 20216.913d0 0.013074d0 -0.374378d0 -0.927184d0) ; 260.6°, e 0.80, 14584–54731 km
    (11629781 -5422484 -215301 -26348660 -47748872 -199695 63135.376d0 0.013171d0 -0.011450d0 0.999848d0) ; 266.1°, e 0.75, 12834–54537 km
    (-12428437 23243795 4421439 35519253 23729315 -16831899 90022.320d0 0.404508d0 0.042516d0 0.913545d0) ; 271.7°, e 0.46, 26726–45913 km
    (3172231 -32963331 26183588 -42439816 5886632 22347784 112706.093d0 0.440140d0 0.584085d0 0.681998d0) ; 277.2°, e 0.25, 42216–48324 km
    (4854611 2369105 -4094066 5558801 -443772 3852802 4362.205d0 -0.163176d0 0.925417d0 0.342020d0) ; 282.8°, e 0.00, 6778–6778 km
    (-2702706 -4624752 -4811138 1863557 2235946 -6585416 4869.699d0 -0.837521d0 0.543893d0 -0.052336d0) ; 288.3°, e 0.00, 7200–7200 km
    (-2173498 -6305032 7451340 3128380 -9366080 -1577902 8124.407d0 -0.872096d0 -0.217438d0 -0.438371d0) ; 293.9°, e 0.00, 10000–10000 km
    (-24608969 3805939 7477531 -10862208 -16373857 17026721 34704.418d0 -0.318064d0 -0.573802d0 -0.754710d0) ; 299.4°, e 0.00, 26000–26000 km
    (-24082847 32119382 -12343490 -41807882 -1304593 -3794609 72574.200d0 0.095492d0 -0.293893d0 -0.951057d0) ; 305.0°, e 0.00, 42000–42000 km
    (4198224 6003998 -110890 7432199 735449 -562119 5103.574d0 0.078990d0 -0.036834d0 0.996195d0) ; 310.6°, e 0.05, 7327–7490 km
    (-6273150 -2250452 3540864 -6560268 3405975 2555754 5741.337d0 0.435286d0 0.175867d0 0.882948d0) ; 316.1°, e 0.05, 7547–7821 km
    (9691471 -3002927 -826935 7012095 -6475106 4361807 9644.560d0 0.278504d0 0.725528d0 0.629320d0) ; 321.7°, e 0.05, 10180–10494 km
    (-8714011 3497544 -24284762 4617225 9916139 -24023083 41298.766d0 -0.421389d0 0.863976d0 0.275637d0) ; 327.2°, e 0.05, 26037–26396 km
    (-13269334 -36818929 15675541 -11784640 -40139481 -3794787 86177.253d0 -0.949177d0 0.290192d0 -0.121869d0) ; 332.8°, e 0.05, 42005–42160 km
    (-4788804 1658890 5746565 -3572898 -869109 6112018 9134.982d0 -0.750000d0 -0.433013d0 -0.500000d0) ; 338.3°, e 0.30, 7133–7662 km
    (-6384905 6376578 -3599619 -7480370 4457400 -2004785 9981.950d0 -0.135379d0 -0.586391d0 -0.798636d0) ; 343.9°, e 0.30, 8936–9715 km
    (3603896 15782584 -2759832 603379 15257753 -3069688 16337.288d0 0.135281d0 -0.200562d0 -0.970296d0) ; 349.4°, e 0.30, 15575–16422 km
    (-33491420 33889107 6078875 -30209743 36454734 5629076 69517.314d0 0.154508d0 -0.024472d0 0.987688d0) ; 355.0°, e 0.30, 47679–48032 km
    ))

;; v1 flown by Kepler for tof reaches r2, and arrives at v2.
(define-test lambert-cases
  (let ((worst-r 0d0) (worst-v 0d0))
    (loop for (x1 y1 z1 x2 y2 z2 tof nx ny nz) in *lambert-cases*
          for r1 = (lis::vec3 x1 y1 z1)
          for r2 = (lis::vec3 x2 y2 z2)
          do (multiple-value-bind (v1 v2) (lambert r1 r2 tof *mu* (lis::vec3 nx ny nz))
               (true v1)
               (when v1
                 (multiple-value-bind (r v)
                     (lis::orbit-state (lis::state->orbit r1 v1 *mu* 0d0) tof)
                   (setf worst-r (max worst-r (rel-diff r r2))
                         worst-v (max worst-v (rel-diff v v2)))))))
    (true (< worst-r 1d-8))
    (true (< worst-v 1d-8))))

;; Within 0.5° of 180° nil; at 0.6° a solution.
(define-test lambert-near-180
  (flet ((at (degrees)
           (let ((a (deg degrees)))
             (lambert (lis::vec3 7d6 0 0) (lis::vec3 (* 7.2d6 (cos a)) (* 7.2d6 (sin a)) 0)
                      2800 *mu*))))
    (false (at 180))
    (false (at 179.6))
    (false (at 180.4))
    (true (at 179.4))
    (true (at 180.6))))

;; (time (dotimes (i 10000) (loop for (r1 r2 tof n) in cases do (lambert-into! v1
;; v2 r1 r2 tof *mu* n)))) over *lambert-cases* as vec3s: ~0.45 µs a solve
;; (docs/architecture.md "Testing").
(define-test lambert-conses-nothing
  (let ((v1 (lis::make-vec3)) (v2 (lis::make-vec3))
        (r1 (km-vec3 5000 10000 2100)) (r2 (km-vec3 -14600 2500 7000)) (n (lis::vec3 0 0 1)))
    (is = 0 (least-bytes-consed
             (lambda () (dotimes (i 1000) (lis::lambert-into! v1 v2 r1 r2 3600d0 398600d9 n)))))))
