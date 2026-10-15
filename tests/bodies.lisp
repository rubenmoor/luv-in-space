(in-package #:luv-in-space.tests)

(defun ut1-days->t (days)
  (+ (* days lis::+day+) lis::+tt-minus-utc+))

(defun revolution-difference (a b)
  "|a − b| in revolutions, across the wrap at 1."
  (let ((d (mod (- a b) 1d0)))
    (min d (- 1 d))))

(define-test bodies-list
  (is equal '(:sun :earth :moon) (mapcar #'lis::body-key lis::*bodies*))
  (is = (+ lis::+radius-earth+ 100d3) (lis::body-crash-radius (lis::body :earth)))
  (is = lis::+radius-moon+ (lis::body-crash-radius (lis::body :moon)))
  (is = lis::+radius-sun+ (lis::body-crash-radius (lis::body :sun)))
  (is eq :earth (lis::body-parent (lis::body :moon)))
  (is = lis::+j2-earth+ (lis::body-j2 (lis::body :earth)))
  (is = 0d0 (lis::body-j2 (lis::body :moon)))
  (fail (lis::body :pluto)))

;; docs/architecture.md "Frames": Earth ~9.25e8 m, Moon ~6.6e7 m.
(define-test laplace-spheres
  (true (< (abs (- 1 (/ (lis::body-soi-radius (lis::body :earth)) 9.25d8))) 0.01))
  (true (< (abs (- 1 (/ (lis::body-soi-radius (lis::body :moon)) 6.6d7))) 0.01))
  (false (lis::body-soi-radius (lis::body :sun))))

(define-test earth-rotation-angle
  (true (< (revolution-difference (lis::earth-rotation-revolutions (ut1-days->t 0d0))
                                  0.7790572732640d0)
           1d-12))
  (true (< (revolution-difference (- (lis::earth-rotation-revolutions (ut1-days->t 1d0))
                                     (lis::earth-rotation-revolutions (ut1-days->t 0d0)))
                                  1.00273781191135448d0)
           1d-12)))

(define-test texture-uv
  (is equal '(0.5d0 0.5d0) (multiple-value-list (lis::body-texture-uv (lis::vec3 1 0 0))))
  (is = 0d0 (nth-value 1 (lis::body-texture-uv (lis::vec3 0 0 1))))
  (is = 1d0 (nth-value 1 (lis::body-texture-uv (lis::vec3 0 0 -1))))
  (is = 0.75d0 (lis::body-texture-uv (lis::vec3 0 1 0))))

;; A point over 0° N 0° E at θ = 0 is 0.25 × 1.00273781191135448 rev west
;; of Greenwich 6 UT1 hours later.
(define-test earth-fixed-longitude
  (let* ((t0 (ut1-days->t (/ (- 1 0.7790572732640d0) 1.00273781191135448d0)))
         (r (lis::vec3 lis::+radius-earth+ 0 0)))
    (multiple-value-bind (latitude longitude) (lis::latitude-longitude :earth r t0)
      (true (< (abs latitude) 1d-12))
      (true (< (abs (lis::rad->deg longitude)) 1d-9)))
    (multiple-value-bind (latitude longitude)
        (lis::latitude-longitude :earth r (+ t0 21600))
      (true (< (abs latitude) 1d-12))
      (true (< (abs (- (lis::rad->deg longitude) (* -360 0.25d0 1.00273781191135448d0)))
               1d-9)))))

(define-test moon-pole
  ;; The IAU pole maps to body-fixed +z; the matrix stays a rotation.
  (dolist (time (list 0d0 (lis::start-t) 1.5d9))
    (let* ((centuries (lis::t->julian-centuries time))
           (alpha (lis::deg->rad (+ 269.9949d0 (* 0.0031d0 centuries))))
           (delta (lis::deg->rad (+ 66.5392d0 (* 0.0130d0 centuries))))
           (m (lis::body-fixed-matrix :moon time))
           (pole (lis::mat3*vec3 m (lis::vec3 (* (cos delta) (cos alpha))
                                              (* (cos delta) (sin alpha))
                                              (sin delta))))
           (identity (lis::mat3* m (lis::mat3-transpose m))))
      (true (< (abs (- 1 (aref pole 2))) 1d-15))
      (dotimes (i 9)
        (true (< (abs (- (aref identity i) (if (member i '(0 4 8)) 1 0))) 1d-15))))))

;;; Ephemerides (docs/architecture.md "Ephemeris spot checks").

(defun jd->t (jd)
  (* (- jd 2451545) 86400d0))

(defun km (x y z)
  (lis::vec3 (* 1d3 x) (* 1d3 y) (* 1d3 z)))

(defun angle-between (a b)
  (atan (lis::vec3-norm (lis::vec3-cross a b)) (lis::vec3-dot a b)))

(defconstant +arcminute+ (/ pi 10800))

;; JPL Horizons API, fetched 2026-10-14 (source DE441): EPHEM_TYPE=VECTORS,
;; VEC_TABLE=2, REF_SYSTEM=ICRF, REF_PLANE=FRAME, VEC_CORR=NONE (geometric),
;; TIME_TYPE=TDB, OUT_UNITS=KM-S, TLIST='2026-01-01 00:00' '2040-01-01 00:00'
;; '2049-06-01 00:00'; (JD TDB, position in km).
;; COMMAND='3' CENTER='500@10': the Earth-Moon barycentre from the Sun.
(defparameter *horizons-emb*
  (list (list 2461041.5d0 (km -2.607038480622956d+07 1.328352222667653d+08 5.758184498672740d+07))
        (list 2466154.5d0 (km -2.455382461922732d+07 1.330790493706418d+08 5.768280063361930d+07))
        (list 2469593.5d0 (km -5.119463906041697d+07 -1.310038912619569d+08 -5.677892382456652d+07))))

;; COMMAND='301' CENTER='500@399': the Moon from Earth.
(defparameter *horizons-moon*
  (list (list 2461041.5d0 (km 1.443257274919273d+05 2.895841575449329d+05 1.601589230016842d+05))
        (list 2466154.5d0 (km -1.744411495543004d+05 3.241622906665963d+05 1.681131783434096d+05))
        (list 2469593.5d0 (km 1.040808655731139d+05 3.563104050245243d+05 1.500414680427390d+05))))

;; Meeus, "Astronomical Algorithms", 2nd ed., example 47.a: 1992-04-12 0h TD.
(define-test moon-series-meeus-example
  (multiple-value-bind (lambda beta distance)
      (lis::moon-ecliptic-of-date (jd->t 2448724.5d0))
    (true (< (abs (- lambda 133.162655d0)) 1d-6))
    (true (< (abs (- beta -3.229126d0)) 1d-6))
    (true (< (abs (- distance 368409.7d3)) 100))))

;; Meeus example 21.c: from J2000.0 to -214 June 30.0 (JDE 1643074.5).
(define-test ecliptic-precession-meeus-example
  (multiple-value-bind (lambda beta)
      (lis::precess-ecliptic 149.48194d0 1.76549d0 0d0 (/ (- 1643074.5d0 2451545) 36525))
    (true (< (abs (- lambda 118.704d0)) 1d-3))
    (true (< (abs (- beta 1.615d0)) 1d-3))))

(define-test emb-against-horizons
  (loop for (jd expected) in *horizons-emb*
        for r = (lis::emb-series (jd->t jd))
        do (true (< (angle-between r expected) +arcminute+))
           (true (< (abs (- 1 (/ (lis::vec3-norm r) (lis::vec3-norm expected)))) 1d-4))))

(define-test moon-against-horizons
  (loop for (jd expected) in *horizons-moon*
        do (dolist (r (list (lis::body-state :moon (jd->t jd) :earth)
                            (lis::body-state-series :moon (jd->t jd) :earth)))
             (true (< (angle-between r expected) +arcminute+))
             (true (< (abs (- (lis::vec3-norm r) (lis::vec3-norm expected))) 50d3)))))

;; docs/design.md "The start": the Sun near RA 280.5°, Dec −23° (ICRF);
;; Horizons, COMMAND='10' CENTER='500@399' EPHEM_TYPE=OBSERVER QUANTITIES=1
;; (astrometric, ICRF) at 2040-01-01 00:00 UT: 280.454127434° −23.086134515°.
(define-test sun-at-start
  (let* ((r (lis::body-state :sun (lis::start-t) :earth))
         (ra (mod (lis::rad->deg (atan (aref r 1) (aref r 0))) 360))
         (dec (lis::rad->deg (asin (/ (aref r 2) (lis::vec3-norm r))))))
    (true (< (abs (- ra 280.5d0)) 0.5d0))
    (true (< (abs (- dec -23d0)) 0.5d0))
    (true (< (abs (- ra 280.454127434d0)) (/ 1 60d0)))
    (true (< (abs (- dec -23.086134515d0)) (/ 1 60d0)))))

(defun times-in-2040 (n)
  (let ((state (sb-ext:seed-random-state 2040))
        (t0 (lis::utc->t 2040 1 1 0 0 0)))
    (loop repeat n collect (+ t0 (random (* 366 86400d0) state)))))

;; Decision 18 estimated 2 cm; 4,096 s nodes give 2.5 cm at most (measured
;; over 2040 in 1,579 s steps), so the bound is 3 cm.
(define-test hermite-against-series
  (lis::clear-ephemeris-cache)
  (dolist (time (times-in-2040 200))
    (let ((moon (lis::body-state :moon time :earth))
          (emb (lis::hermite-into (lis::make-vec3) (lis::make-vec3) 0 time)))
      (true (< (lis::vec3-norm (lis::vec3-sub moon (lis::moon-position-series time))) 0.03d0))
      (true (< (lis::vec3-norm (lis::vec3-sub emb (lis::emb-series time))) 0.02d0)))))

(defun cached-node (quantity index)
  (let ((offset (lis::ephemeris-node quantity index)))
    (subseq (cdr lis::*ephemeris-cache*) offset (+ offset 6))))

(define-test node-is-pure
  (dolist (quantity '(0 1))
    (lis::clear-ephemeris-cache)
    (let ((first (cached-node quantity 115000)))
      (lis::clear-ephemeris-cache)
      (loop for index from 114950 below 115050 unless (= index 115000)
            do (lis::ephemeris-node 0 index) (lis::ephemeris-node 1 index))
      (true (every #'eql first (cached-node quantity 115000))))))

(define-test earth-moon-identities
  (dolist (time (times-in-2040 10))
    (let* ((f (/ lis::+mu-moon+ (+ lis::+mu-earth+ lis::+mu-moon+)))
           (moon (lis::body-state :moon time :earth))
           (earth (lis::body-state :earth time :sun))
           (emb (lis::hermite-into (lis::make-vec3) (lis::make-vec3) 0 time)))
      ;; 1.5e11 m carries ~3e-5 m per ulp.
      (true (< (lis::vec3-norm (lis::vec3-add-scaled (lis::vec3-sub earth emb) moon f)) 1d-4))
      (true (< (lis::vec3-norm (lis::vec3-sub (lis::body-state :moon time :sun)
                                              (lis::vec3-add earth moon)))
               1d-4))
      (true (equalp (lis::vec3-scale moon -1d0) (lis::body-state :earth time :moon)))
      (true (every #'zerop (lis::body-state :earth time :earth))))))

;; The Moon's prime meridian points roughly at Earth (libration < 10°).
(define-test moon-faces-earth
  (loop for (jd) in *horizons-moon*
        for time = (jd->t jd)
        for earth = (lis::mat3*vec3 (lis::body-fixed-matrix :moon time)
                                    (lis::body-state :earth time :moon))
        do (true (< (angle-between earth (lis::vec3 1 0 0)) (lis::deg->rad 10d0)))))

;;; Sunlight at a point (decision 79: Sunlight at a point and the sunlit
;;; ship in the exposure rule).

(defun start-orbit ()
  "The start orbit (decision 38) as a Kepler orbit, u = 0 at (start-t)."
  (destructuring-bind (&key a e i raan u) lis::*start-orbit*
    (lis::make-elliptic-orbit lis::+mu-earth+ a e (lis::deg->rad i) (lis::deg->rad raan)
                              0d0 (lis::deg->rad u) (lis::start-t))))

(define-test sun-visible-fraction-start-and-night
  (let* ((orbit (start-orbit))
         (t0 (lis::start-t))
         (t180 (+ t0 (* 0.5d0 (lis::orbit-period orbit)))))
    (true (< (abs (- 1 (lis::sun-visible-fraction (lis::orbit-state orbit t0) t0))) 1d-12))
    (is = 0d0 (lis::sun-visible-fraction (lis::orbit-state orbit t180) t180))))

(define-test sun-visible-fraction-shadow-entry
  ;; 1 s samples over the first half orbit (docs/design.md "The start"):
  ;; never rising, the penumbra 5-15 s, the 0.5 point at the geometric
  ;; shadow edge, 180 deg - asin(R/a) from the Sun.
  (let* ((orbit (start-orbit))
         (t0 (lis::start-t))
         (samples (coerce (loop for s below (floor (lis::orbit-period orbit) 2)
                                for time = (+ t0 s)
                                collect (lis::sun-visible-fraction (lis::orbit-state orbit time) time))
                          'vector))
         (partial (position-if (lambda (f) (< f 1)) samples))
         (dark (position 0d0 samples)))
    (true (and partial dark))
    (true (every #'>= samples (subseq samples 1)))
    (true (<= 5 (- dark partial) 15) "penumbra ~D s" (- dark partial))
    (let* ((k (position-if (lambda (f) (< f 0.5d0)) samples))
           (a (aref samples (1- k))) (b (aref samples k))
           (time (+ t0 (1- k) (/ (- a 0.5d0) (- a b))))
           (r (lis::orbit-state orbit time))
           (angle (lis::rad->deg (angle-between r (lis::body-state :sun time))))
           (edge (- 180 (lis::rad->deg (asin (/ lis::+radius-earth+ (lis::vec3-norm r)))))))
      (true (< (abs (- angle edge)) 0.1d0) "0.5 at ~,3F deg from the Sun, edge ~,3F" angle edge))))

(define-test sun-visible-fraction-annular
  ;; 380,000 km beyond the Moon on the Sun-Moon line, past the umbra's tip
  ;; (~370,000 km): the Moon's whole disc on the Sun's.
  (let* ((time (lis::start-t))
         (sun (lis::body-state :sun time))
         (moon (lis::body-state :moon time))
         (p (lis::vec3-add-scaled moon (lis::vec3-normalize (lis::vec3-sub moon sun)) 3.8d8))
         (fraction (lis::sun-visible-fraction p time))
         (moon-radius (asin (/ lis::+radius-moon+ 3.8d8)))
         (sun-radius (asin (/ lis::+radius-sun+ (lis::vec3-norm (lis::vec3-sub sun p))))))
    (true (< 0 fraction 1))
    (true (< (abs (- fraction (- 1 (expt (/ moon-radius sun-radius) 2)))) 1d-9) "~F" fraction)))

(defun monte-carlo-overlap (r1 r2 d n)
  "The fraction of N points, uniform in the disc of radius R2 at (D, 0),
inside the disc of radius R1 at the origin; a fixed seed."
  (let ((state (sb-ext:seed-random-state 79)))
    (/ (loop repeat n
             count (let* ((rho (* r2 (sqrt (random 1d0 state))))
                          (phi (random (* 2 pi) state))
                          (x (+ d (* rho (cos phi)))) (y (* rho (sin phi))))
                     (<= (+ (* x x) (* y y)) (* r1 r1))))
       (float n 1d0))))

(define-test lens-area
  (is = 0d0 (lis::lens-area 1d0 0.5d0 1.6d0))
  (is = (* pi 0.01d0) (lis::lens-area 1d0 0.1d0 0.5d0))
  (is = (* pi 0.01d0) (lis::lens-area 0.1d0 1d0 0.5d0))
  (loop for (r1 r2 d) in '((1d0 1d0 1d0) (1d0 0.5d0 1.2d0) (0.3d0 1d0 0.9d0))
        for exact = (/ (lis::lens-area r1 r2 d) (* pi r2 r2))
        for sampled = (monte-carlo-overlap r1 r2 d 1000000)
        do (true (< (abs (- exact sampled)) 2d-3) "~A ~A ~A: ~F, sampled ~F" r1 r2 d exact sampled)))

(define-test sun-visible-fraction-conses-nothing
  (let ((p (lis::vec3 7d6 1d6 0d0))
        (time (lis::start-t)))
    (is = 0 (least-bytes-consed
             (lambda () (dotimes (i 10000) (lis::sun-visible-fraction p time)))))))
