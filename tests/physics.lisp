(in-package #:luv-in-space.tests)

;;; physics.lisp: ship state, gravity models, the coarse step rule
;;; (docs/architecture.md "Gravity model", "Integrator").

(defun relative-error (x reference)
  (/ (abs (- x reference)) (abs reference)))

(defun gravity (r model &optional (time (lis::start-t)))
  (lis::gravity-into! (lis::make-vec3) r time model))

(define-test ship-state-copy
  (let* ((state (lis::make-ship-state (lis::vec3 7d6 0 0) (lis::vec3 0 7.5d3 0)
                                      (lis::quat 1 0 0 0) (lis::vec3 0 0.1d0 0) 28000))
         (copy (lis::copy-ship-state state)))
    (true (equalp state copy))
    (false (eq (lis::ship-state-r state) (lis::ship-state-r copy)))
    (false (eq (lis::ship-state-q state) (lis::ship-state-q copy)))
    (is = 0d0 (lis::ship-state-dv copy))
    (setf (aref (lis::ship-state-w copy) 1) 0.2d0)
    (false (equalp state copy))
    (is = 0.1d0 (aref (lis::ship-state-w state) 1))))

(define-test gravity-none-and-point
  (let ((r (lis::vec3 7d6 -2d6 3d6)))
    (is equalp (lis::vec3 0 0 0) (gravity r :none))
    (let ((a (gravity r :point))
          (rn (lis::vec3-norm r)))
      (true (< (relative-error (lis::vec3-norm a) (/ lis::+mu-earth+ (* rn rn))) 1d-15))
      (true (< (lis::vec3-norm (lis::vec3-cross a r)) (* 1d-15 (lis::vec3-norm a) rn))))
    (fail (gravity r :nonsense))))

(define-test gravity-j2-closed-forms
  (let* ((rr (+ lis::+radius-earth+ 400d3))
         (k (/ lis::+mu-earth+ (* rr rr)))
         (c (* lis::+j2-earth+ (expt (/ lis::+radius-earth+ rr) 2))))
    ;; Equator, at 30° longitude so x and y both carry the term.
    (let* ((u (lis::vec3 (cos (/ pi 6)) (sin (/ pi 6)) 0))
           (a (gravity (lis::vec3-scale u rr) :j2))
           (radial (lis::vec3-dot a u)))
      (true (< (relative-error radial (- (* k (+ 1 (* 1.5d0 c))))) 1d-14))
      (true (< (lis::vec3-norm (lis::vec3-add-scaled a u (- radial)))
               (* 1d-14 (abs radial)))))
    ;; Over the pole.
    (let ((a (gravity (lis::vec3 0 0 rr) :j2)))
      (true (< (relative-error (aref a 2) (- (* k (- 1 (* 3 c))))) 1d-14))
      (is = 0d0 (aref a 0))
      (is = 0d0 (aref a 1)))
    ;; z → −z mirrors the acceleration exactly.
    (let ((a (gravity (lis::vec3 4d6 -3d6 5d6) :j2))
          (b (gravity (lis::vec3 4d6 -3d6 -5d6) :j2)))
      (is = (aref a 0) (aref b 0))
      (is = (aref a 1) (aref b 1))
      (is = (aref a 2) (- (aref b 2))))))

(defun rational-sqrt (x)
  "√x of a positive rational, to far below double precision."
  (let ((y (rational (sqrt (float x 1d0)))))
    (dotimes (i 4 y)
      (setf y (/ (+ y (/ x y)) 2)))))

(defun exact-third-body (r d mu)
  "μ ((d − r)/|d − r|³ − d/|d|³) in rationals, as a list of three."
  (let* ((r (map 'list #'rational r))
         (d (map 'list #'rational d))
         (mu (rational mu))
         (s (mapcar #'- d r))
         (s2 (reduce #'+ (mapcar #'* s s)))
         (d2 (reduce #'+ (mapcar #'* d d)))
         (ks (/ mu (* s2 (rational-sqrt s2))))
         (kd (/ mu (* d2 (rational-sqrt d2)))))
    (mapcar (lambda (si di) (- (* ks si) (* kd di))) s d)))

(defun third-body-term (r d mu)
  (lis::third-body-add! (lis::make-vec3) r d mu))

(defun three-points (d distance)
  "Points at DISTANCE towards, away from and across the direction of d."
  (let ((u (lis::vec3-normalize d))
        (across (lis::vec3-normalize (lis::vec3-cross d (lis::vec3 0 0 1)))))
    (list (lis::vec3-scale u distance)
          (lis::vec3-scale u (- distance))
          (lis::vec3-scale across distance))))

(define-test gravity-third-bodies
  (let ((time (lis::start-t)))
    (dolist (body (list (cons :moon lis::+mu-moon+) (cons :sun lis::+mu-sun+)))
      (let ((d (lis::body-state (car body) time :earth)))
        (dolist (r (three-points d 7d6))
          (let* ((term (third-body-term r d (cdr body)))
                 (exact (exact-third-body r d (cdr body)))
                 (exact-norm (rational-sqrt (reduce #'+ (mapcar #'* exact exact))))
                 (err (rational-sqrt (reduce #'+ (mapcar (lambda (x e) (expt (- (rational x) e) 2))
                                                           (coerce term 'list) exact)))))
            (true (< (/ err exact-norm) 1d-9) "~A at ~A" (car body) r)))))
    ;; :full is :j2 plus both terms.
    (let* ((r (lis::vec3 5d6 -4d6 2d6))
           (expected (gravity r :j2 time)))
      (lis::third-body-add! expected r (lis::body-state :moon time) lis::+mu-moon+)
      (lis::third-body-add! expected r (lis::body-state :sun time) lis::+mu-sun+)
      (is equalp expected (gravity r :full time)))))

;; The tidal term's next order is ±1.5 r/d, ~2.7 % at 7,000 km from Earth,
;; so the plan's 2 % is loosened to 3 r/d (~5.5 %).
(define-test gravity-moon-tidal
  (let* ((d (lis::body-state :moon (lis::start-t)))
         (dn (lis::vec3-norm d))
         (rn 7d6)
         (tidal (/ (* 2 lis::+mu-moon+ rn) (expt dn 3))))
    (dolist (r (subseq (three-points d rn) 0 2))
      (let ((term (third-body-term r d lis::+mu-moon+)))
        (true (< (relative-error (lis::vec3-norm term) tidal) (* 3 (/ rn dn))))
        ;; Away from the Earth along the line, both ways.
        (true (plusp (lis::vec3-dot term r)))))))

(defun power-of-two-p (x)
  (= 0.5d0 (decode-float x)))

(define-test coarse-step-rule
  (let* ((time (lis::start-t))
         (away-from-moon (lis::vec3-normalize (lis::vec3-scale (lis::body-state :moon time) -1d0)))
         (away-from-sun (lis::vec3-normalize (lis::vec3-scale (lis::body-state :sun time) -1d0)))
         (leo (lis::vec3 (+ lis::+radius-earth+ 400d3) 0 0)))
    (flet ((check (r &optional thrusting)
             (let ((h (lis::coarse-step r time thrusting)))
               (true (power-of-two-p h) "~A not a power of two" h)
               h)))
      (is = 2d0 (check leo))
      (is = 2d0 (lis::coarse-step leo 0d0 nil))
      (is = 512d0 (check (lis::vec3-scale away-from-moon 2d8)))
      (is = 1d0 (check leo t))
      (is = 1d0 (check (lis::vec3-scale away-from-moon 2d8) t))
      ;; At the surface the rule gives 2 s (√(R³/μ)/256 ≈ 3.2 s); the lower
      ;; clamp needs d < ~185 km from a centre.
      (is = 2d0 (check (lis::vec3 0 0 lis::+radius-earth+)))
      (is = (/ 1d0 64) (check (lis::vec3 1d5 0 0)))
      (is = (/ 1d0 64) (check (lis::vec3 0 0 0)))
      (is = 16384d0 (check (lis::vec3-scale away-from-sun 1d10)))
      (is = 1d0 (check (lis::vec3-scale away-from-sun 1d10) t)))))

(defun gravity-and-step-loop (a r time n)
  "N evaluations of :full gravity and the step rule; the sum of the steps."
  (declare (type lis::vec3 a r) (type double-float time) (type fixnum n))
  (let ((sum 0d0))
    (declare (type double-float sum))
    (dotimes (i n sum)
      (let ((time (+ time i)))
        (lis::gravity-into! a r time :full)
        (incf sum (lis::coarse-step r time nil))))))

(define-test gravity-and-step-cons-nothing
  (let ((a (lis::make-vec3))
        (r (lis::vec3 6.7d6 1d5 2d5))
        (time (lis::start-t)))
    ;; The first run fills the ephemeris cache.
    (is = 2000d0 (gravity-and-step-loop a r time 1000))
    ;; Measured before the check: Parachute conses its result first.
    (let ((consed (let ((before (sb-ext:get-bytes-consed)))
                    (gravity-and-step-loop a r time 1000)
                    (- (sb-ext:get-bytes-consed) before))))
      ;; The returned sum is one boxed double.
      (true (<= consed 16) "~D bytes" consed))))
