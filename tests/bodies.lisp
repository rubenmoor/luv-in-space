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
