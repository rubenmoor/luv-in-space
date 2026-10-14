(in-package #:luv-in-space.tests)

(defun vec3-near (a b tolerance)
  (< (lis::vec3-norm (lis::vec3-sub a b)) tolerance))

(defun quat-near (a b tolerance)
  "Equal as rotations: q and -q are the same attitude."
  (flet ((off (sign) (loop for i below 4 maximize (abs (- (aref a i) (* sign (aref b i)))))))
    (< (min (off 1) (off -1)) tolerance)))

(defun random-unit-quat (state)
  (lis::quat-normalize
   (lis::quat (- (random 2d0 state) 1) (- (random 2d0 state) 1)
              (- (random 2d0 state) 1) (- (random 2d0 state) 1))))

(defun random-unit-vec3 (state)
  (lis::vec3-normalize
   (lis::vec3 (- (random 2d0 state) 1) (- (random 2d0 state) 1) (- (random 2d0 state) 1))))

(define-test quat-round-trip
  ;; body -> inertial -> body for random unit quaternions and vectors.
  ;; Loosened from 1e-15: two rotations of a unit vector reach 1.2e-15 per
  ;; component (~5 ulp), 1.6e-15 in norm.
  (dolist (seed '(1 2 3 5 8 13 21 34))
    (let ((state (sb-ext:seed-random-state seed)))
      (dotimes (i 50)
        (let* ((q (random-unit-quat state))
               (v (random-unit-vec3 state))
               (back (lis::quat-rotate (lis::quat-conjugate q) (lis::quat-rotate q v))))
          (true (vec3-near v back 2d-15)))))))

(defun integrate-rate (rate)
  "q from identity after 1 s at the constant body rate (p q r), RK4 at 1/64 s."
  (let ((q (lis::quat 1 0 0 0))
        (w (apply #'lis::vec3 rate))
        (h (/ 64d0)))
    (dotimes (i 64 q)
      (flet ((stage (base k s)
               (let ((x (lis::quat-copy base)))
                 (dotimes (j 4 x) (incf (aref x j) (* s (aref k j)))))))
        (let* ((k1 (lis::quat-derivative q w))
               (k2 (lis::quat-derivative (stage q k1 (/ h 2)) w))
               (k3 (lis::quat-derivative (stage q k2 (/ h 2)) w))
               (k4 (lis::quat-derivative (stage q k3 h) w)))
          (dotimes (j 4)
            (incf (aref q j) (* (/ h 6) (+ (aref k1 j) (* 2 (aref k2 j))
                                           (* 2 (aref k3 j)) (aref k4 j)))))
          (lis::quat-normalize! q q))))))

(define-test frd-rates
  ;; FRD: pitch turns the nose up (+x towards -z), yaw right (+x towards +y),
  ;; roll right wing down (+y towards +z).
  (let ((a 0.1d0) (x (lis::vec3 1 0 0)) (y (lis::vec3 0 1 0)))
    (true (vec3-near (lis::quat-rotate (integrate-rate (list 0 a 0)) x)
                     (lis::vec3 (cos a) 0 (- (sin a))) 1d-12))
    (true (vec3-near (lis::quat-rotate (integrate-rate (list 0 0 a)) x)
                     (lis::vec3 (cos a) (sin a) 0) 1d-12))
    (true (vec3-near (lis::quat-rotate (integrate-rate (list a 0 0)) y)
                     (lis::vec3 0 (cos a) (sin a)) 1d-12))
    (true (quat-near (integrate-rate (list 0 a 0))
                     (lis::quat-from-axis-angle y a) 1d-14))))

(define-test quat-slerp
  (let* ((z (lis::vec3 0 0 1))
         (a (lis::quat-from-axis-angle (lis::vec3 1 0 0) 0.3d0))
         (b (lis::quat-mul a (lis::quat-from-axis-angle z 1.2d0)))
         (half (lis::quat-mul a (lis::quat-from-axis-angle z 0.6d0)))
         (-b (map '(simple-array double-float (4)) #'- b)))
    (true (quat-near (lis::quat-slerp a b 0d0) a 1d-15))
    (true (quat-near (lis::quat-slerp a b 1d0) b 1d-15))
    (true (quat-near (lis::quat-slerp a b 0.5d0) half 1d-15))
    ;; Shortest arc: -b is the same attitude, so the midpoint is too.
    (true (quat-near (lis::quat-slerp a -b 0.5d0) half 1d-15))))

(define-test quat-matrix-and-basis
  (let ((state (sb-ext:seed-random-state 7)))
    (dotimes (i 20)
      (let* ((q (random-unit-quat state))
             (v (random-unit-vec3 state))
             (m (lis::mat3-from-quat q))
             (axes (loop for e in '((1 0 0) (0 1 0) (0 0 1))
                         collect (lis::quat-rotate q (apply #'lis::vec3 e)))))
        (true (vec3-near (lis::mat3*vec3 m v) (lis::quat-rotate q v) 1d-15))
        (true (quat-near (apply #'lis::quat-from-basis axes) q 1d-15))))))

(define-test axis-angle-pitch
  ;; The task's ./sly check: a positive rotation about +y turns +x towards -z.
  (let ((v (lis::quat-rotate (lis::quat-from-axis-angle (lis::vec3 0 1 0) 0.1d0)
                             (lis::vec3 1 0 0))))
    (true (minusp (aref v 2)))))

(define-test relative-single-subtracts-first
  ;; Two points 6.8e6 m out and 1.25 m apart: single spacing there is 0.5 m.
  (let ((p (lis::vec3 6800001.25d0 0 0))
        (origin (lis::vec3 6800000 0 0)))
    (is = 1.25 (aref (lis::relative-single p origin) 0))
    (isnt = 1.25 (- (coerce 6800001.25d0 'single-float) (coerce 6800000d0 'single-float)))
    (let ((out (make-array 6 :element-type 'single-float :initial-element 9.0)))
      (lis::relative-single p origin out 3)
      (is equalp #(9.0 9.0 9.0 1.25 0.0 0.0) out))))

(define-test symmetric-inverse
  ;; An inertia tensor diag(1, 2, 3) x 10^4 kg m^2 rotated 10 deg about y.
  (let* ((r (lis::mat3-from-quat (lis::quat-from-axis-angle (lis::vec3 0 1 0)
                                                            (* 10 (/ pi 180)))))
         (i (lis::mat3* (lis::mat3* r (lis::mat3 1d4 0 0 0 2d4 0 0 0 3d4))
                        (lis::mat3-transpose r)))
         (product (lis::mat3* (lis::mat3-inverse-symmetric i) i)))
    (dotimes (k 9)
      (true (< (abs (- (aref product k) (if (member k '(0 4 8)) 1 0))) 1d-15)))))
