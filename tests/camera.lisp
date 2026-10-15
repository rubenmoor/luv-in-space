(in-package #:luv-in-space.tests)

(defun turned-camera (&key (seed 7) (aspect 1.6))
  "A camera in a random orientation and its clip rows."
  (let* ((camera (lis::make-camera :orientation (random-unit-quat (sb-ext:seed-random-state seed))))
         (rows (make-array 16 :element-type 'single-float)))
    (values camera (lis::camera-clip-rows! rows camera aspect))))

(defun camera-axis (camera index)
  "The camera's FRD axis INDEX (0 forward, 1 right, 2 down) in ICRF."
  (let ((axis (lis::vec3 0 0 0)))
    (setf (aref axis index) 1d0)
    (lis::quat-rotate (lis::camera-orientation camera) axis)))

(defun clip (rows p)
  "Clip x y z w of the camera-relative point P (double), applied in single as
the vertex shader does."
  (let ((s (map 'list (lambda (x) (coerce x 'single-float)) p)))
    (loop for row below 4
          collect (+ (loop for k below 3 sum (* (aref rows (+ (* 4 row) k)) (nth k s)))
                     (aref rows (+ (* 4 row) 3))))))

(define-test clip-rows-depth
  ;; Reversed-Z, infinite far plane: depth = near / d.
  (multiple-value-bind (camera rows) (turned-camera)
    (dolist (d '(1d0 10d0 1d7))
      (destructuring-bind (x y z w) (clip rows (lis::vec3-scale (camera-axis camera 0) d))
        (declare (ignore x y))
        (true (< (abs (- (/ (/ z w) (/ 0.1d0 d)) 1)) 1d-6) "depth at ~A m" d)))))

(define-test clip-rows-signs
  ;; Vulkan: clip y points down, so a point below the view axis has clip y > 0.
  (multiple-value-bind (camera rows) (turned-camera)
    (let ((ahead (lis::vec3-scale (camera-axis camera 0) 10d0)))
      (true (plusp (first (clip rows (lis::vec3-add ahead (camera-axis camera 1))))))
      (true (plusp (second (clip rows (lis::vec3-add ahead (camera-axis camera 2))))))
      (true (minusp (second (clip rows (lis::vec3-sub ahead (camera-axis camera 2)))))))))

(define-test clip-rows-aspect
  (let ((square (nth-value 1 (turned-camera :aspect 1)))
        (wide (nth-value 1 (turned-camera :aspect 1.6))))
    (dotimes (i 16)
      (if (< i 4)
          (true (<= (abs (- (* 1.6d0 (aref wide i)) (aref square i)))
                    (* 1d-6 (abs (aref square i)))))
          (is = (aref square i) (aref wide i))))
    (true (notany (lambda (x) (or (sb-ext:float-nan-p x) (sb-ext:float-infinity-p x)))
                  wide))))

(defun offset-term-errors (altitude)
  "Relative errors of sphere-offset-term and of the single-float difference
against |c|^2 - R^2 in rationals, for C at ALTITUDE over Earth."
  (let* ((r lis::+radius-earth+)
         (c (lis::vec3-scale (lis::vec3-normalize (lis::vec3 0.3d0 -0.8d0 0.52d0))
                             (+ r altitude)))
         (exact (- (loop for x across c sum (expt (rational x) 2)) (expt (rational r) 2)))
         (cs (map 'list (lambda (x) (coerce x 'single-float)) c))
         (single (- (loop for x in cs sum (* x x)) (expt (coerce r 'single-float) 2))))
    (values (abs (/ (- (rational (lis::sphere-offset-term c r)) exact) exact))
            (abs (/ (- (rational single) exact) exact)))))

(define-test sphere-offset-term-precision
  (dolist (altitude '(400d3 1d3))
    (multiple-value-bind (double single) (offset-term-errors altitude)
      (true (< double 1d-12) "h (2r + h) at ~A m: ~A" altitude double)
      (true (> single 1d-12) "single difference at ~A m: ~A" altitude single))))

(define-test outside-camera-default-pose
  (let* ((r (lis::vec3 3d6 -4d6 4.5d6))
         (v (lis::vec3-scale (lis::vec3-normalize
                              (lis::vec3-cross (lis::vec3 0.1d0 0.2d0 1d0) r))
                             7.6d3))
         (camera (lis::outside-camera-default! (lis::make-camera) r v))
         ;; The ship relative to the eye; r - eye would add r's roundoff, ~1e-9 m.
         (offset (lis::camera-offset camera))
         (to-ship (lis::vec3-scale offset -1d0))
         (forward (camera-axis camera 0))
         (radial-out (lis::vec3-normalize
                      (lis::vec3-cross (lis::vec3-normalize v)
                                       (lis::vec3-normalize (lis::vec3-cross r v))))))
    (is = (* 50 (/ pi 180)) (lis::camera-field-of-view camera))
    (true (< (abs (- (lis::vec3-norm to-ship) 40d0)) 1d-12))
    (true (equalp (lis::camera-eye camera r) (lis::vec3-add r offset)))
    (true (< (atan (lis::vec3-norm (lis::vec3-cross forward to-ship))
                   (lis::vec3-dot forward to-ship))
             1d-12))
    ;; Radial out is above the view axis: against FRD down.
    (true (minusp (lis::vec3-dot radial-out (camera-axis camera 2))))
    ;; Behind the ship and raised by the elevation.
    (true (< (abs (- (asin (/ (lis::vec3-dot offset radial-out) 40d0))
                     (* 20 (/ pi 180))))
             1d-12))
    (true (minusp (lis::vec3-dot offset v)))))
