(in-package #:luv-in-space.tests)

;;; impostors.lisp: the impostor's quad, its draw form and the precision of
;;; its depth (docs/architecture.md "Rendering precision"). The GPU checks
;;; are in tests/render.lisp.

(defun impostor-block (centre radius &optional (camera (look-along! (lis::make-camera) centre)))
  "The block impostor-params! writes for a sphere at CENTRE from the eye,
and its draw form."
  (let* ((out (make-array lis::+impostor-floats+ :element-type 'single-float))
         (form (lis::impostor-params! out 0 centre radius (lis::sphere-offset-term centre radius)
                                      camera)))
    (values out form)))

(defun block-vec3 (out index)
  (lis::vec3 (aref out index) (aref out (+ index 1)) (aref out (+ index 2))))

(defun quad-coordinates (out direction)
  "Where the ray along DIRECTION meets the quad's plane, in units of its
axes u and v: inside the quad when both are within [-1, 1]."
  (let* ((c (lis::vec3-normalize (block-vec3 out 0)))
         (d (lis::vec3-norm (block-vec3 out 0)))
         (point (lis::vec3-scale direction (/ d (lis::vec3-dot (lis::vec3-normalize direction) c)
                                              (lis::vec3-norm direction))))
         (offset (lis::vec3-sub point (block-vec3 out 0))))
    (flet ((along (index)
             (let ((axis (block-vec3 out index)))
               (/ (lis::vec3-dot offset axis) (lis::vec3-dot axis axis)))))
      (values (along 4) (along 8)))))

(defun tangent-directions (centre radius)
  "The four rays from the eye that graze the sphere, along its quad's axes."
  (let* ((d (lis::vec3-norm centre))
         (rho (asin (/ radius d)))
         (c (lis::vec3-normalize centre))
         (u (lis::vec3-normalize (lis::vec3-cross c (lis::vec3 0 0 1))))
         (v (lis::vec3-cross c u)))
    (loop for axis in (list u v (lis::vec3-scale u -1d0) (lis::vec3-scale v -1d0))
          collect (lis::vec3-add (lis::vec3-scale c (cos rho)) (lis::vec3-scale axis (sin rho))))))

(define-test impostor-quad-covers-the-disc
  (let* ((state (sim-render-state (lis::new-game-sim)))
         (eye (lis::render-state-r state)))
    (loop for (body radius) in (list (list (lis::render-state-moon state) lis::+radius-moon+)
                                     (list (lis::render-state-sun state) lis::+radius-sun+))
          for centre = (lis::vec3-sub body eye)
          do (multiple-value-bind (out form) (impostor-block centre radius)
               (is eq :quad form)
               (dolist (direction (tangent-directions centre radius))
                 (multiple-value-bind (x y) (quad-coordinates out direction)
                   (true (and (<= (abs x) 1.000001d0) (<= (abs y) 1.000001d0))
                         "tangent ray at ~F, ~F" x y)
                   ;; On the quad's inscribed circle: no larger than it needs to be.
                   (true (< (abs (- (+ (* x x) (* y y)) 1)) 1d-4))))))))

(define-test impostor-draw-forms
  (let ((r lis::+radius-earth+))
    ;; Earth from 400 km: 70 deg wide.
    (is eq :full-screen (nth-value 1 (impostor-block (lis::vec3 (- (+ r 400d3)) 0 0) r)))
    ;; The eye inside the halo, R + 100 km, at 50 km.
    (is eq :full-screen (nth-value 1 (impostor-block (lis::vec3 (- (+ r 50d3)) 0 0) (+ r 100d3))))
    ;; The Moon behind the camera.
    (let ((camera (look-along! (lis::make-camera) (lis::vec3 1 0 0))))
      (is eq nil (nth-value 1 (impostor-block (lis::vec3 -3.8d8 0 0) lis::+radius-moon+ camera))))))

(define-test impostor-depth-precision
  ;; The shader's arithmetic in single floats at the disc's centre: the
  ;; stable root gives near / (d - r) within 1e-6.
  (let* ((state (sim-render-state (lis::new-game-sim)))
         (eye (lis::render-state-r state)))
    (loop for (body radius) in (list (list (lis::render-state-moon state) lis::+radius-moon+)
                                     (list (lis::render-state-sun state) lis::+radius-sun+)
                                     (list (lis::vec3 0 0 0) lis::+radius-earth+))
          for centre = (lis::vec3-sub body eye)
          do (let* ((out (impostor-block centre radius))
                    (c (list (aref out 0) (aref out 1) (aref out 2)))
                    (length (sqrt (reduce #'+ (mapcar #'* c c))))
                    (ray (mapcar (lambda (x) (/ x length)) c))
                    (b (reduce #'+ (mapcar #'* ray c)))
                    (q (mapcar (lambda (d x) (- (* d b) x)) ray c))
                    (h2 (- (* (aref out 3) (aref out 3)) (reduce #'+ (mapcar #'* q q))))
                    (tt (/ (aref out 7) (+ b (sqrt (max h2 0.0)))))
                    (depth (/ 0.1 tt))
                    (expected (/ 0.1d0 (- (lis::vec3-norm centre) radius))))
               (true (< (abs (- (/ depth expected) 1)) 1d-6)
                     "depth ~A, expected ~A" depth expected)))))
