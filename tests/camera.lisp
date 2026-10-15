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

;;; The outside view (decision 82: Outside view camera: angles in a frame,
;;; toggles that keep the picture, distance and easing).

(defun deg (x) (* x (/ pi 180)))

(defun vec-angle (a b)
  (atan (lis::vec3-norm (lis::vec3-cross a b)) (lis::vec3-dot a b)))

(defun ship-render-state (r v &optional (q (lis::quat 1 0 0 0)))
  (let ((state (lis::make-render-state)))
    (replace (lis::render-state-r state) r)
    (replace (lis::render-state-v state) v)
    (replace (lis::render-state-q state) q)
    state))

(defun start-render-state (&optional (sim (lis::new-game-sim)))
  (lis::render-state-update! (lis::make-render-state) (lis::new-clock sim) sim))

(defun yawed (state degrees)
  "STATE with the ship turned DEGREES about its body z."
  (let ((q (lis::quat-mul (lis::render-state-q state)
                          (lis::quat-from-axis-angle (lis::vec3 0 0 1) (deg degrees)))))
    (replace (lis::render-state-q state) q)
    state))

(defun outside-camera (view state &optional elapsed)
  (lis::outside-camera-update! (lis::make-camera) view state elapsed))

(defun former-default-offset-forward (r v)
  "Part 4's outside-camera-default!: the eye offset and the view axis."
  (let* ((p (lis::vec3-normalize v))
         (n (lis::vec3-normalize (lis::vec3-cross r v)))
         (o (lis::vec3-cross p n))
         (e (deg 20d0))
         (x (lis::vec3-add-scaled (lis::vec3-scale p (cos e)) o (- (sin e)))))
    (values (lis::vec3-scale x -40d0) x)))

(defun axes-angle (a b)
  "The largest angle between the cameras' corresponding FRD axes."
  (loop for i below 3 maximize (vec-angle (camera-axis a i) (camera-axis b i))))

(define-test outside-default-pose
  (let* ((r (lis::vec3 3d6 -4d6 4.5d6))
         (v (lis::vec3-scale (lis::vec3-normalize
                              (lis::vec3-cross (lis::vec3 0.1d0 0.2d0 1d0) r))
                             7.6d3))
         (camera (outside-camera (lis::new-outside-view) (ship-render-state r v))))
    (multiple-value-bind (offset forward) (former-default-offset-forward r v)
      (is = (* 50 (/ pi 180)) (lis::camera-field-of-view camera))
      (true (< (lis::vec3-norm (lis::vec3-sub offset (lis::camera-offset camera))) 1d-9))
      (true (< (vec-angle forward (camera-axis camera 0)) 1d-12))
      ;; radial out above the view axis
      (true (minusp (lis::vec3-dot r (camera-axis camera 2)))))))

(define-test outside-looks-at-com
  (let ((random (sb-ext:seed-random-state 11))
        (state (start-render-state)))
    (dotimes (i 20)
      (let ((view (lis::new-outside-view)))
        (replace (lis::render-state-q state) (random-unit-quat random))
        (setf (lis::outside-view-frame view) (if (evenp i) :orbital :locked)
              (lis::outside-view-azimuth view) (random (* 2 pi) random)
              (lis::outside-view-elevation view) (- (random (deg 170d0) random) (deg 85d0))
              (lis::outside-view-target view) (+ 15d0 (random 4985d0 random)))
        (let* ((camera (outside-camera view state))
               (to-com (lis::vec3-scale (lis::camera-offset camera) -1d0)))
          (true (< (abs (- (lis::vec3-norm to-com) (lis::outside-view-target view))) 1d-9))
          (true (< (vec-angle (camera-axis camera 0) to-com) 1d-12)))))))

(define-test outside-orbital-frame
  ;; Over a quarter of the start orbit the eye keeps az 180, el 20 in the
  ;; orbital frame, and radial out stays in the view's vertical plane.
  (destructuring-bind (&key a e i raan u) lis::*start-orbit*
    (let* ((t0 (lis::start-t))
           (orbit (lis::make-elliptic-orbit lis::+mu-earth+ a e (lis::deg->rad i)
                                            (lis::deg->rad raan) 0d0 (lis::deg->rad u) t0))
           (period (* 2 pi (sqrt (/ (expt a 3) lis::+mu-earth+))))
           (view (lis::new-outside-view)))
      (dotimes (k 10)
        (multiple-value-bind (r v) (lis::orbit-state orbit (+ t0 (* k 1/36 period)))
          (let* ((camera (outside-camera view (ship-render-state r v)))
                 (offset (lis::camera-offset camera))
                 (p (lis::vec3-normalize v))
                 (out (lis::vec3-normalize (lis::vec3-add-scaled r p (- (lis::vec3-dot r p)))))
                 (normal (lis::vec3-normalize (lis::vec3-cross r (camera-axis camera 0))))
                 (up (lis::vec3-scale (camera-axis camera 2) -1d0)))
            (true (< (abs (- (lis::vec3-dot offset p) (* -40 (cos (deg 20d0))))) 1d-9))
            (true (< (abs (- (lis::vec3-dot offset out) (* 40 (sin (deg 20d0))))) 1d-9))
            (true (< (abs (asin (lis::vec3-dot up normal))) 1d-12))
            (true (plusp (lis::vec3-dot up r)))))))))

(define-test outside-locked-frame
  (let* ((state (start-render-state))
         (view (lis::new-outside-view)))
    (setf (lis::outside-view-frame view) :locked)
    (let* ((before (lis::vec3-copy (lis::camera-offset (outside-camera view state))))
           (axis (lis::quat-rotate (lis::render-state-q state) (lis::vec3 0 0 1)))
           (after (lis::camera-offset (outside-camera view (yawed state 90))))
           (expected (lis::quat-rotate (lis::quat-from-axis-angle axis (deg 90d0)) before)))
      (true (< (vec-angle expected after) 1d-12)))))

(define-test outside-toggle-keeps-picture
  (dolist (yaw '(0 30))
    (let* ((state (yawed (start-render-state) yaw))
           (view (lis::new-outside-view))
           (before (outside-camera view state)))
      (lis::outside-view-toggle-frame! view state)
      (is eq :locked (lis::outside-view-frame view))
      (let ((after (outside-camera view state)))
        (true (< (lis::vec3-norm (lis::vec3-sub (lis::camera-offset before)
                                                (lis::camera-offset after)))
                 1d-9)
              "offset after a ~A deg yaw" yaw)
        (true (< (axes-angle before after) 1d-12) "orientation after a ~A deg yaw" yaw))
      (lis::outside-view-toggle-frame! view state)
      (is eq :orbital (lis::outside-view-frame view))
      (true (< (abs (- (lis::outside-view-azimuth view) pi)) 1d-12))
      (true (< (abs (- (lis::outside-view-elevation view) (deg 20d0))) 1d-12)))))

(define-test outside-drag
  (let ((view (lis::new-outside-view)))
    (lis::outside-view-drag! view 40d0 0d0)
    (true (< (abs (- (lis::outside-view-azimuth view) (deg 170d0))) 1d-12))
    (lis::outside-view-drag! view 0d0 -400d0)
    (true (< (abs (- (lis::outside-view-elevation view) (deg 85d0))) 1d-12)))
  ;; From behind, mouse right moves the eye towards the frame's +y, mouse up raises it.
  (let ((view (lis::new-outside-view))
        (d (lis::make-vec3)))
    (lis::outside-view-drag! view 10d0 0d0)
    (lis::outside-direction! d (lis::outside-view-azimuth view) (lis::outside-view-elevation view))
    (true (plusp (aref d 1)))
    (let ((z (aref d 2)))
      (lis::outside-view-drag! view 0d0 -10d0)
      (lis::outside-direction! d (lis::outside-view-azimuth view) (lis::outside-view-elevation view))
      (true (< (aref d 2) z)))))

(defun inside-part-box-p (p)
  "Whether P (body, from R) is inside a part's box in *ship-parts*."
  (loop for (nil nil centres size) in lis::*ship-parts*
          thereis (loop for c in centres
                          thereis (loop for k below 3
                                        always (< (abs (- (aref p k) (nth k c)))
                                                  (/ (nth k size) 2))))))

(define-test outside-wheel
  (let ((view (lis::new-outside-view))
        (wet lis::*propellant-full*))
    (lis::outside-view-zoom! view 1 wet)
    (true (< (abs (- (lis::outside-view-target view) (/ 40d0 1.2d0))) 1d-12))
    (lis::outside-view-zoom! view 100 wet)
    (let ((min (lis::outside-min-distance wet))
          (com (lis::mass-properties-com (lis::ship-mass-properties wet)))
          (d (lis::make-vec3)))
      (is = min (lis::outside-view-target view))
      (true (>= min 12d0))
      (loop for az from 0 below 360 by 10
            do (loop for el from -90 to 90 by 10
                     do (lis::outside-direction! d (deg (float az 1d0)) (deg (float el 1d0)))
                        (false (inside-part-box-p (lis::vec3-add-scaled com d min))
                               "eye at az ~A el ~A" az el))))
    (lis::outside-view-zoom! view -100 wet)
    (is = 5000d0 (lis::outside-view-target view)))
  ;; The distance eases in log: after tau, 1 - 1/e of the way; nil snaps.
  (let ((view (lis::new-outside-view))
        (state (start-render-state)))
    (lis::outside-view-zoom! view 3 lis::*propellant-full*)
    (outside-camera view state lis::*outside-zoom-tau*)
    (true (< (abs (- (/ (log (/ (lis::outside-view-distance view) 40d0))
                        (log (/ (lis::outside-view-target view) 40d0)))
                     (- 1 (exp -1d0))))
             1d-12))
    (outside-camera view state nil)
    (is = (lis::outside-view-target view) (lis::outside-view-distance view))))

(define-test outside-min-distance-measured
  ;; Measured: wet 14.6446 m, half 13.5826 m, dry 13.6187 m.
  (loop for (m-p expected) in `((,lis::*propellant-full* 14.6446d0)
                                (,(/ lis::*propellant-full* 2) 13.5826d0)
                                (0d0 13.6187d0))
        do (true (< (abs (- (lis::outside-min-distance m-p) expected)) 1d-4)
                 "~A kg: ~A m" m-p (lis::outside-min-distance m-p))))

(define-test outside-camera-update-conses-nothing
  (let ((view (lis::new-outside-view))
        (state (start-render-state))
        (camera (lis::make-camera)))
    (is = 0 (least-bytes-consed
             (lambda () (dotimes (i 1000)
                          (lis::outside-camera-update! camera view state 0.016d0)))))))
