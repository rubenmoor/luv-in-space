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

;;; The map view (decision 83: Map camera: framing on opening, focus, zoom,
;;; drag and exposure).

(defun start-orbit ()
  (destructuring-bind (&key a e i raan u) lis::*start-orbit*
    (values (lis::make-elliptic-orbit lis::+mu-earth+ a e (lis::deg->rad i)
                                      (lis::deg->rad raan) 0d0 (lis::deg->rad u) (lis::start-t))
            (lis::start-t)
            (* 2 pi (sqrt (/ (expt a 3) lis::+mu-earth+))))))

(defun test-orbit-400x2000 ()
  "A 400 × 2,000 km orbit, i 51.6°, periapsis at t0."
  (let* ((rp (+ lis::+radius-earth+ 400d3))
         (ra (+ lis::+radius-earth+ 2000d3)))
    (lis::make-elliptic-orbit lis::+mu-earth+ (/ (+ rp ra) 2) (/ (- ra rp) (+ ra rp))
                              (deg 51.6d0) (deg 30d0) (deg 60d0) 0d0 (lis::start-t))))

(defun orbit-render-state (orbit time)
  (multiple-value-bind (r v) (lis::orbit-state orbit time)
    (let ((state (ship-render-state r v)))
      (setf (lis::render-state-t state) (float time 1d0))
      state)))

(defun opened-map (state)
  (let ((view (lis::new-map-view)))
    (lis::map-view-open! view state)
    (values view (lis::map-camera-update! (lis::make-camera) view state nil))))

(defun segment-misses-sphere-p (a b radius)
  "Whether the segment from A to B stays outside the sphere of RADIUS at 0."
  (let* ((d (lis::vec3-sub b a))
         (s (max 0d0 (min 1d0 (/ (- (lis::vec3-dot a d)) (lis::vec3-dot d d))))))
    (> (lis::vec3-norm (lis::vec3-add-scaled a d s)) radius)))

(define-test map-opening
  (let ((test (test-orbit-400x2000)))
    (dolist (orbit (list (start-orbit) test))
      (let ((state (orbit-render-state orbit (+ (lis::start-t) 600))))
        (multiple-value-bind (view camera) (opened-map state)
          (let* ((offset (lis::camera-offset camera))
                 (n (lis::vec3-normalize (lis::vec3-cross (lis::render-state-r state)
                                                          (lis::render-state-v state))))
                 (q (lis::orbit-apoapsis-radius orbit)))
            (is eq :earth (lis::camera-anchor camera))
            (is = (* 50 (/ pi 180)) (lis::camera-field-of-view camera))
            (true (< (vec-angle (camera-axis camera 0) (lis::vec3-scale offset -1d0)) 1d-12))
            (true (plusp (lis::vec3-dot offset n)))
            (true (< (abs (- (asin (/ (lis::vec3-dot offset n) (lis::vec3-norm offset))) (deg 30d0)))
                     1d-12))
            (true (< (abs (- (/ (lis::map-view-distance view) (* 2.5d0 q)) 1)) 1d-9))
            ;; n's projection is up
            (true (minusp (lis::vec3-dot (camera-axis camera 2) n))))))))
  ;; The ship is never behind Earth's limb on the start orbit.
  (multiple-value-bind (orbit t0 period) (start-orbit)
    (dolist (u '(0 90 180 270))
      (let ((state (orbit-render-state orbit (+ t0 (* u 1/360 period)))))
        (true (segment-misses-sphere-p (lis::camera-offset (nth-value 1 (opened-map state)))
                                       (lis::render-state-r state) lis::+radius-earth+)
              "u ~A deg" u))))
  ;; A hyperbola opens at 2.5 |r|.
  (let* ((orbit (lis::make-hyperbolic-orbit lis::+mu-earth+ (+ lis::+radius-earth+ 400d3) 1.5d0
                                            (deg 20d0) 0d0 0d0 0d0 (lis::start-t)))
         (state (orbit-render-state orbit (+ (lis::start-t) 3600)))
         (view (opened-map state)))
    (true (< (abs (- (/ (lis::map-view-distance view)
                        (* 2.5d0 (lis::vec3-norm (lis::render-state-r state))))
                     1))
             1d-9))))

(define-test map-frame-stays
  (multiple-value-bind (orbit t0 period) (start-orbit)
    (let* ((view (opened-map (orbit-render-state orbit t0)))
           (camera (lis::make-camera))
           (first (lis::vec3-copy (lis::camera-offset
                                   (lis::map-camera-update! camera view (orbit-render-state orbit t0) nil)))))
      (dotimes (k 10)
        (lis::map-camera-update! camera view (orbit-render-state orbit (+ t0 (* k 1/40 period))) 0.016d0)
        (true (equalp first (lis::camera-offset camera)) "step ~A" k)))))

(define-test map-tab
  (let* ((state (orbit-render-state (test-orbit-400x2000) (lis::start-t)))
         (view (opened-map state))
         (az (lis::map-view-azimuth view))
         (el (lis::map-view-elevation view))
         (distance (lis::map-view-distance view)))
    (lis::map-view-cycle-focus! view)
    (is eq :ship (lis::map-view-focus view))
    (is = az (lis::map-view-azimuth view))
    (is = el (lis::map-view-elevation view))
    (is = distance (lis::map-view-distance view))
    (let* ((camera (lis::map-camera-update! (lis::make-camera) view state nil))
           (eye (lis::camera-eye camera (lis::camera-anchor-position! (lis::make-vec3) camera state)))
           (to-ship (lis::vec3-sub (lis::render-state-r state) eye)))
      (is eq :ship (lis::camera-anchor camera))
      (true (< (vec-angle (camera-axis camera 0) to-ship) 1d-12)))
    (lis::map-view-cycle-focus! view)
    (is eq :earth (lis::map-view-focus view))
    (lis::map-view-cycle-focus! view)
    (setf (lis::map-view-target view) 20d0 (lis::map-view-distance view) 20d0)
    (lis::map-view-cycle-focus! view)
    (is = (* 1.2d0 lis::+radius-earth+) (lis::map-view-target view))
    (is = (* 1.2d0 lis::+radius-earth+) (lis::map-view-distance view))))

(define-test map-zoom
  (flet ((view-at (target &optional (focus :earth))
           (let ((view (lis::new-map-view)))
             (setf (lis::map-view-focus view) focus
                   (lis::map-view-target view) target
                   (lis::map-view-distance view) target)
             view)))
    (let ((view (view-at 1d7)))
      (lis::map-view-zoom! view (expt 1.2d0 -1))
      (true (< (abs (- (lis::map-view-target view) 8333333.333d0)) 1d0)))
    (let ((view (view-at 1d7 :ship)))
      (lis::map-view-zoom! view 1/10)
      (true (< (abs (- (lis::map-view-target view) 1d6)) 1d-6)))
    (let ((view (view-at 1d7)))
      (lis::map-view-zoom! view 1/10)
      (is = (* 1.2d0 lis::+radius-earth+) (lis::map-view-target view))
      (true (< (abs (- (lis::map-view-target view) 7654d3)) 1d3)))
    (let ((view (view-at 1d7)))
      (dotimes (i 200) (lis::map-view-zoom! view 1.2d0))
      (is = 1.5d13 (lis::map-view-target view)))
    (let ((view (view-at 1d7 :ship)))
      (dotimes (i 100) (lis::map-view-zoom! view (/ 1.2d0)))
      (is = 50d0 (lis::map-view-target view)))
    ;; From 1.2 R⊕ to 100 AU: 80 notches (log(1.5e13 / 7.654e6) / log 1.2 ≈ 79.5).
    (let ((view (view-at (* 1.2d0 lis::+radius-earth+))))
      (is = 80 (loop for notches from 1
                     do (lis::map-view-zoom! view 1.2d0)
                     until (= (lis::map-view-target view) 1.5d13)
                     finally (return notches))))
    ;; The distance eases in log: after tau, 1 - 1/e of the way; nil snaps.
    (let ((view (view-at 1d7))
          (state (start-render-state))
          (camera (lis::make-camera)))
      (lis::map-view-zoom! view 10)
      (lis::map-camera-update! camera view state lis::*map-zoom-tau*)
      (true (< (abs (- (/ (log (/ (lis::map-view-distance view) 1d7)) (log 10d0))
                       (- 1 (exp -1d0))))
               1d-12))
      (lis::map-camera-update! camera view state nil)
      (is = 1d8 (lis::map-view-distance view)))))

(define-test map-drag-and-home
  (let* ((state (orbit-render-state (test-orbit-400x2000) (lis::start-t)))
         (view (opened-map state))
         (distance (lis::map-view-distance view)))
    (lis::map-view-drag! view 40d0 0d0)
    (true (< (abs (- (lis::map-view-azimuth view) (deg -55d0))) 1d-12))
    (lis::map-view-drag! view 0d0 -400d0)
    (true (< (abs (- (lis::map-view-elevation view) (deg 85d0))) 1d-12))
    (lis::map-view-zoom! view 10)
    (lis::map-view-cycle-focus! view)
    (lis::map-view-open! view state)
    (is eq :earth (lis::map-view-focus view))
    (true (< (abs (- (lis::map-view-azimuth view) (deg -45d0))) 1d-12))
    (true (< (abs (- (lis::map-view-elevation view) (deg 30d0))) 1d-12))
    (is = distance (lis::map-view-distance view))
    (is = distance (lis::map-view-target view))))

(define-test map-precision
  (let* ((state (start-render-state))
         (view (opened-map state))
         (camera (lis::make-camera))
         (single (make-array 3 :element-type 'single-float)))
    (flet ((eye ()
             (lis::camera-eye camera (lis::camera-anchor-position! (lis::make-vec3) camera state))))
      ;; Earth's centre from 100 AU
      (lis::map-view-zoom! view 1d10)
      (lis::map-camera-update! camera view state nil)
      (let* ((eye (eye))
             (double (lis::vec3-sub (lis::vec3 0 0 0) eye)))
        (lis::eye-relative-single (lis::vec3 0 0 0) eye single 0)
        (true (< (/ (loop for i below 3 maximize (abs (- (aref single i) (aref double i))))
                    (lis::vec3-norm double))
                 1d-6)))
      ;; The ship from 50 m
      (lis::map-view-cycle-focus! view)
      (lis::map-view-zoom! view 1d-20)
      (lis::map-camera-update! camera view state nil)
      (is = 50d0 (lis::map-view-distance view))
      (let* ((eye (eye))
             (r (lis::render-state-r state))
             (double (lis::vec3-sub r eye)))
        (lis::eye-relative-single r eye single 0)
        (true (< (loop for i below 3 maximize (abs (- (aref single i) (aref double i)))) 1d-4))))))

(define-test map-camera-update-conses-nothing
  (let* ((state (start-render-state))
         (view (opened-map state))
         (camera (lis::make-camera)))
    (is = 0 (least-bytes-consed
             (lambda () (dotimes (i 1000)
                          (lis::map-camera-update! camera view state 0.016d0)))))))
