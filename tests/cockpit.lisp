(in-package #:luv-in-space.tests)

;;; cockpit.lisp: the cab's shell, fittings and light (decision 126: Cockpit
;;; structure and view in milestone 3: frames, draw order, Slug text, head look,
;;; exposure, cab light, portholes, bezels, tape labels). The GPU checks are in
;;; render.lisp.

(defun cab-distance-from-axis (p)
  "The distance of the body point P from the cab's axis."
  (let ((dy (- (aref p 1) (first lis::*cab-axis-offset*)))
        (dz (- (aref p 2) (second lis::*cab-axis-offset*))))
    (sqrt (+ (* dy dy) (* dz dz)))))

(defun cab-all-positions ()
  (lis::model-positions (lis::cab-model) :engine nil))

(define-test cab-shell-inside-the-lining
  ;; Every vertex within 1.40 m of the axis, along x within the cab, none
  ;; within 0.15 m of the eye; the frames and stringers inward of the lining.
  (let ((positions (cab-all-positions))
        (r lis::*cab-lining-radius*)
        (x0 (first lis::*cab-x-range*)) (x1 (second lis::*cab-x-range*)))
    (true (plusp (length positions)))
    (dolist (p positions)
      (true (<= (cab-distance-from-axis p) (+ r 1d-4)) "~A is ~,4F m from the axis" p (cab-distance-from-axis p))
      (true (<= (- x0 1d-4) (aref p 0) (+ x1 1d-4)) "~A outside the cab's length" p)
      (true (> (lis::vec3-norm p) lis::*cab-eye-clearance*) "~A within 0.15 m of the eye" p))
    (dolist (name '("frames" "stringers"))
      (dolist (p (lis::model-part-positions (lis::cab-model) name))
        (true (< (cab-distance-from-axis p) (- r 0.005)) "~A ~A not inward of the lining" name p)))))

(define-test cab-deck-height
  (let ((zs (mapcar (lambda (p) (aref p 2)) (lis::model-part-positions (lis::cab-model) "deck"))))
    (true (< (abs (- (reduce #'min zs) lis::*cab-deck-z*)) 1d-3) "the deck's top at ~F" (reduce #'min zs))))

(define-test cab-frames-and-stringers
  (let ((model (lis::cab-model)))
    (multiple-value-bind (where start end) (lis::model-part model "stringers")
      (declare (ignore where))
      (true (<= 8 (- end start) 10) "~D stringer segments (eight, one cut around the hatch)" (- end start)))
    (multiple-value-bind (where start end) (lis::model-part model "frames")
      (declare (ignore where))
      ;; Eight rings of 32 segments less the hatch's and the portholes'.
      (true (< 200 (- end start) 256) "~D frame segments" (- end start)))
    ;; Rings every 0.5 m from x 11.0 to 14.5.
    (is equalp '(-3.1d0 -2.6d0 -2.1d0 -1.6d0 -1.1d0 -0.6d0 -0.1d0 0.4d0) lis::*cab-ring-xs*)))

(define-test cab-opening-twin
  (let ((window-x (getf lis::*cab-window* :x)))
    ;; The window: 1.20 x 0.60 m in the front wall.
    (dolist (p '((0d0 0d0) (0.59d0 0.29d0) (-0.59d0 -0.29d0)))
      (true (lis::cab-opening-p (lis::vec3 window-x (first p) (second p))) "inside at ~A" p))
    (dolist (p '((0.601d0 0d0) (-0.601d0 0d0) (0d0 0.301d0) (0d0 -0.301d0)))
      (false (lis::cab-opening-p (lis::vec3 window-x (first p) (second p))) "outside at ~A" p))
    ;; Off the wall's plane: lining.
    (false (lis::cab-opening-p (lis::vec3 (- window-x 0.1d0) 0d0 0d0)))
    ;; The portholes: Ø 0.35 m round their inside centres.
    (dolist (side '(-1 1))
      (multiple-value-bind (centre axis) (lis::cab-porthole-geometry side)
        (let ((across (lis::vec3-normalize (lis::vec3-cross axis (lis::vec3 1d0 0d0 0d0))))
              (along (lis::vec3 1d0 0d0 0d0)))
          (dolist (direction (list across along))
            (true (lis::cab-opening-p (lis::vec3-add-scaled centre direction 0.174d0))
                  "side ~D inside" side)
            (false (lis::cab-opening-p (lis::vec3-add-scaled centre direction 0.176d0))
                   "side ~D 1 mm outside" side)))))))

(define-test cab-porthole-centres
  (dolist (side '(-1 1))
    (multiple-value-bind (centre axis) (lis::cab-porthole-geometry side)
      (true (< (abs (aref centre 0)) 1d-9))
      (true (< (abs (- (aref centre 1) (* side 1.118d0))) 1d-3))
      (true (< (abs (- (aref centre 2) 0.943d0)) 1d-3))
      ;; On the lining, the axis radial outward, 40.1° below horizontal from
      ;; the eye's side (decision 126: 37° from the cab's axis).
      (true (< (abs (- (cab-distance-from-axis centre) lis::*cab-lining-radius*)) 3d-3))
      (true (< (abs (- (lis::vec3-norm axis) 1)) 1d-9)))))

(define-test cab-hatch-where-decision-126-puts-it
  ;; 0.8 (x) x 1.2 m around the +y wall at x 11.4 (-2.7 from the eye), corners
  ;; r 0.2; a handwheel of Ø 0.4 on it.
  (let* ((positions (lis::model-part-positions (lis::cab-model) "hatch"))
         (xs (mapcar (lambda (p) (aref p 0)) positions))
         (angles (mapcar (lambda (p) (atan (- (aref p 2) (second lis::*cab-axis-offset*)) (aref p 1)))
                         positions)))
    (true (< (abs (- (reduce #'min xs) -3.1d0)) 1d-3))
    (true (< (abs (- (reduce #'max xs) -2.3d0)) 1d-3))
    (true (< (abs (- (reduce #'max angles) (/ 0.6d0 1.4d0))) 1d-3))
    (true (< (abs (- (reduce #'min angles) (- (/ 0.6d0 1.4d0)))) 1d-3))
    ;; Rounded corners: the rectangle's own corner (±0.4, ±0.6) is not on it.
    (false (find-if (lambda (p) (and (< (abs (- (aref p 0) -3.1d0)) 1d-3)
                                     (> (abs (atan (- (aref p 2) 0.1d0) (aref p 1))) (/ 0.59d0 1.4d0))))
                    positions))
    (multiple-value-bind (where start end) (lis::model-part (lis::cab-model) "handwheel")
      (declare (ignore where))
      (true (= 15 (- end start)) "ring, spokes and hub: ~D" (- end start)))
    (let ((wheel (lis::model-part-positions (lis::cab-model) "handwheel")))
      (true (< (abs (- (- (reduce #'max (mapcar (lambda (p) (aref p 0)) wheel))
                          (reduce #'min (mapcar (lambda (p) (aref p 0)) wheel)))
                       0.4d0))
               0.08d0)))))

(define-test cab-hatch-is-clear-of-frames
  ;; No ring segment or stringer crosses the hatch or a porthole.
  (dolist (name '("frames" "stringers"))
    (dolist (p (lis::model-part-positions (lis::cab-model) name))
      (let ((angle (atan (- (aref p 2) (second lis::*cab-axis-offset*)) (aref p 1))))
        (false (and (< (abs (- (aref p 0) -2.7d0)) 0.3d0) (< (abs angle) 0.2d0))
               "~A ~A in the hatch" name p)))))

(define-test cab-irradiance-at-the-button-block
  ;; A white 0.8 surface at the button block's centre facing up: radiance
  ;; 0.04 (red, the tint's strongest channel).
  (multiple-value-bind (r g b) (lis::cab-irradiance lis::*cab-reference-point* (lis::vec3 0d0 0d0 -1d0))
    (true (< (abs (- (* 0.8d0 r) 0.04d0)) (* 0.01d0 0.04d0)) "radiance ~F" (* 0.8d0 r))
    ;; Warm: green and blue weaker by the tint.
    (true (< (* 0.8d0 g) (* 0.8d0 r)))
    (true (< b g)))
  ;; Facing away from both strips only the ambient is left.
  (multiple-value-bind (r g b) (lis::cab-irradiance (lis::vec3 0d0 0d0 0d0) (lis::vec3 0d0 0d0 1d0))
    (true (< (abs (- r lis::*cab-ambient*)) 1d-12))
    (true (< (abs (- g lis::*cab-ambient*)) 1d-12))
    (true (< (abs (- b lis::*cab-ambient*)) 1d-12))))

(define-test cab-strips-in-the-model
  ;; The two strips' faces at the lining's height of decision 126, inside it.
  (let ((faces (lis::model-part-positions (lis::cab-model) "strips")))
    (true (plusp (length faces)))
    (dolist (p faces)
      (true (< (cab-distance-from-axis p) lis::*cab-lining-radius*) "~A" p)
      (true (<= -3.401d0 (aref p 0) 0.701d0) "~A" p))))

(define-test cab-size-budget
  ;; Plan 31: ≤ 30,000 vertices, ≤ 2,000 instances (the whole cab by its last piece).
  (true (<= (lis::cab-vertex-count) 30000) "~D vertices" (lis::cab-vertex-count))
  (true (<= (lis::cab-instance-count) 2000) "~D instances" (lis::cab-instance-count)))

(define-test cab-materials-agree-with-the-shader
  (is equal lis::*cab-material-names* luv-in-space.shaders::*cab-shader-material-rows*)
  (let ((n (length lis::*cab-material-names*))
        (model (lis::cab-model)))
    (is = n (array-dimension lis::*cab-materials* 0))
    (let ((hull (lis::model-vertices model)))
      (loop for o from 0 below (length hull) by lis::+vertex-floats+
            do (true (< -1 (round (aref hull (+ o 6))) n))))
    (dolist (g (lis::model-groups model))
      (let ((instances (lis::instance-group-instances g)))
        (dotimes (i (floor (length instances) lis::+instance-floats+))
          (true (< -1 (round (aref instances (+ 15 (* i lis::+instance-floats+)))) n)))))))

(define-test cab-block-rows-and-sun
  ;; The block's clip rows are cab-clip-rows!'s (the camera's orientation less
  ;; the ship's rotation); the Sun's direction is a unit vector in body axes
  ;; with (1 AU / d)^2 x the visible fraction; the static part is in place.
  (let* ((state (cockpit-render-state))
         (view (lis::new-cockpit-view))
         (camera (lis::make-camera))
         (gpu (lis::%make-cockpit-gpu))
         (rows (make-array 16 :element-type 'single-float))
         (eye (lis::vec3 0d0 0d0 0d0)))
    (lis::cab-block-static! (lis::cockpit-gpu-block gpu))
    (setf (lis::cockpit-view-yaw view) (deg 25) (lis::cockpit-view-pitch view) (deg -10))
    (lis::cockpit-camera-update! camera view state nil)
    (lis::camera-eye camera (lis::render-state-r state) eye)
    (let ((block (lis::cab-block! gpu state camera '(1280 800) 4d0 eye)))
      (lis::cab-clip-rows! rows view (/ 1280 800))
      (dotimes (i 16)
        (true (< (abs (- (aref block i) (aref rows i))) 2d-6) "row element ~D: ~A ~A" i (aref block i) (aref rows i)))
      (true (< (abs (- (aref block 16) (/ 2 1280))) 1d-9))
      (true (< (abs (- (aref block 20) 0.25)) 1d-9) "1 / exposure")
      (true (< (abs (- (aref block 22) lis::*cab-ambient*)) 1d-9))
      (let ((sun (lis::vec3 (aref block 24) (aref block 25) (aref block 26))))
        (true (< (abs (- (lis::vec3-norm sun) 1)) 1d-6))
        ;; In the world it points at the Sun.
        (let ((world (lis::quat-rotate (lis::render-state-q state) sun))
              (to-sun (lis::vec3-sub (lis::render-state-sun state) (lis::render-state-r state))))
          (true (> (lis::vec3-dot world (lis::vec3-normalize to-sun)) (- 1 1d-6)))
          (let ((k (/ lis::+au+ (lis::vec3-norm to-sun))))
            (true (< (abs (- (aref block 27) (* k k (lis::sun-visible-fraction eye (lis::render-state-t state)))))
                     1d-5)))))
      ;; The static part: the strips, the light, the window, the portholes.
      (true (< (abs (- (aref block 30) (+ -0.40 0))) 1d-6) "strip a's y")
      (true (< (abs (- (aref block 31) lis::*cab-strip-z*)) 1d-6))
      (true (< (abs (- (aref block 39) lis::*cab-led-level*)) 1d-6))
      (true (< (abs (- (aref block 41) 0.6)) 1d-6))
      (true (< (abs (- (aref block 47) lis::*cab-porthole-radius*)) 1d-6)))))

(define-test cab-block-conses-nothing
  (let* ((state (cockpit-render-state))
         (view (lis::new-cockpit-view))
         (camera (lis::make-camera))
         (gpu (lis::%make-cockpit-gpu))
         (eye (lis::vec3 0d0 0d0 0d0))
         (extent (list 1280 800)))
    (lis::cockpit-camera-update! camera view state nil)
    (lis::camera-eye camera (lis::render-state-r state) eye)
    (lis::cab-block! gpu state camera extent 4d0 eye)
    (lis::cab-materials! (lis::cockpit-gpu-materials gpu))
    ;; The CPU side of encode-cockpit: the block and the material table.
    (is = 0 (least-bytes-consed (lambda ()
                                  (dotimes (i 50)
                                    (lis::cab-block! gpu state camera extent 4d0 eye)
                                    (lis::cab-materials! (lis::cockpit-gpu-materials gpu))))))))

(define-test cab-block-time
  ;; The CPU's share of encode-cockpit: the block per frame, well inside the
  ;; 0.3 ms of plan 31 (the draw calls themselves are measured on the GPU).
  (let* ((state (cockpit-render-state))
         (view (lis::new-cockpit-view))
         (camera (lis::make-camera))
         (gpu (lis::%make-cockpit-gpu))
         (eye (lis::vec3 0d0 0d0 0d0))
         (extent (list 1280 800))
         (n 2000))
    (lis::cockpit-camera-update! camera view state nil)
    (lis::camera-eye camera (lis::render-state-r state) eye)
    (let ((start (get-internal-real-time)))
      (dotimes (i n) (lis::cab-block! gpu state camera extent 4d0 eye))
      (let ((ms (/ (* 1000d0 (- (get-internal-real-time) start)) internal-time-units-per-second n)))
        (true (< ms 0.1d0) "~,4F ms per block" ms)))))

;;; Window frame, glass, cross, porthole rings and name plate (decision 126).

(defun part-extent (name axis)
  "(values min max) of the part NAME's vertices along AXIS."
  (let ((values (mapcar (lambda (p) (aref p axis)) (lis::model-part-positions (lis::cab-model) name))))
    (values (reduce #'min values) (reduce #'max values))))

(define-test cab-window-frame-dimensions
  ;; Inner 1.20 x 0.60, outer 1.30 x 0.70 within 1 mm, standing 50 mm off the wall.
  (multiple-value-bind (y0 y1) (part-extent "window-frame" 1)
    (true (< (abs (- (- y1 y0) 1.30d0)) 1d-3) "outer width ~F" (- y1 y0)))
  (multiple-value-bind (z0 z1) (part-extent "window-frame" 2)
    (true (< (abs (- (- z1 z0) 0.70d0)) 1d-3) "outer height ~F" (- z1 z0)))
  (multiple-value-bind (x0 x1) (part-extent "window-frame" 0)
    (true (< (abs (- x1 0.90d0)) 1d-3))
    (true (< (abs (- x0 0.85d0)) 1d-3)))
  ;; The opening: no vertex of the frame inside 1.20 x 0.60 (but on its edge).
  (dolist (p (lis::model-part-positions (lis::cab-model) "window-frame"))
    (true (or (>= (abs (aref p 1)) (- 0.60d0 1d-3)) (>= (abs (aref p 2)) (- 0.30d0 1d-3)))
          "~A inside the opening" p)))

(define-test cab-window-bolts
  (let ((bolts (lis::cab-window-bolt-positions)))
    (is = 36 (length bolts))
    (is = 36 (multiple-value-bind (where start end) (lis::model-part (lis::cab-model) "window-bolts")
               (declare (ignore where))
               (- end start)))
    ;; Along the top bar: 0.1 m apart.
    (let ((top (sort (loop for (y z) in bolts when (< z -0.3d0) collect y) #'<)))
      (is = 13 (length top))
      (loop for (a b) on top while b do (true (< (abs (- (- b a) 0.1d0)) 1d-9))))))

(define-test cab-boresight-cross-centre
  ;; Two strips 0.10 x 0.012 m, crossing at the window's centre on the nose line.
  (multiple-value-bind (y0 y1) (part-extent "cross" 1)
    (true (< (abs (+ y0 y1)) 1d-6)) (true (< (abs (- (- y1 y0) 0.10d0)) 1d-6)))
  (multiple-value-bind (z0 z1) (part-extent "cross" 2)
    (true (< (abs (+ z0 z1)) 1d-6)) (true (< (abs (- (- z1 z0) 0.10d0)) 1d-6)))
  (multiple-value-bind (x0 x1) (part-extent "cross" 0)
    (declare (ignore x0))
    (true (< x1 0.90d0))))

(define-test cab-porthole-rings-on-the-axes
  ;; Each ring is centred on the porthole's inside centre (within 1 mm) and
  ;; stands in the plane across the porthole's axis.
  (let ((positions (lis::model-part-positions (lis::cab-model) "porthole-rings")))
    (dolist (side '(-1 1))
      (multiple-value-bind (centre axis) (lis::cab-porthole-geometry side)
        (let ((mine (remove-if-not (lambda (p) (plusp (* side (aref p 1)))) positions))
              (sum (lis::vec3 0d0 0d0 0d0)))
          (is = (* 12 36) (length mine))
          (dolist (p mine) (setf sum (lis::vec3-add sum p)))
          (let* ((mean (lis::vec3-scale sum (/ 1d0 (length mine))))
                 (off (lis::vec3-sub mean centre))
                 (along (lis::vec3-dot off axis)))
            ;; The mean is the ring's centre pushed 35 mm in along the axis.
            (true (< (abs (+ along 0.035d0)) 1d-3) "along the axis ~F" along)
            (true (< (lis::vec3-norm (lis::vec3-sub off (lis::vec3-scale axis along))) 1d-3)
                  "off the axis ~A" off)))))))

(define-test cab-glass-twin
  ;; Transmission 0.95 at the centre falling to 0.88 at the edges; the colour
  ;; is premultiplied (the reflection scaled by alpha) and faintly green.
  (is = 0.95d0 (lis::cab-glass-transmission 0d0))
  (true (< (abs (- (lis::cab-glass-transmission 1d0) 0.88d0)) 1d-12))
  (true (< (abs (- (lis::cab-glass-transmission 3d0) 0.88d0)) 1d-12) "clamped")
  (multiple-value-bind (r g b a)
      (lis::cab-glass-colour (lis::vec3 0.9d0 0.3d0 0.15d0) (lis::vec3 -1d0 0d0 0d0) 0d0)
    (true (< (abs (- a 0.05d0)) 1d-12))
    (true (< 0 r)) (true (< 0 b))
    ;; Warm light, faint green tint: green over red relative to the light's own.
    (multiple-value-bind (lr lg) (lis::cab-irradiance (lis::vec3 0.9d0 0.3d0 0.15d0) (lis::vec3 -1d0 0d0 0d0))
      (true (> (/ g r) (/ lg lr))))
    (true (< b g))))

(define-test cab-glass-covers-the-openings
  ;; The glass's vertices: the window quad in the front wall's plane and
  ;; two discs of the portholes' radius.
  (let* ((model (lis::cab-glass-model)) (hull (lis::model-vertices model)))
    (is = (+ 6 (* 2 24 3)) (lis::cab-vertex-count model))
    (loop for o from 0 below (* 6 lis::+vertex-floats+) by lis::+vertex-floats+
          do (true (< (abs (- (aref hull o) 0.9)) 1e-6))
             (true (<= (abs (aref hull (+ o 1))) 0.6001))
             (true (<= (abs (aref hull (+ o 2))) 0.3001)))
    (dolist (side '(-1 1))
      (multiple-value-bind (centre axis) (lis::cab-porthole-geometry side)
        (dolist (p (lis::model-part-positions model (format nil "porthole-glass-~D" side)))
          (let ((d (lis::vec3-sub p centre)))
            (true (< (abs (lis::vec3-dot d axis)) 1d-5))
            (true (<= (lis::vec3-norm d) (+ lis::*cab-porthole-radius* 1d-5)))))))))

(define-test cab-plate-text-centred
  ;; The run's cells are centred on the plate (within 1 mm) at cap 30 mm; the
  ;; plate is the steel box 0.40 x 0.07 m centred (0.89, 0, -0.43).
  (multiple-value-bind (origin cell columns) (lis::cab-plate-geometry)
    (is = 14 columns)
    (let ((middle-y (+ (second origin) (* 0.5d0 columns cell)))
          (middle-z (- (third origin) (* 0.5d0 (getf lis::*cab-plate* :cap)))))
      (true (< (abs middle-y) 1d-3) "run centre y ~F" middle-y)
      (true (< (abs (- middle-z -0.43d0)) 1d-3) "run centre z ~F" middle-z))
    (true (< (* columns cell) 0.40d0) "the run fits the plate")
    (true (< (first origin) 0.88d0) "in front of the plate's face"))
  (multiple-value-bind (y0 y1) (part-extent "plate" 1)
    (true (< (abs (- (- y1 y0) 0.40d0)) 1d-3)))
  (multiple-value-bind (z0 z1) (part-extent "plate" 2)
    (true (< (abs (- (+ z0 z1) -0.86d0)) 1d-3))
    (true (< (abs (- (- z1 z0) 0.07d0)) 1d-3))))
