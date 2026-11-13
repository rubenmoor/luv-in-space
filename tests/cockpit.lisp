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
  "Every vertex of the cab's shell and fittings (the console's parts apart, which
the console's tests check: panels on the wall reach behind it)."
  (loop for part in (lis::model-parts (lis::cab-model))
        unless (member (first part) lis::*cab-console-parts* :test #'string=)
          append (lis::model-part-positions (lis::cab-model) (first part))))

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

;;; The console (plan 31 piece 5; decision 126): the panel table, faces,
;;; bezels, brackets, cables, labels.

(defparameter *layout-table*
  ;; name (dx dy dz) (w h) grid, docs/design.md "Layout".
  '((:caution-lamps (0.90 -0.72 0) (0.08 0.58) nil)
    (:status-lamps (0.90 0.72 0) (0.08 0.58) nil)
    (:orbit (0.60 -0.28 0.35) (0.30 0.20) (24 8))
    (:attitude (0.67 0 0.29) (0.24 0.20) (19 8))
    (:target (0.60 0.28 0.35) (0.30 0.20) (24 8))
    (:strip (0.55 0 0.455) (0.30 0.08) (19 3))
    (:buttons (0.46 0 0.52) (0.26 0.13) nil)
    (:autopilot (0.42 -0.30 0.50) (0.26 0.19) (22 8))
    (:ship (0.42 0.30 0.50) (0.26 0.19) (22 8))
    (:comms (0.34 -0.60 0.12) (0.30 0.24) (26 11))
    (:throttle (0.17 -0.24 0.52) (0.14 0.16) nil)
    (:hull-gauge (0.17 0.24 0.52) (0.12 0.12) nil)))

(define-test console-panel-table
  (is = (length *layout-table*) (length lis::*cockpit-panels*))
  (loop for (name centre size grid) in *layout-table*
        do (let ((panel (lis::cockpit-panel name)))
             (loop for a in centre for b in (getf panel :centre)
                   do (true (< (abs (- a b)) 1d-6) "~A centre" name))
             (loop for a in size for b in (getf panel :size)
                   do (true (< (abs (- a b)) 1d-6) "~A size" name))
             (is equal grid (getf panel :grid)))))

(defun panel-bezel-corners (panel)
  (multiple-value-bind (c n r u hn hr hu) (lis::panel-bezel-box panel)
    (loop for sn in '(-1 1) append
          (loop for sr in '(-1 1) append
                (loop for su in '(-1 1)
                      collect (lis::vlin c (* sn hn) n (* sr hr) r (* su hu) u))))))

(define-test console-inside-the-cab
  (dolist (panel lis::*cockpit-panels*)
    (dolist (p (panel-bezel-corners panel))
      (true (<= (cab-distance-from-axis p) lis::*cab-lining-radius*) "~A bezel corner ~A" (getf panel :name) p)
      (true (< (aref p 2) lis::*cab-deck-z*) "~A above the deck" (getf panel :name)))))

(defun panel-angle-between (a b)
  (/ (* 180 (acos (max -1d0 (min 1d0 (lis::vec3-dot a b))))) pi))

(defun orthonormal-p (n r u)
  (and (< (abs (lis::vec3-dot n r)) 1d-9) (< (abs (lis::vec3-dot n u)) 1d-9)
       (< (abs (lis::vec3-dot r u)) 1d-9) (< (abs (- (lis::vec3-norm r) 1)) 1d-9)))

(define-test console-faces-turn-to-the-eye
  (dolist (panel lis::*cockpit-panels*)
    (multiple-value-bind (c n r u) (lis::panel-frame panel)
      (true (orthonormal-p n r u))
      (let ((turn (getf panel :turn)))
        (if turn
            (true (< (abs (- (panel-angle-between n (lis::vec3 -1d0 0d0 0d0)) (abs turn))) 1d-6)
                  "~A turned ~F deg" (getf panel :name) turn)
            (true (< (panel-angle-between n (lis::vec3-normalize (lis::vec3-scale c -1d0))) 0.1d0)
                  "~A points at the eye" (getf panel :name))))
      ;; The table's direction is where the panel lies from the eye.
      (destructuring-bind (azimuth elevation) (getf panel :direction)
        (let ((az (/ (* 180 (atan (aref c 1) (aref c 0))) pi))
              (el (/ (* 180 (atan (aref c 2) (sqrt (+ (expt (aref c 0) 2) (expt (aref c 1) 2))))) pi)))
          (true (< (abs (- az azimuth)) 1d0) "~A azimuth ~F" (getf panel :name) az)
          (true (< (abs (- el elevation)) 1d0) "~A elevation ~F" (getf panel :name) el))))))

;; 2D separating axes on the faces' gnomonic projections (y/x, z/x).
(defun face-polygon (panel)
  (multiple-value-bind (c n r u) (lis::panel-frame panel)
    (declare (ignore n))
    (destructuring-bind (w h) (getf panel :size)
      (loop for (sr su) in '((-1 -1) (1 -1) (1 1) (-1 1))
            collect (let ((p (lis::vlin c (* sr 0.5d0 w) r (* su 0.5d0 h) u)))
                      (list (/ (aref p 1) (aref p 0)) (/ (aref p 2) (aref p 0))))))))

(defun polygons-overlap-p (a b)
  (flet ((separated (poly other)
           (loop for (p q) on (append poly (list (first poly))) while q
                 thereis (let* ((ex (- (first q) (first p))) (ey (- (second q) (second p)))
                                (nx (- ey)) (ny ex))
                           (flet ((range (pts)
                                    (let ((ds (mapcar (lambda (v) (+ (* nx (first v)) (* ny (second v)))) pts)))
                                      (values (reduce #'min ds) (reduce #'max ds)))))
                             (multiple-value-bind (a0 a1) (range poly)
                               (multiple-value-bind (b0 b1) (range other)
                                 (or (< a1 b0) (< b1 a0)))))))))
    (not (or (separated a b) (separated b a)))))

(define-test console-faces-do-not-overlap-from-the-eye
  (let ((panels (remove-if (lambda (p) (getf p :round)) lis::*cockpit-panels*)))
    (loop for (a . rest) on panels
          do (dolist (b rest)
               (false (polygons-overlap-p (face-polygon a) (face-polygon b))
                      "~A and ~A overlap seen from the eye" (getf a :name) (getf b :name))))))

(defun obb-intersect-p (a b)
  "Whether two oriented boxes (c n r u hn hr hu), as lists, intersect (15 separating axes)."
  (destructuring-bind (ca &rest da) a
    (destructuring-bind (cb &rest db) b
      (let* ((axes-a (subseq da 0 3)) (axes-b (subseq db 0 3))
             (half-a (subseq da 3)) (half-b (subseq db 3))
             (d (lis::vec3-sub cb ca))
             (candidates (append axes-a axes-b
                                 (loop for x in axes-a append
                                       (loop for y in axes-b collect (lis::vec3-cross x y))))))
        (dolist (axis candidates t)
          (let ((norm (lis::vec3-norm axis)))
            (when (> norm 1d-9)
              (let* ((axis (lis::vec3-scale axis (/ norm)))
                     (ra (loop for x in axes-a for h in half-a sum (* h (abs (lis::vec3-dot x axis)))))
                     (rb (loop for x in axes-b for h in half-b sum (* h (abs (lis::vec3-dot x axis))))))
                (when (> (abs (lis::vec3-dot d axis)) (+ ra rb))
                  (return nil))))))))))

(define-test console-bezels-do-not-intersect
  (let ((panels (remove-if (lambda (p) (getf p :round)) lis::*cockpit-panels*)))
    (loop for (a . rest) on panels
          do (dolist (b rest)
               (false (obb-intersect-p (multiple-value-list (lis::panel-bezel-box a))
                                       (multiple-value-list (lis::panel-bezel-box b)))
                      "the bezels of ~A and ~A intersect" (getf a :name) (getf b :name))))))

(define-test console-text-grids-fit
  (dolist (panel lis::*cockpit-panels*)
    (when (getf panel :screen)
      (destructuring-bind (w h) (getf panel :size)
        (destructuring-bind (columns rows) (getf panel :grid)
          (let ((cap (lis::panel-grid-cap panel))
                (distance (lis::vec3-norm (apply #'lis::vec3 (getf panel :centre)))))
            (true (>= cap (* 0.0175d0 distance)) "~A: cap ~,2F mm at ~,2F m" (getf panel :name) (* 1000 cap) distance)
            (true (<= (* columns lis::+text-cell-width+ cap) (+ (- w 0.02d0) 1d-9)))
            (true (<= (* rows lis::+text-cell-height+ cap) (+ (- h 0.02d0) 1d-9)))))))))

(defun head-projection (point yaw pitch)
  "(values x y depth) of the body POINT from the eye for a head at YAW, PITCH
degrees: x right, y down, in tangents (divide by tan(fov/2))."
  (let* ((yaw (lis::degrees-to-radians yaw)) (pitch (lis::degrees-to-radians pitch))
         (forward (lis::vec3 (* (cos pitch) (cos yaw)) (* (cos pitch) (sin yaw)) (sin pitch)))
         (right (lis::vec3 (- (sin yaw)) (cos yaw) 0d0))
         (down (lis::vec3-cross forward right))
         (depth (lis::vec3-dot point forward)))
    (values (/ (lis::vec3-dot point right) depth) (/ (lis::vec3-dot point down) depth) depth)))

(define-test console-home-framing
  ;; At Home (pitch 16, fov 70, 1280 x 800) the window, both lamp columns and
  ;; the console down to 51 degrees are in the frame; the throttle quadrant,
  ;; the hull gauge and COMMS are outside it.
  (let ((half (tan (lis::degrees-to-radians 35d0))) (aspect 1.6d0))
    (flet ((inside-p (point)
             (multiple-value-bind (x y depth) (head-projection point 0 16)
               (and (> depth 0) (<= (abs x) (* aspect half)) (<= (abs y) half)))))
      (dolist (name '(:caution-lamps :status-lamps :orbit :attitude :target :strip :buttons
                      :autopilot :ship))
        (true (inside-p (apply #'lis::vec3 (getf (lis::cockpit-panel name) :centre))) "~A in the frame" name))
      (dolist (name '(:throttle :hull-gauge :comms))
        (false (inside-p (apply #'lis::vec3 (getf (lis::cockpit-panel name) :centre))) "~A outside the frame" name))
      (dolist (z '(-0.30d0 0.30d0))
        (true (inside-p (lis::vec3 0.9d0 0d0 z)) "the window's edge at ~F" z)))))

(defun distance-to-rack (p)
  (let ((best 1d9))
    (dolist (member lis::*cockpit-rack* best)
      (multiple-value-bind (a b) (lis::rack-member-ends member)
        (setf best (min best (- (lis::vec3-norm (lis::vec3-sub p (lis::closest-on-segment p a b)))
                                (* 0.5d0 lis::*cockpit-rack-section*))))))))

(define-test console-brackets
  (dolist (panel lis::*cockpit-panels*)
    (let ((brackets (lis::panel-brackets panel)))
      (true (>= (length brackets) 2) "~A has ~D brackets" (getf panel :name) (length brackets))
      (dolist (bracket brackets)
        (destructuring-bind (p m q member) bracket
          (declare (ignore p m))
          (true (if (eq member :wall)
                    (< (abs (- (aref q 0) (getf lis::*cab-window* :x))) 0.005d0)
                    (< (distance-to-rack q) 0.005d0))
                "~A's bracket ends off ~A" (getf panel :name) member))))))

(defun point-in-face-box-p (point panel margin)
  (multiple-value-bind (c n r u) (lis::panel-frame panel)
    (destructuring-bind (w h) (getf panel :size)
      (let ((d (lis::vec3-sub point c)))
        (and (< (abs (lis::vec3-dot d n)) (+ 0.002d0 margin))
             (< (abs (lis::vec3-dot d r)) (+ (* 0.5d0 w) margin))
             (< (abs (lis::vec3-dot d u)) (+ (* 0.5d0 h) margin)))))))

(define-test console-cables
  (let ((cables (lis::cockpit-cables)))
    (true (<= 12 (length cables) 36))
    ;; One to three a panel; one red (TARGET's), one yellow (the button block's).
    (dolist (panel lis::*cockpit-panels*)
      (true (<= 1 (count (getf panel :name) cables :key #'first) 3)))
    (is = 1 (count :cable-red cables :key #'second))
    (is = 1 (count :cable-yellow cables :key #'second))
    (is eq :target (first (find :cable-red cables :key #'second)))
    (is eq :buttons (first (find :cable-yellow cables :key #'second)))
    (dolist (cable cables)
      (destructuring-bind (name colour radius points) cable
        (declare (ignore colour))
        (true (<= 0.0075d0 radius 0.0125d0) "~A cable Ø ~F" name (* 2 radius))
        (is = 7 (length points))
        ;; The middle sags 8 % of the span below the chord.
        (let* ((a (first points)) (b (car (last points)))
               (span (lis::vec3-norm (lis::vec3-sub b a)))
               (mid (lis::vec3-scale (lis::vec3-add a b) 0.5d0))
               (drop (- (aref (fourth points) 2) (aref mid 2))))
          (true (< (abs (- drop (* 0.08d0 span))) 0.001d0) "~A sags ~,1F mm of ~,1F" name (* 1000 drop) (* 80 span)))
        (dolist (p points)
          (true (<= (cab-distance-from-axis p) lis::*cab-lining-radius*) "~A cable inside the cab ~A" name p)
          (true (<= (first lis::*cab-x-range*) (aref p 0) (second lis::*cab-x-range*)) "~A cable x ~A" name p)
          (dolist (panel lis::*cockpit-panels*)
            (false (point-in-face-box-p p panel 0d0) "~A cable through the face of ~A" name (getf panel :name))))))
    (dolist (trunk (lis::cockpit-trunks))
      (dolist (p (third trunk))
        (true (<= (cab-distance-from-axis p) lis::*cab-lining-radius*) "trunk inside the cab ~A" p)))))

(define-test console-labels
  (is = 14 (length lis::*cockpit-labels*))
  (dolist (label lis::*cockpit-labels*)
    (let ((text (getf label :text)))
      ;; A key in brackets except the listed ones.
      (true (or (find #\[ text) (member text lis::*cockpit-label-keyless* :test #'string=))
            "~S has no key in brackets" text)
      (true (<= (abs (lis::label-angle text)) (lis::degrees-to-radians 2d0)) "~S angle" text)
      (multiple-value-bind (cap advance length) (lis::label-metrics label)
        (multiple-value-bind (centre) (lis::label-frame label)
          (true (>= cap (* 0.0175d0 (lis::vec3-norm centre))) "~S cap ~,2F mm at ~,2F m" text (* 1000 cap)
                (lis::vec3-norm centre)))
        (true (< advance length) "~S inside its strip" text)
        (true (<= cap 0.04d0) "~S cap inside the strip's height" text))
      (multiple-value-bind (centre n r u) (lis::label-frame label)
        (declare (ignore n))
        (true (< (cab-distance-from-axis centre) lis::*cab-lining-radius*))
        (true (< (abs (lis::vec3-dot r u)) 1d-9)))))
  ;; The panel-side labels' strips are clear of their own face.
  (dolist (label lis::*cockpit-labels*)
    (unless (getf label :window)
      (multiple-value-bind (centre n r u height) (lis::label-frame label)
        (declare (ignore n r u))
        (let ((panel (lis::cockpit-panel (getf label :panel))))
          (multiple-value-bind (c pn pr pu) (lis::panel-frame panel)
            (declare (ignore pn pr))
            (let ((dv (lis::vec3-dot (lis::vec3-sub centre c) pu)))
              (true (> (abs dv) (+ (* 0.5d0 (second (getf panel :size))) (* 0.4d0 height)))
                    "~S lies clear of the face" (getf label :text)))))))))

(define-test console-text-regions
  ;; cockpit-text! writes the labels; the screens are drawn by the instruments.
  (let* ((atlas (text-test-atlas)) (batch (lis::make-text-batch)))
    (lis::cockpit-text! atlas batch)
    (dolist (label lis::*cockpit-labels*)
      (true (lis::cockpit-text-region (list :label (getf label :text))) "~S" (getf label :text)))
    (true (< (lis::text-batch-count batch) lis::*text-max-glyphs*))))

(define-test console-budget
  (let ((model (lis::cab-model)))
    (true (<= (lis::cab-vertex-count model) 30000))
    (true (<= (lis::cab-instance-count model) 2000) "~D instances" (lis::cab-instance-count model))
    (dolist (name '("rack" "tray" "panel-faces" "bezels" "bezel-bolts" "hoods" "brackets" "cables"
                    "zip-ties" "kapton" "labels"))
      (true (assoc name (lis::model-parts model) :test #'string=) "part ~A" name))))

;;; The sunlit patch's twin against a brute-force walk along the ray.

(defun brute-patch-p (p s)
  "Walks the ray from P along S in 1 mm steps, interpolating each crossing of
the window's plane and of a porthole's plane."
  (let ((wx (getf lis::*cab-window* :x)))
    (flet ((crossing (f0 f1 at-fraction)
             (and (< f0 0d0) (>= f1 0d0) (funcall at-fraction (/ (- f0) (- f1 f0))))))
      (loop with step = 0.001d0
            for k from 0 below 6000
            for a = (lis::vec3-add-scaled p s (* k step))
            for b = (lis::vec3-add-scaled p s (* (1+ k) step))
            thereis (or (crossing (- (aref a 0) wx) (- (aref b 0) wx)
                                  (lambda (f)
                                    (let ((h (lis::vec3-add-scaled a (lis::vec3-sub b a) f)))
                                      (and (< (abs (aref h 1)) (getf lis::*cab-window* :half-width))
                                           (< (abs (aref h 2)) (getf lis::*cab-window* :half-height))))))
                        (dolist (side '(-1 1) nil)
                          (multiple-value-bind (centre axis) (lis::cab-porthole-geometry side)
                            (when (crossing (lis::vec3-dot (lis::vec3-sub a centre) axis)
                                            (lis::vec3-dot (lis::vec3-sub b centre) axis)
                                            (lambda (f)
                                              (let ((h (lis::vec3-add-scaled a (lis::vec3-sub b a) f)))
                                                (< (lis::vec3-norm (lis::vec3-sub h centre))
                                                   lis::*cab-porthole-radius*))))
                              (return t)))))))))

(define-test sun-patch-twin
  (let ((state (sb-ext:seed-random-state 236)) (lit 0))
    (dotimes (i 1000)
      (let* ((p (lis::vec3 (- (random 4.3d0 state) 3.55d0) (- (random 2.4d0 state) 1.2d0)
                           (- (random 2.3d0 state) 1.15d0)))
             (s (lis::vec3-normalize (lis::vec3 (+ 0.2d0 (random 0.8d0 state)) (- (random 2d0 state) 1d0)
                                                (- (random 2d0 state) 1d0))))
             (brute (brute-patch-p p s)))
        (when brute (incf lit))
        (when (and (lis::sun-patch-p p s -0.002d0) (not brute))
          (fail "twin shrunk lit, brute not: ~A ~A" p s))
        (when (and brute (not (lis::sun-patch-p p s 0.002d0)))
          (fail "brute lit, twin grown not: ~A ~A" p s))))
    (true (< 20 lit 900) "~D of 1000 lit" lit)))

;;; The screens (plan 32 piece 1; decision 127: Cockpit screens' contents in
;;; milestone 3: lines, modes, plots, colours and updates): ORBIT and SHIP.

(defun screen-state (sim &key (run t))
  "(values render-state clock) of SIM, its prediction run to completion."
  (let* ((clock (lis::new-clock sim))
         (state (lis::render-state-update! (lis::make-render-state) clock sim)))
    (when run (lis::prediction-run! (lis::clock-prediction clock) sim))
    (values state clock)))

(defun screen-lines-of (name sim &key (real-time 0d0) mutate)
  "(values texts lines) of screen NAME of SIM; MUTATE a function of the render state."
  (multiple-value-bind (state clock) (screen-state sim)
    (when mutate (funcall mutate state))
    (let ((lines (lis::screen-lines name state clock real-time)))
      (values (mapcar #'lis::line-text lines) lines))))

(defun orbit-period-minutes (a)
  (/ (* 2 pi (sqrt (/ (expt a 3) lis::+mu-earth+))) 60))

(define-test orbit-screen-start-state
  (let ((sim (lis::new-game-sim)))
    (multiple-value-bind (state clock) (screen-state sim)
      (let ((texts (mapcar #'lis::line-text (lis::screen-lines :orbit state clock 0d0)))
            (speed (sqrt (/ lis::+mu-earth+ (+ lis::+radius-earth+ 400000d0)))))
        (is string= "EARTH  SOI --:--" (nth 0 texts))
        (is string= "ALT  400.00 km" (nth 1 texts))
        (is string= (format nil "SPD ~,3F km/s" (/ speed 1000)) (nth 2 texts))
        (is string= "VS  +0.0 m/s" (nth 3 texts))
        (is string= "INC 90.00° ECC 0.0000" (nth 6 texts))
        (is string= (format nil "PER ~,1F min" (orbit-period-minutes (+ lis::+radius-earth+ 400000d0)))
            (nth 7 texts))
        (loop for text in (list (nth 4 texts) (nth 5 texts))
              for kind in '(:apoapsis :periapsis)
              for prefix in '("AP 400.0 km T-" "PE 400.0 km T-")
              do (true (eql 0 (search prefix text)) "~S" text)
                 (let* ((tail (subseq text (1- (length prefix))))
                        (minutes (parse-integer tail :start 1 :end (position #\: tail)))
                        (seconds (parse-integer tail :start (1+ (position #\: tail))))
                        (shown (+ (* 60 minutes) seconds))
                        (event (- (lis::prediction-event (lis::clock-prediction clock) kind)
                                  (lis::render-state-t state))))
                   (true (<= (abs (- shown event)) 1) "~S: ~D s against ~F s" text shown event)))
        (dolist (line (lis::screen-lines :orbit state clock 0d0))
          (is eq :plain (lis::run-style (first line))))))))

(define-test orbit-screen-alarms
  ;; Periapsis below the crash radius: red, inverted, flashing; an escape orbit.
  (let ((sim (lis::new-game-sim)))
    (flet ((slow (state)
             (lis::vec3-scale! (lis::render-state-v state) (lis::render-state-v state) 0.9d0))
           (fast (state)
             (lis::vec3-scale! (lis::render-state-v state) (lis::render-state-v state) 1.5d0)))
      (multiple-value-bind (texts lines) (screen-lines-of :orbit sim :mutate #'slow)
        (let ((pe (nth 5 lines)))
          (true (eql 0 (search "PE " (nth 5 texts))))
          (is = lis::*screen-red* (lis::run-colour (first pe)))
          (is eq :inverted (lis::run-style (first pe)))
          (is eq :on (lis::run-flash (first pe)))))
      (let ((off (nth 5 (nth-value 1 (screen-lines-of :orbit sim :mutate #'slow :real-time 0.3d0)))))
        (is eq :off (lis::run-flash (first off))))
      (multiple-value-bind (texts) (screen-lines-of :orbit sim :mutate #'fast)
        (is string= "AP ESCAPE" (nth 4 texts))
        (is string= "PER --" (nth 7 texts))))))

(define-test ship-screen-wet-and-loaded
  (let ((sim (lis::new-game-sim)))
    (multiple-value-bind (texts) (screen-lines-of :ship sim)
      (let* ((mass (+ lis::*ship-dry-mass* 28000d0))
             (dv (* (lis::engine-exhaust-velocity) (log (/ mass lis::*ship-dry-mass*)))))
        (is string= "PROP 28,000 kg 100 %" (nth 0 texts))
        (is string= (format nil "DV ~,2F km/s" (/ dv 1000)) (nth 1 texts))
        (is string= (format nil "MASS ~,1F t ~,2F m/s²" (/ mass 1000) (/ lis::*engine-thrust* mass))
            (nth 2 texts))
        (is string= "THR 0 %  ENG OFF" (nth 3 texts))
        (is string= "" (nth 4 texts))
        (is equal '("LIFE --- d" "SUN --- AU" "HULL --- K") (last texts 3)))))
  (let* ((sim (lis::stated-sim '(:date "2040-01-19" :orbit (:near :pod-14 :at-slot) :grappled :pod-14)))
         (def (lis::object-def-of (lis::sim-object sim :pod-14))))
    (setf (lis::controls-throttle (lis::sim-controls sim)) 0.5d0)
    (multiple-value-bind (texts) (screen-lines-of :ship sim)
      (let ((mass (+ lis::*ship-dry-mass* 28000d0 (lis::object-def-mass def))))
        (is string= (format nil "MASS ~,1F t ~,2F m/s²" (/ mass 1000) (/ lis::*engine-thrust* mass))
            (nth 2 texts))
        (is string= "THR 50 %  ENG ON" (nth 3 texts))
        (is string= (format nil "LOAD ~,1F t POD 14" (/ (lis::object-def-mass def) 1000)) (nth 4 texts))))))

(define-test ship-screen-berthed
  (let ((sim (lis::stated-sim '(:berthed (:tern-station :berth)))))
    (multiple-value-bind (texts) (screen-lines-of :ship sim)
      (is string= "XFER --" (nth 3 texts))
      (is string= "BERTHED TERN STATION" (nth 4 texts))))
  (let ((sim (lis::stated-sim '(:berthed (:fulmar-depot :a) :propellant 20000d0))))
    (is string= "XFER OFF [B]" (nth 3 (screen-lines-of :ship sim)))
    (setf (lis::sim-transfer sim) t)
    (is string= "XFER 10 kg/s" (nth 3 (screen-lines-of :ship sim)))
    (is string= "BERTHED FULMAR DEPOT" (nth 4 (screen-lines-of :ship sim))))
  (let ((sim (lis::new-game-sim)))
    (setf (lis::ship-state-m-p (lis::sim-ship sim)) 1000d0)
    (let ((line (nth 0 (nth-value 1 (screen-lines-of :ship sim)))))
      (is string= "! PROP 1,000 kg 4 %" (lis::line-text line))
      (true (every (lambda (run) (= (lis::run-colour run) lis::*screen-amber*)) line)))))

(define-test screen-lines-fit-their-grids
  (dolist (sim (list (lis::new-game-sim)
                     (lis::stated-sim '(:berthed (:fulmar-depot :a)))
                     (lis::stated-sim '(:date "2040-01-19" :orbit (:near :pod-14 :at-slot) :grappled :pod-14))))
    (dolist (name '(:orbit :ship :target :autopilot))
      (destructuring-bind (columns rows) (getf (lis::cockpit-panel name) :grid)
        (multiple-value-bind (texts) (screen-lines-of name sim)
          (true (<= (length texts) rows))
          (dolist (text texts)
            (true (<= (length text) columns) "~S: ~D of ~D" text (length text) columns)
            (loop for char across text
                  do (true (or (char= char #\?)
                               (/= (lis::text-repertoire-index char) (lis::text-repertoire-index #\?)))
                           "~S in ~S is not in the text repertoire" char text))))))))

(define-test screen-fit-line-cuts-with-an-ellipsis
  (let* ((line (list (lis::screen-run "NAME " 1) (lis::screen-run "A VERY LONG STATION NAME" 2)))
         (cut (lis::screen-fit-line line 12)))
    (is = 12 (lis::screen-line-length cut))
    (is string= "NAME A VERY…" (lis::line-text cut))
    (is = 2 (lis::run-colour (second cut)))
    (is equal line (lis::screen-fit-line line 40))
    (is string= "NAM…" (lis::line-text (lis::screen-fit-line (list (lis::screen-run "NAME" 1)
                                                                      (lis::screen-run "XY" 2))
                                                                4)))))

(defun test-screen-views ()
  (lis::cockpit-screens-init! (lis::make-text-batch) (lis::make-line-batch)))

(define-test cockpit-update-rebuilds-at-10-hz-and-on-a-change
  (let ((sim (lis::new-game-sim)) (cockpit (lis::make-cockpit-state)) (views (test-screen-views)))
    (multiple-value-bind (state clock) (screen-state sim)
      (flet ((update (real-time)
               (lis::cockpit-update! cockpit views (text-test-atlas) state clock real-time :cockpit)))
        (is = 6 (update 0d0))
        (is = 0 (update 0.04d0))
        (is = 0 (update 0.09d0))
        (is = 6 (update 0.11d0))
        (is = 0 (update 0.15d0))
        ;; a hold change at once, between the 10 Hz rebuilds
        (setf (lis::controls-hold (lis::render-state-controls state)) :retrograde)
        (is = 6 (update 0.16d0))
        (is = 0 (update 0.17d0))
        (setf (lis::clock-paused clock) t)
        (is = 6 (update 0.18d0))
        (is = 6 (update 0.29d0))
        ;; a view change at once; a restart rebuilds
        (is = 6 (lis::cockpit-update! cockpit views (text-test-atlas) state clock 0.30d0 :map))
        (lis::cockpit-state-invalidate! cockpit)
        (is = 6 (update 0.31d0))))))

(define-test cockpit-rebuild-conses-little
  (let ((sim (lis::new-game-sim)) (cockpit (lis::make-cockpit-state)) (views (test-screen-views))
        (atlas (text-test-atlas)))
    (multiple-value-bind (state clock) (screen-state sim)
      (flet ((rebuild ()
               (lis::cockpit-state-invalidate! cockpit)
               (lis::cockpit-update! cockpit views atlas state clock 1d0 :cockpit)))
        (rebuild)
        (let ((before (sb-ext:get-bytes-consed)))
          (dotimes (i 50) (rebuild))
          (let ((each (/ (- (sb-ext:get-bytes-consed) before) 50)))
            (true (<= each 16384) "a rebuild of both screens conses ~D bytes" each)))))))

(define-test cockpit-signature-conses-nothing
  (let ((sim (lis::new-game-sim)))
    (multiple-value-bind (state clock) (screen-state sim)
      (lis::cockpit-signature state clock :cockpit)
      (is = 0 (least-bytes-consed (lambda () (dotimes (i 1000) (lis::cockpit-signature state clock :cockpit))))))))

(define-test screen-draw-writes-regions
  ;; An inverted run: dark text and a block of the run's colour in the line region.
  (let* ((views (test-screen-views)) (view (first views)) (atlas (text-test-atlas))
         (red (list (list (lis::screen-run "AB" lis::*screen-red* :inverted :on)))))
    (is eq :orbit (lis::screen-view-name view))
    (lis::screen-view-draw! view atlas red)
    (let* ((text (lis::screen-view-text view)) (data (lis::text-batch-data (lis::text-region-batch text)))
           (record (* lis::+text-record-floats+ (lis::text-region-start text)))
           (dark (lis::screen-ink lis::*screen-black*)))
      (is = (aref dark 0) (aref data (+ record 17)))
      (let* ((blocks (lis::screen-view-lines view)) (lines (lis::line-batch-data (lis::line-region-batch blocks)))
             (base (* lis::+line-record-floats+ (lis::line-region-start blocks)))
             (red-linear (lis::screen-line-colour lis::*screen-red*)))
        (is = (aref red-linear 0) (aref lines (+ base 6)))
        (true (plusp (aref lines (+ base 9))))
        ;; the block spans two cells
        (true (< (abs (- (* 2 lis::+text-cell-width+ (lis::screen-view-cap view))
            (let ((dx (- (aref lines (+ base 3)) (aref lines base)))
                  (dy (- (aref lines (+ base 4)) (aref lines (+ base 1))))
                  (dz (- (aref lines (+ base 5)) (aref lines (+ base 2)))))
              (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))))
                 1f-6)
              "a block of two cells")))
    ;; off phase: plain red text, no block
    (lis::screen-view-draw! view atlas (list (list (lis::screen-run "AB" lis::*screen-red* :inverted :off))))
    (let* ((blocks (lis::screen-view-lines view)) (lines (lis::line-batch-data (lis::line-region-batch blocks)))
           (base (* lis::+line-record-floats+ (lis::line-region-start blocks))))
      (is = 0.0 (aref lines (+ base 9))))))

(define-test cockpit-instruments-map
  ;; Every instrument of "Flight" → "Instruments" of milestone 3 or earlier has a place.
  (let ((names '("Altitude" "Speed" "Apoapsis and periapsis" "Throttle" "Propellant" "Attitude mode"
                 "Warp" "Clock and date" "Alarms and messages" "Inclination, eccentricity, period"
                 "Vertical speed" "Attitude indicator" "Δv remaining" "Mass and thrust-to-weight"
                 "Burn" "Target" "Grapple range and alignment" "More alarms" "Body and SOI")))
    (dolist (name names)
      (true (assoc name lis::*cockpit-instruments* :test #'string=) "~S has no place" name))
    (dolist (entry lis::*cockpit-instruments*)
      (destructuring-bind (name panel place) entry
        (let ((panel (find panel lis::*cockpit-panels* :key (lambda (p) (getf p :name)))))
          (true panel "~S: no such panel" name)
          (if (listp place)
              (let ((rows (second (getf panel :grid))))
                (true rows "~S: ~S has no grid" name (getf panel :name))
                (dolist (line place)
                  (true (<= 1 line rows) "~S: line ~D of ~D" name line rows)))
              (true (keywordp place))))))))

;;; The dot-matrix strip and COMMS (plan 32 piece 2; decision 128: Cockpit
;;; devices, buttons, picking, the grapple camera and messages in milestone 3).

(defun strip-test-view ()
  (lis::make-strip-view (lis::make-dot-batch)))

(defun strip-lit (view line row gx)
  "Whether the LED of VIEW at LINE (0 to 2), ROW (0 to 6) and dot column GX is lit."
  (= 1f0 (aref (lis::strip-view-data view)
               (+ 7 (* lis::+dot-record-floats+
                       (+ (lis::strip-view-start view)
                          (* (+ (* line 7) row) lis::+strip-dot-columns+) gx))))))

(define-test strip-line-1-and-2
  (is string= "2040-01-01 00:00:00" (lis::strip-line-1 (lis::start-t)))
  (is string= "2040-01-01 01:01:01" (lis::strip-line-1 (+ (lis::start-t) 3661.5d0)))
  (is string= "WARP ×1" (lis::strip-line-2 1 1d0 nil))
  (is string= "WARP ×1" (lis::strip-line-2 1 nil nil))
  (is string= "WARP ×1000 ACT ×490" (lis::strip-line-2 1000 490d0 nil))
  (is string= "WARP ×1000" (lis::strip-line-2 1000 950d0 nil) "no ACT within 90 %")
  (is string= "WARP ×1000000 ×490K" (lis::strip-line-2 1000000 490000d0 nil))
  (is string= "PAUSED" (lis::strip-line-2 1000 490d0 t))
  (dolist (line (list (lis::strip-line-1 (lis::start-t)) (lis::strip-line-2 1000000 490000d0 nil)
                      (lis::strip-line-2 100000 49d0 nil)))
    (true (<= (length line) lis::+strip-columns+) "~S" line)
    (false (lis::glyph-missing line) "~S" line)))

(define-test strip-dots-follow-the-glyph-table
  (let ((view (strip-test-view)))
    (lis::strip-dots! view "H" "" "")
    (let ((rows (lis::glyph #\H)))
      (dotimes (row 7)
        (dotimes (column 6)
          (is eq (and (< column 5) (char= #\# (char (nth row rows) column)) t)
              (and (strip-lit view 0 row column) t) "H row ~D dot ~D" row column))))
    (dotimes (row 7)
      (true (notany (lambda (gx) (strip-lit view 1 row gx)) (loop for g below 114 collect g))))
    ;; Line 3 shifted right by 7 dots: the glyph's column 0 on dot 7.
    (lis::strip-dots! view "" "" "H" 7)
    (true (strip-lit view 2 0 7)) (false (strip-lit view 2 0 6)) (true (strip-lit view 2 3 7))
    ;; Left of the edge: cut off.
    (lis::strip-dots! view "" "" "H" -3)
    (true (strip-lit view 2 0 1)) (false (strip-lit view 2 0 0))))

(define-test strip-dots-conses-nothing
  (let ((view (strip-test-view)) (a "2040-01-01 00:00:00") (b "WARP ×1000 ACT ×490") (c "AUTOPILOT: PLAN COMPLETE"))
    (lis::strip-dots! view a b c 40)
    (is = 0 (least-bytes-consed (lambda () (dotimes (i 200) (lis::strip-dots! view a b c (- 40 (mod i 3)))))))))

(defun strip-test-setup ()
  "(values cockpit strip clock state)"
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim) :run nil)
    (values (lis::make-cockpit-state) (strip-test-view) clock state)))

(defun strip-line-3 (view)
  (lis::strip-view-line-3 view))

(define-test strip-marquee-timing
  (multiple-value-bind (cockpit view clock state) (strip-test-setup)
    (flet ((tick (time) (lis::strip-update! cockpit view state clock time nil)))
      (tick 0d0)
      (is string= "" (strip-line-3 view))
      ;; 12 characters: static for 3 s, then gone.
      (lis::clock-message! clock :target "TARGET LOST!")
      (tick 1d0)
      (is string= "TARGET LOST!" (strip-line-3 view))
      (is = 0 (lis::strip-view-shift view))
      (tick 3.9d0)
      (is string= "TARGET LOST!" (strip-line-3 view))
      (tick 4.05d0)
      (is string= "" (strip-line-3 view))
      ;; 40 characters: from the right edge at 12 characters a second.
      (let ((text "AUTOPILOT ABORT: PROPELLANT LOW FOR PLAN"))
        (is = 40 (length text))
        (lis::clock-message! clock :autopilot text)
        (tick 10d0)
        (is string= text (strip-line-3 view))
        (is = 114 (lis::strip-view-shift view))
        (tick 11d0)
        (is = (- 114 72) (lis::strip-view-shift view))
        (let ((time (+ 10d0 (/ 19d0 12))))
          (tick time)
          (true (<= (abs (lis::strip-view-shift view)) 1) "first character at the left edge: ~D" (lis::strip-view-shift view)))
        (tick (+ 10d0 (/ 58d0 12)))
        (is string= text (strip-line-3 view))
        (tick (+ 10d0 (/ 59d0 12) 0.01d0))
        (is string= "" (strip-line-3 view))))))

(define-test strip-marquee-queue
  (multiple-value-bind (cockpit view clock state) (strip-test-setup)
    (flet ((tick (time) (lis::strip-update! cockpit view state clock time nil)))
      (tick 0d0)
      (dolist (text '("ONE" "TWO" "THREE"))
        (lis::clock-message! clock :target text))
      (tick 1d0)
      (is string= "ONE" (strip-line-3 view))
      (tick 3.5d0) (is string= "ONE" (strip-line-3 view))
      (tick 4.1d0) (is string= "TWO" (strip-line-3 view))
      (tick 7.2d0) (is string= "THREE" (strip-line-3 view))
      (tick 10.3d0) (is string= "" (strip-line-3 view))
    ;; Nine at once: the oldest dropped, eight remain in order.
    (let ((seen '()) (time 20d0))
      (dotimes (i 9) (lis::clock-message! clock :target (format nil "MSG ~D" (1+ i))))
      (dotimes (i 40)
        (tick time)
        (pushnew (strip-line-3 view) seen :test #'string=)
        (incf time 1d0))
      (is equal '("MSG 2" "MSG 3" "MSG 4" "MSG 5" "MSG 6" "MSG 7" "MSG 8" "MSG 9")
          (remove "" (reverse seen) :test #'string=))))))

(define-test strip-idle-line-shows-the-hovered-key
  (multiple-value-bind (cockpit view clock state) (strip-test-setup)
    (setf (lis::cockpit-state-strip-hover cockpit) "PRO [1]")
    (lis::strip-update! cockpit view state clock 0d0 nil)
    (is string= "PRO [1]" (strip-line-3 view))
    (lis::clock-message! clock :target "A MESSAGE")
    (lis::strip-update! cockpit view state clock 0.1d0 nil)
    (is string= "A MESSAGE" (strip-line-3 view))))

(define-test strip-snap-shows-the-latest-message-static
  (multiple-value-bind (cockpit view clock state) (strip-test-setup)
    (lis::clock-message! clock :target "AUTOPILOT: PLAN COMPLETE")
    (lis::strip-update! cockpit view state clock 100d0 t)
    (is string= "AUTOPILOT: PLAN COM" (strip-line-3 view))
    (is = 0 (lis::strip-view-shift view))
    (lis::strip-update! cockpit view state clock 200d0 t)
    (is string= "AUTOPILOT: PLAN COM" (strip-line-3 view))))

(defun comms-test-setup (&key (entries 0))
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim) :run nil)
    (declare (ignore state))
    (dotimes (i entries) (lis::clock-message! clock :target (format nil "ENTRY ~D" (1+ i))))
    (values (lis::make-cockpit-state) clock)))

(defun comms-log-texts (clock cockpit)
  (mapcar #'lis::line-text (nthcdr 4 (lis::comms-lines clock cockpit))))

(define-test comms-lines-job-card-and-rule
  (multiple-value-bind (cockpit clock) (comms-test-setup)
    (let ((lines (lis::comms-lines clock cockpit)))
      (is string= "JOB" (lis::line-text (first lines)))
      (is string= "NONE" (lis::line-text (second lines)))
      (is string= (make-string 26 :initial-element #\-) (lis::line-text (fourth lines)))
      (is equal (list (string lis::*comms-cursor*)) (comms-log-texts clock cockpit)))
    (dolist (name '(:comms))
      (let ((lines (lis::screen-lines name (screen-state (lis::new-game-sim) :run nil) clock 0d0 cockpit)))
        (true (<= (length lines) 11))))))

(define-test comms-log-types-at-60-characters-a-second
  (multiple-value-bind (cockpit clock) (comms-test-setup :entries 3)
    (lis::comms-type! cockpit clock 0d0 nil)
    (is equal (list (string lis::*comms-cursor*)) (comms-log-texts clock cockpit))
    (lis::comms-type! cockpit clock 0.1d0 nil)         ; 6 characters
    (is equal (list (format nil "00:00 ~A" lis::*comms-cursor*)) (comms-log-texts clock cockpit))
    ;; At 0.5 s: 30 characters. The log's order is arrival order: "00:00 ENTRY 1" is 13
    ;; characters, the second 13, the third's first 4.
    (lis::comms-type! cockpit clock 0.5d0 nil)
    (is equal (list "00:00 ENTRY 1" "00:00 ENTRY 2" (format nil "00:0~A" lis::*comms-cursor*))
        (comms-log-texts clock cockpit))
    ;; At 1 s all three, the cursor after the last.
    (lis::comms-type! cockpit clock 1d0 nil)
    (is equal (list "00:00 ENTRY 1" "00:00 ENTRY 2" (format nil "00:00 ENTRY 3~A" lis::*comms-cursor*))
        (comms-log-texts clock cockpit))
    ;; No time passing: nothing more, and a later entry types after.
    (lis::clock-message! clock :target "ENTRY 4")
    (lis::comms-type! cockpit clock 1.05d0 nil)
    (is = 4 (length (comms-log-texts clock cockpit)))
    (is equal (format nil "00:~A" lis::*comms-cursor*) (car (last (comms-log-texts clock cockpit))))))

(define-test comms-log-snap-types-everything
  (multiple-value-bind (cockpit clock) (comms-test-setup :entries 5)
    (lis::comms-type! cockpit clock 0d0 t)
    (is = 5 (length (comms-log-texts clock cockpit)))))

(define-test comms-log-wraps-with-a-continuation-indent
  (multiple-value-bind (cockpit clock) (comms-test-setup)
    (let ((text "AUTOPILOT ABORT: PROPELLANT LOW FOR PLAN"))
      (lis::clock-message! clock :autopilot text)
      (lis::comms-type! cockpit clock 0d0 t)
      (let ((lines (comms-log-texts clock cockpit)))
        (true (> (length lines) 1))
        (dolist (line lines) (true (<= (length line) 26) "~S" line))
        (is string= "00:00 AUTOPILOT ABORT:" (first lines))
        (dolist (line (rest lines))
          (is string= "      " (subseq line 0 6) "~S" line))
        (is string= (format nil "00:00 ~A~A" text lis::*comms-cursor*)
            (format nil "~{~A~^ ~}" (cons (first lines) (mapcar (lambda (l) (string-left-trim " " l)) (rest lines)))))))
    ;; A word longer than the line breaks hard.
    (is equal '("ABCDEFGHIJKLMNOPQRSTUVWXYZ" "      ABCD")
        (lis::wrap-log-text "ABCDEFGHIJKLMNOPQRSTUVWXYZABCD"))))

(define-test comms-log-keeps-200-entries
  (multiple-value-bind (cockpit clock) (comms-test-setup :entries 210)
    (is = 200 (length (lis::clock-log clock)))
    (lis::comms-type! cockpit clock 0d0 t)
    (let ((lines (lis::comms-lines clock cockpit)))
      (is = 11 (length lines))
      (is string= (format nil "00:00 ENTRY 210~A" lis::*comms-cursor*) (lis::line-text (car (last lines)))))
    (lis::comms-scroll! cockpit 1000)
    (let ((lines (lis::comms-lines clock cockpit)))
      (is string= "00:00 ENTRY 11" (lis::line-text (nth 4 lines)) "the oldest kept is 11")
      (is = 193 (cockpit-scroll-of cockpit)))))

(defun cockpit-scroll-of (cockpit) (lis::cockpit-state-scroll cockpit))

(define-test comms-scroll-holds-while-entries-arrive
  (multiple-value-bind (cockpit clock) (comms-test-setup :entries 20)
    (lis::comms-type! cockpit clock 0d0 t)
    (lis::comms-lines clock cockpit)
    (lis::comms-scroll! cockpit 3)
    (let ((before (comms-log-texts clock cockpit)))
      (is string= "00:00 ENTRY 17" (car (last before)) "three lines up")
      (lis::clock-message! clock :target "ENTRY 21")
      (lis::clock-message! clock :target "ENTRY 22")
      (lis::comms-type! cockpit clock 0d0 t)
      (is equal before (comms-log-texts clock cockpit) "the view stays put")
      (is = 5 (cockpit-scroll-of cockpit)))
    ;; Back at the bottom it follows.
    (lis::comms-scroll! cockpit -100)
    (lis::clock-message! clock :target "ENTRY 23")
    (lis::comms-type! cockpit clock 0d0 t)
    (is string= (format nil "00:00 ENTRY 23~A" lis::*comms-cursor*) (car (last (comms-log-texts clock cockpit))))))

(define-test comms-rebuilds-while-typing
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim) :run nil)
    (let ((cockpit (lis::make-cockpit-state)) (views (test-screen-views)) (atlas (text-test-atlas)))
      (flet ((update (time) (lis::cockpit-update! cockpit views atlas state clock time :cockpit)))
        (update 0d0)
        (lis::clock-message! clock :target "A MESSAGE TYPING OUT")
        (update 0.02d0)              ; the log's length changed: all screens
        (is = 1 (update 0.04d0) "COMMS alone, typing")
        (is = 1 (update 0.06d0))
        (update 2d0)
        (is = 0 (update 2.01d0) "typed out: nothing between the 10 Hz rebuilds")))))

;;; Lamps, MSTR, gauges and the throttle lever (plan 32 piece 3; decision 128:
;;; Cockpit devices, buttons, picking, the grapple camera and messages in
;;; milestone 3).

(defun lamp-of (key state clock &optional (cockpit (lis::make-cockpit-state)))
  "(colour flash) of lamp KEY."
  (multiple-value-list (lis::lamp-condition key state clock cockpit)))

(defun slow-state (state)
  "STATE's velocity at 0.9: a periapsis far below the crash radius."
  (lis::vec3-scale! (lis::render-state-v state) (lis::render-state-v state) 0.9d0))

(define-test lamps-dark-at-the-start
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (dolist (entry (lis::lamp-states state clock (lis::make-cockpit-state)))
      (is eq nil (second entry) "~A at the start" (first entry)))
    (is = 11 (length lis::*lamps*))))

(define-test lamp-peri
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (is equal '(nil nil) (lamp-of :peri state clock))
    (slow-state state)
    (is equal '(:red nil) (lamp-of :peri state clock)))
  ;; An impact in the prediction lights it with a safe osculating periapsis.
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (lis::push-event! (lis::clock-prediction clock) :impact (+ (lis::render-state-t state) 600d0))
    (is equal '(:red nil) (lamp-of :peri state clock))))

(define-test lamp-prop
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (is equal '(nil nil) (lamp-of :prop state clock))
    (setf (lis::render-state-m-p state) (* 0.04d0 lis::*propellant-full*))
    (is equal '(:amber nil) (lamp-of :prop state clock))
    (setf (lis::render-state-m-p state) (* 0.06d0 lis::*propellant-full*))
    (is equal '(nil nil) (lamp-of :prop state clock)))
  ;; Below a shown plan's need, and the burns already cut do not count.
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (let* ((model (lis::clock-autopilot clock))
           (ve (lis::engine-exhaust-velocity))
           (now (lis::render-state-t state)))
      (flet ((plan (dv &optional (cut (+ now 1000d0)))
               (lis::make-burn-plan
                (vector (lis::make-planned-burn :prograde 0d0 now (lis::quat 1 0 0 0) 1d0 nil dv :cut-t cut)))))
        (setf (lis::render-state-autopilot-model state) model)
        (setf (lis::autopilot-model-plan model) (plan (* 1.0d0 ve)))
        (setf (lis::autopilot-model-state model) :shown)
        (is equal '(nil nil) (lamp-of :prop state clock))
        (setf (lis::autopilot-model-plan model) (plan (* 2.0d0 ve)))
        (is equal '(:amber nil) (lamp-of :prop state clock))
        (setf (lis::autopilot-model-plan model) (plan (* 2.0d0 ve) (- now 1d0)))
        (is equal '(nil nil) (lamp-of :prop state clock))
        (setf (lis::autopilot-model-state model) :closed)
        (setf (lis::autopilot-model-plan model) (plan (* 2.0d0 ve)))
        (is equal '(nil nil) (lamp-of :prop state clock))))))

(define-test lamp-warp
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (is equal '(nil nil) (lamp-of :warp state clock))
    (setf (lis::clock-level-index clock) 2)
    (is equal '(:white nil) (lamp-of :warp state clock))
    (lis::clock-message! clock :refused "WARP REFUSED")
    (is equal '(:amber :slow) (lamp-of :warp state clock))
    (setf (lis::clock-real-time clock) 2.9d0)
    (is equal '(:amber :slow) (lamp-of :warp state clock))
    (setf (lis::clock-real-time clock) 3.1d0)
    (is equal '(:white nil) (lamp-of :warp state clock))
    (lis::clock-message! clock :lowered "WARP LOWERED")
    (is equal '(:amber :slow) (lamp-of :warp state clock))))

(define-test lamp-rcs
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (is equal '(nil nil) (lamp-of :rcs state clock))
    (setf (lis::clock-level-index clock) 3)
    (is equal '(:amber nil) (lamp-of :rcs state clock))
    (setf (lis::clock-level-index clock) 1)
    (is equal '(nil nil) (lamp-of :rcs state clock))
    (setf (aref (lis::render-state-w state) 0) 0.02d0)
    (is equal '(nil nil) (lamp-of :rcs state clock) "turning under a hold")
    (setf (lis::controls-attitude-mode (lis::render-state-controls state)) :rate-damping)
    (is equal '(:amber nil) (lamp-of :rcs state clock) "turning without a hold")
    (setf (aref (lis::render-state-w state) 0) 0.005d0)
    (is equal '(nil nil) (lamp-of :rcs state clock))))

(define-test lamp-auto
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (let ((model (lis::clock-autopilot clock)))
      (setf (lis::render-state-autopilot-model state) model)
      (is equal '(nil nil) (lamp-of :auto state clock))
      (dolist (planning '(:planning :shown))
        (setf (lis::autopilot-model-state model) planning)
        (is equal '(:white nil) (lamp-of :auto state clock)))
      (setf (lis::autopilot-model-state model) :engaged)
      (is equal '(:green nil) (lamp-of :auto state clock))
      ;; An abort: red and flashing until Y marks the log read.
      (lis::clock-message! clock :autopilot "AUTOPILOT ABORT: UNSAFE ORBIT")
      (setf (lis::autopilot-model-state model) :off
            (lis::autopilot-model-text model) "AUTOPILOT ABORT: UNSAFE ORBIT")
      (is equal '(:red :fast) (lamp-of :auto state clock))
      (lis::clock-acknowledge! clock)
      (is equal '(nil nil) (lamp-of :auto state clock))
      ;; A switched-off plan is no abort.
      (lis::clock-message! clock :autopilot "AUTOPILOT OFF")
      (setf (lis::autopilot-model-text model) "AUTOPILOT OFF")
      (is equal '(nil nil) (lamp-of :auto state clock)))))

(define-test lamp-grpl
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (let ((grapple (lis::render-state-grapple state)))
      (is equal '(nil nil) (lamp-of :grpl state clock))
      (setf (lis::render-grapple-state grapple) :armed)
      (is equal '(:white nil) (lamp-of :grpl state clock))
      (dolist (state-name '(:closing :latched))
        (setf (lis::render-grapple-state grapple) state-name)
        (is equal '(:green nil) (lamp-of :grpl state clock)))
      (setf (lis::render-grapple-state grapple) :armed)
      (lis::clock-message! clock :grapple "NO CAPTURE: TOO FAST")
      (is equal '(:amber :slow) (lamp-of :grpl state clock))
      (setf (lis::clock-real-time clock) 3.5d0)
      (is equal '(:white nil) (lamp-of :grpl state clock))
      ;; A message that is no refusal does not flash it.
      (lis::clock-message! clock :grapple "ARMED")
      (is equal '(:white nil) (lamp-of :grpl state clock))
      (lis::clock-message! clock :grapple "LOAD 90 T OVER 60 T RATING")
      (is equal '(:amber :slow) (lamp-of :grpl state clock))))
  (let ((sim (lis::stated-sim '(:berthed (:tern-station :berth)))))
    (multiple-value-bind (state clock) (screen-state sim)
      (is equal '(:green nil) (lamp-of :grpl state clock)))))

(define-test lamp-eng-and-the-dark-lamps
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (is equal '(nil nil) (lamp-of :eng state clock))
    (setf (lis::controls-throttle (lis::render-state-controls state)) 0.5d0)
    (is equal '(:white nil) (lamp-of :eng state clock))
    (setf (lis::render-state-m-p state) 0d0)
    (is equal '(nil nil) (lamp-of :eng state clock))
    (dolist (key '(:temp :life :msg))
      (is equal '(nil nil) (lamp-of key state clock)))))

(define-test lamp-flashing-phases
  ;; 2 Hz: on at 0.1 s, off at 0.3 s; 1 Hz: on at 0.4 s, off at 0.6 s; paused or not.
  (true (lis::lamp-on-p :fast 0.1d0))
  (false (lis::lamp-on-p :fast 0.3d0))
  (true (lis::lamp-on-p :fast 0.6d0))
  (true (lis::lamp-on-p :slow 0.4d0))
  (false (lis::lamp-on-p :slow 0.6d0))
  (true (lis::lamp-on-p :slow 1.2d0))
  (true (lis::lamp-on-p nil 0.3d0))
  (true (lis::lamp-on-p :fast 12345.1d0)))

(define-test mstr-onsets-and-acknowledgement
  (let ((cockpit (lis::make-cockpit-state)))
    (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
      (flet ((rebuild () (lis::cockpit-lamps-rebuild! cockpit state clock) (lis::cockpit-state-mstr cockpit))
             (lamp (key) (let ((i (position key lis::*lamps* :key #'first)))
                           (list (svref (lis::cockpit-state-lamp-colours cockpit) i)
                                 (svref (lis::cockpit-state-lamp-flashes cockpit) i)))))
        (is eq nil (rebuild))
        (is equal '(nil nil) (lamp :mstr))
        ;; PERI comes on: MSTR flashes red; Y puts it out; the same alarm does not re-arm it.
        (let ((v (copy-seq (lis::render-state-v state))))
          (slow-state state)
          (is eq :red (rebuild))
          (is equal '(:red :fast) (lamp :mstr))
          (is equal '(:red nil) (lamp :peri))
          (lis::cockpit-state-acknowledge! cockpit)
          (is eq nil (rebuild))
          (is equal '(nil nil) (lamp :mstr))
          (is equal '(:red nil) (lamp :peri) "the lamp shows its condition regardless")
          ;; Back to a safe orbit and a second onset: re-armed.
          (replace (lis::render-state-v state) v)
          (is eq nil (rebuild))
          (slow-state state)
          (is eq :red (rebuild)))
        ;; PROP's onset after Y: amber and steady.
        (lis::cockpit-state-acknowledge! cockpit)
        (setf (lis::render-state-m-p state) 1000d0)
        (is eq :amber (rebuild))))
    (let ((cockpit (lis::make-cockpit-state)))
      (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
        (setf (lis::render-state-m-p state) 1000d0)
        (lis::cockpit-lamps-rebuild! cockpit state clock)
        (is eq :amber (lis::cockpit-state-mstr cockpit))
        (is equal :amber (svref (lis::cockpit-state-lamp-colours cockpit) 0))
        (is eq nil (svref (lis::cockpit-state-lamp-flashes cockpit) 0))
        ;; A red onset takes it over; an amber one after does not lower it.
        (slow-state state)
        (lis::cockpit-lamps-rebuild! cockpit state clock)
        (is eq :red (lis::cockpit-state-mstr cockpit))))))

(define-test mstr-abort-onset
  (let ((cockpit (lis::make-cockpit-state)))
    (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
      (let ((model (lis::clock-autopilot clock)))
        (setf (lis::render-state-autopilot-model state) model)
        (lis::cockpit-lamps-rebuild! cockpit state clock)
        (is eq nil (lis::cockpit-state-mstr cockpit))
        (lis::clock-message! clock :autopilot "AUTOPILOT ABORT: UNSAFE ORBIT")
        (setf (lis::autopilot-model-state model) :off
              (lis::autopilot-model-text model) "AUTOPILOT ABORT: UNSAFE ORBIT")
        (lis::cockpit-lamps-rebuild! cockpit state clock)
        (is eq :red (lis::cockpit-state-mstr cockpit))))))

(define-test needle-lags-by-0.3-s
  (let ((cockpit (lis::make-cockpit-state)))
    (multiple-value-bind (state) (screen-state (lis::new-game-sim))
      (setf (lis::render-state-m-p state) 0d0)
      (lis::cockpit-needles-update! cockpit state 10d0 t)
      (is = 0d0 (aref (lis::cockpit-state-needles cockpit) 0))
      (setf (lis::render-state-m-p state) lis::*propellant-full*)
      ;; A frame every 1/60 s; the step from 0 to 100 %.
      (let ((t0 10d0))
        (flet ((after (seconds)
                 (loop for k from 1 to (round (* seconds 60))
                       do (lis::cockpit-needles-update! cockpit state (+ t0 (/ k 60d0)) nil))
                 (aref (lis::cockpit-state-needles cockpit) 0)))
          (let ((at-03 (after 0.3d0)))
            (true (< (abs (- at-03 (- 1 (exp -1d0)))) 0.005d0) "~F at 0.3 s" at-03))
          (let ((at-1 (after 0.7d0)))
            (true (< (abs (- at-1 (- 1 (exp (/ -1d0 0.3d0))))) 0.005d0) "~F at 1 s" at-1)))))
    ;; Snapped in captures; the hull needle at its 150 K stop.
    (multiple-value-bind (state) (screen-state (lis::new-game-sim))
      (lis::cockpit-needles-update! cockpit state 20d0 t)
      (is = (/ 28000d0 lis::*propellant-full*) (aref (lis::cockpit-state-needles cockpit) 0))
      (is = 0d0 (aref (lis::cockpit-state-needles cockpit) 1)))))

(define-test throttle-lever-angles
  (is = -10d0 (lis::lever-angle 0d0))
  (is = 30d0 (lis::lever-angle 0.5d0))
  (is = 60d0 (lis::lever-angle 1d0))
  (is = 6d0 (lis::lever-angle 0.1d0)))

(defun test-devices ()
  (lis::make-cockpit-devices (text-test-atlas) (lis::make-text-batch) (lis::make-line-batch)))

(define-test lamp-engravings-fit-their-lenses
  (let ((devices (test-devices)))
    (dolist (lamp (lis::cockpit-devices-lamps devices))
      (let ((cap (lis::lamp-cap lamp)))
        (true (<= (+ (lis::lamp-advance lamp) 0.002) (* 2 (lis::lamp-half-width lamp)))
              "~A: ~F m on a lens of ~F" (lis::lamp-key lamp) (lis::lamp-advance lamp)
              (* 2 (lis::lamp-half-width lamp)))
        ;; The readability rule's cap at the lens's distance from the eye.
        (let ((d (sqrt (reduce #'+ (map 'list (lambda (x) (* x x)) (lis::plane-frame-centre (lis::lamp-frame lamp)))))))
          (true (>= cap (* 0.0175d0 d)) "~A: cap ~F at ~F m" (lis::lamp-key lamp) cap d))))))

(define-test lamp-lenses-sit-in-front-of-their-faces
  ;; Every lens corner is on the eye's side of the wall (x < 0.90) and of its face.
  (let ((devices (test-devices)))
    (dolist (lamp (lis::cockpit-devices-lamps devices))
      (let* ((frame (lis::lamp-frame lamp)) (c (lis::plane-frame-centre frame))
             (r (lis::plane-frame-r frame)))
        (dolist (sign '(-1 1))
          (let ((x (+ (aref c 0) (* sign (lis::lamp-half-width lamp) (aref r 0)))))
            (true (< x 0.90) "~A lens corner at x ~F" (lis::lamp-key lamp) x)))))))

(define-test devices-draw-lenses-needles-and-lever
  (let* ((devices (test-devices)) (cockpit (lis::make-cockpit-state)))
    (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
      (setf (lis::render-state-m-p state) 1000d0)
      (setf (lis::controls-throttle (lis::render-state-controls state)) 1d0)
      (lis::cockpit-devices-update! devices cockpit state clock 1.1d0 0 t)
      (let* ((region (lis::cockpit-devices-dynamic devices))
             (data (lis::line-batch-data (lis::line-region-batch region)))
             (start (lis::line-region-start region)))
        (flet ((segment (i) (subseq data (* lis::+line-record-floats+ (+ start i))
                                    (* lis::+line-record-floats+ (+ start i 1)))))
          ;; PROP's lens (index 2) lit amber, PERI's (1) the dim red.
          (let ((prop (segment 2)) (peri (segment 1)) (amber (lis::screen-line-colour lis::*screen-amber*)))
            (is = (aref amber 0) (aref prop 6))
            (true (< (aref peri 6) (* 0.1 (aref (lis::screen-line-colour lis::*screen-red*) 0))))
            (is = (* 2 (lis::lamp-half-height (first (lis::cockpit-devices-lamps devices)))) (aref prop 9)))
          ;; Full throttle: the lever's tip 0.1 m from its pivot at 60° from the face's normal.
          (let* ((lever (segment 13))
                 (length (sqrt (+ (expt (- (aref lever 3) (aref lever 0)) 2)
                                  (expt (- (aref lever 4) (aref lever 1)) 2)
                                  (expt (- (aref lever 5) (aref lever 2)) 2)))))
            (true (< (abs (- length 0.10)) 1d-5) "lever ~F m" length))))
      ;; Each frame conses nothing.
      (is = 0 (least-bytes-consed
               (lambda () (dotimes (i 1000) (lis::cockpit-devices-draw! devices cockpit state 2.3d0))))))))

(define-test devices-recolour-the-engraving-with-the-lens
  (let* ((devices (test-devices)) (cockpit (lis::make-cockpit-state))
         (lamp (find :prop (lis::cockpit-devices-lamps devices) :key #'lis::lamp-key)))
    (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
      (flet ((ink () (let* ((region (lis::lamp-region lamp))
                            (data (lis::text-batch-data (lis::text-region-batch region))))
                       (aref data (+ 17 (* lis::+text-record-floats+ (lis::text-region-start region)))))))
        (lis::cockpit-devices-update! devices cockpit state clock 0d0 0 t)
        (is = (aref (lis::screen-ink lis::*screen-grey*) 0) (ink))
        (setf (lis::render-state-m-p state) 1000d0)
        (lis::cockpit-devices-update! devices cockpit state clock 0.5d0 1 t)
        (is = (aref (lis::screen-ink lis::*screen-black*) 0) (ink))))))

(define-test cockpit-lamps-rebuild-at-10-hz-and-on-a-change
  (let* ((devices (test-devices)) (cockpit (lis::make-cockpit-state)))
    (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
      (lis::cockpit-devices-update! devices cockpit state clock 1d0 5 t)
      (setf (lis::render-state-m-p state) 1000d0)
      (lis::cockpit-devices-update! devices cockpit state clock 1.05d0 5 t)
      (is eq nil (svref (lis::cockpit-state-lamp-colours cockpit) 2) "not between rebuilds")
      (lis::cockpit-devices-update! devices cockpit state clock 1.11d0 5 t)
      (is eq :amber (svref (lis::cockpit-state-lamp-colours cockpit) 2))
      (setf (lis::render-state-m-p state) lis::*propellant-full*)
      (lis::cockpit-devices-update! devices cockpit state clock 1.12d0 6 t)
      (is eq nil (svref (lis::cockpit-state-lamp-colours cockpit) 2) "a new signature rebuilds at once"))))

;;; The ATTITUDE screen (plan 32 piece 4; decision 127: Cockpit screens' contents
;;; in milestone 3: lines, modes, plots, colours and updates).

(defun test-attitude-view ()
  (lis::make-attitude-view (lis::make-text-batch) (lis::make-line-batch) (lis::make-plot-batch)))

(defun attitude-face-axes ()
  "(values centre-of-the-plot right up) in the eye's body axes, doubles."
  (multiple-value-bind (c n r u) (lis::panel-frame (lis::cockpit-panel :attitude))
    (declare (ignore n))
    (values (lis::vlin c (getf lis::*attitude-plot* :a) r (getf lis::*attitude-plot* :b) u) r u)))

(defun attitude-marker-point (view index)
  "(values a b): marker INDEX's centre in metres right and up of the plot's centre."
  (let* ((region (lis::attitude-view-markers view))
         (data (lis::text-batch-data (lis::text-region-batch region)))
         (base (* lis::+text-record-floats+ (+ (lis::text-region-start region) index))))
    (multiple-value-bind (centre r u) (attitude-face-axes)
      (let ((p (lis::vec3 (+ (aref data base) (* 0.5 (aref data (+ base 3))) (* 0.5 (aref data (+ base 6))))
                          (+ (aref data (+ base 1)) (* 0.5 (aref data (+ base 4))) (* 0.5 (aref data (+ base 7))))
                          (+ (aref data (+ base 2)) (* 0.5 (aref data (+ base 5))) (* 0.5 (aref data (+ base 8)))))))
        (let ((d (lis::vec3-sub (lis::vec3 (aref p 0) (aref p 1) (aref p 2)) centre)))
          (values (lis::vec3-dot d r) (lis::vec3-dot d u)))))))

(defun attitude-segment (view index)
  "(values from to red width): segment INDEX of the view's lines, vec3 doubles."
  (let* ((region (lis::attitude-view-lines view))
         (data (lis::line-batch-data (lis::line-region-batch region)))
         (base (* lis::+line-record-floats+ (+ (lis::line-region-start region) index))))
    (values (lis::vec3 (aref data base) (aref data (+ base 1)) (aref data (+ base 2)))
            (lis::vec3 (aref data (+ base 3)) (aref data (+ base 4)) (aref data (+ base 5)))
            (aref data (+ base 6)) (aref data (+ base 9)))))

(defun attitude-segment-centre (view index)
  "(values a b) of segment INDEX's middle in metres right and up of the plot's centre."
  (multiple-value-bind (from to) (attitude-segment view index)
    (multiple-value-bind (centre r u) (attitude-face-axes)
      (let ((d (lis::vec3-sub (lis::vec3-scale (lis::vec3-add from to) 0.5d0) centre)))
        (values (lis::vec3-dot d r) (lis::vec3-dot d u))))))

(defun attitude-update (view state)
  (lis::attitude-view-update! view (text-test-atlas) state))

(define-test attitude-plot-projection
  (flet ((point (x y z) (multiple-value-list (lis::attitude-plot-point x y z))))
    ;; The nose is the centre; −z (up in the cab) at 90° is straight up at half the radius.
    (is equalp '(0d0 0d0) (point 1d0 0d0 0d0))
    (let ((p (point 0d0 0d0 -1d0)))
      (true (< (abs (first p)) 1d-12))
      (true (< (abs (- (second p) 0.5d0)) 1d-12)))
    (let ((p (point 0d0 1d0 0d0)))
      (true (< (abs (- (first p) 0.5d0)) 1d-12) "+y is to the right")
      (true (< (abs (second p)) 1d-12)))
    ;; The tail is the rim; 45° off the nose a quarter of the radius.
    (is = 1d0 (second (point -1d0 0d0 0d0)))
    (let ((p (point (cos (/ pi 4)) 0d0 (- (sin (/ pi 4))))))
      (true (< (abs (- (second p) 0.25d0)) 1d-12))))
  ;; The inverse round-trips on 1,000 directions away from the tail.
  (let ((seed 12345) (worst 0d0))
    (flet ((random-unit ()
             (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
             (/ (- (* 2 (/ seed 2147483648d0)) 1) 1)))
      (dotimes (i 1000)
        (let* ((x (random-unit)) (y (random-unit)) (z (random-unit))
               (norm (sqrt (+ (* x x) (* y y) (* z z)))))
          (when (and (> norm 0.1) (> (/ x norm) -0.99))
            (let ((x (/ x norm)) (y (/ y norm)) (z (/ z norm)))
              (multiple-value-bind (px py) (lis::attitude-plot-point x y z)
                (multiple-value-bind (a b c) (lis::attitude-plot-direction px py)
                  (setf worst (max worst (abs (- a x)) (abs (- b y)) (abs (- c z))))))))))
      (true (< worst 1d-9) "round trip off by ~E" worst))))

(define-test attitude-markers-at-the-start-state
  (let ((view (test-attitude-view)) (sim (lis::new-game-sim)))
    (multiple-value-bind (state) (screen-state sim)
      (attitude-update view state)
      (flet ((is-at (index a b what)
               (multiple-value-bind (x y) (attitude-marker-point view index)
                 (true (and (< (abs (- x a)) 5d-4) (< (abs (- y b)) 5d-4))
                       "~A at (~,4F, ~,4F) m, wanted (~,4F, ~,4F)" what x y a b))))
        (let ((half (* 0.5 (getf lis::*attitude-plot* :radius))))
          ;; ○ prograde at the centre, ■ radial out straight up, □ down, ▲ ▼ on the horizontal.
          (is-at 0 0d0 0d0 "prograde")
          (is-at 4 0d0 half "radial out")
          (is-at 5 0d0 (- half) "radial in")
          (multiple-value-bind (a b) (attitude-marker-point view 2)
            (true (and (< (abs b) 5d-4) (< (abs (- (abs a) half)) 5d-4)) "normal at (~F, ~F)" a b))
          (multiple-value-bind (a b) (attitude-marker-point view 3)
            (true (and (< (abs b) 5d-4) (< (abs (- (abs a) half)) 5d-4)) "anti-normal at (~F, ~F)" a b))
          (multiple-value-bind (a) (attitude-marker-point view 2)
            (multiple-value-bind (a2) (attitude-marker-point view 3)
              (true (< (abs (+ a a2)) 1d-5) "▲ and ▼ on opposite sides"))))
        ;; The tail's ⊗ is on the rim, up; no target: ◆ stays blank.
        (multiple-value-bind (x y) (attitude-marker-point view 1)
          (true (and (< (abs x) 1d-3) (< (abs (- y (getf lis::*attitude-plot* :radius))) 1d-3))
                "retrograde at (~F, ~F)" x y))
        (let* ((data (lis::text-batch-data (lis::text-region-batch (lis::attitude-view-markers view))))
               (base (* lis::+text-record-floats+ (+ 6 (lis::text-region-start (lis::attitude-view-markers view))))))
          (true (every #'zerop (subseq data base (+ base lis::+text-record-floats+)))))))))

(define-test attitude-target-diamond-and-hold-cross
  (let ((view (test-attitude-view))
        (sim (lis::stated-sim '(:target :tern-station))))
    (multiple-value-bind (state) (screen-state sim)
      (attitude-update view state)
      (let ((relative (lis::render-state-target-relative state)))
        (true (plusp (aref relative 0)))
        (multiple-value-bind (px py) (lis::attitude-plot-point (aref relative 5) (aref relative 6) (aref relative 7))
          (multiple-value-bind (a b) (attitude-marker-point view 6)
            (let ((radius (getf lis::*attitude-plot* :radius)))
              (true (and (< (abs (- a (* radius px))) 5d-4) (< (abs (- b (* radius py))) 5d-4))
                    "◆ at (~,4F, ~,4F) wanted (~,4F, ~,4F)" a b (* radius px) (* radius py)))))))
    ;; The prograde hold's cross sits on ○ (segments 9 and 10, magenta not).
    (multiple-value-bind (state) (screen-state (lis::new-game-sim))
      (attitude-update view state)
      (multiple-value-bind (a b) (attitude-marker-point view 0)
        (dolist (index '(9 10))
          (multiple-value-bind (ca cb) (attitude-segment-centre view index)
            (true (and (< (abs (- ca a)) 5d-4) (< (abs (- cb b)) 5d-4))
                  "cross ~D centred at (~,4F, ~,4F) on ○ (~,4F, ~,4F)" index ca cb a b)))
        (multiple-value-bind (from to red width) (attitude-segment view 9)
          (declare (ignore from to))
          (true (< red 0.1) "the cross is cyan, not red: ~F" red)
          (true (plusp width))))
      ;; Without a hold the cross goes.
      (setf (lis::controls-attitude-mode (lis::render-state-controls state)) :rate-damping)
      (attitude-update view state)
      (multiple-value-bind (from to red width) (attitude-segment view 9)
        (declare (ignore from to red))
        (is = 0f0 width)))))

(define-test attitude-tick-leans-towards-the-nadir
  ;; At the start state gravity turns the velocity down on the plot.
  (let ((view (test-attitude-view)))
    (multiple-value-bind (state) (screen-state (lis::new-game-sim))
      (attitude-update view state)
      (multiple-value-bind (from to) (attitude-segment view 11)
        (multiple-value-bind (centre r u) (attitude-face-axes)
          (declare (ignore r))
          (let ((a (lis::vec3-dot (lis::vec3-sub from centre) u)) (b (lis::vec3-dot (lis::vec3-sub to centre) u)))
            (true (and (< b a 0) ) "tick from ~F to ~F m below the centre" a b)))))))

(define-test attitude-rate-bars
  (let ((view (test-attitude-view)))
    (multiple-value-bind (state) (screen-state (lis::new-game-sim))
      (lis::vec3-set! (lis::render-state-w state) 0.05d0 -0.02d0 0d0)
      (attitude-update view state)
      (flet ((bar (index)
               ;; (values signed-length red green)
               (multiple-value-bind (from to red) (attitude-segment view (+ 6 index))
                 (multiple-value-bind (centre r u) (attitude-face-axes)
                   (declare (ignore centre r))
                   (let ((d (lis::vec3-sub to from)))
                     (values (lis::vec3-dot d u) red))))))
        (let ((full (getf lis::*attitude-bars* :half))
              (amber-red (aref (lis::screen-line-colour lis::*screen-amber*) 0))
              (green-red (aref (lis::screen-line-colour lis::*screen-green*) 0)))
          ;; P is ω about y, Y about z, R about x: full scale ±5°/s.
          (multiple-value-bind (length red) (bar 0)
            (true (< (abs (- length (* full (/ (* -0.02 (/ 180 pi)) 5)))) 1d-5) "P ~F m" length)
            (is = amber-red red))
          (multiple-value-bind (length red) (bar 1)
            (true (< (abs length) 1d-9))
            (is = green-red red))
          (multiple-value-bind (length red) (bar 2)
            (true (< (abs (- length (* full (/ (* 0.05 (/ 180 pi)) 5)))) 1d-5) "R ~F m" length)
            (is = amber-red red))))
      ;; Beyond full scale the bar stops at the end.
      (lis::vec3-set! (lis::render-state-w state) 0d0 0.5d0 0d0)
      (attitude-update view state)
      (multiple-value-bind (from to) (attitude-segment view 6)
        (multiple-value-bind (centre r u) (attitude-face-axes)
          (declare (ignore centre r))
          (true (< (abs (- (lis::vec3-dot (lis::vec3-sub to from) u) (getf lis::*attitude-bars* :half))) 1d-5)))))))

(define-test attitude-mode-lines
  (let ((sim (lis::new-game-sim)))
    (flet ((mode (&key mutate)
             (let ((lines (nth-value 1 (screen-lines-of :attitude sim :mutate mutate))))
               (values (lis::line-text (nth 7 lines)) (nth 7 lines)))))
      (multiple-value-bind (text line) (mode)
        (is string= "HOLD PRO  SETTLED" text)
        (is = lis::*screen-green* (lis::run-colour (second line))))
      (multiple-value-bind (text line)
          (mode :mutate (lambda (s) (setf (lis::controls-settled (lis::render-state-controls s)) nil)))
        (is string= "HOLD PRO  SLEWING" text)
        (is = lis::*screen-amber* (lis::run-colour (second line))))
      (is string= "DAMP"
          (mode :mutate (lambda (s) (setf (lis::controls-attitude-mode (lis::render-state-controls s)) :rate-damping))))
      (is string= "DIRECT"
          (mode :mutate (lambda (s) (setf (lis::controls-attitude-mode (lis::render-state-controls s)) :direct))))
      (is string= "DAMP +FINE"
          (mode :mutate (lambda (s) (setf (lis::controls-attitude-mode (lis::render-state-controls s)) :rate-damping
                                          (lis::controls-fine (lis::render-state-controls s)) t))))
      (is string= "PRO SETTLED +FINE"
          (mode :mutate (lambda (s) (setf (lis::controls-fine (lis::render-state-controls s)) t))))
      (is string= "HOLD RIN  SETTLED"
          (mode :mutate (lambda (s) (setf (lis::controls-hold (lis::render-state-controls s)) :radial-in))))
      (is string= "AUTOPILOT HOLD PRO"
          (mode :mutate (lambda (s) (setf (lis::render-state-autopilot s) t))))
      (is string= "AUTOPILOT DAMP"
          (mode :mutate (lambda (s) (setf (lis::render-state-autopilot s) t
                                          (lis::controls-attitude-mode (lis::render-state-controls s)) :rate-damping))))
      ;; The bars' letters sit under the bars, columns 13, 15 and 17 of row 7.
      (let ((row (nth 6 (nth-value 1 (screen-lines-of :attitude sim)))))
        (let ((text (lis::line-text row)))
          (is = 13 (position #\P text)) (is = 15 (position #\Y text)) (is = 17 (position #\R text)))))))

(define-test attitude-lines-fit-their-grid
  (destructuring-bind (columns rows) (getf (lis::cockpit-panel :attitude) :grid)
    (dolist (sim (list (lis::new-game-sim) (lis::stated-sim '(:target :tern-station))))
      (dolist (key '(:prograde :retrograde :normal :anti-normal :radial-out :radial-in))
        (dolist (settled '(t nil))
          (dolist (fine '(t nil))
            (multiple-value-bind (texts) (screen-lines-of :attitude sim
                                                           :mutate (lambda (s)
                                                                     (let ((c (lis::render-state-controls s)))
                                                                       (setf (lis::controls-hold c) key
                                                                             (lis::controls-settled c) settled
                                                                             (lis::controls-fine c) fine))))
              (true (<= (length texts) rows))
              (dolist (text texts) (true (<= (length text) columns) "~S" text)))))))
    (dolist (key lis::*hold-abbreviations*)
      (true (<= (length (cdr key)) 4)))))

(define-test attitude-update-conses-nothing
  (let ((view (test-attitude-view)))
    (multiple-value-bind (state) (screen-state (lis::stated-sim '(:target :tern-station)))
      (attitude-update view state)
      (is = 0 (least-bytes-consed (lambda () (dotimes (i 1000) (attitude-update view state)))))
      (setf (lis::controls-attitude-mode (lis::render-state-controls state)) :rate-damping)
      (lis::vec3-set! (lis::render-state-w state) 0.05d0 -0.02d0 0d0)
      (is = 0 (least-bytes-consed (lambda () (dotimes (i 1000) (attitude-update view state)))))
      (setf (lis::controls-attitude-mode (lis::render-state-controls state)) :hold
            (lis::controls-hold (lis::render-state-controls state)) :target)
      (is = 0 (least-bytes-consed (lambda () (dotimes (i 1000) (attitude-update view state))))))))

(define-test attitude-plot-record
  ;; The nadir in body axes and the cosine of the body's angular radius asin(R / r).
  (let ((view (test-attitude-view)))
    (multiple-value-bind (state) (screen-state (lis::new-game-sim))
      (attitude-update view state)
      (let* ((data (lis::plot-batch-data (lis::attitude-view-batch view)))
             (base (* lis::+plot-record-floats+ (lis::attitude-view-record view)))
             (r (+ lis::+radius-earth+ 400000d0)))
        (true (< (abs (- (aref data (+ base 15)) (cos (asin (/ lis::+radius-earth+ r))))) 1f-6))
        ;; the start state's nose is prograde, the nadir +z
        (true (< (abs (- (aref data (+ base 14)) 1)) 1f-5) "nadir z ~F" (aref data (+ base 14)))
        (is = 0.075f0 (aref data (+ base 3)))))))

(define-test attitude-is-a-live-screen
  (true (member :attitude lis::*cockpit-text-screens*))
  (let ((names (mapcar #'lis::screen-view-name (test-screen-views))))
    (true (member :attitude names))))

;;; TARGET and AUTOPILOT (plan 32 piece 5; decision 127)

(defun targeted-sim (key)
  (let ((sim (lis::new-game-sim)))
    (lis::sim-set-target! sim key)
    sim))

(defun format-range-expected (metres)
  (if (>= metres 1000d0) (format nil "~,3F km" (/ metres 1000)) (format nil "~,1F m" metres)))

(define-test target-screen-with-target
  (let ((sim (targeted-sim :tern-station)))
    (multiple-value-bind (state clock) (screen-state sim)
      (let* ((texts (mapcar #'lis::line-text (lis::screen-lines :target state clock 0d0)))
             (rel (lis::render-state-target-relative state)))
        (is string= "TERN STATION" (nth 0 texts))
        (is eq lis::*screen-magenta* (lis::run-colour (first (first (lis::screen-lines :target state clock 0d0)))))
        (is string= (format nil "RNG ~A" (format-range-expected (aref rel 0))) (nth 1 texts))
        (true (eql 0 (search "CLS " (nth 2 texts))) "~S" (nth 2 texts))
        (is string= (format nil "CLS ~A m/s" (lis::signed-fixed (aref rel 1) (if (< (abs (aref rel 1)) 1) 2 1)))
            (nth 2 texts))
        (true (eql 0 (search "F " (nth 3 texts))) "~S" (nth 3 texts))
        (true (search " R " (nth 3 texts)))
        (true (search " D " (nth 3 texts)))
        (multiple-value-bind (time distance) (lis::prediction-closest-approach (lis::clock-prediction clock))
          (if time
              (is string= (format nil "CA ~A ~A" (lis::format-clock-time (- time (lis::render-state-t state)))
                                  (format-range-expected distance))
                  (nth 4 texts))
              (is string= "CA --" (nth 4 texts))))
        (is string= "" (nth 5 texts))
        (is string= "TGT ] NEXT [ PREV" (nth 7 texts))))))

(define-test target-screen-ranges-and-phase
  (let ((sim (targeted-sim :tern-station)))
    (multiple-value-bind (state clock) (screen-state sim)
      (setf (aref (lis::render-state-target-relative state) 0) 340.24d0
            (aref (lis::render-state-target-relative state) 1) 0.5d0)
      (let ((texts (mapcar #'lis::line-text (lis::screen-lines :target state clock 0d0))))
        (is string= "RNG 340.2 m" (nth 1 texts))
        (is string= "CLS +0.50 m/s" (nth 2 texts)))
      (setf (aref (lis::render-state-target-relative state) 0) 12345d0)
      (is string= "RNG 12.345 km" (nth 1 (mapcar #'lis::line-text (lis::screen-lines :target state clock 0d0))))
      ;; station-keeping and the terminal phase, from a plan with a rendezvous part
      (setf (lis::sim-plan sim)
            (lis::make-burn-plan #() :rendezvous (lis::make-rendezvous-part :tern-station nil 100d0 1 1
                                                                            (lis::make-vec3) 0d0 '()))
            (lis::sim-plan-phase sim) :terminal)
      (lis::render-state-update! state clock sim)
      (is string= "TERMINAL" (nth 5 (mapcar #'lis::line-text (lis::screen-lines :target state clock 0d0))))
      (setf (lis::sim-plan-phase sim) :station)
      (is string= "ON STATION 100 m" (nth 5 (mapcar #'lis::line-text (lis::screen-lines :target state clock 0d0)))))))

(define-test target-screen-without-target
  (multiple-value-bind (texts) (screen-lines-of :target (lis::new-game-sim))
    (is string= "NO TARGET" (nth 0 texts))
    (is string= "PICK ] [  OR MAP CLICK" (nth 2 texts))
    (false (nth 3 texts))))

(defun autopilot-screen-of (sim &optional setup (real-time 0d0))
  "(values texts lines state clock model) of AUTOPILOT; SETUP a function of the clock, sim and model."
  (let* ((clock (lis::new-clock sim)) (model (lis::clock-autopilot clock)))
    (when setup (funcall setup clock sim model))
    (let* ((state (lis::render-state-update! (lis::make-render-state) clock sim))
           (lines (lis::screen-lines :autopilot state clock real-time)))
      (values (mapcar #'lis::line-text lines) lines state clock model))))

(defun shown-plan! (clock sim model)
  "MODEL shows a 400 -> 800 km plan with its shot."
  (declare (ignorable clock))
  (let* ((job (lis::new-autopilot-job sim)))
    (lis::autopilot-plan-now sim :change-orbit '(800000d0) job)
    (setf (lis::autopilot-model-plan model) (lis::autopilot-job-plan job)
          (lis::autopilot-model-job model) job)
    (lis::autopilot-keep-shot! model (lis::autopilot-job-plan job) (lis::autopilot-job-shot job))
    (lis::autopilot-set-state! model :shown)))

(define-test autopilot-screen-modes
  (let ((sim (lis::new-game-sim)))
    ;; closed: the plot's rows empty, the presets hint on row 8
    (let ((texts (autopilot-screen-of sim)))
      (is = 8 (length texts))
      (is string= "PRESETS [O]" (nth 7 texts))
      (true (every (lambda (text) (string= "" text)) (subseq texts 0 7))))
    ;; the menu: seven rows, the selection inverted over the grid's width
    (multiple-value-bind (texts lines) (autopilot-screen-of sim (lambda (clock sim model)
                                                                 (lis::autopilot-key! model clock sim :open)))
      (true (eql 0 (search "CIRCULARISE" (nth 0 texts))))
      (is eq :inverted (lis::run-style (first (nth 0 lines))))
      (is = 22 (length (nth 0 texts)))
      (is string= "PLAN [RET]  CLOSE [O]" (nth 7 texts))
      (is = 0 (count-if (lambda (line) (and line (eq :inverted (lis::run-style (first line))))) (subseq lines 1))))
    ;; the window keeps a far selection visible
    (multiple-value-bind (texts lines)
        (autopilot-screen-of sim (lambda (clock sim model)
                                   (lis::autopilot-key! model clock sim :open)
                                   (dotimes (i 9) (lis::autopilot-key! model clock sim :down))))
      (let ((row (position-if (lambda (line) (and line (eq :inverted (lis::run-style (first line))))) lines)))
        (true row)
        (true (< row 7))
        (true (eql 0 (search "RDV " (nth row texts))) "~S" (nth row texts))
        (dolist (text texts) (false (search "…" text) "~S" text))))
    ;; a refusal, planning
    (multiple-value-bind (texts lines)
        (autopilot-screen-of sim (lambda (clock sim model)
                                   (lis::autopilot-key! model clock sim :open)
                                   (lis::autopilot-set-state! model :refused "BELOW 150 KM")))
      (is string= "! BELOW 150 KM" (nth 7 texts))
      (is = lis::*screen-amber* (lis::run-colour (first (nth 7 lines)))))
    (let ((texts (autopilot-screen-of sim (lambda (clock sim model)
                                            (declare (ignore clock sim))
                                            (lis::autopilot-set-state! model :planning "PLANNING…")))))
      (is string= "PLANNING…" (nth 0 texts)))))

(define-test autopilot-screen-plan-and-states
  (let ((sim (lis::new-game-sim)))
    (multiple-value-bind (texts lines state clock model) (autopilot-screen-of sim #'shown-plan!)
      (declare (ignore lines))
      (let ((summary (lis::autopilot-plan-summary (lis::autopilot-model-plan model) sim)))
        (is string= (format nil "BURN 1/~D ~A" (getf summary :burns) (lis::format-clock-time (getf summary :next-in)))
            (nth 0 texts))
        (is string= (format nil "DV ~,1F m/s  ~D s" (getf summary :next-dv) (round (getf summary :next-duration)))
            (nth 1 texts))
        (is string= (format nil "TOTAL ~D m/s ~D kg" (round (getf summary :total-dv)) (round (getf summary :propellant)))
            (nth 2 texts))
        (true (eql 0 (search (format nil "LEFT ~,1F t  ARR " (/ (getf summary :left) 1000)) (nth 3 texts)))
              "~S" (nth 3 texts)))
      (is string= "RET ENGAGE  BKSP DROP" (nth 7 texts))
      (dolist (text texts) (true (<= (length text) 22) "~S" text))
      ;; engaged
      (lis::autopilot-set-state! model :engaged)
      (setf (lis::autopilot-model-engaged model) t
            (lis::autopilot-model-flown model) (lis::autopilot-model-plan model))
      (let ((texts (mapcar #'lis::line-text (lis::screen-lines :autopilot state clock 0d0))))
        (is string= "ENGAGED  OFF [BKSP]" (nth 7 texts))))))

(define-test autopilot-screen-abort
  (let ((sim (lis::new-game-sim)))
    (flet ((setup (clock sim model)
             (declare (ignore sim))
             (lis::clock-message! clock :autopilot "AUTOPILOT: ABORT: LOW PROPELLANT")
             (lis::autopilot-set-state! model :off "AUTOPILOT: ABORT: LOW PROPELLANT")))
      (multiple-value-bind (texts lines state clock) (autopilot-screen-of sim #'setup)
        (is string= "ABORT: LOW PROPELLANT" (nth 7 texts))
        (is = lis::*screen-red* (lis::run-colour (first (nth 7 lines))))
        (is eq :inverted (lis::run-style (first (nth 7 lines))))
        (lis::clock-acknowledge! clock)
        (is string= "PRESETS [O]"
            (nth 7 (mapcar #'lis::line-text (lis::screen-lines :autopilot state clock 0d0))))))))

;;; The orbit plot

(defun plot-segment-face (plot index)
  "(values a0 b0 a1 b1 width) of segment INDEX of PLOT in the face's right and up metres."
  (let* ((region (lis::autopilot-plot-region plot)) (frame (lis::autopilot-plot-frame plot))
         (data (lis::line-batch-data (lis::line-region-batch region)))
         (base (* lis::+line-record-floats+ (+ (lis::line-region-start region) index)))
         (c (lis::plane-frame-centre frame)) (r (lis::plane-frame-r frame)) (u (lis::plane-frame-u frame)))
    (flet ((coord (offset axis)
             (loop for k below 3 sum (* (- (aref data (+ base offset k)) (aref c k)) (aref axis k)))))
      (values (coord 0 r) (coord 0 u) (coord 3 r) (coord 3 u) (aref data (+ base 9))))))

(defun plot-of (sim &optional setup)
  "(values plot state clock model) with the plot drawn for SIM."
  (let* ((clock (lis::new-clock sim)) (model (lis::clock-autopilot clock))
         (plot (lis::make-autopilot-plot (lis::make-line-batch))))
    (lis::prediction-run! (lis::clock-prediction clock) sim)
    (when setup (funcall setup clock sim model))
    (let ((state (lis::render-state-update! (lis::make-render-state) clock sim)))
      (lis::autopilot-plot-segments! plot state)
      (values plot state clock model))))

(define-test autopilot-plot-start-orbit
  (multiple-value-bind (plot) (plot-of (lis::new-game-sim))
    (let* ((unit (lis::autopilot-plot-unit plot)) (hh (lis::autopilot-plot-hh plot))
           (radius (+ lis::+radius-earth+ 400000d0)))
      (is = 1 (lis::autopilot-plot-runs plot))
      (true (< (abs (- (* 0.9d0 hh) (* unit radius))) 1d-6))
      ;; every point of the path on the circle at 90 % of the half-height
      (loop for i from (aref (lis::autopilot-plot-run-start plot) 0) below (aref (lis::autopilot-plot-run-end plot) 0)
            do (let ((d (* unit (sqrt (+ (expt (aref (lis::autopilot-plot-px plot) i) 2)
                                          (expt (aref (lis::autopilot-plot-py plot) i) 2))))))
                 (true (< (abs (- d (* 0.9d0 hh))) 0.0005d0) "point ~D: ~F against ~F" i d (* 0.9d0 hh))))
      ;; every segment inside the plot's rectangle
      (dotimes (k (lis::autopilot-plot-segments plot))
        (multiple-value-bind (a0 b0 a1 b1) (plot-segment-face plot k)
          (dolist (pair (list (cons a0 b0) (cons a1 b1)))
            (true (<= (abs (- (car pair) (lis::autopilot-plot-cx plot))) (+ (lis::autopilot-plot-hw plot) 1e-5)))
            (true (<= (abs (- (cdr pair) (lis::autopilot-plot-cy plot))) (+ (lis::autopilot-plot-hh plot) 1e-5))))))
      ;; a circular orbit has no apsides to mark
      (false (lis::autopilot-plot-mark-p plot)))))

(define-test autopilot-plot-eccentric-orbit-and-target
  (let ((sim (lis::new-game-sim)))
    ;; 400 x 1000 km: speed up at the start radius
    (let* ((r (lis::sim-ship sim))
           (mu lis::+mu-earth+) (rr (lis::vec3-norm (lis::ship-state-r r)))
           (a (/ (+ rr (+ lis::+radius-earth+ 1000000d0)) 2))
           (speed (sqrt (* mu (- (/ 2 rr) (/ 1 a))))))
      (lis::vec3-scale! (lis::ship-state-v r) (lis::ship-state-v r)
                        (/ speed (lis::vec3-norm (lis::ship-state-v r))))
      (multiple-value-bind (plot) (plot-of sim)
        (is eq :both (lis::autopilot-plot-mark-p plot))
        (let* ((m (lis::autopilot-plot-marks plot))
               (ap (sqrt (+ (expt (aref m 0) 2) (expt (aref m 1) 2))))
               (pe (sqrt (+ (expt (aref m 2) 2) (expt (aref m 3) 2)))))
          (true (< (abs (- ap (+ lis::+radius-earth+ 1000000d0))) 1d-3) "Ap ~F" ap)
          (true (< (abs (- pe (+ lis::+radius-earth+ 400000d0))) 1d-3) "Pe ~F" pe)
          ;; on the major axis, opposite each other
          (true (< (abs (- (* (aref m 0) (aref m 3)) (* (aref m 1) (aref m 2)))) 1d-3)))))
    ;; the target's orbit projected into the plane, magenta, apart from the scale
    (let ((target-sim (targeted-sim :tern-station)))
      (multiple-value-bind (plot state) (plot-of target-sim)
        (is = 2 (lis::autopilot-plot-runs plot))
        (is eq :target (svref (lis::autopilot-plot-run-kind plot) 1))
        (let* ((i (aref (lis::autopilot-plot-run-start plot) 1))
               (px (aref (lis::autopilot-plot-px plot) i)) (py (aref (lis::autopilot-plot-py plot) i))
               (target-r (lis::render-state-target-r state))
               (out (lis::vec3-dot target-r (lis::autopilot-plot-eh plot))))
          (declare (ignorable out))
          ;; its distance from the plane's centre is the target orbit's own radius, less the normal part
          (true (<= (sqrt (+ (* px px) (* py py))) (+ 1d-6 (* 1.5d0 (lis::vec3-norm target-r))))))))))

(define-test autopilot-plot-plan-and-menu
  (multiple-value-bind (plot) (plot-of (lis::new-game-sim) #'shown-plan!)
    (is eq :third (lis::autopilot-plot-mode plot))
    (true (plusp (lis::autopilot-plot-ticks plot)) "the burns' ticks")
    ;; the dashed run after the first burn
    (true (find :dashed (lis::autopilot-plot-run-kind plot) :end (lis::autopilot-plot-runs plot)))
    ;; the plot fills only the third: rows 5 to 7
    (dotimes (k (lis::autopilot-plot-segments plot))
      (multiple-value-bind (a0 b0 a1 b1) (plot-segment-face plot k)
        (declare (ignore a0 a1))
        (true (<= (abs (- b0 (lis::autopilot-plot-cy plot))) (+ (lis::autopilot-plot-hh plot) 1e-5)))
        (true (<= (abs (- b1 (lis::autopilot-plot-cy plot))) (+ (lis::autopilot-plot-hh plot) 1e-5))))))
  (multiple-value-bind (plot) (plot-of (lis::new-game-sim)
                                       (lambda (clock sim model) (lis::autopilot-key! model clock sim :open)))
    (is eq nil (lis::autopilot-plot-mode plot))
    (is = 0 (lis::autopilot-plot-segments plot))))

(define-test autopilot-rebuild-conses-little
  (let* ((sim (targeted-sim :tern-station))
         (clock (lis::new-clock sim)) (batch (lis::make-text-batch)) (lines (lis::make-line-batch))
         (atlas (text-test-atlas))
         (view (lis::make-screen-view :autopilot batch lines))
         (state (lis::render-state-update! (lis::make-render-state) clock sim)))
    (lis::prediction-run! (lis::clock-prediction clock) sim)
    (flet ((rebuild ()
             (lis::screen-view-draw! view atlas (lis::screen-lines :autopilot state clock 0d0))
             (lis::autopilot-plot-update! (lis::screen-view-plot view) state)))
      (rebuild) (rebuild)
      (let ((consed (least-bytes-consed #'rebuild 5)))
        (true (< consed 16384) "~D bytes" consed)))))

;;; The buttons, picking and the hover (plan 32 piece 6; decision 128: Cockpit
;;; devices, buttons, picking, the grapple camera and messages in milestone 3).

(defun test-buttons ()
  (lis::make-cockpit-buttons (text-test-atlas) (lis::make-text-batch) (lis::make-line-batch)))

(define-test button-engravings-fit-their-lenses
  (let ((buttons (test-buttons)))
    (is = 18 (length (lis::cockpit-buttons-lenses buttons)))
    (dolist (lens (lis::cockpit-buttons-lenses buttons))
      (let* ((c (lis::plane-frame-centre (lis::button-lens-frame lens)))
             (d (sqrt (reduce #'+ (map 'list (lambda (x) (* x x)) c)))))
        (true (<= (+ (lis::button-lens-advance lens) 0.004) lis::*button-lens*)
              "~A: ~F m on a lens of ~F" (third (lis::button-lens-entry lens))
              (lis::button-lens-advance lens) lis::*button-lens*)
        (true (>= (lis::button-lens-cap lens) (* 0.0175d0 d)))))))

(defun project-pixel (p view width height)
  "The pixel of the eye-axes point P in the cockpit camera of VIEW."
  (let* ((q (lis::make-quat)) (v (lis::make-vec3)))
    (lis::cockpit-head-quat! q view)
    (lis::quat-rotate! v (lis::quat-conjugate q) p)
    (let ((tan-half (tan (* 0.5d0 (lis::cockpit-view-fov view)))) (aspect (/ width height 1d0)))
      (values (* 0.5d0 width (+ 1d0 (/ (aref v 1) (aref v 0) tan-half aspect)))
              (* 0.5d0 height (+ 1d0 (/ (aref v 2) (aref v 0) tan-half)))))))

(defun clickable-pixel (id view width height &optional (index 0))
  (project-pixel (lis::clickable-centre (lis::clickable-for id index)) view width height))

(defun turned-view ()
  (let ((view (lis::new-cockpit-view)))
    (setf (lis::cockpit-view-yaw view) (lis::degrees-to-radians 20)
          (lis::cockpit-view-fov view) (lis::degrees-to-radians 40))
    view))

(define-test cockpit-pick-hits-every-button-mstr-and-a-menu-row
  (dolist (view (list (lis::new-cockpit-view) (turned-view)))
    (dolist (id (append (mapcar #'first lis::*buttons*) '(:mstr :comms)))
      (multiple-value-bind (x y) (clickable-pixel id view 1280 800)
        (is eq id (lis::cockpit-pick view 1280 800 x y) "~A" id)))
    (multiple-value-bind (x y) (clickable-pixel :menu-row view 1280 800 3)
      (multiple-value-bind (id index) (lis::cockpit-pick view 1280 800 x y :menu-rows 7)
        (is eq :menu-row id)
        (is = 3 index))
      (is eq nil (lis::cockpit-pick view 1280 800 x y :menu-rows 3) "a row not shown"))))

(define-test cockpit-pick-misses-beyond-a-lens-edge
  (let ((view (lis::new-cockpit-view)))
    (dolist (id '(:pro :nml :tgt :inrt :xfer :ap))
      (let* ((c (lis::clickable-for id)) (hw (lis::clickable-half-width c)))
        (multiple-value-bind (cx cy) (project-pixel (lis::clickable-centre c) view 1280 800)
          (multiple-value-bind (ex ey)
              (project-pixel (lis::vlin (lis::clickable-centre c) hw (lis::clickable-right c)) view 1280 800)
            (let* ((dx (- ex cx)) (dy (- ey cy)) (n (sqrt (+ (* dx dx) (* dy dy)))))
              (is eq id (lis::cockpit-pick view 1280 800 (- ex (/ dx n)) (- ey (/ dy n)))
                  "just inside ~A" id)
              (is eq nil (lis::cockpit-pick view 1280 800 (+ ex (/ dx n)) (+ ey (/ dy n)))
                  "1 px outside ~A" id))))))))

(define-test cockpit-pick-takes-the-nearer-rectangle
  (let* ((far (lis::make-clickable :id :far :centre (lis::vec3 -0.9d0 0d0 0d0)
                                   :normal (lis::vec3 1d0 0d0 0d0) :right (lis::vec3 0d0 1d0 0d0)
                                   :up (lis::vec3 0d0 0d0 1d0) :half-width 0.5d0 :half-height 0.5d0))
         (near (lis::make-clickable :id :near :centre (lis::vec3 -0.5d0 0d0 0d0)
                                    :normal (lis::vec3 1d0 0d0 0d0) :right (lis::vec3 0d0 1d0 0d0)
                                    :up (lis::vec3 0d0 0d0 1d0) :half-width 0.1d0 :half-height 0.1d0))
         (view (lis::new-cockpit-view)))
    ;; Facing -x from the eye: the rectangles stand at x = -0.5 and -0.9, their normals to the eye.
    (setf (lis::cockpit-view-yaw view) pi (lis::cockpit-view-pitch view) 0d0)
    (is eq :near (lis::cockpit-pick view 1280 800 640 400 :clickables (vector far near)))
    (is eq :near (lis::cockpit-pick view 1280 800 640 400 :clickables (vector near far)))
    (is eq :far (lis::cockpit-pick view 1280 800 640 200 :clickables (vector far near)))))

(define-test button-lighting
  (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
    (flet ((colour (id) (lis::button-colour (find id lis::*buttons* :key #'first) state clock))
           (controls () (lis::render-state-controls state)))
      (is eq :green (colour :pro) "prograde settled")
      (setf (lis::controls-settled (controls)) nil)
      (is eq :amber (colour :pro) "slewing")
      (is eq nil (colour :retr))
      (is eq nil (colour :damp))
      (setf (lis::controls-attitude-mode (controls)) :rate-damping)
      (is eq :green (colour :damp))
      (is eq nil (colour :pro))
      (setf (lis::controls-fine (controls)) t)
      (is eq :green (colour :fine))
      (setf (lis::render-state-transfer state) t)
      (is eq :green (colour :xfer))
      (is eq nil (colour :wrp+))
      (setf (lis::clock-level-index clock) 2)
      (is eq :white (colour :wrp+))
      (is eq nil (colour :wrp-))
      (is eq nil (colour :ap)))))

(define-test button-press-flashes-the-lens-white
  (let* ((buttons (test-buttons)) (cockpit (lis::make-cockpit-state))
         (region (lis::cockpit-buttons-lens-region buttons))
         (data (lis::line-batch-data (lis::line-region-batch region))))
    (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
      (flet ((red (i) (aref data (+ 6 (* lis::+line-record-floats+ (+ (lis::line-region-start region) i))))))
        (lis::cockpit-buttons-draw! buttons cockpit state clock 10d0)
        (let ((idle (red 4)))
          (lis::button-press! cockpit :|5| 10.5d0)
          (lis::cockpit-buttons-draw! buttons cockpit state clock 10.6d0)
          (is = (aref (lis::screen-line-colour lis::*screen-white*) 0) (red 4))
          (lis::cockpit-buttons-draw! buttons cockpit state clock 10.7d0)
          (is = idle (red 4) "after 0.15 s"))))))

(define-test hover-outline-is-drawn-around-the-lens
  (let* ((buttons (test-buttons)) (cockpit (lis::make-cockpit-state))
         (region (lis::cockpit-buttons-outline-region buttons))
         (data (lis::line-batch-data (lis::line-region-batch region))))
    (multiple-value-bind (state clock) (screen-state (lis::new-game-sim))
      (flet ((width (i) (aref data (+ 9 (* lis::+line-record-floats+ (+ (lis::line-region-start region) i))))))
        (lis::cockpit-buttons-draw! buttons cockpit state clock 1d0)
        (is = 0 (width 0))
        (setf (lis::cockpit-state-hover-id cockpit) :pro
              (lis::cockpit-state-pixel-scale cockpit) (/ (* 2 (tan (lis::degrees-to-radians 35))) 800))
        (lis::cockpit-buttons-draw! buttons cockpit state clock 1d0)
        (let ((distance (lis::vec3-norm (lis::clickable-centre (lis::clickable-for :pro)))))
          (true (< (abs (- (width 2) (* 2 distance (lis::cockpit-state-pixel-scale cockpit)))) 1d-6)))
        (setf (lis::cockpit-state-hover-id cockpit) nil)
        (lis::cockpit-buttons-draw! buttons cockpit state clock 1d0)
        (is = 0 (width 0))))))

(define-test button-hover-text-names-the-key
  (is string= "PRO [1]" (lis::button-hover-text :pro))
  (is string= "WRP- [,]" (lis::button-hover-text :wrp-))
  (is eq nil (lis::button-hover-text :mstr)))

;;; The grapple camera (plan 32 piece 7; decision 128, decision 127)

(defun on-axis-state-sim (&optional (metres 20))
  "The ship with the grapple's tip METRES in front of Skua Yard 3's fixture, on its axis."
  (lis::stated-sim `(:orbit (:on-axis :skua-yard-3 ,metres))))

(define-test grapple-camera-sits-at-the-tip
  (let* ((sim (lis::new-game-sim))
         (state (sim-render-state sim))
         (camera (lis::make-camera))
         (axis (lis::vec3-normalize (lis::vec3 0.3d0 -0.5d0 0.8d0)))
         (q (lis::quat-from-axis-angle axis (lis::deg->rad 37d0))))
    (replace (lis::ship-state-q (lis::sim-ship sim)) q)
    (lis::render-state-update! state (lis::new-clock sim) sim)
    (lis::grapple-camera-update! camera state)
    (let ((expected (lis::quat-rotate (lis::render-state-q state)
                                      (lis::vec3-sub lis::*grapple-tip* (lis::render-state-com state)))))
      (dotimes (i 3)
        (true (< (abs (- (aref expected i) (aref (lis::camera-offset camera) i))) 1d-9)
              "offset ~D: ~F against ~F" i (aref (lis::camera-offset camera) i) (aref expected i))))
    (dotimes (i 4)
      (true (< (abs (- (aref (lis::render-state-q state) i) (aref (lis::camera-orientation camera) i))) 1d-12)))
    (is = (lis::deg->rad 50d0) (lis::camera-field-of-view camera))
    (is = 0.1d0 (lis::camera-near camera))
    (is eq :ship (lis::camera-anchor camera))))

(define-test grapple-pin-projection-matches-the-clip-rows
  ;; The pin's pixel against camera-clip-rows! of the grapple camera, a ship
  ;; turned a few degrees off the axis so the pin is off centre.
  (let* ((sim (on-axis-state-sim 20))
         (ship (lis::sim-ship sim))
         (turn (lis::quat-from-axis-angle (lis::vec3 0d0 0.6d0 0.8d0) (lis::deg->rad 3d0))))
    (replace (lis::ship-state-q ship) (lis::quat-mul (lis::ship-state-q ship) turn))
    (let* ((state (sim-render-state sim))
           (camera (lis::make-camera))
           (reading (lis::make-grapple-reading))
           (rows (make-array 16 :element-type 'single-float)))
      (lis::grapple-camera-update! camera state)
      (true (lis::grapple-range-into! reading sim))
      (multiple-value-bind (px py) (lis::grapple-pin-pixel reading)
        (true px)
        (let* ((object (lis::sim-object sim :skua-yard-3))
               (fixture (first (lis::object-def-fixtures (lis::object-def-of object))))
               (o (lis::object-state object))
               (pin (lis::vec3-add (lis::ship-state-r o) (lis::quat-rotate (lis::ship-state-q o) (lis::fixture-pin fixture))))
               (eye (lis::vec3-add (lis::ship-state-r ship) (lis::camera-offset camera)))
               (p (lis::vec3-sub pin eye))
               (width 480) (height 320))
          (lis::camera-clip-rows! rows camera (/ width height))
          (flet ((row (i) (+ (* (aref rows (* 4 i)) (aref p 0)) (* (aref rows (+ 1 (* 4 i))) (aref p 1))
                             (* (aref rows (+ 2 (* 4 i))) (aref p 2)))))
            (let* ((w (row 3)) (x (* 0.5 (+ 1 (/ (row 0) w)) width)) (y (* 0.5 (+ 1 (/ (row 1) w)) height)))
              (true (< (abs (- x px)) 0.01) "x ~F against ~F" px x)
              (true (< (abs (- y py)) 0.01) "y ~F against ~F" py y)
              (true (> (abs (- px (/ width 2))) 5) "off centre: ~F" px))))))))

(defun crafted-reading (&key (range 12.4d0) (closing 0.12d0) (offset-y 0.05d0) (offset-z -0.10d0)
                          (angle 2.1d0) (rotation 0.1d0))
  "A grapple-reading with the given values (degrees for the angle, degrees a second for the rotation), its flags judged."
  (lis::grapple-reading-judge!
   (lis::make-grapple-reading :range range :closing-speed closing :offset-y offset-y :offset-z offset-z
                              :angle (lis::deg->rad angle) :rotation (lis::deg->rad rotation))))

(defun armed-lines-of (reading)
  (let ((state (sim-render-state (lis::new-game-sim))))
    (setf (lis::render-grapple-state (lis::render-state-grapple state)) :armed)
    (lis::grapple-armed-lines state reading)))

(defun run-colours-of (line text)
  "The colour of the run of LINE whose text contains TEXT."
  (lis::run-colour (find text line :key #'lis::run-text :test #'search)))

(define-test grapple-armed-lines
  (let ((lines (armed-lines-of (crafted-reading))))
    (is = 8 (length lines))
    (is string= "RNG 12.4 m CLS 0.12 m/s" (lis::line-text (nth 0 lines)))
    (is string= "OFF Y+0.05 Z-0.10" (lis::line-text (nth 1 lines)))
    (is string= "ANG 2.1° ROT 0.1°/s" (lis::line-text (nth 2 lines)))
    (is string= "ARMED" (lis::line-text (nth 7 lines)))
    (dolist (n '(0 1 2 7)) (is eq :band (lis::run-style (first (nth n lines)))))
    (dolist (text '("0.12 m/s" "Y+0.05" "2.1°" "0.1°/s"))
      (let ((line (find-if (lambda (l) (find text l :key #'lis::run-text :test #'search)) lines)))
        (is = lis::*screen-green* (run-colours-of line text) "~A is green inside" text))))
  ;; each edge of decision 33's window: inside green, outside amber with "!"
  (flet ((check (reading row text inside)
           (let* ((line (nth row (armed-lines-of reading)))
                  (run (find text line :key #'lis::run-text :test #'search))
                  (bang (some (lambda (r) (string= (lis::run-text r) "!")) line)))
             (if inside
                 (progn (is = lis::*screen-green* (lis::run-colour run) "~A" (lis::line-text line))
                        (false bang "~A" (lis::line-text line)))
                 (progn (is = lis::*screen-amber* (lis::run-colour run) "~A" (lis::line-text line))
                        (true bang "~A" (lis::line-text line)))))))
    (check (crafted-reading :closing 0.30d0) 0 "m/s" t)
    (check (crafted-reading :closing 0.3001d0) 0 "m/s" nil)
    (check (crafted-reading :closing 0.01d0) 0 "m/s" t)
    (check (crafted-reading :closing 0.0099d0) 0 "m/s" nil)
    (check (crafted-reading :closing -0.05d0) 0 "m/s" nil)
    (check (crafted-reading :offset-y 0.18d0 :offset-z 0.24d0) 1 "Y" t)
    (check (crafted-reading :offset-y 0.18d0 :offset-z 0.2425d0) 1 "Y" nil)
    (check (crafted-reading :angle 10d0) 2 "°" t)
    (check (crafted-reading :angle 10.01d0) 2 "°" nil)
    (check (crafted-reading :rotation 0.5d0) 2 "°/s" t)
    (check (crafted-reading :rotation 0.51d0) 2 "°/s" nil)))

(define-test grapple-screen-row-8-per-state
  (flet ((row-8 (sim &optional (state :armed))
           (let ((render (sim-render-state sim)))
             (setf (lis::grapple-state (lis::sim-grapple sim)) state)
             (lis::render-state-update! render (lis::new-clock sim) sim)
             (lis::line-text (nth 7 (lis::screen-lines :target render (lis::new-clock sim) 0d0))))))
    (is string= "ARMED" (row-8 (on-axis-state-sim 20)))
    (is string= "NO FIXTURE IN 500 m" (row-8 (lis::new-game-sim)))
    (is string= "CLOSING" (row-8 (on-axis-state-sim 5) :closing))
    (let ((sim (lis::stated-sim '(:grappled :pod-14))))
      (is string= "LATCHED POD 14" (row-8 sim :latched)))
    (let ((sim (lis::stated-sim '(:berthed (:tern-station :berth)))))
      (true (eql 0 (search "BERTHED " (row-8 sim :latched))) "~S" (row-8 sim :latched))
      (is eq lis::*screen-green* (lis::run-colour (second (nth 7 (lis::screen-lines :target (sim-render-state sim) (lis::new-clock sim) 0d0))))))
    ;; stowed: the stowed lines
    (is string= "NO TARGET" (first (screen-lines-of :target (lis::new-game-sim))))))

(defun grapple-view-segment-widths (view)
  (let* ((region (lis::grapple-view-lines view))
         (data (lis::line-batch-data (lis::line-region-batch region))))
    (loop for k below 4
          collect (aref data (+ (* lis::+line-record-floats+ (+ (lis::line-region-start region) k)) 9)))))

(define-test grapple-view-draws-only-while-active
  (let* ((sim (on-axis-state-sim 20))
         (state (sim-render-state sim))
         (view (lis::make-grapple-view (lis::make-text-batch) (lis::make-line-batch)))
         (atlas (text-test-atlas))
         (pin (lis::grapple-view-pin view))
         (data (lis::text-batch-data (lis::text-region-batch pin)))
         (base (* lis::+text-record-floats+ (lis::text-region-start pin))))
    (flet ((pin-drawn () (some #'plusp (subseq data base (+ base lis::+text-record-floats+)))))
      (lis::grapple-view-update! view atlas state)
      (true (every #'zerop (grapple-view-segment-widths view)) "stowed: no crosshair")
      (false (pin-drawn))
      (setf (lis::render-grapple-state (lis::render-state-grapple state)) :armed)
      (lis::grapple-view-update! view atlas state)
      (true (every #'plusp (grapple-view-segment-widths view)) "armed: the crosshair's four segments")
      (true (pin-drawn) "armed with a fixture: the pin's ◆")
      (setf (lis::render-grapple-state (lis::render-state-grapple state)) :latched)
      (lis::grapple-view-update! view atlas state)
      (true (every #'plusp (grapple-view-segment-widths view)))
      (false (pin-drawn) "latched: no pin")
      (setf (lis::render-grapple-state (lis::render-state-grapple state)) :stowed)
      (lis::grapple-view-update! view atlas state)
      (true (every #'zerop (grapple-view-segment-widths view))))))

(define-test grapple-view-update-conses-nothing
  (let* ((sim (on-axis-state-sim 20))
         (state (sim-render-state sim))
         (view (lis::make-grapple-view (lis::make-text-batch) (lis::make-line-batch)))
         (atlas (text-test-atlas)))
    (setf (lis::render-grapple-state (lis::render-state-grapple state)) :armed)
    (lis::grapple-view-update! view atlas state)
    (is = 0 (least-bytes-consed (lambda () (dotimes (i 100) (lis::grapple-view-update! view atlas state)))))))

(define-test no-placeholder-166
  ;; The instruments are all drawn: no PLACEHOLDER(166) stays in src/.
  (let ((hits '()))
    (dolist (file (directory (merge-pathnames "src/*.lisp" (asdf:system-source-directory :luv-in-space))))
      (with-open-file (in file)
        (loop for line = (read-line in nil) while line
              when (search "PLACEHOLDER(166)" line) do (push (file-namestring file) hits))))
    (false hits "PLACEHOLDER(166) in ~{~A~^ ~}" hits)))

(define-test autopilot-screen-shares-plan-lines
  ;; decision 129: the panel and the screen call the same autopilot-plan-lines
  (let ((sim (lis::new-game-sim)))
    (multiple-value-bind (texts lines state clock model) (autopilot-screen-of sim #'shown-plan!)
      (declare (ignore lines state clock))
      (is equal (mapcar #'lis::line-text (lis::autopilot-plan-lines (lis::autopilot-model-plan model) sim))
          (subseq texts 0 4)))))
