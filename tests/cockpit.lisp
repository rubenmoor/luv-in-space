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
;;; bezels, brackets, cables, labels and the screens' placeholders.

(defparameter *layout-table*
  ;; name (dx dy dz) (w h) grid, docs/design.md "Layout".
  '((:caution-lamps (0.90 -0.72 0) (0.08 0.58) nil)
    (:status-lamps (0.90 0.72 0) (0.08 0.58) nil)
    (:orbit (0.60 -0.28 0.35) (0.30 0.20) (24 8))
    (:attitude (0.67 0 0.35) (0.24 0.20) (19 8))
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
            (true (<= (* rows lis::+text-cell-height+ cap) (+ (- h 0.02d0) 1d-9)))
            (true (>= columns (length lis::*placeholder-text*)) "~A holds the placeholder" (getf panel :name))))))))

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
  ;; cockpit-text! writes the labels and one placeholder a screen.
  (let* ((atlas (text-test-atlas)) (batch (lis::make-text-batch)))
    (lis::cockpit-text! atlas batch)
    (dolist (panel lis::*cockpit-panels*)
      (if (getf panel :screen)
          (let ((region (lis::cockpit-text-region (list :placeholder (getf panel :name)))))
            (true region "~A has no placeholder" (getf panel :name))
            (is = (length lis::*placeholder-text*) (lis::text-region-capacity region)))
          (false (lis::cockpit-text-region (list :placeholder (getf panel :name))))))
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
