(in-package #:luv-in-space.tests)

;;; meshes.lisp, CPU part: the generators, instances, the Mule's parts, the
;;; engine's trim, the decal and the bounding radius (decision 80: The
;;; Mule's meshes in numbers: geometry, draws, materials and lights).

(defvar *mule* nil)
(defun mule () (or *mule* (setf *mule* (lis::mule-model))))

(defun mesh-triangles (floats)
  "((p n) (p n) (p n)) per triangle of a vertex vector, as vec3s."
  (loop for o from 0 below (length floats) by (* 3 lis::+vertex-floats+)
        collect (loop for k below 3
                      for v = (+ o (* k lis::+vertex-floats+))
                      collect (list (lis::vec3 (aref floats v) (aref floats (+ v 1)) (aref floats (+ v 2)))
                                    (lis::vec3 (aref floats (+ v 3)) (aref floats (+ v 4))
                                               (aref floats (+ v 5)))))))

(defun generated (generator &rest args)
  (let ((out (lis::make-float-buffer)))
    (apply generator out args)
    (lis::simple-floats out)))

(defun check-closed (floats centre)
  "Unit normals; each triangle's mean normal points away from CENTRE."
  (dolist (triangle (mesh-triangles floats))
    (dolist (v triangle)
      (true (< (abs (- (lis::vec3-norm (second v)) 1)) 1d-6)))
    (let ((centroid (lis::vec3-scale (reduce #'lis::vec3-add (mapcar #'first triangle)) (/ 3d0)))
          (normal (reduce #'lis::vec3-add (mapcar #'second triangle))))
      (true (plusp (lis::vec3-dot normal (lis::vec3-sub centroid centre)))
            "triangle at ~A faces inwards" centroid))))

(defun parameter-range (floats index)
  (loop for o from (+ 7 index) below (length floats) by lis::+vertex-floats+
        minimize (aref floats o) into lo maximize (aref floats o) into hi
        finally (return (list lo hi))))

(define-test mesh-generators
  (let* ((xf (lis::xform '(1 2 3) :x '(1 1 0) :scale '(2 0.5 0.7)))
         (box (generated #'lis::box xf 0))
         (cylinder (generated #'lis::cylinder xf 0 8))
         (frustum (generated #'lis::frustum xf 0 12 0.4 1 :caps t))
         (dome (generated #'lis::hemisphere (lis::xform '(1 2 3) :x '(0 0 -1) :scale '(1.5 1.5 1.5))
                          0 6 16 :base t)))
    (is = 12 (/ (length box) 3 lis::+vertex-floats+))
    (is = (* 4 8) (/ (length cylinder) 3 lis::+vertex-floats+))
    (check-closed box (lis::vec3 1 2 3))
    (check-closed cylinder (lis::xform-point xf 0.5 0 0))
    (check-closed frustum (lis::xform-point xf 0.5 0 0))
    ;; A hemisphere's centroid lies 3/8 of the radius up its axis.
    (check-closed dome (lis::vec3 1 2 (- 3 (* 0.375 1.5))))
    ;; The axial parameter spans 0 to the length, the second is the length.
    (dolist (mesh (list cylinder frustum))
      (is equal '(0.0 2.0) (parameter-range mesh 0))
      (is equal '(2.0 2.0) (parameter-range mesh 1)))
    (dolist (mesh (list (generated #'lis::disc xf 0 8) (generated #'lis::rect xf 0)
                        (generated #'lis::cylinder-patch xf 0 6 0 1)))
      (dolist (triangle (mesh-triangles mesh))
        (dolist (v triangle)
          (true (< (abs (- (lis::vec3-norm (second v)) 1)) 1d-6)))))))

(define-test tube-instances
  (let* ((a (lis::vec3 1 2 -0.5)) (b (lis::vec3 3.5 -1 2))
         (xf (lis::instance-xform (lis::tube-instance a b 0.03 0) 0))
         (axis (lis::vec3-sub b a)))
    (true (< (lis::vec3-norm (lis::vec3-sub (lis::xform-point xf 0 0 0) a)) 1d-6))
    (true (< (lis::vec3-norm (lis::vec3-sub (lis::xform-point xf 1 0 0) b)) 1d-6))
    (true (< (abs (- (lis::vec3-norm (lis::vec3-sub (lis::xform-point xf 0 1 0) a)) 0.03)) 1d-6))
    (true (< (abs (- (lis::vec3-norm (lis::vec3-sub (lis::xform-point xf 0.5 0 1)
                                                   (lis::xform-point xf 0.5 0 0)))
                     0.03))
             1d-6))
    (let ((tube (lis::unit-primitive :tube)))
      (dolist (triangle (mesh-triangles tube))
        (dolist (v triangle)
          (let ((p (first v)) (n (second v)))
            ;; The side's normals, not the caps'.
            (when (< (abs (aref n 0)) 0.5)
              (let ((world (lis::xform-normal xf (aref n 0) (aref n 1) (aref n 2))))
                (true (< (abs (- (lis::vec3-norm world) 1)) 1d-6))
                (true (< (abs (lis::vec3-dot world (lis::vec3-normalize axis))) 1d-6))
                (true (< (abs (aref p 0)) 2))))))))))

(defun part-box (name)
  (lis::positions-box (lis::model-part-positions (mule) name)))

(defun box-within (lo hi expected-lo expected-hi &optional (tolerance 0.01))
  (loop for i below 3
        for elo in expected-lo for ehi in expected-hi
        always (and (or (null elo) (< (abs (- (aref lo i) elo)) tolerance))
                    (or (null ehi) (< (abs (- (aref hi i) ehi)) tolerance)))))

(define-test mule-parts
  (let ((model (mule)))
    (flet ((count-of (name)
             (multiple-value-bind (where start end) (lis::model-part model name)
               (declare (ignore where))
               (- end start))))
      (is = 88 (count-of "truss"))
      (is = 8 (count-of "RCS blocks"))
      (is = 24 (count-of "RCS nozzles"))
      ;; The claws are drawn per frame, not baked (task 223).
      (fail (lis::model-part model "claws") error))
    (multiple-value-bind (lo hi) (lis::positions-box (lis::model-positions model))
      (true (box-within lo hi '(-2.2 -8.3 -5.0) '(16.6 8.3 3.3)) "model box ~A ~A" lo hi))
    (multiple-value-bind (lo hi) (part-box "cab")
      (true (box-within lo hi '(10.5 nil -5.0) '(15.0 nil -2.0))))
    (multiple-value-bind (lo hi) (part-box "LH2 tank")
      (true (box-within lo hi '(0.50 -1.6 -1.6) '(9.36 1.6 1.6))))
    (multiple-value-bind (lo hi) (part-box "LOX tank")
      (true (box-within lo hi '(9.76 -1.6 -1.6) '(13.48 1.6 1.6))))
    (multiple-value-bind (lo hi) (part-box "grapple")
      (true (box-within lo hi '(15.0 nil nil) '(16.6 nil nil))))
    (multiple-value-bind (lo hi) (part-box "engine")
      (true (box-within lo hi '(-2.2 nil nil) '(0 nil nil))))
    ;; Each nozzle points along its thruster's exhaust.
    (let ((nozzles (lis::instance-group-instances (lis::model-group model :cone))))
      (multiple-value-bind (where start) (lis::model-part model "RCS nozzles")
        (is eq :cone where)
        (dotimes (i lis::+rcs-count+)
          (let ((o (* (+ start i) lis::+instance-floats+)))
            (dotimes (k 3)
              (true (< (abs (+ (aref nozzles (+ o 3 k)) (aref lis::*rcs-directions* i k))) 1d-6)))))))))

(define-test engine-trim-through-com
  (dolist (m-p '(28000d0 14000d0 0d0))
    (let* ((com (lis::mass-properties-com (lis::ship-mass-properties m-p)))
           (r (lis::engine-rotation m-p))
           (axis (lis::mat3*vec3 r (lis::vec3 1 0 0)))
           (offset (lis::vec3-sub com lis::*engine-gimbal*)))
      (true (< (lis::vec3-norm (lis::vec3-cross offset axis)) 1d-6) "m-p ~A" m-p)
      (true (plusp (lis::vec3-dot offset axis))))))

(defun pixel-alpha (pixel) (ldb (byte 8 24) pixel))
(defun pixel-red (pixel) (ldb (byte 8 0) pixel))

(defun dark-pixel-p (pixel)
  (and (= 255 (pixel-alpha pixel)) (< (pixel-red pixel) 80)))

(define-test stencil-decal
  (let ((pixels (lis::stencil-decal-pixels)))
    (is equal '(640 512) (array-dimensions pixels))
    (loop for (char x0 y0 x1 y1) in (lis::decal-glyph-boxes)
          do (let ((dark (loop for y from y0 below y1
                               sum (loop for x from x0 below x1
                                         count (dark-pixel-p (aref pixels y x))))))
               (true (<= 0.15 (/ dark (* (- x1 x0) (- y1 y0))) 0.60) "~C covers ~A" char dark)))
    ;; The "0": a transparent path from its inside to outside its box.
    (destructuring-bind (x0 y0 x1 y1) (rest (find #\0 (lis::decal-glyph-boxes) :key #'first))
      (let ((seen (make-hash-table :test #'equal))
            (queue (list (cons (floor (+ x0 x1) 2) (floor (+ y0 y1) 2))))
            (escaped nil))
        (loop while (and queue (not escaped))
              do (destructuring-bind (x . y) (pop queue)
                   (cond ((or (< x x0) (>= x x1) (< y y0) (>= y y1)) (setf escaped t))
                         ((and (not (gethash (cons x y) seen))
                               (zerop (pixel-alpha (aref pixels y x))))
                          (setf (gethash (cons x y) seen) t)
                          (dolist (d '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
                            (push (cons (+ x (car d)) (+ y (cdr d))) queue))))))
        (true escaped "the 0's ring is closed")))
    (loop for (cx cy) in lis::*decal-cairn*
          do (let ((pixel (aref pixels (floor cy) (floor cx))))
               (is = 255 (pixel-alpha pixel))
               (true (< 100 (pixel-red pixel) 200))
               (is = (pixel-red pixel) (ldb (byte 8 8) pixel))
               (is = (pixel-red pixel) (ldb (byte 8 16) pixel))))))

(define-test ship-bounding-radius
  ;; Wet 12.57 m, dry 11.70 m: the engine's exit rim.
  (let ((model (lis::mule-model)))
    (dolist (m-p '(28000d0 0d0))
      (let* ((com (lis::mass-properties-com (lis::ship-mass-properties m-p)))
             (r (lis::engine-rotation m-p))
             (g lis::*engine-gimbal*)
             (brute (max (loop for p in (lis::model-positions model :engine nil)
                               maximize (lis::vec3-norm (lis::vec3-sub p com)))
                         (loop for p in (lis::model-part-positions model "engine")
                               for turned = (lis::vec3-add g (lis::mat3*vec3 r (lis::vec3-sub p g)))
                               maximize (lis::vec3-norm (lis::vec3-sub turned com))))))
        (true (< (abs (- (lis::ship-bounding-radius model m-p) brute)) 1d-6))
        (true (< 11 brute 14) "radius ~A" brute)))
    (is = 0 (least-bytes-consed (lambda () (lis::ship-bounding-radius model 28000d0))))))

(define-test ship-fragment-material-rows
  ;; shaders.lisp's ship-fragment names these rows by number.
  (loop for (name row) in '((:steel 0) (:lh2-foil 1) (:lox-foil 2) (:red-paint 6) (:nozzle 10)
                            (:throat 11) (:hatch 13) (:decal 14) (:decal-pod-14 18) (:rock 19)
                            (:net 20) (:scorched 21) (:peeling-foil 22) (:fixture 23))
        do (is = row (lis::material-id name)))
  (is equal lis::*ship-material-names* luv-in-space.shaders::*ship-shader-material-rows*)
  (is = (length lis::*ship-material-names*) (array-dimension lis::*ship-materials* 0)))

(define-test ship-uniforms
  (flet ((block-of (sim offset)
           (let* ((state (sim-render-state sim))
                  (com (lis::mass-properties-com (lis::ship-mass-properties (lis::render-state-m-p state))))
                  (eye (lis::vec3-add (lis::render-state-r state) offset))
                  (out (make-array lis::+ship-uniform-floats+ :element-type 'single-float)))
             (values (lis::ship-uniforms! out state eye com) state com eye))))
    (multiple-value-bind (out state com eye) (block-of (lis::new-game-sim) (lis::vec3 10 -20 30))
      (let ((rows (lis::mat3-from-quat (lis::render-state-q state)))
            (reference (lis::vec3-sub (lis::vec3-sub (lis::render-state-r state) eye)
                                      (lis::quat-rotate (lis::render-state-q state) com))))
        ;; The rows of q; R relative to the eye, r − q c − eye.
        (dotimes (i 3)
          (dotimes (k 3)
            (true (< (abs (- (aref out (+ (* 4 i) k)) (aref rows (+ (* 3 i) k)))) 1e-6)))
          (true (< (abs (- (aref out (+ 12 i)) (aref reference i))) 1e-5)))
        ;; The eye in body axes: q⁻¹ (eye − R).
        (let ((eye-body (lis::quat-rotate (lis::quat-conjugate (lis::render-state-q state))
                                          (lis::vec3-scale reference -1d0))))
          (dotimes (i 3)
            (true (< (abs (- (aref out (+ 44 i)) (aref eye-body i))) 1e-4))))
        ;; Sunlit at the start, lights on, no emission.
        (let ((d (lis::vec3-norm (lis::vec3-sub (lis::render-state-sun state) (lis::render-state-r state)))))
          (true (< (abs (- (aref out 35) (expt (/ lis::+au+ d) 2))) 1e-6)))
        (is = 1f0 (aref out 47))
        (true (every #'zerop (subseq out 48 51)))))
    ;; In Earth's shadow: no Sun, and the cap below dark (decision 99:
    ;; Earthshine from the lit part of the visible cap).
    (let ((out (block-of (start-sim-at 180) (lis::vec3 0 0 40))))
      (is = 0f0 (aref out 35))
      (is = 0f0 (aref out 39)))))

(define-test earth-lit-cap
  ;; Noon: sin λ / λ; past the terminator by more than the cap: 0; from far,
  ;; cos θ0.
  (let* ((r (lis::vec3 6778136.6d0 0 0))
         (cap (acos (/ lis::+radius-earth+ 6778136.6d0))))
    (true (< (abs (- (lis::earth-lit-cap r (lis::vec3 1 0 0)) (/ (sin cap) cap))) 1d-12))
    (is = 0d0 (lis::earth-lit-cap r (lis::vec3 (cos (+ (/ pi 2) cap 1d-3)) (sin (+ (/ pi 2) cap 1d-3)) 0)))
    (true (< 0 (lis::earth-lit-cap r (lis::vec3 0 1 0)) 0.2d0))
    (true (< (abs (- (lis::earth-lit-cap (lis::vec3 lis::+radius-earth+ 0 0) (lis::vec3 0.6d0 0.8d0 0)) 0.6d0))
             1d-12))))

(define-test strobe-on-p
  ;; 2^30 s is no multiple of the 1.2 s period: K periods just above it.
  (let ((late (* lis::*strobe-period* (ceiling (expt 2d0 30) lis::*strobe-period*))))
    (true (lis::strobe-on-p 0.05d0))
    (false (lis::strobe-on-p 0.5d0))
    (true (< (expt 2d0 30) late (+ (expt 2d0 30) lis::*strobe-period*)))
    (true (lis::strobe-on-p (+ late 0.05d0)))
    (false (lis::strobe-on-p (+ late 0.5d0)))))

(defun glow-state (&key (throttle 0d0) (m-p 28000d0) loss)
  (let ((state (sim-render-state (lis::new-game-sim))))
    (lis::controls-set-throttle (lis::render-state-controls state) throttle)
    (setf (lis::render-state-m-p state) m-p
          (lis::render-state-loss state)
          (and loss (lis::make-loss :crash :earth 0d0 100d0 0d0 0d0)))
    state))

(define-test engine-glow-radiance
  (is = 0d0 (lis::engine-glow-radiance (glow-state)))
  (is = 0d0 (lis::engine-glow-radiance (glow-state :throttle 1d0 :m-p 0d0)))
  (is = 0d0 (lis::engine-glow-radiance (glow-state :throttle 1d0 :loss t)))
  (loop for throttle from 0.1d0 to 1.0d0 by 0.1d0
        do (true (< (abs (- (lis::engine-glow-radiance (glow-state :throttle throttle))
                            (* throttle lis::*engine-glow-radiance*)))
                    1d-12))))

(define-test ship-glows
  (let* ((model (mule))
         (out (make-array (* lis::+glow-floats+ lis::+glow-count+) :element-type 'single-float)))
    (flet ((glows (state)
             (let* ((com (lis::mass-properties-com (lis::ship-mass-properties (lis::render-state-m-p state))))
                    (eye (lis::vec3-add (lis::render-state-r state) (lis::vec3 30 0 0))))
               (values (lis::ship-glows! out model state eye com) state com eye))))
      ;; At lights-t 0 the strobe is lit: red, green, strobe.
      (multiple-value-bind (n state com eye) (glows (glow-state))
        (is = 3 n)
        ;; The red light at q (p − c) + r − eye; red radiance.
        (let ((red (lis::vec3-sub (lis::vec3-add (lis::render-state-r state)
                                                 (lis::quat-rotate (lis::render-state-q state)
                                                                   (lis::vec3-sub (lis::vec3 15 -1.95d0 -1.95d0) com)))
                                  eye)))
          (dotimes (i 3)
            (true (< (abs (- (aref out i) (aref red i))) 1e-4)))
          (is = (float lis::*nav-light-radiance* 1f0) (aref out 4))
          (is = 0f0 (aref out 5))))
      (let ((state (glow-state :throttle 1d0)))
        (setf (lis::render-state-lights-t state) 0.5d0)
        (is = 3 (glows state))
        ;; The engine glow last, at the exit after the trim, blue.
        (true (> (aref out 22) (aref out 20)))
        (is = 0f0 (aref out 23))
        (setf (lis::render-state-ship-lights-on state) nil)
        (is = 1 (glows state))))))

;;; Piece 1 of plan 29: the builders, the fixture, the atlas (decision 124).

(defvar *tanker* nil)
(defun tanker () (or *tanker* (setf *tanker* (lis::tanker-model))))

(defun close-to (a b &optional (tolerance 1d-3))
  (< (lis::vec3-norm (lis::vec3-sub a b)) tolerance))

(define-test truss-ship-models
  (let ((model (tanker)))
    (flet ((span (name)
             (multiple-value-bind (lo hi) (lis::positions-box (lis::model-part-positions model name))
               (list (aref lo 0) (aref hi 0)))))
      ;; 4 longerons, 17 frames of 4, 16 bays of 4 diagonals.
      (is = 136 (multiple-value-bind (where start end) (lis::model-part model "truss")
                  (declare (ignore where)) (- end start)))
      (let ((s (span "truss")))
        (true (< (abs (- (second s) (first s) 24)) 0.1) "truss ~A" s))
      (loop for (name x0 x1) in '(("LH2 tank 1" 0.5 8.1) ("LOX tank 1" 8.5 11.8)
                                  ("LH2 tank 2" 12.2 19.8) ("LOX tank 2" 20.2 23.5))
            do (let ((s (span name)))
                 (true (and (< (abs (- (first s) x0)) 0.01) (< (abs (- (second s) x1)) 0.01))
                       "~A ~A" name s)))
      ;; The nose fixture's plate: its face centre at x 24.15.
      (let* ((plate (lis::model-part-positions model "plate"))
             (face (remove-if-not (lambda (p) (< (abs (- (aref p 0) 24.15)) 1d-5)) plate)))
        (true face)
        (true (close-to (lis::vec3-scale (reduce #'lis::vec3-add face) (/ 1d0 (length face)))
                        (lis::vec3 24.15 0 0)))))
    ;; The truss rods are the Mule's for 15 m.
    (loop for a in (lis::truss-members) for b in (lis::truss-rods 15d0 10)
          do (true (and (< (abs (- (first a) (first b))) 1d-9)
                        (close-to (second a) (second b) 1d-9)
                        (close-to (third a) (third b) 1d-9)
                        (< (abs (- (fourth a) (fourth b))) 1d-9))))
    ;; The capsule: nose at 0, 4.5 m long.
    (multiple-value-bind (lo hi) (lis::positions-box (lis::model-positions (lis::capsule-model)))
      (true (box-within lo hi '(0 -4.95 -1.95) '(4.5 4.95 1.95))))))

(define-test bake-model
  (let* ((model (mule))
         (xf (lis::xform '(10 -3 2) :x '(0 1 0) :y '(-1 0 0)))
         (out (lis::make-builder)))
    (lis::bake-model! out model xf)
    (let* ((baked (lis::builder-model out))
           (source-hull (lis::floats-positions (lis::model-vertices model) 0
                                               (floor (length (lis::model-vertices model))
                                                      lis::+vertex-floats+)))
           (source-engine (lis::floats-positions (lis::model-engine model) 0
                                                 (floor (length (lis::model-engine model))
                                                        lis::+vertex-floats+)))
           (baked-hull (lis::floats-positions (lis::model-vertices baked) 0
                                              (floor (length (lis::model-vertices baked))
                                                     lis::+vertex-floats+))))
      (flet ((moved (p) (lis::xform-point xf (aref p 0) (aref p 1) (aref p 2))))
        (is = (+ (length source-hull) (length source-engine)) (length baked-hull))
        (loop for p in (append source-hull source-engine) for q in baked-hull
              do (true (close-to (moved p) q 1d-5)))
        (dolist (kind '(:tube :box :cone))
          (let ((a (lis::model-group model kind)) (b (lis::model-group baked kind)))
            (loop for p in (lis::instance-positions a 0 (floor (length (lis::instance-group-instances a))
                                                              lis::+instance-floats+))
                  for q in (lis::instance-positions b 0 (floor (length (lis::instance-group-instances b))
                                                              lis::+instance-floats+))
                  do (true (close-to (moved p) q 1d-4)))))
        (is = 3 (length (lis::model-beacons baked)))
        (loop for (kind position) in (lis::model-beacons baked)
              do (true (close-to position (moved (getf (lis::model-lights model) kind)) 1d-5)))))))

(define-test fixture-mesh
  (let* ((axis (lis::vec3-normalize (lis::vec3 1 2 -2)))
         (pin (lis::vec3 3 -1 4))
         (model (lis::fixture-mesh pin axis)))
    (let* ((floats (lis::model-vertices model))
           (n (floor (length floats) lis::+vertex-floats+))
           (face (loop for i below n
                       for o = (* i lis::+vertex-floats+)
                       when (= (aref floats (+ o 6)) (float (lis::material-id :fixture) 1.0))
                         collect o)))
      (true face)
      (let ((centroid (lis::vec3 0 0 0)))
        (dolist (o face)
          (lis::vec3-add! centroid centroid (lis::vec3 (aref floats o) (aref floats (+ o 1)) (aref floats (+ o 2)))))
        (true (close-to (lis::vec3-scale centroid (/ 1d0 (length face))) pin 1d-3)))
      (dolist (o face)
        (let ((normal (lis::vec3 (aref floats (+ o 3)) (aref floats (+ o 4)) (aref floats (+ o 5)))))
          (true (< (acos (min 1d0 (lis::vec3-dot normal axis))) (degrees-to-rad 0.01)))))
      ;; The pin and every guide within 0.6 m of the axis.
      (flet ((off-axis (p)
               (let ((d (lis::vec3-sub p pin)))
                 (lis::vec3-norm (lis::vec3-sub d (lis::vec3-scale axis (lis::vec3-dot d axis)))))))
        (dolist (kind '(:tube :box))
          (let* ((group (lis::model-group model kind)) (instances (lis::instance-group-instances group)))
            (when (eq kind :box) (is = 3 (floor (length instances) lis::+instance-floats+)))
            (dotimes (i (floor (length instances) lis::+instance-floats+))
              (let ((origin (lis::xform-origin (lis::instance-xform instances i))))
                (when (member kind '(:box))
                  (true (< (off-axis origin) 0.6)))))))
        (multiple-value-bind (where start end) (lis::model-part model "pin")
          (declare (ignore where))
          (let ((tube (lis::instance-xform (lis::instance-group-instances (lis::model-group model :tube)) start)))
            (true (< (off-axis (lis::xform-point tube 1 0 0)) 0.6))
            (is = 1 (- end start))))))))

(defun degrees-to-rad (d) (* d (/ pi 180)))

(define-test decal-atlas
  (let ((pixels (lis::stencil-decal-pixels)))
    (loop for row below 5
          for text in '("M-07" "M-04" "M-05" "TERN" "POD 14")
          for boxes = (lis::decal-glyph-boxes row)
          do (is = (count-if-not (lambda (c) (char= c #\Space)) text) (length boxes))
             (is string= (remove #\Space text) (coerce (mapcar #'first boxes) 'string))
             (loop for (char x0 y0 x1 y1) in boxes
                   do (let ((dark (loop for y from y0 below y1
                                        sum (loop for x from x0 below x1
                                                  count (dark-pixel-p (aref pixels y x))))))
                        (true (<= 0.15 (/ dark (* (- x1 x0) (- y1 y0))) 0.60)
                              "~A ~C covers ~A" text char dark))))))

;;; Piece 2 of plan 29: the places' models and lights (decision 124).

(defun part-entries (model)
  "Every part of MODEL as (name positions)."
  (loop for (name where start end) in (lis::model-parts model)
        collect (list name
                      (case where
                        (:hull (lis::floats-positions (lis::model-vertices model) start end))
                        (:engine (lis::floats-positions (lis::model-engine model) start end))
                        (t (lis::instance-positions (lis::model-group model where) start end))))))

(defun parts-named (model name)
  (loop for (n positions) in (part-entries model) when (string= n name) collect positions))

(defun shape-contains-p (shape p grow)
  (let ((c (lis::shape-centre shape)))
    (if (eq (lis::shape-kind shape) :sphere)
        (<= (lis::vec3-norm (lis::vec3-sub p c)) (+ (lis::shape-radius shape) grow))
        (loop for i below 3
              always (<= (abs (- (aref p i) (aref c i))) (+ (* 0.5d0 (aref (lis::shape-size shape) i)) grow))))))

(defun shape-span (shape points)
  "The span per axis of the POINTS inside SHAPE (grown 0.05 m), as a list."
  (let ((inside (remove-if-not (lambda (p) (shape-contains-p shape p 0.05d0)) points)))
    (if inside
        (multiple-value-bind (lo hi) (lis::positions-box inside)
          (loop for i below 3 collect (- (aref hi i) (aref lo i))))
        '(0 0 0))))

(defun shape-extent (shape)
  (if (eq (lis::shape-kind shape) :sphere)
      (make-list 3 :initial-element (* 2 (lis::shape-radius shape)))
      (coerce (lis::shape-size shape) 'list)))

(defparameter *fill-exceptions* '((:highport 4 0 0.8d0))
  "(object shape-index axis least-fill): the berthed Mules are 8.3 m across
the 10 m shape (decision 47's box).")

(defparameter *shape-growth* '((:fulmar-depot . 0.2d0))
  "TANKER 2's tail is 0.15 m beyond the shape's 24 m (its nose pin on fixture B).")

(define-test places-models-fit-shapes
  (dolist (def lis::*objects*)
    (let* ((key (lis::object-def-key def)) (model (lis::object-model key))
           (shapes (lis::object-def-shapes def))
           (grow (or (cdr (assoc key *shape-growth*)) 0.05d0))
           (all '()))
      (loop for (name positions) in (part-entries model)
            do (setf all (append positions all))
               (unless (member name lis::*thin-part-names* :test #'string=)
                 (let ((outside (remove-if (lambda (p) (some (lambda (shape) (shape-contains-p shape p grow))
                                                             shapes))
                                           positions)))
                   (true (null outside) "~A ~A: ~D vertices outside the shapes, first ~A"
                         key name (length outside) (first outside)))))
      (loop for shape in shapes for index from 0
            do (loop for span in (shape-span shape all)
                     for size in (shape-extent shape)
                     for axis from 0
                     for least = (or (fourth (find-if (lambda (e) (and (eq (first e) key) (= (second e) index)
                                                                       (= (third e) axis)))
                                                      *fill-exceptions*))
                                     0.9d0)
                     do (true (>= span (* least size))
                              "~A shape ~D axis ~D: ~,2F of ~,2F" key index axis span size))))))

(defun fixture-plates (model)
  "((centre normal) …) of every fixture plate in MODEL: the fixture material's
vertices of the \"plate\" parts, by part."
  (let ((floats (lis::model-vertices model)) (result '()))
    (loop for (name where start end) in (lis::model-parts model)
          when (string= name "plate")
            do (let ((sum (lis::vec3 0 0 0)) (normal nil) (n 0))
                 (loop for i from start below end
                       for o = (* i lis::+vertex-floats+)
                       when (= (aref floats (+ o 6)) (float (lis::material-id :fixture) 1.0))
                         do (lis::vec3-add! sum sum (lis::vec3 (aref floats o) (aref floats (+ o 1))
                                                               (aref floats (+ o 2))))
                            (setf normal (lis::vec3 (aref floats (+ o 3)) (aref floats (+ o 4))
                                                    (aref floats (+ o 5))))
                            (incf n))
                 (push (list (lis::vec3-scale sum (/ 1d0 n)) normal) result)))
    (nreverse result)))

(defun vector-angle (a b)
  (acos (max -1d0 (min 1d0 (lis::vec3-dot (lis::vec3-normalize a) (lis::vec3-normalize b))))))

(define-test places-fixture-plates
  (dolist (def lis::*objects*)
    (let* ((model (lis::object-model (lis::object-def-key def)))
           (plates (fixture-plates model)))
      (dolist (fixture (lis::object-def-fixtures def))
        (let* ((pin (lis::fixture-pin fixture)) (axis (lis::fixture-axis fixture))
               ;; Free: the fixture's own plate faces along its axis; held: the
               ;; scenery ship's nose plate faces back at it.
               (sign (if (lis::fixture-free fixture) 1d0 -1d0))
               (plate (find-if (lambda (p) (close-to (first p) pin 1d-3)) plates)))
          ;; A Mule holds Highport's arms: its grapple tip is checked apart.
          (unless (or plate (and (eq (lis::object-def-key def) :highport) (not (lis::fixture-free fixture))))
            (true plate "~A ~A: no plate on the pin" (lis::object-def-key def) (lis::fixture-name fixture)))
          (when plate
            (true (< (vector-angle (second plate) (lis::vec3-scale axis sign)) (degrees-to-rad 0.01))
                  "~A ~A: plate normal ~A" (lis::object-def-key def) (lis::fixture-name fixture)
                  (second plate))))))))

(define-test places-scenery-ships
  ;; AURORA 11 nose-in on Tern's node, x 13.08 to 17.58.
  (let ((tern (lis::object-model :tern-station)))
    (let ((points (append (first (parts-named tern "frustum")) (first (parts-named tern "service section")))))
      (multiple-value-bind (lo hi) (lis::positions-box points)
        (true (< (abs (- (aref lo 0) 13.08)) 0.01) "AURORA nose ~A" (aref lo 0))
        (true (< (abs (- (aref hi 0) 17.58)) 0.01) "AURORA tail ~A" (aref hi 0)))))
  ;; The Mules' grapple tips on the arms' pins.
  (let* ((def (lis::object-definition :highport)) (model (lis::object-model :highport)))
    (loop for name in '("arm 1" "arm 2")
          for pin = (lis::fixture-pin (find name (lis::object-def-fixtures def)
                                            :key #'lis::fixture-name :test #'string=))
          do (let ((grapple (find-if (lambda (points)
                                       (multiple-value-bind (lo hi) (lis::positions-box points)
                                         (< (abs (- (* 0.5 (+ (aref lo 1) (aref hi 1))) (aref pin 1))) 0.01)))
                                     (parts-named model "grapple"))))
               (true grapple "~A: no grapple" name)
               (multiple-value-bind (lo hi) (lis::positions-box grapple)
                 (true (< (abs (- (aref hi 2) (aref pin 2))) 0.01) "tip z ~A" (aref hi 2))
                 (true (> (- (aref hi 2) (aref lo 2)) 1.55) "grapple length")
                 (true (< (abs (- (* 0.5 (+ (aref lo 0) (aref hi 0))) (aref pin 0))) 0.01))
                 (true (< (abs (- (* 0.5 (+ (aref lo 1) (aref hi 1))) (aref pin 1))) 0.01)))))))

(defparameter *expected-lights*
  `((:tern-station (:red (-2.92d0 -39d0 0)) (:green (-2.92d0 39d0 0)) (:strobe (11.08d0 0 -2.15d0)))
    (:fulmar-depot (:strobe (-15.78d0 0 -0.45d0)))
    (:highport (:red (0.02d0 -70d0 -4.84d0)) (:green (0.02d0 70d0 -4.84d0))
     (:strobe (0.02d0 -29.5d0 -8.14d0)) (:strobe (0.02d0 29.5d0 -8.14d0)))
    (:rock-mover (:red (-6.31d0 -3.75d0 -1.94d0)) (:green (-6.31d0 3.75d0 -1.94d0))
     (:strobe (-8.56d0 0 -5.04d0))))
  "The places' own lights; scenery ships' and fixture lamps are checked apart.")

(define-test places-lights
  (dolist (def lis::*objects*)
    (let* ((key (lis::object-def-key def)) (model (lis::object-model key))
           (lights (loop for (kind position) on (lis::model-lights model) by #'cddr
                         collect (list kind position)))
           (lamps (remove :lamp lights :key #'first :test-not #'eq)))
      (is equal lights (lis::model-beacons model) "~A beacons" key)
      (loop for (kind position) in (cdr (assoc key *expected-lights*))
            do (true (find-if (lambda (l) (and (eq (first l) kind)
                                               (close-to (second l) (apply #'lis::vec3 position) 1d-3)))
                              lights)
                     "~A ~A at ~A" key kind position))
      ;; Four lamps on a circle of 0.55 m round each free berth fixture, none at others.
      (dolist (fixture (lis::object-def-fixtures def))
        (let ((near (remove-if-not (lambda (l) (< (lis::vec3-norm (lis::vec3-sub (second l) (lis::fixture-pin fixture)))
                                                  0.7d0))
                                   lamps)))
          (if (and (lis::fixture-free fixture) (eq (lis::fixture-kind fixture) :berth))
              (progn
                (is = 4 (length near) "~A ~A lamps" key (lis::fixture-name fixture))
                (dolist (l near)
                  (let ((d (lis::vec3-sub (second l) (lis::fixture-pin fixture))))
                    (true (< (abs (- (lis::vec3-norm d) 0.55d0)) 1d-3))
                    (true (< (abs (lis::vec3-dot d (lis::fixture-axis fixture))) 1d-3))
                    ;; 45 degrees off the body axes
                    (let ((big (loop for i below 3 maximize (abs (aref d i)))))
                      (true (< (abs (- big (* 0.55d0 (sqrt 0.5d0)))) 1d-3))))))
              (is = 0 (length near) "~A ~A lamps" key (lis::fixture-name fixture)))))))
  ;; Scenery ships' lights count: (red green strobe lamp).
  (loop for (key . counts) in '((:tern-station 1 1 1 4) (:fulmar-depot 0 0 2 4) (:highport 3 3 4 4)
                                (:rock-mover 1 1 1 0) (:k-17 0 0 0 0) (:pod-14 0 0 0 0) (:skua-yard-3 0 0 0 0))
        do (let ((kinds (loop for (k) on (lis::model-lights (lis::object-model key)) by #'cddr collect k)))
             (is equal counts (mapcar (lambda (kind) (count kind kinds)) '(:red :green :strobe :lamp)) "~A" key))))

(define-test places-model-sizes
  (dolist (def lis::*objects*)
    (let ((model (lis::object-model (lis::object-def-key def))))
      (true (<= (floor (length (lis::model-vertices model)) lis::+vertex-floats+) 40000))
      (true (<= (loop for g in (lis::model-groups model)
                      sum (floor (length (lis::instance-group-instances g)) lis::+instance-floats+))
                2000))
      (true (eq model (lis::object-model (lis::object-def-key def)))))))

(define-test object-gpu-dot-numbers
  ;; decision 124: a far object's dot is a disc of a quarter of its shapes'
  ;; surface; a rock's is pi r^2; its extent is its model's.
  (let ((rock (lis::object-definition :skua-yard-3)))
    (true (< (abs (- (lis::shapes-quarter-area rock)
                     (* pi (expt (lis::shape-radius (first (lis::object-def-shapes rock))) 2))))
             1d-9))))

;;; The claws (decision 124: Meshes of the places: models, scenery liveries,
;;; lights, far dots, the grapple's claws, floodlight and range finder; task 223).

(define-test claw-tips
  ;; Hinged at x 16.0 on radius 0.85, 0.6 m long, turned about the tangent:
  ;; the tip at the hinge + 0.6 (cos a x + sin a u), u the claw's radial direction.
  (let ((out (make-array (* 3 lis::+instance-floats+) :element-type 'single-float)))
    (dolist (angle '(0 30 -8))
      (lis::ship-claw-instances! out angle)
      (dotimes (k 3)
        (let* ((a (degrees-to-rad (* 120 k)))
               (u (lis::vec3 0 (sin a) (- (cos a))))
               (theta (degrees-to-rad angle))
               (expected (lis::vec3 (+ 16d0 (* 0.6d0 (cos theta)))
                                    (* (aref u 1) (+ 0.85d0 (* 0.6d0 (sin theta))))
                                    (* (aref u 2) (+ 0.85d0 (* 0.6d0 (sin theta))))))
               (tip (lis::claw-tip out k)))
          (true (close-to tip expected 1d-5) "claw ~D at ~D deg: ~A against ~A" k angle tip expected))))
    ;; Stowed: where the static claws stood (16.6, 0.85 sin, -0.85 cos).
    (lis::ship-claw-instances! out 0)
    (true (close-to (lis::claw-tip out 0) (lis::vec3 16.6d0 0d0 -0.85d0) 1d-5))))
