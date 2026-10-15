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
      (is = 3 (count-of "claws")))
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
    (is equal '(128 512) (array-dimensions pixels))
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
                            (:throat 11) (:hatch 13) (:decal 14))
        do (is = row (lis::material-id name))))

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
