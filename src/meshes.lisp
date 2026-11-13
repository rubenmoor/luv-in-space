(in-package #:luv-in-space)

;;; The Mule's meshes, CPU part (decision 80: The Mule's meshes in numbers:
;;; geometry, draws, materials and lights; docs/design.md "How they look",
;;; "Parts"). Ship body axes FRD, metres from R, single floats. A vertex is
;;; position, normal, material id and two parameters: along tubes, cones and
;;; the bell the axial coordinate and the length (m); on the decal texture
;;; u, v; on the window, hatch and portholes u, v in metres from the centre.
;;; Triangle lists. No HAL here.

(defconstant +vertex-floats+ 9)
(defconstant +instance-floats+ 16
  "Origin, the unit basis's x, y and z axes, scale, material.")

(deftype floats () '(simple-array single-float (*)))

(defun make-float-buffer ()
  (make-array 1024 :element-type 'single-float :adjustable t :fill-pointer 0))

(defun simple-floats (buffer) (coerce buffer 'floats))

;;; Materials (decision 80): linear albedo, Blinn-Phong weight and exponent,
;;; emission.

(defparameter *ship-material-names*
  '(:steel :lh2-foil :lox-foil :cab-paint :stencil :cairn :red-paint :solar-cells
    :radiator :aluminium :nozzle :throat :glass :hatch :decal
    :decal-m04 :decal-m05 :decal-tern :decal-pod-14 :rock :net :scorched :peeling-foil
    :fixture :boreal-green)
  "Row order of *ship-materials*; a vertex's material id is the row. The decals
are rows 14 to 18, one per row of the decal atlas.")

(defparameter *ship-materials*
  (let ((rows '((0.45 0.45 0.47 0.15 16.0 0.0)     ; steel, x0.6 at weld nodes
                (0.80 0.55 0.20 0.6 64.0 0.0)      ; LH2 foil, gold
                (0.75 0.75 0.78 0.6 64.0 0.0)      ; LOX foil, silver
                (0.80 0.80 0.78 0.05 8.0 0.0)      ; cab white paint
                (0.04 0.04 0.04 0.05 8.0 0.0)      ; stencil black
                (0.35 0.35 0.35 0.05 8.0 0.0)      ; cairn grey
                (0.50 0.06 0.04 0.05 8.0 0.0)      ; red hand paint
                (0.03 0.04 0.09 0.4 128.0 0.0)     ; solar cells
                (0.85 0.85 0.85 0.05 8.0 0.0)      ; radiators
                (0.60 0.60 0.62 0.2 16.0 0.0)      ; aluminium boxes
                (0.08 0.08 0.08 0.1 16.0 0.0)      ; nozzle
                (0.35 0.33 0.30 0.1 16.0 0.0)      ; throat band, first 0.15 m
                (0.02 0.02 0.02 0.8 128.0 0.005)   ; window and porthole glass
                (0.80 0.80 0.78 0.05 8.0 0.0)      ; hatch: paint, red border
                (0.80 0.80 0.78 0.05 8.0 0.0)      ; decal: paint under the texture
                (0.80 0.80 0.78 0.05 8.0 0.0)      ; decal row 1
                (0.80 0.80 0.78 0.05 8.0 0.0)      ; decal row 2
                (0.80 0.80 0.78 0.05 8.0 0.0)      ; decal row 3
                (0.80 0.80 0.78 0.05 8.0 0.0)      ; decal row 4
                (0.07 0.065 0.06 0.05 8.0 0.0)     ; rock
                (0.55 0.50 0.40 0.05 8.0 0.0)      ; net cord, over the rock
                (0.25 0.25 0.25 0.05 8.0 0.0)      ; scorched, streaked by noise
                (0.75 0.75 0.78 0.6 64.0 0.0)      ; peeling foil, patches of scorched
                (0.85 0.85 0.85 0.5 32.0 0.0)      ; fixture plate, black target
                (0.45 0.70 0.50 0.05 8.0 0.0))))   ; Boreal green band
    (make-array (list (length rows) 6) :element-type 'single-float :initial-contents rows))
  "Rows of *ship-material-names*: r g b, specular weight, exponent, emission; live.")

(defun material-id (name)
  (or (position name *ship-material-names*) (error "No ship material ~S." name)))

;;; Transforms: body = origin + basis (scale ⊙ local); normals through
;;; basis (n / scale), normalised.

(defstruct (xform (:constructor %make-xform (origin basis scale)))
  (origin (make-vec3) :type vec3)
  (basis (make-mat3) :type mat3)        ; columns: the local x, y, z axes
  (scale (make-vec3) :type vec3))

(defun v3 (x) (if (typep x 'vec3) x (apply #'vec3 x)))

(defun orthonormal-basis (x &optional y-hint)
  "Unit x along X, y from Y-HINT (default the body axis least along x) made
perpendicular, z = x × y."
  (let* ((x (vec3-normalize (v3 x)))
         (hint (v3 (or y-hint (if (< (abs (aref x 1)) 0.9d0) '(0 1 0) '(0 0 1)))))
         (y (vec3-normalize (vec3-add-scaled hint x (- (vec3-dot hint x))))))
    (values x y (vec3-cross x y))))

(defun xform (origin &key (x '(1 0 0)) y (scale '(1 1 1)))
  (multiple-value-bind (ax ay az) (orthonormal-basis x y)
    (%make-xform (vec3-copy (v3 origin))
                 (mat3 (aref ax 0) (aref ay 0) (aref az 0)
                       (aref ax 1) (aref ay 1) (aref az 1)
                       (aref ax 2) (aref ay 2) (aref az 2))
                 (v3 scale))))

(defun xform-point (xf x y z)
  (let ((m (xform-basis xf)) (o (xform-origin xf)) (s (xform-scale xf)))
    (let ((lx (* x (aref s 0))) (ly (* y (aref s 1))) (lz (* z (aref s 2))))
      (vec3 (+ (aref o 0) (* (aref m 0) lx) (* (aref m 1) ly) (* (aref m 2) lz))
            (+ (aref o 1) (* (aref m 3) lx) (* (aref m 4) ly) (* (aref m 5) lz))
            (+ (aref o 2) (* (aref m 6) lx) (* (aref m 7) ly) (* (aref m 8) lz))))))

(defun xform-normal (xf nx ny nz)
  (let ((m (xform-basis xf)) (s (xform-scale xf)))
    (let ((lx (/ nx (aref s 0))) (ly (/ ny (aref s 1))) (lz (/ nz (aref s 2))))
      (vec3-normalize
       (vec3 (+ (* (aref m 0) lx) (* (aref m 1) ly) (* (aref m 2) lz))
             (+ (* (aref m 3) lx) (* (aref m 4) ly) (* (aref m 5) lz))
             (+ (* (aref m 6) lx) (* (aref m 7) ly) (* (aref m 8) lz)))))))

;;; Generators: each appends triangles to OUT, a float buffer. A local
;;; vertex is (x y z nx ny nz p0 p1) in the primitive's unit shape.

(defun emit! (out xf material vertex)
  (destructuring-bind (x y z nx ny nz p0 p1) vertex
    (let ((p (xform-point xf x y z)) (n (xform-normal xf nx ny nz)))
      (dolist (v (list (aref p 0) (aref p 1) (aref p 2) (aref n 0) (aref n 1) (aref n 2)
                       material p0 p1))
        (vector-push-extend (float v 1.0) out)))))

(defun triangle! (out xf material a b c)
  (dolist (v (list a b c)) (emit! out xf material v)))

(defun quad! (out xf material a b c d)
  "Two triangles; A B C D counter-clockwise seen from outside."
  (triangle! out xf material a b c)
  (triangle! out xf material a c d))

(defun ring-angle (k sides &optional (a0 0d0) (a1 (* 2 pi)))
  (+ a0 (* (- a1 a0) (/ k sides))))

(defun box (out xf material)
  "The unit cube, centred, −0.5 to 0.5; MATERIAL an id or six ids for the
faces +x −x +y −y +z −z. Parameters 0."
  (dotimes (axis 3)
    (dolist (sign '(1 -1))
      (let ((m (if (listp material) (nth (+ (* 2 axis) (if (= sign 1) 0 1)) material) material))
            (u (mod (+ axis 1) 3)) (v (mod (+ axis 2) 3)))
        (flet ((corner (su sv)
                 (let ((p (list 0 0 0)) (n (list 0 0 0)))
                   (setf (nth axis p) (* 0.5d0 sign) (nth u p) (* 0.5d0 su) (nth v p) (* 0.5d0 sv)
                         (nth axis n) sign)
                   (append p n (list 0 0)))))
          (let ((corners (list (corner -1 -1) (corner 1 -1) (corner 1 1) (corner -1 1))))
            (apply #'quad! out xf m (if (= sign 1) corners (reverse corners)))))))))

(defun frustum (out xf material sides r0 r1 &key caps)
  "Along local x from 0 to 1, radius R0 at 0 and R1 at 1; CAPS nil, t,
:start or :end. Parameters: the axial coordinate and the length, metres."
  (let ((length (aref (xform-scale xf) 0)))
    (flet ((side (x r a)
             (list x (* r (cos a)) (* r (sin a)) (- r0 r1) (cos a) (sin a) (* x length) length))
           (cap (x a nx)
             (list x (* (if (zerop x) r0 r1) (cos a)) (* (if (zerop x) r0 r1) (sin a))
                   nx 0 0 (* x length) length)))
      (dotimes (k sides)
        (let* ((a (ring-angle k sides)) (b (ring-angle (1+ k) sides)) (mid (* 0.5d0 (+ a b))))
          (cond ((zerop r0) (triangle! out xf material (side 0 0 mid) (side 1 r1 b) (side 1 r1 a)))
                ((zerop r1) (triangle! out xf material (side 0 r0 a) (side 0 r0 b) (side 1 0 mid)))
                (t (quad! out xf material (side 0 r0 a) (side 0 r0 b) (side 1 r1 b) (side 1 r1 a))))
          (when (and (member caps '(t :start)) (plusp r0))
            (triangle! out xf material (list 0 0 0 -1 0 0 0 length) (cap 0 b -1) (cap 0 a -1)))
          (when (and (member caps '(t :end)) (plusp r1))
            (triangle! out xf material (list 1 0 0 1 0 0 length length) (cap 1 a 1) (cap 1 b 1))))))))

(defun cylinder (out xf material sides &key (caps t))
  "Along local x from 0 to 1, radius 1; closed with CAPS: 4 SIDES triangles."
  (frustum out xf material sides 1 1 :caps caps))

(defun cylinder-patch (out xf material sides a0 a1 &key (uv '(0 1 0 1)))
  "The unit cylinder's wall between angles A0 and A1 (from local y towards
z); u runs along x, v along the angle, from and to the values of UV."
  (destructuring-bind (u0 u1 v0 v1) uv
    (flet ((wall (x k)
             (let ((a (ring-angle k sides a0 a1)))
               (list x (cos a) (sin a) 0 (cos a) (sin a)
                     (+ u0 (* x (- u1 u0))) (+ v0 (* (/ k sides) (- v1 v0)))))))
      (dotimes (k sides)
        (quad! out xf material (wall 0 k) (wall 0 (1+ k)) (wall 1 (1+ k)) (wall 1 k))))))

(defun hemisphere (out xf material rings sides &key base)
  "Radius 1, the dome towards local +x, the rim at x 0; BASE closes it.
Parameters: the axial coordinate and the radius along x, metres."
  (let ((length (aref (xform-scale xf) 0)))
    (flet ((dome (i a)
             (let* ((phi (* (/ pi 2) (- 1 (/ i rings))))
                    (x (cos phi)) (y (* (sin phi) (cos a))) (z (* (sin phi) (sin a))))
               (list x y z x y z (* x length) length))))
      (dotimes (k sides)
        (let ((a (ring-angle k sides)) (b (ring-angle (1+ k) sides)))
          (dotimes (i rings)
            (if (= i (1- rings))
                (triangle! out xf material (dome i a) (dome i b) (dome rings (* 0.5d0 (+ a b))))
                (quad! out xf material (dome i a) (dome i b) (dome (1+ i) b) (dome (1+ i) a))))
          (when base
            (triangle! out xf material (list 0 0 0 -1 0 0 0 length)
                       (list 0 (cos b) (sin b) -1 0 0 0 length)
                       (list 0 (cos a) (sin a) -1 0 0 0 length))))))))

(defun disc (out xf material sides)
  "Radius 1 in the local y-z plane, facing +x; u, v the y and z in metres."
  (let ((s (xform-scale xf)))
    (flet ((rim (a)
             (list 0 (cos a) (sin a) 1 0 0 (* (cos a) (aref s 1)) (* (sin a) (aref s 2)))))
      (dotimes (k sides)
        (triangle! out xf material (list 0 0 0 1 0 0 0 0)
                   (rim (ring-angle k sides)) (rim (ring-angle (1+ k) sides)))))))

(defun rect (out xf material &key uv)
  "The unit square in the local y-z plane, centred, facing +x; u along y
and v along z from and to UV, by default metres from the centre."
  (let ((s (xform-scale xf)))
    (destructuring-bind (u0 u1 v0 v1)
        (or uv (list (* -0.5d0 (aref s 1)) (* 0.5d0 (aref s 1))
                     (* -0.5d0 (aref s 2)) (* 0.5d0 (aref s 2))))
      (flet ((corner (y z)
               (list 0 y z 1 0 0 (if (minusp y) u0 u1) (if (minusp z) v0 v1))))
        (quad! out xf material (corner -0.5 -0.5) (corner 0.5 -0.5)
               (corner 0.5 0.5) (corner -0.5 0.5))))))

;;; Instances (decision 80): unit primitives drawn per instance.

(defun unit-primitive (kind)
  (let ((out (make-float-buffer)) (xf (xform '(0 0 0))))
    (ecase kind
      (:tube (cylinder out xf 0 8))
      (:box (box out xf 0))
      (:cone (frustum out xf 0 8 0.3d0 1 :caps :start)))
    (simple-floats out)))

(defun instance (origin x-axis y-axis scale material)
  "16 singles: origin, the unit basis (x along X-AXIS, y from Y-AXIS), scale, material."
  (multiple-value-bind (ax ay az) (orthonormal-basis x-axis y-axis)
    (let ((out (make-array +instance-floats+ :element-type 'single-float)))
      (loop for v in (append (coerce (v3 origin) 'list) (coerce ax 'list) (coerce ay 'list)
                             (coerce az 'list) (coerce (v3 scale) 'list) (list material))
            for i from 0
            do (setf (aref out i) (float v 1.0)))
      out)))

(defun tube-instance (a b radius material)
  "The unit tube from A to B with RADIUS."
  (let ((d (vec3-sub (v3 b) (v3 a))))
    (instance a d nil (list (vec3-norm d) radius radius) material)))

(defun instance-xform (instances index)
  "The transform of instance INDEX in the floats INSTANCES, in double."
  (let ((o (* index +instance-floats+)))
    (flet ((f (i) (float (aref instances (+ o i)) 1d0)))
      (%make-xform (vec3 (f 0) (f 1) (f 2))
                   (mat3 (f 3) (f 6) (f 9) (f 4) (f 7) (f 10) (f 5) (f 8) (f 11))
                   (vec3 (f 12) (f 13) (f 14))))))

;;; The model

(defstruct (instance-group (:constructor make-instance-group (kind primitive instances)))
  (kind :tube :type keyword)
  (primitive nil :type floats)          ; the unit primitive's vertices
  (instances nil :type floats))         ; +instance-floats+ each

(defstruct model
  (vertices nil :type floats)           ; the baked hull
  (engine nil :type floats)             ; the engine at zero trim
  (groups '())                          ; instance-groups, :tube :box :cone
  (parts '())                           ; (name where start end), where :hull :engine or a kind
  (lights '())                          ; plist of light positions, body axes
  (beacons '())                         ; (kind position): :red :green :strobe :lamp
  (hull-points nil :type (or null (simple-array double-float (*))))
  (engine-points nil :type (or null (simple-array double-float (*))))
  (scratch (make-mass-properties) :type mass-properties))

(defun model-group (model kind) (find kind (model-groups model) :key #'instance-group-kind))

(defun model-part (model name)
  "(values where start end): vertices of :hull or :engine, or instances of a kind."
  (let ((part (or (assoc name (model-parts model) :test #'string=)
                  (error "No part ~S." name))))
    (values-list (rest part))))

(defun floats-positions (floats start end)
  (loop for i from start below end
        for o = (* i +vertex-floats+)
        collect (vec3 (aref floats o) (aref floats (+ o 1)) (aref floats (+ o 2)))))

(defun instance-positions (group start end)
  (let ((primitive (instance-group-primitive group)))
    (loop for i from start below end
          for xf = (instance-xform (instance-group-instances group) i)
          append (loop for p in (floats-positions primitive 0 (floor (length primitive)
                                                                     +vertex-floats+))
                       collect (xform-point xf (aref p 0) (aref p 1) (aref p 2))))))

(defun model-part-positions (model name)
  "Body positions of every vertex of part NAME, instances expanded, the engine at zero trim."
  (multiple-value-bind (where start end) (model-part model name)
    (case where
      (:hull (floats-positions (model-vertices model) start end))
      (:engine (floats-positions (model-engine model) start end))
      (t (instance-positions (model-group model where) start end)))))

(defun model-positions (model &key (engine t))
  (append (floats-positions (model-vertices model) 0
                            (floor (length (model-vertices model)) +vertex-floats+))
          (loop for g in (model-groups model)
                append (instance-positions g 0 (floor (length (instance-group-instances g))
                                                      +instance-floats+)))
          (when engine
            (floats-positions (model-engine model) 0
                              (floor (length (model-engine model)) +vertex-floats+)))))

(defun positions-box (positions)
  "(values min max) over POSITIONS."
  (let ((lo (vec3-copy (first positions))) (hi (vec3-copy (first positions))))
    (dolist (p positions (values lo hi))
      (dotimes (i 3)
        (setf (aref lo i) (min (aref lo i) (aref p i)) (aref hi i) (max (aref hi i) (aref p i)))))))

;;; Builders (decision 124: Meshes of the places: models, scenery liveries,
;;; lights, far dots, the grapple's claws, floodlight and range finder). A
;;; builder collects a model's hull, engine, instances, parts and beacons;
;;; `bake-model!` appends another model to it under a transform.

(defstruct (builder (:constructor make-builder ()))
  (hull (make-float-buffer))
  (engine (make-float-buffer))
  (groups (list (list :tube) (list :box) (list :cone)))
  (parts '())
  (beacons '()))

(defun builder-vertex-count (b where)
  (floor (fill-pointer (if (eq where :engine) (builder-engine b) (builder-hull b)))
         +vertex-floats+))

(defun builder-add (b kind instance)
  (nconc (assoc kind (builder-groups b)) (list instance)))

(defun builder-tube (b a c radius material)
  (builder-add b :tube (tube-instance a c radius material)))

(defun builder-part (b name where thunk)
  "Run THUNK, recording the vertices or instances it adds as part NAME of WHERE."
  (let* ((group (assoc where (builder-groups b)))
         (count (lambda () (if group (length (rest group)) (builder-vertex-count b where))))
         (start (funcall count)))
    (funcall thunk)
    (push (list name where start (funcall count)) (builder-parts b))))

(defun builder-model (b &optional lights)
  "The model of builder B; LIGHTS the plist of its light positions."
  (make-model
   :vertices (simple-floats (builder-hull b))
   :engine (simple-floats (builder-engine b))
   :groups (loop for (kind . instances) in (builder-groups b)
                 collect (make-instance-group
                          kind (unit-primitive kind)
                          (let ((out (make-array (* +instance-floats+ (length instances))
                                                 :element-type 'single-float)))
                            (loop for inst in instances for i from 0
                                  do (replace out inst :start1 (* i +instance-floats+)))
                            out)))
   :parts (reverse (builder-parts b))
   :lights lights
   :beacons (reverse (builder-beacons b))))

(defun light-beacons (lights)
  "((kind position) …) of the navigation lights and strobe in the plist LIGHTS."
  (loop for (kind position) on lights by #'cddr
        when (member kind '(:red :green :strobe :lamp))
          collect (list kind position)))

(defun bake-model! (out model xform)
  "Append MODEL to the builder OUT under XFORM, an origin, a rotation and a
uniform scale: its hull, its engine (at zero trim, into the hull), its
instance groups (the bases composed with the transform) and its beacons.
The instances' scale must be uniform for this to be exact."
  (let* ((s (xform-scale xform))
         (k (aref s 0))
         (m (xform-basis xform))
         (hull (builder-hull out))
         (hull-before (builder-vertex-count out :hull))
         (source-hull (floor (length (model-vertices model)) +vertex-floats+))
         (before (loop for (kind . instances) in (builder-groups out) collect (cons kind (length instances)))))
    (unless (and (= (aref s 0) (aref s 1)) (= (aref s 1) (aref s 2)))
      (error "bake-model!: the scale ~A is not uniform." s))
    (dolist (floats (list (model-vertices model) (model-engine model)))
      (loop for o from 0 below (length floats) by +vertex-floats+
            do (let ((p (xform-point xform (aref floats o) (aref floats (+ o 1)) (aref floats (+ o 2))))
                     (n (xform-normal xform (aref floats (+ o 3)) (aref floats (+ o 4))
                                      (aref floats (+ o 5)))))
                 (dolist (v (list (aref p 0) (aref p 1) (aref p 2) (aref n 0) (aref n 1) (aref n 2)
                                  (aref floats (+ o 6)) (aref floats (+ o 7)) (aref floats (+ o 8))))
                   (vector-push-extend (float v 1.0) hull)))))
    (dolist (g (model-groups model))
      (let ((instances (instance-group-instances g)))
        (dotimes (i (floor (length instances) +instance-floats+))
          (let* ((xf (instance-xform instances i))
                 (o (xform-point xform (aref (xform-origin xf) 0) (aref (xform-origin xf) 1)
                                 (aref (xform-origin xf) 2)))
                 (new (make-array +instance-floats+ :element-type 'single-float))
                 (b (xform-basis xf)))
            (dotimes (c 3)
              (let ((axis (mat3*vec3 m (vec3 (aref b c) (aref b (+ c 3)) (aref b (+ c 6))))))
                (dotimes (r 3) (setf (aref new (+ 3 (* 3 c) r)) (float (aref axis r) 1.0)))))
            (dotimes (r 3)
              (setf (aref new r) (float (aref o r) 1.0)
                    (aref new (+ 12 r)) (float (* k (aref (xform-scale xf) r)) 1.0)))
            (setf (aref new 15) (aref instances (+ (* i +instance-floats+) 15)))
            (builder-add out (instance-group-kind g) new)))))
    ;; The parts come along, the engine's after the hull's.
    (dolist (part (model-parts model))
      (destructuring-bind (name where start end) part
        (let ((shift (case where
                       (:hull hull-before)
                       (:engine (+ hull-before source-hull))
                       (t (cdr (assoc where before))))))
          (push (list name (if (eq where :engine) :hull where) (+ start shift) (+ end shift))
                (builder-parts out)))))
    (dolist (beacon (model-beacons model))
      (destructuring-bind (kind position) beacon
        (push (list kind (xform-point xform (aref position 0) (aref position 1) (aref position 2)))
              (builder-beacons out))))
    out))

(defun model-extent (model &optional (com (vec3 0 0 0)))
  "The largest distance from COM (default the body origin) to any vertex of
MODEL, instances expanded, the engine at zero trim."
  (sqrt (loop for p in (model-positions model)
              maximize (let ((d (vec3-sub p com))) (vec3-dot d d)))))

;;; The truss ships: the Mule, tankers (decision 80; docs/design.md "How
;;; they look", "Parts", "Rigid bodies")

(defparameter *cab-axis* '(0 -3.5d0) "y, z of the cab's axis.")
(defparameter *cab-radius* 1.5d0)
(defparameter *cab-skin* 0.004d0 "Decal and hatch above the cab's wall, m.")
(defparameter *porthole-skin* 0.012d0 "A flat porthole's centre above the curved wall, m.")
(defparameter *decal-span* '(12.1d0 14.9d0) "x of the decal, m.")
(defparameter *decal-height* 0.7d0 "Arc length of the decal, m.")
(defparameter *decal-angle* (* 25 (/ pi 180)) "The decal's centre above the y axis.")

(defun degrees-to-radians (d) (* d (/ pi 180)))

(defun truss-rods (length bays)
  "The truss's rods like `truss-members` (decision 71), for LENGTH m in BAYS
bays: ((weight centre direction rod-length) …), 4 longerons, 4 per frame, 4
diagonals per bay. The Mule's are 15 m and 10 bays."
  (let ((members '()) (bay (/ length bays)))
    (flet ((rod (weight centre direction rod-length)
             (push (list weight (apply #'vec3 centre) (vec3-normalize (apply #'vec3 direction))
                         rod-length)
                   members)))
      (dolist (y '(1.8d0 -1.8d0))
        (dolist (z '(1.8d0 -1.8d0))
          (rod 112.65d0 (list (* 0.5d0 length) y z) '(1 0 0) length)))
      (dotimes (k (1+ bays))
        (let ((x (* bay k)))
          (dolist (s '(1.8d0 -1.8d0))
            (rod 16.06d0 (list x s 0) '(0 0 1) 3.6d0)
            (rod 16.06d0 (list x 0 s) '(0 1 0) 3.6d0))))
      (dotimes (k bays)
        (let ((x (+ (* bay k) (* 0.5d0 bay)))
              (sign (if (evenp k) 3.6d0 -3.6d0))
              (rod-length (sqrt (+ (* bay bay) (* 3.6d0 3.6d0)))))
          (dolist (s '(1.8d0 -1.8d0))
            (rod 17.39d0 (list x s 0) (list bay 0 sign) rod-length)
            (rod 17.39d0 (list x 0 s) (list bay sign 0) rod-length)))))
    (nreverse members)))

(defun truss-ship-model (&key (length 15d0) (bays 10) tanks (tank-radius 1.6d0)
                           cab decal (nose :grapple) (engine-x -0.6d0) (engine-z -0.5d0)
                           (rcs-x '(0.5d0 14.5d0)) (wings t) (radiators t) (equipment t)
                           (structure t) (rcs t) (engine-p t) (decal-u 1d0) lights)
  "A ship on a truss of LENGTH m in BAYS bays along +x from x 0: TANKS
((name x0 x1 material) …) with hemispherical domes of TANK-RADIUS; CAB nil or
(x0 x1), the cab on top with its struts, window, portholes and hatch, and
DECAL nil or the decal material on its sides; NOSE :grapple (the Mule's, with
its claws) or (:fixture x) for a fixture plate with its pin at x; the engine
at zero trim, its bell's rim at ENGINE-X on the axis at z ENGINE-Z; RCS blocks
at the x in RCS-X (four each, with the Mule's thruster triples; blocks past
x 7 move with the nose); WINGS, RADIATORS, EQUIPMENT (the gas conditioning
and the equipment bay) flags; STRUCTURE (the truss and the thrust frame), RCS
and ENGINE-P flags, nil for a cab alone; DECAL-U the share of the decal's width
(its left, the cairn) drawn; LIGHTS the plist of light positions. The Mule
is one case (`mule-model`), a tanker another."
  (let* ((b (make-builder)) (hull (builder-hull b)) (engine (builder-engine b))
         (cab-dx (if cab (- (first cab) 10.5d0) 0d0))
         (nose-shift (- length 15d0))
         (gimbal (vec3 (+ engine-x 0.6d0) 0 engine-z)))
    (labels ((id (name) (material-id name))
             (part (name where thunk) (builder-part b name where thunk))
             (add (kind instance) (builder-add b kind instance))
             (tube (a c radius) (builder-tube b a c radius (id :steel)))
             (cab-xf (x0 x1 radius)
               (xform (list x0 0 (second *cab-axis*)) :y '(0 1 0) :scale (list (- x1 x0) radius radius))))
      (when structure
        (part "truss" :tube
              (lambda ()
                (loop for (nil centre direction rod-length) in (truss-rods length bays)
                      for half = (vec3-scale direction (* 0.5d0 rod-length))
                      do (tube (vec3-sub centre half) (vec3-add centre half)
                               (if (= rod-length length) 0.038d0 0.030d0))))))
      (when cab
        (part "cab struts" :tube
              (lambda ()
                (dolist (x (list (+ 11.0d0 cab-dx) (+ 14.5d0 cab-dx)))
                  (dolist (s '(1 -1))
                    (tube (list x (* s 1.8d0) -1.8d0) (list x (* s 0.9d0) -2.3d0) 0.04d0))))))
      (when structure
        (part "thrust frame" :tube
              (lambda ()
                (dolist (y '(1.8d0 -1.8d0))
                  (dolist (z '(1.8d0 -1.8d0))
                    (let* ((corner (vec3 0 y z))
                           (end (vec3-add-scaled gimbal (vec3-normalize (vec3-sub corner gimbal)) 0.3d0)))
                      (tube corner end 0.04d0)))))))
      (when (eq nose :grapple)
        (part "grapple struts" :tube
              (lambda ()
                (dolist (y '(1 -1))
                  (dolist (z '(1 -1))
                    (let ((rim (* 0.3d0 (sqrt 0.5d0))))
                      (tube (list length (* y 1.8d0) (* z 1.8d0)) (list length (* y rim) (* z rim))
                            0.03d0)))))))
      (when wings
        (part "wing booms" :tube
              (lambda ()
                (dolist (s '(1 -1))
                  (tube (list 5.0d0 (* s 1.8d0) 0) (list 5.0d0 (* s 2.3d0) 0) 0.04d0)))))
      (when rcs
        ;; RCS: a block per thruster triple of ship.lisp, a nozzle per thruster
        ;; pointing along its exhaust, from the block's face.
        (flet ((rcs-position (i)
                 (let ((x (aref *rcs-positions* i 0)))
                   (vec3 (if (> x 7) (+ (second rcs-x) (- x 14.5d0)) (+ (first rcs-x) (- x 0.5d0)))
                         (aref *rcs-positions* i 1) (aref *rcs-positions* i 2)))))
          (part "RCS blocks" :box
                (lambda ()
                  (loop for i from 0 below +rcs-count+ by 3
                        do (add :box (instance (rcs-position i) '(1 0 0) '(0 1 0)
                                               '(0.3d0 0.4d0 0.4d0) (id :aluminium))))))
          (part "RCS nozzles" :cone
                (lambda ()
                  (dotimes (i +rcs-count+)
                    (let* ((exhaust (vec3 (- (aref *rcs-directions* i 0)) (- (aref *rcs-directions* i 1))
                                          (- (aref *rcs-directions* i 2))))
                           (face (+ (* 0.15d0 (abs (aref exhaust 0)))
                                    (* 0.2d0 (+ (abs (aref exhaust 1)) (abs (aref exhaust 2))))))
                           (centre (rcs-position i)))
                      (add :cone (instance (vec3-add-scaled centre exhaust face) exhaust nil
                                           '(0.12d0 0.08d0 0.08d0) (id :nozzle)))))))))
      (when (eq nose :grapple)
        (part "claws" :box
              (lambda ()
                (dotimes (k 3)
                  (let ((a (degrees-to-radians (* 120 k))))
                    (add :box (instance (list (+ 16.3d0 nose-shift) (* 0.85d0 (sin a)) (* -0.85d0 (cos a)))
                                        '(1 0 0) (list 0 (sin a) (- (cos a)))
                                        '(0.6d0 0.1d0 0.1d0) (id :steel))))))))
      ;; Tanks with hemispherical domes, foil-wrapped.
      (loop for (name x0 x1 material) in tanks
            do (let ((r tank-radius) (m (id material)))
                 (part name :hull
                       (lambda ()
                         (cylinder hull (xform (list (+ x0 r) 0 0) :scale (list (- x1 x0 r r) r r))
                                   m 32 :caps nil)
                         (hemisphere hull (xform (list (+ x0 r) 0 0) :x '(-1 0 0) :y '(0 -1 0)
                                                                     :scale (list r r r))
                                     m 8 32)
                         (hemisphere hull (xform (list (- x1 r) 0 0) :scale (list r r r)) m 8 32)))))
      ;; Cab: flat ends, window, portholes, hatch, decals.
      (when cab
        (destructuring-bind (cx0 cx1) cab
          (part "cab" :hull
                (lambda () (cylinder hull (cab-xf cx0 cx1 *cab-radius*) (id :cab-paint) 32)))
          (part "window" :hull
                (lambda () (rect hull (xform (list (+ cx1 0.003d0) 0 -3.6d0) :y '(0 1 0)
                                             :scale '(1 1.2d0 0.6d0))
                                 (id :glass))))
          (part "portholes" :hull
                (lambda ()
                  (dolist (s '(1 -1))
                    (let* ((a (degrees-to-radians 37)) (r (+ *cab-radius* *porthole-skin*))
                           (normal (list 0 (* s (cos a)) (sin a))))
                      (disc hull (xform (list (+ 14.1d0 cab-dx) (* s r (cos a))
                                              (+ (second *cab-axis*) (* r (sin a))))
                                        :x normal :y '(1 0 0) :scale '(1 0.175d0 0.175d0))
                            (id :glass) 16)))))
          (part "hatch" :hull
                (lambda ()
                  (let ((half (/ 0.6d0 (+ *cab-radius* *cab-skin*))))
                    (cylinder-patch hull (cab-xf (+ 11.0d0 cab-dx) (+ 11.8d0 cab-dx)
                                                 (+ *cab-radius* *cab-skin*))
                                    (id :hatch) 12 (- half) half :uv '(-0.4d0 0.4d0 -0.6d0 0.6d0)))))
          ;; The decal reads left to right from outside: along +x on +y, -x on -y;
          ;; v 0 at its top (towards -z).
          (when decal
            (destructuring-bind (x0 x1) (mapcar (lambda (x) (+ x cab-dx)) *decal-span*)
              (let* ((r (+ *cab-radius* *cab-skin*))
                     (half (/ *decal-height* r 2)))
                (part "decal +y" :hull
                      (lambda ()
                        (cylinder-patch hull (cab-xf x0 (+ x0 (* decal-u (- x1 x0))) r) (id decal) 8
                                        (- (- *decal-angle*) half) (+ (- *decal-angle*) half)
                                        :uv (list 0 decal-u 0 1))))
                (part "decal -y" :hull
                      (lambda ()
                        (cylinder-patch hull (cab-xf (- x1 (* decal-u (- x1 x0))) x1 r) (id decal) 8
                                        (- (+ pi *decal-angle*) half) (+ pi *decal-angle* half)
                                        :uv (list decal-u 0 1 0)))))))))
      (case (if (consp nose) (first nose) nose)
        (:grapple
         (part "grapple" :hull
               (lambda ()
                 (cylinder hull (xform (list length 0 0) :scale '(0.8d0 0.3d0 0.3d0)) (id :steel) 16)
                 (frustum hull (xform (list (+ length 0.8d0) 0 0) :scale '(0.8d0 1 1)) (id :steel) 16
                          0.3d0 0.8d0))))
        (:fixture
         (let ((fixture (fixture-mesh (list (second nose) 0 0) '(1 0 0)
                                      :standoff (- (second nose) 0.05d0 length))))
           (bake-model! b fixture (xform '(0 0 0))))))
      (when wings
        (part "solar wings" :hull
              (lambda ()
                (dolist (s '(1 -1))
                  (box hull (xform (list 5.0d0 (* s 5.3d0) 0) :scale '(2.0d0 6.0d0 0.05d0))
                       (append (make-list 5 :initial-element (id :steel)) (list (id :solar-cells))))))))
      (when radiators
        (part "radiators" :hull
              (lambda ()
                (dolist (s '(1 -1))
                  (box hull (xform (list 8.0d0 (* s 1.2d0) 2.55d0) :scale '(4.0d0 0.05d0 1.5d0))
                       (id :radiator))))))
      (when equipment
        (part "gas conditioning" :hull
              (lambda () (box hull (xform '(9.5d0 0 1.4d0) :scale '(1.0d0 1.2d0 0.6d0)) (id :aluminium))))
        (part "equipment bay" :hull
              (lambda () (box hull (xform '(10.0d0 0 2.3d0) :scale '(1.0d0 1.6d0 0.8d0)) (id :aluminium)))))
      ;; The engine at zero trim: the bell from the throat (the axial
      ;; parameter 0 there), chamber, turbopumps.
      (when engine-p (part "engine" :engine
            (lambda ()
              (frustum engine (xform (list engine-x 0 engine-z) :x '(-1 0 0) :y '(0 -1 0)
                                                                :scale '(1.6d0 1 1))
                       (id :nozzle) 24 0.15d0 0.725d0)
              (cylinder engine (xform (list engine-x 0 engine-z) :scale '(0.6d0 0.25d0 0.25d0))
                        (id :steel) 16)
              (dolist (s '(1 -1))
                (box engine (xform (list (+ engine-x 0.3d0) (* s 0.35d0) engine-z)
                                   :scale '(0.3d0 0.25d0 0.25d0))
                     (id :aluminium))))))
      (let ((lights (or lights
                        (list :red (vec3 length -1.95d0 -1.95d0) :green (vec3 length 1.95d0 -1.95d0)
                              :strobe (vec3 (* 0.5d0 length) 0 -2.1d0)
                              :engine-exit (vec3 (- engine-x 1.6d0) 0 engine-z)))))
        (dolist (beacon (light-beacons lights)) (push beacon (builder-beacons b)))
        (builder-model b lights)))))

(defun mule-model (&key (decal :decal))
  "The Mule: the baked hull, the instance groups, the engine, the lights;
DECAL the material of its registration (:decal M-07, :decal-m04, :decal-m05)."
  (truss-ship-model
   :tanks '(("LH2 tank" 0.50d0 9.36d0 :lh2-foil) ("LOX tank" 9.76d0 13.48d0 :lox-foil))
   :cab '(10.5d0 15.0d0) :decal decal
   ;; The navigation lights on brackets 0.15 m out from the truss's top
   ;; front corners, so its tubes don't cut them (decision 100).
   :lights (list :red (vec3 15.0d0 -1.95d0 -1.95d0) :green (vec3 15.0d0 1.95d0 -1.95d0)
                 :strobe (vec3 12.75d0 0 -5.05d0) :engine-exit (vec3 -2.2d0 0 -0.5d0))))

(defun tanker-model ()
  "TANKER 3 (docs/design.md \"Rigid bodies\"): 24 m of truss in 16 bays, two
LH2 and two LOX tanks in a row, the nose fixture's pin at x 24.15, the engine
on the axis."
  (truss-ship-model
   :length 24d0 :bays 16
   :tanks '(("LH2 tank 1" 0.50d0 8.10d0 :lh2-foil) ("LOX tank 1" 8.50d0 11.80d0 :lox-foil)
            ("LH2 tank 2" 12.20d0 19.80d0 :lh2-foil) ("LOX tank 2" 20.20d0 23.50d0 :lox-foil))
   :nose '(:fixture 24.15d0) :engine-x -0.7d0 :engine-z 0d0 :rcs-x '(0.5d0 23.5d0)))

(defun capsule-model ()
  "AURORA 11's capsule, nose at x 0, the body along +x: a frustum Ø 1.5 → 3.9 over
2.5 m, the service section Ø 3.9 × 2.0 with a 0.6 m Boreal band round its
middle, two wings 1.0 × 3.0 m."
  (let* ((b (make-builder)) (hull (builder-hull b)))
    (flet ((id (name) (material-id name)))
      (builder-part b "frustum" :hull
                    (lambda () (frustum hull (xform '(0 0 0) :scale '(2.5d0 1 1)) (id :cab-paint) 32
                                        0.75d0 1.95d0)))
      (builder-part b "service section" :hull
                    (lambda ()
                      (cylinder hull (xform '(2.5d0 0 0) :scale '(2.0d0 1.95d0 1.95d0)) (id :cab-paint) 32)))
      (builder-part b "band" :hull
                    (lambda ()
                      (cylinder hull (xform '(3.2d0 0 0) :scale '(0.6d0 1.953d0 1.953d0))
                                (id :boreal-green) 32 :caps nil)))
      (builder-part b "wings" :hull
                    (lambda ()
                      (dolist (s '(1 -1))
                        (box hull (xform (list 3.5d0 (* s 3.45d0) 0) :scale '(1.0d0 3.0d0 0.05d0))
                             (append (make-list 5 :initial-element (id :steel)) (list (id :solar-cells))))))))
    (builder-model b)))

(defun fixture-mesh (pin axis &key (standoff 0.1d0) (material :fixture))
  "The standard berth fixture as a model (decision 124): a disc plate Ø 1.0 ×
0.05 m whose outer face centre is PIN, its normal the unit AXIS, in MATERIAL
(with the black target mark from the plate's centre coordinates), a pin Ø
0.08 × 0.25 outward, three guides 0.06 × 0.20 × 0.30 at 120° on radius 0.42
leaning 30° out, and a stand-off tube Ø 0.25 STANDOFF m back from the plate."
  (let* ((b (make-builder)) (hull (builder-hull b))
         (pin (v3 pin)) (a (vec3-normalize (v3 axis))))
    (multiple-value-bind (ax ay az) (orthonormal-basis a)
      (declare (ignore ax))
      (flet ((id (name) (material-id name))
             (along (distance) (vec3-add-scaled pin a distance)))
        (builder-part b "plate" :hull
                      (lambda ()
                        (cylinder hull (xform (along -0.05d0) :x a :scale '(0.05d0 0.5d0 0.5d0))
                                  (id :aluminium) 24 :caps :start)
                        (disc hull (xform pin :x a :scale '(1 0.5d0 0.5d0)) (id material) 24)))
        (builder-part b "pin" :tube
                      (lambda () (builder-tube b pin (along 0.25d0) 0.04d0 (id :aluminium))))
        (builder-part b "guides" :box
                      (lambda ()
                        (dotimes (k 3)
                          (let* ((angle (degrees-to-radians (* 120 k)))
                                 (radial (vec3-add (vec3-scale ay (cos angle)) (vec3-scale az (sin angle))))
                                 (lean (vec3-add (vec3-scale a (cos (degrees-to-radians 30)))
                                                 (vec3-scale radial (sin (degrees-to-radians 30)))))
                                 (tilt (vec3-add (vec3-scale radial (cos (degrees-to-radians 30)))
                                                 (vec3-scale a (- (sin (degrees-to-radians 30))))))
                                 (origin (vec3-add (vec3-add-scaled pin radial 0.42d0)
                                                   (vec3-scale lean 0.15d0))))
                            (builder-add b :box (instance origin (vec3-cross radial a) tilt
                                                          '(0.06d0 0.20d0 0.30d0) (id :aluminium)))))))
        (builder-part b "stand-off" :tube
                      (lambda ()
                        (builder-tube b (along (- (+ 0.05d0 standoff))) (along -0.05d0) 0.125d0
                                      (id :aluminium))))))
    (builder-model b)))

(defvar *models* (make-hash-table :test #'eq) "Models by key, made once.")

(defun cached-model (key maker)
  (or (gethash key *models*) (setf (gethash key *models*) (funcall maker))))

;;; The places' models (decision 124: Meshes of the places; docs/design.md
;;; "Earth-orbit points of interest", "Other ships", "Rigid bodies"): one model
;;; per object definition in its body axes about the CoM, scenery ships baked
;;; in, each fixture's mesh from its definition's pin and axis.

(defparameter *fixture-proud* 0.3d0
  "m: a fixture's plate beyond the surface it stands on (decision 47).")
(defparameter *thin-part-names*
  '("solar wings" "wing booms" "radiators" "plate" "pin" "guides" "stand-off" "arms" "net frame"
    "engine" "thrust frame" "wings" "decal +y" "decal -y" "cryocooler fins" "seams"
    "RCS blocks" "RCS nozzles" "gas conditioning" "equipment bay")
  "Parts the shapes need not hold (wings, booms, fixtures, the engine): the
tests' exemption.")
(defparameter *lamp-radius* 0.55d0 "m: a free berth fixture's lamps from its pin.")

(defun sphere (out xf material rings sides)
  "The unit sphere, poles on local ±x, RINGS bands from +x to -x. Parameters:
the arc length from the +x pole and the arc along the latitude (metres at the
scale's x), the net's coordinates."
  (let ((r (aref (xform-scale xf) 0)))
    (flet ((point (i k)
             (let* ((theta (* pi (/ i rings))) (phi (ring-angle k sides))
                    (x (cos theta)) (y (* (sin theta) (cos phi))) (z (* (sin theta) (sin phi))))
               (list x y z x y z (* r theta) (* r (sin theta) phi)))))
      (dotimes (k sides)
        (dotimes (i rings)
          (let ((a (point i k)) (b (point (1+ i) k)) (c (point (1+ i) (1+ k))) (d (point i (1+ k))))
            (cond ((= i 0) (triangle! out xf material a b c))
                  ((= i (1- rings)) (triangle! out xf material a c d))
                  (t (quad! out xf material a b c d)))))))))

(defun capped-tank (b name from to radius material)
  "A tank of RADIUS from the point FROM to TO (end to end, domes included)
along its axis."
  (let* ((from (v3 from)) (to (v3 to))
         (axis (vec3-sub to from)) (length (vec3-norm axis)) (u (vec3-normalize axis))
         (hull (builder-hull b)) (m (material-id material)))
    (builder-part b name :hull
                  (lambda ()
                    (cylinder hull (xform (vec3-add-scaled from u radius) :x u
                                                                          :scale (list (- length radius radius) radius radius))
                              m 32 :caps nil)
                    (hemisphere hull (xform (vec3-add-scaled from u radius) :x (vec3-scale u -1d0)
                                                                            :scale (list radius radius radius))
                                m 8 32)
                    (hemisphere hull (xform (vec3-add-scaled to u (- radius)) :x u
                                                                              :scale (list radius radius radius))
                                m 8 32)))))

(defun fixture-lamp-lights (fixture)
  "(:lamp position …) for a free berth FIXTURE: four lamps on a circle of
*lamp-radius* round its pin, 45° off the body axes (decision 124)."
  (when (and (fixture-free fixture) (eq (fixture-kind fixture) :berth))
    (multiple-value-bind (ax ay az) (orthonormal-basis (fixture-axis fixture))
      (declare (ignore ax))
      (loop for k below 4
            for angle = (degrees-to-radians (+ 45 (* 90 k)))
            append (list :lamp (vec3-add (fixture-pin fixture)
                                         (vec3-add (vec3-scale ay (* *lamp-radius* (cos angle)))
                                                   (vec3-scale az (* *lamp-radius* (sin angle))))))))))

(defun bake-fixtures! (b def)
  "The meshes of DEF's free fixtures into B; the lamps' lights."
  (loop for fixture in (object-def-fixtures def)
        when (fixture-free fixture)
          do (bake-model! b (fixture-mesh (fixture-pin fixture) (fixture-axis fixture)
                                          :standoff (- *fixture-proud* 0.05d0))
                          (xform '(0 0 0)))
          and append (fixture-lamp-lights fixture)))

(defun bake-ship! (b model origin &key (x '(1 0 0)) y (keep '(:red :green :strobe)))
  "MODEL baked into B with its local origin at ORIGIN, its x along X; its
lights of the kinds KEEP, transformed, as a plist."
  (let ((xf (xform origin :x x :y y)))
    (bake-model! b model xf)
    (loop for (kind position) on (model-lights model) by #'cddr
          when (member kind keep)
            append (list kind (xform-point xf (aref position 0) (aref position 1) (aref position 2))))))

(defun finish-place-model (b lights)
  (let ((model (builder-model b lights)))
    (setf (model-beacons model) (light-beacons lights))
    model))

(defun place-wing! (b centre size faces &key (wide 0) (thin 2))
  "A solar wing: a box at CENTRE of SIZE (long along y, its thin axis THIN
0.05 m), FACES the six materials, and four booms 0.03 m round along its long
edges at ±0.22 m on the thin axis and the WIDE axis' edges."
  (let ((hull (builder-hull b)) (centre (v3 centre)))
    (builder-part b "solar wings" :hull (lambda () (box hull (xform centre :scale size) faces)))
    (builder-part b "wing booms" :tube
                  (lambda ()
                    (dolist (sw '(1 -1))
                      (dolist (st '(1 -1))
                        (flet ((end (long)
                                 (let ((p (vec3-copy centre)))
                                   (incf (aref p 1) (* long 0.5d0 (second size)))
                                   (incf (aref p wide) (* sw (- (* 0.5d0 (nth wide size)) 0.03d0)))
                                   (incf (aref p thin) (* st 0.22d0))
                                   p)))
                          (builder-tube b (end -1) (end 1) 0.03d0 (material-id :steel)))))))))

(defun module-decal! (b x0 x1 radius material)
  "A decal on the sides of a cylinder along x, between X0 and X1."
  (let* ((hull (builder-hull b)) (r (+ radius 0.006d0)) (half (/ *decal-height* r 2))
         (xf (xform (list x0 0 0) :y '(0 1 0) :scale (list (- x1 x0) r r))))
    (builder-part b "decal +y" :hull
                  (lambda () (cylinder-patch hull xf (material-id material) 8 (- half) half
                                             :uv '(0 1 0 1))))
    (builder-part b "decal -y" :hull
                  (lambda () (cylinder-patch hull xf (material-id material) 8 (- pi half) (+ pi half)
                                             :uv '(1 0 1 0))))))

(defun tern-model (def)
  "Tern Station: three modules Ø 4.2 × 8 and the node Ø 4.2 × 4 in white
with Boreal's green bands, the truss across the middle module, radiators,
wings, AURORA 11 nose-in on the node (x 13.08 to 17.58)."
  (let* ((b (make-builder)) (hull (builder-hull b)) (white (material-id :cab-paint))
         (green (material-id :boreal-green)))
    (builder-part b "modules" :hull
                  (lambda ()
                    (dolist (x '(-10.92d0 -2.92d0 5.08d0))
                      (cylinder hull (xform (list (- x 4) 0 0) :scale '(8d0 2.1d0 2.1d0)) white 32)
                      (cylinder hull (xform (list (- x 0.3d0) 0 0) :scale '(0.6d0 2.103d0 2.103d0))
                                green 32 :caps nil))
                    (cylinder hull (xform '(9.08d0 0 0) :scale '(4d0 2.1d0 2.1d0)) white 32)))
    (module-decal! b -4.32d0 -1.52d0 2.1d0 :decal-tern)
    (builder-part b "truss" :hull
                  (lambda () (box hull (xform '(-2.92d0 0 0) :scale '(1.5d0 28d0 1.5d0))
                                  (material-id :aluminium))))
    (builder-part b "radiators" :hull
                  (lambda ()
                    (dolist (s '(1 -1))
                      (box hull (xform (list -2.92d0 (* s 8d0) 0.77d0) :scale '(4d0 10d0 0.05d0))
                           (material-id :radiator)))))
    (dolist (s '(1 -1))
      (let ((y (* s 26.5d0)))
        (place-wing! b (list -2.92d0 y 0) '(8d0 25d0 0.05d0)
                     (append (make-list 5 :initial-element (material-id :steel))
                             (list (material-id :solar-cells))))))
    (let ((lights (list :red (vec3 -2.92d0 -39d0 0) :green (vec3 -2.92d0 39d0 0)
                        :strobe (vec3 11.08d0 0 -2.15d0))))
      (setf lights (append lights (bake-fixtures! b def)))
      (bake-model! b (capsule-model) (xform '(13.08d0 0 0)))
      (finish-place-model b lights))))

(defun fulmar-model (def)
  "Fulmar Depot: the 30 m spine, two LH2 tanks Ø 6 × 14 in gold foil and two
LOX tanks Ø 4.5 × 6 in silver at y ±3.9, the gold sunshade disc Ø 16 at the
sunward end, the aft mount with its two arms along ±z and the cryocooler
fins; TANKER 2 berthed at fixture B, its nose on the pin, fixture A free."
  (let* ((b (make-builder)) (hull (builder-hull b)) (white (material-id :cab-paint)))
    (builder-part b "sunshade" :hull
                  (lambda () (cylinder hull (xform '(14.07d0 0 0) :scale '(0.3d0 8d0 8d0))
                                       (material-id :lh2-foil) 48)))
    (builder-part b "spine" :hull
                  (lambda () (cylinder hull (xform '(-15.98d0 0 0) :scale '(30.05d0 0.4d0 0.4d0))
                                       white 16)))
    (dolist (s '(1 -1))
      (capped-tank b "LH2 tank" (list -0.48d0 (* s 3.9d0) 0) (list 13.52d0 (* s 3.9d0) 0) 3d0 :lh2-foil)
      (capped-tank b "LOX tank" (list -7.98d0 (* s 3.9d0) 0) (list -1.98d0 (* s 3.9d0) 0) 2.25d0
                   :lox-foil))
    (builder-part b "tank mounts" :tube
                  (lambda ()
                    (dolist (x '(-6.0d0 6.0d0))
                      (dolist (s '(1 -1))
                        (builder-tube b (list x 0 0) (list x (* s 3.9d0) 0) 0.12d0 (material-id :steel))))))
    (builder-part b "aft mount" :hull
                  (lambda () (box hull (xform '(-11.98d0 0 0) :scale '(8d0 1d0 0.8d0))
                                  (material-id :aluminium))))
    (builder-part b "arms" :tube
                  (lambda ()
                    (builder-tube b '(-13.98d0 0 -4d0) '(-13.98d0 0 4d0) 0.3d0 (material-id :aluminium))))
    (builder-part b "cryocooler fins" :hull
                  (lambda ()
                    (dolist (z '(2d0 -2d0))
                      (dolist (y '(0.7d0 -0.7d0))
                        (box hull (xform (list -11.98d0 y z) :scale '(6d0 0.05d0 2.5d0))
                             (material-id :radiator))))))
    (let* ((lights (list :strobe (vec3 -15.78d0 0 -0.45d0)))
           (lamps (bake-fixtures! b def))
           (tanker (tanker-model))
           (pin (fixture-pin (find "B" (object-def-fixtures def) :key #'fixture-name :test #'string=))))
      (setf lights
            (append lights lamps
                    (bake-ship! b tanker (vec3-add pin (vec3 0 0 -24.15d0)) :x '(0 0 1) :y '(0 1 0)
                                         :keep '(:strobe))))
      (finish-place-model b lights))))

(defun highport-truss! (b)
  "The open truss 6.5 × 60 × 6.5 along y, centred (0.02, 0, -4.84): 12 bays of
5 m, longerons, frames and diagonals as tubes 0.06 m round."
  (let ((cx 0.02d0) (cz -4.84d0) (h 3.19d0) (steel (material-id :steel)))
    (builder-part b "truss" :tube
                  (lambda ()
                    (flet ((tube (x1 y1 z1 x2 y2 z2)
                             ;; the end frames 0.06 m in, so the tubes' radius stays inside 30 m
                             (builder-tube b (list (+ cx x1) (max -29.94d0 (min 29.94d0 y1)) (+ cz z1))
                                           (list (+ cx x2) (max -29.94d0 (min 29.94d0 y2)) (+ cz z2))
                                           0.06d0 steel)))
                      (dolist (sx '(1 -1))
                        (dolist (sz '(1 -1))
                          (tube (* sx h) -30d0 (* sz h) (* sx h) 30d0 (* sz h))))
                      (dotimes (k 13)
                        (let ((y (+ -30d0 (* 5 k))))
                          (tube h y h (- h) y h) (tube h y (- h) (- h) y (- h))
                          (tube h y h h y (- h)) (tube (- h) y h (- h) y (- h))))
                      (dotimes (k 12)
                        (let ((y (+ -30d0 (* 5 k))) (sign (if (evenp k) 1 -1)))
                          (dolist (s '(1 -1))
                            (tube (* s h) y (* sign h) (* s h) (+ y 5) (* (- sign) h))
                            (tube (* sign h) y (* s h) (* (- sign) h) (+ y 5) (* s h))))))))))

(defun highport-model (def)
  "Highport: the open truss, four tanks Ø 6 × 12, the water plant below it
with two radiator fins edge-on to the Sun, wings 40 × 20 at y ±50, three arms;
STUBBORN (M-04) and THISTLE (M-05) berthed at the arms, nose on the pin, their
+x along +z, the plant fixture free."
  (let* ((b (make-builder)) (hull (builder-hull b)) (white (material-id :cab-paint)))
    (highport-truss! b)
    (dolist (c '(7d0 -7d0 21d0 -21d0))
      (capped-tank b "tank" (list 0.02d0 (- c 6) -4.84d0) (list 0.02d0 (+ c 6) -4.84d0) 3d0 :lox-foil))
    (builder-part b "water plant" :hull
                  (lambda () (box hull (xform '(0.02d0 0 3.41d0) :scale '(6d0 6d0 10d0)) white)))
    (builder-part b "plant fins" :hull
                  (lambda ()
                    (dolist (s '(1 -1))
                      (box hull (xform (list (+ 0.02d0 (* s 2.5d0)) 0 18.41d0) :scale '(0.05d0 6d0 20d0))
                           (material-id :radiator)))))
    (dolist (s '(1 -1))
      (place-wing! b (list 0.02d0 (* s 50d0) -4.84d0) '(0.05d0 40d0 20d0)
                   (cons (material-id :solar-cells) (make-list 5 :initial-element (material-id :steel)))
                   :wide 2 :thin 0))
    (builder-part b "arms" :tube
                  (lambda ()
                    (dolist (s '(1 -1))
                      (builder-tube b (list 0.02d0 (* s 20d0) -8.09d0) (list 0.02d0 (* s 20d0) -11.09d0)
                                    0.25d0 (material-id :aluminium)))))
    (let* ((lights (list :red (vec3 0.02d0 -70d0 -4.84d0) :green (vec3 0.02d0 70d0 -4.84d0)
                         :strobe (vec3 0.02d0 -29.5d0 -8.14d0) :strobe (vec3 0.02d0 29.5d0 -8.14d0)))
           (lamps (bake-fixtures! b def)))
      (setf lights (append lights lamps))
      (loop for (name y decal) in '(("arm 1" 20d0 :decal-m04) ("arm 2" -20d0 :decal-m05))
            do (let ((pin (fixture-pin (find name (object-def-fixtures def) :key #'fixture-name
                                                                            :test #'string=))))
                 (setf lights (append lights
                                      (bake-ship! b (mule-model :decal decal) (vec3-add pin (vec3 0 0 -16.6d0))
                                                  :x '(0 0 1) :y '(0 1 0))))))
      (finish-place-model b lights))))

(defun k-17-model (def)
  "Derelict K-17: the tank Ø 3.05 × 12.7 in peeling foil and scorched steel,
the adapter ring at x 5.80, the bell 1.5 m out of the aft end, the bright
fixture."
  (let* ((b (make-builder)) (hull (builder-hull b)))
    (builder-part b "tank" :hull
                  (lambda ()
                    (cylinder hull (xform '(-6.65d0 0 0) :scale '(8.0d0 1.525d0 1.525d0))
                              (material-id :peeling-foil) 32 :caps :start)
                    (cylinder hull (xform '(1.35d0 0 0) :scale '(4.25d0 1.525d0 1.525d0))
                              (material-id :scorched) 32 :caps nil)
                    (frustum hull (xform '(5.6d0 0 0) :scale '(0.45d0 1 1)) (material-id :scorched) 32
                             1.525d0 1.0d0 :caps :end)))
    (builder-part b "adapter ring" :hull
                  (lambda () (cylinder hull (xform '(5.75d0 0 0) :scale '(0.1d0 1.4d0 1.4d0))
                                       (material-id :scorched) 32)))
    (builder-part b "bell" :hull
                  (lambda ()
                    (frustum hull (xform '(-6.65d0 0 0) :x '(-1 0 0) :y '(0 -1 0) :scale '(1.5d0 1 1))
                             (material-id :nozzle) 24 0.15d0 0.725d0)))
    (bake-fixtures! b def)
    (let ((model (finish-place-model b '())))
      model)))

(defun pod-model (def)
  "POD 14: a box 4.0 × 2.5 × 2.5 with panel seams (rings at x -1, 0 and 1,
the edges), \"POD 14\" stencilled on both long sides."
  (let* ((b (make-builder)) (hull (builder-hull b)))
    (builder-part b "pod" :hull
                  (lambda () (box hull (xform '(0 0 0) :scale '(4d0 2.5d0 2.5d0)) (material-id :cab-paint))))
    (builder-part b "seams" :tube
                  (lambda ()
                    (flet ((tube (a c) (builder-tube b a c 0.02d0 (material-id :steel))))
                      (dolist (x '(-1d0 0d0 1d0))
                        (tube (list x -1.25d0 -1.25d0) (list x 1.25d0 -1.25d0))
                        (tube (list x 1.25d0 -1.25d0) (list x 1.25d0 1.25d0))
                        (tube (list x 1.25d0 1.25d0) (list x -1.25d0 1.25d0))
                        (tube (list x -1.25d0 1.25d0) (list x -1.25d0 -1.25d0))))))
    (dolist (s '(1 -1))
      (builder-part b (if (= s 1) "decal +y" "decal -y") :hull
                    (lambda ()
                      (rect hull (xform (list 0 (* s 1.254d0) 0) :x (list 0 s 0) :y (list s 0 0)
                                                                   :scale '(1d0 2.8d0 0.7d0))
                            (material-id :decal-pod-14) :uv '(0 1 1 0)))))
    (bake-fixtures! b def)
    (finish-place-model b '())))

(defun rock-model (def)
  "A Skua Yard rock: a sphere of its radius under the net, the net's frame (a
square of four tubes 1.2 m across round the stand-off) and the fixture."
  (let* ((b (make-builder)) (hull (builder-hull b))
         (r (shape-radius (first (object-def-shapes def)))))
    (builder-part b "rock" :hull
                  (lambda () (sphere hull (xform '(0 0 0) :scale (list r r r)) (material-id :net) 12 24)))
    (builder-part b "net frame" :tube
                  (lambda ()
                    (let ((x (+ (sqrt (- (* r r) 0.72d0)) 0.01d0)))
                      (loop for (y1 z1 y2 z2) in '((0.6d0 0.6d0 -0.6d0 0.6d0) (-0.6d0 0.6d0 -0.6d0 -0.6d0)
                                                   (-0.6d0 -0.6d0 0.6d0 -0.6d0) (0.6d0 -0.6d0 0.6d0 0.6d0))
                            do (builder-tube b (list x y1 z1) (list x y2 z2) 0.02d0 (material-id :steel))))))
    (bake-fixtures! b def)
    (finish-place-model b '())))

(defun grindstone-model (def)
  "GRINDSTONE: two 20 m Mule trusses at y ±1.8 with their engines, tanks Ø 3.04 × 17
and grapples, the Mule cab on top at x 15.5 to 20 showing the cairn only, the
netted rock Ø 10 at its tabled centre."
  (declare (ignore def))
  (let* ((b (make-builder)) (hull (builder-hull b)) (x0 -26.31d0)
         (truss (truss-ship-model :length 20d0 :bays 13 :tank-radius 1.52d0
                                  :tanks '(("tank" 0.5d0 17.5d0 :lh2-foil))
                                  :wings nil :radiators nil :equipment nil :rcs nil
                                  :lights (list :engine-exit (vec3 -2.2d0 0 -0.5d0))))
         (cab (truss-ship-model :length 20d0 :structure nil :rcs nil :engine-p nil :wings nil
                                :radiators nil :equipment nil :nose nil :cab '(15.5d0 20d0)
                                :decal :decal :decal-u 0.27d0
                                :lights (list :engine-exit (vec3 0 0 0))))
         (lights '()))
    (dolist (y '(1.8d0 -1.8d0)) (bake-ship! b truss (vec3 x0 y 0.01d0)))
    (bake-ship! b cab (vec3 x0 0 0.01d0))
    (builder-part b "rock" :hull
                  (lambda () (sphere hull (xform '(0.59d0 0 0.01d0) :scale '(5d0 5d0 5d0))
                                     (material-id :net) 16 32)))
    (setf lights (list :red (vec3 -6.31d0 -3.75d0 -1.94d0) :green (vec3 -6.31d0 3.75d0 -1.94d0)
                       :strobe (vec3 -8.56d0 0 -5.04d0)))
    (finish-place-model b lights)))

(defun build-object-model (key)
  (let ((def (object-definition key)))
    (ecase key
      (:tern-station (tern-model def))
      (:fulmar-depot (fulmar-model def))
      (:highport (highport-model def))
      (:k-17 (k-17-model def))
      (:pod-14 (pod-model def))
      ((:skua-yard-1 :skua-yard-2 :skua-yard-3 :skua-yard-4 :skua-yard-5 :skua-yard-6)
       (rock-model def))
      (:rock-mover (grindstone-model def)))))

(defun object-model (key)
  "The model of the object definition KEY, made once."
  (cached-model key (lambda () (build-object-model key))))

;;; The engine's trim (docs/design.md "Propulsion"): its axis through the
;;; gimbal turned onto the CoM.

(defun engine-rotation (m-p)
  "The engine's rotation about body y through *engine-gimbal* by
ship-trim-angle: body x goes to (cos a, 0, sin a). A mat3; the engine's
points go to g + R (p − g)."
  (let* ((a (ship-trim-angle m-p)) (c (cos a)) (s (sin a)))
    (mat3 c 0 (- s) 0 1 0 s 0 c)))

(defun engine-exit-position (model m-p)
  "The nozzle exit's centre in body axes, the engine at its trim at M-P."
  (let ((g *engine-gimbal*))
    (vec3-add g (mat3*vec3 (engine-rotation m-p)
                           (vec3-sub (getf (model-lights model) :engine-exit) g)))))

;;; Lights and engine glow (decision 80; decision 100: The lights' clock and
;;; the glows' numbers): camera-facing Gaussian billboards. A glow is its
;;; centre relative to the eye, its radius (the Gaussian's sigma), its RGB
;;; radiance and how far it is pulled towards the eye, clear of the joint it
;;; sits on. Every number live.

(defparameter *strobe-on* 0.1d0 "Seconds the strobe is lit in each period.")
(defparameter *strobe-period* 1.2d0 "Seconds.")
(defparameter *nav-light-radius* 0.05d0 "m.")
(defparameter *nav-light-radiance* 5d0)
(defparameter *nav-red* '(1d0 0d0 0d0)
  "Linear primaries, so the tone map keeps their hue when they saturate.")
(defparameter *nav-green* '(0d0 1d0 0d0))
(defparameter *strobe-radius* 0.05d0 "m.")
(defparameter *strobe-radiance* 20d0)
(defparameter *engine-glow-radius* 0.8d0 "m.")
(defparameter *engine-glow-colour* '(0.45d0 0.6d0 1.0d0))
(defparameter *engine-glow-radiance* 0.1d0
  "At full throttle; faint in sunlight, unsaturated at the deep exposure.")
(defparameter *glow-min-pixels* 1.5d0 "Least size across (2 sigma), pixels.")
(defparameter *light-pull* 1d0 "A navigation light's or the strobe's pull, in radii.")

(defconstant +glow-floats+ 8 "Centre relative to the eye, radius; radiance, pull.")
(defconstant +glow-count+ 4 "Red, green, the strobe, the engine.")

(defun strobe-on-p (time)
  "Whether the strobe is lit at TIME s of the lights' clock: its phase,
TIME mod *strobe-period*, reduced in double on the CPU (docs/architecture.md
\"Floating-origin pitfalls\")."
  (< (mod (float time 1d0) *strobe-period*) *strobe-on*))

(defun engine-glow-radiance (state)
  "*engine-glow-radiance* x throttle while the engine fires (throttle > 0,
propellant > 0, not lost), else 0."
  (let ((throttle (controls-throttle (render-state-controls state))))
    (if (and (> throttle 0d0) (> (render-state-m-p state) 0d0) (not (render-state-loss state)))
        (* *engine-glow-radiance* throttle)
        0d0)))

(defun ship-glows! (out model state eye com)
  "The glows of the render STATE seen from EYE into OUT, +glow-floats+
each, the CoM COM in body axes; returns their count. The navigation lights
and the strobe only while the lights are on; the engine glow at the nozzle
exit's centre while it fires."
  (let ((reference (ship-reference-position state com))
        (q (render-state-q state))
        (lights (model-lights model))
        (n 0))
    (flet ((glow (p radius radiance colour pull)
             ;; q p + (r − q c) − eye, in double.
             (let ((centre (vec3-sub (vec3-add (quat-rotate q p) reference) eye))
                   (o (* n +glow-floats+)))
               (loop for i below 3 for c in colour
                     do (setf (aref out (+ o i)) (coerce (aref centre i) 'single-float)
                              (aref out (+ o 4 i)) (coerce (* radiance c) 'single-float)))
               (setf (aref out (+ o 3)) (coerce radius 'single-float)
                     (aref out (+ o 7)) (coerce pull 'single-float))
               (incf n))))
      (when (render-state-ship-lights-on state)
        (let ((pull (* *light-pull* *nav-light-radius*)))
          (glow (getf lights :red) *nav-light-radius* *nav-light-radiance* *nav-red* pull)
          (glow (getf lights :green) *nav-light-radius* *nav-light-radiance* *nav-green* pull))
        (when (strobe-on-p (render-state-lights-t state))
          (glow (getf lights :strobe) *strobe-radius* *strobe-radiance* '(1d0 1d0 1d0)
                (* *light-pull* *strobe-radius*))))
      (let ((radiance (engine-glow-radiance state)))
        (when (plusp radiance)
          (glow (engine-exit-position model (render-state-m-p state)) *engine-glow-radius*
                radiance *engine-glow-colour* 0d0)))
      n)))

;;; The stack's bounding radius (decision 80)

(defun unique-points (positions)
  (let ((seen (make-hash-table :test #'equalp)) (out '()))
    (dolist (p positions)
      (unless (gethash p seen)
        (setf (gethash p seen) t)
        (push p out)))
    (let ((array (make-array (* 3 (length out)) :element-type 'double-float)))
      (loop for p in out for i from 0 do (replace array p :start1 (* 3 i)))
      array)))

(defun ship-bounding-radius (model m-p)
  "The largest distance from the CoM at M-P kg of propellant to any vertex,
instances expanded, the engine at its trim; conses nothing after the first call."
  (declare (type model model) (type double-float m-p))
  (unless (model-hull-points model)
    (setf (model-hull-points model) (unique-points (model-positions model :engine nil))
          (model-engine-points model)
          (unique-points (floats-positions (model-engine model) 0
                                           (floor (length (model-engine model)) +vertex-floats+)))))
  (bounding-radius (model-hull-points model) (model-engine-points model)
                   (mass-properties-com (ship-mass-properties m-p nil (model-scratch model)))))

(defun bounding-radius (hull engine com)
  (declare (optimize speed) (type (simple-array double-float (*)) hull engine) (type vec3 com))
  (let* ((cx (aref com 0)) (cy (aref com 1)) (cz (aref com 2))
         (gx (aref *engine-gimbal* 0)) (gz (aref *engine-gimbal* 2))
         (h (sqrt (+ (expt (- cx gx) 2) (expt (- cz gz) 2))))
         (c (/ (- cx gx) h)) (s (/ (- cz gz) h))
         (best 0d0))
    (declare (type double-float best))
    (loop for i of-type fixnum from 0 below (length hull) by 3
          do (setf best (max best (+ (expt (- (aref hull i) cx) 2)
                                     (expt (- (aref hull (+ i 1)) cy) 2)
                                     (expt (- (aref hull (+ i 2)) cz) 2)))))
    (loop for i of-type fixnum from 0 below (length engine) by 3
          do (let* ((px (- (aref engine i) gx)) (pz (- (aref engine (+ i 2)) gz))
                    (x (+ gx (* c px) (- (* s pz)))) (z (+ gz (* s px) (* c pz))))
               (setf best (max best (+ (expt (- x cx) 2) (expt (- (aref engine (+ i 1)) cy) 2)
                                       (expt (- z cz) 2))))))
    (sqrt best)))

;;; The stencil decals (decision 80; decision 124): an atlas of 512 x 128 rows,
;;; row 0 at the top, each 0.6 m letters at 183 px/m: Cairn Water's cairn and
;;; "M-07", "M-04", "M-05", then Boreal's "TERN" and the pod's "POD 14". A
;;; model picks its row by its decal material (rows 14 to 18 of the shader).

(defparameter *decal-pixel-size* '(128 512) "Rows, columns of one atlas row.")
(defparameter *decal-cell* 15 "Pixels per cell of the 5 x 7 grid: 0.6 m letters at 183 px/m.")
(defparameter *decal-rows*
  ;; (text first-letter-left top pitch cairn)
  '(("M-07" 150 12 90 t) ("M-04" 150 12 90 t) ("M-05" 150 12 90 t)
    ("TERN" 83 12 90 nil) ("POD 14" 18 12 80 nil))
  "The atlas rows from the top: the text, its first letter's left column, the
letters' top row and pitch (px), and whether the cairn is drawn.")
(defparameter *decal-cairn* '((71 94 55 22) (71 58 41 16.5) (71 32 27.5 11))
  "The cairn's stones from the bottom: centre column, row, half width, half height, px.")

(defparameter *stencil-glyphs*
  ;; (char rows bridges): bridges are cleared rectangles (col0 row0 col1 row1)
  ;; in cells; the dash is a band 1.5 cells tall.
  '((#\M ("10001" "11011" "10101" "10001" "10001" "10001" "10001")
     ((0.85 1 1.15 2) (3.85 1 4.15 2)))
    (#\- ("00000" "00000" "00000" "00000" "00000" "00000" "00000")
     ())
    (#\0 ("01110" "10001" "10001" "10001" "10001" "10001" "01110")
     ((2.35 0 2.65 1) (2.35 6 2.65 7)))
    (#\7 ("11111" "00001" "00010" "00100" "01000" "01000" "01000")
     ((4 0.85 5 1.15)))
    (#\1 ("00100" "01100" "00100" "00100" "00100" "00100" "01110") ())
    (#\4 ("00010" "00110" "01010" "10010" "11111" "00010" "00010") ())
    (#\5 ("11111" "10000" "11110" "00001" "00001" "10001" "01110") ())
    (#\D ("11110" "10001" "10001" "10001" "10001" "10001" "11110")
     ((2.35 0 2.65 1) (2.35 6 2.65 7)))
    (#\E ("11111" "10000" "10000" "11110" "10000" "10000" "11111") ())
    (#\N ("10001" "11001" "10101" "10011" "10001" "10001" "10001") ())
    (#\O ("01110" "10001" "10001" "10001" "10001" "10001" "01110")
     ((2.35 0 2.65 1) (2.35 6 2.65 7)))
    (#\P ("11110" "10001" "10001" "11110" "10000" "10000" "10000")
     ((2.35 0 2.65 1)))
    (#\R ("11110" "10001" "10001" "11110" "10100" "10010" "10001")
     ((2.35 0 2.65 1)))
    (#\T ("11111" "00100" "00100" "00100" "00100" "00100" "00100") ())
    (#\Space ("00000" "00000" "00000" "00000" "00000" "00000" "00000") ())))

(defun linear-srgb-byte (x)
  (let ((x (max 0d0 (min 1d0 (float x 1d0)))))
    (round (* 255 (if (<= x 0.0031308d0) (* 12.92d0 x) (- (* 1.055d0 (expt x (/ 2.4d0))) 0.055d0))))))

(defun decal-colour (material)
  "The opaque RGBA pixel of MATERIAL's albedo, sRGB bytes, R lowest."
  (let ((row (material-id material)))
    (logior (linear-srgb-byte (aref *ship-materials* row 0))
            (ash (linear-srgb-byte (aref *ship-materials* row 1)) 8)
            (ash (linear-srgb-byte (aref *ship-materials* row 2)) 16)
            (ash 255 24))))

(defun decal-glyph-boxes (&optional (row 0))
  "((char left top right bottom) …): each letter's 5 x 7 box of atlas row ROW
in atlas pixels, ends exclusive; spaces have none."
  (destructuring-bind (text left top pitch cairn) (nth row *decal-rows*)
    (declare (ignore cairn))
    (loop for char across text for i from 0
          for x = (+ left (* i pitch))
          for y = (+ top (* row (first *decal-pixel-size*)))
          unless (char= char #\Space)
            collect (list char x y (+ x (* 5 *decal-cell*)) (+ y (* 7 *decal-cell*))))))

(defun stencil-decal-pixels ()
  "The decal atlas: rank-2 (unsigned-byte 32), 5 x 128 rows of 512, for
texture-from-pixels; alpha 0 off the cairn and the letters."
  (let* ((rows (first *decal-pixel-size*)) (columns (second *decal-pixel-size*))
         (pixels (make-array (list (* rows (length *decal-rows*)) columns)
                             :element-type '(unsigned-byte 32) :initial-element 0))
         (black (decal-colour :stencil)) (grey (decal-colour :cairn)) (cell *decal-cell*))
    (loop for (nil nil nil nil cairn) in *decal-rows* for row from 0
          for y-base = (* row rows)
          do (flet ((fill-box (x0 y0 x1 y1 colour)
                      (loop for y from (max y-base (round y0)) below (min (+ y-base rows) (round y1))
                            do (loop for x from (max 0 (round x0)) below (min columns (round x1))
                                     do (setf (aref pixels y x) colour)))))
               (when cairn
                 (loop for (cx cy rx ry) in *decal-cairn*
                       do (dotimes (y rows)
                            (dotimes (x columns)
                              (when (<= (+ (expt (/ (- (+ x 0.5) cx) rx) 2)
                                           (expt (/ (- (+ y 0.5) cy) ry) 2))
                                        1)
                                (setf (aref pixels (+ y-base y) x) grey))))))
               (loop for (char left top) in (decal-glyph-boxes row)
                     for (nil glyph-rows bridges) = (assoc char *stencil-glyphs*)
                     do (loop for glyph-row in glyph-rows for j from 0
                              do (loop for c across glyph-row for i from 0
                                       when (char= c #\1)
                                         do (fill-box (+ left (* i cell)) (+ top (* j cell))
                                                      (+ left (* (1+ i) cell)) (+ top (* (1+ j) cell))
                                                      black)))
                        (when (char= char #\-)
                          (fill-box left (+ top (* 2.75 cell)) (+ left (* 5 cell))
                                    (+ top (* 4.25 cell)) black))
                        (loop for (c0 r0 c1 r1) in bridges
                              do (fill-box (+ left (* c0 cell)) (+ top (* r0 cell))
                                           (+ left (* c1 cell)) (+ top (* r1 cell)) 0)))))
    pixels))

;;; The Mule on the GPU (decision 80; docs/architecture.md "Rendering
;;; precision" item 3): the baked hull and the engine in static vertex
;;; buffers, each instance group's unit primitive and instances in static
;;; buffers, the decal texture; per target a ship block and the material
;;; table, written each frame.

(defvar *mule* nil "The Mule's model, made once.")

(defun mule ()
  (or *mule* (setf *mule* (mule-model))))

(defparameter *earth-albedo* 0.3d0 "Earthshine (decision 80); live.")
(defparameter *earthshine-tint* '(0.85d0 0.92d0 1.0d0) "Live.")
(defparameter *ship-ambient* 0.0005d0 "Live.")

(defconstant +ship-uniform-floats+ 52 "shaders.lisp's ship block: 13 vec4s.")

(defstruct (ship-gpu (:constructor %make-ship-gpu))
  "The static resources of the Mule's draws and the bind group layout."
  model layout hull hull-count engine engine-count
  groups                                ; (primitive count instances count) per group
  decal-view decal-sampler corners
  (objects (make-hash-table :test #'eq)) ; an object-gpu per object definition key
  (object-glow-floats 0 :type fixnum)   ; the most floats one object's glows take
  (object-glows nil)                    ; scratch of that size
  (object-uniforms (make-array +ship-uniform-floats+ :element-type 'single-float
                                                     :initial-element 0f0))
  (scratch (make-mass-properties) :type mass-properties)
  (glows (make-array (* +glow-floats+ +glow-count+) :element-type 'single-float
                                                    :initial-element 0f0))
  (uniforms (make-array +ship-uniform-floats+ :element-type 'single-float :initial-element 0f0))
  (materials (make-array (* 8 (array-dimension *ship-materials* 0))
                         :element-type 'single-float :initial-element 0f0)))

(defun ship-vertex-buffers (&optional instanced)
  `((:array-stride ,(* 4 +vertex-floats+)
     :attributes ((:shader-location 0 :offset 0 :format :float32x3)
                  (:shader-location 1 :offset 12 :format :float32x3)
                  (:shader-location 2 :offset 24 :format :float32x3)))
    ,@(when instanced
        `((:array-stride ,(* 4 +instance-floats+) :step-mode :instance
           :attributes ((:shader-location 3 :offset 0 :format :float32x3)
                        (:shader-location 4 :offset 12 :format :float32x3)
                        (:shader-location 5 :offset 24 :format :float32x3)
                        (:shader-location 6 :offset 36 :format :float32x3)
                        (:shader-location 7 :offset 48 :format :float32x4)))))))

(defun glow-vertex-buffers ()
  `((:array-stride 8 :attributes ((:shader-location 0 :offset 0 :format :float32x2)))
    (:array-stride ,(* 4 +glow-floats+) :step-mode :instance
     :attributes ((:shader-location 1 :offset 0 :format :float32x4)
                  (:shader-location 2 :offset 16 :format :float32x4)))))

(defun make-ship-gpu (renderer format)
  "The Mule's buffers, decal and layout, owned by RENDERER, and its
:ship-mesh and :ship-instanced live pipelines into FORMAT: opaque, depth
written, compare :greater; and :ship-glow: additive, depth tested, not written."
  (let* ((model (mule))
         (layout (gpu-create renderer (luv:make-bind-group-layout-descriptor
                                       :entries '((:binding 0 :type :uniform-buffer)
                                                  (:binding 1 :type :texture)
                                                  (:binding 2 :type :sampler)
                                                  (:binding 3 :type :uniform-buffer)
                                                  (:binding 4 :type :storage-buffer)))))
         (depth '(:format :depth32-float :depth-write-enabled t :depth-compare :greater)))
    (flet ((count-of (floats size) (floor (length floats) size)))
      (multiple-value-bind (texture view sampler)
          (texture-from-pixels renderer (stencil-decal-pixels) "decal")
        (declare (ignore texture))
        (live-pipeline renderer :ship-mesh layout (ship-vertex-buffers) :format format
                                                                         :depth-stencil depth)
        (live-pipeline renderer :ship-instanced layout (ship-vertex-buffers t) :format format
                                                                                :depth-stencil depth)
        (live-pipeline renderer :ship-glow layout (glow-vertex-buffers)
                       :format format :blend :premultiplied-alpha
                       :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                        :depth-compare :greater))
        (let ((gpu (%make-ship-gpu
         :model model :layout layout
         :hull (vertex-buffer renderer (model-vertices model) "hull")
         :hull-count (count-of (model-vertices model) +vertex-floats+)
         :engine (vertex-buffer renderer (model-engine model) "engine")
         :engine-count (count-of (model-engine model) +vertex-floats+)
         :groups (loop for g in (model-groups model)
                       for kind = (string-downcase (instance-group-kind g))
                       collect (list (vertex-buffer renderer (instance-group-primitive g) kind)
                                     (count-of (instance-group-primitive g) +vertex-floats+)
                                     (vertex-buffer renderer (instance-group-instances g)
                                                    (format nil "~A instances" kind))
                                     (count-of (instance-group-instances g) +instance-floats+)))
         :decal-view view :decal-sampler sampler
         :corners (vertex-buffer renderer (star-corners) "glow corners"))))
          (make-object-gpus! gpu renderer)
          gpu)))))

(defstruct ship-slot
  "A target's ship block, material table, bind group and glow instances, and
its OBJECTS: an object-slot per entry of *objects* (decision 124)."
  buffer materials bind-group glows (objects (vector)))

(defstruct object-slot
  "One object's ship block, bind group (the frame block, the shared decal and
material table, its own block) and glow instances in a target."
  buffer bind-group glows)

(defun make-ship-target-slot (renderer ship frame-buffer)
  "The per-target resources of SHIP's draws; FRAME-BUFFER the target's frame block."
  (let* ((device (renderer-device renderer))
         (buffer (luv:create device (luv:make-buffer-descriptor
                                     :label "ship" :size (* 4 +ship-uniform-floats+)
                                     :usage '(:uniform :copy-dst))))
         (materials (luv:create device (luv:make-buffer-descriptor
                                        :label "ship materials"
                                        :size (* 4 (length (ship-gpu-materials ship)))
                                        :usage '(:storage :copy-dst)))))
    (make-ship-slot
     :objects (make-object-slots renderer ship frame-buffer materials)
     :buffer buffer :materials materials
     :glows (luv:create device (luv:make-buffer-descriptor
                                :label "ship glows"
                                :size (* 4 (length (ship-gpu-glows ship)))
                                :usage '(:vertex :copy-dst)))
     :bind-group (luv:create device (luv:make-bind-group-descriptor
                                     :layout (ship-gpu-layout ship)
                                     :entries `((:binding 0 :resource ,frame-buffer)
                                                (:binding 1 :resource ,(ship-gpu-decal-view ship))
                                                (:binding 2 :resource ,(ship-gpu-decal-sampler ship))
                                                (:binding 3 :resource ,buffer)
                                                (:binding 4 :resource ,materials)))))))

(defun release-ship-slot (slot)
  (loop for o across (ship-slot-objects slot)
        do (luv:destroy (object-slot-glows o))
           (luv:destroy (object-slot-bind-group o))
           (luv:destroy (object-slot-buffer o)))
  (luv:destroy (ship-slot-glows slot))
  (luv:destroy (ship-slot-bind-group slot))
  (luv:destroy (ship-slot-materials slot))
  (luv:destroy (ship-slot-buffer slot)))

(defun ship-materials! (out)
  "*ship-materials* as two vec4s per row: r g b weight; exponent emission 0 0."
  (dotimes (row (array-dimension *ship-materials* 0) out)
    (let ((o (* 8 row)))
      (dotimes (k 4)
        (setf (aref out (+ o k)) (aref *ship-materials* row k)))
      (setf (aref out (+ o 4)) (aref *ship-materials* row 4)
            (aref out (+ o 5)) (aref *ship-materials* row 5)
            (aref out (+ o 6)) 0f0 (aref out (+ o 7)) 0f0))))

(defun ship-reference-position (state com &optional (out (make-vec3)))
  "R's position, r − q c, double (decision 80)."
  (let ((qc (quat-rotate (render-state-q state) com)))
    (vec3-sub! out (render-state-r state) qc)))

(declaim (inline earth-lit-cap))
(defun earth-lit-cap (r s-hat)
  "The lit share of the Earth seen from R (decision 99: Earthshine from the
lit part of the visible cap): max(0, cos θ) of the Sun's zenith angle θ,
averaged over θ0 ± λ, θ0 its angle at the sub-ship point (S-HAT the unit
direction to the Sun), λ = acos(R⊕ / |r|) the cap's half-angle. 0 when the
whole cap is past the terminator. Conses nothing."
  (declare (optimize speed) (type vec3 r s-hat))
  (let* ((d (vec3-norm r))
         (cap (if (> d +radius-earth+)
                  (acos (the (double-float 0d0 1d0) (/ +radius-earth+ d)))
                  0d0))
         (theta (acos (the (double-float -1d0 1d0)
                           (max -1d0 (min 1d0 (/ (vec3-dot r s-hat) d))))))
         (low (- theta cap)))
    (declare (type double-float d cap theta low))
    (cond ((>= low (/ pi 2)) 0d0)
          ((< cap 1d-9) (max 0d0 (cos theta)))
          (t (/ (- (sin (min (+ theta cap) (/ pi 2))) (sin low)) (* 2 cap))))))

(defun ship-uniforms! (out state eye com)
  "The ship block (shaders.lisp) into OUT for the render STATE seen from EYE,
the CoM COM in body axes: the rows of q, R relative to the eye (decision 5:
Precision of positions) with the glows' least sigma in pixels, the gimbal and the trim rotation at m_p, then in
body axes the Sun's direction with (1 AU / d)^2 x sun-fraction, the
direction to Earth with the earthshine, the tint and ambient, the eye with
the lights-on flag, the emissive colour."
  (let* ((q (render-state-q state))
         (rows (mat3-from-quat q))
         (inverse (quat-conjugate q))
         (r (render-state-r state))
         (reference (ship-reference-position state com))
         (sun (vec3-sub (render-state-sun state) r))
         (d-sun (vec3-norm sun))
         (irradiance (expt (/ +au+ d-sun) 2))
         (r-hat (vec3-normalize r))
         (s-hat (vec3-scale sun (/ d-sun)))
         (earthshine (* *earth-albedo* (expt (/ +radius-earth+ (vec3-norm r)) 2)
                        (earth-lit-cap r s-hat) irradiance))
         (trim (engine-rotation (render-state-m-p state))))
    (flet ((put (index x y z w)
             (setf (aref out index) (coerce x 'single-float)
                   (aref out (+ index 1)) (coerce y 'single-float)
                   (aref out (+ index 2)) (coerce z 'single-float)
                   (aref out (+ index 3)) (coerce w 'single-float)))
           (body (v) (quat-rotate inverse v)))
      (dotimes (i 3)
        (put (* 4 i) (aref rows (* 3 i)) (aref rows (+ (* 3 i) 1)) (aref rows (+ (* 3 i) 2)) 0))
      (relative-single reference eye out 12)
      (setf (aref out 15) (coerce (* 0.5d0 *glow-min-pixels*) 'single-float))
      (put 16 (aref *engine-gimbal* 0) (aref *engine-gimbal* 1) (aref *engine-gimbal* 2) 0)
      (dotimes (i 3)
        (put (+ 20 (* 4 i)) (aref trim (* 3 i)) (aref trim (+ (* 3 i) 1)) (aref trim (+ (* 3 i) 2)) 0))
      (let ((s (body s-hat)) (e (body (vec3-scale r-hat -1d0)))
            (eye-body (body (vec3-sub eye reference)))
            (emissive (render-state-ship-emissive state)))
        (put 32 (aref s 0) (aref s 1) (aref s 2) (* irradiance (render-state-sun-fraction state)))
        (put 36 (aref e 0) (aref e 1) (aref e 2) earthshine)
        (destructuring-bind (tr tg tb) *earthshine-tint*
          (put 40 tr tg tb *ship-ambient*))
        (put 44 (aref eye-body 0) (aref eye-body 1) (aref eye-body 2)
             (if (render-state-ship-lights-on state) 1 0))
        (put 48 (aref emissive 0) (aref emissive 1) (aref emissive 2) 0)))
    out))

(defun encode-ship (pass renderer ship slot state eye)
  "The Mule into the open scene PASS (after the impostors, before the
ribbons), about the stack's CoM (render-state-com, with the latched load):
the hull, the instance groups, then the engine as instance 1 of
:ship-mesh, so its vertex stage turns it by its trim; then its glows."
  (let ((com (if (render-state-com-valid state)
                 (render-state-com state)
                 (mass-properties-com (ship-mass-properties (render-state-m-p state) nil
                                                            (ship-gpu-scratch ship)))))
        (bind-group (ship-slot-bind-group slot)))
    (luv:write-buffer (ship-slot-buffer slot) (ship-uniforms! (ship-gpu-uniforms ship) state eye com))
    (luv:write-buffer (ship-slot-materials slot) (ship-materials! (ship-gpu-materials ship)))
    (luv:set-pipeline pass (renderer-pipeline renderer :ship-mesh))
    (luv:set-bind-group pass 0 bind-group)
    (luv:set-vertex-buffer pass 0 (ship-gpu-hull ship))
    (luv:draw pass (ship-gpu-hull-count ship))
    (luv:set-vertex-buffer pass 0 (ship-gpu-engine ship))
    (luv:draw pass (ship-gpu-engine-count ship) 1 0 1)
    (luv:set-pipeline pass (renderer-pipeline renderer :ship-instanced))
    (luv:set-bind-group pass 0 bind-group)
    (loop for (primitive vertices instances count) in (ship-gpu-groups ship)
          do (luv:set-vertex-buffer pass 0 primitive)
             (luv:set-vertex-buffer pass 1 instances)
             (luv:draw pass vertices count))
    (let ((glows (ship-glows! (ship-gpu-glows ship) (ship-gpu-model ship) state eye com)))
      (when (plusp glows)
        (luv:write-buffer (ship-slot-glows slot) (ship-gpu-glows ship))
        (luv:set-pipeline pass (renderer-pipeline renderer :ship-glow))
        (luv:set-bind-group pass 0 bind-group)
        (luv:set-vertex-buffer pass 0 (ship-gpu-corners ship))
        (luv:set-vertex-buffer pass 1 (ship-slot-glows slot))
        (luv:draw pass 6 glows)))))

;;; Objects (decision 124: Meshes of the places: models, scenery liveries,
;;; lights, far dots, the grapple's claws, floodlight and range finder): each
;;; promoted or rails object of the render state as its model's mesh from 2 px
;;; of projected extent, below that as a dim dot, its lights always.

(defparameter *lamp-glow-radius* 0.04d0 "m: a fixture lamp.")
(defparameter *lamp-radiance* 5d0)
(defparameter *strobe-offset* 0.37d0 "Seconds of strobe phase per object index.")
(defparameter *dot-radiance* 0.5d0 "x the Sun term x (1 + cos phase) / 2 x sun-fraction.")
(defparameter *object-mesh-pixels* 2d0 "Least projected extent drawn as a mesh, px.")

(defstruct (object-gpu (:constructor %make-object-gpu))
  "One model's buffers (the hull, the engine if any, the instance groups as
ship-gpu's), its EXTENT (the farthest vertex from the CoM), the DOT-RADIUS of
a disc of a quarter of its shapes' surface, and its BEACONS (kind position)."
  key hull (hull-count 0 :type fixnum) engine (engine-count 0 :type fixnum) groups
  (extent 0d0 :type double-float) (dot-radius 0d0 :type double-float) beacons)

(defun shapes-quarter-area (def)
  "A quarter of DEF's shapes' surface, m²: the mean cross-section of a convex body."
  (/ (loop for shape in (object-def-shapes def)
           sum (ecase (shape-kind shape)
                 (:sphere (* 4 pi (expt (shape-radius shape) 2)))
                 (:box (let ((s (shape-size shape)))
                         (* 2 (+ (* (aref s 0) (aref s 1)) (* (aref s 1) (aref s 2))
                                 (* (aref s 2) (aref s 0))))))))
     4d0))

(defun make-object-gpu (renderer def)
  (let* ((model (object-model (object-def-key def)))
         (label (string-downcase (object-def-key def))))
    (flet ((count-of (floats size) (floor (length floats) size)))
      (%make-object-gpu
       :key (object-def-key def)
       :hull (and (plusp (length (model-vertices model)))
                  (vertex-buffer renderer (model-vertices model) label))
       :hull-count (count-of (model-vertices model) +vertex-floats+)
       :engine (and (plusp (length (model-engine model)))
                    (vertex-buffer renderer (model-engine model) (format nil "~A engine" label)))
       :engine-count (count-of (model-engine model) +vertex-floats+)
       :groups (loop for g in (model-groups model)
                     for kind = (string-downcase (instance-group-kind g))
                     for count = (count-of (instance-group-instances g) +instance-floats+)
                     when (plusp count)
                       collect (list (vertex-buffer renderer (instance-group-primitive g)
                                                    (format nil "~A ~A" label kind))
                                     (count-of (instance-group-primitive g) +vertex-floats+)
                                     (vertex-buffer renderer (instance-group-instances g)
                                                    (format nil "~A ~A instances" label kind))
                                     count))
       :extent (float (model-extent model) 1d0)
       :dot-radius (sqrt (/ (shapes-quarter-area def) pi))
       :beacons (model-beacons model)))))

(defun make-object-gpus! (gpu renderer)
  "An object-gpu for each of *objects* into GPU's table, and the glow capacity
(each model's beacons and a dot)."
  (let ((most 1))
    (dolist (def *objects*)
      (let ((object (make-object-gpu renderer def)))
        (setf (gethash (object-def-key def) (ship-gpu-objects gpu)) object)
        (setf most (max most (1+ (length (object-gpu-beacons object)))))))
    (setf (ship-gpu-object-glow-floats gpu) (* +glow-floats+ most)
          (ship-gpu-object-glows gpu) (make-array (* +glow-floats+ most) :element-type 'single-float
                                                                          :initial-element 0f0))
    gpu))

(defun make-object-slots (renderer ship frame-buffer materials)
  "An object-slot per entry of *objects* for one target (the object's block,
bind group sharing the target's material table MATERIALS, glow instances)."
  (let ((device (renderer-device renderer)))
    (coerce
     (loop for def in *objects*
           collect (let ((buffer (luv:create device (luv:make-buffer-descriptor
                                                      :label "object" :size (* 4 +ship-uniform-floats+)
                                                      :usage '(:uniform :copy-dst)))))
                     (make-object-slot
                      :buffer buffer
                      :glows (luv:create device (luv:make-buffer-descriptor
                                                 :label "object glows"
                                                 :size (* 4 (ship-gpu-object-glow-floats ship))
                                                 :usage '(:vertex :copy-dst)))
                      :bind-group (luv:create
                                   device (luv:make-bind-group-descriptor
                                           :layout (ship-gpu-layout ship)
                                           :entries `((:binding 0 :resource ,frame-buffer)
                                                      (:binding 1 :resource ,(ship-gpu-decal-view ship))
                                                      (:binding 2 :resource ,(ship-gpu-decal-sampler ship))
                                                      (:binding 3 :resource ,buffer)
                                                      (:binding 4 :resource ,materials)))))))
     'simple-vector)))
