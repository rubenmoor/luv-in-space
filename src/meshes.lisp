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
    :radiator :aluminium :nozzle :throat :glass :hatch :decal)
  "Row order of *ship-materials*; a vertex's material id is the row.")

(defparameter *ship-materials*
  (make-array '(15 6)
              :element-type 'single-float
              :initial-contents
              '((0.45 0.45 0.47 0.15 16.0 0.0)     ; steel, x0.6 at weld nodes
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
                (0.80 0.80 0.78 0.05 8.0 0.0)))    ; decal: paint under the texture
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

(defparameter *unit-primitives*
  '((:tube . "8-sided tube, x 0 to 1, radius 1, closed")
    (:box . "cube, centred, side 1")
    (:cone . "nozzle cone, x 0 to 1, radius 0.3 to 1, closed at 0")))

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

;;; The Mule (decision 80; docs/design.md "How they look", "Parts")

(defparameter *cab-axis* '(0 -3.5d0) "y, z of the cab's axis.")
(defparameter *cab-radius* 1.5d0)
(defparameter *cab-skin* 0.004d0 "Decal and hatch above the cab's wall, m.")
(defparameter *porthole-skin* 0.012d0 "A flat porthole's centre above the curved wall, m.")
(defparameter *decal-span* '(12.1d0 14.9d0) "x of the decal, m.")
(defparameter *decal-height* 0.7d0 "Arc length of the decal, m.")
(defparameter *decal-angle* (* 25 (/ pi 180)) "The decal's centre above the y axis.")

(defun degrees-to-radians (d) (* d (/ pi 180)))

(defun mule-model ()
  "The Mule: the baked hull, the instance groups, the engine, the lights."
  (let ((hull (make-float-buffer)) (engine (make-float-buffer))
        (groups (list (list :tube) (list :box) (list :cone)))
        (parts '()))
    (labels ((id (name) (material-id name))
             (vertex-count (buffer) (floor (fill-pointer buffer) +vertex-floats+))
             (part (name where thunk)
               (let* ((group (assoc where groups))
                      (start (if group (length (rest group))
                                 (vertex-count (if (eq where :engine) engine hull)))))
                 (funcall thunk)
                 (push (list name where start
                             (if group (length (rest group))
                                 (vertex-count (if (eq where :engine) engine hull))))
                       parts)))
             (add (kind instance) (nconc (assoc kind groups) (list instance)))
             (tube (a b radius) (add :tube (tube-instance a b radius (id :steel))))
             (cab-xf (x0 x1 radius)
               (xform (list x0 0 (second *cab-axis*)) :y '(0 1 0) :scale (list (- x1 x0) radius radius))))
      ;; Truss: the rods of ship.lisp's truss-members.
      (part "truss" :tube
            (lambda ()
              (loop for (nil centre direction length) in (truss-members)
                    for half = (vec3-scale direction (* 0.5d0 length))
                    do (tube (vec3-sub centre half) (vec3-add centre half)
                             (if (= length 15.0d0) 0.038d0 0.030d0)))))
      (part "cab struts" :tube
            (lambda ()
              (dolist (x '(11.0d0 14.5d0))
                (dolist (s '(1 -1))
                  (tube (list x (* s 1.8d0) -1.8d0) (list x (* s 0.9d0) -2.3d0) 0.04d0)))))
      (part "thrust frame" :tube
            (lambda ()
              (dolist (y '(1.8d0 -1.8d0))
                (dolist (z '(1.8d0 -1.8d0))
                  (let* ((corner (vec3 0 y z))
                         (end (vec3-add-scaled *engine-gimbal*
                                               (vec3-normalize (vec3-sub corner *engine-gimbal*))
                                               0.3d0)))
                    (tube corner end 0.04d0))))))
      (part "grapple struts" :tube
            (lambda ()
              (dolist (y '(1 -1))
                (dolist (z '(1 -1))
                  (let ((rim (* 0.3d0 (sqrt 0.5d0))))
                    (tube (list 15.0d0 (* y 1.8d0) (* z 1.8d0)) (list 15.0d0 (* y rim) (* z rim))
                          0.03d0))))))
      (part "wing booms" :tube
            (lambda ()
              (dolist (s '(1 -1))
                (tube (list 5.0d0 (* s 1.8d0) 0) (list 5.0d0 (* s 2.3d0) 0) 0.04d0))))
      ;; RCS: a block per thruster triple of ship.lisp, a nozzle per thruster
      ;; pointing along its exhaust, from the block's face.
      (part "RCS blocks" :box
            (lambda ()
              (loop for i from 0 below +rcs-count+ by 3
                    do (add :box (instance (list (aref *rcs-positions* i 0) (aref *rcs-positions* i 1)
                                                 (aref *rcs-positions* i 2))
                                           '(1 0 0) '(0 1 0) '(0.3d0 0.4d0 0.4d0) (id :aluminium))))))
      (part "RCS nozzles" :cone
            (lambda ()
              (dotimes (i +rcs-count+)
                (let* ((exhaust (vec3 (- (aref *rcs-directions* i 0)) (- (aref *rcs-directions* i 1))
                                      (- (aref *rcs-directions* i 2))))
                       (face (+ (* 0.15d0 (abs (aref exhaust 0)))
                                (* 0.2d0 (+ (abs (aref exhaust 1)) (abs (aref exhaust 2))))))
                       (centre (vec3 (aref *rcs-positions* i 0) (aref *rcs-positions* i 1)
                                     (aref *rcs-positions* i 2))))
                  (add :cone (instance (vec3-add-scaled centre exhaust face) exhaust nil
                                       '(0.12d0 0.08d0 0.08d0) (id :nozzle)))))))
      (part "claws" :box
            (lambda ()
              (dotimes (k 3)
                (let ((a (degrees-to-radians (* 120 k))))
                  (add :box (instance (list 16.3d0 (* 0.85d0 (sin a)) (* -0.85d0 (cos a)))
                                      '(1 0 0) (list 0 (sin a) (- (cos a)))
                                      '(0.6d0 0.1d0 0.1d0) (id :steel)))))))
      ;; Tanks with hemispherical domes, foil-wrapped.
      (loop for (name x0 x1 material) in '(("LH2 tank" 0.50d0 9.36d0 :lh2-foil)
                                           ("LOX tank" 9.76d0 13.48d0 :lox-foil))
            do (let ((r 1.6d0) (m (id material)))
                 (part name :hull
                       (lambda ()
                         (cylinder hull (xform (list (+ x0 r) 0 0) :scale (list (- x1 x0 r r) r r))
                                   m 32 :caps nil)
                         (hemisphere hull (xform (list (+ x0 r) 0 0) :x '(-1 0 0) :y '(0 -1 0)
                                                                     :scale (list r r r))
                                     m 8 32)
                         (hemisphere hull (xform (list (- x1 r) 0 0) :scale (list r r r)) m 8 32)))))
      ;; Cab: flat ends, window, portholes, hatch, decals.
      (part "cab" :hull
            (lambda () (cylinder hull (cab-xf 10.5d0 15.0d0 *cab-radius*) (id :cab-paint) 32)))
      (part "window" :hull
            (lambda () (rect hull (xform '(15.003d0 0 -3.6d0) :y '(0 1 0) :scale '(1 1.2d0 0.6d0))
                             (id :glass))))
      (part "portholes" :hull
            (lambda ()
              (dolist (s '(1 -1))
                (let* ((a (degrees-to-radians 37)) (r (+ *cab-radius* *porthole-skin*))
                       (normal (list 0 (* s (cos a)) (sin a))))
                  (disc hull (xform (list 14.1d0 (* s r (cos a)) (+ (second *cab-axis*) (* r (sin a))))
                                    :x normal :y '(1 0 0) :scale '(1 0.175d0 0.175d0))
                        (id :glass) 16)))))
      (part "hatch" :hull
            (lambda ()
              (let ((half (/ 0.6d0 (+ *cab-radius* *cab-skin*))))
                (cylinder-patch hull (cab-xf 11.0d0 11.8d0 (+ *cab-radius* *cab-skin*)) (id :hatch)
                                12 (- half) half :uv '(-0.4d0 0.4d0 -0.6d0 0.6d0)))))
      ;; The decal reads left to right from outside: along +x on +y, -x on -y;
      ;; v 0 at its top (towards -z).
      (destructuring-bind (x0 x1) *decal-span*
        (let* ((r (+ *cab-radius* *cab-skin*))
               (half (/ *decal-height* r 2))
               (xf (cab-xf x0 x1 r)))
          (part "decal +y" :hull
                (lambda ()
                  (cylinder-patch hull xf (id :decal) 8 (- (- *decal-angle*) half)
                                  (+ (- *decal-angle*) half) :uv '(0 1 0 1))))
          (part "decal -y" :hull
                (lambda ()
                  (cylinder-patch hull xf (id :decal) 8 (- (+ pi *decal-angle*) half)
                                  (+ pi *decal-angle* half) :uv '(1 0 1 0))))))
      (part "grapple" :hull
            (lambda ()
              (cylinder hull (xform '(15.0d0 0 0) :scale '(0.8d0 0.3d0 0.3d0)) (id :steel) 16)
              (frustum hull (xform '(15.8d0 0 0) :scale '(0.8d0 1 1)) (id :steel) 16 0.3d0 0.8d0)))
      (part "solar wings" :hull
            (lambda ()
              (dolist (s '(1 -1))
                (box hull (xform (list 5.0d0 (* s 5.3d0) 0) :scale '(2.0d0 6.0d0 0.05d0))
                     (append (make-list 5 :initial-element (id :steel)) (list (id :solar-cells)))))))
      (part "radiators" :hull
            (lambda ()
              (dolist (s '(1 -1))
                (box hull (xform (list 8.0d0 (* s 1.2d0) 2.55d0) :scale '(4.0d0 0.05d0 1.5d0))
                     (id :radiator)))))
      (part "gas conditioning" :hull
            (lambda () (box hull (xform '(9.5d0 0 1.4d0) :scale '(1.0d0 1.2d0 0.6d0)) (id :aluminium))))
      (part "equipment bay" :hull
            (lambda () (box hull (xform '(10.0d0 0 2.3d0) :scale '(1.0d0 1.6d0 0.8d0)) (id :aluminium))))
      ;; The engine at zero trim, its axis at z -0.5: the bell from the
      ;; throat (the axial parameter 0 there), chamber, turbopumps.
      (part "engine" :engine
            (lambda ()
              (frustum engine (xform '(-0.6d0 0 -0.5d0) :x '(-1 0 0) :y '(0 -1 0) :scale '(1.6d0 1 1))
                       (id :nozzle) 24 0.15d0 0.725d0)
              (cylinder engine (xform '(-0.6d0 0 -0.5d0) :scale '(0.6d0 0.25d0 0.25d0)) (id :steel) 16)
              (dolist (s '(1 -1))
                (box engine (xform (list -0.3d0 (* s 0.35d0) -0.5d0) :scale '(0.3d0 0.25d0 0.25d0))
                     (id :aluminium)))))
      (make-model
       :vertices (simple-floats hull)
       :engine (simple-floats engine)
       :groups (loop for (kind . instances) in groups
                     collect (make-instance-group
                              kind (unit-primitive kind)
                              (let ((out (make-array (* +instance-floats+ (length instances))
                                                     :element-type 'single-float)))
                                (loop for inst in instances for i from 0
                                      do (replace out inst :start1 (* i +instance-floats+)))
                                out)))
       :parts (reverse parts)
       :lights (list :red (vec3 15.0d0 -1.8d0 -1.8d0) :green (vec3 15.0d0 1.8d0 -1.8d0)
                     :strobe (vec3 12.75d0 0 -5.05d0) :engine-exit (vec3 -2.2d0 0 -0.5d0))))))

;;; The engine's trim (docs/design.md "Propulsion"): its axis through the
;;; gimbal turned onto the CoM.

(defun engine-rotation (m-p)
  "The engine's rotation about body y through *engine-gimbal* by
ship-trim-angle: body x goes to (cos a, 0, sin a). A mat3; the engine's
points go to g + R (p − g)."
  (let* ((a (ship-trim-angle m-p)) (c (cos a)) (s (sin a)))
    (mat3 c 0 (- s) 0 1 0 s 0 c)))

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

;;; The stencil decal (decision 80): Cairn Water's cairn and "M-07", 2.8 x
;;; 0.7 m in a 512 x 128 texture, row 0 at the top.

(defparameter *decal-pixel-size* '(128 512) "Rows, columns.")
(defparameter *decal-cell* 15 "Pixels per cell of the 5 x 7 grid: 0.6 m letters at 183 px/m.")
(defparameter *decal-letters* '(150 12 90)
  "The first letter's left column and top row, and the letters' pitch, px.")
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
     ((4 0.85 5 1.15)))))

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

(defun decal-glyph-boxes ()
  "((char left top right bottom) …): each letter's 5 x 7 box in pixels, ends exclusive."
  (destructuring-bind (left top pitch) *decal-letters*
    (loop for char across "M-07" for i from 0
          for x = (+ left (* i pitch))
          collect (list char x top (+ x (* 5 *decal-cell*)) (+ top (* 7 *decal-cell*))))))

(defun stencil-decal-pixels ()
  "The decal: rank-2 (unsigned-byte 32), 128 x 512, for texture-from-pixels;
alpha 0 off the cairn and the letters."
  (let ((pixels (make-array *decal-pixel-size* :element-type '(unsigned-byte 32) :initial-element 0))
        (black (decal-colour :stencil)) (grey (decal-colour :cairn)) (cell *decal-cell*))
    (flet ((fill-box (x0 y0 x1 y1 colour)
             (loop for y from (max 0 (round y0)) below (min 128 (round y1))
                   do (loop for x from (max 0 (round x0)) below (min 512 (round x1))
                            do (setf (aref pixels y x) colour)))))
      (loop for (cx cy rx ry) in *decal-cairn*
            do (dotimes (y 128)
                 (dotimes (x 512)
                   (when (<= (+ (expt (/ (- (+ x 0.5) cx) rx) 2) (expt (/ (- (+ y 0.5) cy) ry) 2)) 1)
                     (setf (aref pixels y x) grey)))))
      (loop for (char left top) in (decal-glyph-boxes)
            for (nil rows bridges) = (assoc char *stencil-glyphs*)
            do (loop for row in rows for j from 0
                     do (loop for c across row for i from 0
                              when (char= c #\1)
                                do (fill-box (+ left (* i cell)) (+ top (* j cell))
                                             (+ left (* (1+ i) cell)) (+ top (* (1+ j) cell)) black)))
               (when (char= char #\-)
                 (fill-box left (+ top (* 2.75 cell)) (+ left (* 5 cell)) (+ top (* 4.25 cell)) black))
               (loop for (c0 r0 c1 r1) in bridges
                     do (fill-box (+ left (* c0 cell)) (+ top (* r0 cell))
                                  (+ left (* c1 cell)) (+ top (* r1 cell)) 0))))
    pixels))
