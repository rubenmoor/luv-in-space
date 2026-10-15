(in-package #:luv-in-space)

;;; The Mule: its parts, mass properties, engine and RCS layout
;;; (docs/design.md "The ship(s)"; decision 22: The player's ship and its
;;; propulsion, decision 71: The Mule's mass model in ship.lisp). Body axes
;;; FRD, positions in metres from R, the truss's aft end on its centre line.

;;; Parts

(defparameter *ship-parts*
  ;; (name mass-kg centres size-xyz shape): the mass is split evenly over the
  ;; centres; size is the part's box, a cylinder's (length Ø Ø) along x.
  ;; Every row is docs/design.md "Parts", shapes of decision 71.
  '(("truss" 2131 ((7.5d0 0 0)) (15.0d0 3.6d0 3.6d0) :truss)
    ("crew cab" 2466 ((12.75d0 0 -3.5d0)) (4.5d0 3.0d0 3.0d0) :tube)
    ("main engine" 190 ((-0.7d0 0 -0.5d0)) (2.2d0 1.45d0 1.45d0) :cylinder)
    ("LH2 tank" 1060 ((4.93d0 0 0)) (8.86d0 3.2d0 3.2d0) :tube)
    ("LOX tank" 579 ((11.62d0 0 0)) (3.73d0 3.2d0 3.2d0) :tube)
    ("grapple" 450 ((15.8d0 0 0)) (1.6d0 1.6d0 1.6d0) :cylinder)
    ("RCS block" 136 ((0.5d0 1.8d0 1.8d0) (0.5d0 -1.8d0 1.8d0)
                      (0.5d0 1.8d0 -1.8d0) (0.5d0 -1.8d0 -1.8d0)
                      (14.5d0 1.8d0 1.8d0) (14.5d0 -1.8d0 1.8d0)
                      (14.5d0 1.8d0 -1.8d0) (14.5d0 -1.8d0 -1.8d0))
     (0.3d0 0.4d0 0.4d0) :box)
    ("gas conditioning" 220 ((9.5d0 0 1.4d0)) (1.0d0 1.2d0 0.6d0) :box)
    ("radiator" 120 ((8.0d0 1.2d0 2.55d0) (8.0d0 -1.2d0 2.55d0)) (4.0d0 0.05d0 1.5d0) :box)
    ("solar wing" 140 ((5.0d0 5.3d0 0) (5.0d0 -5.3d0 0)) (2.0d0 6.0d0 0.05d0) :box)
    ("equipment bay" 270 ((10.0d0 0 2.3d0)) (1.0d0 1.6d0 0.8d0) :box)
    ;; 199 kg, not the table's estimated 200, so the parts sum to the dry 7,961 kg.
    ("cabling and plumbing" 199 ((7.5d0 0 -1.8d0)) (15.0d0 0.2d0 0.2d0) :box))
  "The Mule's parts, docs/design.md \"Parts\".")

(defun truss-members ()
  "The truss as rods (decision 71): ((weight centre direction length) …),
weights in kg per member before the ×1.15 that scales them to the row."
  (let ((members '()))
    (flet ((rod (weight centre direction length)
             (push (list weight (apply #'vec3 centre) (vec3-normalize (apply #'vec3 direction))
                         length)
                   members)))
      (dolist (y '(1.8d0 -1.8d0))
        (dolist (z '(1.8d0 -1.8d0))
          (rod 112.65d0 (list 7.5d0 y z) '(1 0 0) 15.0d0)))
      (dotimes (k 11)
        (let ((x (* 1.5d0 k)))
          (dolist (s '(1.8d0 -1.8d0))
            (rod 16.06d0 (list x s 0) '(0 0 1) 3.6d0)
            (rod 16.06d0 (list x 0 s) '(0 1 0) 3.6d0))))
      (dotimes (k 10)
        (let ((x (+ (* 1.5d0 k) 0.75d0))
              (sign (if (evenp k) 3.6d0 -3.6d0)))
          (dolist (s '(1.8d0 -1.8d0))
            (rod 17.39d0 (list x s 0) (list 1.5d0 0 sign) 3.9d0)
            (rod 17.39d0 (list x 0 s) (list 1.5d0 sign 0) 3.9d0)))))
    (nreverse members)))

;;; Mass sums about R: mass, first moment Σ m c, second moment
;;; Σ (I_c + m (|c|² 𝟙 − c cᵀ)) (docs/design.md "Inertia").

(declaim (inline add-mass!))

(defun add-mass! (second m centre local)
  "Add a body of mass m at CENTRE with tensor LOCAL about it to SECOND."
  (declare (type mat3 second local) (type vec3 centre) (type double-float m))
  (let ((c2 (vec3-dot centre centre)))
    (dotimes (i 3 second)
      (dotimes (j 3)
        (incf (aref second (+ (* 3 i) j))
              (+ (aref local (+ (* 3 i) j))
                 (* m (- (if (= i j) c2 0d0) (* (aref centre i) (aref centre j))))))))))

(defun shape-tensor (shape m size)
  "The tensor of a part about its own centre, body axes."
  (destructuring-bind (a b c) (mapcar (lambda (x) (float x 1d0)) size)
    (ecase shape
      (:box (mat3 (/ (* m (+ (* b b) (* c c))) 12) 0 0
                  0 (/ (* m (+ (* a a) (* c c))) 12) 0
                  0 0 (/ (* m (+ (* a a) (* b b))) 12)))
      ;; Thin-walled and solid cylinders along x: length a, Ø b.
      (:tube (let ((r (/ b 2)))
               (mat3 (* m r r) 0 0
                     0 (* m (+ (/ (* r r) 2) (/ (* a a) 12))) 0
                     0 0 (* m (+ (/ (* r r) 2) (/ (* a a) 12))))))
      (:cylinder (let ((r (/ b 2)))
                   (mat3 (/ (* m r r) 2) 0 0
                         0 (/ (* m (+ (* 3 r r) (* a a))) 12) 0
                         0 0 (/ (* m (+ (* 3 r r) (* a a))) 12)))))))

(defun rod-tensor (m direction length)
  "m L²/12 (𝟙 − u uᵀ): a thin rod along the unit u, about its centre."
  (let ((k (/ (* m length length) 12))
        (out (make-mat3)))
    (dotimes (i 3 out)
      (dotimes (j 3)
        (setf (aref out (+ (* 3 i) j))
              (* k (- (if (= i j) 1d0 0d0) (* (aref direction i) (aref direction j)))))))))

(defun parts-mass-sums (parts)
  "Mass, first and second moments about R of a parts list like *ship-parts*."
  (let ((mass 0d0) (first (make-vec3)) (second (make-mat3)))
    ;; The mass from the rows, so whole kilograms sum exactly.
    (flet ((add (m centre local)
             (vec3-add-scaled! first first centre m)
             (add-mass! second m centre local)))
      (loop for (nil part-mass centres size shape) in parts
            for m = (/ (float part-mass 1d0) (length centres))
            do (incf mass part-mass)
               (dolist (c centres)
                 (let ((centre (apply #'vec3 c)))
                   (if (eq shape :truss)
                       (let* ((members (truss-members))
                              (scale (/ m (reduce #'+ members :key #'first))))
                         (loop for (weight mc direction length) in members
                               for mm = (* scale weight)
                               do (add mm (vec3-add mc (vec3-sub centre (vec3 7.5 0 0)))
                                       (rod-tensor mm direction length))))
                       (add m centre (shape-tensor shape m size)))))))
    (values mass first second)))

(declaim (type double-float *ship-dry-mass*) (type vec3 *ship-dry-first-moment*)
         (type mat3 *ship-dry-second-moment*))
(defparameter *ship-dry-mass* 0d0 "kg, from *ship-parts*.")
(defparameter *ship-dry-first-moment* (make-vec3))
(defparameter *ship-dry-second-moment* (make-mat3))

(defun update-ship-dry-sums ()
  "Recompute the dry sums from *ship-parts* (after editing it live)."
  (multiple-value-bind (m first second) (parts-mass-sums *ship-parts*)
    (setf *ship-dry-mass* m *ship-dry-first-moment* first *ship-dry-second-moment* second)))

(update-ship-dry-sums)

;;; Propellant (docs/design.md "Inertia"): solid cylinders of r 1.52 m
;;; filling each tank from its aft end, MR 5.5.

(defparameter *propellant-full* 28000d0 "kg, docs/design.md \"Parts\".")
(defparameter *mixture-ratio* 5.5d0 "LOX : LH2, docs/design.md \"Propulsion\".")
(defparameter *propellant-radius* 1.52d0 "m, 0.95 × the tank radius, \"Inertia\".")
(defparameter *lh2-aft-end* 0.50d0 "m, docs/design.md \"Parts\".")
(defparameter *lh2-length* 8.86d0 "m, docs/design.md \"Inertia\".")
(defparameter *lox-aft-end* 9.76d0 "m, docs/design.md \"Parts\".")
(defparameter *lox-length* 3.72d0 "m, docs/design.md \"Inertia\".")
(declaim (type double-float *propellant-full* *mixture-ratio* *propellant-radius*
               *lh2-aft-end* *lh2-length* *lox-aft-end* *lox-length*))

;;; Mass properties

;; Inline, so the step path can stack-allocate one (controls.lisp).
(declaim (inline make-mass-properties))
(defstruct (mass-properties (:copier nil))
  (mass 0d0 :type double-float)                ; kg
  (com (make-vec3) :type vec3)                 ; m, body, from R
  (inertia (make-mat3) :type mat3)             ; kg m², about the CoM
  (inertia-inverse (make-mat3) :type mat3))

(declaim (inline mass-properties-from-sums!))

(defun mass-properties-from-sums! (out mass first second)
  "Fill OUT from sums about R: CoM = first / mass, the tensor shifted to the
CoM (I = J − m (|C|² 𝟙 − C Cᵀ)) and its inverse."
  (declare (type mass-properties out) (type double-float mass) (type vec3 first)
           (type mat3 second))
  (let ((com (mass-properties-com out))
        (inertia (mass-properties-inertia out)))
    (setf (mass-properties-mass out) mass)
    (vec3-scale! com first (/ mass))
    (let ((c2 (vec3-dot com com)))
      (dotimes (i 3)
        (dotimes (j 3)
          (setf (aref inertia (+ (* 3 i) j))
                (- (aref second (+ (* 3 i) j))
                   (* mass (- (if (= i j) c2 0d0) (* (aref com i) (aref com j)))))))))
    (mat3-inverse-symmetric! (mass-properties-inertia-inverse out) inertia)
    out))

;;; A grappled load (decision 112: Grapple, stack and berth in the sim:
;;; state, notices to the clock, the latest checkpoint): its definition and
;;; its pose in ship axes, fixed at the capture.

(defstruct (stack-load (:constructor make-stack-load (def com q)) (:copier nil))
  (def nil :type object-def)
  (com (make-vec3) :type vec3)                 ; m, its CoM in body axes from R
  (q (quat 1 0 0 0) :type quat))               ; its body axes -> the ship's

(declaim (inline rotated-tensor!))

(defun rotated-tensor! (out q tensor)
  "R I Rᵀ into OUT, R the rotation of the unit Q."
  (declare (type mat3 out tensor) (type quat q))
  (let ((r (make-mat3)))
    (declare (dynamic-extent r))
    (with-quat (((w x y z) q))
      (setf (aref r 0) (- 1 (* 2 (+ (* y y) (* z z)))) (aref r 1) (* 2 (- (* x y) (* w z)))
            (aref r 2) (* 2 (+ (* x z) (* w y))) (aref r 3) (* 2 (+ (* x y) (* w z)))
            (aref r 4) (- 1 (* 2 (+ (* x x) (* z z)))) (aref r 5) (* 2 (- (* y z) (* w x)))
            (aref r 6) (* 2 (- (* x z) (* w y))) (aref r 7) (* 2 (+ (* y z) (* w x)))
            (aref r 8) (- 1 (* 2 (+ (* x x) (* y y))))))
    (dotimes (i 3 out)
      (dotimes (j 3)
        (setf (aref out (+ (* 3 i) j))
              (let ((sum 0d0))
                (declare (type double-float sum))
                (dotimes (k 3 sum)
                  (dotimes (l 3)
                    (incf sum (* (aref r (+ (* 3 i) k)) (aref tensor (+ (* 3 k) l))
                                 (aref r (+ (* 3 j) l))))))))))))

(declaim (sb-ext:maybe-inline ship-mass-properties))

(defun ship-mass-properties (m-p &optional load (out (make-mass-properties)))
  "The Mule's mass, CoM, tensor about the CoM and its inverse with m-p kg of
propellant and the stack-load LOAD (or nil), written into OUT. Conses
nothing inline."
  (declare (optimize speed) (type double-float m-p) (type (or null stack-load) load)
           (type mass-properties out))
  (let ((first (make-vec3)) (second (make-mat3))
        (f (/ m-p *propellant-full*))
        (r2 (* *propellant-radius* *propellant-radius*)))
    (declare (dynamic-extent first second))
    (replace first *ship-dry-first-moment*)
    (replace second *ship-dry-second-moment*)
    (flet ((tank (m aft-end full-length)
             (declare (type double-float m aft-end full-length))
             ;; A solid cylinder along x: about R, J_xx += m r²/2,
             ;; J_yy = J_zz += m (3r² + L²)/12 + m x².
             (let* ((l (* f full-length))
                    (x (+ aft-end (* 0.5d0 l)))
                    (lateral (+ (/ (* m (+ (* 3 r2) (* l l))) 12) (* m x x))))
               (incf (aref first 0) (* m x))
               (incf (aref second 0) (* 0.5d0 m r2))
               (incf (aref second 4) lateral)
               (incf (aref second 8) lateral))))
      (let ((lh2 (/ m-p (+ 1 *mixture-ratio*))))
        (tank lh2 *lh2-aft-end* *lh2-length*)
        (tank (- m-p lh2) *lox-aft-end* *lox-length*)))
    (if load
        (let* ((def (stack-load-def load))
               (m (object-def-mass def))
               (p (stack-load-com load))
               (turned (make-mat3)))
          (declare (dynamic-extent turned))
          (rotated-tensor! turned (stack-load-q load) (object-def-tensor def))
          (vec3-add-scaled! first first p m)
          (add-mass! second m p turned)
          (mass-properties-from-sums! out (+ *ship-dry-mass* m-p m) first second))
        (mass-properties-from-sums! out (+ *ship-dry-mass* m-p) first second))))

;;; The ship's collision shapes and the stack (docs/design.md "Grappling and
;;; berthing", "Inertia").

(defparameter *ship-collision-boxes*
  (vector (make-shape :box (vec3 6.5d0 0 0) :size (vec3 17.0d0 3.6d0 3.6d0))
          (make-shape :box (vec3 12.75d0 0 -3.5d0) :size (vec3 4.5d0 3.0d0 3.0d0))
          (make-shape :box (vec3 15.8d0 0 0) :size (vec3 1.6d0 1.6d0 1.6d0)))
  "Truss with tanks and engine, cab, grapple cone; centres in body axes from
R, unlike an object's shapes about its CoM.")

(defparameter *grapple-tip* (vec3 16.6d0 0 0) "m, body axes from R: the cone's tip.")
(declaim (type simple-vector *ship-collision-boxes*) (type vec3 *grapple-tip*))

(defun stack-design-pose (def &optional (fixture (first (object-def-fixtures def))))
  "DEF's stack-load at docs/design.md \"Inertia\"'s pose: FIXTURE's pin at
the grapple's tip, its outward axis along −x (a rock's centre at 16.9 + r)."
  (let* ((axis (fixture-axis fixture))
         (inward (vec3 -1 0 0))
         (c (vec3-dot axis inward))
         (q (cond ((> c (- 1d0 1d-12)) (quat 1 0 0 0))
                  ;; Opposite: half a turn about z.
                  ((< c (- 1d-12 1d0)) (quat 0 0 0 1))
                  (t (quat-from-axis-angle (vec3-cross axis inward) (acos c))))))
    (make-stack-load def (vec3-sub *grapple-tip* (quat-rotate q (fixture-pin fixture))) q)))

(declaim (sb-ext:maybe-inline stack-bound))

(defun stack-bound (com &optional load)
  "m: the largest distance from COM (body axes from R) to a corner of the
ship's boxes or to LOAD's bounding sphere."
  (declare (optimize speed) (type vec3 com) (type (or null stack-load) load))
  (let ((bound 0d0))
    (declare (type double-float bound))
    (loop for shape across *ship-collision-boxes*
          for c of-type vec3 = (shape-centre shape)
          for size of-type vec3 = (shape-size shape)
          do (dotimes (k 8)
               (flet ((corner (axis)
                        (- (+ (aref c axis)
                              (* (if (logbitp axis k) 0.5d0 -0.5d0) (aref size axis)))
                           (aref com axis))))
                 (let ((dx (corner 0)) (dy (corner 1)) (dz (corner 2)))
                   (setf bound (max bound (+ (* dx dx) (* dy dy) (* dz dz))))))))
    (setf bound (sqrt bound))
    (if load
        (let* ((p (stack-load-com load))
               (dx (- (aref p 0) (aref com 0)))
               (dy (- (aref p 1) (aref com 1)))
               (dz (- (aref p 2) (aref com 2))))
          (max bound (+ (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
                        (object-def-bound (stack-load-def load)))))
        bound)))

;;; The range finder (docs/design.md "Grappling and berthing"; decision 113:
;;; Grapple and contact details the design leaves open).

(defparameter *grapple-range-max* 500d0 "m: the range finder's reach.")
(defparameter *capture-closing-min* 0.01d0 "m/s, decision 33 and decision 113.")
(defparameter *capture-closing-max* 0.30d0 "m/s, decision 33.")
(defparameter *capture-offset* 0.30d0 "m, the pin's lateral offset; decision 33.")
(defparameter *capture-angle* (deg->rad 10d0) "rad, decision 33.")
(defparameter *capture-rotation* (deg->rad 0.5d0) "rad/s, decision 33.")
(declaim (type double-float *grapple-range-max* *capture-closing-min* *capture-closing-max*
               *capture-offset* *capture-angle* *capture-rotation*))

(defstruct (grapple-reading (:copier nil))
  (object 0 :type fixnum)                      ; index in sim-objects
  (fixture 0 :type fixnum)                     ; index in its fixtures
  (range 0d0 :type double-float)               ; m, tip to the plate's plane along the axis
  (closing-speed 0d0 :type double-float)       ; m/s, the range's rate of fall
  (offset-y 0d0 :type double-float)            ; m, the pin from the tip, body axes
  (offset-z 0d0 :type double-float)
  (angle 0d0 :type double-float)               ; rad, +x to the inward axis
  (rotation 0d0 :type double-float)            ; rad/s, |ω_ship − ω_object|
  (closing-ok nil :type boolean)               ; each value inside its capture tolerance
  (offset-ok nil :type boolean)                ; √(y² + z²)
  (angle-ok nil :type boolean)
  (rotation-ok nil :type boolean))

(defun grapple-range-into! (out sim)
  "OUT, a grapple-reading, for the nearest free fixture of a promoted object
whose pin lies ahead of the tip (body x) within *grapple-range-max* and
whose axis faces it; nil when there is none. Pure; conses nothing."
  (declare (optimize speed) (type grapple-reading out)
           (inline ship-mass-properties make-mass-properties))
  (let* ((ship (sim-ship sim))
         (q-s (ship-state-q ship))
         (com (make-vec3)) (inertia (make-mat3)) (inverse (make-mat3))
         (properties (make-mass-properties :com com :inertia inertia :inertia-inverse inverse))
         (q-s* (make-quat)) (tip-b (make-vec3)) (tip (make-vec3)) (w-s (make-vec3))
         (w-o (make-vec3))
         (pin (make-vec3)) (axis (make-vec3)) (d (make-vec3)) (d-b (make-vec3))
         (u (make-vec3)) (u-v (make-vec3))
         (best-object -1) (best-fixture 0) (best 0d0))
    (declare (dynamic-extent com inertia inverse properties q-s* tip-b tip w-s w-o
                             pin axis d d-b u u-v)
             (type fixnum best-object best-fixture) (type double-float best))
    ;; PLACEHOLDER(188): the latched load's mass and exclusion, once the sim
    ;; holds the grapple; until then every promoted object is free.
    (ship-mass-properties (ship-state-m-p ship) nil properties)
    (quat-conjugate! q-s* q-s)
    ;; The tip in inertial axes: r + q (tip − com).
    (vec3-sub! tip-b *grapple-tip* com)
    (quat-rotate! tip q-s tip-b)
    (vec3-add! tip tip (ship-state-r ship))
    (flet ((fixture-geometry (state fixture)
             ;; PIN, AXIS (inertial) and D = pin − tip, D-B in ship axes.
             (quat-rotate! pin (ship-state-q state) (fixture-pin fixture))
             (vec3-add! pin pin (ship-state-r state))
             (quat-rotate! axis (ship-state-q state) (fixture-axis fixture))
             (vec3-sub! d pin tip)
             (quat-rotate! d-b q-s* d)))
      ;; A promoted object's state is the sim's own (object-state-into!).
      (loop for object across (the simple-vector (sim-objects sim))
            for i of-type fixnum from 0
            when (object-promoted-p object)
              do (loop for fixture in (object-def-fixtures (object-def-of object))
                       for j of-type fixnum from 0
                       when (fixture-free fixture)
                         do (fixture-geometry (object-state object) fixture)
                            (let ((distance (vec3-norm d)))
                              (when (and (> (aref d-b 0) 0d0)
                                         (<= distance *grapple-range-max*)
                                         (< (vec3-dot axis d) 0d0)
                                         (or (< best-object 0) (< distance best)))
                                (setf best-object i best-fixture j best distance)))))
      (when (>= best-object 0)
        (let* ((object (aref (the simple-vector (sim-objects sim)) best-object))
               (fixture (nth best-fixture (object-def-fixtures (object-def-of object))))
               (state (object-state object)))
          (fixture-geometry state fixture)
          ;; Rates: ω in inertial axes; the pin's and the tip's velocities.
          (quat-rotate! w-o (ship-state-q state) (ship-state-w state))
          (quat-rotate! w-s q-s (ship-state-w ship))
          (vec3-sub! u pin (ship-state-r state))
          (vec3-cross! u w-o u)
          (vec3-add! u u (ship-state-v state))     ; the pin's velocity
          (quat-rotate! u-v q-s tip-b)
          (vec3-cross! u-v w-s u-v)
          (vec3-add! u-v u-v (ship-state-v ship))  ; the tip's
          (vec3-sub! u u u-v)
          (let ((closing (+ (vec3-dot axis u)
                            (vec3-dot (vec3-cross! u-v w-o axis) d))))
            (vec3-sub! w-s w-s w-o)
            (quat-rotate! u q-s* axis)
            (let ((y (aref d-b 1)) (z (aref d-b 2))
                  (angle (acos (max -1d0 (min 1d0 (- (aref u 0))))))
                  (rotation (vec3-norm w-s)))
              (setf (grapple-reading-object out) best-object
                    (grapple-reading-fixture out) best-fixture
                    (grapple-reading-range out) (- (vec3-dot axis d))
                    (grapple-reading-closing-speed out) closing
                    (grapple-reading-offset-y out) y
                    (grapple-reading-offset-z out) z
                    (grapple-reading-angle out) angle
                    (grapple-reading-rotation out) rotation
                    (grapple-reading-closing-ok out) (<= *capture-closing-min* closing
                                                         *capture-closing-max*)
                    (grapple-reading-offset-ok out) (<= (sqrt (+ (* y y) (* z z)))
                                                        *capture-offset*)
                    (grapple-reading-angle-ok out) (<= angle *capture-angle*)
                    (grapple-reading-rotation-ok out) (<= rotation *capture-rotation*))
              out)))))))

;;; Main engine (docs/design.md "Propulsion"): thrust along +x through the
;;; CoM, the gimbal trimmed, no torque.

(defparameter *engine-thrust* 101800d0 "N, docs/design.md \"Propulsion\".")
(defparameter *engine-isp* 449.7d0 "s, docs/design.md \"Propulsion\".")
(defparameter *throttle-min* 0.1d0 "Lowest setting above off, docs/design.md \"Propulsion\".")
(defparameter *engine-gimbal* (vec3 0 0 -0.5) "m from R, docs/design.md \"Parts\".")
(declaim (type double-float *engine-thrust* *engine-isp* *throttle-min*)
         (type vec3 *engine-gimbal*))

(declaim (inline engine-exhaust-velocity engine-throttle ship-engine-force ship-engine-mdot))

(defun engine-exhaust-velocity () (* *engine-isp* +g0+))

(defun engine-throttle (throttle)
  "The setting the engine runs at: 0 below *throttle-min*, at most 1."
  (declare (type double-float throttle))
  (if (< throttle *throttle-min*) 0d0 (min throttle 1d0)))

(defun ship-engine-force (throttle)
  "N along body +x."
  (declare (type double-float throttle))
  (* (engine-throttle throttle) *engine-thrust*))

(defun ship-engine-mdot (throttle)
  "kg/s: F / (Isp g₀)."
  (declare (type double-float throttle))
  (/ (ship-engine-force throttle) (engine-exhaust-velocity)))

(defun ship-trim-angle (m-p)
  "Radians about body y: atan((z_cm − z_g)/(x_cm − x_g)), the gimbal through the CoM."
  (let ((com (mass-properties-com (ship-mass-properties (float m-p 1d0)))))
    (atan (- (aref com 2) (aref *engine-gimbal* 2))
          (- (aref com 0) (aref *engine-gimbal* 0)))))

(defun ship-dv-remaining (m-p)
  "v_e ln(m / m_dry): the engine's Δv left (docs/design.md \"Mass budget and Δv\")."
  (* (engine-exhaust-velocity) (log (/ (+ *ship-dry-mass* m-p) *ship-dry-mass*))))

;;; RCS (docs/design.md "RCS forces, torques and accelerations"): eight
;;; blocks at the truss corners, each with an axial nozzle (aft blocks push
;;; +x, front −x) and a y and a z nozzle pushing towards the axis.

(defparameter *rcs-thrust* 445d0 "N per thruster, docs/design.md \"Propulsion\".")
(defparameter *rcs-isp* 360d0 "s, docs/design.md \"Propulsion\".")
(defparameter *rcs-block-aft-x* 0.5d0 "m, docs/design.md \"RCS forces, torques and accelerations\".")
(defparameter *rcs-block-front-x* 14.5d0 "m, as *rcs-block-aft-x*.")
(defparameter *rcs-block-offset* 1.8d0 "m, |y| and |z| of every block, as *rcs-block-aft-x*.")
(declaim (type double-float *rcs-thrust* *rcs-isp* *rcs-block-aft-x* *rcs-block-front-x*
               *rcs-block-offset*))

(defconstant +rcs-count+ 24)

(deftype rcs-layout () '(simple-array double-float (24 3)))
(deftype rcs-matrix () '(simple-array double-float (6 24)))

(defun rcs-layout ()
  "Positions (m from R) and force directions of the 24 thrusters, a fixed
order: aft blocks then front, y then z of each block +/−, nozzles x, y, z."
  (let ((positions (make-array '(24 3) :element-type 'double-float))
        (directions (make-array '(24 3) :element-type 'double-float :initial-element 0d0))
        (i 0))
    (dolist (x (list *rcs-block-aft-x* *rcs-block-front-x*))
      (dolist (y (list *rcs-block-offset* (- *rcs-block-offset*)))
        (dolist (z (list *rcs-block-offset* (- *rcs-block-offset*)))
          (dotimes (axis 3)
            (setf (aref positions i 0) x (aref positions i 1) y (aref positions i 2) z
                  (aref directions i axis)
                  (ecase axis
                    (0 (if (= x *rcs-block-aft-x*) 1d0 -1d0))
                    (1 (- (signum y)))
                    (2 (- (signum z)))))
            (incf i)))))
    (values positions directions)))

(declaim (type rcs-layout *rcs-positions* *rcs-directions*))
(defparameter *rcs-positions* (nth-value 0 (rcs-layout)))
(defparameter *rcs-directions* (nth-value 1 (rcs-layout)))

(defun rcs-matrix! (b com)
  "Write into the 6×24 B each thruster's full force (rows 0–2, N) and its
torque about COM (rows 3–5, N m), body axes."
  (declare (optimize speed) (type rcs-matrix b) (type vec3 com))
  (let ((p *rcs-positions*) (d *rcs-directions*) (f *rcs-thrust*))
    (dotimes (i +rcs-count+ b)
      (let ((px (- (aref p i 0) (aref com 0)))
            (py (- (aref p i 1) (aref com 1)))
            (pz (- (aref p i 2) (aref com 2)))
            (fx (* f (aref d i 0))) (fy (* f (aref d i 1))) (fz (* f (aref d i 2))))
        (setf (aref b 0 i) fx (aref b 1 i) fy (aref b 2 i) fz
              (aref b 3 i) (- (* py fz) (* pz fy))
              (aref b 4 i) (- (* pz fx) (* px fz))
              (aref b 5 i) (- (* px fy) (* py fx)))))))

(defun make-rcs-matrix ()
  (make-array '(6 24) :element-type 'double-float :initial-element 0d0))

(declaim (inline rcs-couple rcs-translation-capability))

(defun rcs-couple (axis)
  "N m about body AXIS (0 roll, 1 pitch, 2 yaw), the flight computer's clamp
(decision 70): roll 4 F d, pitch and yaw 2 F (x_front − x_aft)."
  (declare (type (integer 0 2) axis))
  (if (= axis 0)
      (* 4 *rcs-thrust* *rcs-block-offset*)
      (* 2 *rcs-thrust* (- *rcs-block-front-x* *rcs-block-aft-x*))))

(defun rcs-translation-capability (axis com)
  "N along body AXIS without torque (decision 70): ±x the four axial
nozzles; ±y, ±z 2F (1 + min(a_f, a_r)/max(a_f, a_r)), a_f and a_r the front
and aft blocks' arms from the CoM between them (milestone 3 adds a CoM ahead
of the front blocks)."
  (declare (type (integer 0 2) axis) (type vec3 com))
  (if (= axis 0)
      (* 4 *rcs-thrust*)
      (let ((a-f (- *rcs-block-front-x* (aref com 0)))
            (a-r (- (aref com 0) *rcs-block-aft-x*)))
        (* 2 *rcs-thrust* (+ 1 (/ (min a-f a-r) (max a-f a-r)))))))
