(in-package #:luv-in-space)

;;; The render state interpolated from the clock, and the frame's passes
;;; (docs/architecture.md "The clock", "Rendering precision"; decision 77:
;;; The frame in main.lisp: render state, capture of a given sim, part 4's
;;; placeholders and first asset). Reads the simulation, never changes it.

;;; The render state

(defstruct (object-render (:constructor make-object-render ()) (:copier nil))
  "One object as a frame draws it (plan 29, decision 124: Meshes of the
places: models, scenery liveries, lights, far dots, the grapple's claws,
floodlight and range finder): R, V, Q, W as in ship-state (Q body ->
inertial, W in body axes), DRAWN nil for a removed object, SUN-FRACTION at
its position."
  (r (make-vec3) :type vec3)
  (v (make-vec3) :type vec3)
  (q (quat 1 0 0 0) :type quat)
  (w (make-vec3) :type vec3)
  (drawn nil)
  (sun-fraction 1d0 :type double-float)
  (def nil :type (or null object-def)))   ; its definition (model, bound)

(defstruct (render-grapple (:constructor make-render-grapple ()) (:copier nil))
  "The grapple as a frame draws it: the sim's STATE and attached OBJECT
index, the claws' TARGET angle (deg) at the render time and the ANGLE shown,
FLOODLIGHT and RANGE-FINDER on."
  (state :stowed :type (member :stowed :armed :closing :latched))
  (object nil :type (or null fixnum))
  (target 0d0 :type double-float)
  (angle 0d0 :type double-float)
  (floodlight nil)
  (range-finder nil))

(defstruct (render-state (:constructor make-render-state ()) (:copier nil))
  "What a frame draws, at the render time T: the ship (R, V, Q, W as in
ship-state), M-P, CONTROLS and LOSS from the sim, the Sun and the Moon
relative to Earth, Earth's body-fixed matrix and the ship's SUN-FRACTION
(sun-visible-fraction) at T; the hull's SHIP-EMISSIVE colour,
SHIP-LIGHTS-ON (decision 80) and LIGHTS-T, the real seconds the clock has
run, the strobe's clock (decision 100: The lights' clock and the glows'
numbers), which render-state-update! leaves alone; the TARGET's key (nil
without one), its TARGET-R and TARGET-V at T and TARGET-RELATIVE
(target-relative-into!; decision 114: Targets and target holds in the sim;
a rails target's closing speed from its position's rate, decision 148: The
closing speed of a rails target: the position's rate)."
  (t 0d0 :type double-float)
  (r (make-vec3) :type vec3)
  (v (make-vec3) :type vec3)
  (q (quat 1 0 0 0) :type quat)
  (w (make-vec3) :type vec3)
  (m-p 0d0 :type double-float)
  (controls (new-controls) :type controls)
  (autopilot nil :type boolean)         ; a plan engaged (sim-plan)
  (loss nil)
  (sun (make-vec3) :type vec3)
  (moon (make-vec3) :type vec3)
  (earth-matrix (make-mat3) :type mat3)
  (sun-fraction 1d0 :type double-float)
  (ship-emissive (make-vec3) :type vec3)
  (ship-lights-on t)
  (lights-t 0d0 :type double-float)
  (target nil :type (or null keyword))
  (target-r (make-vec3) :type vec3)
  (target-v (make-vec3) :type vec3)
  (target-relative (make-target-relative) :type target-relative)
  (objects (coerce (loop repeat (length *objects*) collect (make-object-render))
                   'simple-vector)
   :type simple-vector)                 ; an object-render per sim-objects entry
  (grapple (make-render-grapple) :type render-grapple)
  (com (make-vec3) :type vec3)          ; the stack's CoM in body axes from R (with the latched load)
  (own (make-mass-properties) :type mass-properties) ; scratch: the Mule's own
  (com-valid nil)                       ; true once render-state-update! set COM
  (load-reach 0d0 :type double-float)   ; m: the latched load's CoM distance from COM + its bound, 0 none
  (stack-shift 0d0 :type double-float)  ; m: COM from the Mule's own CoM
  (claws-eased nil)                     ; true: the shown angle eases (render-state-ease-claws!)
  (state-a (make-ship-state (make-vec3) (make-vec3) (quat 1 0 0 0) (make-vec3) 0d0)
   :type ship-state)                    ; scratch: an object's previous and current state
  (state-b (make-ship-state (make-vec3) (make-vec3) (quat 1 0 0 0) (make-vec3) 0d0)
   :type ship-state)
  (scratch (make-vec3) :type vec3))     ; the bodies' velocities, unused

(defun render-state-assign-ship! (out ship)
  (replace (render-state-r out) (ship-state-r ship))
  (replace (render-state-v out) (ship-state-v ship))
  (replace (render-state-q out) (ship-state-q ship))
  (replace (render-state-w out) (ship-state-w ship)))

(declaim (inline hermite-states!))
(defun hermite-states! (r v q w previous current h s)
  "Cubic Hermite position and velocity over a step of H s at S, slerp of
the attitude, ω blended linearly, from the ship-states PREVIOUS and CURRENT
into the arrays R, V, Q, W."
  (declare (type vec3 r v w) (type quat q) (type ship-state previous current)
           (type double-float h s))
  (let* ((s2 (* s s)) (s3 (* s2 s))
         (h00 (+ (* 2 s3) (* -3 s2) 1)) (h10 (+ s3 (* -2 s2) s))
         (h01 (+ (* -2 s3) (* 3 s2))) (h11 (- s3 s2))
         (d00 (/ (* 6 (- s2 s)) h)) (d10 (+ (* 3 s2) (* -4 s) 1)) (d11 (- (* 3 s2) (* 2 s)))
         (p0 (ship-state-r previous)) (v0 (ship-state-v previous))
         (p1 (ship-state-r current)) (v1 (ship-state-v current))
         (w0 (ship-state-w previous)) (w1 (ship-state-w current)))
    (dotimes (i 3)
      (setf (aref r i)
            (+ (* h00 (aref p0 i)) (* h h10 (aref v0 i))
               (* h01 (aref p1 i)) (* h h11 (aref v1 i)))
            (aref v i)
            (+ (* d00 (- (aref p0 i) (aref p1 i)))
               (* d10 (aref v0 i)) (* d11 (aref v1 i)))
            (aref w i)
            (+ (* (- 1 s) (aref w0 i)) (* s (aref w1 i)))))
    (quat-slerp! q (ship-state-q previous) (ship-state-q current) s)))

(declaim (inline render-state-hermite!))
(defun render-state-hermite! (out previous current h s)
  "hermite-states! into OUT's ship."
  (declare (type render-state out) (type ship-state previous current)
           (type double-float h s))
  (hermite-states! (render-state-r out) (render-state-v out) (render-state-q out)
                   (render-state-w out) previous current h s))

;;; Objects and the grapple (decision 124)

(defun object-render-assign! (out state)
  (replace (object-render-r out) (ship-state-r state))
  (replace (object-render-v out) (ship-state-v state))
  (replace (object-render-q out) (ship-state-q state))
  (replace (object-render-w out) (ship-state-w state)))

(declaim (inline object-state-for-render!))
(defun object-state-for-render! (state object time)
  "OBJECT's state at TIME: the stored one of a promoted or latched object
(valid at the sim's t), else its rails' into the scratch STATE. The
ship-state, or nil past its rails' end."
  (declare (type ship-state state) (type object object) (type double-float time)
           (inline object-state-into! rails-state-into! object-attitude-into!))
  (if (member (object-status object) '(:promoted :latched))
      (object-state object)
      (and (object-state-into! (ship-state-r state) (ship-state-v state) (ship-state-q state)
                               (ship-state-w state) object time)
           state)))

(defun attached-object-render! (o out grapple properties load)
  "O, the latched load's or the berthed station's pose from OUT's ship
render pose and the grapple's relative pose (decision 112; decision 138:
Berthed pose: the ship's R point in the station's axes, the engine off)."
  (declare (type object-render o) (type render-state out) (type grapple grapple)
           (type mass-properties properties))
  (let* ((r (render-state-r out)) (v (render-state-v out))
         (q (render-state-q out)) (w (render-state-w out))
         (x (make-vec3)) (win (make-vec3)) (y (make-vec3)) (conjugate (make-quat)))
    (declare (dynamic-extent x win y conjugate))
    (quat-rotate! win q w)
    (cond (load
           (vec3-sub! x (stack-load-com load) (mass-properties-com properties))
           (quat-rotate! x q x)
           (vec3-add! (object-render-r o) r x)
           (quat-mul! (object-render-q o) q (stack-load-q load))
           (vec3-add! (object-render-v o) v (vec3-cross! y win x)))
          (t
           ;; r_s = r − q_s com_g − q com_ship, q_s = q ⊗ q_rel⁻¹
           (quat-mul! (object-render-q o) q (quat-conjugate! conjugate (grapple-q grapple)))
           (quat-rotate! x (object-render-q o) (grapple-com grapple))
           (vec3-sub! y r x)
           (quat-rotate! x q (mass-properties-com properties))
           (vec3-sub! (object-render-r o) y x)
           (vec3-sub! x r (object-render-r o))
           (vec3-sub! (object-render-v o) v (vec3-cross! y win x))))
    (quat-rotate! (object-render-w o) (quat-conjugate! conjugate (object-render-q o)) win)
    o))

(declaim (inline render-objects-update! render-grapple-update!))
(defun render-objects-update! (out sim previous-objects h s t-prev tr)
  "OUT's objects at the render time TR (decision 124): rails at both ends
of the step exactly at TR; promoted, or promoted or demoted in the step, the
Hermite and slerp of the previous and current states (S of H from T-PREV);
the latched load and the berthed station from the ship's render pose; a
removed object not drawn. Conses nothing."
  (declare (optimize speed) (type render-state out) (type sim sim) (type simple-vector previous-objects)
           (type double-float h s t-prev tr)
           (inline object-state-into! rails-state-into! object-attitude-into!))
  (let* ((objects (sim-objects sim))
         (grapple (sim-grapple sim))
         (attached (grapple-object grapple))
         (berthed (sim-berthed sim))
         (time (sim-t sim))
         (renders (render-state-objects out))
         (a (render-state-state-a out))
         (b (render-state-state-b out)))
    (declare (type (or null fixnum) attached))
    (with-stack-properties (properties sim load)
      (let ((com (mass-properties-com properties))
            (own (make-vec3)))
        (declare (dynamic-extent own))
        (replace (render-state-com out) com)
        (setf (render-state-com-valid out) t)
        (vec3-set! own 0d0 0d0 0d0)
        (setf (render-state-stack-shift out)
              (if load
                  (let ((plain (render-state-own out)))
                    (locally (declare (inline ship-mass-properties))
                      (ship-mass-properties (ship-state-m-p (sim-ship sim)) nil plain))
                    (vec3-sub! own com (mass-properties-com plain))
                    (vec3-norm own))
                  0d0)
              (render-state-load-reach out)
              (if load
                  (progn (vec3-sub! own (stack-load-com load) com)
                         (+ (vec3-norm own) (object-def-bound (stack-load-def load))))
                  0d0)))
      (dotimes (i (length objects))
        (let* ((object (svref objects i))
               (o (svref renders i))
               (status (object-status object))
               (drawn t))
          (declare (type object object) (type object-render o))
          (cond ((eq status :removed) (setf drawn nil))
                ((and attached (= i attached) (or load berthed))
                 (attached-object-render! o out grapple properties load))
                (t
                 (let ((old (svref previous-objects i)))
                   (declare (type object old))
                   (if (and (eq status :rails) (eq (object-status old) :rails))
                       (setf drawn (object-state-into! (object-render-r o) (object-render-v o)
                                                       (object-render-q o) (object-render-w o)
                                                       object tr))
                       (let* ((cur (object-state-for-render! b object time))
                              (prev (or (and (not (object-removed-p old))
                                             (object-state-for-render! a old t-prev))
                                        cur)))
                         (cond ((not cur) (setf drawn nil))
                               ((= s 1d0) (object-render-assign! o cur))
                               ((= s 0d0) (object-render-assign! o prev))
                               (t (hermite-states! (object-render-r o) (object-render-v o)
                                                   (object-render-q o) (object-render-w o)
                                                   prev cur h s))))))))
          (setf (object-render-drawn o) (and drawn t)
                (object-render-def o) (object-def-of object))
          (when drawn
            (setf (object-render-sun-fraction o)
                  (sun-visible-fraction (object-render-r o) tr))))))
    out))

(defun render-grapple-update! (out sim tr eased)
  "OUT's grapple from the sim's at the render time TR: the claws' target
angle (30° armed, −8° latched, closing linearly between over the closing
time to its end), the lights; the shown angle follows closing and latched
exactly, stowed and armed unless EASED (render-state-ease-claws!)."
  (declare (optimize speed) (type render-grapple out) (type sim sim) (type double-float tr))
  (let* ((grapple (sim-grapple sim))
         (state (grapple-state grapple))
         (target (ecase state
                   (:stowed 0d0)
                   (:armed 30d0)
                   (:latched -8d0)
                   (:closing
                    (let ((f (max 0d0 (min 1d0 (/ (- tr (- (grapple-closing-end grapple)
                                                            *claws-closing-time*))
                                                   *claws-closing-time*)))))
                      (- 30d0 (* 38d0 f)))))))
    (declare (type double-float target))
    (setf (render-grapple-state out) state
          (render-grapple-object out) (grapple-object grapple)
          (render-grapple-target out) target
          (render-grapple-floodlight out) (and (not (eq state :stowed)) t)
          (render-grapple-range-finder out) (and (member state '(:armed :closing)) t))
    (when (or (not eased) (member state '(:closing :latched)))
      (setf (render-grapple-angle out) target))
    out))

(defparameter *claws-ease-rate* 90d0 "deg per real second: the claws between stowed and armed.")

(defun render-state-ease-claws! (state elapsed)
  "The shown claw angle toward its target at *claws-ease-rate* over ELAPSED
real seconds; closing and latched follow their target exactly, and without
ELAPSED (a capture) or an unset claws-eased the angle snaps. STATE."
  (declare (type render-state state) (type (or null double-float) elapsed))
  (let* ((grapple (render-state-grapple state))
         (target (render-grapple-target grapple))
         (angle (render-grapple-angle grapple)))
    (setf (render-grapple-angle grapple)
          (if (and elapsed (render-state-claws-eased state)
                   (member (render-grapple-state grapple) '(:stowed :armed)))
              (let ((step (* *claws-ease-rate* elapsed)))
                (max (- angle step) (min (+ angle step) target)))
              target))
    state))

(defun render-state-update! (out clock sim)
  "Fill OUT for the frame (decision 77): render time t_prev + s (t − t_prev),
s = clamp(acc / (t − t_prev), 0, 1); the sim's state as it is when t_prev is
not below t or the sim is lost; the target at the render time
(target-state-into!) and its relative values. Conses nothing; OUT."
  (declare (optimize speed) (type render-state out) (type clock clock) (type sim sim)
           (inline target-state-into!))
  (let ((current (sim-ship sim))
        (time (sim-t sim)))
    (let ((previous (clock-previous clock))
          (t-prev (clock-previous-t clock)))
      (let* ((h (- time t-prev))
             (s (if (or (sim-loss sim) (<= h 0d0))
                    1d0
                    (max 0d0 (min 1d0 (/ (clock-accumulator clock) h))))))
        (declare (type double-float h s))
        (cond ((= s 1d0)
               (render-state-assign-ship! out current)
               (setf (render-state-t out) time))
              ((= s 0d0)
               (render-state-assign-ship! out previous)
               (setf (render-state-t out) t-prev))
              (t
               (render-state-hermite! out previous current h s)
               (setf (render-state-t out) (+ t-prev (* s h)))))
        (unless (= (length (render-state-objects out)) (length (sim-objects sim)))
          (setf (render-state-objects out)
                (coerce (loop repeat (length (sim-objects sim)) collect (make-object-render))
                        'simple-vector)))
        (render-objects-update! out sim (clock-previous-objects clock) h s t-prev
                                (render-state-t out))
        (render-grapple-update! (render-state-grapple out) sim (render-state-t out)
                                (render-state-claws-eased out))))
    (setf (render-state-m-p out) (ship-state-m-p current)
          (render-state-loss out) (sim-loss sim)
          (render-state-autopilot out) (and (sim-plan sim) t))
    (controls-assign! (render-state-controls out) (sim-controls sim))
    (let ((tr (render-state-t out)))
      (body-state-into (render-state-sun out) (render-state-scratch out) :sun tr :earth)
      (body-state-into (render-state-moon out) (render-state-scratch out) :moon tr :earth)
      (earth-fixed-matrix! (render-state-earth-matrix out) tr)
      (setf (render-state-sun-fraction out) (sun-visible-fraction (render-state-r out) tr))
      (let ((target-r (render-state-target-r out)) (target-v (render-state-target-v out)))
        (setf (render-state-target out)
              (and (target-state-into! target-r target-v sim tr) (sim-target sim)))
        (if (render-state-target out)
            (let ((segment (target-rails-segment sim tr))
                  (rate (make-vec3)))
              (declare (dynamic-extent rate))
              (replace rate target-v)
              (when segment
                (rails-rate-into! rate target-r segment))
              (target-relative-into! (render-state-target-relative out) (render-state-r out)
                                     (render-state-v out) (render-state-q out) target-r target-v
                                     rate))
            (fill (render-state-target-relative out) 0d0))))
    out))

;;; Drawing the objects (decision 124): their blocks, glows and draw kinds,
;;; here for the render state's accessors to inline (meshes.lisp holds their
;;; buffers and slots).

(declaim (inline put4!))
(defun put4! (out index x y z w)
  (declare (type (simple-array single-float (*)) out) (type fixnum index)
           (type double-float x y z w))
  (setf (aref out index) (coerce x 'single-float)
        (aref out (+ index 1)) (coerce y 'single-float)
        (aref out (+ index 2)) (coerce z 'single-float)
        (aref out (+ index 3)) (coerce w 'single-float)))

;;; The grapple at work (decision 124: Meshes of the places: models, scenery
;;; liveries, lights, far dots, the grapple's claws, floodlight and range
;;; finder): where the Mule's floodlight and range finder are in the world,
;;; the floodlight in an object's axes and the range finder's spot.

(defun stack-reference-into! (out state)
  "OUT: R's position, r − q c about the stack's CoM c (with the latched load).
Conses nothing once render-state-update! ran."
  (declare (type vec3 out) (type render-state state))
  (let ((com (if (render-state-com-valid state)
                 (render-state-com state)
                 (mass-properties-com (ship-mass-properties (render-state-m-p state) nil
                                                            (render-state-own state)))))
        (qc (make-vec3)))
    (declare (dynamic-extent qc))
    (quat-rotate! qc (render-state-q state) com)
    (vec3-sub! out (render-state-r state) qc)))

(defun ship-point-into! (out state p)
  "OUT: the body point P of the Mule in the world."
  (declare (type vec3 out p) (type render-state state))
  (let ((reference (make-vec3)))
    (declare (dynamic-extent reference))
    (stack-reference-into! reference state)
    (quat-rotate! out (render-state-q state) p)
    (vec3-add! out out reference)))

(defun object-floodlight-rows! (out state o)
  "The ship block's floodlight rows (52 to 59) of OUT for the object O of the
render STATE: the Mule's lamp and beam axis in O's body axes, the lamp's
intensity in w, zero while the floodlight is off. Conses nothing."
  (declare (optimize speed) (type (simple-array single-float (*)) out)
           (type render-state state) (type object-render o))
  (if (render-grapple-floodlight (render-state-grapple state))
      (let ((lamp (make-vec3)) (axis (make-vec3)) (inverse (make-quat)) (local (make-vec3)))
        (declare (dynamic-extent lamp axis inverse local))
        (ship-point-into! lamp state *floodlight-position*)
        (vec3-set! axis 1d0 0d0 0d0)
        (quat-rotate! axis (render-state-q state) axis)
        (quat-conjugate! inverse (object-render-q o))
        (vec3-sub! lamp lamp (object-render-r o))
        (quat-rotate! local inverse lamp)
        (put4! out 52 (aref local 0) (aref local 1) (aref local 2)
               (the double-float *floodlight-intensity*))
        (quat-rotate! local inverse axis)
        (put4! out 56 (aref local 0) (aref local 1) (aref local 2) 0d0))
      (progn (put4! out 52 0d0 0d0 0d0 0d0)
             (put4! out 56 0d0 0d0 0d0 0d0))))

(declaim (inline ray-shape-distance))
(defun ray-shape-distance (origin direction shape)
  "The first distance t ≥ 0 along the ray ORIGIN + t DIRECTION (unit), both in the
body axes of the object holding SHAPE, where it meets SHAPE; nil when it
does not. Conses nothing."
  (declare (optimize speed) (type vec3 origin direction) (type shape shape))
  (let ((o (make-vec3)))
    (declare (dynamic-extent o))
    (vec3-sub! o origin (shape-centre shape))
    (ecase (shape-kind shape)
      (:sphere
       (let* ((b (vec3-dot o direction))
              (radius (shape-radius shape))
              (c (- (vec3-dot o o) (* radius radius)))
              (discriminant (- (* b b) c)))
         (declare (type double-float b radius c discriminant))
         (when (>= discriminant 0d0)
           (let ((near (- (- b) (sqrt discriminant))))
             (declare (type double-float near))
             (cond ((>= near 0d0) near)
                   ((>= c 0d0) nil)
                   (t 0d0))))))
      (:box
       (let ((size (shape-size shape)) (near 0d0) (far 1d300))
         (declare (type vec3 size) (type double-float near far))
         (dotimes (i 3)
           (let ((half (* 0.5d0 (aref size i))) (oi (aref o i)) (di (aref direction i)))
             (declare (type double-float half oi di))
             (if (< (abs di) 1d-12)
                 (when (> (abs oi) half) (return-from ray-shape-distance nil))
                 (let ((t1 (/ (- (- half) oi) di)) (t2 (/ (- half oi) di)))
                   (declare (type double-float t1 t2))
                   (setf near (max near (min t1 t2))
                         far (min far (max t1 t2)))))))
         (and (<= near far) near))))))

(defun range-finder-spot (out state)
  "The range finder's spot: the ray from the grapple's tip along the Mule's +x
meets the drawn objects' shapes (any of their boxes and spheres) within
*range-finder-max*: true, and OUT the first hit pulled *range-spot-pull*
towards the ship; nil when nothing is hit. Conses nothing."
  (declare (optimize speed) (type vec3 out) (type render-state state))
  (let ((origin (make-vec3)) (direction (make-vec3)) (inverse (make-quat))
        (o (make-vec3)) (d (make-vec3)) (c (make-vec3))
        (best (the double-float *range-finder-max*)) (hit nil))
    (declare (dynamic-extent origin direction inverse o d c) (type double-float best))
    (ship-point-into! origin state *grapple-tip*)
    (vec3-set! direction 1d0 0d0 0d0)
    (quat-rotate! direction (render-state-q state) direction)
    (loop for object across (the simple-vector (render-state-objects state))
          do (let ((def (object-render-def (the object-render object))))
               (when (and def (object-render-drawn object))
                 (vec3-sub! c (object-render-r object) origin)
                 (when (<= (vec3-norm c) (+ best (object-def-bound def)))
                   (quat-conjugate! inverse (object-render-q object))
                   (vec3-sub! c origin (object-render-r object))
                   (quat-rotate! o inverse c)
                   (quat-rotate! d inverse direction)
                   (dolist (shape (object-def-shapes def))
                     (let ((distance (ray-shape-distance o d shape)))
                       (when (and distance (< (the double-float distance) best))
                         (setf best distance hit t))))))))
    (when hit
      (let ((pulled (max 0d0 (- best (the double-float *range-spot-pull*)))))
        (declare (type double-float pulled))
        (vec3-add-scaled! out origin direction pulled))
      t)))

(defun object-uniforms! (out state o eye)
  "The ship block (shaders.lisp) into OUT for the object O of the render
STATE seen from EYE: the rows of its q, its CoM (the body origin) relative to
the eye (decision 5: Precision of positions) with the glows' least sigma, no
gimbal, identity trim, then in its body axes the Sun's direction with
(1 AU / d)² x its sun-fraction, the direction to Earth with the earthshine
from its position, the tint and ambient, the eye with the lights on, no
emission. Conses nothing."
  (declare (optimize speed) (type (simple-array single-float (*)) out)
           (type render-state state) (type object-render o) (type vec3 eye))
  (let ((q (object-render-q o)) (r (object-render-r o))
        (sun (make-vec3)) (s-hat (make-vec3)) (r-hat (make-vec3)) (v (make-vec3))
        (inverse (make-quat)))
    (declare (dynamic-extent sun s-hat r-hat v inverse))
    (with-quat (((w x y z) q))
      (put4! out 0 (- 1 (* 2 (+ (* y y) (* z z)))) (* 2 (- (* x y) (* w z))) (* 2 (+ (* x z) (* w y))) 0d0)
      (put4! out 4 (* 2 (+ (* x y) (* w z))) (- 1 (* 2 (+ (* x x) (* z z)))) (* 2 (- (* y z) (* w x))) 0d0)
      (put4! out 8 (* 2 (- (* x z) (* w y))) (* 2 (+ (* y z) (* w x))) (- 1 (* 2 (+ (* x x) (* y y)))) 0d0))
    (dotimes (i 3)
      (setf (aref out (+ 12 i)) (coerce (- (aref r i) (aref eye i)) 'single-float)))
    (setf (aref out 15) (coerce (* 0.5d0 (the double-float *glow-min-pixels*)) 'single-float))
    (put4! out 16 0d0 0d0 0d0 0d0)
    (put4! out 20 1d0 0d0 0d0 0d0)
    (put4! out 24 0d0 1d0 0d0 0d0)
    (put4! out 28 0d0 0d0 1d0 0d0)
    (vec3-sub! sun (render-state-sun state) r)
    (let* ((d-sun (vec3-norm sun))
           (irradiance (let ((k (/ +au+ d-sun))) (* k k)))
           (earthshine (let* ((distance (vec3-norm r))
                              (k (/ +radius-earth+ distance)))
                         (vec3-scale! s-hat sun (/ d-sun))
                         (* (the double-float *earth-albedo*) k k (earth-lit-cap r s-hat) irradiance)))
           (tint *earthshine-tint*))
      (quat-conjugate! inverse q)
      (vec3-scale! r-hat r (/ -1d0 (vec3-norm r)))
      (quat-rotate! v inverse s-hat)
      (put4! out 32 (aref v 0) (aref v 1) (aref v 2) (* irradiance (object-render-sun-fraction o)))
      (quat-rotate! v inverse r-hat)
      (put4! out 36 (aref v 0) (aref v 1) (aref v 2) earthshine)
      (put4! out 40 (the double-float (first tint)) (the double-float (second tint))
             (the double-float (third tint)) (the double-float *ship-ambient*))
      (vec3-sub! v eye r)
      (quat-rotate! r-hat inverse v)
      (put4! out 44 (aref r-hat 0) (aref r-hat 1) (aref r-hat 2) 1d0)
      (put4! out 48 0d0 0d0 0d0 0d0))
    (object-floodlight-rows! out state o)
    out))

(declaim (inline strobe-lit-p))
(defun strobe-lit-p (time)
  "Whether a strobe is lit at TIME (s, not below 0) of the lights' clock: the
phase in *strobe-period*, reduced in double. Conses nothing."
  (declare (optimize speed) (type double-float time))
  (let* ((period (the double-float *strobe-period*))
         (n (truncate (the (double-float 0d0 1d12) (/ (max 0d0 time) period))))
         (phase (- (max 0d0 time) (* period n))))
    (declare (type double-float period phase))
    (< phase (the double-float *strobe-on*))))

(declaim (inline put-glow!))
(defun put-glow! (out n centre eye radius red green blue pull)
  "Glow N of OUT: CENTRE − EYE (double, then single), RADIUS, the colour and PULL."
  (declare (type (simple-array single-float (*)) out) (type fixnum n) (type vec3 centre eye)
           (type double-float radius red green blue pull))
  (let ((o (* n +glow-floats+)))
    (declare (type fixnum o))
    (dotimes (i 3)
      (setf (aref out (+ o i)) (coerce (- (aref centre i) (aref eye i)) 'single-float)))
    (setf (aref out (+ o 3)) (coerce radius 'single-float)
          (aref out (+ o 4)) (coerce red 'single-float)
          (aref out (+ o 5)) (coerce green 'single-float)
          (aref out (+ o 6)) (coerce blue 'single-float)
          (aref out (+ o 7)) (coerce pull 'single-float))))

(defun object-glows! (out gpu state o eye dot-p index)
  "The glows of the object O (the INDEXth of the sim's) seen from EYE into OUT,
+glow-floats+ each; returns their count: its beacons (red and green steady,
lamps steady, strobes on the lights' clock offset by *strobe-offset* x INDEX)
and, with DOT-P, the dot: a glow of the radius of a disc of a quarter of its
shapes' surface, radiance *dot-radiance* x the Sun term x (1 + cos phase) / 2
x its sun-fraction (decision 124). Conses nothing."
  (declare (optimize speed) (type (simple-array single-float (*)) out) (type object-gpu gpu)
           (type render-state state) (type object-render o) (type vec3 eye) (type fixnum index))
  (let ((q (object-render-q o)) (r (object-render-r o))
        (centre (make-vec3)) (local (make-vec3)) (n 0)
        (clock (+ (render-state-lights-t state) (* (the double-float *strobe-offset*) index))))
    (declare (dynamic-extent centre local) (type fixnum n) (type double-float clock))
    (dolist (beacon (object-gpu-beacons gpu))
      (let ((kind (first beacon)) (p (second beacon)))
        (declare (type vec3 p))
        (when (or (not (eq kind :strobe)) (strobe-lit-p clock))
          (quat-rotate! local q p)
          (vec3-add! centre local r)
          (let ((radius (ecase kind
                          ((:red :green) (the double-float *nav-light-radius*))
                          (:strobe (the double-float *strobe-radius*))
                          (:lamp (the double-float *lamp-glow-radius*)))))
            (declare (type double-float radius))
            (ecase kind
              (:red (let ((c *nav-red*) (l (the double-float *nav-light-radiance*)))
                      (put-glow! out n centre eye radius (* l (the double-float (first c)))
                                 (* l (the double-float (second c))) (* l (the double-float (third c)))
                                 (* (the double-float *light-pull*) radius))))
              (:green (let ((c *nav-green*) (l (the double-float *nav-light-radiance*)))
                        (put-glow! out n centre eye radius (* l (the double-float (first c)))
                                   (* l (the double-float (second c))) (* l (the double-float (third c)))
                                   (* (the double-float *light-pull*) radius))))
              (:strobe (let ((l (the double-float *strobe-radiance*)))
                         (put-glow! out n centre eye radius l l l
                                    (* (the double-float *light-pull*) radius))))
              (:lamp (let ((l (the double-float *lamp-radiance*)))
                       (put-glow! out n centre eye radius l l l
                                  (* (the double-float *light-pull*) radius)))))
            (incf n)))))
    (when dot-p
      (let* ((sun (make-vec3)) (to-eye (make-vec3)))
        (declare (dynamic-extent sun to-eye))
        (vec3-sub! sun (render-state-sun state) r)
        (vec3-sub! to-eye eye r)
        (let* ((d-sun (vec3-norm sun)) (d-eye (vec3-norm to-eye))
               (irradiance (let ((k (/ +au+ d-sun))) (* k k)))
               (phase (/ (vec3-dot sun to-eye) (* d-sun d-eye)))
               (radiance (* (the double-float *dot-radiance*) irradiance 0.5d0 (+ 1d0 phase)
                            (object-render-sun-fraction o))))
          (declare (type double-float d-sun d-eye irradiance phase radiance))
          (when (> radiance 0d0)
            (put-glow! out n r eye (object-gpu-dot-radius gpu) radiance radiance radiance 0d0)
            (incf n)))))
    n))

(defun object-draw-kind (gpu o camera extent eye)
  "How the frame draws the object O: :mesh when its model's extent projects to
*object-mesh-pixels* or more and reaches in front of the near plane, :dot when
smaller and ahead of the eye, nil when behind it. CAMERA, EXTENT (width
height) and EYE as the frame's. Conses nothing."
  (declare (optimize speed) (type object-gpu gpu) (type object-render o) (type camera camera)
           (type list extent) (type vec3 eye))
  (let ((c (make-vec3)) (forward (make-vec3)) (axis (make-vec3)))
    (declare (dynamic-extent c forward axis))
    (vec3-sub! c (object-render-r o) eye)
    (vec3-set! axis 1d0 0d0 0d0)
    (quat-rotate! forward (camera-orientation camera) axis)
    (let* ((d (max (vec3-norm c) 1d-3))
           (depth (vec3-dot forward c))
           (focal (/ (tan (* 0.5d0 (camera-field-of-view camera)))))
           (pixels (/ (* (object-gpu-extent gpu) focal 0.5d0 (float (the fixnum (second extent)) 1d0)) d)))
      (declare (type double-float d depth focal pixels))
      (cond ((>= pixels (the double-float *object-mesh-pixels*))
             (and (> (+ depth (object-gpu-extent gpu)) (camera-near camera)) :mesh))
            ((> depth 0d0) :dot)
            (t nil)))))

(defun encode-objects (pass renderer ship slot state camera extent eye)
  "The objects of the render STATE into the open scene PASS, after the Mule
(every view): each drawn one as its mesh or, far, its dot (object-draw-kind),
the hull, the engine if any, the instance groups, then its glows through
:ship-glow. Each object has its own block, bind group and glow buffer in SLOT."
  (let ((objects (render-state-objects state))
        (slots (ship-slot-objects slot)))
    (dotimes (i (min (length objects) (length slots)))
      (let* ((o (svref objects i))
             (def (object-render-def o))
             (gpu (and def (object-render-drawn o)
                       (gethash (object-def-key def) (ship-gpu-objects ship))))
             (kind (and gpu (object-draw-kind gpu o camera extent eye))))
        (when kind
          (let ((object-slot (svref slots i))
                (glows (ship-gpu-object-glows ship)))
            (let ((bind-group (object-slot-bind-group object-slot)))
              (luv:write-buffer (object-slot-buffer object-slot)
                                (object-uniforms! (ship-gpu-object-uniforms ship) state o eye))
              (when (eq kind :mesh)
                (when (object-gpu-hull gpu)
                  (luv:set-pipeline pass (renderer-pipeline renderer :ship-mesh))
                  (luv:set-bind-group pass 0 bind-group)
                  (luv:set-vertex-buffer pass 0 (object-gpu-hull gpu))
                  (luv:draw pass (object-gpu-hull-count gpu)))
                (when (object-gpu-groups gpu)
                  (luv:set-pipeline pass (renderer-pipeline renderer :ship-instanced))
                  (luv:set-bind-group pass 0 bind-group)
                  (loop for (primitive vertices instances count) in (object-gpu-groups gpu)
                        do (luv:set-vertex-buffer pass 0 primitive)
                           (luv:set-vertex-buffer pass 1 instances)
                           (luv:draw pass vertices count))))
              (let ((count (object-glows! glows gpu state o eye (eq kind :dot) i)))
                (when (plusp count)
                  (luv:write-buffer (object-slot-glows object-slot) glows)
                  (luv:set-pipeline pass (renderer-pipeline renderer :ship-glow))
                  (luv:set-bind-group pass 0 bind-group)
                  (luv:set-vertex-buffer pass 0 (ship-gpu-corners ship))
                  (luv:set-vertex-buffer pass 1 (object-slot-glows object-slot))
                  (luv:draw pass 6 count))))))))
      ;; The range finder's spot last: the objects' surfaces would cover it.
      (when (plusp (ship-spot-glow! (ship-gpu-spot ship) state eye))
        (luv:write-buffer (ship-slot-spot slot) (ship-gpu-spot ship))
        (luv:set-pipeline pass (renderer-pipeline renderer :ship-glow))
        (luv:set-bind-group pass 0 (ship-slot-bind-group slot))
        (luv:set-vertex-buffer pass 0 (ship-gpu-corners ship))
        (luv:set-vertex-buffer pass 1 (ship-slot-spot slot))
        (luv:draw pass 6 1))))

(defun single-floats (list)
  (map '(simple-array single-float (*)) (lambda (x) (coerce x 'single-float)) list))

;;; The renderer

(defparameter *background* '(0.0 0.0 0.0002) "Radiance the HDR target is cleared to.")
(defparameter *earth-texture* "earth/blue-marble-5400.jpg"
  "decision 40: Earth's look and drag.")
(defvar *earth-pixels* nil
  "Nil, or (pixels . coarse copy) taken instead of decoding *earth-texture*:
the tests' one decode for every canvas they open.")

(defconstant +frame-uniform-floats+ 116 "shaders.lisp's frame block: 29 vec4s.")
(defconstant +impostor-uniform-index+ 48 "The Sun's block, the Moon's, Earth's, the halo's.")
(defconstant +halo-dusk-index+ 112)

(defclass frame-renderer (renderer)
  ((context :initarg :context :reader frame-renderer-context)
   (layout :initarg :layout :reader frame-renderer-layout)
   (earth-view :initarg :earth-view :reader frame-renderer-earth-view)
   (earth-coarse-view :initarg :earth-coarse-view :reader frame-renderer-earth-coarse-view
                      :documentation "The Blue Marble's 1/8 copy (downsample-pixels).")
   (earth-sampler :initarg :earth-sampler :reader frame-renderer-earth-sampler)
   (earth-rows :initarg :earth-rows :reader frame-renderer-earth-rows)
   (stars :initarg :stars :reader frame-renderer-stars
          :documentation "The star instances (src/stars.lisp).")
   (star-corners :initarg :star-corners :reader frame-renderer-star-corners)
   (star-count :initarg :star-count :reader frame-renderer-star-count)
   (impostor-corners :initarg :impostor-corners :reader frame-renderer-impostor-corners)
   (ship :initarg :ship :reader frame-renderer-ship
         :documentation "The Mule's static resources (src/meshes.lisp).")
   (ribbon :initform (make-ribbon) :reader frame-renderer-ribbon
           :documentation "The map's orbit points and instances (src/ribbons.lisp).")
   (ribbon-corners :initarg :ribbon-corners :reader frame-renderer-ribbon-corners)
   (markers :initform (make-map-markers) :reader frame-renderer-markers
            :documentation "The map's markers (src/ribbons.lisp).")
   (overlay :initform (make-overlay-buffer) :reader frame-renderer-overlay
            :documentation "The frame's overlay vertices: the map's markers, the readout.")
   (overlay-floats :reader frame-renderer-overlay-floats
                   :documentation "The used part of overlay's floats, for write-buffer.")
   (post :initarg :post :reader frame-renderer-post)
   (post-sampler :initarg :post-sampler :reader frame-renderer-post-sampler)
   (test-quads :initform nil :accessor frame-renderer-test-quads
               :documentation "Tests only: nil or (buffer . count), drawn as stars.")
   (text-atlas :initarg :text-atlas :reader frame-renderer-text-atlas
               :documentation "The glyphs of all three fonts (src/text.lisp).")
   (text-layout :initarg :text-layout :reader frame-renderer-text-layout)
   (text-corners :initarg :text-corners :reader frame-renderer-text-corners)
   (text-batch :initform (make-text-batch) :reader frame-renderer-text-batch
               :documentation "The text records of the frame: regions callers rewrite.")
   (text-block :initform (make-array +text-block-floats+ :element-type 'single-float
                                                         :initial-element 0f0)
               :reader frame-renderer-text-block)
   (test-text :initform nil :accessor frame-renderer-test-text
              :documentation "Tests only: nil or a text-batch drawn in the scene by the world
camera's clip rows (positions relative to the eye in ICRF axes).")
   (frames :initform (luv:make-canvas-frame-resource-cache) :reader frame-renderer-frames
           :documentation "Per target: its frame-slot.")
   (swapchain-extent :initform nil :accessor frame-renderer-swapchain-extent)
   (swapchain-targets :initform '() :accessor frame-renderer-swapchain-targets
                      :documentation "The swapchain textures drawn at that extent.")
   (uniforms :initform (make-array +frame-uniform-floats+ :element-type 'single-float
                                                          :initial-element 0f0)
             :reader frame-renderer-uniforms)
   (eye :initform (make-vec3) :reader frame-renderer-eye)))

(defmethod initialize-instance :after ((renderer frame-renderer) &key)
  (let ((floats (overlay-buffer-floats (frame-renderer-overlay renderer))))
    (setf (slot-value renderer 'overlay-floats)
          (make-array (length floats) :element-type 'single-float
                                      :displaced-to floats :fill-pointer 0))))

(defun make-frame-renderer (device context)
  "The renderer of the game's frame on DEVICE, drawing into CONTEXT's format."
  (let* ((renderer (make-instance 'frame-renderer :device device :context context
                                                  :format (luv:canvas-format context)))
         (layout (gpu-create renderer (luv:make-bind-group-layout-descriptor
                                       :entries '((:binding 0 :type :uniform-buffer)
                                                  (:binding 1 :type :texture)
                                                  (:binding 2 :type :sampler)
                                                  (:binding 3 :type :texture)))))
         (stars (star-instances (load-star-table))))
    (let* ((pixels (or (car *earth-pixels*) (load-image-pixels (asset-path *earth-texture*))))
           (coarse (or (cdr *earth-pixels*) (downsample-pixels pixels *earth-coarse-factor*))))
      (multiple-value-bind (texture view sampler) (texture-from-pixels renderer pixels "earth")
        (declare (ignore texture))
        (reinitialize-instance renderer :earth-view view :earth-sampler sampler
                                        :earth-rows (array-dimension pixels 0)
                                        :earth-coarse-view
                                        (nth-value 1 (texture-from-pixels renderer coarse
                                                                          "earth 1/8")))))
    (reinitialize-instance
     renderer
     :layout layout
     :stars (vertex-buffer renderer stars "stars")
     :star-corners (vertex-buffer renderer (star-corners) "star corners")
     :star-count (/ (length stars) +star-instance-floats+)
     :impostor-corners (vertex-buffer renderer (impostor-corners) "impostor corners")
     :ribbon-corners (vertex-buffer renderer (ribbon-corners) "ribbon corners")
     :post (vertex-buffer renderer (post-vertices) "post")
     :post-sampler (gpu-create renderer (luv:make-sampler-descriptor
                                         :label "post"
                                         :address-mode-u :clamp-to-edge
                                         :address-mode-v :clamp-to-edge
                                         :mag-filter :linear :min-filter :linear)))
    (stars-pipeline renderer layout +hdr-format+)
    (impostor-pipelines renderer layout +hdr-format+)
    (reinitialize-instance renderer :ship (make-ship-gpu renderer +hdr-format+))
    (ribbon-pipeline renderer layout +hdr-format+)
    (live-pipeline renderer :quads layout (overlay-vertex-buffers) :format +hdr-format+
                   :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                    :depth-compare :always))
    (post-pipelines renderer layout)
    (overlay-pipeline renderer layout)
    (let ((text-layout (text-layout renderer)))
      (reinitialize-instance renderer
                             :text-atlas (make-text-atlas-for (renderer-device renderer))
                             :text-layout text-layout
                             :text-corners (vertex-buffer renderer (text-corners) "text corners"))
      (text-pipeline renderer text-layout +hdr-format+))
    renderer))

(defstruct frame-slot
"A target's frame block, its scene bind group, its ship-slot, its ribbon
instance buffer, its overlay vertices, its text-slot, and the bind groups sampling the
post levels in POST-VIEWS."
  buffer bind-group ship ribbon overlay text (post-views '()) (post-bind-groups '()))

(defun release-frame-resources (slot)
  (release-ship-slot (frame-slot-ship slot))
  (luv:destroy (frame-slot-ribbon slot))
  (luv:destroy (frame-slot-overlay slot))
  (release-text-slot (frame-slot-text slot))
  (mapc #'luv:destroy (frame-slot-post-bind-groups slot))
  (luv:destroy (frame-slot-bind-group slot))
  (luv:destroy (frame-slot-buffer slot)))

(defun frame-bind-group (renderer buffer view sampler &optional (base view))
  (luv:create (renderer-device renderer)
              (luv:make-bind-group-descriptor
               :layout (frame-renderer-layout renderer)
               :entries `((:binding 0 :resource ,buffer)
                          (:binding 1 :resource ,view)
                          (:binding 2 :resource ,sampler)
                          (:binding 3 :resource ,base)))))

(defun frame-resources (renderer target)
  "TARGET's frame-slot, made on first use; a swapchain texture's is free
again once it is acquired (luv's canvas-frame-resource-cache)."
  (values (luv:canvas-frame-resource
           (frame-renderer-frames renderer) (frame-renderer-context renderer) target
           (lambda (key texture)
             (declare (ignore key texture))
             (let ((buffer (luv:create (renderer-device renderer)
                                       (luv:make-buffer-descriptor
                                        :label "frame" :size (* 4 +frame-uniform-floats+)
                                        :usage '(:uniform)))))
               (make-frame-slot
                :buffer buffer
                :bind-group (frame-bind-group renderer buffer
                                              (frame-renderer-earth-view renderer)
                                              (frame-renderer-earth-sampler renderer)
                                              (frame-renderer-earth-coarse-view renderer))
                :ship (make-ship-target-slot renderer (frame-renderer-ship renderer) buffer)
                :ribbon (make-ribbon-buffer (renderer-device renderer)
                                            (frame-renderer-ribbon renderer))
                :text (make-text-slot renderer (frame-renderer-text-layout renderer)
                                      (frame-renderer-text-atlas renderer)
                                      (text-batch-capacity (frame-renderer-text-batch renderer)))
                :overlay (luv:create (renderer-device renderer)
                                     (luv:make-buffer-descriptor
                                      :label "overlay"
                                      :size (* 4 (array-total-size
                                                  (frame-renderer-overlay-floats renderer)))
                                      :usage '(:vertex :copy-dst)))))))))

(defun post-bind-groups (renderer slot views)
  "SLOT's bind groups sampling VIEWS, remade when the post targets were."
  (unless (and (frame-slot-post-views slot) (every #'eq views (frame-slot-post-views slot)))
    (mapc #'luv:destroy (frame-slot-post-bind-groups slot))
    (setf (frame-slot-post-views slot) views
          (frame-slot-post-bind-groups slot)
          (loop for (source base) in (post-sources views)
                collect (frame-bind-group renderer (frame-slot-buffer slot) source
                                          (frame-renderer-post-sampler renderer) base))))
  (frame-slot-post-bind-groups slot))

(defun set-test-quads (renderer vertices)
  "Tests only: overlay VERTICES (a list of floats) drawn opaque through the
:quads pipeline into the HDR target; nil removes them."
  (setf (frame-renderer-test-quads renderer)
        (and vertices
             (let ((data (single-floats vertices)))
               (cons (vertex-buffer renderer data "test quads")
                     (/ (length data) +overlay-vertex-floats+))))))

(defun forget-target (renderer target)
  "Release what the renderer keeps for TARGET, before TARGET goes."
  (luv:evict-canvas-frame-resource (frame-renderer-frames renderer)
                                   (frame-renderer-context renderer) target
                                   #'release-frame-resources)
  (forget-color-view renderer target))

(defun note-swapchain-target (renderer target extent)
  "Record TARGET, a swapchain texture of the canvas's EXTENT. A new extent
means luv rebuilt the swapchain: forget its old textures first."
  (unless (equal extent (frame-renderer-swapchain-extent renderer))
    (dolist (old (frame-renderer-swapchain-targets renderer))
      (forget-target renderer old))
    (setf (frame-renderer-swapchain-targets renderer) '()
          (frame-renderer-swapchain-extent renderer) extent))
  (pushnew target (frame-renderer-swapchain-targets renderer)))

(defun destroy-frame-renderer (renderer)
  (luv:destroy-canvas-frame-resource-cache (frame-renderer-frames renderer)
                                           #'release-frame-resources)
  (release-text-atlas (frame-renderer-text-atlas renderer))
  (destroy-renderer renderer))

(defun srgb-target-p (renderer)
  (search "SRGB" (symbol-name (renderer-format renderer))))

(defun frame-uniforms! (renderer extent state camera exposure)
  "The frame block: clip rows, the halo's colours, the Sun's direction and
(1 AU / d)^2, clip units per pixel, Earth's body-fixed matrix, the post and
stars vec4s, the ribbon's width and radiance, the impostors of the Sun, the Moon, Earth and its halo (centres
relative to the eye, decision 5: Precision of positions). Returns the block
and the four draw forms."
  (destructuring-bind (width height) extent
    (let ((data (frame-renderer-uniforms renderer))
          (eye (let ((eye (frame-renderer-eye renderer)))
                 (camera-eye camera (camera-anchor-position! eye camera state) eye)))
          (sun (render-state-sun state))
          (matrix (render-state-earth-matrix state)))
      (camera-clip-rows! data camera (/ width height))
      (halo-colours! data 16 +halo-dusk-index+)
      (let ((d (vec3-norm sun)))
        (dotimes (i 3)
          (setf (aref data (+ 20 i)) (coerce (/ (aref sun i) d) 'single-float)))
        (setf (aref data 23) (coerce (expt (/ +au+ d) 2) 'single-float)))
      (setf (aref data 24) (coerce (/ 2 width) 'single-float)
            (aref data 25) (coerce (/ 2 height) 'single-float)
            (aref data 26) (coerce *ribbon-width-px* 'single-float)
            (aref data 27) (coerce *ribbon-radiance* 'single-float))
      (dotimes (row 3)
        (dotimes (k 3)
          (setf (aref data (+ 28 (* 4 row) k)) (coerce (aref matrix (+ (* 3 row) k)) 'single-float)))
        (setf (aref data (+ 31 (* 4 row))) 0f0))
      (setf (aref data 40) (coerce exposure 'single-float)
            (aref data 41) (coerce *bloom-strength* 'single-float)
            (aref data 42) (if (srgb-target-p renderer) 0f0 1f0)
            (aref data 43) 0f0)
      (star-uniforms! data 44)
      (multiple-value-bind (sun-form moon-form earth-form halo-form)
          (impostor-uniforms! data +impostor-uniform-index+ state eye camera
                              (frame-renderer-earth-rows renderer))
        (values data (list sun-form moon-form earth-form halo-form))))))

(defun encode-overlay (pass renderer bind-group buffer state camera extent view readout panel)
  "The overlay into the open PASS through BUFFER: in :map the markers and
their labels (decision 84), then READOUT, nil or readout-lines' lines, top
left (decision 85), then PANEL, nil or lost-panel-lines' lines (decision 86)."
  (let ((out (overlay-clear! (frame-renderer-overlay renderer)))
        (floats (frame-renderer-overlay-floats renderer)))
    (when (eq view :map)
      (map-marker-vertices! out (map-markers state camera extent (frame-renderer-markers renderer))))
    (when readout
      (readout-vertices! out readout *readout-margin-px* *readout-margin-px*))
    (when panel
      (multiple-value-bind (x y) (lost-panel-origin panel extent)
        (readout-vertices! out panel x y)))
    (setf (fill-pointer floats) (* +overlay-vertex-floats+ (overlay-buffer-count out)))
    (when (plusp (overlay-buffer-count out))
      (luv:write-buffer buffer floats)
      (luv:set-pipeline pass (renderer-pipeline renderer :overlay))
      (luv:set-bind-group pass 0 bind-group)
      (luv:set-vertex-buffer pass 0 buffer)
      (luv:draw pass (overlay-buffer-count out)))))

(defun encode-frame (renderer encoder target extent state camera exposure
                     &optional (view :outside) readout panel &key (interior t))
  "The frame into TARGET of EXTENT (width height) from the render STATE seen
by CAMERA, in three stages (decision 76: Frame passes, radiance scale and
exposure): the scene pass into the HDR target (the stars, the Sun, the
Moon, Earth, Earth's halo, the Mule, the objects, in :map the orbit's ribbon); the bloom
chain; one pass on TARGET, the tone map at EXPOSURE and then the overlay
(in :map the markers and their labels, then READOUT, readout-lines' lines
or nil, then the LOST PANEL's lines or nil). VIEW: :outside, :cockpit (the
Mule without its cab exterior, the eye being inside the cab; decision 126:
Cockpit structure and view in milestone 3: frames, draw order, Slug text, head
look, exposure, cab light, portholes, bezels, tape labels) or :map. INTERIOR
nil leaves the cab's interior out of :cockpit (for tests and the grapple
camera); the interior itself comes with plan 31's later pieces."
  (declare (ignorable interior))
  (let* ((slot (frame-resources renderer target))
         (views (post-views renderer extent))
         (bind-groups (post-bind-groups renderer slot views))
         (bind-group (frame-slot-bind-group slot)))
    (multiple-value-bind (data forms)
        (frame-uniforms! renderer extent state camera exposure)
      (luv:write-buffer (frame-slot-buffer slot) data)
      (flet ((draw (pass role vertices count)
               (luv:set-pipeline pass (renderer-pipeline renderer role))
               (luv:set-bind-group pass 0 bind-group)
               (luv:set-vertex-buffer pass 0 vertices)
               (luv:draw pass count)))
        (let ((pass (luv:begin-render-pass
                     encoder
                     (luv:make-render-pass-descriptor
                      :color-attachments
                      `((:view ,(first views) :load-op :clear :store-op :store
                         :clear-value ,(coerce (append *background* '(1.0)) 'vector)))
                      :depth-stencil-attachment
                      `(:view ,(depth-view renderer extent)
                        :depth-load-op :clear :depth-store-op :store
                        :depth-clear-value 0.0)))))
          (luv:set-pipeline pass (renderer-pipeline renderer :stars))
          (luv:set-bind-group pass 0 bind-group)
          (luv:set-vertex-buffer pass 0 (frame-renderer-star-corners renderer))
          (luv:set-vertex-buffer pass 1 (frame-renderer-stars renderer))
          (luv:draw pass 6 (frame-renderer-star-count renderer))
          (let ((corners (frame-renderer-impostor-corners renderer)))
            (loop for role in '(:sun :moon :earth :earth-halo)
                  for form in forms
                  do (draw-impostor pass renderer role form bind-group corners)))
          (encode-ship pass renderer (frame-renderer-ship renderer) (frame-slot-ship slot)
                       state (frame-renderer-eye renderer) (eq view :cockpit))
          (encode-objects pass renderer (frame-renderer-ship renderer) (frame-slot-ship slot)
                          state camera extent (frame-renderer-eye renderer))
          (let ((text (frame-renderer-test-text renderer)))
            (when text
              (encode-text pass renderer (frame-slot-text slot) text
                           (text-block! (frame-renderer-text-block renderer) data extent exposure)
                           (frame-renderer-text-corners renderer))))
          (when (eq view :map)
            (encode-ribbon pass renderer bind-group (frame-renderer-ribbon renderer)
                           (frame-slot-ribbon slot) (frame-renderer-ribbon-corners renderer)
                           state camera (frame-renderer-eye renderer) (second extent)))
          (let ((quads (frame-renderer-test-quads renderer)))
            (when quads (draw pass :quads (car quads) (cdr quads))))
          (luv:end-pass pass))
        (encode-bloom renderer encoder views bind-groups (frame-renderer-post renderer))
        (let ((pass (luv:begin-render-pass
                     encoder
                     (luv:make-render-pass-descriptor
                      :color-attachments
                      `((:view ,(color-view renderer target) :load-op :clear :store-op :store
                         :clear-value #(0.0 0.0 0.0 1.0)))))))
          (draw-tone-map renderer pass (car (last bind-groups)) (frame-renderer-post renderer))
          (encode-overlay pass renderer bind-group (frame-slot-overlay slot)
                          state camera extent view readout panel)
          (luv:end-pass pass))))))
