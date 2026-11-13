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
  (sun-fraction 1d0 :type double-float))

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
          (setf (object-render-drawn o) (and drawn t))
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
    renderer))

(defstruct frame-slot
"A target's frame block, its scene bind group, its ship-slot, its ribbon
instance buffer, its overlay vertices, and the bind groups sampling the
post levels in POST-VIEWS."
  buffer bind-group ship ribbon overlay (post-views '()) (post-bind-groups '()))

(defun release-frame-resources (slot)
  (release-ship-slot (frame-slot-ship slot))
  (luv:destroy (frame-slot-ribbon slot))
  (luv:destroy (frame-slot-overlay slot))
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
                     &optional (view :outside) readout panel)
  "The frame into TARGET of EXTENT (width height) from the render STATE seen
by CAMERA, in three stages (decision 76: Frame passes, radiance scale and
exposure): the scene pass into the HDR target (the stars, the Sun, the
Moon, Earth, Earth's halo, the Mule, in :map the orbit's ribbon); the bloom
chain; one pass on TARGET, the tone map at EXPOSURE and then the overlay
(in :map the markers and their labels, then READOUT, readout-lines' lines
or nil, then the LOST PANEL's lines or nil). VIEW: :outside or :map."
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
                       state (frame-renderer-eye renderer))
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
