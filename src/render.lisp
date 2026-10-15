(in-package #:luv-in-space)

;;; The render state interpolated from the clock, and the frame's passes
;;; (docs/architecture.md "The clock", "Rendering precision"; decision 77:
;;; The frame in main.lisp: render state, capture of a given sim, part 4's
;;; placeholders and first asset). Reads the simulation, never changes it.

;;; The render state

(defstruct (render-state (:constructor make-render-state ()) (:copier nil))
  "What a frame draws, at the render time T: the ship (R, V, Q, W as in
ship-state), M-P, CONTROLS and LOSS from the sim, the Sun and the Moon
relative to Earth and Earth's body-fixed matrix at T."
  (t 0d0 :type double-float)
  (r (make-vec3) :type vec3)
  (v (make-vec3) :type vec3)
  (q (quat 1 0 0 0) :type quat)
  (w (make-vec3) :type vec3)
  (m-p 0d0 :type double-float)
  (controls (new-controls) :type controls)
  (loss nil)
  (sun (make-vec3) :type vec3)
  (moon (make-vec3) :type vec3)
  (earth-matrix (make-mat3) :type mat3)
  (scratch (make-vec3) :type vec3))     ; the bodies' velocities, unused

(defun render-state-assign-ship! (out ship)
  (replace (render-state-r out) (ship-state-r ship))
  (replace (render-state-v out) (ship-state-v ship))
  (replace (render-state-q out) (ship-state-q ship))
  (replace (render-state-w out) (ship-state-w ship)))

(declaim (inline render-state-hermite!))
(defun render-state-hermite! (out previous current h s)
  "Cubic Hermite position and velocity over a step of H s at S, slerp of
the attitude, ω blended linearly."
  (declare (type render-state out) (type ship-state previous current)
           (type double-float h s))
  (let* ((s2 (* s s)) (s3 (* s2 s))
         (h00 (+ (* 2 s3) (* -3 s2) 1)) (h10 (+ s3 (* -2 s2) s))
         (h01 (+ (* -2 s3) (* 3 s2))) (h11 (- s3 s2))
         (d00 (/ (* 6 (- s2 s)) h)) (d10 (+ (* 3 s2) (* -4 s) 1)) (d11 (- (* 3 s2) (* 2 s)))
         (p0 (ship-state-r previous)) (v0 (ship-state-v previous))
         (p1 (ship-state-r current)) (v1 (ship-state-v current))
         (w0 (ship-state-w previous)) (w1 (ship-state-w current)))
    (dotimes (i 3)
      (setf (aref (render-state-r out) i)
            (+ (* h00 (aref p0 i)) (* h h10 (aref v0 i))
               (* h01 (aref p1 i)) (* h h11 (aref v1 i)))
            (aref (render-state-v out) i)
            (+ (* d00 (- (aref p0 i) (aref p1 i)))
               (* d10 (aref v0 i)) (* d11 (aref v1 i)))
            (aref (render-state-w out) i)
            (+ (* (- 1 s) (aref w0 i)) (* s (aref w1 i)))))
    (quat-slerp! (render-state-q out) (ship-state-q previous) (ship-state-q current) s)))

(defun render-state-update! (out clock sim)
  "Fill OUT for the frame (decision 77): render time t_prev + s (t − t_prev),
s = clamp(acc / (t − t_prev), 0, 1); the sim's state as it is when t_prev is
not below t or the sim is lost. Conses nothing; OUT."
  (declare (optimize speed) (type render-state out) (type clock clock) (type sim sim))
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
               (setf (render-state-t out) (+ t-prev (* s h)))))))
    (setf (render-state-m-p out) (ship-state-m-p current)
          (render-state-loss out) (sim-loss sim))
    (controls-assign! (render-state-controls out) (sim-controls sim))
    (let ((tr (render-state-t out)))
      (body-state-into (render-state-sun out) (render-state-scratch out) :sun tr :earth)
      (body-state-into (render-state-moon out) (render-state-scratch out) :moon tr :earth)
      (earth-fixed-matrix! (render-state-earth-matrix out) tr))
    out))

;;; Meshes: pure functions, no GPU.

(defun single-floats (list)
  (map '(simple-array single-float (*)) (lambda (x) (coerce x 'single-float)) list))

(defun uv-sphere (&key (stacks 32) (slices 64))
  "Triangle-list vertices of a unit sphere, three floats each; each position
is also its normal."
  (let ((data (make-array (* stacks slices 6 3) :element-type 'single-float))
        (index 0))
    (flet ((corner (i j)
             (let ((theta (* pi (/ i stacks))) (phi (* 2 pi (/ j slices))))
               (dolist (x (list (* (sin theta) (cos phi)) (cos theta)
                                (* (sin theta) (sin phi))))
                 (setf (aref data index) (coerce x 'single-float))
                 (incf index)))))
      (dotimes (i stacks)
        (dotimes (j slices)
          (loop for (a b) in '((0 0) (1 0) (1 1) (0 0) (1 1) (0 1))
                do (corner (+ i a) (+ j b))))))
    data))

;; PLACEHOLDER(76): screen-space stars until part 5's catalogue.
(defun star-vertices (&key (count 300) (seed 23))
  "Overlay vertices of COUNT faint stars at fixed random screen positions;
radiances seen at *exposure-deep*, gone at *exposure-sunlit* (decision 76)."
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat count
          for brightness = (+ 0.001 (random 0.004 state))
          append (quad (- (random 2.0 state) 1.0) (- (random 2.0 state) 1.0)
                       -1 -1 (if (< (random 1.0 state) 0.1) 3 2)
                       (list brightness brightness (* 1.1 brightness))))))

;;; The renderer

(defparameter *background* '(0.0 0.0 0.0002) "Radiance the HDR target is cleared to.")
(defparameter *earth-stacks* 64)
(defparameter *earth-slices* 128)
(defparameter *earth-texture* "earth/blue-marble-5400.jpg"
  "decision 40: Earth's look and drag.")

(defconstant +frame-uniform-floats+ 44 "shaders.lisp's frame block: eleven vec4s.")

(defclass frame-renderer (renderer)
  ((context :initarg :context :reader frame-renderer-context)
   (layout :initarg :layout :reader frame-renderer-layout)
   (earth :initarg :earth :reader frame-renderer-earth)
   (earth-count :initarg :earth-count :reader frame-renderer-earth-count)
   (earth-view :initarg :earth-view :reader frame-renderer-earth-view)
   (earth-sampler :initarg :earth-sampler :reader frame-renderer-earth-sampler)
   (stars :initarg :stars :reader frame-renderer-stars)
   (star-count :initarg :star-count :reader frame-renderer-star-count)
   (label :initarg :label :reader frame-renderer-label)
   (label-count :initarg :label-count :reader frame-renderer-label-count)
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
   (eye :initform (make-vec3) :reader frame-renderer-eye)
   (origin :initform (make-vec3) :reader frame-renderer-origin)))

(defun make-frame-renderer (device context)
  "The renderer of the game's frame on DEVICE, drawing into CONTEXT's format."
  (let* ((renderer (make-instance 'frame-renderer :device device :context context
                                                  :format (luv:canvas-format context)))
         (layout (gpu-create renderer (luv:make-bind-group-layout-descriptor
                                       :entries '((:binding 0 :type :uniform-buffer)
                                                  (:binding 1 :type :texture)
                                                  (:binding 2 :type :sampler)
                                                  (:binding 3 :type :texture)))))
         (earth (uv-sphere :stacks *earth-stacks* :slices *earth-slices*))
         (stars (single-floats (star-vertices)))
         (label (single-floats (label-vertices *label*))))
    (multiple-value-bind (texture view sampler)
        (texture-from-pixels renderer (load-image-pixels (asset-path *earth-texture*)) "earth")
      (declare (ignore texture))
      (reinitialize-instance renderer :earth-view view :earth-sampler sampler))
    (reinitialize-instance
     renderer
     :layout layout
     :earth (vertex-buffer renderer earth "earth") :earth-count (/ (length earth) 3)
     :stars (vertex-buffer renderer stars "stars")
     :star-count (/ (length stars) +overlay-vertex-floats+)
     :label (vertex-buffer renderer label "label")
     :label-count (/ (length label) +overlay-vertex-floats+)
     :post (vertex-buffer renderer (post-vertices) "post")
     :post-sampler (gpu-create renderer (luv:make-sampler-descriptor
                                         :label "post"
                                         :address-mode-u :clamp-to-edge
                                         :address-mode-v :clamp-to-edge
                                         :mag-filter :linear :min-filter :linear)))
    (live-pipeline renderer :earth layout
                   '((:array-stride 12
                      :attributes ((:shader-location 0 :offset 0 :format :float32x3))))
                   :format +hdr-format+
                   ;; decision 4: Depth strategy
                   :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                    :depth-compare :greater))
    (live-pipeline renderer :stars layout (overlay-vertex-buffers) :format +hdr-format+
                   :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                    :depth-compare :always))
    (post-pipelines renderer layout)
    (overlay-pipeline renderer layout)
    renderer))

(defstruct frame-slot
  "A target's frame block, its scene bind group, and the bind groups
sampling the post levels in POST-VIEWS."
  buffer bind-group (post-views '()) (post-bind-groups '()))

(defun release-frame-resources (slot)
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
                                              (frame-renderer-earth-sampler renderer))))))))

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
  "Tests only: overlay VERTICES (a list of floats) drawn through the stars'
pipeline into the HDR target; nil removes them."
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
  "The frame block: clip rows, Earth relative to the eye (decision 5:
Precision of positions), the Sun's direction, clip units per pixel, Earth's
body-fixed matrix, the post vec4."
  (destructuring-bind (width height) extent
    (let ((data (frame-renderer-uniforms renderer))
          (eye (camera-eye camera (render-state-r state) (frame-renderer-eye renderer)))
          (sun (render-state-sun state))
          (matrix (render-state-earth-matrix state)))
      (camera-clip-rows! data camera (/ width height))
      (eye-relative-single (frame-renderer-origin renderer) eye data 16)
      (setf (aref data 19) (coerce +radius-earth+ 'single-float))
      (let ((d (vec3-norm sun)))
        (dotimes (i 3)
          (setf (aref data (+ 20 i)) (coerce (/ (aref sun i) d) 'single-float))))
      (setf (aref data 23) 0f0
            (aref data 24) (coerce (/ 2 width) 'single-float)
            (aref data 25) (coerce (/ 2 height) 'single-float)
            (aref data 26) 0f0 (aref data 27) 0f0)
      (dotimes (row 3)
        (dotimes (k 3)
          (setf (aref data (+ 28 (* 4 row) k)) (coerce (aref matrix (+ (* 3 row) k)) 'single-float)))
        (setf (aref data (+ 31 (* 4 row))) 0f0))
      (setf (aref data 40) (coerce exposure 'single-float)
            (aref data 41) (coerce *bloom-strength* 'single-float)
            (aref data 42) (if (srgb-target-p renderer) 0f0 1f0)
            (aref data 43) 0f0)
      data)))

(defun encode-frame (renderer encoder target extent state camera exposure)
  "The frame into TARGET of EXTENT (width height) from the render STATE seen
by CAMERA, in three stages (decision 76: Frame passes, radiance scale and
exposure): the scene pass into the HDR target (the placeholder stars, the
placeholder Earth, PLACEHOLDER(76)); the bloom chain; one pass on TARGET,
the tone map at EXPOSURE and then the overlay."
  (let* ((slot (frame-resources renderer target))
         (views (post-views renderer extent))
         (bind-groups (post-bind-groups renderer slot views))
         (bind-group (frame-slot-bind-group slot)))
    (luv:write-buffer (frame-slot-buffer slot)
                      (frame-uniforms! renderer extent state camera exposure))
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
        (draw pass :stars (frame-renderer-stars renderer) (frame-renderer-star-count renderer))
        (draw pass :earth (frame-renderer-earth renderer) (frame-renderer-earth-count renderer))
        (let ((quads (frame-renderer-test-quads renderer)))
          (when quads (draw pass :stars (car quads) (cdr quads))))
        (luv:end-pass pass))
      (encode-bloom renderer encoder views bind-groups (frame-renderer-post renderer))
      (let ((pass (luv:begin-render-pass
                   encoder
                   (luv:make-render-pass-descriptor
                    :color-attachments
                    `((:view ,(color-view renderer target) :load-op :clear :store-op :store
                       :clear-value #(0.0 0.0 0.0 1.0)))))))
        (draw-tone-map renderer pass (car (last bind-groups)) (frame-renderer-post renderer))
        (draw pass :overlay (frame-renderer-label renderer) (frame-renderer-label-count renderer))
        (luv:end-pass pass)))))
