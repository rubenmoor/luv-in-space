(in-package #:luv-in-space)

;;; The render state interpolated from the clock, and the frame's passes
;;; (docs/architecture.md "The clock", "Rendering precision"; decision 77:
;;; The frame in main.lisp: render state, capture of a given sim, part 4's
;;; placeholders and first asset). Reads the simulation, never changes it.

;;; The render state

(defstruct (render-state (:constructor make-render-state ()) (:copier nil))
  "What a frame draws, at the render time T: the ship (R, V, Q, W as in
ship-state), M-P, CONTROLS and LOSS from the sim, the Sun and the Moon
relative to Earth, Earth's body-fixed matrix and the ship's SUN-FRACTION
(sun-visible-fraction) at T; the hull's SHIP-EMISSIVE colour,
SHIP-LIGHTS-ON (decision 80) and LIGHTS-T, the real seconds the clock has
run, the strobe's clock (decision 100: The lights' clock and the glows'
numbers), which render-state-update! leaves alone."
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
  (sun-fraction 1d0 :type double-float)
  (ship-emissive (make-vec3) :type vec3)
  (ship-lights-on t)
  (lights-t 0d0 :type double-float)
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
      (earth-fixed-matrix! (render-state-earth-matrix out) tr)
      (setf (render-state-sun-fraction out) (sun-visible-fraction (render-state-r out) tr)))
    out))

(defun single-floats (list)
  (map '(simple-array single-float (*)) (lambda (x) (coerce x 'single-float)) list))

;;; The renderer

(defparameter *background* '(0.0 0.0 0.0002) "Radiance the HDR target is cleared to.")
(defparameter *earth-texture* "earth/blue-marble-5400.jpg"
  "decision 40: Earth's look and drag.")

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
   (eye :initform (make-vec3) :reader frame-renderer-eye)))

(defun make-frame-renderer (device context)
  "The renderer of the game's frame on DEVICE, drawing into CONTEXT's format."
  (let* ((renderer (make-instance 'frame-renderer :device device :context context
                                                  :format (luv:canvas-format context)))
         (layout (gpu-create renderer (luv:make-bind-group-layout-descriptor
                                       :entries '((:binding 0 :type :uniform-buffer)
                                                  (:binding 1 :type :texture)
                                                  (:binding 2 :type :sampler)
                                                  (:binding 3 :type :texture)))))
         (stars (star-instances (load-star-table)))
         (label (label-vertices *label*)))
    (let ((pixels (load-image-pixels (asset-path *earth-texture*))))
      (multiple-value-bind (texture view sampler) (texture-from-pixels renderer pixels "earth")
        (declare (ignore texture))
        (reinitialize-instance renderer :earth-view view :earth-sampler sampler
                                        :earth-rows (array-dimension pixels 0)
                                        :earth-coarse-view
                                        (nth-value 1 (texture-from-pixels
                                                      renderer
                                                      (downsample-pixels pixels *earth-coarse-factor*)
                                                      "earth 1/8")))))
    (reinitialize-instance
     renderer
     :layout layout
     :stars (vertex-buffer renderer stars "stars")
     :star-corners (vertex-buffer renderer (star-corners) "star corners")
     :star-count (/ (length stars) +star-instance-floats+)
     :impostor-corners (vertex-buffer renderer (impostor-corners) "impostor corners")
     :ribbon-corners (vertex-buffer renderer (ribbon-corners) "ribbon corners")
     :label (vertex-buffer renderer label "label")
     :label-count (/ (length label) +overlay-vertex-floats+)
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
instance buffer, and the bind groups sampling the post levels in POST-VIEWS."
  buffer bind-group ship ribbon (post-views '()) (post-bind-groups '()))

(defun release-frame-resources (slot)
  (release-ship-slot (frame-slot-ship slot))
  (luv:destroy (frame-slot-ribbon slot))
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
                                            (frame-renderer-ribbon renderer))))))))

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

(defun encode-frame (renderer encoder target extent state camera exposure &optional (view :outside))
  "The frame into TARGET of EXTENT (width height) from the render STATE seen
by CAMERA, in three stages (decision 76: Frame passes, radiance scale and
exposure): the scene pass into the HDR target (the stars, the Sun, the
Moon, Earth, Earth's halo, the Mule, in :map the orbit's ribbon); the bloom
chain; one pass on TARGET, the tone map at EXPOSURE and then the overlay.
VIEW: :outside or :map."
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
          (draw pass :overlay (frame-renderer-label renderer) (frame-renderer-label-count renderer))
          (luv:end-pass pass))))))
