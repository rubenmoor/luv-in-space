(in-package #:luv-in-space)

;;; HAL helpers: buffers, live pipelines, size-cached targets and their views,
;;; textures, and the renderer that owns them (docs/luv-capabilities.md
;;; "Rendering"). luv's destroy is deferred until submitted work is done.

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   (format :initarg :format :reader renderer-format
           :documentation "The canvas's colour format.")
   (pipelines :initform (make-hash-table :test #'eq) :reader renderer-pipelines
              :documentation "Role -> live shader pipeline.")
   (owned :initform '() :accessor renderer-owned
          :documentation "Buffers, textures, views, samplers and bind groups to destroy.")
   (targets :initform (make-hash-table :test #'eq) :reader renderer-targets
            :documentation "Name -> (extent format texture view).")
   (color-views :initform (make-hash-table :test #'eq) :reader renderer-color-views
                :documentation "Views of textures the renderer does not own.")))

(defun renderer-own (renderer object)
  (push object (renderer-owned renderer))
  object)

(defun gpu-create (renderer descriptor)
  "Create DESCRIPTOR's object on the renderer's device; the renderer destroys it."
  (renderer-own renderer (luv:create (renderer-device renderer) descriptor)))

(defun vertex-buffer (renderer data label)
  "A vertex buffer holding DATA, a single-float vector."
  (let ((buffer (gpu-create renderer (luv:make-buffer-descriptor
                                      :label label :size (* 4 (length data))
                                      :usage '(:vertex :copy-dst)))))
    (luv:write-buffer buffer data)
    buffer))

(defun uniform-buffer (renderer floats label)
  "A uniform buffer of FLOATS single floats."
  (gpu-create renderer (luv:make-buffer-descriptor
                        :label label :size (* 4 floats) :usage '(:uniform))))

(defun live-pipeline (renderer role layout vertex-buffers
                      &key (format (renderer-format renderer)) depth-stencil blend
                        (topology :triangle-list))
  "A luv live shader pipeline for ROLE (decision 11: Render pipelines are luv
live shader pipelines), kept in the renderer under ROLE: rebuilt when its
shaders are redefined, the last good one kept when a rebuild fails."
  (let ((old (gethash role (renderer-pipelines renderer))))
    (when old (luv:release-live-artifact old)))
  (setf (gethash role (renderer-pipelines renderer))
        (luv:make-live-shader-pipeline
         :role role :vertex-role role :label (string-downcase role)
         :device (renderer-device renderer) :layout layout
         :vertex-buffers vertex-buffers
         :target-format format :target-blend blend
         :primitive `(:topology ,topology)
         :depth-stencil depth-stencil)))

(defun renderer-pipeline (renderer role)
  "The native pipeline of ROLE's live pipeline, for set-pipeline."
  (luv:live-shader-pipeline-native-pipeline (gethash role (renderer-pipelines renderer))))

(defun renderer-live-artifacts (renderer)
  (loop for pipeline being the hash-values of (renderer-pipelines renderer)
        collect pipeline))

(defun render-target (renderer name extent format &key (usage '(:render-attachment)))
  "The texture NAME of EXTENT (width height) and FORMAT and its view, remade
when either changes; the old ones go through luv's deferred destroy.
Returns the view and the texture."
  (let ((cached (gethash name (renderer-targets renderer))))
    (if (and cached (equal (first cached) extent) (eq (second cached) format))
        (values (fourth cached) (third cached))
        (let* ((device (renderer-device renderer))
               (texture (luv:create device (luv:make-texture-descriptor
                                            :label (string-downcase name) :size extent
                                            :dimensions :2d :format format
                                            :usage usage)))
               (view (luv:create device (luv:make-texture-view-descriptor
                                         :texture texture))))
          (when cached
            (luv:destroy (fourth cached))
            (luv:destroy (third cached)))
          (setf (gethash name (renderer-targets renderer))
                (list extent format texture view))
          (values view texture)))))

(defun depth-view (renderer extent)
  "The reversed-Z depth target's view (decision 4: Depth strategy)."
  (values (render-target renderer :depth extent :depth32-float)))

;; A view must outlive the submit; one per swapchain or capture texture.
(defun color-view (renderer texture)
  (or (gethash texture (renderer-color-views renderer))
      (setf (gethash texture (renderer-color-views renderer))
            (luv:create (renderer-device renderer)
                        (luv:make-texture-view-descriptor :texture texture)))))

(defun forget-color-view (renderer texture)
  "Destroy TEXTURE's cached view, before TEXTURE itself goes."
  (let ((view (gethash texture (renderer-color-views renderer))))
    (when view
      (luv:destroy view)
      (remhash texture (renderer-color-views renderer)))))

(defun texture-from-pixels (renderer pixels label)
  "Upload PIXELS, a rank-2 (unsigned-byte 32) array rows x columns of RGBA
bytes (R lowest), as an :rgba8-unorm-srgb texture. Returns the texture, its
view and a linear sampler repeating in u and clamped in v."
  (check-type pixels (array (unsigned-byte 32) (* *)))
  (destructuring-bind (height width) (array-dimensions pixels)
    (let* ((device (renderer-device renderer))
           (texture (gpu-create renderer (luv:make-texture-descriptor
                                          :label label :size (list width height)
                                          :dimensions :2d :format :rgba8-unorm-srgb
                                          :usage '(:copy-dst :texture-binding))))
           (view (gpu-create renderer (luv:make-texture-view-descriptor :texture texture)))
           (sampler (gpu-create renderer (luv:make-sampler-descriptor
                                          :label label
                                          :address-mode-u :repeat
                                          :address-mode-v :clamp-to-edge
                                          :mag-filter :linear :min-filter :linear))))
      (luv:write-texture (luv:device-queue device)
                         (luv:make-texture-copy :texture texture)
                         pixels
                         (luv:make-texture-data-layout :bytes-per-row (* width 4)
                                                       :rows-per-image height)
                         (list width height))
      (values texture view sampler))))

(defun destroy-renderer (renderer)
  "Release the pipelines and destroy what the renderer created; views of
foreign textures go with the device."
  (mapc #'luv:release-live-artifact (renderer-live-artifacts renderer))
  (clrhash (renderer-pipelines renderer))
  (loop for (nil nil texture view) being the hash-values of (renderer-targets renderer)
        do (luv:destroy view)
           (luv:destroy texture))
  (clrhash (renderer-targets renderer))
  (mapc #'luv:destroy (renderer-owned renderer))
  (setf (renderer-owned renderer) '())
  (clrhash (renderer-color-views renderer))
  (values))
