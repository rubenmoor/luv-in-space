(in-package #:luv-in-space)

;;; PLACEHOLDER(23): the walking skeleton's scene, a lit Earth placeholder
;;; against a few stars, with a dot-matrix label. Camera and depth follow the
;;; sphere probe (docs/luv-capabilities.md, "Appendix: probe program").

(defparameter *label* "PLACEHOLDER(23)")

(defparameter *background* '(0.0 0.0 0.004)
  "Linear RGB the frame is cleared to.")

(defparameter *eye* '(0d0 0.6d0 3.2d0))
(defparameter *target* '(0d0 0d0 0d0))
(defparameter *earth-center* '(0d0 0d0 0d0))
(defparameter *earth-radius* 1d0)
(defparameter *sun* '(-1d0 0.5d0 0.7d0))
(defparameter *near* 0.1d0)
(defparameter *field-of-view* 45d0 "Vertical, in degrees.")

;;; Vector helpers on lists of doubles.

(defun v- (a b) (mapcar #'- a b))
(defun dot3 (a b) (reduce #'+ (mapcar #'* a b)))
(defun cross (a b)
  (destructuring-bind ((ax ay az) (bx by bz)) (list a b)
    (list (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))))
(defun unit (a) (let ((l (sqrt (dot3 a a)))) (mapcar (lambda (x) (/ x l)) a)))

(defun single-floats (list)
  (map '(simple-array single-float (*)) (lambda (x) (coerce x 'single-float)) list))

;;; Meshes: pure functions, no GPU.

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

(defparameter *glyphs*
  '((#\P "####." "#...#" "#...#" "####." "#...." "#...." "#....")
    (#\L "#...." "#...." "#...." "#...." "#...." "#...." "#####")
    (#\A ".###." "#...#" "#...#" "#####" "#...#" "#...#" "#...#")
    (#\C ".###." "#...#" "#...." "#...." "#...." "#...#" ".###.")
    (#\E "#####" "#...." "#...." "####." "#...." "#...." "#####")
    (#\H "#...#" "#...#" "#...#" "#####" "#...#" "#...#" "#...#")
    (#\O ".###." "#...#" "#...#" "#...#" "#...#" "#...#" ".###.")
    (#\D "####." "#...#" "#...#" "#...#" "#...#" "#...#" "####.")
    (#\R "####." "#...#" "#...#" "####." "#.#.." "#..#." "#...#")
    (#\( "..#.." ".#..." "#...." "#...." "#...." ".#..." "..#..")
    (#\) "..#.." "...#." "....#" "....#" "....#" "...#." "..#..")
    (#\0 ".###." "#...#" "#..##" "#.#.#" "##..#" "#...#" ".###.")
    (#\1 "..#.." ".##.." "..#.." "..#.." "..#.." "..#.." ".###.")
    (#\2 ".###." "#...#" "....#" "...#." "..#.." ".#..." "#####")
    (#\3 "####." "....#" "....#" ".###." "....#" "....#" "####.")
    (#\4 "...#." "..##." ".#.#." "#..#." "#####" "...#." "...#.")
    (#\5 "#####" "#...." "####." "....#" "....#" "#...#" ".###.")
    (#\6 ".###." "#...." "#...." "####." "#...#" "#...#" ".###.")
    (#\7 "#####" "....#" "...#." "..#.." ".#..." ".#..." ".#...")
    (#\8 ".###." "#...#" "#...#" ".###." "#...#" "#...#" ".###.")
    (#\9 ".###." "#...#" "#...#" ".####" "....#" "....#" ".###."))
  "5x7 dot-matrix glyphs for placeholder labels; other characters are blank.")

(defun quad (anchor-x anchor-y left top size color)
  "Two triangles of overlay vertices: a SIZE-pixel square at pixel offset
LEFT, TOP from the clip-space anchor."
  (loop for (dx dy) in '((0 0) (1 0) (1 1) (0 0) (1 1) (0 1))
        append (list* anchor-x anchor-y (+ left (* dx size)) (+ top (* dy size)) color)))

(defun label-vertices (string &key (dot 3) (margin 16) (color '(1.0 0.75 0.2)))
  "Overlay vertices drawing STRING in dot-matrix glyphs at the top left."
  (loop for char across string
        for column from 0
        append (loop for row in (rest (assoc char *glyphs*))
                     for y from 0
                     append (loop for cell across row
                                  for x from 0
                                  when (char= cell #\#)
                                    append (quad -1.0 -1.0
                                                 (+ margin (* dot (+ x (* 6 column))))
                                                 (+ margin (* dot y))
                                                 dot color)))))

(defun star-vertices (&key (count 300) (seed 23))
  "Overlay vertices of COUNT faint stars at fixed random screen positions."
  (let ((state (sb-ext:seed-random-state seed)))
    (loop repeat count
          for brightness = (+ 0.15 (random 0.6 state))
          append (quad (- (random 2.0 state) 1.0) (- (random 2.0 state) 1.0)
                       -1 -1 (if (< (random 1.0 state) 0.1) 3 2)
                       (list brightness brightness (* 1.1 brightness))))))

(defun overlay-vertices ()
  "Stars, then the label; returns the vertex data and both vertex counts."
  (let ((stars (star-vertices)) (label (label-vertices *label*)))
    (values (single-floats (append stars label))
            (/ (length stars) 7) (/ (length label) 7))))

;;; The frame's uniforms.

(defun uniform-data (width height)
  "Clip rows for camera-relative points (reversed-Z, infinite far plane), the
Earth relative to the eye, the sun, and clip units per pixel."
  (let* ((forward (unit (v- *target* *eye*)))
         (right (unit (cross forward '(0d0 1d0 0d0))))
         (up (cross right forward))
         (focal (/ (tan (/ (* *field-of-view* (/ pi 180)) 2))))
         (aspect (/ width height)))
    (single-floats
     (append (mapcar (lambda (x) (* x (/ focal aspect))) right) '(0)
             ;; clip y points down on luv's Vulkan path
             (mapcar (lambda (x) (* x (- focal))) up) '(0)
             (list 0 0 0 *near*)
             forward '(0)
             (v- *earth-center* *eye*) (list *earth-radius*)
             (unit *sun*) '(0)
             (list (/ 2d0 width) (/ 2d0 height) 0 0)))))

;;; GPU resources.

(defclass scene ()
  ((device :initarg :device :reader scene-device)
   (earth-vertices :initarg :earth-vertices :reader scene-earth-vertices)
   (earth-count :initarg :earth-count :reader scene-earth-count)
   (overlay-vertices :initarg :overlay-vertices :reader scene-overlay-vertices)
   (star-count :initarg :star-count :reader scene-star-count)
   (label-count :initarg :label-count :reader scene-label-count)
   (uniforms :initarg :uniforms :reader scene-uniforms)
   (bind-group :initarg :bind-group :reader scene-bind-group)
   (earth-pipeline :initarg :earth-pipeline :reader scene-earth-pipeline)
   (overlay-pipeline :initarg :overlay-pipeline :reader scene-overlay-pipeline)
   (modules :initarg :modules :reader scene-modules)
   (depth-target :initform nil :accessor scene-depth-target)
   (color-views :initform (make-hash-table :test #'eq) :reader scene-color-views)))

(defun shader-module (device code)
  (luv:create device (luv:make-shader-module-descriptor :language :mathematical :code code)))

(defun vertex-buffer (device data label)
  (let ((buffer (luv:create device (luv:make-buffer-descriptor
                                    :label label :size (* 4 (length data))
                                    :usage '(:vertex :copy-dst)))))
    (luv:write-buffer buffer data)
    buffer))

(defun make-scene (device format)
  "The scene's buffers and pipelines on DEVICE, drawing into FORMAT."
  (let* ((earth (uv-sphere))
         (layout (luv:create device (luv:make-bind-group-layout-descriptor
                                     :entries '((:binding 0 :type :uniform-buffer)))))
         (uniforms (luv:create device (luv:make-buffer-descriptor
                                       :label "frame" :size (* 4 28) :usage '(:uniform))))
         (targets `((:format ,format)))
         (modules (mapcar (lambda (code) (shader-module device code))
                          (list (luv-in-space.shaders::earth-vertex)
                                (luv-in-space.shaders::earth-fragment)
                                (luv-in-space.shaders::overlay-vertex)
                                (luv-in-space.shaders::overlay-fragment)))))
    (multiple-value-bind (overlay star-count label-count) (overlay-vertices)
      (make-instance
       'scene
       :device device
       :earth-vertices (vertex-buffer device earth "earth")
       :earth-count (/ (length earth) 3)
       :overlay-vertices (vertex-buffer device overlay "overlay")
       :star-count star-count :label-count label-count
       :uniforms uniforms :modules modules
       :bind-group (luv:create device (luv:make-bind-group-descriptor
                                       :layout layout
                                       :entries `((:binding 0 :resource ,uniforms))))
       :earth-pipeline
       (luv:create device
                   (luv:make-render-pipeline-descriptor
                    :label "earth" :layout layout
                    :vertex `(:module ,(nth 0 modules)
                              :buffers ((:array-stride 12
                                         :attributes ((:shader-location 0 :offset 0
                                                       :format :float32x3)))))
                    :fragment `(:module ,(nth 1 modules)
                                :targets ,targets)
                    :primitive '(:topology :triangle-list)
                    ;; decision 4: Depth strategy
                    :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                     :depth-compare :greater)))
       :overlay-pipeline
       (luv:create device
                   (luv:make-render-pipeline-descriptor
                    :label "overlay" :layout layout
                    :vertex `(:module ,(nth 2 modules)
                              :buffers ((:array-stride 28
                                         :attributes ((:shader-location 0 :offset 0
                                                       :format :float32x2)
                                                      (:shader-location 1 :offset 8
                                                       :format :float32x2)
                                                      (:shader-location 2 :offset 16
                                                       :format :float32x3)))))
                    :fragment `(:module ,(nth 3 modules)
                                :targets ,targets)
                    :primitive '(:topology :triangle-list)
                    :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                     :depth-compare :always)))))))

(defun destroy-scene (scene)
  "Destroy what the device would otherwise reclaim only by garbage collection."
  (mapc #'luv:destroy (scene-modules scene)))

(defun depth-view (scene extent)
  "A depth32-float view of EXTENT, remade when the size changes."
  (let ((cached (scene-depth-target scene)))
    (if (and cached (equal (first cached) extent))
        (third cached)
        (let* ((device (scene-device scene))
               (texture (luv:create device (luv:make-texture-descriptor
                                            :label "depth" :size extent
                                            :dimensions :2d :format :depth32-float
                                            :usage '(:render-attachment))))
               (view (luv:create device (luv:make-texture-view-descriptor
                                         :texture texture))))
          ;; The old target may still be in flight; the skeleton leaks it.
          (setf (scene-depth-target scene) (list extent texture view))
          view))))

;; A view must outlive the submit; one per swapchain or capture texture.
(defun color-view (scene texture)
  (or (gethash texture (scene-color-views scene))
      (setf (gethash texture (scene-color-views scene))
            (luv:create (scene-device scene)
                        (luv:make-texture-view-descriptor :texture texture)))))

(defun encode-scene (scene encoder target extent)
  "Draw stars, Earth and the label into TARGET of EXTENT (width height)."
  (luv:write-buffer (scene-uniforms scene) (uniform-data (first extent) (second extent)))
  (let ((pass (luv:begin-render-pass
               encoder
               (luv:make-render-pass-descriptor
                :color-attachments
                `((:view ,(color-view scene target) :load-op :clear :store-op :store
                   :clear-value ,(coerce (append *background* '(1.0)) 'vector)))
                :depth-stencil-attachment
                `(:view ,(depth-view scene extent)
                  :depth-load-op :clear :depth-store-op :store
                  :depth-clear-value 0.0)))))
    (flet ((overlay (first count)
             (luv:set-pipeline pass (scene-overlay-pipeline scene))
             (luv:set-bind-group pass 0 (scene-bind-group scene))
             (luv:set-vertex-buffer pass 0 (scene-overlay-vertices scene))
             (luv:draw pass count 1 first)))
      (overlay 0 (scene-star-count scene))
      (luv:set-pipeline pass (scene-earth-pipeline scene))
      (luv:set-bind-group pass 0 (scene-bind-group scene))
      (luv:set-vertex-buffer pass 0 (scene-earth-vertices scene))
      (luv:draw pass (scene-earth-count scene))
      (overlay (scene-star-count scene) (scene-label-count scene)))
    (luv:end-pass pass)))
