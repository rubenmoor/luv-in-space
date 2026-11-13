(in-package #:luv-in-space)

;;; The grapple camera on the TARGET screen (plan 32: Cockpit instruments,
;;; buttons, the grapple camera and messages, piece 7; decision 128: Cockpit
;;; devices, buttons, picking, the grapple camera and messages in milestone 3;
;;; decision 127: Cockpit screens' contents in milestone 3: lines, modes,
;;; plots, colours and updates): while the grapple is armed, closing or
;;; latched the TARGET face shows the view from the cone's tip (render.lisp's
;;; grapple pass), a crosshair and the fixture's pin as a magenta ◆ over it,
;;; and its lines on opaque bands. The camera's pose, the pin's projection,
;;; the armed lines and the crosshair's regions are here.

(defparameter *grapple-crosshair* '(:gap 0.1d0 :width 0.0013d0 :depth 0.0015d0)
  "The crosshair's gap at the middle (the share of the face's size), the
lines' width and their height over the face, metres.")

(defun grapple-camera-update! (camera state)
  "Pose CAMERA at the cone's tip: offset q (tip - c) from R (c the stack's CoM,
as the cockpit's camera), oriented q, 50 degrees vertical, near 0.1 m.
Conses nothing. CAMERA."
  (declare (optimize speed) (type camera camera) (type render-state state))
  (let ((q (render-state-q state)) (d (make-vec3)))
    (declare (dynamic-extent d))
    (vec3-sub! d *grapple-tip* (render-state-com state))
    (quat-rotate! (camera-offset camera) q d)
    (replace (camera-orientation camera) q)
    (setf (camera-anchor camera) :ship
          (camera-field-of-view camera) *grapple-field-of-view*
          (camera-near camera) *grapple-near*)
    camera))

(declaim (inline grapple-pin-pixel))
(defun grapple-pin-pixel (reading &optional (extent *grapple-extent*)
                                            (field-of-view *grapple-field-of-view*))
  "(values px py): the pin of READING's fixture in the grapple image of EXTENT
pixels, from the top left, as the camera's clip rows put it (x = (f / aspect)
y / depth, y = f z / depth, f = 1 / tan(fov / 2)); nil when it is not ahead."
  (declare (type grapple-reading reading) (type (cons fixnum (cons fixnum null)) extent)
           (type double-float field-of-view) (optimize speed))
  (let ((depth (grapple-reading-offset-x reading)))
    (when (> depth 1d-6)
      (let* ((width (float (the fixnum (first extent)) 1d0))
             (height (float (the fixnum (second extent)) 1d0))
             (f (/ (tan (* 0.5d0 field-of-view))))
             (nx (/ (* (/ f (/ width height)) (grapple-reading-offset-y reading)) depth))
             (ny (/ (* (the double-float +clip-y-sign+) f (grapple-reading-offset-z reading)) depth)))
        (declare (type double-float width height f nx ny))
        (values (* 0.5d0 (+ nx 1d0) width) (* 0.5d0 (+ ny 1d0) height))))))

(defun grapple-active-p (state)
  "Whether the grapple is armed, closing or latched."
  (not (eq (render-grapple-state (render-state-grapple state)) :stowed)))

(defun grapple-reading-of (state)
  "A grapple-reading of the nearest fixture ahead of the tip within the range
finder's reach for the render STATE's sim, or nil."
  (let ((sim (render-state-sim state)))
    (when sim
      (let ((reading (make-grapple-reading)))
        (and (grapple-range-into! reading sim) reading)))))

;;; TARGET's armed lines (decision 127): rows 1 to 3 and 8 on opaque bands.

(defun band-line (&rest parts)
  "A line of PARTS (screen-line's) on an opaque band across the face."
  (cons (screen-run "" *screen-black* :band) (apply #'screen-line parts)))

(defun flagged-parts (ok &rest parts)
  "PARTS (colour text ...) in green when OK, else amber with a \"!\" after."
  (let ((colour (if ok :green :amber)))
    (append (loop for (nil text) on parts by #'cddr append (list colour text))
            (unless ok (list :amber "!")))))

(defun grapple-armed-lines (state &optional (reading (and (eq (render-grapple-state (render-state-grapple state)) :armed)
                                                          (grapple-reading-of state))))
  "The armed screen's eight lines (lists of runs, empty ones for the image) for
READING (the nearest fixture's, nil without one)."
  (let* ((grapple (render-state-grapple state))
         (name (lambda (def) (if def (object-def-name def) ""))))
    (list
     (when reading
       (apply #'band-line
              :grey "RNG " :white (format nil "~A m" (format-fixed (grapple-reading-range reading) 1))
              :grey " CLS "
              (flagged-parts (grapple-reading-closing-ok reading)
                             :x (format nil "~A m/s" (format-fixed (grapple-reading-closing-speed reading) 2)))))
     (when reading
       (apply #'band-line
              :grey "OFF "
              (flagged-parts (grapple-reading-offset-ok reading)
                             :x (format nil "Y~A Z~A"
                                        (signed-fixed (grapple-reading-offset-y reading) 2)
                                        (signed-fixed (grapple-reading-offset-z reading) 2)))))
     (when reading
       (apply #'band-line
              :grey "ANG "
              (append (flagged-parts (grapple-reading-angle-ok reading)
                                     :x (format nil "~A°" (format-fixed (rad->deg (grapple-reading-angle reading)) 1)))
                      (list :grey " ROT ")
                      (flagged-parts (grapple-reading-rotation-ok reading)
                                     :x (format nil "~A°/s" (format-fixed (rad->deg (grapple-reading-rotation reading)) 1))))))
     '() '() '() '()
     (ecase (render-grapple-state grapple)
       (:armed (if reading (band-line :white "ARMED") (band-line :amber "NO FIXTURE IN 500 m")))
       (:closing (band-line :white "CLOSING"))
       (:latched (if (render-state-berth-def state)
                     (band-line :green (format nil "BERTHED ~A" (funcall name (render-state-berth-def state))))
                     (band-line :green (format nil "LATCHED ~A" (funcall name (render-state-load-def state))))))))))

;;; The TARGET face's image quad: static, 1 mm in front of the face.

(defun screen-image-quad ()
  "The face's centre and half edges as three vec4s (singles), body axes from the eye."
  (multiple-value-bind (c n r u) (panel-frame (cockpit-panel :target))
    (destructuring-bind (w h) (getf (cockpit-panel :target) :size)
      (let ((centre (vlin c 0.001d0 n)))
        (coerce (loop for v in (list centre (vec3-scale r (* 0.5d0 w)) (vec3-scale u (* 0.5d0 h)))
                      append (list (float (aref v 0) 1f0) (float (aref v 1) 1f0) (float (aref v 2) 1f0) 0f0))
                '(simple-array single-float (*)))))))

;;; The crosshair and the pin's mark, every frame.

(defstruct (grapple-view (:copier nil) (:constructor %make-grapple-view))
  (cap 0f0 :type single-float)
  (lines nil)                           ; four crosshair segments
  (pin nil)                             ; the text-region of the pin's ◆
  (plane nil) (frame nil)
  (reading (make-grapple-reading) :type grapple-reading) ; scratch: the nearest fixture's
  (width 0f0 :type single-float) (height 0f0 :type single-float) ; the face, metres
  (white nil) (magenta nil))

(defun make-grapple-view (text-batch line-batch)
  (let* ((panel (cockpit-panel :target))
         (cap (panel-grid-cap panel)))
    (multiple-value-bind (c n r u) (panel-frame panel)
      (destructuring-bind (w h) (getf panel :size)
        (%make-grapple-view
         :cap (float cap 1f0) :width (float w 1f0) :height (float h 1f0)
         :lines (line-batch-region line-batch 4)
         :pin (text-batch-region text-batch 1)
         :plane (make-text-plane :origin (coerce (vlin c 0.003d0 n) 'list)
                                 :right (coerce r 'list) :up (coerce u 'list))
         :frame (panel-plane-frame :target 0d0 0d0 (getf *grapple-crosshair* :depth))
         :white (screen-line-colour *screen-white*)
         :magenta (screen-ink *screen-magenta*))))))

(defun grapple-view-clear! (view)
  (line-region-clear! (grapple-view-lines view))
  (text-region-clear! (grapple-view-pin view))
  view)

(defun grapple-view-update! (view atlas state)
  "The crosshair (2 px lines through the middle, a gap of 10 %) and the pin's ◆
for the render STATE; cleared while the grapple is stowed. Conses nothing."
  (declare (type grapple-view view) (type render-state state))
  (if (not (grapple-active-p state))
      (grapple-view-clear! view)
      (let* ((w (grapple-view-width view)) (h (grapple-view-height view))
             (gap (float (getf *grapple-crosshair* :gap) 1f0))
             (width (float (getf *grapple-crosshair* :width) 1f0))
             (lines (grapple-view-lines view)) (frame (grapple-view-frame view))
             (white (grapple-view-white view))
             (a (* 0.5f0 w)) (b (* 0.5f0 h)) (ga (* 0.5f0 gap w)) (gb (* 0.5f0 gap h)))
        (declare (type single-float w h gap width a b ga gb))
        (plane-put! lines 0 frame (- a) 0f0 0f0 (- ga) 0f0 0f0 white width)
        (plane-put! lines 1 frame ga 0f0 0f0 a 0f0 0f0 white width)
        (plane-put! lines 2 frame 0f0 (- b) 0f0 0f0 (- gb) 0f0 white width)
        (plane-put! lines 3 frame 0f0 gb 0f0 0f0 b 0f0 white width)
        (let ((reading (and (eq (render-grapple-state (render-state-grapple state)) :armed)
                            (render-state-sim state)
                            (grapple-range-into! (grapple-view-reading view) (render-state-sim state))
                            (grapple-view-reading view))))
          (multiple-value-bind (px py) (if reading (grapple-pin-pixel reading) nil)
            (if (and px (<= 0d0 px (first *grapple-extent*)) (<= 0d0 py (second *grapple-extent*)))
                (marker-run! atlas (grapple-view-pin view) 0 (grapple-view-plane view) #\◆
                             (float (* (- (/ px (first *grapple-extent*)) 0.5d0) w) 1f0)
                             (float (* (- 0.5d0 (/ py (second *grapple-extent*))) h) 1f0)
                             :cap (grapple-view-cap view) :colour (grapple-view-magenta view))
                (text-region-clear! (grapple-view-pin view)))))))
  view)
