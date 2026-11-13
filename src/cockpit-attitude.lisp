(in-package #:luv-in-space)

;;; The ATTITUDE screen's plot (plan 32: Cockpit instruments, buttons, the
;;; grapple camera and messages; decision 127: Cockpit screens' contents in
;;; milestone 3: lines, modes, plots, colours and updates): the whole sky in
;;; body axes on a disc of 0.150 m, azimuthal equidistant about the nose (+x),
;;; -z up, the rim the tail. The :cab-attitude shader fills the dominant
;;; body's disc and draws the rings; here the markers (glyphs of the text
;;; atlas), the hold's cross, the prograde tick and the rate bars, written
;;; every frame into preallocated regions, consing nothing. The screen's text
;;; (the bars' letters, the mode line) is cockpit-screens.lisp's attitude-lines.

(defparameter *attitude-plot* '(:a -0.035d0 :b 0.015d0 :radius 0.075d0)
  "The plot's centre (right, up) from the face's centre and its radius, metres:
the face's left 0.15 m under 10 mm margins, rows 1 to 7.")

(defparameter *attitude-bars* '(:columns (13 15 17) :zero 0.03d0 :half 0.05d0 :width 0.005d0)
  "The rate bars P Y R over the grid's columns 13, 15 and 17: the zero line's
height, the half length of full scale and the bar's width, metres.")

(defparameter *attitude-full-scale* 5d0 "degrees a second at a bar's end.")
(defparameter *attitude-quiet-rate* 0.01d0 "rad/s: a bar within it is green (the warp limit).")
(defparameter *attitude-body-fill* #x24303A "The body's disc: a dark blue-grey.")
(defparameter *attitude-tick-drift* 0.05d0 "rad: how far the prograde marker's tick looks ahead.")

(declaim (inline attitude-plot-point))
(defun attitude-plot-point (x y z)
  "(values px py): the direction (X Y Z) in body axes (any length) on the plot's
unit disc, px to the right, py up. The tail (-x) is on the rim, up."
  (declare (type double-float x y z))
  (let ((s (sqrt (+ (* y y) (* z z)))))
    (declare (type (double-float 0d0) s))
    (if (< s 1d-14)
        (values 0d0 (if (< x 0d0) 1d0 0d0))
        (let ((rho (/ (atan s x) pi)))
          (declare (type double-float rho))
          (values (/ (* rho y) s) (/ (* rho (- z)) s))))))

(defun attitude-plot-direction (px py)
  "(values x y z): the unit body direction of the plot's point (PX, PY), the
inverse of attitude-plot-point."
  (declare (type double-float px py))
  (let ((rho (sqrt (+ (* px px) (* py py)))))
    (if (< rho 1d-14)
        (values 1d0 0d0 0d0)
        (let* ((theta (* pi rho)) (k (/ (sin theta) rho)))
          (values (cos theta) (* k px) (- (* k py)))))))

(defstruct (attitude-view (:copier nil) (:constructor %make-attitude-view))
  (cap 0f0 :type single-float)
  (markers nil)                          ; the text-region of the seven markers
  (lines nil)                            ; the line-region of bars, cross and tick
  (batch nil) (record 0 :type fixnum)    ; the plot-batch and its record's index
  (plane nil)                            ; the markers' text-plane, at the plot's centre
  (frame nil)                            ; the lines' plane-frame, 2 mm in front of the face
  (qinv (make-quat) :type quat) (hold-q (make-quat) :type quat) (hold-w (make-vec3) :type vec3)
  (p (make-vec3) :type vec3) (n (make-vec3) :type vec3) (o (make-vec3) :type vec3)
  (body (make-vec3) :type vec3) (inertial (make-vec3) :type vec3)
  (bar-x (make-array 3 :element-type 'single-float :initial-element 0f0)
   :type (simple-array single-float (3))) ; the bars' places, metres right of the face's centre
  (px 0f0 :type single-float) (py 0f0 :type single-float) ; the last marker's plot point, metres
  (white nil) (grey nil) (green nil) (amber nil) (cyan nil) (magenta nil)) ; lines' colours

(defun make-attitude-view (text-batch line-batch plot-batch)
  "The screen's regions in the batches, the plot's static record written."
  (let* ((panel (cockpit-panel :attitude))
         (cap (panel-grid-cap panel))
         (record (plot-batch-take! plot-batch))
         (data (plot-batch-data plot-batch))
         (base (* +plot-record-floats+ record))
         (a (getf *attitude-plot* :a)) (b (getf *attitude-plot* :b)) (radius (getf *attitude-plot* :radius))
         (fill (screen-line-colour *attitude-body-fill*))
         (grey (screen-line-colour *screen-grey*)))
    (multiple-value-bind (c n r u) (panel-frame panel)
      (flet ((put (index &rest values)
               (loop for v in values for i from (+ base index)
                     do (setf (aref data i) (float v 1f0))))
             (at (depth) (vlin c a r b u depth n)))
        (let ((centre (at 0.001d0)))
          (put 0 (aref centre 0) (aref centre 1) (aref centre 2) radius)
          (put 4 (aref r 0) (aref r 1) (aref r 2) 0d0)
          (put 8 (aref u 0) (aref u 1) (aref u 2) 0d0)
          (put 12 0d0 0d0 1d0 0d0)
          (put 16 (aref fill 0) (aref fill 1) (aref fill 2) 0d0)
          (put 20 (aref grey 0) (aref grey 1) (aref grey 2) 0d0))
        (let ((origin (at 0.003d0))
              (bars (make-array 3 :element-type 'single-float)))
          (loop for column in (getf *attitude-bars* :columns) for k from 0
                do (setf (aref bars k)
                         (float (+ (* -0.5d0 (first (getf panel :size))) 0.01d0
                                   (* (+ column 0.5d0) +text-cell-width+ cap))
                                1f0)))
          (%make-attitude-view
           :bar-x bars
           :cap (float cap 1f0)
           :markers (text-batch-region text-batch 7)
           :lines (line-batch-region line-batch 12)
           :batch plot-batch :record record
           :plane (make-text-plane :origin (coerce origin 'list) :right (coerce r 'list)
                                   :up (coerce u 'list))
           :frame (panel-plane-frame :attitude 0d0 0d0 0.002d0)
           :white (screen-line-colour *screen-white*) :grey grey
           :green (screen-line-colour *screen-green*) :amber (screen-line-colour *screen-amber*)
           :cyan (screen-line-colour *screen-cyan*) :magenta (screen-line-colour *screen-magenta*)))))))

;;; Per frame

(defconstant +attitude-radius+ 0.075f0 "The plot's radius, metres (*attitude-plot*).")

(declaim (inline attitude-plot-metres!))
(defun attitude-plot-metres! (view)
  "The body direction in VIEW's scratch on the plot, in metres from its centre,
into the view's px and py."
  (declare (type attitude-view view) (optimize speed (safety 0)))
  (let ((b (attitude-view-body view)))
    (multiple-value-bind (x y) (attitude-plot-point (aref b 0) (aref b 1) (aref b 2))
      (declare (type double-float x y))
      (setf (attitude-view-px view) (* +attitude-radius+ (float x 1f0))
            (attitude-view-py view) (* +attitude-radius+ (float y 1f0))))))

(defun attitude-marker! (view atlas index char sign v)
  "Marker INDEX: CHAR at the plot point of SIGN times the inertial direction V."
  (declare (type attitude-view view) (type fixnum index) (type vec3 v) (type double-float sign))
  (let ((inertial (attitude-view-inertial view)))
    (vec3-scale! inertial v sign)
    (quat-rotate! (attitude-view-body view) (attitude-view-qinv view) inertial)
    (attitude-plot-metres! view)
    (marker-run! atlas (attitude-view-markers view) index (attitude-view-plane view) char
                 (attitude-view-px view) (attitude-view-py view)
                 :cap (attitude-view-cap view) :colour (screen-ink *screen-white*))))

(defun attitude-line! (view index a0 b0 a1 b1 colour width)
  "Segment INDEX of the lines, in the face's right and up metres."
  (declare (type attitude-view view) (type fixnum index) (type single-float a0 b0 a1 b1 width))
  (plane-put! (attitude-view-lines view) index (attitude-view-frame view)
              a0 b0 0f0 a1 b1 0f0 colour width))

(defun attitude-line-clear! (view index)
  (declare (type attitude-view view) (type fixnum index))
  (attitude-line! view index 0f0 0f0 0f0 0f0 (attitude-view-grey view) 0f0))

(defun attitude-bars! (view state)
  "The three rate bars (P Y R: ω about y, z and x), their tracks and zero ticks."
  (declare (type attitude-view view) (type render-state state) (optimize speed))
  (let* ((w (render-state-w state))
         (xs (attitude-view-bar-x view))
         (zero (load-time-value (float (getf *attitude-bars* :zero) 1f0)))
         (half (load-time-value (float (getf *attitude-bars* :half) 1f0)))
         (width (load-time-value (float (getf *attitude-bars* :width) 1f0)))
         (grey (attitude-view-grey view))
         (quiet (load-time-value (float *attitude-quiet-rate* 1f0))))
    (declare (type single-float zero half width quiet))
    (dotimes (index 3)
      (let* ((x (aref xs index))
             (rate (float (aref w (case index (0 1) (1 2) (t 0))) 1f0))
             (length (* half (max -1f0 (min 1f0 (/ (* rate 57.29578f0) 5f0)))))
             (colour (if (<= (abs rate) quiet) (attitude-view-green view) (attitude-view-amber view))))
        (declare (type single-float x rate length))
        (attitude-line! view index x (- zero half) x (+ zero half) grey 0.001f0)
        (attitude-line! view (+ 3 index) (- x 0.006f0) zero (+ x 0.006f0) zero grey 0.0015f0)
        (attitude-line! view (+ 6 index) x zero x (+ zero length) colour width)))))

(defun attitude-hold-cross! (view state)
  "The hold's direction as a cyan cross, cleared without a hold."
  (declare (type attitude-view view) (type render-state state))
  (let ((controls (render-state-controls state)))
    (if (eq (controls-attitude-mode controls) :hold)
        (let ((q (attitude-view-hold-q view)) (inertial (attitude-view-inertial view))
              (body (attitude-view-body view)) (x (make-vec3)))
          (declare (dynamic-extent x))
          (hold-attitude-into! q (attitude-view-hold-w view) (controls-hold controls)
                               (render-state-r state) (render-state-v state)
                               (controls-roll-offset controls) (controls-inertial-target controls)
                               (controls-target-state controls) (controls-target-attitude controls)
                               (- (render-state-t state) (aref (controls-target-state controls) 9)))
          (vec3-set! x 1d0 0d0 0d0)
          (quat-rotate! inertial q x)
          (quat-rotate! body (attitude-view-qinv view) inertial)
          (attitude-plot-metres! view)
          (let ((px (+ (float (getf *attitude-plot* :a) 1f0) (attitude-view-px view)))
                (py (+ (float (getf *attitude-plot* :b) 1f0) (attitude-view-py view)))
                (arm 0.007f0) (cyan (attitude-view-cyan view)))
            (attitude-line! view 9 (- px arm) py (+ px arm) py cyan 0.0022f0)
            (attitude-line! view 10 px (- py arm) px (+ py arm) cyan 0.0022f0)))
        (progn (attitude-line-clear! view 9) (attitude-line-clear! view 10)))))

(defun attitude-tick! (view state)
  "A short tick beside the prograde marker towards where gravity turns the velocity."
  (declare (type attitude-view view) (type render-state state))
  (let* ((p (attitude-view-p view)) (n (attitude-view-n view))
         (r (render-state-r state)) (inertial (attitude-view-inertial view))
         (body (attitude-view-body view)) (qinv (attitude-view-qinv view))
         (a (make-vec3)))
    (declare (dynamic-extent a))
    ;; the velocity direction P turns towards -R: the part of -R across P
    (vec3-scale! a r -1d0)
    (vec3-scale! n p (vec3-dot a p))
    (vec3-sub! a a n)
    (let ((norm (vec3-norm a)))
      (if (< norm (* 1d-9 (vec3-norm r)))
          (attitude-line-clear! view 11)
          (progn
            (quat-rotate! body qinv p)
            (attitude-plot-metres! view)
            (let ((x0 (attitude-view-px view)) (y0 (attitude-view-py view)))
              (vec3-scale! a a (/ *attitude-tick-drift* norm))
              (vec3-add! inertial p a)
              (quat-rotate! body qinv inertial)
              (attitude-plot-metres! view)
              (let* ((dx (- (attitude-view-px view) x0)) (dy (- (attitude-view-py view) y0))
                     (length (sqrt (+ (* dx dx) (* dy dy)))))
                (declare (type single-float x0 y0 dx dy length))
                (if (< length 1f-7)
                    (attitude-line-clear! view 11)
                    (let ((ux (/ dx length)) (uy (/ dy length))
                          (a0 (float (getf *attitude-plot* :a) 1f0))
                          (b0 (float (getf *attitude-plot* :b) 1f0)))
                      (attitude-line! view 11
                                      (+ a0 x0 (* 0.0065f0 ux)) (+ b0 y0 (* 0.0065f0 uy))
                                      (+ a0 x0 (* 0.0115f0 ux)) (+ b0 y0 (* 0.0115f0 uy))
                                      (attitude-view-white view) 0.0015f0))))))))))

(defun attitude-view-update! (view atlas state)
  "The plot's nadir, the markers, the cross, the tick and the bars for the render
STATE; every frame, consing nothing."
  (declare (type attitude-view view) (type render-state state))
  (let* ((r (render-state-r state)) (v (render-state-v state))
         (qinv (attitude-view-qinv view))
         (p (attitude-view-p view)) (n (attitude-view-n view)) (o (attitude-view-o view))
         (distance (vec3-norm r))
         (data (plot-batch-data (attitude-view-batch view)))
         (base (* +plot-record-floats+ (attitude-view-record view)))
         (inertial (attitude-view-inertial view)) (body (attitude-view-body view)))
    (declare (type fixnum base) (type double-float distance))
    (quat-conjugate! qinv (render-state-q state))
    (vec3-normalize! p v)
    (vec3-normalize! n (vec3-cross! n r v))
    (vec3-cross! o p n)
    ;; The body's disc about the nadir: cos asin(R / r).
    (vec3-scale! inertial r (/ -1d0 distance))
    (quat-rotate! body qinv inertial)
    (let ((ratio (min 1d0 (/ +radius-earth+ distance))))
      (setf (aref data (+ base 12)) (float (aref body 0) 1f0)
            (aref data (+ base 13)) (float (aref body 1) 1f0)
            (aref data (+ base 14)) (float (aref body 2) 1f0)
            (aref data (+ base 15)) (float (sqrt (- 1d0 (* ratio ratio))) 1f0)))
    (attitude-marker! view atlas 0 #\○ 1d0 p)
    (attitude-marker! view atlas 1 #\⊗ -1d0 p)
    (attitude-marker! view atlas 2 #\▲ 1d0 n)
    (attitude-marker! view atlas 3 #\▼ -1d0 n)
    (attitude-marker! view atlas 4 #\■ 1d0 o)
    (attitude-marker! view atlas 5 #\□ -1d0 o)
    ;; The target ◆ where target-relative-into! puts its body direction.
    (let ((markers (attitude-view-markers view)))
      (if (and (render-state-target state) (> (aref (render-state-target-relative state) 0) 0d0))
          (let ((relative (render-state-target-relative state)))
            (vec3-set! body (aref relative 5) (aref relative 6) (aref relative 7))
            (attitude-plot-metres! view)
            (marker-run! atlas markers 6 (attitude-view-plane view) #\◆
                         (attitude-view-px view) (attitude-view-py view)
                         :cap (attitude-view-cap view) :colour (screen-ink *screen-magenta*)))
          (let ((data (text-batch-data (text-region-batch markers)))
                (start (* +text-record-floats+ (+ (text-region-start markers) 6))))
            (fill data 0f0 :start start :end (+ start +text-record-floats+)))))
    (attitude-hold-cross! view state)
    (attitude-tick! view state)
    (attitude-bars! view state)
    view))
