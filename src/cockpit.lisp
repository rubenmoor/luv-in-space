(in-package #:luv-in-space)

;;; The DIY cockpit's structure (docs/design.md "The cockpit", "The DIY look";
;;; decision 126: Cockpit structure and view in milestone 3: frames, draw
;;; order, Slug text, head look, exposure, cab light, portholes, bezels, tape
;;; labels; decision 4: Depth strategy; decision 11: Render pipelines are luv
;;; live shader pipelines; decision 76: Frame passes, radiance scale and
;;; exposure). So far the cab's shell, fittings and light: the lining, the
;;; walls, the ring frames and stringers, the tread-plate deck, the ECLSS rack,
;;; the net with its bags, the extinguisher, the hatch and its handwheel, the
;;; folded jump seat, the headrest edges and the two LED strips, and the
;;; openings the window and portholes cut. Window frame, glass, plate, the
;;; console, cables and labels come with the following pieces of plan 31.
;;;
;;; Everything is in ship body axes (FRD) in metres from the eye point
;;; (*cockpit-eye*), single floats, so no world position reaches a cockpit
;;; shader and the cab keeps its exactness (docs/architecture.md "Rendering
;;; precision"). The draws use the cab block (shaders.lisp): the clip rows of
;;; the head camera in body axes, so cab and scene agree in depth.

;;; Materials: linear albedo, specular weight, exponent, emission (the rows
;;; the cab fragment names, *cab-shader-material-rows*).

(defparameter *cab-material-names*
  '(:lining :steel-angle :tread :eclss :red :tape :marker :black-plastic :grey-plastic
    :green-surplus :beige :aluminium :screen-face :cable-black :cable-grey :cable-red
    :cable-yellow :kapton :cardboard :sheet-metal :led :fabric :seat :hatch :plate)
  "Row order of *cab-materials*; a vertex's material id is the row.")

(defparameter *cab-materials*
  (let ((rows '((0.75 0.75 0.75 0.05 8.0 0.0)      ; lining, off-white
                (0.45 0.45 0.47 0.15 16.0 0.0)     ; steel angle
                (0.40 0.40 0.41 0.15 16.0 0.0)     ; tread plate
                (0.50 0.50 0.52 0.10 16.0 0.0)     ; ECLSS grey
                (0.50 0.06 0.04 0.05 8.0 0.0)      ; red
                (0.12 0.12 0.12 0.05 8.0 0.0)      ; gaffer tape
                (0.85 0.85 0.85 0.05 8.0 0.0)      ; marker
                (0.04 0.04 0.04 0.10 16.0 0.0)     ; bezel: black plastic
                (0.30 0.30 0.30 0.10 16.0 0.0)     ; bezel: grey
                (0.10 0.14 0.08 0.10 16.0 0.0)     ; bezel: military green
                (0.62 0.57 0.46 0.10 16.0 0.0)     ; bezel: beige
                (0.55 0.55 0.57 0.30 32.0 0.0)     ; bezel: aluminium
                (0.02 0.02 0.02 0.80 128.0 0.0)    ; screen face, glossy
                (0.03 0.03 0.03 0.10 16.0 0.0)     ; cable, black
                (0.25 0.25 0.25 0.10 16.0 0.0)     ; cable, grey
                (0.50 0.06 0.04 0.10 16.0 0.0)     ; cable, red
                (0.70 0.55 0.05 0.10 16.0 0.0)     ; cable, yellow
                (0.60 0.35 0.05 0.10 16.0 0.0)     ; Kapton amber
                (0.45 0.35 0.22 0.05 8.0 0.0)      ; cardboard
                (0.50 0.50 0.52 0.15 16.0 0.0)     ; sheet metal
                (1.00 0.85 0.65 0.00 8.0 0.5)      ; LED strip face, emissive
                (0.30 0.32 0.22 0.05 8.0 0.0)      ; net, bags
                (0.12 0.12 0.14 0.05 8.0 0.0)      ; seat
                (0.70 0.70 0.68 0.05 8.0 0.0)      ; hatch paint
                (0.40 0.40 0.42 0.15 16.0 0.0))))  ; name plate, steel
    (make-array (list (length rows) 6) :element-type 'single-float :initial-contents rows))
  "Rows of *cab-material-names*: r g b, specular weight, exponent, emission; live.")

(defun cab-material-id (name)
  (or (position name *cab-material-names*) (error "No cab material ~S." name)))

(defun cab-materials! (out)
  "*cab-materials* as two vec4s per row: r g b weight; exponent emission 0 0."
  (dotimes (row (array-dimension *cab-materials* 0) out)
    (let ((o (* 8 row)))
      (dotimes (k 4)
        (setf (aref out (+ o k)) (aref *cab-materials* row k)))
      (setf (aref out (+ o 4)) (aref *cab-materials* row 4)
            (aref out (+ o 5)) (aref *cab-materials* row 5)
            (aref out (+ o 6)) 0f0 (aref out (+ o 7)) 0f0))))

;;; Dimensions (decision 126; docs/design.md "Layout": the front wall at
;;; dx 0.90, the cab's axis at dz +0.10, the deck at dz +1.15).

(defparameter *cab-lining-radius* 1.40d0 "m: the lining's inner radius.")
(defparameter *cab-x-range* '(-3.55d0 0.90d0) "m from the eye: the aft and front walls (x 10.55 to 15.0).")
(defparameter *cab-axis-offset* '(0d0 0.10d0) "y, z of the cab's axis from the eye.")
(defparameter *cab-deck-z* 1.15d0 "m: the tread plate's top face.")
(defparameter *cab-eye-clearance* 0.15d0 "m: nothing of the shell nearer the eye.")
(defparameter *cab-ring-xs* '(-3.1d0 -2.6d0 -2.1d0 -1.6d0 -1.1d0 -0.6d0 -0.1d0 0.4d0)
  "x of the ring frames, every 0.5 m (x 11.0 to 14.5).")
(defparameter *cab-hatch*
  '(:x -2.7d0 :width 0.8d0 :arc 1.2d0 :corner 0.2d0 :handwheel 0.4d0)
  "The hatch in the +y wall: centred x 11.4 (m from the eye -2.7), 0.8 m along x
by 1.2 m around, corners r 0.2, a handwheel Ø 0.4 (decision 126).")
(defparameter *cab-window* '(:x 0.90d0 :half-width 0.60d0 :half-height 0.30d0 :slack 0.02d0)
  "The window's opening in the front wall: 1.20 x 0.60 m centred on the eye's y and z.")
(defparameter *cab-porthole-centre* '(1.118d0 0.943d0)
  "|y|, z of a porthole's inside opening centre from the eye (decision 126); x 0.")
(defparameter *cab-porthole-radius* 0.175d0 "m: Ø 0.35.")
(defparameter *cab-porthole-depth* 0.15d0 "m: half the opening's cylinder along its axis.")
;;; The window's frame and glass, the boresight cross, the porthole rings, the plate.

(defparameter *cab-window-frame*
  '(:inner-width 1.20d0 :inner-height 0.60d0 :outer-width 1.30d0 :outer-height 0.70d0
    :depth 0.05d0 :bolt-diameter 0.012d0 :bolt-pitch 0.1d0)
  "The steel angle round the window: inner 1.20 x 0.60 m, outer 1.30 x 0.70 m,
standing 50 mm off the front wall; bolts Ø 12 mm every 0.1 m.")
(defparameter *cab-cross* '(:length 0.10d0 :width 0.012d0)
  "The boresight cross: two strips of white tape on the glass.")
(defparameter *cab-glass-transmission* '(0.95d0 0.88d0)
  "The glass's transmission at the centre and at the edges.")
(defparameter *cab-glass-tint* '(0.85d0 1.0d0 0.9d0)
  "The faint green of the glass's reflection of the cab light.")
(defparameter *cab-glass-reflectance* 0.5d0
  "The radiance of the cab the glass reflects, as a share of the light at it.")
(defparameter *cab-porthole-ring* '(:width 0.03d0 :standoff 0.035d0)
  "The porthole's frame ring: 30 mm wide, standing 35 mm off the lining plane, clear of the lining's curve.")
(defparameter *cab-plate* '(:centre (0.89d0 0d0 -0.43d0) :width 0.40d0 :height 0.07d0
                            :thickness 0.02d0 :cap 0.030d0 :text "M-07  PATIENCE")
  "The name plate above the window (decision 126), its text in Monaspace Neon Bold.")
(defparameter *cab-plate-ink* '(40 40 40) "Black paint, sRGB.")
(defparameter *cab-glass* t "Whether encode-cockpit draws the glass (tests turn it off).")

(defparameter *cab-strip-x* '(-3.4d0 0.7d0) "m: the LED strips' ends (x 10.7 to 14.8).")
(defparameter *cab-strip-y* 0.40d0 "m: each strip's |y|.")
(defparameter *cab-strip-z* -1.21d0 "m: the strips' z (1.40 m lining, decision 126).")
(defparameter *cab-ambient* 0.002d0 "The cab's ambient radiance factor.")
(defparameter *cab-led-tint* '(1.0d0 0.85d0 0.65d0) "The strips' warm white.")
(defparameter *cab-reference-point* (vec3 0.46d0 0d0 0.52d0)
  "The button block's centre: a white 0.8 surface facing up there has radiance 0.04.")
(defparameter *cab-reference-radiance* 0.04d0)

(defun cab-axis-point (x &optional (y 0d0) (z 0d0))
  "The point at X along the cab, Y and Z across from its axis, in body axes from the eye."
  (vec3 x (+ y (first *cab-axis-offset*)) (+ z (second *cab-axis-offset*))))

(defun cab-wall-point (x angle radius)
  "The point at X on the circle of RADIUS about the cab's axis at ANGLE
(radians from +y towards +z, i.e. down)."
  (cab-axis-point x (* radius (cos angle)) (* radius (sin angle))))

;;; The cab's light (decision 126): two strips of eight point samples each,
;;; inverse square and Lambert, plus an ambient; no shadows.

(defparameter *cab-strips*
  (let* ((x0 (first *cab-strip-x*)) (x1 (second *cab-strip-x*))
         (step (/ (- x1 x0) 8)))
    (loop for y in (list (- *cab-strip-y*) *cab-strip-y*)
          collect (list (+ x0 (* 0.5d0 step)) step y *cab-strip-z*)))
  "Each strip as (x of the first sample, the step, y, z).")

(defun cab-strip-sum (p n)
  "The sum over the sixteen samples of n . l / d^2 at P with unit normal N."
  (let ((sum 0d0))
    (dolist (strip *cab-strips* sum)
      (destructuring-bind (x0 step y z) strip
        (dotimes (k 8)
          (let* ((dx (- (+ x0 (* k step)) (aref p 0))) (dy (- y (aref p 1))) (dz (- z (aref p 2)))
                 (r2 (max (+ (* dx dx) (* dy dy) (* dz dz)) 0.0004d0))
                 (dot (+ (* dx (aref n 0)) (* dy (aref n 1)) (* dz (aref n 2)))))
            (incf sum (/ (max dot 0d0) (* r2 (sqrt r2))))))))))

(defun cab-led-level-for (radiance albedo reference)
  "The strips' level L with ALBEDO x (L x sum at REFERENCE facing up + ambient) = RADIANCE."
  (let ((up (vec3 0d0 0d0 -1d0)))
    (/ (- (/ radiance albedo) *cab-ambient*) (cab-strip-sum reference up))))

(defparameter *cab-led-level* (cab-led-level-for *cab-reference-radiance* 0.8d0 *cab-reference-point*)
  "The strips' level, set so a white 0.8 surface at the button block's centre
facing up has the red channel's radiance 0.04; live.")

(defun cab-irradiance (p n)
  "The cab's radiance factor at P with unit normal N, as (values r g b): the
strips' tint x level x the samples' sum, plus the ambient. The twin of the
shader function of the same name."
  (let ((sum (* *cab-led-level* (cab-strip-sum p n))))
    (values-list (loop for tint in *cab-led-tint* collect (+ (* tint sum) *cab-ambient*)))))

;;; The openings (the window in the front wall, two portholes in the lining).

(defun cab-porthole-geometry (side)
  "(values centre axis) of the porthole on SIDE (+1 starboard, -1 port): its
inside centre and the unit outward direction from the cab's axis."
  (destructuring-bind (y z) *cab-porthole-centre*
    (let* ((centre (vec3 0d0 (* side y) z))
           (out (vec3 0d0 (* side y) (- z (second *cab-axis-offset*)))))
      (values centre (vec3-normalize out)))))

(defun cab-opening-p (p)
  "Whether the body point P (from the eye) lies in the window's opening (within
its slack of the front wall's plane) or in a porthole's cylinder: the twin of
the shader function cab-opening-cut."
  (or (and (< (abs (- (aref p 0) (getf *cab-window* :x))) (getf *cab-window* :slack))
           (< (abs (aref p 1)) (getf *cab-window* :half-width))
           (< (abs (aref p 2)) (getf *cab-window* :half-height)))
      (dolist (side '(-1 1) nil)
        (multiple-value-bind (centre axis) (cab-porthole-geometry side)
          (let* ((d (vec3-sub p centre))
                 (along (vec3-dot d axis)))
            (when (and (< (abs along) *cab-porthole-depth*)
                       (< (- (vec3-dot d d) (* along along)) (expt *cab-porthole-radius* 2)))
              (return t)))))))

;;; The shell

(defun cab-wall-sides () 96)

(defun cab-lining! (out material x0 x1)
  "The lining: the cylinder of *cab-lining-radius* about the cab's axis from X0 to X1,
normals inward."
  (let ((xf (xform '(0 0 0))) (sides (cab-wall-sides)) (r *cab-lining-radius*))
    (flet ((vertex (x k)
             (let ((a (ring-angle k sides)))
               (list x (+ (* r (cos a)) (first *cab-axis-offset*))
                     (+ (* r (sin a)) (second *cab-axis-offset*))
                     0 (- (cos a)) (- (sin a)) 0 0))))
      (dotimes (k sides)
        (quad! out xf material (vertex x0 k) (vertex x0 (1+ k)) (vertex x1 (1+ k)) (vertex x1 k))))))

(defun cab-wall! (out material x normal)
  "A wall disc of *cab-lining-radius* at X facing NORMAL (+1 or -1 along x)."
  (let ((xf (xform '(0 0 0))) (sides (cab-wall-sides)) (r *cab-lining-radius*))
    (flet ((rim (k)
             (let ((a (ring-angle k sides)))
               (list x (+ (* r (cos a)) (first *cab-axis-offset*))
                     (+ (* r (sin a)) (second *cab-axis-offset*)) normal 0 0 0 0))))
      (dotimes (k sides)
        (triangle! out xf material
                   (list x (first *cab-axis-offset*) (second *cab-axis-offset*) normal 0 0 0 0)
                   (rim k) (rim (1+ k)))))))

(defun rounded-rectangle (width height corner)
  "The outline of a WIDTH x HEIGHT rectangle centred on the origin with CORNER
radius corners, counter-clockwise, as (u v) points, six segments a corner."
  (let ((hw (* 0.5d0 width)) (hh (* 0.5d0 height)) (points '()))
    (loop for (cx cy start) in (list (list (- hw corner) (- hh corner) 0)
                                     (list (- corner hw) (- hh corner) 90)
                                     (list (- corner hw) (- corner hh) 180)
                                     (list (- hw corner) (- corner hh) 270))
          do (dotimes (i 7)
               (let ((a (degrees-to-radians (+ start (* i 15)))))
                 (push (list (+ cx (* corner (cos a))) (+ cy (* corner (sin a)))) points))))
    (nreverse points)))

(defun cab-hatch-patch! (out material radius grow)
  "The hatch's rounded rectangle (GROW m larger all round) on the cylinder of
RADIUS in the +y wall, a fan from its centre, normals inward."
  (let* ((xf (xform '(0 0 0)))
         (hx (getf *cab-hatch* :x))
         (outline (rounded-rectangle (+ (getf *cab-hatch* :width) grow grow)
                                     (+ (getf *cab-hatch* :arc) grow grow)
                                     (+ (getf *cab-hatch* :corner) grow))))
    (flet ((vertex (u v)
             (let ((a (/ v *cab-lining-radius*)))
               (list (+ hx u) (+ (* radius (cos a)) (first *cab-axis-offset*))
                     (+ (* radius (sin a)) (second *cab-axis-offset*))
                     0 (- (cos a)) (- (sin a)) 0 0))))
      (loop for (a b) on (append outline (list (first outline)))
            while b
            do (triangle! out xf material (vertex 0 0) (vertex (first a) (second a))
                          (vertex (first b) (second b)))))))

(defun cab-opening-skipped-p (x angle margin)
  "Whether a frame or stringer element at X and ANGLE (radians) lies in the
hatch or a porthole, their MARGIN m larger."
  (let* ((hatch-x (getf *cab-hatch* :x))
         (half-x (+ (* 0.5d0 (getf *cab-hatch* :width)) margin))
         (half-angle (/ (+ (* 0.5d0 (getf *cab-hatch* :arc)) margin) *cab-lining-radius*))
         (angle (mod (+ angle pi) (* 2 pi)))
         (wrapped (- angle pi)))
    (or (and (< (abs (- x hatch-x)) half-x) (< (abs wrapped) half-angle))
        (dolist (side '(-1 1) nil)
          (multiple-value-bind (centre axis) (cab-porthole-geometry side)
            (let ((a (atan (- (aref axis 2) 0d0) (aref axis 1))))
              (when (and (< (abs (- x (aref centre 0))) (+ *cab-porthole-radius* margin))
                         (< (abs (- wrapped a)) (/ (+ *cab-porthole-radius* margin) *cab-lining-radius*)))
                (return t))))))))

(defun cab-ring-frames! (b)
  "Eight rings of 50 x 50 mm angle, 32 segments each, inward of the lining; the
segments in the hatch and the portholes left out."
  (let* ((sides 32) (rc 1.36d0)
         (length (* 2 rc (sin (/ pi sides)) 1.02d0))
         (material (cab-material-id :steel-angle)))
    (dolist (x *cab-ring-xs*)
      (dotimes (k sides)
        (let ((a (ring-angle (+ k 0.5d0) sides)))
          (unless (cab-opening-skipped-p x a 0.05d0)
            (builder-add b :box (instance (cab-wall-point x a rc) '(1 0 0)
                                          (list 0 (- (sin a)) (cos a))
                                          (list 0.05d0 length 0.05d0) material))))))))

(defun cab-stringers! (b)
  "Eight stringers of 50 x 50 mm angle along x at 22.5° + 45° k, the one by the
hatch cut around it."
  (let ((material (cab-material-id :steel-angle))
        (hatch-x (getf *cab-hatch* :x)) (half (* 0.5d0 (getf *cab-hatch* :width))))
    (dotimes (k 8)
      (let* ((a (degrees-to-radians (+ 22.5d0 (* 45 k)))) (rc 1.36d0)
             (spans (if (< (abs (- (mod (+ a pi) (* 2 pi)) pi)) (/ (+ 0.6d0 0.03d0) *cab-lining-radius*))
                        (list (list -3.50d0 (- hatch-x half 0.03d0))
                              (list (+ hatch-x half 0.03d0) 0.85d0))
                        (list (list -3.50d0 0.85d0)))))
        (loop for (from to) in spans
              do (builder-add b :box (instance (cab-wall-point (* 0.5d0 (+ from to)) a rc) '(1 0 0)
                                               (list 0 (- (sin a)) (cos a))
                                               (list (- to from) 0.05d0 0.05d0) material)))))))

(defun cab-box! (b centre size material &key (x '(1 0 0)) y)
  (builder-add b :box (instance centre x y size (cab-material-id material))))

(defun cab-handwheel! (b)
  "The hatch's handwheel: a ring of twelve segments, two spokes and a hub, on the
hatch's face."
  (let* ((hx (getf *cab-hatch* :x)) (radius (* 0.5d0 (getf *cab-hatch* :handwheel)))
         (y 1.34d0) (z (second *cab-axis-offset*)) (material :steel-angle))
    (dotimes (k 12)
      (let ((a (ring-angle k 12)))
        (cab-box! b (vec3 (+ hx (* radius (cos a))) y (+ z (* radius (sin a))))
                  (list (* 2 radius (sin (/ pi 12)) 1.05d0) 0.025d0 0.03d0) material
                  :x (list (- (sin a)) 0 (cos a)) :y '(0 1 0))))
    (cab-box! b (vec3 hx y z) (list (* 2 radius) 0.02d0 0.03d0) material)
    (cab-box! b (vec3 hx y z) (list 0.03d0 0.02d0 (* 2 radius)) material)
    (cab-box! b (vec3 hx (- y 0.01d0) z) '(0.07d0 0.04d0 0.07d0) :red)))

(defun cab-aft-fittings! (b)
  "The ECLSS rack against the aft wall (boxes with fan grilles), the stowage
net with its bags, the red extinguisher."
  (let ((x0 (first *cab-x-range*)))
    (builder-part b "eclss" :box
                  (lambda ()
                    (cab-box! b (vec3 (+ x0 0.20d0) -0.65d0 0.50d0) '(0.40d0 0.55d0 0.60d0) :eclss)
                    (cab-box! b (vec3 (+ x0 0.20d0) -0.65d0 -0.25d0) '(0.40d0 0.55d0 0.50d0) :eclss)
                    (cab-box! b (vec3 (+ x0 0.12d0) -0.18d0 0.85d0) '(0.24d0 0.30d0 0.40d0) :eclss)))
    (builder-part b "grilles" :tube
                  (lambda ()
                    (loop for (y z) in '((-0.65d0 0.50d0) (-0.65d0 -0.25d0))
                          do (builder-add b :tube (tube-instance (vec3 (+ x0 0.40d0) y z)
                                                                 (vec3 (+ x0 0.412d0) y z) 0.11d0
                                                                 (cab-material-id :black-plastic))))))
    (builder-part b "net" :box
                  (lambda ()
                    (cab-box! b (vec3 (+ x0 0.02d0) 0.65d0 0.10d0) '(0.02d0 0.70d0 0.90d0) :fabric)))
    (builder-part b "bags" :box
                  (lambda ()
                    (cab-box! b (vec3 (+ x0 0.14d0) 0.55d0 0.45d0) '(0.20d0 0.30d0 0.30d0) :fabric)
                    (cab-box! b (vec3 (+ x0 0.12d0) 0.80d0 0.15d0) '(0.18d0 0.26d0 0.34d0) :fabric)
                    (cab-box! b (vec3 (+ x0 0.15d0) 0.50d0 -0.12d0) '(0.22d0 0.28d0 0.22d0) :fabric)))
    (builder-part b "extinguisher" :tube
                  (lambda ()
                    (builder-add b :tube (tube-instance (vec3 (+ x0 0.15d0) 0.18d0 0.40d0)
                                                        (vec3 (+ x0 0.15d0) 0.18d0 1.10d0) 0.08d0
                                                        (cab-material-id :red)))))))

(defun cab-seat! (b)
  "The jump seat folded against the -y wall aft, and the headrest's two side
rolls behind the eye."
  (builder-part b "seat" :box
                (lambda ()
                  (cab-box! b (vec3 -2.6d0 -1.20d0 0.20d0) '(0.50d0 0.06d0 0.80d0) :seat)
                  (cab-box! b (vec3 -2.6d0 -1.12d0 0.55d0) '(0.46d0 0.10d0 0.12d0) :seat)
                  (cab-box! b (vec3 -2.6d0 -1.15d0 -0.25d0) '(0.46d0 0.04d0 0.06d0) :steel-angle)))
  (builder-part b "headrest" :box
                (lambda ()
                  (dolist (side '(-1 1))
                    (cab-box! b (vec3 -0.40d0 (* side 0.30d0) 0d0) '(0.10d0 0.07d0 0.24d0) :seat)))))

(defun cab-strips! (b)
  "The two LED strips: an emissive face over a steel channel, each from x 10.7
to 14.8 (m from the eye -3.4 to 0.7)."
  (builder-part b "strips" :box
                (lambda ()
                  (let* ((x0 (first *cab-strip-x*)) (x1 (second *cab-strip-x*))
                         (centre (* 0.5d0 (+ x0 x1))) (length (- x1 x0)))
                    (dolist (side '(-1 1))
                      (cab-box! b (vec3 centre (* side *cab-strip-y*) (- *cab-strip-z* 0.012d0))
                                (list length 0.07d0 0.012d0) :steel-angle)
                      (cab-box! b (vec3 centre (* side *cab-strip-y*) *cab-strip-z*)
                                (list length 0.05d0 0.012d0) :led))))))

;;; Window frame, glass, cross, porthole rings, name plate

(defun cab-window-bolt-positions ()
  "The 36 bolts of the window frame as (y z) from the eye: 13 along each of the
top and bottom bars and 5 more along each side bar between them."
  (let* ((pitch (getf *cab-window-frame* :bolt-pitch))
         (outer-w (getf *cab-window-frame* :outer-width)) (outer-h (getf *cab-window-frame* :outer-height))
         (inner-w (getf *cab-window-frame* :inner-width)) (inner-h (getf *cab-window-frame* :inner-height))
         (bar-y (* 0.25d0 (+ outer-w inner-w))) (bar-z (* 0.25d0 (+ outer-h inner-h)))
         (across (round (* 0.5d0 inner-w) pitch)) (down (round (* 0.5d0 inner-h) pitch))
         (positions '()))
    (loop for z in (list (- bar-z) bar-z)
          do (loop for k from (- across) to across do (push (list (* k pitch) z) positions)))
    (loop for y in (list (- bar-y) bar-y)
          do (loop for k from (- (1- down)) to (1- down) do (push (list y (* k pitch)) positions)))
    (nreverse positions)))

(defun cab-window-frame! (b)
  "The window's frame: four bars of steel angle in front of the wall's opening
and its 36 bolt heads, in the part \"window-frame\" and \"window-bolts\"."
  (destructuring-bind (&key inner-width inner-height outer-width outer-height depth bolt-diameter
                         &allow-other-keys) *cab-window-frame*
    (let* ((x (- (getf *cab-window* :x) (* 0.5d0 depth)))
           (bar-w (* 0.5d0 (- outer-width inner-width))) (bar-h (* 0.5d0 (- outer-height inner-height))))
      (builder-part b "window-frame" :box
                    (lambda ()
                      (dolist (side '(-1 1))
                        (cab-box! b (vec3 x 0d0 (* side (+ (* 0.5d0 inner-height) (* 0.5d0 bar-h))))
                                  (list depth outer-width bar-h) :steel-angle)
                        (cab-box! b (vec3 x (* side (+ (* 0.5d0 inner-width) (* 0.5d0 bar-w))) 0d0)
                                  (list depth bar-w inner-height) :steel-angle))))
      (builder-part b "window-bolts" :tube
                    (lambda ()
                      (loop for (y z) in (cab-window-bolt-positions)
                            do (builder-add b :tube (tube-instance (vec3 (- x (* 0.5d0 depth)) y z)
                                                                   (vec3 (- x (* 0.5d0 depth) 0.008d0) y z)
                                                                   (* 0.5d0 bolt-diameter)
                                                                   (cab-material-id :steel-angle)))))))))

(defun cab-boresight-cross! (b)
  "The boresight cross: two white tape strips 0.10 x 0.012 m on the glass at the
window's centre, a 2 mm off it."
  (builder-part b "cross" :box
                (lambda ()
                  (let ((x (- (getf *cab-window* :x) 0.003d0))
                        (long (getf *cab-cross* :length)) (wide (getf *cab-cross* :width)))
                    (cab-box! b (vec3 x 0d0 0d0) (list 0.002d0 long wide) :marker)
                    (cab-box! b (vec3 x 0d0 0d0) (list 0.002d0 wide long) :marker)))))

(defun cab-porthole-basis (side)
  "(values centre axis u v): the porthole's inside centre, its outward axis and
two unit vectors across it."
  (multiple-value-bind (centre axis) (cab-porthole-geometry side)
    (let* ((u (vec3 1d0 0d0 0d0))
           (v (vec3-normalize (vec3-cross axis u))))
      (values centre axis u v))))

(defun cab-porthole-rings! (b)
  "Each porthole's frame ring: twelve steel angle segments round the opening,
standing off the lining."
  (builder-part b "porthole-rings" :box
                (lambda ()
                  (let* ((width (getf *cab-porthole-ring* :width))
                         (standoff (getf *cab-porthole-ring* :standoff))
                         (radius (+ *cab-porthole-radius* (* 0.5d0 width)))
                         (length (* 2 radius (sin (/ pi 12)) 1.05d0)))
                    (dolist (side '(-1 1))
                      (multiple-value-bind (centre axis u v) (cab-porthole-basis side)
                        (dotimes (k 12)
                          (let* ((a (ring-angle k 12))
                                 (radial (vec3-add (vec3-scale u (cos a)) (vec3-scale v (sin a))))
                                 (tangent (vec3-sub (vec3-scale v (cos a)) (vec3-scale u (sin a)))))
                            (cab-box! b (vec3-sub (vec3-add centre (vec3-scale radial radius))
                                                  (vec3-scale axis standoff))
                                      (list length width width) :steel-angle
                                      :x tangent :y radial)))))))))

(defun cab-plate-geometry ()
  "(values plane-origin cell-width columns): the name plate's text plane (body
axes from the eye, 3 mm in front of the face: 1 mm z-fights with the face at
1 m in the depth buffer), centring the run."
  (destructuring-bind (&key centre width height thickness cap text &allow-other-keys) *cab-plate*
    (declare (ignore width height))
    (let* ((cell (* +text-cell-width+ cap)) (columns (length text))
           (x (- (first centre) (* 0.5d0 thickness) 0.003d0)))
      (values (list x (- (* 0.5d0 columns cell)) (+ (third centre) (* 0.5d0 cap)))
              cell columns))))

(defun cab-plate! (b)
  (builder-part b "plate" :box
                (lambda ()
                  (destructuring-bind (&key centre width height thickness &allow-other-keys) *cab-plate*
                    (cab-box! b (vec3 (first centre) (second centre) (third centre))
                              (list thickness width height) :plate)))))

(defun cab-glass-corner (x y z nx ny nz kind u v)
  (list x y z nx ny nz kind u v))

(defun build-cab-glass ()
  "The glass as hull vertices (material = kind, 0 window, 1 porthole; the two
parameters the position across it, -1 to 1): the window's quad in its opening
and a disc of 24 triangles in each porthole."
  (let* ((b (make-builder)) (hull (builder-hull b)) (xf (xform '(0 0 0)))
         (x (getf *cab-window* :x)) (w (getf *cab-window* :half-width)) (h (getf *cab-window* :half-height)))
    (flet ((corner (y z u v) (list x y z -1 0 0 u v)))
      (builder-part b "window-glass" :hull
                    (lambda ()
                      (quad! hull xf 0 (corner (- w) (- h) -1 -1) (corner (- w) h -1 1)
                             (corner w h 1 1) (corner w (- h) 1 -1)))))
    (dolist (side '(-1 1))
      (builder-part b (format nil "porthole-glass-~D" side) :hull
                    (lambda ()
                      (multiple-value-bind (centre axis u v) (cab-porthole-basis side)
                        (let ((n (vec3-scale axis -1d0)) (r *cab-porthole-radius*))
                          (flet ((rim (k)
                                   (let* ((a (ring-angle k 24)) (c (cos a)) (s (sin a))
                                          (p (vec3-add centre (vec3-add (vec3-scale u (* r c))
                                                                        (vec3-scale v (* r s))))))
                                     (list (aref p 0) (aref p 1) (aref p 2) (aref n 0) (aref n 1) (aref n 2)
                                           c s))))
                            (dotimes (k 24)
                              (triangle! hull xf 1
                                         (list (aref centre 0) (aref centre 1) (aref centre 2)
                                               (aref n 0) (aref n 1) (aref n 2) 0 0)
                                         (rim k) (rim (1+ k))))))))))
    (builder-model b)))

(defvar *cab-glass-model* nil)

(defun cab-glass-model ()
  (or *cab-glass-model* (setf *cab-glass-model* (build-cab-glass))))

(defun cab-glass-transmission (edge)
  "The glass's transmission at EDGE, 0 at the centre to 1 at the border."
  (destructuring-bind (centre border) *cab-glass-transmission*
    (- centre (* (- centre border) (min 1d0 edge)))))

(defun cab-glass-colour (p n edge)
  "The glass at body point P with unit normal N turned to the eye, EDGE its
position across it (0 centre, 1 border), as (values r g b alpha) premultiplied:
alpha = 1 - transmission, the colour its reflection of the cab light, tinted
green. The twin of cab-glass-fragment."
  (let ((alpha (- 1d0 (cab-glass-transmission edge))))
    (multiple-value-bind (r g b) (cab-irradiance p n)
      (let ((k (* alpha *cab-glass-reflectance*)))
        (values (* k r (first *cab-glass-tint*)) (* k g (second *cab-glass-tint*))
                (* k b (third *cab-glass-tint*)) alpha)))))

(defparameter *cab-plate-region* nil "The text batch's region holding the name plate.")

(defun cockpit-text! (atlas batch)
  "Write the cockpit's static text (so far the name plate) into a region of BATCH."
  (let* ((text (getf *cab-plate* :text))
         (region (text-batch-region batch (length text))))
    (multiple-value-bind (origin) (cab-plate-geometry)
      (grid-run! atlas region 0
                 (make-text-plane :origin origin :right '(0 1 0) :up '(0 0 -1))
                 text :font :mono-bold :cap (float (getf *cab-plate* :cap) 1f0) :kind :lit
                      :colour (apply #'text-colour *cab-plate-ink*)))
    (setf *cab-plate-region* region)))

(defun build-cab-model ()
  "The cab's shell and fittings as a model (meshes.lisp's builder): hull
vertices for the lining, walls and hatch, instanced boxes and tubes for the
rest, in body axes from the eye."
  (let* ((b (make-builder)) (hull (builder-hull b))
         (x0 (first *cab-x-range*)) (x1 (second *cab-x-range*)))
    (builder-part b "lining" :hull
                  (lambda () (cab-lining! hull (cab-material-id :lining) x0 x1)))
    (builder-part b "front-wall" :hull
                  (lambda () (cab-wall! hull (cab-material-id :lining) x1 -1)))
    (builder-part b "aft-wall" :hull
                  (lambda () (cab-wall! hull (cab-material-id :lining) x0 1)))
    (builder-part b "hatch-rim" :hull
                  (lambda () (cab-hatch-patch! hull (cab-material-id :steel-angle) 1.396d0 0.04d0)))
    (builder-part b "hatch" :hull
                  (lambda () (cab-hatch-patch! hull (cab-material-id :hatch) 1.392d0 0d0)))
    (builder-part b "handwheel" :box (lambda () (cab-handwheel! b)))
    (builder-part b "frames" :box (lambda () (cab-ring-frames! b)))
    (builder-part b "stringers" :box (lambda () (cab-stringers! b)))
    (builder-part b "deck" :box
                  (lambda ()
                    (cab-box! b (vec3 (* 0.5d0 (+ x0 x1)) 0d0 (+ *cab-deck-z* 0.01d0))
                              (list (- x1 x0) 1.80d0 0.02d0) :tread)))
    (cab-aft-fittings! b)
    (cab-seat! b)
    (cab-strips! b)
    (cab-window-frame! b)
    (cab-boresight-cross! b)
    (cab-porthole-rings! b)
    (cab-plate! b)
    (builder-model b)))

(defvar *cab-model* nil "The cab's model, made once.")

(defun cab-model ()
  (or *cab-model* (setf *cab-model* (build-cab-model))))

(defun cab-shell-part-names ()
  (mapcar #'first (model-parts (cab-model))))

;;; Counts for the budget (plan 31: ≤ 30,000 vertices, ≤ 2,000 instances).

(defun cab-vertex-count (&optional (model (cab-model)))
  (floor (length (model-vertices model)) +vertex-floats+))

(defun cab-instance-count (&optional (model (cab-model)))
  (loop for g in (model-groups model)
        sum (floor (length (instance-group-instances g)) +instance-floats+)))

;;; On the GPU

(defconstant +cab-block-floats+ 60 "shaders.lisp's cab block: 15 vec4s (= +text-block-floats+).")

(defstruct (cockpit-gpu (:constructor %make-cockpit-gpu))
  "The cab's static resources: its model, layout, hull and instance groups,
the block's data (the static part filled once), the scratch camera and the
materials' data."
  model layout hull (hull-count 0 :type fixnum)
  glass (glass-count 0 :type fixnum)    ; the glass's vertex buffer and count
  groups                                ; (primitive vertices instances count)
  (block (make-array +cab-block-floats+ :element-type 'single-float :initial-element 0f0))
  (camera (make-camera))
  (materials (make-array (* 8 (array-dimension *cab-materials* 0))
                         :element-type 'single-float :initial-element 0f0)))

(defstruct cab-slot
  "A target's cab block, material table and the bind group of both."
  buffer materials bind-group)

(defun cab-block-static! (out)
  "The cab block's parts that do not change per frame: the strips, the light,
the window and the portholes."
  (flet ((put (index &rest values)
           (loop for v in values for i from index
                 do (setf (aref out i) (coerce v 'single-float)))))
    (destructuring-bind (a b) *cab-strips*
      (apply #'put 28 a)
      (apply #'put 32 b))
    (put 36 (first *cab-led-tint*) (second *cab-led-tint*) (third *cab-led-tint*) *cab-led-level*)
    (put 40 (getf *cab-window* :x) (getf *cab-window* :half-width) (getf *cab-window* :half-height)
         (getf *cab-window* :slack))
    (loop for side in '(-1 1) for index in '(44 52)
          do (multiple-value-bind (centre axis) (cab-porthole-geometry side)
               (put index (aref centre 0) (aref centre 1) (aref centre 2) *cab-porthole-radius*)
               (put (+ index 4) (aref axis 0) (aref axis 1) (aref axis 2) *cab-porthole-depth*)))
    out))

(defun make-cockpit-gpu (renderer format)
  "The cab's buffers and layout, owned by RENDERER, and its :cab-mesh and
:cab-instanced live pipelines into FORMAT: opaque, depth written, compare
:greater."
  (let* ((model (cab-model))
         (layout (gpu-create renderer (luv:make-bind-group-layout-descriptor
                                       :entries '((:binding 0 :type :uniform-buffer)
                                                  (:binding 1 :type :storage-buffer)))))
         (depth '(:format :depth32-float :depth-write-enabled t :depth-compare :greater)))
    (live-pipeline renderer :cab-mesh layout (ship-vertex-buffers) :format format
                                                                   :depth-stencil depth)
    (live-pipeline renderer :cab-instanced layout (ship-vertex-buffers t) :format format
                                                                          :depth-stencil depth)
    (live-pipeline renderer :cab-glass layout (ship-vertex-buffers) :format format
                   :blend :premultiplied-alpha
                   :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                    :depth-compare :greater))
    (let ((gpu (%make-cockpit-gpu
                :model model :layout layout
                :glass (vertex-buffer renderer (model-vertices (cab-glass-model)) "cab glass")
                :glass-count (cab-vertex-count (cab-glass-model))
                :hull (vertex-buffer renderer (model-vertices model) "cab hull")
                :hull-count (cab-vertex-count model)
                :groups (loop for g in (model-groups model)
                              for count = (floor (length (instance-group-instances g)) +instance-floats+)
                              for kind = (string-downcase (instance-group-kind g))
                              when (plusp count)
                                collect (list (vertex-buffer renderer (instance-group-primitive g)
                                                             (format nil "cab ~A" kind))
                                              (floor (length (instance-group-primitive g)) +vertex-floats+)
                                              (vertex-buffer renderer (instance-group-instances g)
                                                             (format nil "cab ~A instances" kind))
                                              count)))))
      (cab-block-static! (cockpit-gpu-block gpu))
      gpu)))

(defun make-cab-slot-for (renderer gpu)
  (let* ((device (renderer-device renderer))
         (buffer (luv:create device (luv:make-buffer-descriptor
                                     :label "cab block" :size (* 4 +cab-block-floats+)
                                     :usage '(:uniform :copy-dst))))
         (materials (luv:create device (luv:make-buffer-descriptor
                                        :label "cab materials"
                                        :size (* 4 (length (cockpit-gpu-materials gpu)))
                                        :usage '(:storage :copy-dst)))))
    (make-cab-slot
     :buffer buffer :materials materials
     :bind-group (luv:create device (luv:make-bind-group-descriptor
                                     :label "cab" :layout (cockpit-gpu-layout gpu)
                                     :entries `((:binding 0 :resource ,buffer)
                                                (:binding 1 :resource ,materials)))))))

(defun release-cab-slot (slot)
  (luv:destroy (cab-slot-bind-group slot))
  (luv:destroy (cab-slot-materials slot))
  (luv:destroy (cab-slot-buffer slot)))
