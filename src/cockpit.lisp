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
    :cable-yellow :kapton :cardboard :sheet-metal :led :fabric :seat :hatch :plate
    :channel-x :channel-y :channel-z)
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
                (0.40 0.40 0.42 0.15 16.0 0.0)     ; name plate, steel
                (0.45 0.45 0.47 0.15 16.0 0.0)     ; slotted channel, along x
                (0.45 0.45 0.47 0.15 16.0 0.0)     ; slotted channel, along y
                (0.45 0.45 0.47 0.15 16.0 0.0))))  ; slotted channel, along z
    (let ((table (make-array (list (length rows) 6) :element-type 'single-float
                                                     :initial-contents rows))
          (row (position :screen-face *cab-material-names*)))
      ;; The screen faces' backlight: the albedo is the radiance that maps to
      ;; #0A0C0C at the cockpit's exposure, the emission factor 1 (decision 127).
      (multiple-value-bind (r g b) (emissive-for-srgb #x0A #x0C #x0C)
        (setf (aref table row 0) (float r 1f0) (aref table row 1) (float g 1f0)
              (aref table row 2) (float b 1f0) (aref table row 5) 1f0))
      table))
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

;;; The sunlit patch (docs/design.md "The DIY look"): the ray from a point
;;; towards the Sun meets the window's rectangle or a porthole's disc.

(defun sun-patch-p (p s &optional (margin 0d0))
  "Whether the ray from the body point P along the unit direction S (towards
the Sun) passes through the window's rectangle (in the front wall's plane) or
a porthole's disc (at its inside centre), the openings grown by MARGIN m. The
twin of the shader function sun-patch."
  (let ((wx (getf *cab-window* :x)) (sx (aref s 0)))
    (or (and (> sx 0.001d0) (< (aref p 0) wx)
             (let ((tt (/ (- wx (aref p 0)) sx)))
               (and (< (abs (+ (aref p 1) (* tt (aref s 1)))) (+ (getf *cab-window* :half-width) margin))
                    (< (abs (+ (aref p 2) (* tt (aref s 2)))) (+ (getf *cab-window* :half-height) margin)))))
        (dolist (side '(-1 1) nil)
          (multiple-value-bind (centre axis) (cab-porthole-geometry side)
            (let ((facing (vec3-dot s axis)))
              (when (> facing 0.001d0)
                (let ((tt (/ (vec3-dot (vec3-sub centre p) axis) facing)))
                  (when (> tt 0d0)
                    (let ((d (vec3-sub (vec3-add-scaled p s tt) centre)))
                      (when (< (vec3-norm d) (+ *cab-porthole-radius* margin))
                        (return t))))))))))))

(defun cab-sun-radiance (p n s factor albedo)
  "The sun patch's radiance at P, unit normal N, ALBEDO: FACTOR x max(n.s, 0) x
ALBEDO where (sun-patch-p P S), else 0."
  (if (sun-patch-p p s)
      (* factor (max (vec3-dot n s) 0d0) albedo)
      0d0))

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

(defparameter *cab-text-regions* '() "((:label text) | (:placeholder panel-name)) -> text-region.")

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
    (setf *cab-plate-region* region)
    (setf *cab-text-regions* '())
    (cockpit-labels-text! atlas batch)
    (cockpit-placeholders-text! atlas batch)))

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
    (cab-console! b)
    (builder-model b)))

(defvar *cab-model* nil "The cab's model, made once.")

(defun cab-model ()
  (or *cab-model* (setf *cab-model* (build-cab-model))))

(defun cab-shell-part-names ()
  (mapcar #'first (model-parts (cab-model))))

;;; The console (plan 31 piece 5; decision 126): the rack, the panels with
;;; their faces, bezels, brackets and hoods, the cables and the tape labels.
;;; *cockpit-panels* is the layout table of docs/design.md "Layout" as data,
;;; reachable by name (cockpit-panel); plan 32's instruments hang on it.

(defparameter *cockpit-panels*
  '((:name :caution-lamps :centre (0.90d0 -0.72d0 0d0) :direction (-38.7d0 0d0) :turn 30d0
     :size (0.08d0 0.58d0) :wall t
     :bezel (:material :black-plastic :border 0.015d0 :depth 0.04d0)
     :cables (:cable-black))
    (:name :status-lamps :centre (0.90d0 0.72d0 0d0) :direction (38.7d0 0d0) :turn -30d0
     :size (0.08d0 0.58d0) :wall t
     :bezel (:material :black-plastic :border 0.015d0 :depth 0.04d0)
     :cables (:cable-grey))
    (:name :orbit :centre (0.60d0 -0.28d0 0.35d0) :direction (-25d0 27.5d0)
     :size (0.30d0 0.20d0) :grid (24 8) :screen t
     :bezel (:material :grey-plastic :border 0.025d0 :depth 0.05d0 :bolts 4)
     :hood :cardboard :cables (:cable-black :cable-grey))
    (:name :attitude :centre (0.67d0 0d0 0.35d0) :direction (0d0 27.5d0)
     :size (0.24d0 0.20d0) :grid (19 8) :screen t
     :bezel (:material :black-plastic :border 0.015d0 :depth 0.04d0)
     :cables (:cable-black))
    (:name :target :centre (0.60d0 0.28d0 0.35d0) :direction (25d0 27.5d0)
     :size (0.30d0 0.20d0) :grid (24 8) :screen t
     :bezel (:material :green-surplus :border 0.03d0 :depth 0.06d0)
     :hood :sheet-metal :cables (:cable-red :cable-black :cable-grey))
    (:name :strip :centre (0.55d0 0d0 0.455d0) :direction (0d0 39.6d0)
     :size (0.30d0 0.08d0) :grid (19 3) :screen t :face :green-surplus
     :bezel (:material :aluminium :border 0.012d0 :depth 0.03d0)
     :cables (:cable-grey))
    (:name :buttons :centre (0.46d0 0d0 0.52d0) :direction (0d0 48.5d0)
     :size (0.26d0 0.13d0)
     :bezel (:material :black-plastic :border 0.015d0 :depth 0.04d0)
     :cables (:cable-yellow :cable-black :cable-grey))
    (:name :autopilot :centre (0.42d0 -0.30d0 0.50d0) :direction (-35.5d0 45d0)
     :size (0.26d0 0.19d0) :grid (22 8) :screen t
     :bezel (:material :beige :border 0.035d0 :chin 0.05d0 :depth 0.08d0)
     :cables (:cable-black :cable-grey))
    (:name :ship :centre (0.42d0 0.30d0 0.50d0) :direction (35.5d0 45d0)
     :size (0.26d0 0.19d0) :grid (22 8) :screen t
     :bezel (:material :tape :border 0.035d0 :depth 0.03d0)
     :cables (:cable-black :cable-grey))
    (:name :comms :centre (0.34d0 -0.60d0 0.12d0) :direction (-60d0 10d0)
     :size (0.30d0 0.24d0) :grid (26 11) :screen t
     :bezel (:material :black-plastic :border 0.02d0 :depth 0.06d0)
     :cables (:cable-grey :cable-black))
    (:name :throttle :centre (0.17d0 -0.24d0 0.52d0) :direction (-55d0 61d0)
     :size (0.14d0 0.16d0)
     :bezel (:material :black-plastic :border 0.015d0 :depth 0.04d0)
     :cables (:cable-black :cable-grey))
    (:name :hull-gauge :centre (0.17d0 0.24d0 0.52d0) :direction (55d0 61d0)
     :size (0.12d0 0.12d0) :round t :face :aluminium
     :bezel (:material :black-plastic :border 0.015d0 :depth 0.04d0)
     :cables (:cable-grey)))
  "The layout table (docs/design.md \"Layout\"), one plist a panel: :name,
:centre (dx dy dz) m from the eye, :direction (azimuth elevation) degrees
(informative: the face turns to the eye from its centre), :size (w h) m of the
face, :grid (columns rows) of its text, :screen (a Slug screen, PLACEHOLDER(166)
until plan 32 draws it), :turn (the lamp columns' 30 degrees on the wall),
:wall (fixed to the front wall), :round, :face (its material), :bezel (material,
border, chin, depth, bolts), :hood, :cables (their colours).")

(defparameter *cockpit-bezel-front* 0.004d0 "m: a bezel stands this far in front of its face.")
(defparameter *cockpit-face-thickness* 0.004d0)
(defparameter *cockpit-hood-length* 0.09d0 "m: how far a hood reaches towards the eye.")
(defparameter *cockpit-rack-section* 0.041d0 "m: the slotted channel's square section.")
(defparameter *cockpit-tray* '(:x 0.78d0 :z 0.90d0 :y 0.85d0 :width 0.12d0 :height 0.03d0)
  "The cable tray on the rack: along y at x 0.78, its middle at z 0.90, from -y to +y.")
(defparameter *cockpit-conduit-radius* 1.29d0 "m: the ceiling conduit's distance from the cab's axis.")

(defparameter *cockpit-rack*
  '((:rail-high (0.85d0 -0.85d0 0.42d0) (0.85d0 0.85d0 0.42d0))
    (:rail-mid (0.85d0 -0.85d0 0.62d0) (0.85d0 0.85d0 0.62d0))
    (:rail-low (0.85d0 -0.85d0 0.80d0) (0.85d0 0.85d0 0.80d0))
    (:wall-post-port (0.85d0 -0.85d0 1.15d0) (0.85d0 -0.85d0 0.0d0))
    (:wall-post-starboard (0.85d0 0.85d0 1.15d0) (0.85d0 0.85d0 0.0d0))
    (:stand-port (0.20d0 -0.24d0 1.15d0) (0.20d0 -0.24d0 0.64d0))
    (:stand-starboard (0.20d0 0.24d0 1.15d0) (0.20d0 0.24d0 0.64d0))
    (:tie-port (0.20d0 -0.24d0 0.64d0) (0.85d0 -0.24d0 0.64d0))
    (:tie-starboard (0.20d0 0.24d0 0.64d0) (0.85d0 0.24d0 0.64d0))
    (:comms-post (0.50d0 -0.80d0 1.15d0) (0.50d0 -0.80d0 -0.30d0))
    (:comms-tie (0.50d0 -0.80d0 0.45d0) (0.85d0 -0.85d0 0.42d0)))
  "The rack of slotted channel, deck to front wall: (name from to), body axes from the eye.")

(defun cockpit-panel (name)
  (or (find name *cockpit-panels* :key (lambda (p) (getf p :name)))
      (error "No cockpit panel ~S." name)))

(defun vlin (origin &rest terms)
  "ORIGIN + k1 v1 + k2 v2 + ... for TERMS (k1 v1 k2 v2 ...)."
  (let ((p origin))
    (loop for (k v) on terms by #'cddr do (setf p (vec3-add p (vec3-scale v (float k 1d0)))))
    p))

(defun panel-frame (panel)
  "(values centre normal right up): the face's centre, its unit normal (to the
eye, or turned as :turn says), its right and up, all vec3 doubles."
  (let* ((c (apply #'vec3 (getf panel :centre)))
         (turn (getf panel :turn))
         (n (if turn
                (let ((a (degrees-to-radians turn))) (vec3 (- (cos a)) (sin a) 0d0))
                (vec3-normalize (vec3-scale c -1d0))))
         (up0 (vec3 0d0 0d0 -1d0))
         (u (vec3-normalize (vec3-sub up0 (vec3-scale n (vec3-dot up0 n)))))
         (r (vec3-cross (vec3-scale n -1d0) u)))
    (values c n r u)))

(defun panel-nominal-sides (panel)
  "The bezel's left, right, top and bottom borders as the table gives them."
  (let* ((bezel (getf panel :bezel)) (border (getf bezel :border)))
    (list border border border (or (getf bezel :chin) border))))

(defparameter *panel-side-minimum* 0d0 "m: the least a bezel border is clipped to.")
(defvar *panel-sides* nil "Alist panel name -> (left right top bottom), made by clip-panel-sides.")

(defun box-penetration (a b)
  "How far the oriented boxes A and B (as panel-bezel-box's values) overlap along the
axis where it is least; negative when they are apart."
  (destructuring-bind (ca &rest da) a
    (destructuring-bind (cb &rest db) b
      (let* ((axes-a (subseq da 0 3)) (axes-b (subseq db 0 3))
             (half-a (subseq da 3)) (half-b (subseq db 3))
             (d (vec3-sub cb ca)) (best 1d9))
        (dolist (axis (append axes-a axes-b
                              (loop for x in axes-a
                                    append (loop for y in axes-b collect (vec3-cross x y))))
                      best)
          (let ((norm (vec3-norm axis)))
            (when (> norm 1d-9)
              (let* ((axis (vec3-scale axis (/ norm)))
                     (ra (loop for x in axes-a for h in half-a sum (* h (abs (vec3-dot x axis)))))
                     (rb (loop for x in axes-b for h in half-b sum (* h (abs (vec3-dot x axis))))))
                (setf best (min best (- (+ ra rb) (abs (vec3-dot d axis)))))))))))))

(defun clip-panel-sides ()
  "The panels' borders, clipped where bezels would meet: the faces of the layout
table nearly touch (a few millimetres apart), so a pair whose boxes intersect
gives up the penetration between them from the sides facing each other (not
below *panel-side-minimum*); elsewhere the table's borders stand."
  (let* ((panels (remove-if (lambda (p) (getf p :round)) *cockpit-panels*))
         (sides (mapcar (lambda (p) (cons (getf p :name) (copy-list (panel-nominal-sides p)))) panels)))
    (flet ((box (panel) (panel-bezel-box panel (cdr (assoc (getf panel :name) sides)))))
      (dotimes (pass 60)
        (let ((changed nil))
          (loop for (a . rest) on panels
                do (dolist (b rest)
                     (let ((pen (box-penetration (multiple-value-list (box a)) (multiple-value-list (box b)))))
                       (when (> pen -0.001d0)
                         (setf changed t)
                         (flet ((clip (panel other share)
                                  (multiple-value-bind (c n r u) (panel-frame panel)
                                    (declare (ignore n))
                                    (let* ((d (vec3-sub (panel-frame other) c))
                                           (hw (* 0.5d0 (first (getf panel :size))))
                                           (hh (* 0.5d0 (second (getf panel :size))))
                                           (dr (/ (vec3-dot d r) hw)) (du (/ (vec3-dot d u) hh))
                                           (index (if (> (abs dr) (abs du)) (if (> dr 0) 1 0) (if (> du 0) 2 3)))
                                           (cell (cdr (assoc (getf panel :name) sides))))
                                      (setf (nth index cell)
                                            (max *panel-side-minimum* (- (nth index cell) share)))))))
                           (let ((share (* 0.5d0 (+ pen 0.002d0))))
                             (clip a b share) (clip b a share)))))))
          (unless changed (return)))))
    sides))

(defun panel-borders (panel)
  "(values left right top bottom depth) of PANEL's bezel, metres: the table's
borders clipped where the neighbouring panels leave no room (clip-panel-sides)."
  (let ((sides (if (getf panel :round)
                   (panel-nominal-sides panel)
                   (cdr (assoc (getf panel :name)
                               (or *panel-sides* (setf *panel-sides* (clip-panel-sides))))))))
    (values (first sides) (second sides) (third sides) (fourth sides)
            (getf (getf panel :bezel) :depth))))

(defun panel-bezel-box (panel &optional sides)
  "PANEL's bezel as an oriented box: (values centre normal right up half-n half-r half-u)."
  (multiple-value-bind (c n r u) (panel-frame panel)
    (destructuring-bind (left right top bottom &rest more) (or sides (multiple-value-list (panel-borders panel)))
      (declare (ignore more))
      (let ((depth (getf (getf panel :bezel) :depth)))
        (destructuring-bind (w h) (getf panel :size)
          (let* ((half-r (+ (* 0.5d0 w) (* 0.5d0 (+ left right)))) (half-u (* 0.5d0 (+ h top bottom)))
                 (half-n (* 0.5d0 (+ depth *cockpit-bezel-front*))))
            (values (vlin c (* 0.5d0 (- right left)) r (* 0.5d0 (- top bottom)) u
                          (- *cockpit-bezel-front* half-n) n)
                    n r u half-n half-r half-u)))))))

(defun panel-grid-cap (panel)
  "The cap height of PANEL's text grid: the largest that fits its face with 10 mm margins."
  (destructuring-bind (w h) (getf panel :size)
    (destructuring-bind (columns rows) (getf panel :grid)
      (min (/ (- w 0.02d0) (* columns +text-cell-width+))
           (/ (- h 0.02d0) (* rows +text-cell-height+))))))

(defun panel-grid-origin (panel cap)
  "The text plane's origin of PANEL's grid (its first row's baseline, left),
3 mm in front of the face."
  (multiple-value-bind (c n r u) (panel-frame panel)
    (destructuring-bind (w h) (getf panel :size)
      (vlin c (- (* 0.5d0 w) 0.01d0) (vec3-scale r -1d0) (- (* 0.5d0 h) 0.01d0 cap) u 0.003d0 n))))

(defun cab-panel-box! (b panel material du dv dn size-n size-r size-u)
  (multiple-value-bind (c n r u) (panel-frame panel)
    (cab-box! b (vlin c du r dv u dn n) (list size-n size-r size-u) material :x n :y r)))

(defun cab-panel-faces! (b)
  (dolist (panel *cockpit-panels*)
    (unless (getf panel :round)
      (destructuring-bind (w h) (getf panel :size)
        (cab-panel-box! b panel (getf panel :face :screen-face) 0 0 (* -0.5d0 *cockpit-face-thickness*)
                        *cockpit-face-thickness* w h)))))

(defun cab-panel-bezels! (b)
  "Four borders and a back plate behind each face, standing 4 mm in front of it."
  (dolist (panel *cockpit-panels*)
    (unless (getf panel :round)
      (destructuring-bind (w h) (getf panel :size)
        (multiple-value-bind (left right top bottom depth) (panel-borders panel)
          (let* ((material (getf (getf panel :bezel) :material))
                 (front *cockpit-bezel-front*)
                 (centre-n (- front (* 0.5d0 (+ depth front))))
                 (across (+ w left right)))
            (flet ((bar (du dv sr su)
                     (cab-panel-box! b panel material du dv centre-n (+ depth front) sr su)))
              (bar (* 0.5d0 (- right left)) (+ (* 0.5d0 h) (* 0.5d0 top)) across top)
              (bar (* 0.5d0 (- right left)) (- (+ (* 0.5d0 h) (* 0.5d0 bottom))) across bottom)
              (bar (+ (* 0.5d0 w) (* 0.5d0 right)) 0 right h)
              (bar (- (+ (* 0.5d0 w) (* 0.5d0 left))) 0 left h))
            (cab-panel-box! b panel material (* 0.5d0 (- right left)) (* 0.5d0 (- top bottom))
                            (- (+ depth 0.002d0)) 0.004d0 across (+ h top bottom))))))))

(defun cab-panel-bolts! (b)
  "The ruggedised bezel's four bolts, and the round gauge's dial and ring."
  (dolist (panel *cockpit-panels*)
    (multiple-value-bind (c n r u) (panel-frame panel)
      (destructuring-bind (w h) (getf panel :size)
        (multiple-value-bind (left right top bottom depth) (panel-borders panel)
          (if (getf panel :round)
              (let ((radius (* 0.5d0 w)))
                (builder-add b :tube (tube-instance (vlin c (- depth) n) (vlin c *cockpit-bezel-front* n)
                                                    (+ radius left)
                                                    (cab-material-id (getf (getf panel :bezel) :material))))
                (builder-add b :tube (tube-instance (vlin c *cockpit-bezel-front* n)
                                                    (vlin c (+ *cockpit-bezel-front* 0.002d0) n) radius
                                                    (cab-material-id (getf panel :face :screen-face)))))
              (when (getf (getf panel :bezel) :bolts)
                (dolist (sr '(-1 1))
                  (dolist (su '(-1 1))
                    (let ((p (vlin c (* sr (+ (* 0.5d0 w) (* 0.5d0 (if (plusp sr) right left)))) r
                                   (* su (+ (* 0.5d0 h) (* 0.5d0 (if (plusp su) top bottom)))) u)))
                      (builder-add b :tube (tube-instance (vlin p *cockpit-bezel-front* n)
                                                          (vlin p (+ *cockpit-bezel-front* 0.006d0) n)
                                                          (min 0.006d0 (* 0.4d0 (min left right top bottom)))
                                                          (cab-material-id :steel-angle)))))))))))))

(defun cab-panel-hoods! (b)
  "A cardboard or sheet-metal sun hood over a panel's top edge, reaching towards the eye."
  (dolist (panel *cockpit-panels*)
    (let ((material (getf panel :hood)))
      (when material
        (destructuring-bind (w h) (getf panel :size)
          (multiple-value-bind (left right top) (panel-borders panel)
            (cab-panel-box! b panel material (* 0.5d0 (- right left)) (+ (* 0.5d0 h) top 0.0015d0)
                            (+ *cockpit-bezel-front* (* 0.5d0 *cockpit-hood-length*))
                            *cockpit-hood-length* (+ w left right) 0.003d0)))))))

(defun closest-on-segment (p a b)
  (let* ((ab (vec3-sub b a))
         (tt (max 0d0 (min 1d0 (/ (vec3-dot (vec3-sub p a) ab) (vec3-dot ab ab))))))
    (vec3-add a (vec3-scale ab tt))))

(defun cockpit-rack-member (name)
  (or (assoc name *cockpit-rack*) (error "No rack member ~S." name)))

(defun rack-member-ends (member)
  (values (apply #'vec3 (second member)) (apply #'vec3 (third member))))

(defun panel-bracket-anchors (panel)
  "Two points on PANEL's bezel the brackets start from: behind it, left and
right, or (a panel on the wall) at the ends of its bezel's face."
  (multiple-value-bind (c n r u) (panel-frame panel)
    (destructuring-bind (w h) (getf panel :size)
      (multiple-value-bind (left right top bottom depth) (panel-borders panel)
        (cond ((getf panel :wall)
               (list (vlin c (+ (* 0.5d0 h) (* 0.5d0 top)) u)
                     (vlin c (- (+ (* 0.5d0 h) (* 0.5d0 bottom))) u)))
              ((getf panel :round)
               (list (vlin c (* 0.3d0 w) r (- depth) n) (vlin c (* -0.3d0 w) r (- depth) n)))
              (t (list (vlin c (+ (* 0.3d0 w) (* 0.5d0 (- right left))) r (- depth) n)
                       (vlin c (+ (* -0.3d0 w) (* 0.5d0 (- right left))) r (- depth) n))))))))

(defun panel-brackets (panel)
  "PANEL's L-brackets as (P M Q member): P on the bezel, M the corner (P moved
along the normal), Q on the nearest rack member's surface (or the front
wall, member :wall)."
  (multiple-value-bind (c n) (panel-frame panel)
    (declare (ignore c))
    (loop for p in (panel-bracket-anchors panel)
          collect (let ((best nil) (best-q nil) (best-d 1d9) (half (* 0.5d0 *cockpit-rack-section*)))
                    (dolist (member *cockpit-rack*)
                      (multiple-value-bind (a b) (rack-member-ends member)
                        (let* ((m (closest-on-segment p a b)) (d (vec3-norm (vec3-sub p m))))
                          (when (< d best-d)
                            (setf best-d d best member
                                  best-q (vec3-add m (vec3-scale (vec3-normalize (vec3-sub p m)) half)))))))
                    (let ((wall-d (abs (- (aref p 0) (getf *cab-window* :x)))))
                      (when (< wall-d best-d)
                        (setf best :wall best-q (vec3 (getf *cab-window* :x) (aref p 1) (aref p 2)))))
                    (let ((m (vec3-add p (vec3-scale n (vec3-dot (vec3-sub best-q p) n)))))
                      (list p m best-q (if (eq best :wall) :wall (first best))))))))

(defun cab-flat! (b from to material hint &optional (width 0.025d0) (thickness 0.004d0))
  "A flat bar from FROM to TO, WIDTH wide across HINT's direction."
  (let ((length (vec3-norm (vec3-sub to from))))
    (when (> length 0.002d0)
      (cab-box! b (vec3-scale (vec3-add from to) 0.5d0) (list length width thickness) material
                :x (vec3-sub to from) :y hint))))

(defun cab-panel-brackets! (b)
  (dolist (panel *cockpit-panels*)
    (multiple-value-bind (c n r) (panel-frame panel)
      (declare (ignore c))
      (dolist (bracket (panel-brackets panel))
        (destructuring-bind (p m q member) bracket
          (declare (ignore member))
          (cab-flat! b p m :steel-angle r)
          (cab-flat! b m q :steel-angle n))))))

(defun rack-material (a b)
  (let ((d (vec3-sub b a)))
    (cond ((and (> (abs (aref d 2)) (abs (aref d 0))) (> (abs (aref d 2)) (abs (aref d 1)))) :channel-z)
          ((> (abs (aref d 1)) (abs (aref d 0))) :channel-y)
          (t :channel-x))))

(defun cab-rack! (b)
  (dolist (member *cockpit-rack*)
    (multiple-value-bind (a e) (rack-member-ends member)
      (cab-box! b (vec3-scale (vec3-add a e) 0.5d0)
                (list (vec3-norm (vec3-sub e a)) *cockpit-rack-section* *cockpit-rack-section*)
                (rack-material a e) :x (vec3-sub e a)
                :y (if (> (abs (aref (vec3-sub e a) 1)) 0.5d0) (vec3 0d0 0d0 1d0) (vec3 0d0 1d0 0d0))))))

(defun cab-tray! (b)
  (destructuring-bind (&key x z y width height) *cockpit-tray*
    (cab-box! b (vec3 x 0d0 z) (list width (* 2 y) height) :sheet-metal)))

;;; Cables

(defun cable-points (a b &optional (segments 6))
  "SEGMENTS + 1 points from A to B sagging down (+z) by 8 % of the span at the middle."
  (let ((drop (* 0.08d0 (vec3-norm (vec3-sub b a)))))
    (loop for i to segments
          for s = (/ i (float segments 1d0))
          collect (vec3-add (vec3-add (vec3-scale a (- 1 s)) (vec3-scale b s))
                            (vec3 0d0 0d0 (* drop 4 s (- 1 s)))))))

(defun cable-radius (index)
  "Ø 15 to 25 mm in turn."
  (nth (mod index 3) '(0.0075d0 0.010d0 0.0125d0)))

(defun panel-cable-start (panel k count radius)
  (multiple-value-bind (c n r u) (panel-frame panel)
    (destructuring-bind (w h) (getf panel :size)
      (multiple-value-bind (left right top bottom depth) (panel-borders panel)
        (declare (ignore left right top))
        (let ((spread (* (- k (* 0.5d0 (1- count))) 0.03d0)))
          (if (getf panel :wall)
              (vlin c (- (+ (* 0.5d0 h) bottom 0.02d0)) u spread r 0.015d0 n)
              (vlin c (- depth radius) n (* -0.3d0 h) u spread r)))))))

(defun cockpit-cables ()
  "Every panel's cables as (panel material radius points): from its back to the tray."
  (let ((index 0) (cables '()))
    (dolist (panel *cockpit-panels*)
      (let* ((colours (getf panel :cables)) (count (length colours)))
        (loop for colour in colours for k from 0
              do (let* ((radius (cable-radius index))
                        (start (panel-cable-start panel k count radius))
                        (end (vec3 (+ (getf *cockpit-tray* :x) (* 0.02d0 (- k 1)))
                                   (max -0.8d0 (min 0.8d0 (+ (aref start 1) (* 0.03d0 (- k 1)))))
                                   (- (getf *cockpit-tray* :z) (* 0.5d0 (getf *cockpit-tray* :height)) radius))))
                   (push (list (getf panel :name) colour radius (cable-points start end)) cables)
                   (incf index)))))
    (nreverse cables)))

(defun cockpit-trunks ()
  "The cables' way on from the tray: up the wall at -y and aft along the ceiling
conduit, one tube a colour: (material radius points)."
  (let* ((tray-x (getf *cockpit-tray* :x)) (tray-z (- (getf *cockpit-tray* :z) 0.02d0))
         (conduit (cab-wall-point 0d0 (degrees-to-radians 225d0) *cockpit-conduit-radius*))
         (y (aref conduit 1)) (z (aref conduit 2))
         (x1 (first *cab-x-range*)))
    (loop for colour in '(:cable-black :cable-grey :cable-red :cable-yellow)
          for k from 0
          collect (let ((dx (* 0.03d0 k)))
                    (list colour 0.012d0
                          (list (vec3 (+ tray-x dx) (- (getf *cockpit-tray* :y) 0.05d0) tray-z)
                                (vec3 (+ tray-x dx) y tray-z)
                                (vec3 (+ tray-x dx) y (+ z (* 0.02d0 k)))
                                (vec3 (+ x1 0.2d0) y (+ z (* 0.02d0 k)))))))))

(defun cab-cables! (b)
  (dolist (cable (cockpit-cables))
    (destructuring-bind (name colour radius points) cable
      (declare (ignore name))
      (loop for (p q) on points while q
            do (builder-add b :tube (tube-instance p q radius (cab-material-id colour))))))
  (dolist (trunk (cockpit-trunks))
    (destructuring-bind (colour radius points) trunk
      (loop for (p q) on points while q
            do (builder-add b :tube (tube-instance p q radius (cab-material-id colour)))))))

(defun polyline-point (points distance)
  "The point DISTANCE m along POINTS and the unit direction there, or nil past the end."
  (loop for (p q) on points while q
        do (let ((length (vec3-norm (vec3-sub q p))))
             (if (<= distance length)
                 (return (values (vec3-add p (vec3-scale (vec3-sub q p) (/ distance length)))
                                 (vec3-normalize (vec3-sub q p))))
                 (decf distance length)))))

(defun cab-zip-ties! (b)
  "A zip tie every 0.2 m along each cable: a thin ring a little thicker than it."
  (dolist (cable (cockpit-cables))
    (destructuring-bind (name colour radius points) cable
      (declare (ignore name colour))
      (loop for distance from 0.1d0 by 0.2d0
            do (multiple-value-bind (p d) (polyline-point points distance)
                 (unless p (return))
                 (builder-add b :tube (tube-instance (vec3-sub p (vec3-scale d 0.002d0))
                                                     (vec3-add p (vec3-scale d 0.002d0))
                                                     (+ radius 0.003d0) (cab-material-id :cable-black))))))))

(defun cab-kapton! (b)
  "Amber Kapton tape on each cable's joints: where it leaves the panel and where it meets the tray."
  (dolist (cable (cockpit-cables))
    (destructuring-bind (name colour radius points) cable
      (declare (ignore name colour))
      (let ((first (first points)) (second (second points))
            (last (car (last points))) (before (car (last points 2))))
        (flet ((wrap (from towards)
                 (let ((d (vec3-normalize (vec3-sub towards from))))
                   (builder-add b :tube (tube-instance (vec3-add from (vec3-scale d 0.01d0))
                                                       (vec3-add from (vec3-scale d 0.05d0))
                                                       (+ radius 0.002d0) (cab-material-id :kapton))))))
          (wrap first second)
          (wrap last before)))))
  (dolist (trunk (cockpit-trunks))
    (destructuring-bind (colour radius points) trunk
      (declare (ignore colour))
      (loop for (p q) on (rest points) while q
            do (let ((d (vec3-normalize (vec3-sub q p))))
                 (builder-add b :tube (tube-instance (vec3-sub p (vec3-scale d 0.02d0))
                                                     (vec3-add p (vec3-scale d 0.02d0))
                                                     (+ radius 0.002d0) (cab-material-id :kapton))))))))

;;; Tape labels

(defparameter *cockpit-labels*
  '((:text "ORBIT" :panel :orbit :side :above)
    (:text "ATT [WASD QE]" :panel :attitude :side :above)
    (:text "TGT NEXT ] PREV [ CLR \\" :panel :target :side :above)
    (:text "AUTOPILOT [O] [RET]" :panel :autopilot)
    (:text "SHIP  XFER [B]" :panel :ship)
    (:text "COMMS  ACK [Y]" :panel :comms)
    (:text "WARP [. , /] PAUSE [P]" :panel :strip :side :above)
    (:text "COMPUTER  DAMP [T]" :panel :buttons)
    (:text "THR [SHFT/CTRL]" :panel :throttle :side :above)
    (:text "[Z] MAX [X] CUT" :panel :throttle)
    (:text "HULL TEMP" :panel :hull-gauge)
    (:text "ACK [Y]" :panel :caution-lamps)
    (:text "STATUS" :panel :status-lamps)
    (:text "VIEW [V]  MAP [M]  HOME [HOME]" :window t))
  "The tape labels (docs/design.md \"The DIY look\", decision 126), each beside
its panel: :at (du dv) from the panel's centre in its plane, else :side :above
its bezel's top edge or (the default) under its chin. The upper row's tapes go
above: the console's lower panels stand right under them, so a strip below
would be hidden.")

(defparameter *cockpit-label-keyless* '("ORBIT" "STATUS" "HULL TEMP")
  "The labels without a bracketed key (decision 126).")

(defparameter *cab-label*
  '(:height 0.05d0 :window-height 0.04d0 :margin 0.015d0 :rows 5 :ink (237 237 237)
    :cap-per-distance 0.023d0 :max-angle 1.9d0)
  "Tape 50 mm tall in five rows (ragged ends), the text a white marker's, its
cap 2.3 % of the distance (the readability rule's 1.75 % with a margin).")

(defun label-angle (text)
  "A fixed pseudo-random angle of TEXT's tape within +-2 degrees, radians."
  (* (degrees-to-radians (getf *cab-label* :max-angle))
     (sin (* 1.7d0 (reduce #'+ (map 'list #'char-code text))))))

(defun label-frame (label)
  "(values centre normal right up height) of LABEL's tape, turned by its angle in its plane."
  (let ((angle (label-angle (getf label :text))))
    (multiple-value-bind (centre n r u height)
        (if (getf label :window)
            (values (vec3 0.838d0 0d0 -0.325d0) (vec3 -1d0 0d0 0d0) (vec3 0d0 1d0 0d0) (vec3 0d0 0d0 -1d0)
                    (getf *cab-label* :window-height))
            (let ((panel (cockpit-panel (getf label :panel))) (height (getf *cab-label* :height)))
              (multiple-value-bind (c n r u) (panel-frame panel)
                (destructuring-bind (w h) (getf panel :size)
                  (declare (ignore w))
                  (multiple-value-bind (left right border chin) (panel-borders panel)
                    (declare (ignore left right))
                    (destructuring-bind (du dv) (or (getf label :at)
                                                    (list 0d0 (if (eq (getf label :side) :above)
                                                                  (+ (* 0.5d0 h) border 0.01d0 (* 0.5d0 height))
                                                                  (- (+ (* 0.5d0 h) chin 0.01d0 (* 0.5d0 height))))))
                      (values (vlin c du r dv u (+ *cockpit-bezel-front* 0.002d0) n) n r u height)))))))
      (values centre n
              (vec3-add (vec3-scale r (cos angle)) (vec3-scale u (sin angle)))
              (vec3-sub (vec3-scale u (cos angle)) (vec3-scale r (sin angle)))
              height))))

(let ((loaders (make-hash-table)))
  (defun label-loader (key)
    (or (gethash key loaders)
        (setf (gethash key loaders) (luv.slug:slug-font-loader (font-path key))))))

(defun label-metrics (label)
  "(values cap advance length) of LABEL: the text's cap height in metres, its
shaped advance, and its tape's length."
  (let* ((text (getf label :text))
         (loader (label-loader :label))
         (upm (zpb-ttf:units/em loader))
         (cap-em (/ (luv.slug:slug-font-cap-height loader) upm))
         (centre (label-frame label))
         (cap (* (getf *cab-label* :cap-per-distance) (vec3-norm centre)))
         (shaped (luv.slug:slug-shaped-text-for (font-path :label) text))
         (advance (* (/ cap cap-em) (/ (luv.slug:slug-shaped-text-x-advance shaped)
                                       (luv.slug:slug-shaped-text-units-per-em shaped)))))
    (values cap advance (+ advance (* 2 (getf *cab-label* :margin))))))

(defun label-ragged-ends (text row)
  "(values left right) the lengths in metres row ROW of TEXT's tape is cut short at its ends."
  (flet ((frac (seed) (let ((x (* 43758.5453d0 (sin (* seed 12.9898d0))))) (- x (floor x)))))
    (let ((seed (+ (reduce #'+ (map 'list #'char-code text)) (* 7 row))))
      (values (* 0.006d0 (frac seed)) (* 0.006d0 (frac (+ seed 0.5d0)))))))

(defun cab-labels! (b)
  "The tape strips: five rows of grey gaffer tape, 2 mm thick, ragged at the ends."
  (dolist (label *cockpit-labels*)
    (multiple-value-bind (centre n r u height) (label-frame label)
      (multiple-value-bind (cap advance length) (label-metrics label)
        (declare (ignore cap advance))
        (let* ((rows (getf *cab-label* :rows)) (row-height (/ height rows)))
          (dotimes (i rows)
            (multiple-value-bind (left right) (label-ragged-ends (getf label :text) i)
              (cab-box! b (vlin centre (* 0.5d0 (- left right)) r (* (- i (* 0.5d0 (1- rows))) row-height) u)
                        (list 0.002d0 (- length left right) (+ row-height 0.0006d0)) :tape
                        :x n :y r))))))))

;;; The text regions: the labels' tape letters and the screens' placeholders.

(defun cockpit-text-region (key)
  (cdr (assoc key *cab-text-regions* :test #'equal)))

(defparameter *cockpit-text-screens* '(:orbit :ship :comms)
  "The screens of text plan 32 has drawn so far (cockpit-screens.lisp).")

(defparameter *cockpit-live-screens* (append *cockpit-text-screens* '(:strip))
  "The panels plan 32 has drawn so far (the strip with dots); a placeholder stands on each other screen.")

(defparameter *placeholder-text* "PLACEHOLDER(166)")
(defparameter *placeholder-ink* '(232 228 216) "Paper white (docs/design.md \"Readability\").")

(defun cockpit-labels-text! (atlas batch)
  (dolist (label *cockpit-labels*)
    (multiple-value-bind (centre n r u) (label-frame label)
      (let* ((cap (label-metrics label)) (text (getf label :text))
             (region (text-batch-region batch (length text)))
             (origin (vlin centre 0.004d0 n (* -0.5d0 cap) u)))
        (shaped-run! atlas region 0
                     (make-text-plane :origin (coerce origin 'list) :right (coerce r 'list) :up (coerce u 'list))
                     text :font :label :cap (float cap 1f0) :kind :lit :align :centre
                          :colour (apply #'text-colour (getf *cab-label* :ink)))
        (push (cons (list :label text) region) *cab-text-regions*)))))

(defun cockpit-placeholders-text! (atlas batch)
  (dolist (panel *cockpit-panels*)
    (when (and (getf panel :screen)
               (not (member (getf panel :name) *cockpit-live-screens*)))
      (let* ((cap (panel-grid-cap panel)) (origin (panel-grid-origin panel cap))
             (region (text-batch-region batch (length *placeholder-text*))))
        (multiple-value-bind (c n r u) (panel-frame panel)
          (declare (ignore c n))
          (grid-run! atlas region 0
                     (make-text-plane :origin (coerce origin 'list) :right (coerce r 'list) :up (coerce u 'list))
                     *placeholder-text* :font :mono :cap (float cap 1f0) :kind :emissive
                                        :colour (apply #'text-colour *placeholder-ink*)))
        (push (cons (list :placeholder (getf panel :name)) region) *cab-text-regions*)))))

(defparameter *cab-console-parts*
  '("rack" "tray" "panel-faces" "bezels" "bezel-bolts" "hoods" "brackets" "cables" "zip-ties"
    "kapton" "labels")
  "The model's parts of the console, in the order cab-console! adds them.")

(defun cab-console! (b)
  "The console's parts."
  (builder-part b "rack" :box (lambda () (cab-rack! b)))
  (builder-part b "tray" :box (lambda () (cab-tray! b)))
  (builder-part b "panel-faces" :box (lambda () (cab-panel-faces! b)))
  (builder-part b "bezels" :box (lambda () (cab-panel-bezels! b)))
  (builder-part b "bezel-bolts" :tube (lambda () (cab-panel-bolts! b)))
  (builder-part b "hoods" :box (lambda () (cab-panel-hoods! b)))
  (builder-part b "brackets" :box (lambda () (cab-panel-brackets! b)))
  (builder-part b "cables" :tube (lambda () (cab-cables! b)))
  (builder-part b "zip-ties" :tube (lambda () (cab-zip-ties! b)))
  (builder-part b "kapton" :tube (lambda () (cab-kapton! b)))
  (builder-part b "labels" :box (lambda () (cab-labels! b))))

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
