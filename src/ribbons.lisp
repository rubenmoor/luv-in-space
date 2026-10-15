(in-package #:luv-in-space)

;;; The map's osculating orbit as a screen-space ribbon, its CPU half
;;; (decision 84: Map ribbons and markers: osculating orbit, adaptive
;;; segments, overlay markers and visibility): points in double relative to
;;; Earth's centre, recomputed each frame and split by their error in pixels
;;; (docs/architecture.md "Floating-origin pitfalls"), then one instance per
;;; segment with both endpoints eye-relative in single.

(defparameter *ribbon-base-segments* 256 "Segments uniform in the anomaly before splitting; decision 84.")
(defparameter *ribbon-max-split* 8 "A segment splits into at most 2^this pieces; decision 84.")
(defparameter *ribbon-max-error-px* 0.25d0 "A piece's sagitta seen from the eye, px; decision 84.")
(defparameter *ribbon-max-segments* 8192 "Pieces in all; decision 84.")
(defparameter *ribbon-colour* #x40D0FF "Cyan, sRGB; decision 84.")
(defparameter *ribbon-crash-colour* #xFF3030 "Red below the crash radius, sRGB; decision 84.")
(declaim (type fixnum *ribbon-base-segments* *ribbon-max-split* *ribbon-max-segments*)
         (type double-float *ribbon-max-error-px*)
         (type (unsigned-byte 24) *ribbon-colour* *ribbon-crash-colour*))

(defconstant +ribbon-instance-floats+ 12
  "Per segment: the start (x y z 1), the end (x y z 1), the linear colour (r g b 1).")

(deftype doubles () '(simple-array double-float (*)))

(defstruct (ribbon (:constructor %make-ribbon))
  "Preallocated buffers for one ribbon of at most CAPACITY segments: COUNT
segments, their COUNT + 1 POINTS (x y z, double, relative to the body) with
each point's RADIUS and ANOMALY (E or H), each segment's MID-RADII at its
mid-anomaly, and its INSTANCES; per base segment its split k, eye distance
and an index order for the budget; ORBIT, the frame's osculating orbit."
  (capacity 0 :type fixnum)
  (count 0 :type fixnum)
  (points nil :type doubles)
  (radii nil :type doubles)
  (anomalies nil :type doubles)
  (mid-radii nil :type doubles)
  (instances nil :type (simple-array single-float (*)))
  (splits nil :type (simple-array fixnum (*)))
  (distances nil :type doubles)
  (order nil :type (simple-array fixnum (*)))
  (p-axis (make-vec3) :type vec3)
  (q-axis (make-vec3) :type vec3)
  (scratch (make-vec3) :type vec3)
  (orbit (make-orbit 0d0 0d0 0d0 0d0 0d0 0d0 0d0) :type orbit))

(defun make-ribbon (&key (capacity *ribbon-max-segments*) (base *ribbon-base-segments*))
  (flet ((doubles (n) (make-array n :element-type 'double-float :initial-element 0d0))
         (fixnums (n) (make-array n :element-type 'fixnum :initial-element 0)))
    (%make-ribbon :capacity capacity
                  :points (doubles (* 3 (1+ capacity)))
                  :radii (doubles (1+ capacity))
                  :anomalies (doubles (1+ capacity))
                  :mid-radii (doubles capacity)
                  :instances (make-array (* +ribbon-instance-floats+ capacity)
                                         :element-type 'single-float :initial-element 0f0)
                  :splits (fixnums base)
                  :distances (doubles base)
                  :order (fixnums base))))

(defun camera-focal-px (camera height)
  "The focal length in pixels of a frame HEIGHT pixels tall."
  (/ (* 0.5d0 (float height 1d0)) (tan (* 0.5d0 (camera-field-of-view camera)))))

(defun orbit-ribbon-points! (ribbon orbit eye-rel-body focal-px)
  "Fill RIBBON with ORBIT's points relative to its body: an ellipse as
*ribbon-base-segments* segments uniform in E from periapsis, a hyperbola from
−H_s to H_s where r reaches Earth's Laplace sphere; each segment split into
2^k pieces (k ≤ *ribbon-max-split*) so that its sagitta chord² / (8 ρ) over
its distance from EYE-REL-BODY (the eye relative to the body) times FOCAL-PX
is at most *ribbon-max-error-px*, k lowered from the farthest segments first
while the pieces exceed *ribbon-max-segments*. Returns the segment count.
Conses nothing."
  (declare (optimize speed) (type ribbon ribbon) (type orbit orbit)
           (type vec3 eye-rel-body) (type double-float focal-px))
  (let* ((e (orbit-e orbit))
         (ellipse (< e 1d0))
         (a (/ (orbit-q orbit) (abs (- 1d0 e))))
         (b (* a (sqrt (abs (- 1d0 (* e e))))))
         (base (min *ribbon-base-segments* (length (ribbon-splits ribbon))))
         (budget (min *ribbon-max-segments* (ribbon-capacity ribbon)))
         (max-split (max 0 (min 30 *ribbon-max-split*)))
         (lo (if ellipse
                 0d0
                 (let ((c (max 1d0 (/ (+ (/ (the double-float (body-soi-radius (body :earth))) a) 1d0) e))))
                   (- (log (+ c (sqrt (- (* c c) 1d0))))))))
         (hi (if ellipse (* 2 pi) (- lo)))
         (step (/ (- hi lo) base))
         (pv (ribbon-p-axis ribbon)) (qv (ribbon-q-axis ribbon))
         (points (ribbon-points ribbon)) (radii (ribbon-radii ribbon))
         (anomalies (ribbon-anomalies ribbon)) (mid-radii (ribbon-mid-radii ribbon))
         (splits (ribbon-splits ribbon)) (distances (ribbon-distances ribbon))
         (order (ribbon-order ribbon))
         (ex (aref eye-rel-body 0)) (ey (aref eye-rel-body 1)) (ez (aref eye-rel-body 2)))
    (declare (type double-float e a b lo hi step ex ey ez) (type fixnum base budget) (type (integer 0 30) max-split))
    (perifocal-axes! pv qv (orbit-i orbit) (orbit-raan orbit) (orbit-argp orbit))
    (labels ((perifocal (x)
               (if ellipse
                   (values (* a (- (cos x) e)) (* b (sin x)))
                   (values (* a (- e (cosh x))) (* b (sinh x)))))
             (radius (x)
               (if ellipse (* a (- 1d0 (* e (cos x)))) (* a (- (* e (cosh x)) 1d0))))
             (curvature-radius (x)
               (multiple-value-bind (s c)
                   (if ellipse (values (sin x) (cos x)) (values (sinh x) (cosh x)))
                 (let ((u (the (double-float 0d0) (+ (* a a s s) (* b b c c)))))
                   (/ (* u (sqrt u)) (* a b)))))
             (world (x i)
               (multiple-value-bind (px py) (perifocal x)
                 (values (+ (* px (aref pv i)) (* py (aref qv i))))))
             (eye-distance (x0 x1)
               ;; from the eye to the chord from x0 to x1
               (let* ((ax (- (world x0 0) ex)) (ay (- (world x0 1) ey)) (az (- (world x0 2) ez))
                      (dx (- (world x1 0) (world x0 0)))
                      (dy (- (world x1 1) (world x0 1)))
                      (dz (- (world x1 2) (world x0 2)))
                      (dd (+ (* dx dx) (* dy dy) (* dz dz)))
                      (s (if (> dd 0d0)
                             (max 0d0 (min 1d0 (/ (- (+ (* ax dx) (* ay dy) (* az dz))) dd)))
                             0d0))
                      (cx (+ ax (* s dx))) (cy (+ ay (* s dy))) (cz (+ az (* s dz))))
                 (values (sqrt (+ (* cx cx) (* cy cy) (* cz cz))) dd))))
      (declare (inline perifocal radius curvature-radius world eye-distance))
      ;; k per base segment from its sagitta seen from the eye
      (let ((total 0))
        (declare (type fixnum total))
        (dotimes (j base)
          (let* ((x0 (+ lo (* j step))) (x1 (+ x0 step)))
            (multiple-value-bind (distance chord2) (eye-distance x0 x1)
              (let* ((sagitta (/ chord2 (* 8d0 (curvature-radius (+ x0 (* 0.5d0 step))))))
                     (error-px (/ (* sagitta focal-px) (max 1d-3 (- distance sagitta))))
                     (k 0))
                (declare (type fixnum k) (type double-float error-px))
                (loop while (and (> error-px *ribbon-max-error-px*) (< k max-split))
                      do (setf error-px (* 0.25d0 error-px)) (incf k))
                (setf (aref splits j) k (aref distances j) distance (aref order j) j)
                (incf total (ash 1 (the (integer 0 30) k)))))))
        (when (> total budget)
          ;; farthest first: order by distance, descending (insertion sort)
          (loop for i from 1 below base
                do (let ((key (aref order i)) (m (1- i)))
                     (declare (type fixnum key m))
                     (loop while (and (>= m 0) (< (aref distances (aref order m)) (aref distances key)))
                           do (setf (aref order (1+ m)) (aref order m)) (decf m))
                     (setf (aref order (1+ m)) key)))
          (loop for i below base
                while (> total budget)
                do (let ((j (aref order i)))
                     (loop while (and (> total budget) (> (aref splits j) 0))
                           do (decf (aref splits j))
                              (decf total (ash 1 (the (integer 0 30) (aref splits j)))))))))
      ;; the points
      (let ((index 0))
        (declare (type (integer 0 #.(floor most-positive-fixnum 4)) index))
        (flet ((put (x)
                 (multiple-value-bind (px py) (perifocal x)
                   (dotimes (i 3)
                     (setf (aref points (+ (* 3 index) i))
                           (+ (* px (aref pv i)) (* py (aref qv i))))))
                 (setf (aref radii index) (radius x) (aref anomalies index) x)))
          (declare (inline put))
          (dotimes (j base)
            (let* ((pieces (ash 1 (the (integer 0 30) (aref splits j))))
                   (piece (/ step pieces))
                   (x0 (+ lo (* j step))))
              (declare (type fixnum pieces) (type double-float piece x0))
              (dotimes (i pieces)
                (let ((x (+ x0 (* i piece))))
                  (put x)
                  (setf (aref mid-radii index) (radius (+ x (* 0.5d0 piece))))
                  (incf index)))))
          (put hi))
        (setf (ribbon-count ribbon) index)))))

(declaim (inline srgb-channel-linear))
(defun srgb-channel-linear (rgb shift)
  "The linear value of RGB's 8-bit sRGB channel at SHIFT."
  (declare (type (unsigned-byte 24) rgb) (type (integer 0 16) shift))
  (let ((c (/ (float (ldb (byte 8 shift) rgb) 1d0) 255d0)))
    (if (<= c 0.04045d0)
        (/ c 12.92d0)
        (expt (the (double-float 0d0) (/ (+ c 0.055d0) 1.055d0)) 2.4d0))))

(defun orbit-ribbon-instances! (ribbon eye-rel-body
                                &optional (crash-radius (body-crash-radius (body :earth))))
  "Write one instance per segment of RIBBON: both endpoints (point − EYE-REL-BODY,
i.e. point + body − eye) in double, then single, and the linear colour, red
where the segment's mid-anomaly radius is below CRASH-RADIUS. Returns the
segment count. Conses nothing."
  (declare (optimize speed) (type ribbon ribbon) (type vec3 eye-rel-body)
           (type double-float crash-radius))
  (let ((points (ribbon-points ribbon)) (mid-radii (ribbon-mid-radii ribbon))
        (out (ribbon-instances ribbon)) (p (ribbon-scratch ribbon))
        (count (ribbon-count ribbon)))
    (macrolet ((colour (rgb)
                 `(values (coerce (srgb-channel-linear ,rgb 16) 'single-float)
                          (coerce (srgb-channel-linear ,rgb 8) 'single-float)
                          (coerce (srgb-channel-linear ,rgb 0) 'single-float))))
      (multiple-value-bind (cr cg cb) (colour *ribbon-colour*)
        (multiple-value-bind (dr dg db) (colour *ribbon-crash-colour*)
          (dotimes (s count)
            (let ((o (* s +ribbon-instance-floats+)))
              (declare (type fixnum o))
              (dotimes (end 2)
                (let ((k (* 3 (+ s end))))
                  (vec3-set! p (aref points k) (aref points (+ k 1)) (aref points (+ k 2))))
                (eye-relative-single p eye-rel-body out (+ o (* 4 end)))
                (setf (aref out (+ o (* 4 end) 3)) 1f0))
              (if (< (aref mid-radii s) crash-radius)
                  (setf (aref out (+ o 8)) dr (aref out (+ o 9)) dg (aref out (+ o 10)) db)
                  (setf (aref out (+ o 8)) cr (aref out (+ o 9)) cg (aref out (+ o 10)) cb))
              (setf (aref out (+ o 11)) 1f0))))))
    count))

;;; The GPU half: the :ribbon live pipeline and the draw in the map's scene
;;; pass, after the meshes (docs/architecture.md "Rendering precision").

(defparameter *ribbon-width-px* 2.0d0 "The line's width in pixels; decision 84.")
(defparameter *ribbon-radiance* 1.0d0 "Times the colour, over the exposure; decision 84.")

(defun ribbon-corners ()
  "The 6-vertex quad: along 0 or 1, across ±1."
  (map '(simple-array single-float (*)) (lambda (x) (float x 1f0)) '(0 -1 1 -1 1 1 0 -1 1 1 0 1)))

(defun ribbon-vertex-buffers ()
  `((:array-stride 8 :attributes ((:shader-location 0 :offset 0 :format :float32x2)))
    (:array-stride ,(* 4 +ribbon-instance-floats+) :step-mode :instance
     :attributes ((:shader-location 1 :offset 0 :format :float32x4)
                  (:shader-location 2 :offset 16 :format :float32x4)
                  (:shader-location 3 :offset 32 :format :float32x4)))))

(defun ribbon-pipeline (renderer layout format)
  "The :ribbon live pipeline into FORMAT: additive, depth compared :greater,
not written (decision 4: Depth strategy)."
  (live-pipeline renderer :ribbon layout (ribbon-vertex-buffers)
                 :format format :blend :premultiplied-alpha
                 :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                  :depth-compare :greater)))

(defun make-ribbon-buffer (device ribbon)
  "A target's instance buffer for RIBBON's capacity."
  (luv:create device (luv:make-buffer-descriptor
                      :label "ribbon" :size (* 4 (length (ribbon-instances ribbon)))
                      :usage '(:vertex :copy-dst))))

(defun encode-ribbon (pass renderer bind-group ribbon buffer corners state camera eye height)
  "The ship's osculating orbit about Earth into the open scene PASS: points
and instances for the eye EYE (relative to Earth's centre) and a frame
HEIGHT px tall, written to BUFFER. Nothing for a state without an orbit
plane (r × v = 0)."
  (let ((r (render-state-r state)) (v (render-state-v state)) (h (ribbon-scratch ribbon)))
    (when (> (vec3-norm (vec3-cross! h r v)) (* 1d-9 (vec3-norm r) (vec3-norm v)))
      (orbit-ribbon-points! ribbon (state->orbit-into! (ribbon-orbit ribbon) r v +mu-earth+
                                                       (render-state-t state))
                            eye (camera-focal-px camera height))
      (let ((count (orbit-ribbon-instances! ribbon eye)))
        (when (plusp count)
          (luv:write-buffer buffer (ribbon-instances ribbon))
          (luv:set-pipeline pass (renderer-pipeline renderer :ribbon))
          (luv:set-bind-group pass 0 bind-group)
          (luv:set-vertex-buffer pass 0 corners)
          (luv:set-vertex-buffer pass 1 buffer)
          (luv:draw pass 6 count))))))

;;; The map's markers (decision 84): the ship, the apsides, the impact and
;;; bodies below 6 px, each a pixel position, a colour and a label, from the
;;; render state's r, v in double; drawn by overlay.lisp after the tone map.

(defparameter *marker-body-px* 6d0 "Bodies whose disc is below this many px get a marker; decision 84.")
(defparameter *marker-label-gap-px* 8 "The label's left edge right of the marker's centre, px; decision 84.")
(defparameter *marker-colour* #x40D0FF "The ship and the apsides, cyan, sRGB; decision 84.")
(defparameter *marker-impact-colour* #xFF3030 "The impact, red, sRGB; decision 84.")
(defparameter *marker-body-colour* #x8A8A80 "Bodies, grey, sRGB; decision 84.")
(defparameter *marker-label-colour* #xE8E4D8 "Labels, paper white, sRGB; decision 84.")
(defparameter *marker-ship-label* "M-07" "decision 49: Story: names, call signs and ships.")
(declaim (type double-float *marker-body-px*)
         (type fixnum *marker-label-gap-px*)
         (type (unsigned-byte 24) *marker-colour* *marker-impact-colour* *marker-body-colour*
               *marker-label-colour*))

(defstruct (map-marker (:constructor make-map-marker ()))
  "KEY (:ship :apoapsis :periapsis :impact :earth :moon :sun), the shape
KIND, pixel X, Y from the top left, the sRGB COLOUR, LABEL or nil, the
POINT relative to Earth's centre and, for the impact, its TIME; where
overlay.lisp placed the label: LABEL-SIDE (nil when dropped), its left
edge LABEL-X."
  (key :ship :type symbol)
  (kind :ring :type symbol)
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (colour 0 :type (unsigned-byte 24))
  (label nil :type (or null string))
  (point (make-vec3) :type vec3)
  (time 0d0 :type double-float)
  (label-side nil :type (member nil :right :left))
  (label-x 0d0 :type double-float))

(defconstant +map-marker-capacity+ 7)

(defun label-buffer ()
  (make-array 32 :element-type 'character :fill-pointer 0))

(defstruct (map-markers (:constructor make-map-markers ()))
  "One frame's markers: the first COUNT of ITEMS; the apsides' label
strings and scratch vectors, preallocated."
  (count 0 :type fixnum)
  (items (coerce (loop repeat +map-marker-capacity+ collect (make-map-marker)) 'simple-vector)
   :type simple-vector)
  (apoapsis-label (label-buffer) :type string)
  (periapsis-label (label-buffer) :type string)
  (eye (make-vec3) :type vec3)
  (forward (make-vec3) :type vec3)
  (right (make-vec3) :type vec3)
  (down (make-vec3) :type vec3)
  (h (make-vec3) :type vec3)
  (p-axis (make-vec3) :type vec3)
  (q-axis (make-vec3) :type vec3)
  (scratch (make-vec3) :type vec3))

(defvar *map-markers* (make-map-markers) "map-markers' default output.")

(defun map-marker-ref (markers key)
  "The marker KEY among MARKERS' current ones, or nil."
  (loop for i below (map-markers-count markers)
        for marker = (svref (map-markers-items markers) i)
        when (eq (map-marker-key marker) key) return marker))

(declaim (inline write-altitude-label!))
(defun write-altitude-label! (out prefix radius)
  "OUT: PREFIX, the altitude of RADIUS above R⊕ in whole km, \" KM\"."
  (declare (type string out prefix) (type double-float radius))
  (setf (fill-pointer out) 0)
  (loop for c across prefix do (vector-push c out))
  (let ((n (round (max -1d15 (min 1d15 (m->km (- radius +radius-earth+)))))))
    (declare (type (integer -1000000000000 1000000000000) n))
    (when (minusp n)
      (vector-push #\- out)
      (setf n (- n)))
    (let ((power 1))
      (declare (type (integer 0 10000000000000) power))
      (loop while (<= (* 10 power) n) do (setf power (* 10 power)))
      (loop while (>= power 1)
            do (multiple-value-bind (digit rest) (floor n power)
                 (vector-push (code-char (+ 48 digit)) out)
                 (setf n rest power (floor power 10))))))
  (loop for c across " KM" do (vector-push c out))
  out)

(defun map-markers (render-state camera extent &optional (out *map-markers*))
  "The frame's markers into OUT (decision 84: Map ribbons and markers:
osculating orbit, adaptive segments, overlay markers and visibility) for
CAMERA and EXTENT (width height): the ship's ring, always; apoapsis (not
on a hyperbola) and periapsis diamonds; the impact cross at the osculating
orbit's next inbound crossing of the crash radius; dots for Earth, the Moon
and the Sun below *marker-body-px*. Markers behind the eye go; markers other
than the ship's whose sight line meets Earth or the Moon go. Conses nothing;
OUT."
  (declare (optimize speed) (type render-state render-state) (type camera camera)
           (type list extent) (type map-markers out))
  (let* ((width (float (the fixnum (first extent)) 1d0))
         (height (float (the fixnum (second extent)) 1d0))
         (f (/ (tan (* 0.5d0 (camera-field-of-view camera)))))
         (fx (/ (* f height) width))
         (fy (* +clip-y-sign+ f))
         (eye (map-markers-eye out)) (forward (map-markers-forward out))
         (right (map-markers-right out)) (down (map-markers-down out))
         (h (map-markers-h out)) (pv (map-markers-p-axis out)) (qv (map-markers-q-axis out))
         (scratch (map-markers-scratch out))
         (items (map-markers-items out))
         (moon (render-state-moon render-state))
         (r (render-state-r render-state)) (v (render-state-v render-state))
         (count 0))
    (declare (type double-float width height f fx fy) (type fixnum count) (type vec3 moon r v))
    (camera-eye camera (camera-anchor-position! eye camera render-state) eye)
    (let ((q (camera-orientation camera)))
      (quat-rotate! forward q (vec3-set! scratch 1d0 0d0 0d0))
      (quat-rotate! right q (vec3-set! scratch 0d0 1d0 0d0))
      (quat-rotate! down q (vec3-set! scratch 0d0 0d0 1d0)))
    (labels ((occluded-p (p cx cy cz radius)
               ;; the segment from the eye to P meets the sphere at (cx cy cz)
               (declare (type vec3 p) (type double-float cx cy cz radius))
               (let* ((dx (- (aref p 0) (aref eye 0))) (dy (- (aref p 1) (aref eye 1)))
                      (dz (- (aref p 2) (aref eye 2)))
                      (ax (- cx (aref eye 0))) (ay (- cy (aref eye 1))) (az (- cz (aref eye 2)))
                      (dd (+ (* dx dx) (* dy dy) (* dz dz)))
                      (s (if (> dd 0d0)
                             (max 0d0 (min 1d0 (/ (+ (* ax dx) (* ay dy) (* az dz)) dd)))
                             0d0))
                      (ox (- (* s dx) ax)) (oy (- (* s dy) ay)) (oz (- (* s dz) az)))
                 (< (+ (* ox ox) (* oy oy) (* oz oz)) (* radius radius))))
             (by-earth-p (p) (occluded-p p 0d0 0d0 0d0 +radius-earth+))
             (by-moon-p (p) (occluded-p p (aref moon 0) (aref moon 1) (aref moon 2) +radius-moon+))
             (add (key kind colour label p occluders &optional (time 0d0))
               ;; P relative to Earth's centre; OCCLUDERS :both, :earth, :moon or nil
               (declare (type vec3 p) (type double-float time))
               (let* ((dx (- (aref p 0) (aref eye 0))) (dy (- (aref p 1) (aref eye 1)))
                      (dz (- (aref p 2) (aref eye 2)))
                      (w (+ (* dx (aref forward 0)) (* dy (aref forward 1)) (* dz (aref forward 2)))))
                 (when (and (> w 0d0) (< count +map-marker-capacity+)
                            (not (and (member occluders '(:both :earth)) (by-earth-p p)))
                            (not (and (member occluders '(:both :moon)) (by-moon-p p))))
                   (let ((marker (svref items count))
                         (x (+ (* dx (aref right 0)) (* dy (aref right 1)) (* dz (aref right 2))))
                         (y (+ (* dx (aref down 0)) (* dy (aref down 1)) (* dz (aref down 2)))))
                     (declare (type map-marker marker) (type double-float x y))
                     (setf (map-marker-key marker) key (map-marker-kind marker) kind
                           (map-marker-colour marker) colour (map-marker-label marker) label
                           (map-marker-x marker) (* 0.5d0 width (+ 1d0 (/ (* fx x) w)))
                           (map-marker-y marker) (* 0.5d0 height (+ 1d0 (/ (* fy y) w)))
                           (map-marker-time marker) time)
                     (replace (map-marker-point marker) p)
                     (incf count)))))
             (put-on-conic (radius theta)
               ;; SCRATCH: the point at true anomaly THETA and RADIUS
               (declare (type double-float radius theta))
               (vec3-scale! scratch pv (* radius (cos theta)))
               (vec3-add-scaled! scratch scratch qv (* radius (sin theta))))
             (body-marker (key label p radius occluders)
               (declare (type vec3 p) (type double-float radius))
               (let ((d (sqrt (the (double-float 0d0) (+ (expt (- (aref p 0) (aref eye 0)) 2)
                                 (expt (- (aref p 1) (aref eye 1)) 2)
                                 (expt (- (aref p 2) (aref eye 2)) 2))))))
                 (when (and (> d radius)
                            (< (* height (tan (asin (the (double-float 0d0 1d0) (/ radius d)))) f) *marker-body-px*))
                   (add key :dot *marker-body-colour* label p occluders)))))
      (declare (inline occluded-p by-earth-p by-moon-p add put-on-conic body-marker))
      (add :ship :ring *marker-colour* *marker-ship-label* r nil)
      ;; the osculating conic about Earth: P to periapsis, Q 90° ahead
      (let* ((mu +mu-earth+)
             (rn (vec3-norm r))
             (hn (vec3-norm (vec3-cross! h r v))))
        (when (> hn (* 1d-9 rn (vec3-norm v)))
          ;; e = (v × h) / μ − r / |r|
          (vec3-cross! pv v h)
          (vec3-scale! pv pv (/ mu))
          (vec3-add-scaled! pv pv r (/ -1d0 rn))
          (let* ((e (vec3-norm pv))
                 (p (/ (* hn hn) mu))
                 (q (/ p (+ 1d0 e)))
                 (crash (body-crash-radius (body :earth)))
                 (circular (the double-float *circular-e*)))
            (declare (type double-float e p q crash circular))
            (if (< e circular)
                (vec3-scale! pv r (/ rn))
                (vec3-scale! pv pv (/ e)))
            (vec3-scale! h h (/ hn))
            (vec3-cross! qv h pv)
            (put-on-conic q 0d0)
            (add :periapsis :diamond *marker-colour*
                 (write-altitude-label! (map-markers-periapsis-label out) "PE " q) scratch :both)
            (when (< e 1d0)
              (let ((apoapsis (/ p (- 1d0 e))))
                (put-on-conic apoapsis pi)
                (add :apoapsis :diamond *marker-colour*
                     (write-altitude-label! (map-markers-apoapsis-label out) "AP " apoapsis)
                     scratch :both)))
            ;; the next inbound crossing of the crash radius after t_r
            (when (if (< e 1d0)
                      (and (>= e circular) (<= q crash (/ p (- 1d0 e))))
                      (>= crash q))
              (let* ((theta (- (acos (the (double-float -1d0 1d0)
                                             (max -1d0 (min 1d0 (/ (- (/ p crash) 1d0) e)))))))
                     (ship-theta (atan (vec3-dot r qv) (vec3-dot r pv)))
                     (a (abs (/ p (- 1d0 (* e e)))))
                     (n (sqrt (the (double-float 0d0) (/ mu (* a a a))))))
                (declare (type double-float theta ship-theta a n))
                (flet ((mean (x)
                         (declare (type double-float x))
                         (if (< e 1d0)
                             (let ((ea (* 2 (atan (* (sqrt (the (double-float 0d0) (- 1d0 e))) (sin (* 0.5d0 x)))
                                                  (* (sqrt (the (double-float 0d0) (+ 1d0 e)))
                                                     (cos (* 0.5d0 x)))))))
                               (- ea (* e (sin ea))))
                             (let ((ha (asinh (/ (* (sin x) (sqrt (the (double-float 0d0) (- (* e e) 1d0))))
                                                 (+ 1d0 (* e (cos x)))))))
                               (- (* e (sinh ha)) ha)))))
                  (declare (inline mean))
                  (let ((dm (- (mean theta) (mean ship-theta))))
                    (declare (type double-float dm))
                    (when (< e 1d0)
                      (setf dm (mod dm (* 2 pi)))
                      (when (= dm 0d0) (setf dm (* 2 pi))))
                    (when (> dm 0d0)
                      (put-on-conic crash theta)
                      (add :impact :cross *marker-impact-colour* "IMPACT" scratch :both
                           (+ (render-state-t render-state) (/ dm n))))))))))
        (vec3-set! scratch 0d0 0d0 0d0)
        (body-marker :earth "EARTH" scratch +radius-earth+ :moon)
        (body-marker :moon "MOON" moon +radius-moon+ :earth)
        (body-marker :sun "SUN" (render-state-sun render-state) +radius-sun+ :both)))
    (setf (map-markers-count out) count)
    out))
