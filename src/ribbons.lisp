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

(defparameter *ribbon-width-px* 2.0d0 "The line's width in pixels; decision 84.")
(defparameter *ribbon-radiance* 1.0d0 "Times the colour, over the exposure; decision 84.")

(defconstant +ribbon-instance-floats+ 16
  "Per segment: the start (x y z 1), the end (x y z 1), the linear colour (r g b 1),
the style (dash phase in px at the start or −1 for solid, the phase at the end,
the width in px, the radiance factor); decision 125: Map in milestone 3.")

(defparameter *dash-period-px* 14d0 "A dashed line repeats every this many px; decision 125.")
(defparameter *target-orbit-colour* #xE040E0 "The target's orbit, magenta, sRGB; decision 125.")
(defparameter *map-lines-capacity* 32768 "Segments in the map's one line buffer; decision 125.")
(declaim (type double-float *dash-period-px*) (type (unsigned-byte 24) *target-orbit-colour*)
         (type fixnum *map-lines-capacity*))

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
  (orbit (make-orbit 0d0 0d0 0d0 0d0 0d0 0d0 0d0) :type orbit)
  ;; The predicted trajectory's work (trajectory-ribbon-points!): each
  ;; point's TIMES, the elements (a run of nodes, or one interval in 2^k
  ;; pieces): first node, node intervals, k, eye distance and base error in
  ;; px; the BREAKS (times); BOUND, the worst piece error reached in px.
  (times nil :type doubles)
  (e-start nil :type (simple-array fixnum (*)))
  (e-length nil :type (simple-array fixnum (*)))
  (e-k nil :type (simple-array fixnum (*)))
  (e-distance nil :type doubles)
  (e-error nil :type doubles)
  (breaks nil :type doubles)
  (bound 0d0 :type double-float))

(defun make-ribbon (&key (capacity *ribbon-max-segments*) (base *ribbon-base-segments*)
                      (elements *prediction-capacity*))
  (flet ((doubles (n) (make-array n :element-type 'double-float :initial-element 0d0))
         (fixnums (n) (make-array n :element-type 'fixnum :initial-element 0)))
    (%make-ribbon :capacity capacity
                  :times (doubles (1+ capacity))
                  :e-start (fixnums (+ 2 elements)) :e-length (fixnums (+ 2 elements))
                  :e-k (fixnums (+ 2 elements)) :e-distance (doubles (+ 2 elements))
                  :e-error (doubles (+ 2 elements))
                  :breaks (doubles (+ 8 16 *prediction-event-capacity*))
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
                                &key (crash-radius (body-crash-radius (body :earth)))
                                  (colour *ribbon-colour*) (crash-colour *ribbon-crash-colour*)
                                  (width *ribbon-width-px*) (radiance 1d0)
                                  (out (ribbon-instances ribbon)) (start 0))
  "Write one instance per segment of RIBBON into OUT from segment START: both
endpoints (point − EYE-REL-BODY, i.e. point + body − eye) in double, then
single, the linear COLOUR (sRGB integer), CRASH-COLOUR where the segment's
mid-anomaly radius is below CRASH-RADIUS, solid (phase −1), WIDTH px and
RADIANCE as the style (dash-phases! sets dashes). Returns the segment count.
Conses nothing."
  (declare (optimize speed) (type ribbon ribbon) (type vec3 eye-rel-body)
           (type double-float crash-radius width radiance)
           (type (unsigned-byte 24) colour crash-colour)
           (type (simple-array single-float (*)) out) (type fixnum start))
  (let ((points (ribbon-points ribbon)) (mid-radii (ribbon-mid-radii ribbon))
        (p (ribbon-scratch ribbon))
        (count (ribbon-count ribbon))
        (width (coerce width 'single-float)) (radiance (coerce radiance 'single-float)))
    (macrolet ((colour (rgb)
                 `(values (coerce (srgb-channel-linear ,rgb 16) 'single-float)
                          (coerce (srgb-channel-linear ,rgb 8) 'single-float)
                          (coerce (srgb-channel-linear ,rgb 0) 'single-float))))
      (multiple-value-bind (cr cg cb) (colour colour)
        (multiple-value-bind (dr dg db) (colour crash-colour)
          (dotimes (s count)
            (let ((o (* (+ start s) +ribbon-instance-floats+)))
              (declare (type fixnum o))
              (dotimes (end 2)
                (let ((k (* 3 (+ s end))))
                  (vec3-set! p (aref points k) (aref points (+ k 1)) (aref points (+ k 2))))
                (eye-relative-single p eye-rel-body out (+ o (* 4 end)))
                (setf (aref out (+ o (* 4 end) 3)) 1f0))
              (if (< (aref mid-radii s) crash-radius)
                  (setf (aref out (+ o 8)) dr (aref out (+ o 9)) dg (aref out (+ o 10)) db)
                  (setf (aref out (+ o 8)) cr (aref out (+ o 9)) cg (aref out (+ o 10)) cb))
              (setf (aref out (+ o 11)) 1f0
                    (aref out (+ o 12)) -1f0 (aref out (+ o 13)) -1f0
                    (aref out (+ o 14)) width (aref out (+ o 15)) radiance))))))
    count))

(defun dash-phases! (out start count points eye camera extent phase &optional (first 0))
  "Dash the COUNT instances of OUT from segment START, which join the
COUNT + 1 POINTS (x y z, double, relative to the same origin as EYE, the eye):
each segment's phases are its start's phase (PHASE for the first, the running
sum of the projected pixel lengths after, reduced modulo *dash-period-px*) and
that plus its own length, the projection that of CAMERA for a frame of EXTENT
(width height) in double; a segment with an endpoint behind the eye has
length 0, so dashes run on across segments at every zoom (decision 125: Map
in milestone 3). Returns the phase at the end, to continue a ribbon. Conses
nothing. FIRST: the index of the point the COUNT segments start at."
  (declare (optimize speed) (type (simple-array single-float (*)) out) (type fixnum start count first)
           (type doubles points) (type vec3 eye) (type camera camera) (type list extent)
           (type double-float phase))
  (let* ((width (float (the fixnum (first extent)) 1d0))
         (height (float (the fixnum (second extent)) 1d0))
         (f (/ (tan (* 0.5d0 (camera-field-of-view camera)))))
         (fx (/ (* f height) width))
         (fy (* +clip-y-sign+ f))
         (forward (make-vec3)) (right (make-vec3)) (down (make-vec3)) (scratch (make-vec3))
         (q (camera-orientation camera))
         (period *dash-period-px*)
         (px 0d0) (py 0d0) (visible nil))
    (declare (dynamic-extent forward right down scratch) (type double-float width height f fx fy period px py))
    (quat-rotate! forward q (vec3-set! scratch 1d0 0d0 0d0))
    (quat-rotate! right q (vec3-set! scratch 0d0 1d0 0d0))
    (quat-rotate! down q (vec3-set! scratch 0d0 0d0 1d0))
    (flet ((project (i)
             ;; the pixel of point I into PX, PY; VISIBLE when in front of the eye
             (declare (type fixnum i))
             (let* ((dx (- (aref points (* 3 (+ first i))) (aref eye 0)))
                    (dy (- (aref points (+ (* 3 (+ first i)) 1)) (aref eye 1)))
                    (dz (- (aref points (+ (* 3 (+ first i)) 2)) (aref eye 2)))
                    (w (+ (* dx (aref forward 0)) (* dy (aref forward 1)) (* dz (aref forward 2)))))
               (declare (type double-float dx dy dz w))
               (setf visible (> w 0d0))
               (when visible
                 (setf px (* 0.5d0 width (+ 1d0 (/ (* fx (+ (* dx (aref right 0)) (* dy (aref right 1))
                                                           (* dz (aref right 2))))
                                                  w)))
                       py (* 0.5d0 height (+ 1d0 (/ (* fy (+ (* dx (aref down 0)) (* dy (aref down 1))
                                                               (* dz (aref down 2))))
                                                      w))))))))
      (declare (inline project))
      (project 0)
      (let ((prev-x px) (prev-y py) (prev-visible visible)
            (phase (mod phase period)))
        (declare (type double-float prev-x prev-y phase))
        (dotimes (s count)
          (project (1+ s))
          (let ((length (if (and prev-visible visible)
                            (sqrt (the (double-float 0d0) (+ (expt (- px prev-x) 2) (expt (- py prev-y) 2))))
                            0d0))
                (o (* (+ start s) +ribbon-instance-floats+)))
            (declare (type double-float length) (type fixnum o))
            (setf (aref out (+ o 12)) (coerce phase 'single-float)
                  (aref out (+ o 13)) (coerce (+ phase length) 'single-float))
            (setf phase (mod (+ phase length) period)
                  prev-x px prev-y py prev-visible visible)))
        phase))))

;;; The ship's line from the clock's prediction (decision 125: Map in
;;; milestone 3): the render state's r at t_r, then the stored path to each
;;; node after it, merged in aligned runs where the nodes are dense and split
;;; into Hermite pieces where they are sparse, all within a pixel budget.

(defparameter *ribbon-run-max* 64 "Nodes in the longest merged run (a power of two); decision 125.")
(defparameter *ribbon-merge-tries* 40 "Times the merge tolerance doubles at most for the budget.")
(declaim (type fixnum *ribbon-run-max* *ribbon-merge-tries*))

(declaim (inline trajectory-first-node trajectory-drawn-p))
(defun trajectory-first-node (pred time)
  "The index of the first node of PRED stored after TIME when at least two
follow it, else nil: the trajectory is drawn from them, otherwise the
osculating orbit is. Conses nothing."
  (declare (optimize speed) (type prediction pred) (type double-float time))
  (let* ((count (prediction-count pred)) (head (prediction-head pred))
         (j (node-interval pred time))
         (first (cond (j (1+ j))
                      ((and (< head count) (< time (node-t (prediction-nodes pred) head))) head))))
    (declare (type fixnum count head))
    (and first (>= (- count (the fixnum first)) 2) first)))

(defun trajectory-drawn-p (pred time)
  "True when the map draws PRED's trajectory at the render time TIME: two
nodes after it and a copy about Earth; else it draws the osculating orbit."
  (declare (type prediction pred) (type double-float time))
  (let ((sim (prediction-sim pred)))
    (and sim (eq (sim-body sim) :earth) (trajectory-first-node pred time) t)))

(defun trajectory-impact-p (pred)
  "True when PRED's copy was lost by a crash: its last node is the impact."
  (let ((loss (and (prediction-sim pred) (sim-loss (prediction-sim pred)))))
    (and loss (eq (loss-cause loss) :crash))))

(defun trajectory-ribbon-points! (ribbon pred state eye focal-px &optional start-time start-r)
  "Fill RIBBON with the ship's trajectory from PRED for the eye EYE (relative
to Earth's centre): the points (and their times) from the render STATE's r at
t_r, then along the Hermite of the stored interval holding t_r (a chord when
none does) to each stored node after it, to the horizon or the impact. Node
intervals merge in aligned runs of at most *ribbon-run-max* nodes while the
run's sagitta chord² / 8ρ (ρ = |v|³ / |v × a|, point-mass a at its first
node) and its middle node's distance from the chord, seen from the eye, stay
within the merge tolerance (*ribbon-max-error-px*) and no break (a burn's
start or cut, the last interval before an impact) falls inside; a single
interval splits into 2^k Hermite pieces (k ≤ *ribbon-max-split*) by the
bound of its curvature; k is lowered farthest first, then the tolerance
doubled, until the pieces fit the budget. The mid-radii mark the piece that
ends at an impact red. Returns the segment count, RIBBON-BOUND holding the
worst piece error in px reached; nil, RIBBON untouched, when PRED has fewer
than two nodes after t_r or is not about Earth (draw the osculating orbit).
START-TIME and START-R (relative to Earth's centre) replace t_r and the
render state's r: a plan's shot is drawn from a burn's start. Conses nothing."
  (declare (optimize speed) (type ribbon ribbon) (type prediction pred)
           (type render-state state) (type vec3 eye) (type double-float focal-px)
           (type (or null double-float) start-time) (type (or null vec3) start-r))
  (let* ((sim (prediction-sim pred))
         (tr (the double-float (or start-time (render-state-t state))))
         (first (trajectory-first-node pred tr)))
    (declare (type double-float tr))
    (unless (and sim first (eq (sim-body sim) :earth))
      (return-from trajectory-ribbon-points! nil))
    (let* ((first first)
           (nodes (prediction-nodes pred))
           (count (prediction-count pred))
           (r (the vec3 (or start-r (render-state-r state))))
           (j (node-interval pred tr))
           (e-start (ribbon-e-start ribbon)) (e-length (ribbon-e-length ribbon))
           (e-k (ribbon-e-k ribbon)) (e-distance (ribbon-e-distance ribbon))
           (e-error (ribbon-e-error ribbon))
           (points (ribbon-points ribbon)) (times (ribbon-times ribbon))
           (mid-radii (ribbon-mid-radii ribbon))
           (breaks (ribbon-breaks ribbon))
           (budget (min *ribbon-max-segments* (ribbon-capacity ribbon)))
           (max-split (max 0 (min 20 *ribbon-max-split*)))
           (run-max (max 1 *ribbon-run-max*))
           (impact (trajectory-impact-p pred))
           (last-interval (min (- count 2) (+ first (- (length e-start) 4))))
           (ex (aref eye 0)) (ey (aref eye 1)) (ez (aref eye 2))
           (rx (aref r 0)) (ry (aref r 1)) (rz (aref r 2))
           (s0 0d0) (hsag 0d0) (nb 0) (n 0) (total 0)
           (tol *ribbon-max-error-px*)
           (c (make-array 12 :element-type 'double-float :initial-element 0d0)))
      (declare (type fixnum first count last-interval budget max-split run-max nb n total)
               (type (simple-array double-float (*)) nodes) (type double-float ex ey ez rx ry rz s0 hsag tol)
               (type hermite c) (dynamic-extent c))
      (when (and impact (< last-interval (- count 2))) (setf impact nil))
      ;; the partial interval's start s0 and its curvature bound
      (when j
        (let ((h (interval-hermite! c pred j)))
          (declare (ignore h))
          (setf s0 (/ (- tr (node-t nodes j)) (- (node-t nodes (1+ j)) (node-t nodes j)))
                hsag (let ((a 0d0) (b 0d0))
                       (declare (type (double-float 0d0) a b))
                       (dotimes (i 3)
                         (let ((c2 (aref c (+ 6 i))) (c3 (aref c (+ 9 i))))
                           (incf a (* c2 c2)) (incf b (expt (+ c2 (* 3 c3)) 2))))
                       (* 0.25d0 (sqrt (max a b)))))))
      ;; the breaks, sorted
      (flet ((add-break (time)
               (declare (type double-float time))
               (when (and (> time tr) (< nb (length breaks)))
                 (let ((m nb))
                   (declare (type fixnum m))
                   (loop while (and (> m 0) (> (aref breaks (1- m)) time))
                         do (setf (aref breaks m) (aref breaks (1- m))) (decf m))
                   (setf (aref breaks m) time)
                   (incf nb)))))
        (declare (inline add-break))
        (let ((times (prediction-event-times pred)) (kinds (prediction-event-kinds pred)))
          (dotimes (i (prediction-event-count pred))
            (when (eq (svref kinds i) :burn-start) (add-break (aref times i)))))
        (dotimes (i (prediction-cut-count pred))
          (add-break (aref (prediction-cut-times pred) i)))
        (when (and impact (>= (- count 2) first))
          (add-break (node-t nodes (- count 2)))))
      (labels ((px-error (sag distance)
                 (declare (type double-float sag distance))
                 (/ (* sag focal-px) (max 1d-3 (- distance sag))))
               (chord (ax ay az bx by bz)
                 ;; the eye's distance to the chord A–B, and |B − A|²
                 (declare (type double-float ax ay az bx by bz))
                 (let* ((qx (- ax ex)) (qy (- ay ey)) (qz (- az ez))
                        (dx (- bx ax)) (dy (- by ay)) (dz (- bz az))
                        (dd (+ (* dx dx) (* dy dy) (* dz dz)))
                        (s (if (> dd 0d0)
                               (max 0d0 (min 1d0 (/ (- (+ (* qx dx) (* qy dy) (* qz dz))) dd)))
                               0d0))
                        (cx (+ qx (* s dx))) (cy (+ qy (* s dy))) (cz (+ qz (* s dz))))
                   (values (sqrt (+ (* cx cx) (* cy cy) (* cz cz))) dd)))
               (node (i k)
                 (declare (type (integer 0 1000000) i) (type (integer 0 2) k))
                 (aref nodes (+ (* +node-size+ i) 1 k)))
               (velocity (i k)
                 (declare (type (integer 0 1000000) i) (type (integer 0 2) k))
                 (aref nodes (+ (* +node-size+ i) 4 k)))
               (split-for (error-px)
                 (declare (type double-float error-px))
                 (let ((k 0))
                   (declare (type fixnum k))
                   (loop while (and (> error-px tol) (< k max-split))
                         do (setf error-px (* 0.25d0 error-px)) (incf k))
                   k))
               (run-error (i len)
                 ;; the run of LEN intervals from node I: its px error and eye distance
                 (declare (type fixnum i len))
                 (let ((ax (node i 0)) (ay (node i 1)) (az (node i 2))
                       (bx (node (+ i len) 0)) (by (node (+ i len) 1)) (bz (node (+ i len) 2))
                       (vx (velocity i 0)) (vy (velocity i 1)) (vz (velocity i 2)))
                   (declare (type double-float ax ay az bx by bz vx vy vz))
                   (multiple-value-bind (distance chord2) (chord ax ay az bx by bz)
                     (declare (type double-float distance chord2))
                     (let* ((rn2 (+ (* ax ax) (* ay ay) (* az az)))
                            (rn3 (* rn2 (sqrt rn2)))
                            (cx (- (* vy az) (* vz ay))) (cy (- (* vz ax) (* vx az)))
                            (cz (- (* vx ay) (* vy ax)))
                            (vcross (sqrt (+ (* cx cx) (* cy cy) (* cz cz))))
                            (v2 (+ (* vx vx) (* vy vy) (* vz vz)))
                            (v3 (* v2 (sqrt v2)))
                            (sag (if (and (> v3 0d0) (> rn3 0d0))
                                     (/ (* chord2 (/ (* +mu-earth+ vcross) rn3)) (* 8d0 v3))
                                     0d0))
                            (error-px (px-error sag distance)))
                       (declare (type double-float rn2 rn3 cx cy cz vcross v2 v3 sag error-px))
                       (when (> len 1)
                         ;; the middle node's distance from the chord
                         (let* ((m (+ i (ash len -1)))
                                (dx (- bx ax)) (dy (- by ay)) (dz (- bz az))
                                (mx (- (node m 0) ax)) (my (- (node m 1) ay)) (mz (- (node m 2) az))
                                (ux (- (* my dz) (* mz dy))) (uy (- (* mz dx) (* mx dz)))
                                (uz (- (* mx dy) (* my dx)))
                                (dd (+ (* dx dx) (* dy dy) (* dz dz)))
                                (off (if (> dd 0d0)
                                         (/ (sqrt (+ (* ux ux) (* uy uy) (* uz uz))) (sqrt dd))
                                         0d0)))
                           (declare (type double-float dx dy dz mx my mz ux uy uz dd off))
                           (setf error-px (max error-px (px-error off distance)))))
                       (values error-px distance)))))
               (build (tol-now)
                 ;; the elements at the merge tolerance TOL-NOW; the pieces in all
                 (declare (type double-float tol-now))
                 (setf tol tol-now)
                 (let ((i first) (bp 0) (pieces 0))
                   (declare (type fixnum i bp pieces))
                   ;; element 0: from r to the first node after t_r
                   (multiple-value-bind (distance chord2)
                       (chord rx ry rz (node first 0) (node first 1) (node first 2))
                     (declare (type double-float distance chord2) (ignorable chord2))
                     (let* ((span (- 1d0 s0))
                            (error-px (if j (px-error (* hsag span span) distance) 0d0))
                            (k (split-for error-px)))
                       (setf (aref e-start 0) (if j j -1) (aref e-length 0) 1 (aref e-k 0) k
                             (aref e-distance 0) distance (aref e-error 0) error-px)
                       (incf pieces (ash 1 (the (integer 0 30) k)))))
                   (setf n 1)
                   (loop while (and (<= i last-interval) (< n (- (length e-start) 1)))
                         do (let ((ti (node-t nodes i)) (len 1))
                              (declare (type double-float ti) (type fixnum len))
                              (loop while (and (< bp nb) (<= (aref breaks bp) ti)) do (incf bp))
                              (let ((try run-max))
                                (declare (type fixnum try))
                                (loop while (> try 1)
                                      do (when (and (zerop (mod i try)) (<= (+ i try -1) last-interval)
                                                    (or (>= bp nb)
                                                        (>= (aref breaks bp) (node-t nodes (+ i try)))))
                                           (multiple-value-bind (error-px distance) (run-error i try)
                                             (declare (type double-float error-px distance))
                                             (when (<= error-px tol)
                                               (setf len try
                                                     (aref e-distance n) distance
                                                     (aref e-error n) error-px)
                                               (return))))
                                         (setf try (ash try -1))))
                              (setf (aref e-start n) i (aref e-length n) len)
                              (cond ((> len 1)
                                     (setf (aref e-k n) 0)
                                     (incf pieces))
                                    (t
                                     (let ((h (interval-hermite! c pred i)))
                                       (declare (ignore h))
                                       (multiple-value-bind (distance chord2)
                                           (chord (node i 0) (node i 1) (node i 2)
                                                  (node (1+ i) 0) (node (1+ i) 1) (node (1+ i) 2))
                                         (declare (type double-float distance) (ignore chord2))
                                         (let* ((a 0d0) (b 0d0))
                                           (declare (type (double-float 0d0) a b))
                                           (dotimes (q 3)
                                             (let ((c2 (aref c (+ 6 q))) (c3 (aref c (+ 9 q))))
                                               (incf a (* c2 c2)) (incf b (expt (+ c2 (* 3 c3)) 2))))
                                           (let* ((sag (* 0.25d0 (sqrt (max a b))))
                                                  (error-px (px-error sag distance))
                                                  (k (split-for error-px)))
                                             (setf (aref e-k n) k (aref e-distance n) distance
                                                   (aref e-error n) error-px)
                                             (incf pieces (ash 1 k))))))))
                              (incf n)
                              (incf i len)))
                   pieces))
               (lower (excess)
                 ;; lower k by one on the farthest elements until EXCESS pieces are saved;
                 ;; the pieces saved, 0 when no element is split
                 (declare (type fixnum excess))
                 (flet ((saving (cutoff)
                          (declare (type double-float cutoff) (values fixnum &optional))
                          (let ((sum 0))
                            (declare (type fixnum sum))
                            (dotimes (e n sum)
                              (when (and (> (aref e-k e) 0) (>= (aref e-distance e) cutoff))
                                (incf sum (ash 1 (the (integer 0 29) (1- (aref e-k e))))))))))
                   (let ((lo 0d0) (hi 0d0))
                     (declare (type double-float lo hi))
                     (dotimes (e n) (setf hi (max hi (aref e-distance e))))
                     (cond ((zerop (saving 0d0)) 0)
                           ((< (saving 0d0) excess)
                            (dotimes (e n) (when (> (aref e-k e) 0) (decf (aref e-k e))))
                            (saving 0d0))
                           (t
                            ;; the largest cutoff whose saving is still enough
                            (setf hi (* 1.000001d0 hi))
                            (dotimes (iteration 40)
                              (let ((mid (* 0.5d0 (+ lo hi))))
                                (if (>= (saving mid) excess) (setf lo mid) (setf hi mid))))
                            (let ((saved (saving lo)))
                              (dotimes (e n)
                                (when (and (> (aref e-k e) 0) (>= (aref e-distance e) lo))
                                  (decf (aref e-k e))))
                              saved)))))))
        (declare (inline px-error chord node velocity split-for))
        ;; elements within the budget
        (setf total (build *ribbon-max-error-px*))
        (let ((tries 0))
          (declare (type fixnum tries))
          (loop while (> total budget)
                do (let ((saved (lower (- total budget))))
                     (declare (type fixnum saved))
                     (decf total saved)
                     (when (and (> total budget) (zerop saved))
                       (when (>= (incf tries) *ribbon-merge-tries*) (return))
                       (setf total (build (* 2d0 tol)))))))
        ;; the points
        (let ((p 0) (bound 0d0))
          (declare (type fixnum p) (type double-float bound))
          (flet ((put (x y z time)
                   (declare (type double-float x y z time))
                   (when (< p (ribbon-capacity ribbon))
                     (setf (aref points (* 3 p)) x (aref points (+ (* 3 p) 1)) y
                           (aref points (+ (* 3 p) 2)) z (aref times p) time))
                   (incf p)))
            (declare (inline put))
            (put rx ry rz tr)
            (dotimes (e n)
              (let* ((start (aref e-start e)) (len (aref e-length e)) (k (aref e-k e)))
                (declare (type fixnum start len k))
                (setf bound (max bound (* (aref e-error e) (scale-float 1d0 (* -2 (the (integer 0 30) k))))))
                (cond ((> len 1)
                       (let ((end (+ start len)))
                         (put (node end 0) (node end 1) (node end 2) (node-t nodes end))))
                      ((< start 0)
                       (put (node first 0) (node first 1) (node first 2) (node-t nodes first)))
                      (t
                       (let* ((pieces (ash 1 k))
                              (h (interval-hermite! c pred start))
                              (t0 (node-t nodes start))
                              (from (if (= e 0) s0 0d0))
                              (dp (make-vec3)) (out (make-vec3)))
                         (declare (type fixnum pieces) (type double-float h t0 from)
                                  (dynamic-extent dp out))
                         (loop for m of-type fixnum from 1 below pieces
                               do (let ((s (+ from (/ (* (- 1d0 from) m) pieces))))
                                    (declare (type double-float s))
                                    (hermite-point! out dp c s)
                                    (put (aref out 0) (aref out 1) (aref out 2) (+ t0 (* s h)))))
                         (let ((end (1+ start)))
                           (put (node end 0) (node end 1) (node end 2) (node-t nodes end)))))))))
          (let ((segments (max 0 (1- (min p (1+ (ribbon-capacity ribbon)))))))
            (declare (type fixnum segments))
            (dotimes (s segments)
              (let ((x (* 0.5d0 (+ (aref points (* 3 s)) (aref points (+ (* 3 s) 3)))))
                    (y (* 0.5d0 (+ (aref points (+ (* 3 s) 1)) (aref points (+ (* 3 s) 4)))))
                    (z (* 0.5d0 (+ (aref points (+ (* 3 s) 2)) (aref points (+ (* 3 s) 5))))))
                (setf (aref mid-radii s) (sqrt (+ (* x x) (* y y) (* z z))))))
            (when (and impact (> segments 0))
              (setf (aref mid-radii (1- segments)) 0d0))
            (setf (ribbon-count ribbon) segments
                  (ribbon-bound ribbon) bound)
            segments))))))

;;; The one line buffer of the map (decision 125): every ribbon appends its
;;; segments here and one draw covers them.

(defstruct (map-lines (:constructor %make-map-lines))
  "COUNT segments of up to CAPACITY in INSTANCES (+ribbon-instance-floats+ each);
VIEW is INSTANCES seen up to the used part, for write-buffer."
  (capacity 0 :type fixnum)
  (count 0 :type fixnum)
  (instances nil :type (simple-array single-float (*)))
  (view nil :type (and vector (not simple-array))))

(defun make-map-lines (&key (capacity *map-lines-capacity*))
  (let ((instances (make-array (* +ribbon-instance-floats+ capacity)
                               :element-type 'single-float :initial-element 0f0)))
    (%make-map-lines :capacity capacity :instances instances
                     :view (make-array (length instances) :element-type 'single-float
                                                          :displaced-to instances :fill-pointer 0))))

(defun map-lines-clear! (lines)
  (setf (map-lines-count lines) 0)
  lines)

(defun map-lines-add-orbit! (lines ribbon eye-rel-body camera extent
                             &key (colour *ribbon-colour*) (crash-colour *ribbon-crash-colour*)
                               (width *ribbon-width-px*) (radiance 1d0) dashed (phase 0d0) (from 0))
  "Append RIBBON's segments (orbit-ribbon-points! done) to LINES through
orbit-ribbon-instances! with COLOUR, CRASH-COLOUR, WIDTH and RADIANCE; DASHED:
with dash phases from PHASE, from the point FROM on (the segments before it
stay solid). Segments past the capacity are dropped. Returns the segment
count appended and the dash phase at the end. Conses nothing."
  (declare (type map-lines lines) (type ribbon ribbon))
  (let* ((start (map-lines-count lines))
         (n (min (- (map-lines-capacity lines) start) (ribbon-count ribbon)))
         (kept (ribbon-count ribbon))
         (end phase))
    (declare (type fixnum from))
    (when (plusp n)
      (setf (ribbon-count ribbon) n)
      (orbit-ribbon-instances! ribbon eye-rel-body :out (map-lines-instances lines) :start start
                               :colour colour :crash-colour crash-colour :width width
                               :radiance radiance)
      (when (and dashed (< from n))
        (setf end (dash-phases! (map-lines-instances lines) (+ start from) (- n from)
                                (ribbon-points ribbon) eye-rel-body camera extent phase from)))
      (setf (ribbon-count ribbon) kept)
      (incf (map-lines-count lines) n))
    (values (max n 0) end)))

(defun ribbon-corners ()
  "The 6-vertex quad: along 0 or 1, across ±1."
  (map '(simple-array single-float (*)) (lambda (x) (float x 1f0)) '(0 -1 1 -1 1 1 0 -1 1 1 0 1)))

(defun ribbon-vertex-buffers ()
  `((:array-stride 8 :attributes ((:shader-location 0 :offset 0 :format :float32x2)))
    (:array-stride ,(* 4 +ribbon-instance-floats+) :step-mode :instance
     :attributes ((:shader-location 1 :offset 0 :format :float32x4)
                  (:shader-location 2 :offset 16 :format :float32x4)
                  (:shader-location 3 :offset 32 :format :float32x4)
                  (:shader-location 4 :offset 48 :format :float32x4)))))

(defun ribbon-pipeline (renderer layout format)
  "The :ribbon live pipeline into FORMAT: additive, depth compared :greater,
not written (decision 4: Depth strategy)."
  (live-pipeline renderer :ribbon layout (ribbon-vertex-buffers)
                 :format format :blend :premultiplied-alpha
                 :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                  :depth-compare :greater)))

(defun make-ribbon-buffer (device lines)
  "A target's instance buffer for the map LINES' capacity."
  (luv:create device (luv:make-buffer-descriptor
                      :label "map lines" :size (* 4 (length (map-lines-instances lines)))
                      :usage '(:vertex :copy-dst))))

(defun map-lines-conic-p (r v h)
  "R × v (into H) is not zero: the state has an orbit plane."
  (> (vec3-norm (vec3-cross! h r v)) (* 1d-9 (vec3-norm r) (vec3-norm v))))

(defparameter *plan-kept-colour* #x8A8A80 "A kept plan's lines and burns, grey, sRGB; decision 125.")
(defparameter *goal-orbit-width-px* 1d0 "The goal orbit's thin line; decision 125.")
(declaim (type (unsigned-byte 24) *plan-kept-colour*) (type double-float *goal-orbit-width-px*))

(defun ribbon-time-index (ribbon time)
  "The first point of RIBBON (trajectory-ribbon-points!) at or after TIME
(within a microsecond), its count when none."
  (declare (optimize speed) (type ribbon ribbon) (type double-float time))
  (let ((times (ribbon-times ribbon)) (n (ribbon-count ribbon)))
    (declare (type fixnum n))
    (or (loop for s of-type fixnum from 0 to n
              when (>= (aref times s) (- time 1d-6)) return s)
        n)))

(defun plan-dash-start (plan time mode)
  "The start of the burn where the plan's dashes begin (decision 125): the
first one for a plan :shown, else the first not yet started at TIME; nil
when none."
  (declare (type burn-plan plan) (type double-float time))
  (let ((burns (burn-plan-burns plan)))
    (if (eq mode :shown)
        (and (plusp (length burns)) (planned-burn-start (svref burns 0)))
        (loop for burn across burns
              when (> (planned-burn-start burn) time) return (planned-burn-start burn)))))

(defun shot-first-time (shot)
  (declare (type prediction shot))
  (node-t (prediction-nodes shot) (prediction-head shot)))

(defun map-lines-fill! (lines ribbon state camera eye extent)
  "LINES: the ship's predicted trajectory (cyan; trajectory-ribbon-points!)
or, without two nodes of a prediction after t_r, its osculating orbit about
Earth, and with a target its osculating orbit (magenta, *target-orbit-colour*),
for the eye EYE (relative to Earth's centre) and a frame of EXTENT (width
height). No orbit for a state without an orbit plane (r × v = 0). The
autopilot's plan (autopilot-map-plan, decision 125): its goal orbit thin and
solid, its path dashed from the first burn not yet started, along the
trajectory when the prediction flies the plan, else along its kept shot; grey
for a kept plan. Conses nothing."
  (map-lines-clear! lines)
  (let ((h (ribbon-scratch ribbon)) (focal (camera-focal-px camera (second extent)))
        (tr (render-state-t state))
        (model (render-state-autopilot-model state))
        (p (make-vec3)))
    (declare (dynamic-extent p) (type double-float tr))
    (multiple-value-bind (plan shot mode) (if model (autopilot-map-plan model) (values nil nil nil))
      (let* ((kept (eq mode :kept))
             (colour (if kept *plan-kept-colour* *ribbon-colour*))
             (dash-t (and plan (plan-dash-start plan tr mode)))
             (end-t tr) (phase 0d0))
        (declare (type double-float end-t phase))
        (flet ((orbit-of (r v colour)
                 (when (map-lines-conic-p r v h)
                   (orbit-ribbon-points! ribbon (state->orbit-into! (ribbon-orbit ribbon) r v +mu-earth+
                                                                    (render-state-t state))
                                         eye focal)
                   (map-lines-add-orbit! lines ribbon eye camera extent :colour colour))))
          (when (render-state-target state)
            (orbit-of (render-state-target-r state) (render-state-target-v state) *target-orbit-colour*))
          (when (and plan (burn-plan-goal-orbit plan) (not (burn-plan-rendezvous plan)))
            (orbit-ribbon-points! ribbon (burn-plan-goal-orbit plan) eye focal)
            (map-lines-add-orbit! lines ribbon eye camera extent :colour colour :crash-colour colour
                                  :width *goal-orbit-width-px*))
          (let ((pred (render-state-prediction state)))
            (if (and pred (trajectory-ribbon-points! ribbon pred state eye focal))
                (let ((dash (and (eq mode :engaged) dash-t)))
                  (multiple-value-bind (n end)
                      (map-lines-add-orbit! lines ribbon eye camera extent :colour *ribbon-colour*
                                            :dashed (and dash t)
                                            :from (if dash (ribbon-time-index ribbon dash) 0))
                    (declare (ignore n))
                    (setf phase end
                          end-t (aref (ribbon-times ribbon) (ribbon-count ribbon)))))
                (orbit-of (render-state-r state) (render-state-v state) *ribbon-colour*)))
          (map-lines-add-terminal-path! lines ribbon state eye camera extent)
          ;; the shot's dashes: from the first burn (shown, kept), or on from the prediction's end
          (when (and shot dash-t)
            (let* ((from (if (eq mode :engaged) (max end-t (shot-first-time shot)) (max dash-t (shot-first-time shot))))
                   (from (if (eq mode :engaged) from (max from tr))))
              (declare (type double-float from))
              (when (and (< from (node-t (prediction-nodes shot) (1- (prediction-count shot))))
                         (prediction-position! p shot from)
                         (trajectory-ribbon-points! ribbon shot state eye focal from p))
                (map-lines-add-orbit! lines ribbon eye camera extent :colour colour
                                      :crash-colour (if kept colour *ribbon-crash-colour*)
                                      :dashed t :phase phase
                                      :from (if (> dash-t from) (ribbon-time-index ribbon dash-t) 0))))))))
    lines))

(defun encode-map-lines (pass renderer bind-group lines buffer corners state camera eye extent)
  "The map's lines into the open scene PASS in one draw: LINES filled for the eye EYE
(relative to Earth's centre) and EXTENT, written to BUFFER. The renderer's test
lines function, if any, adds its own first."
  (map-lines-fill! lines (frame-renderer-ribbon renderer) state camera eye extent)
  (let ((hook (frame-renderer-test-lines renderer)))
    (when hook (funcall hook lines state camera eye extent)))
  (let ((count (map-lines-count lines)) (view (map-lines-view lines)))
    (when (plusp count)
      (setf (fill-pointer view) (* count +ribbon-instance-floats+))
      (luv:write-buffer buffer view)
      (luv:set-pipeline pass (renderer-pipeline renderer :ribbon))
      (luv:set-bind-group pass 0 bind-group)
      (luv:set-vertex-buffer pass 0 corners)
      (luv:set-vertex-buffer pass 1 buffer)
      (luv:draw pass 6 count))))

;;; The rendezvous aim point and terminal path (decision 125; decision 119).

(defun render-state-aim-update! (state sim)
  "STATE's aim-valid, aim-r and stop-r: while the map's plan is a rendezvous
whose arrival lies after t_r, its aim point at t_r (the target's drop-point
state there plus the plan's offset, kept in the terminal frame: turned by the
frame at t_r from the frame at the arrival) and its stop point at t_r
(stop-point-into!), both Earth-centred; else aim-valid nil."
  (declare (type render-state state) (type sim sim))
  (setf (render-state-aim-valid state) nil)
  (let ((model (render-state-autopilot-model state)))
    (when model
      (let* ((plan (autopilot-map-plan model))
             (part (and plan (burn-plan-rendezvous plan)))
             (tr (render-state-t state)))
        (declare (type double-float tr))
        (when (and part (> (rendezvous-part-arrival part) tr)
                   (target-available-p sim (rendezvous-part-target part)))
          (let* ((target (rendezvous-part-target part))
                 (object (target-object sim target))
                 (arrival (rendezvous-part-arrival part))
                 (rt (make-vec3)) (vt (make-vec3)) (q (make-quat)) (w (make-vec3))
                 (offset (make-vec3)) (stop-v (make-vec3)))
            (when (and (stop-target-state-into! rt vt sim target :drop-point arrival)
                       (progn
                         (terminal-frame-into! q w object rt vt arrival)
                         (quat-rotate! offset (quat-conjugate! q q) (rendezvous-part-offset part))
                         (stop-target-state-into! rt vt sim target :drop-point tr)))
              (terminal-frame-into! q w object rt vt tr)
              (quat-rotate! offset q offset)
              (vec3-add! (render-state-aim-r state) rt offset)
              (when (stop-point-into! (render-state-stop-r state) stop-v sim target
                                      (rendezvous-part-fixture part) (rendezvous-part-range part) tr)
                (setf (render-state-aim-valid state) t)))))))
    state))

(defun map-lines-add-terminal-path! (lines ribbon state eye camera extent)
  "The terminal path of the rendezvous plan: the straight segment from the aim
point to the stop point (render-state-aim-update!), cyan and dashed, grey for
a kept plan. Conses nothing."
  (when (and (render-state-aim-valid state) (render-state-autopilot-model state))
    (let ((points (ribbon-points ribbon)) (kept (ribbon-count ribbon)))
      (replace points (render-state-aim-r state))
      (replace points (render-state-stop-r state) :start1 3)
      (setf (aref (ribbon-mid-radii ribbon) 0) 1d12
            (ribbon-count ribbon) 1)
      (let ((colour (if (eq (nth-value 2 (autopilot-map-plan (render-state-autopilot-model state))) :kept)
                        *plan-kept-colour* *ribbon-colour*)))
        (map-lines-add-orbit! lines ribbon eye camera extent :colour colour :crash-colour colour
                                                             :dashed t))
      (setf (ribbon-count ribbon) kept))))

;;; The map's markers (decision 84): the ship, the apsides, the impact and
;;; bodies below 6 px, each a pixel position, a colour and a label, from the
;;; render state's r, v in double; drawn by overlay.lisp after the tone map.

(defparameter *marker-body-px* 6d0 "Bodies whose disc is below this many px get a marker; decision 84.")
(defparameter *marker-label-gap-px* 8 "The label's left edge right of the marker's centre, px; decision 84.")
(defparameter *marker-colour* #x40D0FF "The ship and the apsides, cyan, sRGB; decision 84.")
(defparameter *marker-impact-colour* #xFF3030 "The impact, red, sRGB; decision 84.")
(defparameter *marker-body-colour* #x8A8A80 "Bodies, grey, sRGB; decision 84.")
(defparameter *marker-label-colour* #xE8E4D8 "Labels, paper white, sRGB; decision 84.")
(defparameter *marker-target-colour* #xE040E0 "The target, its closest-approach point and the relative nodes, magenta, sRGB; decision 125.")
(defparameter *marker-target-px* 12 "The target's box; decision 125.")
(defparameter *marker-aim-px* 9 "The rendezvous aim point's ring; decision 125.")
(defparameter *marker-candidate-px* 8 "The other candidates' boxes; decision 125.")
(defparameter *marker-candidate-body-px* 6d0
  "A candidate whose bound projects to this many px or more has no box; decision 125.")
(defparameter *map-pick-px* 10d0 "A click picks a marker within this many px; decision 125.")
(defparameter *marker-ship-label* "M-07" "decision 49: Story: names, call signs and ships.")
(declaim (type double-float *marker-body-px* *marker-candidate-body-px* *map-pick-px*)
         (type fixnum *marker-target-px* *marker-candidate-px* *marker-aim-px*)
         (type (unsigned-byte 24) *marker-target-colour*)
         (type fixnum *marker-label-gap-px*)
         (type (unsigned-byte 24) *marker-colour* *marker-impact-colour* *marker-body-colour*
               *marker-label-colour*))

(defstruct (map-marker (:constructor make-map-marker ()))
  "KEY (:ship :apoapsis :periapsis :impact :earth :moon :sun :target
:candidate :closest-approach :closest-approach-target :ascending
:descending), the shape KIND, pixel X, Y from the top left, the sRGB
COLOUR, LABEL or nil, the SIZE in px (nil: the kind's), the TARGET key of
a :target or :candidate marker, the POINT relative to Earth's centre and,
for the impact, a closest approach and a node, its TIME; where
overlay.lisp placed the label: LABEL-SIDE (nil when dropped), its left
edge LABEL-X."
  (key :ship :type symbol)
  (kind :ring :type symbol)
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (colour 0 :type (unsigned-byte 24))
  (label nil :type (or null string))
  (size nil :type (or null fixnum))
  (target nil :type (or null keyword))
  (point (make-vec3) :type vec3)
  (time 0d0 :type double-float)
  (label-side nil :type (member nil :right :left))
  (label-x 0d0 :type double-float))

(defconstant +map-marker-capacity+ 48)
(defconstant +map-burn-labels+ 8 "Burn markers with their own label strings.")

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
  (closest-label (label-buffer) :type string)
  (ascending-label (label-buffer) :type string)
  (descending-label (label-buffer) :type string)
  (burn-labels (coerce (loop repeat +map-burn-labels+ collect (label-buffer)) 'simple-vector)
   :type simple-vector)
  (candidate-r (make-vec3) :type vec3)
  (candidate-v (make-vec3) :type vec3)
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

(defun push-integer! (out n)
  "OUT's fill pointer end: the non-negative integer N in decimal."
  (declare (type string out) (type (integer 0 1000000000000000) n))
  (let ((power 1))
    (declare (type (integer 0 10000000000000000) power))
    (loop while (<= (* 10 power) n) do (setf power (* 10 power)))
    (loop while (>= power 1)
          do (multiple-value-bind (digit rest) (floor n power)
               (vector-push (code-char (+ 48 digit)) out)
               (setf n rest power (floor power 10)))))
  out)

(defun push-fixed! (out x decimals)
  "OUT's end: the non-negative X rounded to DECIMALS (0-2) places."
  (declare (type string out) (type double-float x) (type (integer 0 2) decimals))
  (let* ((scale (expt 10 decimals))
         (n (round (min 1d12 (max 0d0 x)) (/ 1d0 scale))))
    (declare (type (integer 0 1000000000000000) n))
    (multiple-value-bind (whole fraction) (floor n scale)
      (push-integer! out whole)
      (when (> decimals 0)
        (vector-push #\. out)
        (when (and (= decimals 2) (< fraction 10)) (vector-push #\0 out))
        (push-integer! out fraction))))
  out)

(defun write-distance-label! (out prefix distance)
  "OUT: PREFIX, DISTANCE in m: whole metres below 1 km (\"CA 340 M\"), one
decimal below 100 km (\"CA 12.3 KM\"), whole km above (decision 125: Map in
milestone 3)."
  (declare (type string out prefix) (type double-float distance))
  (setf (fill-pointer out) 0)
  (loop for c across prefix do (vector-push c out))
  (cond ((< distance 999.5d0) (push-fixed! out distance 0) (loop for c across " M" do (vector-push c out)))
        ((< distance 99950d0) (push-fixed! out (/ distance 1d3) 1)
         (loop for c across " KM" do (vector-push c out)))
        (t (push-fixed! out (/ distance 1d3) 0) (loop for c across " KM" do (vector-push c out))))
  out)

(defun write-angle-label! (out prefix angle)
  "OUT: PREFIX, ANGLE (rad) in degrees: two decimals below 1°, one below 10°,
whole above, then \"°\" (decision 125)."
  (declare (type string out prefix) (type double-float angle))
  (setf (fill-pointer out) 0)
  (loop for c across prefix do (vector-push c out))
  (let ((deg (abs (* angle (/ 180d0 pi)))))
    (declare (type double-float deg))
    (push-fixed! out deg (cond ((< deg 0.995d0) 2) ((< deg 9.95d0) 1) (t 0))))
  (vector-push (code-char 176) out)
  out)

(defun write-burn-label! (out n dv)
  "OUT: \"BURN N DV M/S\", DV (m/s) whole from 100, one decimal from 1, two
below (decision 125: Map in milestone 3)."
  (declare (type string out) (type fixnum n) (type double-float dv))
  (setf (fill-pointer out) 0)
  (loop for c across "BURN " do (vector-push c out))
  (push-integer! out (max 0 (min 999 n)))
  (vector-push #\Space out)
  (push-fixed! out dv (cond ((< dv 0.995d0) 2) ((< dv 99.95d0) 1) (t 0)))
  (loop for c across " M/S" do (vector-push c out))
  out)

(defun drop-point-into! (r v place render-state)
  "R, V: PLACE's point on rails at the render time (no double crosses a call)."
  (declare (optimize speed) (type vec3 r v) (type place place) (type render-state render-state)
           (inline rails-state-into!))
  (let ((time (render-state-t render-state)))
    (rails-state-into! r v (rails-segment-at (place-rails place) time) time)))

(defun map-pick (markers x y)
  "The clickable marker (the target, a candidate, the ship, Earth) of
MARKERS whose centre is nearest pixel X, Y within *map-pick-px*, or nil
(decision 125). Conses nothing."
  (declare (optimize speed) (type map-markers markers) (type real x y))
  (let ((x (float x 1d0)) (y (float y 1d0)) (best nil) (best-d (* *map-pick-px* *map-pick-px*)))
    (declare (type double-float x y best-d))
    (dotimes (i (map-markers-count markers) best)
      (let ((marker (svref (map-markers-items markers) i)))
        (when (member (map-marker-key marker) '(:target :candidate :ship :earth))
          (let ((d (+ (expt (- (map-marker-x marker) x) 2) (expt (- (map-marker-y marker) y) 2))))
            (declare (type double-float d))
            (when (<= d best-d)
              (setf best marker best-d d))))))))

(defun map-markers (render-state camera extent &optional (out *map-markers*))
  "The frame's markers into OUT (decision 84: Map ribbons and markers:
osculating orbit, adaptive segments, overlay markers and visibility) for
CAMERA and EXTENT (width height): the ship's ring, always; apoapsis (not
on a hyperbola) and periapsis diamonds; the impact cross at the end of the
drawn trajectory (the prediction's last node, decision 125) or, with none, at the
osculating orbit's next inbound crossing of the crash radius; dots for Earth, the Moon
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
         (pred (render-state-prediction render-state))
         (trajectory (and pred (trajectory-drawn-p pred (render-state-t render-state))))
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
             (add (key kind colour label p occluders &optional (time 0d0) size target)
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
                           (map-marker-time marker) time (map-marker-size marker) size
                           (map-marker-target marker) target)
                     (replace (map-marker-point marker) p)
                     (incf count)))))
             (put-on-conic (radius theta)
               ;; SCRATCH: the point at true anomaly THETA and RADIUS
               (declare (type double-float radius theta))
               (vec3-scale! scratch pv (* radius (cos theta)))
               (vec3-add-scaled! scratch scratch qv (* radius (sin theta))))
             (distance-to (p)
               (declare (type vec3 p))
               (sqrt (the (double-float 0d0) (+ (expt (- (aref p 0) (aref eye 0)) 2)
                                                 (expt (- (aref p 1) (aref eye 1)) 2)
                                                 (expt (- (aref p 2) (aref eye 2)) 2)))))
             (candidate (key)
               ;; a candidate's box, when its bound projects below the limit
               (declare (type keyword key))
               (unless (eq key (render-state-target render-state))
                 (let* ((object-key (target-object-key key))
                        (p (map-markers-candidate-r out))
                        (bound 0d0))
                   (declare (type vec3 p) (type double-float bound))
                   (if object-key
                       (let* ((i (position object-key *objects* :key #'object-def-key))
                              (o (and i (svref (render-state-objects render-state) i)))
                              (grapple (render-state-grapple render-state)))
                         (when (and o (object-render-drawn o) (object-render-def o)
                                    (not (and (eq (render-grapple-state grapple) :latched)
                                              (eql (render-grapple-object grapple) i))))
                           (replace p (object-render-r o))
                           (setf bound (object-def-bound (object-render-def o)))
                           (setf object-key :ok)))
                       (progn
                         (drop-point-into! p (map-markers-candidate-v out) (place key) render-state)
                         (setf object-key :ok)))
                   (when object-key
                     (let ((d (distance-to p)))
                       (when (and (> d bound)
                                  (< (* height (tan (asin (the (double-float 0d0 1d0) (/ bound d)))) f)
                                     *marker-candidate-body-px*))
                         (add :candidate :box *marker-body-colour* (target-name key) p :both
                              0d0 *marker-candidate-px* key)))))))
             (body-marker (key label p radius occluders)
               (declare (type vec3 p) (type double-float radius))
               (let ((d (sqrt (the (double-float 0d0) (+ (expt (- (aref p 0) (aref eye 0)) 2)
                                 (expt (- (aref p 1) (aref eye 1)) 2)
                                 (expt (- (aref p 2) (aref eye 2)) 2))))))
                 (when (and (> d radius)
                            (< (* height (tan (asin (the (double-float 0d0 1d0) (/ radius d)))) f) *marker-body-px*))
                   (add key :dot *marker-body-colour* label p occluders)))))
      (declare (inline occluded-p by-earth-p by-moon-p add put-on-conic body-marker distance-to candidate))
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
            (when (and (not trajectory)
                       (if (< e 1d0)
                           (and (>= e circular) (<= q crash (/ p (- 1d0 e))))
                           (>= crash q)))
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
        ;; the prediction's impact: its last node, where the line ends
        (when (and trajectory (trajectory-impact-p pred))
          (let* ((nodes (prediction-nodes pred)) (k (* +node-size+ (1- (prediction-count pred)))))
            (vec3-set! scratch (aref nodes (+ k 1)) (aref nodes (+ k 2)) (aref nodes (+ k 3)))
            (add :impact :cross *marker-impact-colour* "IMPACT" scratch :both
                 (loss-time (sim-loss (prediction-sim pred))))))
        ;; the plan's burns, at their centre times on the drawn path
        (let ((model (render-state-autopilot-model render-state)))
          (when model
            (multiple-value-bind (plan shot mode) (autopilot-map-plan model)
              (when plan
                (let ((burns (burn-plan-burns plan)) (tr (render-state-t render-state)))
                  (declare (type double-float tr))
                  (dotimes (i (min (length burns) +map-burn-labels+))
                    (let* ((burn (svref burns i))
                           (start (planned-burn-start burn))
                           (time (* 0.5d0 (+ start (max start (planned-burn-cut-t burn))))))
                      (declare (type double-float start time))
                      (when (and (if (eq mode :kept) (> start tr) (> (max start (planned-burn-cut-t burn)) tr))
                                 (if (eq mode :engaged)
                                     (or (and pred (prediction-position! scratch pred time))
                                         (and shot (prediction-position! scratch shot time)))
                                     (and shot (prediction-position! scratch shot time))))
                        (add :burn :plus (if (eq mode :kept) *plan-kept-colour* *marker-colour*)
                             (write-burn-label! (svref (map-markers-burn-labels out) i) (1+ i)
                                                (planned-burn-dv burn))
                             scratch :both time)))))))))
        (when (render-state-aim-valid render-state)
          (add :aim :ring *marker-target-colour* "AIM" (render-state-aim-r render-state) :both
               0d0 *marker-aim-px*))
        (let ((target (render-state-target render-state)))
          (when target
            (add :target :box *marker-target-colour* (target-name target)
                 (render-state-target-r render-state) nil 0d0 *marker-target-px* target)
            (when pred
              (multiple-value-bind (time distance) (prediction-closest-approach pred)
                (when (and time (prediction-position! scratch pred time))
                  (add :closest-approach :triangle *marker-colour*
                       (write-distance-label! (map-markers-closest-label out) "CA " distance)
                       scratch :both time)
                  (when (prediction-target-position! scratch pred time)
                    (add :closest-approach-target :triangle *marker-target-colour* nil scratch :both time))))
              (multiple-value-bind (time angle) (prediction-relative-node pred :ascending)
                (when (and time (prediction-position! scratch pred time))
                  (add :ascending :diamond *marker-target-colour*
                       (write-angle-label! (map-markers-ascending-label out) "AN " angle)
                       scratch :both time)))
              (multiple-value-bind (time angle) (prediction-relative-node pred :descending)
                (when (and time (prediction-position! scratch pred time))
                  (add :descending :diamond *marker-target-colour*
                       (write-angle-label! (map-markers-descending-label out) "DN " angle)
                       scratch :both time))))))
        (dolist (place *places*)
          (candidate (place-key place)))
        (dolist (def *objects*)
          (when (and (eq (object-def-kind def) :towable)
                     (not (find (object-def-key def) *places* :key #'place-object)))
            (candidate (object-def-key def))))
        (vec3-set! scratch 0d0 0d0 0d0)
        (body-marker :earth "EARTH" scratch +radius-earth+ :moon)
        (body-marker :moon "MOON" moon +radius-moon+ :earth)
        (body-marker :sun "SUN" (render-state-sun render-state) +radius-sun+ :both)))
    (setf (map-markers-count out) count)
    out))
