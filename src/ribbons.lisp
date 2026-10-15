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
and an index order for the budget."
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
  (scratch (make-vec3) :type vec3))

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
