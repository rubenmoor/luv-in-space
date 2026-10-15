(in-package #:luv-in-space)

;;; The camera and the one projection function (docs/architecture.md
;;; "Rendering precision"; decision 4: Depth strategy, decision 5: Precision
;;; of positions, decision 30: Views and cameras).

(defconstant +clip-y-sign+ 1d0
  "s: +1 where clip y points down (luv's Vulkan path), -1 for y up.")

(defparameter *outside-distance* 40d0 "Outside view: metres from the ship.")
(defparameter *outside-elevation* (* 20 (/ pi 180))
  "Outside view: radians above the ship's track, towards radial out.")
(defparameter *outside-field-of-view* (* 50 (/ pi 180)) "Outside view: vertical, radians.")

(defstruct camera
  "A camera: the eye at ANCHOR's position + OFFSET (double, ICRF axes),
ORIENTATION body (FRD: x view, y right, z down) -> ICRF, the vertical
FIELD-OF-VIEW in radians, NEAR in metres."
  (anchor :ship)
  (offset (make-vec3) :type vec3)
  (orientation (quat 1 0 0 0) :type quat)
  (field-of-view (float *outside-field-of-view* 1d0) :type double-float)
  (near 0.1d0 :type double-float))

(defun camera-clip-rows! (out camera aspect)
  "Write the four clip rows (x y z w, four singles each) for camera-relative
points (p, 1) in ICRF axes into OUT: x = (f / aspect)(p . right),
y = s f (p . down), z = near, w = p . forward, f = 1 / tan(fov / 2):
reversed-Z with an infinite far plane."
  (declare (type (simple-array single-float (*)) out) (type real aspect))
  (let* ((q (camera-orientation camera))
         (f (/ (tan (* 0.5d0 (camera-field-of-view camera)))))
         (fx (/ f (float aspect 1d0)))
         (fy (* +clip-y-sign+ f))
         (axis (make-vec3)) (column (make-vec3)))
    (declare (dynamic-extent axis column) (type double-float f fx fy))
    (flet ((row (index i scale)
             (fill axis 0d0)
             (setf (aref axis i) 1d0)
             (quat-rotate! column q axis)
             (dotimes (k 3)
               (setf (aref out (+ index k)) (coerce (* scale (aref column k)) 'single-float)))
             (setf (aref out (+ index 3)) 0f0)))
      (row 0 1 fx)
      (row 4 2 fy)
      (fill out 0f0 :start 8 :end 11)
      (setf (aref out 11) (coerce (camera-near camera) 'single-float))
      (row 12 0 1d0))
    out))

(defun camera-eye (camera anchor-position &optional (out (make-vec3)))
  "The eye: ANCHOR-POSITION + the camera's offset, in double."
  (vec3-add! out anchor-position (camera-offset camera)))

(defun eye-relative-single (p eye out index)
  "P - EYE in double, then single, into OUT at INDEX."
  (relative-single p eye out index))

(defun sphere-offset-term (c r)
  "|c|^2 - r^2 for a sphere of radius R centred at C relative to the eye, as
h (2r + h) with h = |c| - r in double, so it keeps its precision near the
surface (docs/architecture.md \"Floating-origin pitfalls\")."
  (declare (type vec3 c) (type double-float r))
  (let ((h (- (vec3-norm c) r)))
    (* h (+ (* 2 r) h))))

(defun outside-camera-default! (camera r v)
  "Set CAMERA to the outside view's default pose for a ship at R with
velocity V (decision 30: Views and cameras): in the orbital frame,
*outside-distance* behind the ship along -prograde, raised
*outside-elevation* towards radial out, looking at the ship, radial out up."
  (declare (type vec3 r v))
  (let ((p (make-vec3)) (n (make-vec3)) (o (make-vec3))
        (x (make-vec3)) (y (make-vec3)) (z (make-vec3))
        (e (float *outside-elevation* 1d0))
        (d (float *outside-distance* 1d0)))
    (declare (dynamic-extent p n o x y z))
    (vec3-normalize! p v)
    (vec3-normalize! n (vec3-cross! n r v))
    (vec3-cross! o p n)
    (let ((ce (cos e)) (se (sin e)))
      ;; forward: towards the ship, down: away from radial out
      (vec3-scale! x p ce)
      (vec3-add-scaled! x x o (- se))
      (vec3-scale! z p (- se))
      (vec3-add-scaled! z z o (- ce))
      (vec3-cross! y z x)
      (vec3-scale! (camera-offset camera) x (- d)))
    (quat-from-basis! (camera-orientation camera) x y z)
    (setf (camera-anchor camera) :ship
          (camera-field-of-view camera) (float *outside-field-of-view* 1d0))
    camera))
