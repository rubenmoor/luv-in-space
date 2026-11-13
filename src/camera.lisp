(in-package #:luv-in-space)

;;; The camera and the one projection function (docs/architecture.md
;;; "Rendering precision"; decision 4: Depth strategy, decision 5: Precision
;;; of positions, decision 30: Views and cameras).

(defconstant +clip-y-sign+ 1d0
  "s: +1 where clip y points down (luv's Vulkan path), -1 for y up.")

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

(defun camera-anchor-position! (out camera render-state)
  "The camera's anchor in the render state: the ship's r, Earth's centre or
the target's r at t_r (the ship's without a target)."
  (declare (type vec3 out) (type camera camera))
  (ecase (camera-anchor camera)
    (:ship (replace out (render-state-r render-state)))
    (:target (if (render-state-target render-state)
                 (replace out (render-state-target-r render-state))
                 (replace out (render-state-r render-state))))
    (:earth (vec3-set! out 0d0 0d0 0d0))))

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


;;; The outside view (decision 82: Outside view camera: angles in a frame,
;;; toggles that keep the picture, distance and easing): the eye at the CoM
;;; + distance (cos el cos az, cos el sin az, −sin el) in a frame's FRD
;;; basis, looking at the CoM with the frame's +z down. Game state, not the
;;; sim's.

(defparameter *outside-azimuth* pi "Default (Home): behind the ship; decision 82.")
(defparameter *outside-elevation* (* 20 (/ pi 180)) "Default, radians towards the frame's −z; decision 82.")
(defparameter *outside-distance* 40d0 "Default, m; decision 82.")
(defparameter *outside-drag-rate* (* 0.25d0 (/ pi 180)) "Radians per pixel of right-drag; decision 82.")
(defparameter *outside-elevation-limit* (* 85 (/ pi 180)) "±, radians; decision 82.")
(defparameter *outside-zoom-step* 1.2d0 "Target distance factor per wheel notch; decision 82.")
(defparameter *outside-min-factor* 1.2d0 "Minimum distance over the bounding radius; decision 82.")
(defparameter *outside-max-distance* 5000d0 "m; decision 82.")
(defparameter *outside-zoom-tau* 0.15d0 "s of real time, the distance's easing in log; decision 82.")
(declaim (type double-float *outside-azimuth* *outside-elevation* *outside-distance*
               *outside-drag-rate* *outside-elevation-limit* *outside-zoom-step*
               *outside-min-factor* *outside-max-distance* *outside-zoom-tau*))

(defstruct (outside-view (:constructor new-outside-view ()))
  (frame :orbital :type (member :orbital :locked))
  (azimuth *outside-azimuth* :type double-float)
  (elevation *outside-elevation* :type double-float)
  (distance *outside-distance* :type (double-float (0d0)))
  (target *outside-distance* :type (double-float (0d0))))

(defun outside-view-reset! (view)
  "Home: the default pose, in the orbital frame."
  (let ((default (new-outside-view)))
    (setf (outside-view-frame view) (outside-view-frame default)
          (outside-view-azimuth view) (outside-view-azimuth default)
          (outside-view-elevation view) (outside-view-elevation default)
          (outside-view-distance view) (outside-view-distance default)
          (outside-view-target view) (outside-view-target default))
    view))

(defun basis-column! (m j v)
  (declare (type mat3 m) (type vec3 v) (type (integer 0 2) j))
  (dotimes (i 3 m)
    (setf (aref m (+ (* 3 i) j)) (aref v i))))

(defun outside-frame-basis! (out view render-state &optional (frame (outside-view-frame view)))
  "Write the frame's FRD axes as OUT's columns (frame -> ICRF): :orbital x
prograde, z radial in, from the render state's r, v; :locked the body axes
from its q."
  (declare (optimize speed) (type mat3 out) (type outside-view view))
  (let ((x (make-vec3)) (y (make-vec3)) (z (make-vec3)))
    (declare (dynamic-extent x y z))
    (ecase frame
      (:orbital
       (let ((r (render-state-r render-state)))
         (vec3-normalize! x (render-state-v render-state))
         ;; y = −(r × v)/|r × v|, z = x × y
         (vec3-normalize! y (vec3-cross! y x r))
         (vec3-cross! z x y)))
      (:locked
       (let ((q (render-state-q render-state)))
         (quat-rotate! x q (vec3-set! x 1d0 0d0 0d0))
         (quat-rotate! y q (vec3-set! y 0d0 1d0 0d0))
         (quat-rotate! z q (vec3-set! z 0d0 0d0 1d0)))))
    (basis-column! out 0 x)
    (basis-column! out 1 y)
    (basis-column! out 2 z)))

(declaim (inline outside-direction! outside-ease-distance))
(defun outside-direction! (out azimuth elevation)
  "The unit eye direction (cos el cos az, cos el sin az, −sin el)."
  (declare (type vec3 out) (type double-float azimuth elevation))
  (let ((ce (cos elevation)))
    (vec3-set! out (* ce (cos azimuth)) (* ce (sin azimuth)) (- (sin elevation)))))

(defun outside-ease-distance (distance target elapsed)
  "DISTANCE eased towards TARGET in log over ELAPSED real seconds; TARGET
when ELAPSED is nil."
  (declare (type (double-float (0d0)) distance target) (type (or null real) elapsed))
  (if elapsed
      (* target (exp (* (log (/ distance target))
                        (exp (- (/ (float elapsed 1d0) *outside-zoom-tau*))))))
      target))

(defun outside-camera-update! (camera view render-state elapsed)
  "Ease VIEW's distance over ELAPSED real seconds (snap when nil) and pose
CAMERA: the eye offset from the CoM in ICRF, looking at the CoM with the
frame's +z as down, 50° vertical. Conses nothing."
  (declare (optimize speed) (type camera camera) (type outside-view view)
           (type (or null double-float) elapsed))
  (setf (outside-view-distance view)
        (outside-ease-distance (outside-view-distance view) (outside-view-target view) elapsed))
  (let ((basis (make-mat3)) (d (make-vec3))
        (x (make-vec3)) (y (make-vec3)) (z (make-vec3)))
    (declare (dynamic-extent basis d x y z))
    (outside-frame-basis! basis view render-state)
    (outside-direction! d (outside-view-azimuth view) (outside-view-elevation view))
    (mat3*vec3! d basis d)
    (vec3-scale! (camera-offset camera) d (outside-view-distance view))
    ;; forward towards the CoM; down the frame's +z without its forward part
    (vec3-scale! x d -1d0)
    (vec3-set! z (aref basis 2) (aref basis 5) (aref basis 8))
    (vec3-add-scaled! z z x (- (vec3-dot z x)))
    (vec3-normalize! z z)
    (vec3-cross! y z x)
    (quat-from-basis! (camera-orientation camera) x y z)
    (setf (camera-anchor camera) :ship
          (camera-field-of-view camera) (float *outside-field-of-view* 1d0))
    camera))

(defun outside-view-toggle-frame! (view render-state)
  "C: the other frame, its az and el chosen so the eye offset stays."
  (let ((from (make-mat3)) (to (make-mat3)) (d (make-vec3)))
    (declare (dynamic-extent from to d))
    (let ((other (if (eq (outside-view-frame view) :orbital) :locked :orbital)))
      (outside-frame-basis! from view render-state)
      (outside-frame-basis! to view render-state other)
      (outside-direction! d (outside-view-azimuth view) (outside-view-elevation view))
      (mat3*vec3! d from d)
      ;; into the other frame: the transpose of its basis
      (let ((cx (+ (* (aref to 0) (aref d 0)) (* (aref to 3) (aref d 1)) (* (aref to 6) (aref d 2))))
            (cy (+ (* (aref to 1) (aref d 0)) (* (aref to 4) (aref d 1)) (* (aref to 7) (aref d 2))))
            (cz (+ (* (aref to 2) (aref d 0)) (* (aref to 5) (aref d 1)) (* (aref to 8) (aref d 2)))))
        (setf (outside-view-frame view) other
              (outside-view-azimuth view) (mod (atan cy cx) (* 2 pi))
              (outside-view-elevation view) (atan (- cz) (sqrt (+ (* cx cx) (* cy cy))))))))
  view)

(defun outside-view-drag! (view dx dy)
  "Right-drag by DX, DY pixels (luv's y down): mouse right moves the eye to
the viewer's right, mouse up raises it (decision 82's sign)."
  (let ((limit (float *outside-elevation-limit* 1d0)))
    (setf (outside-view-azimuth view)
          (mod (- (outside-view-azimuth view) (* *outside-drag-rate* dx)) (* 2 pi))
          (outside-view-elevation view)
          (max (- limit) (min limit (- (outside-view-elevation view) (* *outside-drag-rate* dy)))))
    view))

(defun outside-min-distance (m-p &optional load)
  "1.2 × the largest distance from the stack's CoM at M-P kg of propellant to
a corner of a part's box in *ship-parts*, or, with the stack-load LOAD, to
its bounding sphere (its CoM's distance and its definition's bound) if that
is larger (decision 124: Meshes of the places: models, scenery liveries,
lights, far dots, the grapple's claws, floodlight and range finder)."
  (let ((com (mass-properties-com (ship-mass-properties (float m-p 1d0) load)))
        (radius 0d0))
    (loop for (nil nil centres size) in *ship-parts*
          do (dolist (c centres)
               (loop for sx in '(-0.5d0 0.5d0)
                     do (loop for sy in '(-0.5d0 0.5d0)
                              do (loop for sz in '(-0.5d0 0.5d0)
                                       for corner = (vec3 (+ (first c) (* sx (first size)))
                                                          (+ (second c) (* sy (second size)))
                                                          (+ (third c) (* sz (third size))))
                                       do (setf radius (max radius (vec3-norm (vec3-sub corner com)))))))))
    (when load
      (setf radius (max radius (+ (vec3-norm (vec3-sub (stack-load-com load) com))
                                  (object-def-bound (stack-load-def load))))))
    (* *outside-min-factor* radius)))

(defun outside-view-zoom! (view notches m-p &optional load)
  "The wheel: the target distance × 1.2^(−NOTCHES), clamped to
[outside-min-distance (with the stack-load LOAD), 5 km]."
  (setf (outside-view-target view)
        (max (outside-min-distance m-p load)
             (min (float *outside-max-distance* 1d0)
                  (* (outside-view-target view) (expt *outside-zoom-step* (- notches))))))
  view)


;;; The map view (decision 83: Map camera: framing on opening, focus, zoom,
;;; drag and exposure): the eye at the focus + distance (cos el cos az e1 +
;;; cos el sin az e2 + sin el n) in a basis fixed when the map opens, looking
;;; at the focus with n's projection as up. Game state, not the sim's.

(defparameter *map-azimuth* (* -45 (/ pi 180)) "On opening (Home), radians; decision 83.")
(defparameter *map-elevation* (* 30 (/ pi 180)) "On opening, radians above the orbit plane; decision 83.")
(defparameter *map-opening-factor* 2.5d0 "Opening distance over max(apoapsis radius, |r|); decision 83.")
(defparameter *map-fov* (* 50 (/ pi 180)) "Vertical, radians; decision 83.")
(defparameter *map-min-distance-ship* 50d0 "m; decision 83.")
(defparameter *map-min-factor-earth* 1.2d0 "Minimum distance over R⊕; decision 83.")
(defparameter *map-max-distance* 1.5d13 "m, about 100 AU; decision 83.")
(defparameter *map-zoom-tau* 0.15d0 "s of real time, the distance's easing in log; decision 83.")
(defparameter *map-drag-rate* (* 0.25d0 (/ pi 180)) "Radians per pixel of right-drag; decision 83.")
(defparameter *map-elevation-limit* (* 85 (/ pi 180)) "±, radians; decision 83.")
(defparameter *map-zoom-step* 1.2d0 "Target distance factor per wheel notch; decision 83.")
(defparameter *map-page-factor* 10d0 "Target distance factor per Page key; decision 83.")
(defparameter *map-degenerate-h* 1d-9 "|r × v| below this × |r||v|: n = ICRF z; decision 83.")
(declaim (type double-float *map-azimuth* *map-elevation* *map-opening-factor* *map-fov*
               *map-min-distance-ship* *map-min-factor-earth* *map-max-distance*
               *map-zoom-tau* *map-drag-rate* *map-elevation-limit* *map-zoom-step*
               *map-page-factor* *map-degenerate-h*))

(defstruct (map-view (:constructor new-map-view ()))
  (focus :earth :type (member :earth :ship :target))
  (bound 0d0 :type double-float)        ; the target's bound while focused, m
  (e1 (vec3 1 0 0) :type vec3)
  (e2 (vec3 0 1 0) :type vec3)
  (n (vec3 0 0 1) :type vec3)
  (azimuth *map-azimuth* :type double-float)
  (elevation *map-elevation* :type double-float)
  (distance *map-max-distance* :type (double-float (0d0)))
  (target *map-max-distance* :type (double-float (0d0))))

(defun map-min-distance (focus &optional (bound 0d0))
  "50 m around the ship, 1.2 R⊕ from Earth's centre, max(50 m, 2 × BOUND)
from the target (decision 125: Map in milestone 3: the trajectory line, line
styles, plan display, markers, labels, click and focus)."
  (ecase focus
    (:ship *map-min-distance-ship*)
    (:target (max *map-min-distance-ship* (* 2 bound)))
    (:earth (* *map-min-factor-earth* +radius-earth+))))

(defun map-clamp-distance (view distance)
  (max (map-min-distance (map-view-focus view) (map-view-bound view))
       (min *map-max-distance* distance)))

(defun map-view-set-focus! (view focus &optional (bound 0d0))
  "FOCUS (:ship, :earth or :target with its BOUND), keeping basis, angles and
distance, clamped to the new focus's range."
  (setf (map-view-focus view) focus
        (map-view-bound view) (float bound 1d0)
        (map-view-target view) (map-clamp-distance view (map-view-target view))
        (map-view-distance view) (map-clamp-distance view (map-view-distance view)))
  view)

(defun target-bound (key)
  "Target KEY's object's bound, m; 0 for a point on rails (Skua Yard's drop point)."
  (let ((object (target-object-key key)))
    (if object (object-def-bound (object-definition object)) 0d0)))

(defun map-view-open! (view render-state)
  "M or Home: the basis from the render state's r, v, focus Earth, the
opening angles, distance and target 2.5 × max(apoapsis radius, |r|)."
  (let* ((r (render-state-r render-state))
         (v (render-state-v render-state))
         (e1 (map-view-e1 view)) (e2 (map-view-e2 view)) (n (map-view-n view))
         (rn (vec3-norm r)))
    (vec3-cross! n r v)
    (if (< (vec3-norm n) (* *map-degenerate-h* rn (vec3-norm v)))
        (vec3-set! n 0d0 0d0 1d0)
        (vec3-normalize! n n))
    (vec3-scale! e1 r (/ rn))
    ;; r ∥ n only in the degenerate case with r along ICRF z
    (vec3-add-scaled! e1 e1 n (- (vec3-dot e1 n)))
    (if (< (vec3-norm e1) 1d-9)
        (vec3-set! e1 1d0 0d0 0d0)
        (vec3-normalize! e1 e1))
    (vec3-cross! e2 n e1)
    (let* ((apoapsis (orbit-apoapsis-radius (state->orbit r v +mu-earth+ (render-state-t render-state))))
           (distance (* *map-opening-factor* (if apoapsis (max apoapsis rn) rn))))
      (setf (map-view-focus view) :earth
            (map-view-bound view) 0d0
            (map-view-azimuth view) *map-azimuth*
            (map-view-elevation view) *map-elevation*
            (map-view-target view) (map-clamp-distance view distance)
            (map-view-distance view) (map-view-target view))))
  view)

(defun map-view-cycle-focus! (view &optional target)
  "Tab: the ship → Earth → TARGET (a target key, only when there is one) →
the ship, keeping the basis, angles and distance, clamped to the new focus's
range (decision 125)."
  (ecase (map-view-focus view)
    (:ship (map-view-set-focus! view :earth))
    (:earth (if target
                (map-view-set-focus! view :target (target-bound target))
                (map-view-set-focus! view :ship)))
    (:target (map-view-set-focus! view :ship))))

(defun map-view-zoom! (view factor)
  "The target distance × FACTOR, clamped: the wheel 1.2^(−notches), Page-up
1/10, Page-down 10."
  (setf (map-view-target view)
        (map-clamp-distance view (* (map-view-target view) (float factor 1d0))))
  view)

(defun map-view-drag! (view dx dy)
  "Right-drag by DX, DY pixels with decision 82's signs."
  (let ((limit *map-elevation-limit*))
    (setf (map-view-azimuth view)
          (- (map-view-azimuth view) (* *map-drag-rate* dx))
          (map-view-elevation view)
          (max (- limit) (min limit (- (map-view-elevation view) (* *map-drag-rate* dy)))))
    view))

(declaim (inline map-ease-distance))
(defun map-ease-distance (distance target elapsed)
  "DISTANCE eased towards TARGET in log over ELAPSED real seconds; TARGET
when ELAPSED is nil."
  (declare (type (double-float (0d0)) distance target) (type (or null real) elapsed))
  (if elapsed
      (* target (exp (* (log (/ distance target))
                        (exp (- (/ (float elapsed 1d0) *map-zoom-tau*))))))
      target))

(defun map-camera-update! (camera view render-state elapsed)
  "Ease VIEW's distance over ELAPSED real seconds (snap when nil) and pose
CAMERA anchored on the focus, looking at it with n's projection as up, 50°
vertical. Conses nothing."
  (declare (optimize speed) (type camera camera) (type map-view view)
           (type (or null double-float) elapsed))
  (when (and (eq (map-view-focus view) :target) (not (render-state-target render-state)))
    (map-view-set-focus! view :ship))
  (setf (map-view-distance view)
        (map-ease-distance (map-view-distance view) (map-view-target view) elapsed))
  (let ((d (make-vec3)) (x (make-vec3)) (y (make-vec3)) (z (make-vec3))
        (az (map-view-azimuth view)) (el (map-view-elevation view)))
    (declare (dynamic-extent d x y z))
    (let ((ce (cos el)))
      (vec3-scale! d (map-view-e1 view) (* ce (cos az)))
      (vec3-add-scaled! d d (map-view-e2 view) (* ce (sin az)))
      (vec3-add-scaled! d d (map-view-n view) (sin el)))
    (vec3-scale! (camera-offset camera) d (map-view-distance view))
    ;; forward towards the focus; down is −n without its forward part
    (vec3-scale! x d -1d0)
    (vec3-scale! z (map-view-n view) -1d0)
    (vec3-add-scaled! z z x (- (vec3-dot z x)))
    (vec3-normalize! z z)
    (vec3-cross! y z x)
    (quat-from-basis! (camera-orientation camera) x y z)
    (setf (camera-anchor camera) (map-view-focus view)
          (camera-field-of-view camera) *map-fov*)
    camera))


;;; The cockpit view (decision 126: Cockpit structure and view in milestone 3:
;;; frames, draw order, Slug text, head look, exposure, cab light, portholes,
;;; bezels, tape labels): the eye at *cockpit-eye* in the ship's body axes, the
;;; head turned by yaw (positive right) and pitch (positive down) within limits,
;;; the vertical field of view eased by the wheel. Game state, not the sim's.

(defparameter *cockpit-eye* (vec3 14.1d0 0d0 -3.6d0)
  "The pilot's eye, m in body axes (FRD) from R; decision 126.")
(defparameter *cockpit-yaw-limit* (* 150 (/ pi 180)) "±, radians; decision 126.")
(defparameter *cockpit-pitch-up-limit* (* 60 (/ pi 180)) "Pitch down to this negative, radians.")
(defparameter *cockpit-pitch-down-limit* (* 80 (/ pi 180)) "Radians.")
(defparameter *cockpit-drag-rate* (* 0.25d0 (/ pi 180)) "Radians per pixel of right-drag.")
(defparameter *cockpit-home-pitch* (* 16 (/ pi 180)) "Home: radians down.")
(defparameter *cockpit-home-fov* (* 70 (/ pi 180)) "Home: vertical, radians.")
(defparameter *cockpit-min-fov* (* 20 (/ pi 180)) "Radians.")
(defparameter *cockpit-max-fov* (* 90 (/ pi 180)) "Radians.")
(defparameter *cockpit-zoom-step* 1.2d0 "tan(fov/2) factor per wheel notch.")
(defparameter *cockpit-zoom-tau* 0.15d0 "s of real time, the field of view's easing in log.")
(declaim (type double-float *cockpit-yaw-limit* *cockpit-pitch-up-limit* *cockpit-pitch-down-limit*
               *cockpit-drag-rate* *cockpit-home-pitch* *cockpit-home-fov* *cockpit-min-fov*
               *cockpit-max-fov* *cockpit-zoom-step* *cockpit-zoom-tau*))

(defstruct (cockpit-view (:constructor new-cockpit-view ()))
  (yaw 0d0 :type double-float)
  (pitch *cockpit-home-pitch* :type double-float)
  (fov *cockpit-home-fov* :type double-float)
  (target-fov *cockpit-home-fov* :type double-float)
  (scratch (make-camera) :type camera))

(defun cockpit-view-reset! (view)
  "Home: yaw 0, pitch 16° down, field of view 70°."
  (setf (cockpit-view-yaw view) 0d0
        (cockpit-view-pitch view) *cockpit-home-pitch*
        (cockpit-view-fov view) *cockpit-home-fov*
        (cockpit-view-target-fov view) *cockpit-home-fov*)
  view)

(defun cockpit-view-drag! (view dx dy)
  "Right-drag by DX, DY pixels (luv's y down): mouse right turns the head
right, mouse up raises it; clamped to the limits."
  (setf (cockpit-view-yaw view)
        (max (- *cockpit-yaw-limit*)
             (min *cockpit-yaw-limit* (+ (cockpit-view-yaw view) (* *cockpit-drag-rate* dx))))
        (cockpit-view-pitch view)
        (max (- *cockpit-pitch-up-limit*)
             (min *cockpit-pitch-down-limit*
                  (+ (cockpit-view-pitch view) (* *cockpit-drag-rate* dy)))))
  view)

(defun cockpit-view-zoom! (view notches)
  "The wheel: the target's tan(fov/2) ÷ 1.2^NOTCHES (up narrows), within 20°
to 90°."
  (let ((tan-half (/ (tan (* 0.5d0 (cockpit-view-target-fov view)))
                     (expt *cockpit-zoom-step* notches))))
    (setf (cockpit-view-target-fov view)
          (max *cockpit-min-fov* (min *cockpit-max-fov* (* 2d0 (atan tan-half))))))
  view)

(declaim (inline cockpit-ease-fov))
(defun cockpit-ease-fov (fov target elapsed)
  "FOV eased towards TARGET in log of tan(fov/2) over ELAPSED real seconds;
TARGET when ELAPSED is nil."
  (declare (type double-float fov target) (type (or null double-float) elapsed))
  (if elapsed
      (let ((a (tan (* 0.5d0 fov))) (b (tan (* 0.5d0 target))))
        (* 2d0 (atan (* b (exp (* (log (/ a b))
                                  (exp (- (/ elapsed *cockpit-zoom-tau*)))))))))
      target))

(defun cockpit-head-quat! (out view)
  "The head's rotation in body axes: yaw about z, then pitch (down positive)
about y; OUT looks along (cos p cos y, cos p sin y, sin p)."
  (declare (optimize speed) (type quat out) (type cockpit-view view))
  (let* ((hy (* 0.5d0 (cockpit-view-yaw view)))
         (hp (* -0.5d0 (cockpit-view-pitch view)))
         (qz (make-quat)) (qy (make-quat)))
    (declare (dynamic-extent qz qy))
    (quat-set! qz (cos hy) 0d0 0d0 (sin hy))
    (quat-set! qy (cos hp) 0d0 (sin hp) 0d0)
    (quat-mul! out qz qy)))

(defun cockpit-camera-update! (camera view render-state elapsed)
  "Ease VIEW's field of view over ELAPSED real seconds (snap when nil) and
pose CAMERA at the eye: offset q (e − c) from R (c the stack's CoM, as
encode-ship receives it), oriented q × head. Conses nothing."
  (declare (optimize speed) (type camera camera) (type cockpit-view view)
           (type (or null double-float) elapsed))
  (setf (cockpit-view-fov view)
        (cockpit-ease-fov (cockpit-view-fov view) (cockpit-view-target-fov view) elapsed))
  (let ((q (render-state-q render-state))
        (head (make-quat)) (d (make-vec3)))
    (declare (dynamic-extent head d))
    (vec3-sub! d *cockpit-eye* (render-state-com render-state))
    (quat-rotate! (camera-offset camera) q d)
    (cockpit-head-quat! head view)
    (quat-mul! (camera-orientation camera) q head)
    (setf (camera-anchor camera) :ship
          (camera-field-of-view camera) (cockpit-view-fov view))
    camera))

(defun cab-clip-rows! (out view aspect)
  "The clip rows (as camera-clip-rows!) for points in body axes relative to
the eye: the head camera's projection without the ship's rotation."
  (let ((camera (cockpit-view-scratch view)))
    (cockpit-head-quat! (camera-orientation camera) view)
    (setf (camera-field-of-view camera) (cockpit-view-fov view))
    (camera-clip-rows! out camera aspect)))
