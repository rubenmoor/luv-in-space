(in-package #:luv-in-space)

;;; The Sun, Earth and Moon: constants, rotations and body-fixed frames
;;; (docs/architecture.md "Frames"). Ephemerides come with plan 13's piece 4.

(defstruct (body (:constructor %make-body))
  (key nil :type keyword :read-only t)
  (name "" :type string :read-only t)
  (mu 0d0 :type double-float :read-only t)
  (radius 0d0 :type double-float :read-only t)
  (crash-radius 0d0 :type double-float :read-only t)  ; decision 34: How death works
  (parent nil :type (or null keyword) :read-only t)
  (semi-major-axis nil :type (or null double-float) :read-only t) ; mean, about the parent
  (soi-radius nil :type (or null double-float) :read-only t)      ; Laplace sphere
  (j2 0d0 :type double-float :read-only t))

(defun laplace-sphere (a mu parent-mu)
  "r = a (m/M)^(2/5)."
  (* a (expt (/ mu parent-mu) 0.4d0)))

(defun make-body (key name mu radius crash-radius &key parent semi-major-axis (j2 0d0))
  (%make-body :key key :name name :mu mu :radius radius :crash-radius crash-radius
              :parent parent :semi-major-axis semi-major-axis :j2 j2
              :soi-radius (and parent
                               (laplace-sphere semi-major-axis mu
                                               (ecase parent
                                                 (:sun +mu-sun+)
                                                 (:earth +mu-earth+))))))

(defparameter *bodies*
  (list (make-body :sun "Sun" +mu-sun+ +radius-sun+ +radius-sun+)
        (make-body :earth "Earth" +mu-earth+ +radius-earth+ (+ +radius-earth+ 100d3)
                   :parent :sun
                   :semi-major-axis (* 1.00000261d0 +au+) ; EMB, Standish, Table 1
                   :j2 +j2-earth+)
        (make-body :moon "Moon" +mu-moon+ +radius-moon+ +radius-moon+
                   :parent :earth
                   :semi-major-axis 3.84399d8))           ; Simon et al. 1994, NASA Moon fact sheet
  "Every body, in a fixed order (parents first).")

(defun body (designator)
  "The body of a key, or the body itself."
  (if (body-p designator)
      designator
      (or (find designator *bodies* :key #'body-key)
          (error "No body ~S." designator))))

;;; Rotations. A body-fixed matrix maps body-centred inertial (ICRF) axes
;;; to body-fixed ones.

(defun passive-rz (angle)
  (let ((c (cos angle)) (s (sin angle)))
    (mat3 c s 0 (- s) c 0 0 0 1)))

(defun passive-rx (angle)
  (let ((c (cos angle)) (s (sin angle)))
    (mat3 1 0 0 0 c s 0 (- s) c)))

(defun earth-rotation-revolutions (time)
  "Earth Rotation Angle in revolutions, [0, 1) (IERS Conventions 2010):
0.7790572732640 + 1.00273781191135448 D_U, the whole days of D_U dropped
before they are multiplied."
  (declare (type double-float time))
  (let ((d (t->ut1-days time)))
    (mod (+ (nth-value 1 (ffloor d)) 0.7790572732640d0 (* 0.00273781191135448d0 d))
         1d0)))

(defun earth-rotation-angle (time)
  "θ in radians, [0, 2π)."
  (* 2 pi (earth-rotation-revolutions time)))

(defun iau-body-fixed-matrix (alpha0 delta0 w)
  "R_z(W) R_x(90° − δ₀) R_z(90° + α₀) from the IAU pole and prime meridian,
in degrees (Archinal et al. 2018)."
  (mat3* (passive-rz (deg->rad w))
         (mat3* (passive-rx (deg->rad (- 90 delta0)))
                (passive-rz (deg->rad (+ 90 alpha0))))))

(defun body-fixed-matrix (body time)
  "Inertial -> body-fixed rotation of BODY at TIME; IAU WGCCRE 2015 leading
terms (Archinal et al. 2018), no periodic terms."
  (declare (type double-float time))
  (let* ((d (/ time +day+))
         (centuries (t->julian-centuries time)))
    (ecase (body-key (body body))
      (:earth (passive-rz (earth-rotation-angle time)))
      (:moon (iau-body-fixed-matrix (+ 269.9949d0 (* 0.0031d0 centuries))
                                    (+ 66.5392d0 (* 0.0130d0 centuries))
                                    (mod (+ 38.3213d0 (* 13.17635815d0 d) (* -1.4d-12 d d))
                                         360d0)))
      (:sun (iau-body-fixed-matrix 286.13d0 63.87d0 (mod (+ 84.176d0 (* 14.1844000d0 d)) 360d0))))))

(defun latitude-longitude (body r time)
  "(values latitude longitude) in radians of a body-centred inertial position;
longitude east, in (−π, π]."
  (let ((fixed (mat3*vec3 (body-fixed-matrix body time) r)))
    (with-vec3 (((x y z) fixed))
      (values (asin (/ z (vec3-norm fixed))) (atan y x)))))

(defun body-texture-uv (direction)
  "(values u v) of a body-fixed direction on an equirectangular map with the
prime meridian in the centre column and north at the top."
  (with-vec3 (((x y z) direction))
    (values (+ 0.5d0 (/ (atan y x) (* 2 pi)))
            (- 0.5d0 (/ (asin (/ z (vec3-norm direction))) pi)))))
