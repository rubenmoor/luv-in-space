(in-package #:luv-in-space)

;;; Ray-traced sphere impostors: the Sun and the Moon (decision 78: Sky in
;;; numbers: stars, Sun, Moon and Earth's look; docs/architecture.md
;;; "Rendering precision"). Each body's centre reaches the GPU relative to
;;; the eye from doubles, with c.c - r^2 in double (decision 5: Precision of
;;; positions); the fragment shader intersects the view ray with the sphere,
;;; discards misses and writes the hit's reversed-Z depth (decision 4: Depth
;;; strategy).

(defparameter *sun-radiance* 4.6d4
  "The Sun's centre radiance at every distance (decision 76); live.")
(defparameter *sun-limb-darkening* 0.6d0 "u in I/I0 = 1 - u (1 - mu); live.")
(defparameter *sun-colour* '(1.0d0 0.97d0 0.94d0) "Scaled to luminance 1; live.")
(defparameter *moon-albedo* 0.12d0 "Grey Lambert; live.")
(defparameter *impostor-full-screen-angle* (/ pi 4)
  "Angular radius beyond which a body draws as the full-screen triangle.")

(defconstant +impostor-floats+ 16
  "Per body in the frame block: centre and radius; quad axis u and c.c - r^2;
quad axis v; shade.")

(defun impostor-params! (out index centre radius offset-term camera)
  "Write a sphere's block into OUT at INDEX: CENTRE (relative to the eye,
double) and RADIUS as singles, the camera-facing quad's axes u and v (each
d tan rho long, so the quad holds the tangent cone's disc at the centre's
distance) and OFFSET-TERM (sphere-offset-term). Returns the draw form:
:full-screen when the eye is inside or the angular radius exceeds
*impostor-full-screen-angle*, nil when the cone is wholly behind CAMERA,
else :quad."
  (declare (type (simple-array single-float (*)) out) (type vec3 centre)
           (type double-float radius offset-term))
  (let* ((d (vec3-norm centre))
         (inside (or (<= offset-term 0d0) (<= d radius)))
         (rho (if inside pi (asin (/ radius d))))
         (forward (quat-rotate (camera-orientation camera) (vec3 1 0 0)))
         (angle (atan (vec3-norm (vec3-cross forward centre)) (vec3-dot forward centre)))
         (form (cond ((or inside (> rho *impostor-full-screen-angle*)) :full-screen)
                     ((> angle (+ (/ pi 2) rho)) nil)
                     (t :quad))))
    (fill out 0f0 :start index :end (+ index 12))
    (dotimes (k 3)
      (setf (aref out (+ index k)) (coerce (aref centre k) 'single-float)))
    (setf (aref out (+ index 3)) (coerce radius 'single-float)
          (aref out (+ index 7)) (coerce offset-term 'single-float))
    (when (eq form :quad)
      (let* ((axis (if (< (abs (aref centre 0)) (* 0.5d0 d)) (vec3 1 0 0) (vec3 0 1 0)))
             (u (vec3-normalize (vec3-cross centre axis)))
             (v (vec3-normalize (vec3-cross centre u)))
             (size (/ (* radius d) (sqrt offset-term))))
        (dotimes (k 3)
          (setf (aref out (+ index 4 k)) (coerce (* size (aref u k)) 'single-float)
                (aref out (+ index 8 k)) (coerce (* size (aref v k)) 'single-float)))))
    form))

(defun impostor-corners ()
  "The quad's six corners (z 0), then the full-screen triangle's three (z 1)."
  (coerce '(-1.0 -1.0 0.0 1.0 -1.0 0.0 1.0 1.0 0.0 -1.0 -1.0 0.0 1.0 1.0 0.0 -1.0 1.0 0.0
            -1.0 -1.0 1.0 3.0 -1.0 1.0 -1.0 3.0 1.0)
          '(simple-array single-float (*))))

(defun sun-shade! (out index)
  "The Sun's shade at INDEX: *sun-colour* at luminance 1 x *sun-radiance*,
then the limb darkening."
  (let ((scale (/ *sun-radiance* (apply #'srgb-luminance *sun-colour*))))
    (loop for c in *sun-colour*
          for k from index
          do (setf (aref out k) (coerce (* scale c) 'single-float)))
    (setf (aref out (+ index 3)) (coerce *sun-limb-darkening* 'single-float))))

(defun moon-shade! (out index moon sun)
  "The Moon's shade at INDEX from MOON and SUN (same origin): the direction
to the Sun and *moon-albedo* x (1 AU / d)^2."
  (let* ((to-sun (vec3-sub sun moon))
         (d (vec3-norm to-sun)))
    (dotimes (k 3)
      (setf (aref out (+ index k)) (coerce (/ (aref to-sun k) d) 'single-float)))
    (setf (aref out (+ index 3)) (coerce (* *moon-albedo* (expt (/ +au+ d) 2)) 'single-float))))

(defun impostor-uniforms! (out index state eye camera)
  "The Sun's and then the Moon's blocks into OUT from INDEX, seen from EYE
(relative to Earth) at the render STATE; returns their draw forms."
  (let* ((sun (render-state-sun state))
         (moon (render-state-moon state))
         (sun-centre (vec3-sub sun eye))
         (moon-centre (vec3-sub moon eye))
         (moon-index (+ index +impostor-floats+)))
    (sun-shade! out (+ index 12))
    (moon-shade! out (+ moon-index 12) moon sun)
    (values (impostor-params! out index sun-centre +radius-sun+
                              (sphere-offset-term sun-centre +radius-sun+) camera)
            (impostor-params! out moon-index moon-centre +radius-moon+
                              (sphere-offset-term moon-centre +radius-moon+) camera))))

(defun impostor-pipelines (renderer layout format)
  "The :sun and :moon live pipelines into the HDR target of FORMAT: opaque,
depth written, compare :greater."
  (dolist (role '(:sun :moon))
    (live-pipeline renderer role layout
                   '((:array-stride 12
                      :attributes ((:shader-location 0 :offset 0 :format :float32x3))))
                   :format format
                   :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                    :depth-compare :greater))))

(defun draw-impostor (pass renderer role form bind-group corners)
  "Draw ROLE in its FORM (impostor-params!) from CORNERS; nil draws nothing."
  (when form
    (luv:set-pipeline pass (renderer-pipeline renderer role))
    (luv:set-bind-group pass 0 bind-group)
    (luv:set-vertex-buffer pass 0 corners)
    (if (eq form :quad)
        (luv:draw pass 6)
        (luv:draw pass 3 1 6))))
