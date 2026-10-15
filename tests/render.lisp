(in-package #:luv-in-space.tests)

;;; render.lisp: the render state (decision 77: The frame in main.lisp:
;;; render state, capture of a given sim, part 4's placeholders and first
;;; asset) and the frame on the GPU ("GPU tests").

(defun stepped-sim (&key (sim (drift-sim)) (level 1) until)
  "SIM, a clock whose previous state is SIM's before one sim-step!, and that step's h."
  (let ((clock (lis::new-clock sim)))
    (values sim clock (lis::sim-step! sim level :until until))))

(defun ship-matches-p (state ship)
  (and (equalp (lis::render-state-r state) (lis::ship-state-r ship))
       (equalp (lis::render-state-v state) (lis::ship-state-v ship))
       (equalp (lis::render-state-q state) (lis::ship-state-q ship))
       (equalp (lis::render-state-w state) (lis::ship-state-w ship))))

(define-test render-state-ends-bit-for-bit
  (multiple-value-bind (sim clock h) (stepped-sim)
    (let ((state (lis::make-render-state))
          (previous (lis::copy-ship-state (lis::clock-previous clock))))
      (setf (lis::clock-accumulator clock) 0d0)
      (lis::render-state-update! state clock sim)
      (true (ship-matches-p state previous))
      (is = (lis::clock-previous-t clock) (lis::render-state-t state))
      (setf (lis::clock-accumulator clock) h)
      (lis::render-state-update! state clock sim)
      (true (ship-matches-p state (lis::sim-ship sim)))
      (is = (lis::sim-t sim) (lis::render-state-t state))
      (is = (lis::ship-state-m-p (lis::sim-ship sim)) (lis::render-state-m-p state))
      (true (equalp (lis::render-state-controls state) (lis::sim-controls sim))))))

(define-test render-state-hermite-midpoint
  ;; Half of a 2 s coarse step on the circular orbit against Kepler.
  (let ((t0 (lis::start-t)))
    (multiple-value-bind (sim clock h) (stepped-sim :level 1000 :until (+ t0 2d0))
      (is = 2d0 h)
      (setf (lis::clock-accumulator clock) 1d0)
      (let ((state (lis::render-state-update! (lis::make-render-state) clock sim)))
        (is = (+ t0 1d0) (lis::render-state-t state))
        (multiple-value-bind (r v) (lis::orbit-state (drift-orbit 0d0) (+ t0 1d0))
          (let ((dr (lis::vec3-norm (lis::vec3-sub (lis::render-state-r state) r)))
                (dv (lis::vec3-norm (lis::vec3-sub (lis::render-state-v state) v))))
            (true (< dr 1d-3) "position off by ~A m" dr)
            (true (< dv 1d-3) "velocity off by ~A m/s" dv)))
        (multiple-value-bind (sun) (lis::body-state :sun (+ t0 1d0))
          (true (equalp sun (lis::render-state-sun state))))
        (true (equalp (lis::body-fixed-matrix :earth (+ t0 1d0))
                      (lis::render-state-earth-matrix state)))))))

(define-test render-state-fallbacks
  (let ((state (lis::make-render-state)))
    ;; clock-restart!: the checkpoint as it is.
    (multiple-value-bind (sim clock) (stepped-sim)
      (let ((checkpoint (lis::take-checkpoint (lis::new-game-sim))))
        (step-sim sim 10 1)
        (lis::clock-restart! clock sim checkpoint)
        (setf (lis::clock-accumulator clock) 0.01d0)
        (lis::render-state-update! state clock sim)
        (true (ship-matches-p state (lis::sim-ship checkpoint)))
        (is = (lis::sim-t checkpoint) (lis::render-state-t state))))
    ;; A restore without the clock: t_prev stale, ahead of t.
    (multiple-value-bind (sim clock) (stepped-sim)
      (let ((checkpoint (lis::take-checkpoint (lis::new-game-sim))))
        (setf (lis::clock-previous-t clock) (+ (lis::sim-t sim) 1d0))
        (lis::restore-checkpoint! sim checkpoint)
        (setf (lis::clock-accumulator clock) 0.01d0)
        (lis::render-state-update! state clock sim)
        (true (ship-matches-p state (lis::sim-ship checkpoint)))))
    ;; Lost: the sim's state at its loss.
    (multiple-value-bind (sim clock h) (stepped-sim)
      (let ((loss (lis::make-loss :crash :earth (lis::sim-t sim) 7000d0 0d0 0d0)))
        (setf (lis::sim-loss sim) loss
              (lis::clock-accumulator clock) (* 0.5d0 h))
        (lis::render-state-update! state clock sim)
        (true (ship-matches-p state (lis::sim-ship sim)))
        (is eq loss (lis::render-state-loss state))))))

(define-test render-state-update-conses-nothing
  (multiple-value-bind (sim clock h) (stepped-sim)
    (setf (lis::clock-accumulator clock) (* 0.5d0 h))
    (let* ((state (lis::make-render-state))
           (consed (least-bytes-consed
                    (lambda () (dotimes (i 1000) (lis::render-state-update! state clock sim))))))
      (is = 0 consed))))

;;; The frame on the GPU: one hidden canvas, 320x200 (decision 2: Screenshots
;;; and GPU test: offscreen capture on a hidden canvas, headless on lavapipe).

(defun png-size (pathname)
  "Width and height from a PNG's IHDR chunk, and whether its signature is right."
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((header (make-array 24 :element-type '(unsigned-byte 8))))
      (read-sequence header in)
      (flet ((u32 (start) (reduce (lambda (a b) (+ (* a 256) b)) header
                                  :start start :end (+ start 4))))
        (values (u32 16) (u32 20)
                (equalp (subseq header 0 8) #(137 80 78 71 13 10 26 10)))))))

(defun pixel-luminance (pixels width x y)
  "Mean of the three colour bytes at X, Y (any channel order)."
  (let ((offset (* 4 (+ (* y width) x))))
    (/ (+ (aref pixels offset) (aref pixels (+ offset 1)) (aref pixels (+ offset 2))) 3)))

(defun pixel-red-blue (pixels format width x y)
  "Red and blue bytes at X, Y of packed PIXELS in FORMAT (RGBA or BGRA order)."
  (let ((offset (* 4 (+ (* y width) x)))
        (bgra (search "BGRA" (string format))))
    (values (aref pixels (+ offset (if bgra 2 0)))
            (aref pixels (+ offset (if bgra 0 2))))))

(defun pixel-colour (pixels width x y)
  (let ((offset (* 4 (+ (* y width) x))))
    (list (aref pixels offset) (aref pixels (+ offset 1)) (aref pixels (+ offset 2)))))

(defun pixel-mode (pixels width from to)
  "The commonest colour in rows FROM below TO, as a list of three bytes."
  (let ((counts (make-hash-table :test #'equal)))
    (loop for y from from below to
          do (dotimes (x width) (incf (gethash (pixel-colour pixels width x y) counts 0))))
    (loop with best and most = 0
          for colour being the hash-keys of counts using (hash-value count)
          when (> count most) do (setf best colour most count)
          finally (return best))))

(defun rows-unlike (pixels width from to colour)
  "The fraction of pixels in rows FROM below TO other than COLOUR."
  (/ (loop for y from from below to
           sum (loop for x below width count (not (equal colour (pixel-colour pixels width x y)))))
     (float (* width (- to from)))))

(defun rows-luminance (pixels width from to)
  "Mean luminance of rows FROM below TO."
  (/ (loop for y from from below to
           sum (loop for x below width sum (pixel-luminance pixels width x y)))
     (* width (- to from))))

(defun clear-of-ship! (camera &optional (distance 50d0))
  "CAMERA moved DISTANCE m forward from the ship, so the Mule is behind it."
  (lis::vec3-scale! (lis::camera-offset camera)
                    (lis::quat-rotate (lis::camera-orientation camera) (lis::vec3 1 0 0))
                    distance)
  camera)

(defun nadir-camera (camera state)
  "Straight down from the ship, prograde up."
  (let* ((x (lis::vec3-normalize (lis::vec3-scale (lis::render-state-r state) -1d0)))
         (z (lis::vec3-normalize (lis::vec3-cross x (lis::vec3-cross (lis::render-state-v state) x))))
         (z (lis::vec3-scale z -1d0))
         (y (lis::vec3-cross z x)))
    (lis::quat-from-basis! (lis::camera-orientation camera) x y z)
    (clear-of-ship! camera)))

(defun away-camera (camera state)
  "From the ship, away from Earth and the Sun: a dark view."
  (let* ((r (lis::render-state-r state))
         (n (lis::vec3-normalize (lis::vec3-cross r (lis::render-state-sun state)))))
    (clear-of-ship! (look-along! camera (lis::vec3-add (lis::vec3-scale n 2d0) (lis::vec3-normalize r))))))

(defun centre-quad (radiance &key (dx 0) (dy 0) (size 2))
  "Overlay vertices of a SIZE-pixel square of RADIANCE at the centre plus DX, DY pixels."
  (lis::quad 0.0 0.0 (- dx (/ size 2)) (- dy (/ size 2)) size (list radiance radiance radiance)))

(defun capture-with-quads (game png quads &key (strength lis::*bloom-strength*))
  "The away view with QUADS drawn as stars, at bloom STRENGTH; the pixels."
  (let ((saved lis::*bloom-strength*))
    (lis::call-on-canvas-thread game (lambda ()
                                       (setf lis::*bloom-strength* strength)
                                       (lis::set-test-quads (lis::game-renderer game) quads)))
    (unwind-protect
         (nth-value 3 (lis::capture-game-state game png :frames 1 :camera-fn #'away-camera))
      (lis::call-on-canvas-thread game (lambda ()
                                         (setf lis::*bloom-strength* saved)
                                         (lis::set-test-quads (lis::game-renderer game) nil))))))

(defun srgb-byte (x)
  (round (* 255 (if (<= x 0.0031308) (* 12.92 x) (- (* 1.055 (expt x (/ 2.4))) 0.055)))))

(defun pixel-rgb (pixels format width x y)
  (multiple-value-bind (red blue) (pixel-red-blue pixels format width x y)
    (list red (aref pixels (+ 1 (* 4 (+ (* y width) x)))) blue)))

;;; The Sun and the Moon (decision 78: Sky in numbers: stars, Sun, Moon and
;;; Earth's look; src/impostors.lisp).

(defun body-camera (key field-of-view)
  "A camera-fn: from the ship towards body KEY, a vertical FIELD-OF-VIEW in degrees."
  (lambda (camera state)
    (look-along! camera (lis::vec3-sub (ecase key
                                         (:sun (lis::render-state-sun state))
                                         (:moon (lis::render-state-moon state)))
                                       (lis::render-state-r state)))
    (clear-of-ship! camera)
    (setf (lis::camera-field-of-view camera) (deg field-of-view))))

(defun disc-pixels (key sim field-of-view height)
  "Body KEY's angular radius from SIM's ship, in pixels of a view of
FIELD-OF-VIEW degrees and HEIGHT pixels, at the view's centre."
  (let* ((state (sim-render-state sim))
         (centre (lis::vec3-sub (ecase key
                                  (:sun (lis::render-state-sun state))
                                  (:moon (lis::render-state-moon state)))
                                (lis::render-state-r state)))
         (radius (ecase key (:sun lis::+radius-sun+) (:moon lis::+radius-moon+))))
    (/ (* 0.5d0 height (tan (asin (/ radius (lis::vec3-norm centre)))))
       (tan (* 0.5d0 (deg field-of-view))))))

(defmacro with-canvas-values ((game &rest bindings) &body body)
  "BODY with the special variables of BINDINGS ((variable value) ...) set on
GAME's canvas thread, restored after."
  (let ((saved (loop repeat (length bindings) collect (gensym))))
    `(let ,(loop for g in saved for (variable) in bindings collect `(,g ,variable))
       (lis::call-on-canvas-thread ,game (lambda () (setf ,@(loop for (variable value) in bindings
                                                                append (list variable value)))))
       (unwind-protect (progn ,@body)
         (lis::call-on-canvas-thread ,game (lambda ()
                                             (setf ,@(loop for g in saved for (variable) in bindings
                                                           append (list variable g)))))))))

(defun sun-and-moon-checks (game png width height)
  (let ((cx (floor width 2)) (cy (floor height 2))
        ;; u = 340 deg: the Sun 10 deg from the zenith, Earth behind the
        ;; camera; u = 150 deg: in Earth's shadow, the Moon 26 deg from the zenith.
        (day (start-sim-at 340))
        (night (start-sim-at 150)))
    (flet ((capture (sim camera-fn)
             (nth-value 3 (lis::capture-game-state game png :sim sim :frames 1 :camera-fn camera-fn)))
           (at (pixels x y) (pixel-luminance pixels width x y)))
      ;; The disc's width on the centre row and its limb darkening, bloom
      ;; off. At decision 78's exposure 2e-5 the centre and 0.8 R read 229
      ;; and 220 (tone-map-srgb), in the tone curve's shoulder; at 1e-5 202
      ;; and 186.
      (with-canvas-values (game (lis::*bloom-strength* 0.0) (lis::*exposure-sunlit* 1d-5))
        (let* ((pixels (capture day (body-camera :sun 2)))
               (radius (disc-pixels :sun day 2 height))
               (lit (loop for x below width count (> (at pixels x cy) 60))))
          (true (< (abs (- lit (* 2 radius))) 2) "Sun ~D px wide, ~,1F expected" lit (* 2 radius))
          (true (> (- (at pixels cx cy) (at pixels (round (+ cx (* 0.8 radius))) cy)) 10)
                "centre ~,1F, at 0.8 R ~,1F" (at pixels cx cy)
                (at pixels (round (+ cx (* 0.8 radius))) cy))))
      ;; Glare from the bloom: 20 px from the centre, against the same view
      ;; without the Sun.
      (let ((glare (capture day (body-camera :sun 50)))
            (background (with-canvas-values (game (lis::*sun-radiance* 0d0))
                          (capture day (body-camera :sun 50)))))
        (loop for (x y) in (list (list (+ cx 20) cy) (list (- cx 20) cy)
                                 (list cx (+ cy 20)) (list cx (- cy 20)))
              do (true (> (- (at glare x y) (at background x y)) 30)
                       "glare at ~D, ~D: ~,1F over ~,1F" x y (at glare x y) (at background x y)))
        ;; Round: the glow's extent (more than 5 over the background) along
        ;; the row and along the diagonals x sqrt 2. The tent's rounded
        ;; square measured 64.5 and 69.7 px (8 %), the disc kernel 74.5 and
        ;; 76.0 (2 %); task 145 asked for under 15 %.
        (flet ((run (dx dy)
                 (loop for k from 0
                       for x = (+ cx (* k dx)) for y = (+ cy (* k dy))
                       while (and (< -1 x width) (< -1 y height)
                                  (> (- (at glare x y) (at background x y)) 5))
                       finally (return k))))
          (let ((row (/ (+ (run 1 0) (run -1 0)) 2.0))
                (diagonal (* (sqrt 2.0) (/ (+ (run 1 1) (run -1 -1) (run 1 -1) (run -1 1)) 4.0))))
            (true (< (abs (- (/ diagonal row) 1)) 0.05)
                  "glow ~,1F px along the row, ~,1F along the diagonal" row diagonal))))
      ;; Behind Earth at u = 180 deg: hidden by depth.
      (let ((pixels (capture (start-sim-at 180) (body-camera :sun 20))))
        (true (<= (nth-value 2 (brightest-pixel pixels width height)) 250)
              "brightest ~,1F towards the hidden Sun" (nth-value 2 (brightest-pixel pixels width height))))
      ;; The Moon, grey, lit as its phase says; bloom off, as it spreads the
      ;; disc at the deep exposure.
      (let* ((pixels (with-canvas-values (game (lis::*bloom-strength* 0.0))
                       (capture night (body-camera :moon 2))))
             (radius (disc-pixels :moon night 2 height))
             (state (sim-render-state night))
             (moon (lis::render-state-moon state))
             (phase (angle-between (lis::vec3-sub (lis::render-state-sun state) moon)
                                   (lis::vec3-sub (lis::render-state-r state) moon)))
             (lit 0) (coloured 0))
        (dotimes (y height)
          (dotimes (x width)
            (when (and (< (+ (expt (- (+ x 0.5) cx) 2) (expt (- (+ y 0.5) cy) 2)) (expt (+ radius 2) 2))
                       (> (at pixels x y) 30))
              (incf lit)
              (destructuring-bind (r g b) (pixel-colour pixels width x y)
                (unless (and (<= (abs (- r g)) 4) (<= (abs (- g b)) 4))
                  (incf coloured))))))
        (is = 0 coloured)
        (let ((fraction (/ lit (* pi radius radius)))
              (expected (* 0.5 (+ 1 (cos phase)))))
          (true (< (abs (- fraction expected)) 0.1)
                "Moon lit ~,3F of its disc, phase ~,1F deg: ~,3F" fraction (lis::rad->deg phase) expected)))
      (lis::game-load-state! game (lis::new-game-sim)))))

;;; Earth and its halo (decision 78; src/impostors.lisp).

(defun horizon-camera (camera state)
  "From the ship towards the horizon on the Sun's side, down by the dip
acos(R / r), so the limb crosses the centre row; radial out up."
  (let* ((r (lis::render-state-r state))
         (up (lis::vec3-normalize r))
         (sun (lis::vec3-normalize (lis::render-state-sun state)))
         (level (lis::vec3-normalize (lis::vec3-add-scaled sun up (- (lis::vec3-dot sun up)))))
         (dip (acos (/ lis::+radius-earth+ (lis::vec3-norm r))))
         (x (lis::vec3-add (lis::vec3-scale level (cos dip)) (lis::vec3-scale up (- (sin dip)))))
         (z (lis::vec3-scale (lis::vec3-add (lis::vec3-scale level (sin dip))
                                            (lis::vec3-scale up (cos dip)))
                             -1d0)))
    (lis::quat-from-basis! (lis::camera-orientation camera) x (lis::vec3-cross z x) z)
    (clear-of-ship! camera)))

(defun far-earth-camera (distance field-of-view)
  "A camera-fn: DISTANCE m from Earth's centre towards the Sun, looking at
Earth, a vertical FIELD-OF-VIEW in degrees."
  (lambda (camera state)
    (let ((sun (lis::vec3-normalize (lis::render-state-sun state))))
      (look-along! camera (lis::vec3-scale sun -1d0))
      (replace (lis::camera-offset camera)
               (lis::vec3-sub (lis::vec3-scale sun distance) (lis::render-state-r state)))
      (setf (lis::camera-field-of-view camera) (deg field-of-view)))))

(defun lit-run (pixels width height threshold &key vertical)
  "The length of the run of pixels above THRESHOLD through the centre, along
the centre row or, VERTICAL, the centre column."
  (let ((cx (floor width 2)) (cy (floor height 2)))
    (flet ((lit (i) (> (if vertical
                           (pixel-luminance pixels width cx i)
                           (pixel-luminance pixels width i cy))
                       threshold)))
      (let ((centre (if vertical cy cx)) (end (if vertical height width)))
        (+ 1 (loop for i downfrom (1- centre) to 0 while (lit i) count t)
           (loop for i from (1+ centre) below end while (lit i) count t))))))

(defun earth-checks (game png width height)
  (let ((cx (floor width 2)) (cy (floor height 2)))
    (flet ((capture (sim camera-fn)
             (nth-value 3 (lis::capture-game-state game png :sim sim :frames 1 :camera-fn camera-fn)))
           (at (pixels y) (pixel-luminance pixels width cx y)))
      ;; The horizon on the Sun's side at the start: space, then the halo
      ;; above the limb (the centre row), then the surface.
      (multiple-value-bind (path w h pixels format)
          (lis::capture-game-state game png :sim (lis::new-game-sim) :frames 1
                                            :camera-fn #'horizon-camera)
        (declare (ignore path w h))
        (let ((space (at pixels (- cy 30))))
          (loop for y from (- cy 6) to (- cy 2)
                do (multiple-value-bind (red blue) (pixel-red-blue pixels format width cx y)
                     (true (> blue red) "halo ~D px above the limb: red ~D, blue ~D" (- cy y) red blue))
                   (true (> (at pixels y) (+ space 20))
                         "halo ~D px above the limb ~,1F, space ~,1F" (- cy y) (at pixels y) space))
          (loop for y from (+ cy 4) to (+ cy 30) by 2
                do (true (> (at pixels y) (+ space 10))
                         "surface ~D px below the limb ~,1F, space ~,1F" (- y cy) (at pixels y) space))))
      ;; Night: straight down in Earth's shadow, dark.
      (let ((pixels (capture (start-sim-at 180) #'nadir-camera)))
        (true (< (at pixels cy) 8) "night side at the nadir ~,1F" (at pixels cy)))
      ;; Round from 40,000 km on the Sun's side, halo and bloom off; at the
      ;; exposure 1.0 the open Pacific falls to the background (lit runs of 83
      ;; and 32 px above 2 of 255), at 8 it reads above 10.
      (with-canvas-values (game (lis::*bloom-strength* 0.0) (lis::*halo-radiance* 0d0)
                                (lis::*exposure-sunlit* 8d0))
        (let* ((distance 4d7) (fov 30)
               (pixels (capture (lis::new-game-sim) (far-earth-camera distance fov)))
               (expected (/ (* height (tan (asin (/ lis::+radius-earth+ distance))))
                            (tan (* 0.5d0 (deg fov))))))
          (dolist (vertical '(nil t))
            (let ((run (lit-run pixels width height 10 :vertical vertical)))
              (true (<= (abs (- run expected)) 2) "Earth ~:[horizontally~;vertically~] ~D px, ~,1F expected"
                    vertical run expected)))))
      (lis::game-load-state! game (lis::new-game-sim)))))

;;; The stars (decision 78: Sky in numbers: stars, Sun, Moon and Earth's
;;; look), seen from Earth's shadow at u = 180 deg so the exposure is deep.

(defun sky-direction (ra dec)
  "The ICRF unit vector of RA, DEC in degrees."
  (let ((a (deg ra)) (d (deg dec)))
    (lis::vec3 (* (cos d) (cos a)) (* (cos d) (sin a)) (sin d))))

(defun sky-camera (ra dec field-of-view)
  "A camera-fn: from the ship towards RA, DEC, celestial north up, a vertical
FIELD-OF-VIEW in degrees."
  (lambda (camera state)
    (declare (ignore state))
    (let* ((x (sky-direction ra dec))
           (up (lis::vec3-normalize (lis::vec3-sub (lis::vec3 0 0 1)
                                                   (lis::vec3-scale x (aref x 2)))))
           (z (lis::vec3-scale up -1d0)))
      (lis::quat-from-basis! (lis::camera-orientation camera) x (lis::vec3-cross z x) z)
      (clear-of-ship! camera)
      (setf (lis::camera-field-of-view camera) (deg field-of-view)))))

(defun sky-pixel (camera-fn ra dec width height)
  "Where the projection puts the star at RA, DEC, in pixels (x right, y down)."
  (let ((camera (lis::make-camera))
        (rows (make-array 16 :element-type 'single-float))
        (d (sky-direction ra dec)))
    (funcall camera-fn camera nil)
    (lis::camera-clip-rows! rows camera (/ width height))
    (flet ((row (i) (loop for k below 3 sum (* (aref rows (+ i k)) (aref d k)))))
      (let ((w (row 12)))
        (values (* 0.5 width (+ 1 (/ (row 0) w))) (* 0.5 height (+ 1 (/ (row 4) w))))))))

(defun brightest-pixel (pixels width height &key (from-row 40) (x0 0) (x1 width) (y1 height))
  "The brightest pixel's centre below the label's rows, in the box given."
  (loop with best = -1 and bx and by
        for y from from-row below y1
        do (loop for x from x0 below x1
                 for l = (pixel-luminance pixels width x y)
                 when (> l best) do (setf best l bx x by y))
        finally (return (values (+ bx 0.5) (+ by 0.5) best))))

(defun count-above (pixels width height threshold &key (from-row 40))
  (loop for y from from-row below height
        sum (loop for x below width count (> (pixel-luminance pixels width x y) threshold))))

;;; The Mule (decision 80: The Mule's meshes in numbers: geometry, draws,
;;; materials and lights; src/meshes.lisp).

(defun ship-com (state)
  (lis::mass-properties-com (lis::ship-mass-properties (lis::render-state-m-p state))))

(defun ship-view (direction distance target)
  "A camera-fn: DISTANCE m from the body point TARGET along the body
DIRECTION, looking back at it, the cab (body -z) up."
  (lambda (camera state)
    (let* ((q (lis::render-state-q state))
           (d (lis::vec3-normalize (lis::quat-rotate q (lis::v3 direction))))
           (x (lis::vec3-scale d -1d0))
           (hint (lis::quat-rotate q (lis::vec3 0 0 -1)))
           (up (if (< (lis::vec3-norm (lis::vec3-cross hint x)) 0.1d0)
                   (lis::quat-rotate q (lis::vec3 1 0 0))
                   hint))
           (y (lis::vec3-normalize (lis::vec3-cross x up)))
           (z (lis::vec3-cross x y)))
      (lis::quat-from-basis! (lis::camera-orientation camera) x y z)
      (replace (lis::camera-offset camera)
               (lis::vec3-add (lis::quat-rotate q (lis::vec3-sub (lis::v3 target) (ship-com state)))
                              (lis::vec3-scale d (float distance 1d0))))
      (setf (lis::camera-field-of-view camera) (float lis::*outside-field-of-view* 1d0)))))

(defun posed-camera (camera-fn state)
  (let ((camera (lis::make-camera)))
    (funcall camera-fn camera state)
    camera))

(defun ship-point (state camera p)
  "The body point P relative to the eye, ICRF: q (p − c) − offset."
  (lis::vec3-sub (lis::quat-rotate (lis::render-state-q state) (lis::vec3-sub (lis::v3 p) (ship-com state)))
                 (lis::camera-offset camera)))

(defun project (camera point width height)
  "Pixel x, y of POINT (relative to the eye) through camera-clip-rows!, and its w."
  (let ((rows (make-array 16 :element-type 'single-float)))
    (lis::camera-clip-rows! rows camera (/ width height))
    (flet ((row (i) (loop for k below 3 sum (* (aref rows (+ i k)) (aref point k)))))
      (let ((w (row 12)))
        (values (* 0.5 width (+ 1 (/ (row 0) w))) (* 0.5 height (+ 1 (/ (row 4) w))) w)))))

(defun pixel-ray (camera px py width height)
  "The unit view ray through pixel PX, PY (ICRF)."
  (let* ((q (lis::camera-orientation camera))
         (f (tan (* 0.5d0 (lis::camera-field-of-view camera))))
         (x (* f (/ width height) (- (/ (* 2 (+ px 0.5d0)) width) 1)))
         (y (* f (- (/ (* 2 (+ py 0.5d0)) height) 1))))
    (lis::vec3-normalize (lis::quat-rotate q (lis::vec3 1 x y)))))

(defun disc-in-frame-p (camera centre radius width height)
  "Whether the sphere of RADIUS at CENTRE (relative to the eye) reaches into
the frame: its angular radius past the view axis against the half-diagonal."
  (let* ((d (lis::vec3-norm centre))
         (rho (if (<= d radius) pi (asin (/ radius d))))
         (forward (lis::quat-rotate (lis::camera-orientation camera) (lis::vec3 1 0 0)))
         (f (tan (* 0.5d0 (lis::camera-field-of-view camera))))
         (half-diagonal (atan (* f (sqrt (+ 1 (expt (/ width height) 2)))))))
    (< (- (angle-between forward centre) rho) half-diagonal)))

(defparameter *cab-view* '(0.3d0 0.4d0 -0.85d0)
  "Body direction of the lit-ship checks' camera: above the cab, to the side.")

(defun turn-ship! (sim body world)
  "Turn SIM's ship so its BODY direction points along WORLD."
  (let* ((q (lis::ship-state-q (lis::sim-ship sim)))
         (current (lis::quat-rotate q (lis::vec3-normalize (lis::v3 body))))
         (target (lis::vec3-normalize world))
         (turn (lis::quat-from-axis-angle (lis::vec3-cross current target)
                                          (angle-between current target))))
    (replace q (lis::quat-normalize (lis::quat-mul turn q)))
    sim))

(defun lit-ship-sim ()
  "u = 60 deg of the start orbit, the Sun 83 deg from the zenith, the ship
turned so *cab-view* points 40 deg from the Sun towards the nadir, turned
-30 deg about the Sun to keep the Moon out: a camera there has the Sun
behind it and looks up, away from Earth. (Under the
prograde hold the Sun lights the cab's wall only near the zenith, where a
camera with the Sun behind sees Earth.)"
  (let* ((sim (start-sim-at 60))
         (state (sim-render-state sim))
         (r (lis::render-state-r state))
         (s (lis::vec3-normalize (lis::vec3-sub (lis::render-state-sun state) r)))
         (nadir (lis::vec3-normalize (lis::vec3-scale r -1d0)))
         (p (lis::quat-rotate (lis::quat-from-axis-angle s (deg -30))
                              (lis::vec3-normalize (lis::vec3-add-scaled nadir s (- (lis::vec3-dot nadir s)))))))
    (turn-ship! sim *cab-view* (lis::vec3-add (lis::vec3-scale s (cos (deg 40)))
                                              (lis::vec3-scale p (sin (deg 40)))))))

(defun decal-view (side)
  "A camera-fn 8 m out from the decal on SIDE (+1 or −1 for ±y), level
with it and facing it; and the decal's corners in body axes."
  (let* ((a (if (plusp side) (- lis::*decal-angle*) (+ pi lis::*decal-angle*)))
         (r (+ lis::*cab-radius* lis::*cab-skin*))
         (half (/ lis::*decal-height* r 2))
         (z0 (second lis::*cab-axis*))
         (centre (list 13.5d0 (* r (cos a)) (+ z0 (* r (sin a)))))
         (normal (list 0 (cos a) (sin a))))
    (values (ship-view normal 8 centre)
            (loop for x in lis::*decal-span*
                  append (loop for b in (list (- a half) (+ a half))
                               collect (list x (* r (cos b)) (+ z0 (* r (sin b)))))))))

(defun decal-sim (side)
  "The start state turned so the decal on SIDE faces 70 deg from the Sun,
leaning away from Earth: its paint lit, its letters below the tone curve's
toe."
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim))
         (r (lis::ship-state-r ship))
         (state (sim-render-state sim))
         (s (lis::vec3-normalize (lis::vec3-sub (lis::render-state-sun state) r)))
         (up (lis::vec3-normalize r))
         (p (lis::vec3-normalize (lis::vec3-add-scaled up s (- (lis::vec3-dot up s)))))
         (target (lis::vec3-add (lis::vec3-scale s (cos (deg 70))) (lis::vec3-scale p (sin (deg 70)))))
         (normal (list 0 (* side (cos lis::*decal-angle*)) (- (sin lis::*decal-angle*)))))
    (turn-ship! sim normal target)))

(defun box-pixels (pixels width height x0 y0 x1 y1)
  "The luminances in the pixel box X0..X1, Y0..Y1 (inclusive, clipped)."
  (loop for y from (max 0 (floor y0)) to (min (1- height) (floor y1))
        append (loop for x from (max 0 (floor x0)) to (min (1- width) (floor x1))
                     collect (pixel-luminance pixels width x y))))

;; The registration's numbers: 53 and 183 of 255 on +y, 40 and 201 on −y
;; (lavapipe); the shadow's mean 0.4.
(defun map-checks (game png width height)
  "The map on the start state (decision 83: Map camera: framing on opening,
focus, zoom, drag and exposure): Earth's disc in the centre, the corners
at the background, neither black nor white-out."
  (let* ((pixels (nth-value 3 (lis::capture-game-state game png :sim (lis::new-game-sim)
                                                                :view :map :frames 1)))
         (background (/ (reduce #'+ (pixel-mode pixels width 0 height)) 3))
         (centre (pixel-luminance pixels width (floor width 2) (floor height 2)))
         (mean (rows-luminance pixels width 0 height)))
    (is eq :map (lis::game-view game))
    (is = lis::*exposure-map* (lis::game-exposure game))
    (true (> centre (+ background 20)) "map centre ~,1F, background ~,1F" centre background)
    (loop for (x y) in (list (list 0 0) (list (1- width) 0) (list 0 (1- height))
                             (list (1- width) (1- height)))
          do (let ((corner (pixel-luminance pixels width x y)))
               (true (<= (abs (- corner background)) 10) "corner ~D, ~D: ~,1F, background ~,1F"
                     x y corner background)))
    (true (< 5 mean 200) "map mean luminance ~,1F" mean))
  (lis::capture-game-state game png :frames 1)
  (is eq :outside (lis::game-view game)))

;; The map's ribbon (decision 84: Map ribbons and markers: osculating
;; orbit, adaptive segments, overlay markers and visibility).

(defun map-camera-fn (elevation)
  "A camera-fn: the map's opening frame at ELEVATION degrees above the orbit plane."
  (lambda (camera state)
    (let ((view (lis::new-map-view)))
      (lis::map-view-open! view state)
      (setf (lis::map-view-elevation view) (deg elevation))
      (lis::map-camera-update! camera view state nil))))

(defun ribbon-pixel-p (rgb)
  "Cyan: green and blue each 60 or more above red."
  (destructuring-bind (red green blue) rgb
    (and (>= (- green red) 60) (>= (- blue red) 60))))

(defun orbit-pixels (state camera width height)
  "Pixels (x y) of the osculating orbit's points in front of the eye, as two
lists: those seen and those behind Earth from the eye (double, CPU)."
  (let* ((orbit (lis::state->orbit (lis::render-state-r state) (lis::render-state-v state)
                                   lis::+mu-earth+ (lis::render-state-t state)))
         (eye (lis::camera-offset camera))
         (seen '()) (hidden '()))
    (dotimes (k 2048 (values seen hidden))
      (let* ((p (conic-point orbit (* 2 pi (/ k 2048))))
             (d (lis::vec3-sub p eye))
             (s (max 0d0 (min 1d0 (/ (- (lis::vec3-dot eye d)) (lis::vec3-dot d d)))))
             (behind (< (lis::vec3-norm (lis::vec3-add-scaled eye d s)) lis::+radius-earth+)))
        (multiple-value-bind (x y w) (project camera d width height)
          (when (and (plusp w) (<= 0 x (1- width)) (<= 0 y (1- height)))
            (if behind (push (list x y) hidden) (push (list x y) seen))))))))

(defun near-test (points radius width height)
  "A test of pixel centres (x + 0.5, y + 0.5): within RADIUS of any of POINTS,
from a mask marked once."
  (let ((mask (make-array (list height width) :element-type 'bit :initial-element 0)))
    (loop for (px py) in points
          do (loop for y from (max 0 (floor (- py radius 1))) to (min (1- height) (ceiling (+ py radius)))
                   do (loop for x from (max 0 (floor (- px radius 1))) to (min (1- width) (ceiling (+ px radius)))
                            when (<= (+ (expt (- (+ x 0.5) px) 2) (expt (- (+ y 0.5) py) 2)) (* radius radius))
                              do (setf (sbit mask y x) 1))))
    (lambda (cx cy) (= 1 (sbit mask (floor cy) (floor cx))))))

(defun ribbon-checks (game png width height)
  "The start orbit's ring on the map's opening frame; Earth hiding its far
side by depth. Edge-on (el 0) both halves fall on one line, so the depth
check looks from el 10 deg, where the far half lies apart."
  (flet ((capture (camera-fn)
           (multiple-value-bind (path w h pixels format)
               (lis::capture-game-state game png :sim (lis::new-game-sim) :view :map :frames 1
                                                 :camera-fn camera-fn)
             (declare (ignore path w h))
             (values pixels format)))
         (count-pixels (test)
           (loop for y below height
                 sum (loop for x below width count (funcall test x y (+ x 0.5) (+ y 0.5))))))
    (flet ((cyan (pixels format test)
             (count-pixels (lambda (x y cx cy)
                             (and (ribbon-pixel-p (pixel-rgb pixels format width x y))
                                  (funcall test cx cy))))))
      (let* ((camera-fn (map-camera-fn 30))
             (state (sim-render-state (lis::new-game-sim)))
             (camera (posed-camera camera-fn state))
             (seen (orbit-pixels state camera width height)))
        (let ((on-ring (near-test seen 2 width height)))
          ;; Cyan pixels within 2 px of the ring: the first measured frame,
          ;; 631 (lavapipe), x 0.5; none with the ribbon's radiance at 0.
          (multiple-value-bind (pixels format) (capture camera-fn)
            (let ((n (cyan pixels format on-ring)))
              (true (>= n 315) "ribbon: ~D cyan pixels on the ring" n)))
          (multiple-value-bind (pixels format)
              (with-canvas-values (game (lis::*ribbon-radiance* 0d0)
                                        (lis::*marker-colour* #x8A8A80))
                (capture camera-fn))
            (is = 0 (cyan pixels format on-ring)))))
      (let* ((camera-fn (map-camera-fn 10))
             (state (sim-render-state (lis::new-game-sim)))
             (camera (posed-camera camera-fn state)))
        (multiple-value-bind (seen hidden) (orbit-pixels state camera width height)
          (let* ((near-hidden (near-test hidden 1 width height))
                 (near-seen (near-test seen 3 width height))
                 (far-side (lambda (x y) (and (funcall near-hidden x y) (not (funcall near-seen x y))))))
            (multiple-value-bind (pixels format) (capture camera-fn)
              (true (> (count-pixels (lambda (x y cx cy) (declare (ignore x y)) (funcall far-side cx cy))) 100))
              (true (> (cyan pixels format (near-test seen 2 width height)) 0))
              (is = 0 (cyan pixels format far-side)))))))
    (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))))

;; The map's markers (decision 84): exact colours after the tone map.

(defun rgb-bytes (rgb)
  (list (ldb (byte 8 16) rgb) (ldb (byte 8 8) rgb) (ldb (byte 8 0) rgb)))

(defun marker-checks (game png width height)
  "The 400 x 2,000 km orbit, the ship at true anomaly 135 deg: the AP
diamond's centre pixel cyan and a dot of its label's A paper white, both
exact (the GPU's rounding aside)."
  (let* ((orbit (test-orbit-400x2000))
         (time (time-at-true-anomaly orbit (deg 135d0)))
         (sim (orbit-sim orbit time))
         (state (sim-render-state sim))
         (markers (lis::map-markers state (nth-value 1 (opened-map state)) (list width height)
                                    (lis::make-map-markers)))
         (ap (lis::map-marker-ref markers :apoapsis))
         (pixels (nth-value 3 (lis::capture-game-state game png :sim sim :view :map :frames 1)))
         (format (lis::renderer-format (lis::game-renderer game))))
    (true ap)
    (let ((x (lis::map-marker-x ap)) (y (lis::map-marker-y ap)))
      (flet ((check (px py rgb what)
               (let ((pixel (pixel-rgb pixels format width px py)))
                 (true (every (lambda (a b) (<= (abs (- a b)) 1)) (rgb-bytes rgb) pixel)
                       "~A at ~D, ~D: ~A" what px py pixel))))
        (check (floor x) (floor y) #x40D0FF "AP diamond")
        ;; A's top row .###.: its second dot spans x + 10 .. 12, y − 7 .. −5
        (check (floor (+ x 10 1)) (floor (- y 6)) #xE8E4D8 "AP label")
        ;; its first, unlit dot: the backing (decision 104)
        (check (floor (+ x 8 1)) (floor (- y 6)) #x0A0C0C "AP label backing"))))
  (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))
  (lis::game-load-state! game (lis::new-game-sim)))

(defun ship-checks (game png width height)
  (flet ((capture (sim camera-fn)
           (nth-value 3 (lis::capture-game-state game png :sim sim :frames 1 :camera-fn camera-fn))))
    ;; Lit against black (lit-ship-sim), the stars off.
    (let* ((cab '(12.75d0 0 -3.5d0))
           (camera-fn (ship-view *cab-view* 40 cab))
           (day (lit-ship-sim))
           (state (sim-render-state day))
           (camera (posed-camera camera-fn state))
           (eye (lis::vec3-add (lis::render-state-r state) (lis::camera-offset camera)))
           (pixels (with-canvas-values (game (lis::*star-radiance-limit* 0.0))
                     (capture day camera-fn)))
           (com (ship-point state camera (ship-com state)))
           (radius (lis::ship-bounding-radius (lis::mule) (lis::render-state-m-p state)))
           (rho (asin (/ radius (lis::vec3-norm com))))
           (margin (* 8 (/ (* 2 (tan (* 0.5d0 (lis::camera-field-of-view camera)))) height)))
           (outside 0) (brightest-outside 0))
      (true (< (angle-between (lis::camera-offset camera)
                              (lis::vec3-sub (lis::render-state-sun state) (lis::render-state-r state)))
               (deg 60)))
      (false (disc-in-frame-p camera (lis::vec3-scale eye -1d0) (lis::halo-radius) width height))
      (false (disc-in-frame-p camera (lis::vec3-sub (lis::render-state-moon state) eye)
                              lis::+radius-moon+ width height))
      (multiple-value-bind (x y) (project camera (ship-point state camera cab) width height)
        (destructuring-bind (red green blue) (pixel-colour pixels width (floor x) (floor y))
          (true (and (> red 150) (> green 150) (> blue 150) (< (abs (- red blue)) 30))
                "cab centre at ~,1F, ~,1F: ~D ~D ~D" x y red green blue)))
      ;; More than 8 px outside the bounding sphere, below the label: black.
      (loop for y from 40 below height
            do (dotimes (x width)
                 (when (> (angle-between (pixel-ray camera x y width height) com) (+ rho margin))
                   (incf outside)
                   (setf brightest-outside (max brightest-outside (pixel-luminance pixels width x y))))))
      (true (plusp outside))
      (true (< brightest-outside 20) "~,1F outside the bounding sphere" brightest-outside)
      ;; Pixels above 40: the first measured frame, 1,505 (lavapipe), x 0.5 and x 2.
      (let ((n (count-above pixels width height 40)))
        (true (<= 752 n 3010) "lit ship: ~D pixels above 40" n))
      ;; In Earth's shadow at u = 180 deg, the camera at the same body
      ;; direction: the box around the bounding sphere stays dark.
      (let* ((night (start-sim-at 180))
             (state (sim-render-state night))
             (camera (posed-camera camera-fn state))
             (com (ship-point state camera (ship-com state)))
             (d (lis::vec3-norm com))
             (pixels (capture night camera-fn)))
        (multiple-value-bind (x y w) (project camera com width height)
          (let* ((r (/ (* 0.5 height (/ radius (sqrt (- (* d d) (* radius radius)))))
                       (tan (* 0.5d0 (lis::camera-field-of-view camera)))))
                 (values (box-pixels pixels width height (- x r) (max 40 (- y r)) (+ x r) (+ y r)))
                 (mean (/ (reduce #'+ values) (length values))))
            (true (plusp w))
            (true (< mean 40) "the ship in Earth's shadow: mean ~,1F" mean)))))
    ;; The registration on both sides: black letters on white paint.
    (dolist (side '(1 -1))
      (multiple-value-bind (camera-fn corners) (decal-view side)
        (let* ((sim (decal-sim side))
               (state (sim-render-state sim))
               (camera (posed-camera camera-fn state))
               (pixels (capture sim camera-fn))
               (xs '()) (ys '()))
          (dolist (corner corners)
            (multiple-value-bind (x y) (project camera (ship-point state camera corner) width height)
              (push x xs) (push y ys)))
          (let ((values (box-pixels pixels width height (+ (reduce #'min xs) 1) (+ (reduce #'min ys) 1)
                                    (- (reduce #'max xs) 1) (- (reduce #'max ys) 1))))
            (true (< (reduce #'min values) 60) "decal ~@D: darkest ~,1F" side (reduce #'min values))
            (true (> (reduce #'max values) 150) "decal ~@D: brightest ~,1F" side (reduce #'max values))))))
    (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))
    (lis::game-load-state! game (lis::new-game-sim))))

;; The lights and the engine glow (decision 100: The lights' clock and the
;; glows' numbers), in Earth's shadow at u = 180 deg.
(defun light-checks (game png width height)
  (flet ((capture (sim camera-fn)
           "(fn (x y)): the red, green and blue bytes there."
           (multiple-value-bind (path w h pixels format)
               (lis::capture-game-state game png :sim sim :frames 1 :camera-fn camera-fn)
             (declare (ignore path w h))
             (lambda (xy) (pixel-rgb pixels format width (first xy) (second xy)))))
         (at (camera state p)
           (multiple-value-bind (x y) (project camera (ship-point state camera p) width height)
             (list (floor x) (floor y))))
         (luminance (rgb) (/ (reduce #'+ rgb) 3)))
    (let* ((night (start-sim-at 180))
           (state (sim-render-state night))
           (lights (lis::model-lights (lis::mule)))
           (camera-fn (ship-view *cab-view* 40 '(12.75d0 0 -3.5d0)))
           (camera (posed-camera camera-fn state)))
      ;; Red and green, the strobe off.
      (let ((pixels (with-canvas-values (game (lis::*strobe-on* 0d0)) (capture night camera-fn))))
        (destructuring-bind (red green blue) (funcall pixels (at camera state (getf lights :red)))
          (declare (ignore blue))
          (true (and (> red 150) (> red (+ green 50))) "red light: ~D ~D" red green))
        (destructuring-bind (red green blue) (funcall pixels (at camera state (getf lights :green)))
          (declare (ignore blue))
          (true (and (> green 150) (> green (+ red 50))) "green light: ~D ~D" red green)))
      ;; The strobe lit against unlit.
      (let* ((xy (at camera state (getf lights :strobe)))
             (on (with-canvas-values (game (lis::*strobe-on* lis::*strobe-period*))
                   (capture night camera-fn)))
             (off (with-canvas-values (game (lis::*strobe-on* 0d0)) (capture night camera-fn)))
             (lit (luminance (funcall on xy)))
             (dark (luminance (funcall off xy))))
        (true (> (- lit dark) 50) "strobe: ~,1F lit, ~,1F unlit" lit dark)))
    ;; The engine from 30 m behind on -x: full throttle against off.
    (let* ((night (start-sim-at 180))
           (state (sim-render-state night))
           (exit (lis::engine-exit-position (lis::mule) (lis::render-state-m-p state)))
           (camera-fn (ship-view '(-1 0 0) 30 exit))
           (camera (posed-camera camera-fn state))
           (xy (at camera state exit))
           (off (capture night camera-fn))
           (on (progn (lis::controls-set-throttle (lis::sim-controls night) 1d0)
                      (capture night camera-fn))))
      (destructuring-bind (red green blue) (funcall on xy)
        (declare (ignore green))
        (true (> blue red) "engine glow: red ~D, blue ~D" red blue))
      (let ((lit (luminance (funcall on xy)))
            (dark (luminance (funcall off xy))))
        (true (> (- lit dark) 20) "engine glow: ~,1F firing, ~,1F off" lit dark)))
    (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))
    (lis::game-load-state! game (lis::new-game-sim))))

(defun readout-checks (game png width height)
  "The start state's readout at WIDTH x HEIGHT (decision 85): inside its box
only the box's, the labels' and the values' exact colours, each present;
without it the scene shows there."
  (let ((lines (lis::call-on-canvas-thread
                game (lambda () (lis::readout-lines (lis::game-render-state game) (lis::game-clock game)
                                                    (lis::game-real-time game)))))
        (allowed (mapcar #'rgb-bytes '(#x0A0C0C #x8A8A80 #xE8E4D8)))
        (x0 lis::*readout-margin-px*) (y0 lis::*readout-margin-px*))
    ;; frozen, not paused (task 151)
    (true (member "WARP ×1" (mapcar #'lis::line-text lines) :test #'string=))
    (multiple-value-bind (box-width box-height) (lis::readout-box-size lines)
      (true (and (< (+ x0 box-width) width) (< (+ y0 box-height) height)))
      (flet ((colours (readout)
               (multiple-value-bind (path w h pixels format)
                   (lis::capture-game-state game png :sim (lis::new-game-sim) :frames 1 :readout readout)
                 (declare (ignore path w h))
                 (let ((counts (make-hash-table :test #'equal)))
                   (loop for y from y0 below (+ y0 box-height)
                         do (loop for x from x0 below (+ x0 box-width)
                                  do (incf (gethash (pixel-rgb pixels format width x y) counts 0))))
                   counts))))
        (let ((counts (colours t)))
          (dolist (colour allowed)
            (true (plusp (gethash colour counts 0)) "no pixel of ~A in the readout" colour))
          (loop for colour being the hash-keys of counts using (hash-value n)
                do (true (member colour allowed :test #'equal)
                         "~D pixels of ~A in the readout" n colour)))
        (let ((counts (colours nil)))
          (true (loop for colour being the hash-keys of counts
                      thereis (not (equal colour (first allowed))))
                "the scene under the readout's box"))))
    (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))))

(defun lost-sim ()
  "The deorbit case of hazards' tests, lost at the impact."
  (let ((sim (fly-until-lost (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3)))
                             1000 (+ (lis::start-t) 20000))))
    (true (lis::sim-lost-p sim))
    sim))

(defun lost-checks (game png width height)
  "A lost sim at WIDTH x HEIGHT (decision 86): the LOST panel's box in its
exact colours only, each present; the ship redder at the glow's peak than
after it."
  (let* ((sim (lost-sim))
         (lines (lis::lost-panel-lines (lis::sim-loss sim)))
         (allowed (mapcar #'rgb-bytes '(#x0A0C0C #xFF3030 #xE8E4D8 #x8A8A80))))
    (multiple-value-bind (x0 y0) (lis::lost-panel-origin lines (list width height))
      (multiple-value-bind (box-width box-height) (lis::readout-box-size lines)
        (true (and (< (+ x0 box-width) width) (< (+ y0 box-height) height)))
        (multiple-value-bind (path w h pixels format)
            (lis::capture-game-state game png :sim sim :frames 2)
          (declare (ignore path w h))
          (let ((counts (make-hash-table :test #'equal)))
            (loop for y from y0 below (+ y0 box-height)
                  do (loop for x from x0 below (+ x0 box-width)
                           do (incf (gethash (pixel-rgb pixels format width x y) counts 0))))
            (dolist (colour allowed)
              (true (plusp (gethash colour counts 0)) "no pixel of ~A in the panel" colour))
            (loop for colour being the hash-keys of counts using (hash-value n)
                  do (true (member colour allowed :test #'equal)
                           "~D pixels of ~A in the panel" n colour))))))
    ;; The glow: mean R − B over the ship's box, above the panel, at 0 s and 3.5 s.
    (let* ((cab '(12.75d0 0 -3.5d0))
           (camera-fn (ship-view *cab-view* 40 cab))
           (state (sim-render-state sim))
           (camera (posed-camera camera-fn state))
           (com (ship-point state camera (ship-com state)))
           (radius (lis::ship-bounding-radius (lis::mule) (lis::render-state-m-p state)))
           (d (lis::vec3-norm com))
           (r (/ (* 0.5 height (/ radius (sqrt (- (* d d) (* radius radius)))))
                 (tan (* 0.5d0 (lis::camera-field-of-view camera))))))
      (multiple-value-bind (cx cy) (project camera com width height)
        (flet ((redness (elapsed)
                 (multiple-value-bind (path w h pixels format)
                     (lis::capture-game-state game png :sim sim :frames 1 :camera-fn camera-fn
                                                       :readout nil :death-elapsed elapsed)
                   (declare (ignore path w h))
                   (loop with sum = 0 with n = 0
                         for y from (max 0 (floor (- cy r)))
                           below (min (floor (* lis::*lost-panel-top* height)) (ceiling (+ cy r)))
                         do (loop for x from (max 0 (floor (- cx r))) below (min width (ceiling (+ cx r)))
                                  do (destructuring-bind (red green blue)
                                         (pixel-rgb pixels format width x y)
                                       (declare (ignore green))
                                       (incf sum (- red blue))
                                       (incf n)))
                         finally (return (/ sum (max n 1)))))))
          ;; Margin: about half the first measured frame's 31.9 (lavapipe).
          (let ((peak (redness 0)) (after (redness 3.5d0)))
            (true (> (- peak after) 15) "glow: mean R - B ~,2F at 0 s, ~,2F at 3.5 s"
                  peak after)))))
    (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))
    (lis::game-load-state! game (lis::new-game-sim))))

(define-test render-frames
  (let ((width 320) (height 200))
    (luv:call-with-sdl-main-thread
     (lambda ()
       ;; :readout nil: at 320 x 200 the readout's box would cover nearly
       ;; the whole frame and every region check below; readout-checks
       ;; turns it on at 1280 x 800.
       (lis::with-capture-game (game :width width :height height :readout nil)
         (uiop:with-temporary-file (:pathname png :type "png")
           ;; The start state from the default outside view.
           (multiple-value-bind (path w h pixels)
               (lis::capture-game-state game png :frames 2)
             (is = width w)
             (is = height h)
             (multiple-value-bind (png-width png-height signature-p) (png-size path)
               (true signature-p)
               (is = width png-width)
               (is = height png-height))
             ;; Earth below the horizon, sunlit at the start; space above. The
             ;; Blue Marble's Pacific is dark, so: Earth's pixels differ from
             ;; the background (the upper third's commonest colour) and are
             ;; brighter than it.
             (let* ((rows (floor height 3))
                    (background (pixel-mode pixels width 0 rows))
                    (upper (rows-unlike pixels width 0 rows background))
                    (lower (rows-unlike pixels width (- height rows) height background))
                    (lit (rows-luminance pixels width (- height rows) height))
                    (dark (/ (reduce #'+ background) 3)))
               (true (> lower 0.95) "lower third ~,3F unlike the background" lower)
               (true (< upper 0.5) "upper third ~,3F unlike the background" upper)
               (true (> lit (+ dark 2)) "lower third ~,1F, background ~,1F" lit dark))
             (let ((mean (rows-luminance pixels width 0 height)))
               (true (< mean 200) "mean luminance ~,1F: white-out" mean))
             (true (find-if #'plusp pixels) "fully black frame"))
           ;; Nadir: the sub-ship point fills the centre, open Pacific.
           (multiple-value-bind (path w h pixels format)
               (lis::capture-game-state game png :frames 1 :camera-fn #'nadir-camera)
             (declare (ignore path w h))
             (let ((sim (lis::new-game-sim)))
               (multiple-value-bind (latitude longitude)
                   (lis::latitude-longitude :earth (lis::ship-state-r (lis::sim-ship sim))
                                            (lis::sim-t sim))
                 (true (< (abs latitude) 0.01) "sub-ship latitude ~F" latitude)
                 (true (< (/ (* 160 pi) 180) longitude pi) "sub-ship longitude ~F" longitude)))
             (multiple-value-bind (red blue) (pixel-red-blue pixels format width
                                                             (floor width 2) (floor height 2))
               (true (> blue red) "the nadir centre: red ~D, blue ~D" red blue)))
           ;; Bloom: a 2x2 px quad of 4e4 in a dark view lifts the pixels 20 px
           ;; away, and not at strength 0.
           (let* ((state (lis::game-render-state game))
                  (camera (lis::make-camera)))
             (away-camera camera state)
             (is = lis::*exposure-deep* (lis::exposure-target camera state :outside)))
           (let* ((cx (floor width 2)) (cy (floor height 2))
                  (around (list (list (+ cx 20) cy) (list (- cx 20) cy)
                                (list cx (+ cy 20)) (list cx (- cy 20))))
                  (baseline (capture-with-quads game png nil))
                  (bloomed (capture-with-quads game png (centre-quad 4e4)))
                  (unbloomed (capture-with-quads game png (centre-quad 4e4) :strength 0.0)))
             (flet ((at (pixels xy) (pixel-luminance pixels width (first xy) (second xy))))
               (dolist (xy around)
                 (true (> (- (at bloomed xy) (at baseline xy)) 10)
                       "bloom at ~A: ~,1F over ~,1F" xy (at bloomed xy) (at baseline xy))
                 (true (<= (abs (- (at unbloomed xy) (at baseline xy))) 1)
                       "strength 0 at ~A: ~,1F against ~,1F" xy (at unbloomed xy) (at baseline xy)))
               (true (> (at unbloomed (list cx cy)) 250))))
           ;; Out of range: an overflow to +inf in half floats is white,
           ;; negative and NaN radiance are clamped to 0, not white.
           (let* ((cx (floor width 2)) (cy (floor height 2))
                  (baseline (capture-with-quads game png nil :strength 0.0))
                  (infinite (capture-with-quads game png (centre-quad 1e6 :size 4) :strength 0.0))
                  (nan (sb-kernel:make-single-float #x7fc00000))
                  (invalid (capture-with-quads game png (append (centre-quad -1e3 :dx -10 :size 4)
                                                                (centre-quad nan :dx 10 :size 4))
                                               :strength 0.0)))
             (true (> (pixel-luminance infinite width cx cy) 250))
             (dolist (x (list (- cx 10) (+ cx 10)))
               (true (<= (abs (- (pixel-luminance invalid width x cy)
                                 (pixel-luminance baseline width x cy)))
                         1)
                     "invalid radiance at ~D: ~,1F" x (pixel-luminance invalid width x cy))))
           (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))
           ;; The shader's tone map against tone-map-srgb: grey quads of known
           ;; radiance at the deep exposure, the stars off.
           (let ((saved lis::*star-radiance-limit*))
             (setf lis::*star-radiance-limit* 0.0)
             (unwind-protect
                  (let ((pixels (capture-with-quads
                                 game png (loop for radiance in '(0.002 0.01 0.03 0.1)
                                                for dx from -60 by 40
                                                append (centre-quad radiance :dx dx :size 8))
                                 :strength 0.0)))
                    (loop for radiance in '(0.002 0.01 0.03 0.1)
                          for x from (- (floor width 2) 60) by 40
                          for expected = (lis::tone-map-srgb radiance lis::*exposure-deep*)
                          do (dolist (byte (pixel-colour pixels width x (floor height 2)))
                               (true (<= (abs (- byte expected)) 2)
                                     "radiance ~F: ~D, tone-map-srgb ~D" radiance byte expected))))
               (setf lis::*star-radiance-limit* saved)))
           ;; Sirius at the centre of a 20 deg view; Procyon in a 60 deg one
           ;; between them, up and to the left (east) of Sirius, where the
           ;; projection puts it.
           (let ((night (start-sim-at 180))
                 (sirius (sky-camera 101.287 -16.716 20))
                 (wide (sky-camera 108 -6 60)))
             (let ((pixels (nth-value 3 (lis::capture-game-state game png :sim night :frames 1
                                                                          :camera-fn sirius))))
               (multiple-value-bind (x y) (brightest-pixel pixels width height)
                 (true (<= (sqrt (+ (expt (- x (/ width 2)) 2) (expt (- y (/ height 2)) 2))) 1)
                       "Sirius's brightest pixel at ~F, ~F" x y))
               ;; Live: the radiance limit takes on the next frame.
               (let ((saved lis::*star-radiance-limit*))
                 (setf lis::*star-radiance-limit* 0.0)
                 (unwind-protect
                      (let ((dark (nth-value 3 (lis::capture-game-state game png :frames 1
                                                                                 :camera-fn sirius))))
                        (true (< (nth-value 2 (brightest-pixel dark width height)) 5)
                              "stars at radiance limit 0"))
                   (setf lis::*star-radiance-limit* saved))))
             (let ((pixels (nth-value 3 (lis::capture-game-state game png :frames 1 :camera-fn wide))))
               (multiple-value-bind (px py) (sky-pixel wide 114.825 5.225 width height)
                 (multiple-value-bind (sx sy) (sky-pixel wide 101.287 -16.716 width height)
                   (true (and (< px sx) (< 40 py sy)) "Procyon at ~F, ~F, Sirius at ~F, ~F"
                         px py sx sy))
                 (multiple-value-bind (x y) (brightest-pixel pixels width height
                                                             :x0 (floor (- px 8)) :x1 (ceiling (+ px 8))
                                                             :from-row (floor (- py 8))
                                                             :y1 (ceiling (+ py 8)))
                   (true (<= (sqrt (+ (expt (- x px) 2) (expt (- y py) 2))) 2)
                         "Procyon's brightest pixel at ~F, ~F, predicted ~F, ~F" x y px py))))
             ;; Orion at 50 deg: pixels above 30 of 255. Bounds: the first
             ;; measured frame, 985 px (lavapipe), x 0.5 and x 2.
             (let* ((pixels (nth-value 3 (lis::capture-game-state game png :frames 1
                                                                           :camera-fn (sky-camera 84 -1 50))))
                    (n (count-above pixels width height 30)))
               (true (<= 490 n 1970) "Orion: ~D pixels above 30" n))
             (lis::game-load-state! game (lis::new-game-sim)))
           (sun-and-moon-checks game png width height)
           (earth-checks game png width height)
           (ship-checks game png width height)
           (light-checks game png width height)
           (map-checks game png width height)
           (ribbon-checks game png width height)
           (marker-checks game png width height)
           (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))
           ;; Resizes replace the swapchain's textures; what the renderer kept
           ;; for the old ones goes, so its per-target state stays bounded.
           (let* ((renderer (lis::game-renderer game))
                  (frames (lis::frame-renderer-frames renderer))
                  (images (luv:canvas-frame-resource-count frames)))
             (true (plusp images))
             (is = images (hash-table-count (lis::renderer-color-views renderer)))
             (dolist (size (list (list 240 160) (list 400 250) (list width height)))
               (luv:resize-canvas (lis::game-canvas game) (first size) (second size))
               (lis::wait-for-frames game 6)
               (is equal size (luv:canvas-extent (lis::game-context game))))
             (true (<= (luv:canvas-frame-resource-count frames) images)
                   "~D frame slots after resizes, ~D images"
                   (luv:canvas-frame-resource-count frames) images)
             (true (<= (hash-table-count (lis::renderer-color-views renderer)) images)))
           (luv:resize-canvas (lis::game-canvas game) 1280 800)
           (lis::wait-for-frames game 6)
           (readout-checks game png 1280 800)
           (lost-checks game png 1280 800)))))))
