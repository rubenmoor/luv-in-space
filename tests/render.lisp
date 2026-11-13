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

;;; Objects and the grapple in the render state (task 221, decision 124)

(defun object-render-of (state key)
  (svref (lis::render-state-objects state)
         (position key lis::*objects* :key #'lis::object-def-key)))

(defun update-at (state clock sim fraction)
  "STATE updated with the accumulator at FRACTION of the last step's h."
  (setf (lis::clock-accumulator clock) (* fraction (- (lis::sim-t sim) (lis::clock-previous-t clock))))
  (lis::render-state-update! state clock sim))

(define-test render-objects-rails-exact
  (multiple-value-bind (sim clock h) (stepped-sim :sim (lis::new-game-sim))
    (let ((state (lis::make-render-state)))
      (dolist (fraction '(0d0 0.5d0 1d0))
        (update-at state clock sim fraction)
        (let ((tr (lis::render-state-t state)))
          (is = (length lis::*objects*) (length (lis::render-state-objects state)))
          (loop for object across (lis::sim-objects sim)
                for o across (lis::render-state-objects state)
                do (let ((r (lis::make-vec3)) (v (lis::make-vec3)) (q (lis::make-quat))
                         (w (lis::make-vec3)))
                     (true (lis::object-state-into! r v q w object tr))
                     (true (lis::object-render-drawn o))
                     (true (and (equalp r (lis::object-render-r o)) (equalp v (lis::object-render-v o))
                                (equalp q (lis::object-render-q o)) (equalp w (lis::object-render-w o))))))))
      (true (> h 0)))))

(define-test render-objects-promoted-hermite
  (let* ((sim (skua-yard-sim))
         (t0 (lis::sim-t sim))
         (reference (lis::copy-sim sim))
         (clock (lis::new-clock sim))
         (state (lis::make-render-state))
         (h (lis::sim-step! sim 1000 :until (+ t0 2d0)))
         (index (position :skua-yard-3 lis::*objects* :key #'lis::object-def-key)))
    (is = 2d0 h)
    (true (lis::object-promoted-p (svref (lis::sim-objects sim) index)))
    (let ((o (svref (lis::render-state-objects state) index))
          (before (lis::object-state (svref (lis::sim-objects reference) index)))
          (after (lis::object-state (svref (lis::sim-objects sim) index))))
      (update-at state clock sim 0d0)
      (true (and (equalp (lis::object-render-r o) (lis::ship-state-r before))
                 (equalp (lis::object-render-q o) (lis::ship-state-q before))))
      (update-at state clock sim 1d0)
      (true (and (equalp (lis::object-render-r o) (lis::ship-state-r after))
                 (equalp (lis::object-render-q o) (lis::ship-state-q after))))
      (update-at state clock sim 0.5d0)
      (step-sim reference 64 1)
      (let ((fine (lis::object-state (svref (lis::sim-objects reference) index))))
        (is = (+ t0 1d0) (lis::sim-t reference))
        (true (< (lis::vec3-norm (lis::vec3-sub (lis::object-render-r o) (lis::ship-state-r fine)))
                 1d-3))))))

(define-test render-objects-promoted-last-step-continuous
  ;; Tern Station on rails before the step, promoted in it: drawn near its rails
  ;; (the step's Hermite starts on them).
  (let* ((sim (ship-near :tern-station 100d0))
         (fresh (lis::new-game-sim))
         (clock (lis::new-clock fresh))
         (state (lis::make-render-state))
         (tern (lis::sim-object fresh :tern-station)))
    (true (lis::object-promoted-p (lis::sim-object sim :tern-station)))
    (lis::sim-step! sim 1)
    (update-at state clock sim 0.5d0)
    (let ((o (object-render-of state :tern-station))
          (r (lis::make-vec3)))
      (lis::object-state-into! r (lis::make-vec3) (lis::make-quat) (lis::make-vec3)
                               tern
                               (lis::render-state-t state))
      ;; The rails' position runs ~4 m/s off their own velocity (the plane's
      ;; precession), so the integrated state starts ~0.17 m off in 1/64 s.
      (true (< (lis::vec3-norm (lis::vec3-sub r (lis::object-render-r o))) 0.3d0)))))

(defun relative-pose (state key)
  "The object KEY's position and attitude in the ship's axes at STATE."
  (let* ((o (object-render-of state key))
         (conjugate (lis::quat-conjugate (lis::render-state-q state))))
    (values (lis::quat-rotate conjugate (lis::vec3-sub (lis::object-render-r o)
                                                       (lis::render-state-r state)))
            (lis::quat-mul conjugate (lis::object-render-q o)))))

(defun check-attached-pose (sim key)
  (let ((clock (lis::new-clock sim))
        (state (lis::make-render-state)))
    (lis::sim-step! sim 1)
    (update-at state clock sim 1d0)
    (multiple-value-bind (position attitude) (relative-pose state key)
      (dolist (fraction '(0.3d0 0.75d0))
        (update-at state clock sim fraction)
        (multiple-value-bind (p q) (relative-pose state key)
          (true (< (lis::vec3-norm (lis::vec3-sub p position)) 1d-9)
                "~E m" (lis::vec3-norm (lis::vec3-sub p position)))
          (true (< (reduce #'max (map 'list (lambda (a b) (abs (- a b))) q attitude)) 1d-12)))))))

(define-test render-objects-latched-and-berthed
  (let ((sim (skua-yard-sim)))
    (lis::grapple-capture! sim (position :skua-yard-3 lis::*objects* :key #'lis::object-def-key) 0)
    (is eq :latched (lis::object-status (lis::sim-object sim :skua-yard-3)))
    (check-attached-pose sim :skua-yard-3))
  (let ((sim (ship-near :tern-station 100d0)))
    (lis::berth-capture! sim (position :tern-station lis::*objects* :key #'lis::object-def-key) 0)
    (true (lis::sim-berthed sim))
    (check-attached-pose sim :tern-station)))

(define-test render-objects-removed-not-drawn
  (multiple-value-bind (sim clock) (stepped-sim :sim (lis::new-game-sim))
    (setf (lis::object-status (lis::sim-object sim :k-17)) :removed)
    (let ((state (update-at (lis::make-render-state) clock sim 0.5d0)))
      (false (lis::object-render-drawn (object-render-of state :k-17)))
      (true (lis::object-render-drawn (object-render-of state :tern-station))))))

(define-test render-grapple-claws
  (let* ((sim (lis::new-game-sim))
         (grapple (lis::sim-grapple sim))
         (clock (lis::new-clock sim))
         (state (lis::make-render-state))
         (shown (lis::render-state-grapple state)))
    (flet ((update () (lis::render-state-update! state clock sim)))
      (update)
      (is = 0 (lis::render-grapple-angle shown))
      (false (lis::render-grapple-floodlight shown))
      (setf (lis::grapple-state grapple) :armed)
      (update)
      (is = 30 (lis::render-grapple-angle shown))
      (true (lis::render-grapple-floodlight shown))
      (true (lis::render-grapple-range-finder shown))
      (setf (lis::grapple-state grapple) :closing
            (lis::grapple-closing-end grapple) (+ (lis::sim-t sim) 1d0))
      (update)
      (is = 11 (lis::render-grapple-angle shown))
      (setf (lis::grapple-state grapple) :latched)
      (update)
      (is = -8 (lis::render-grapple-angle shown))
      (false (lis::render-grapple-range-finder shown))
      ;; Easing at 90 deg/s between stowed and armed, in the game only.
      (setf (lis::render-state-claws-eased state) t
            (lis::grapple-state grapple) :stowed)
      (update)
      (lis::render-state-ease-claws! state 0.1d0)
      (is = 0 (lis::render-grapple-angle shown))
      (setf (lis::grapple-state grapple) :armed)
      (update)
      (is = 0 (lis::render-grapple-angle shown))
      (lis::render-state-ease-claws! state 0.1d0)
      (true (< (abs (- 9 (lis::render-grapple-angle shown))) 1d-12))
      (lis::render-state-ease-claws! state 1d0)
      (is = 30 (lis::render-grapple-angle shown))
      (lis::render-state-ease-claws! state nil)
      (is = 30 (lis::render-grapple-angle shown)))))

(define-test render-state-objects-cons-nothing
  (flet ((check (sim)
           (let ((clock (lis::new-clock sim)) (state (lis::make-render-state)))
             (lis::sim-step! sim 1)
             (setf (lis::clock-accumulator clock) 0.005d0)
             (lis::render-state-update! state clock sim)
             (is = 0 (least-bytes-consed
                      (lambda () (dotimes (i 1000) (lis::render-state-update! state clock sim))))))))
    (check (lis::new-game-sim))
    (check (skua-yard-sim))
    (let ((sim (skua-yard-sim)))
      (lis::grapple-capture! sim (position :skua-yard-3 lis::*objects* :key #'lis::object-def-key) 0)
      (check sim))))

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
  ;; Without a target, a target on rails, a promoted one.
  (dolist (setup (list (lambda () (drift-sim))
                       (lambda () (let ((sim (drift-sim)))
                                    (lis::sim-set-target! sim :tern-station)
                                    sim))
                       (lambda () (let ((sim (ship-near :tern-station 1000d0)))
                                    (lis::sim-set-target! sim :tern-station)
                                    sim))))
    (multiple-value-bind (sim clock h) (stepped-sim :sim (funcall setup))
      (setf (lis::clock-accumulator clock) (* 0.5d0 h))
      (let* ((state (lis::make-render-state))
             (consed (least-bytes-consed
                      (lambda () (dotimes (i 1000) (lis::render-state-update! state clock sim))))))
        (is = 0 consed)))))

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
    (loop for (x y) in (list (list (1- width) 0) (list 0 (1- height))
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
          ;; the line is the predicted trajectory (decision 125), run to completion
          (let* ((render-state (lis::game-render-state game))
                 (pred (lis::render-state-prediction render-state)))
            (true (and pred (lis::prediction-complete-p pred)
                       (lis::trajectory-drawn-p pred (lis::render-state-t render-state)))
                  "ribbon: the trajectory is drawn"))
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

;; The map's line styles and the target's orbit (decision 125: Map in
;; milestone 3).

(defun test-line-hook (from-px to-px &key (dashed t) (width 2f0) (row-y nil))
  "A frame-renderer-test-lines function adding one cyan segment from pixel
FROM-PX to TO-PX (x y) 1,000 m from the eye; dashed from phase 0 by its length
in px, or solid."
  (declare (ignore row-y))
  (lambda (lines state camera eye extent)
    (declare (ignore state eye))
    (destructuring-bind (w h) extent
      (let* ((out (lis::map-lines-instances lines))
             (o (* (lis::map-lines-count lines) lis::+ribbon-instance-floats+))
             (length (sqrt (+ (expt (- (first to-px) (first from-px)) 2)
                              (expt (- (second to-px) (second from-px)) 2)))))
        (loop for (px py) in (list from-px to-px)
              for k from 0 by 4
              for ray = (lis::vec3-scale (pixel-ray camera (- px 0.5d0) (- py 0.5d0) w h) 1000d0)
              do (dotimes (i 3) (setf (aref out (+ o k i)) (coerce (aref ray i) 'single-float)))
                 (setf (aref out (+ o k 3)) 1f0))
        (setf (aref out (+ o 8)) (coerce (lis::srgb-channel-linear #x40D0FF 16) 'single-float)
              (aref out (+ o 9)) (coerce (lis::srgb-channel-linear #x40D0FF 8) 'single-float)
              (aref out (+ o 10)) (coerce (lis::srgb-channel-linear #x40D0FF 0) 'single-float)
              (aref out (+ o 11)) 1f0
              (aref out (+ o 12)) (if dashed 0f0 -1f0)
              (aref out (+ o 13)) (if dashed (coerce length 'single-float) -1f0)
              (aref out (+ o 14)) width (aref out (+ o 15)) 1f0)
        (incf (lis::map-lines-count lines))))))

(defun capture-with-lines (game png hook camera-fn &key (sim (lis::new-game-sim)))
  "The map with the test-line HOOK; the candidates' boxes and labels off, which
sit along the top where the lines are measured."
  (let ((renderer (lis::game-renderer game))
        (limit lis::*marker-candidate-body-px*))
    (setf lis::*marker-candidate-body-px* 0d0)
    (lis::call-on-canvas-thread game (lambda () (setf (lis::frame-renderer-test-lines renderer) hook)))
    (unwind-protect
         (multiple-value-bind (path w h pixels format)
             (lis::capture-game-state game png :sim sim :view :map :frames 1 :camera-fn camera-fn)
           (declare (ignore path w h))
           (values pixels format))
      (setf lis::*marker-candidate-body-px* limit)
      (lis::call-on-canvas-thread game (lambda () (setf (lis::frame-renderer-test-lines renderer) nil))))))

(defun run-lengths (flags)
  "The lengths of the runs of equal values in FLAGS, as (value . length), in order."
  (let ((runs '()))
    (loop for flag in flags
          do (if (and runs (eq (car (first runs)) flag))
                 (incf (cdr (first runs)))
                 (push (cons flag 1) runs)))
    (nreverse runs)))

(defun conic-pixels-from (orbit eye camera width height)
  "Pixels (x y) of ORBIT's points in the frame and not behind Earth from EYE
(relative to Earth's centre), in double."
  (let ((seen '()))
    (dotimes (k 2048 seen)
      (let* ((p (conic-point orbit (* 2 pi (/ k 2048))))
             (d (lis::vec3-sub p eye))
             (s (max 0d0 (min 1d0 (/ (- (lis::vec3-dot eye d)) (lis::vec3-dot d d)))))
             (behind (< (lis::vec3-norm (lis::vec3-add-scaled eye d s)) lis::+radius-earth+)))
        (multiple-value-bind (x y w) (project camera d width height)
          (when (and (plusp w) (not behind) (<= 0 x (1- width)) (<= 0 y (1- height)))
            (push (list x y) seen)))))))

(defun target-orbit-checks (game png width height)
  "Tern Station targeted: magenta pixels on its orbit where it lies apart from
the ship's (seen from the ship, 150 km out, its orbit 20 km higher)."
  (let* ((sim (let ((sim (lis::new-game-sim))) (lis::sim-set-target! sim :tern-station) sim))
         (state (sim-render-state sim))
         (camera-fn (lambda (camera state)
                      (let ((view (lis::new-map-view)))
                        (lis::map-view-open! view state)
                        (lis::map-view-cycle-focus! view)
                        (setf (lis::map-view-elevation view) (deg 30)
                              (lis::map-view-distance view) 150d3
                              (lis::map-view-target view) 150d3)
                        (lis::map-camera-update! camera view state nil))))
         (camera (posed-camera camera-fn state))
         (eye (lis::camera-eye camera (lis::camera-anchor-position! (lis::make-vec3) camera state)))
         (mu lis::+mu-earth+) (time (lis::render-state-t state))
         (ship-orbit (lis::state->orbit (lis::render-state-r state) (lis::render-state-v state) mu time))
         (target-orbit (lis::state->orbit (lis::render-state-target-r state)
                                          (lis::render-state-target-v state) mu time))
         (near-ship (near-test (conic-pixels-from ship-orbit eye camera width height) 6 width height))
         (apart (remove-if (lambda (pt) (funcall near-ship (first pt) (second pt)))
                           (conic-pixels-from target-orbit eye camera width height)))
         (near-apart (near-test apart 2 width height)))
    (true (> (length apart) 10) "target orbit apart from the ship's: ~D points" (length apart))
    (multiple-value-bind (pixels format) (capture-with-lines game png nil camera-fn :sim sim)
      (let ((n (loop for y below height
                     sum (loop for x below width
                               count (and (funcall near-apart (+ x 0.5) (+ y 0.5))
                                          (destructuring-bind (r g b) (pixel-rgb pixels format width x y)
                                            (and (>= (- r g) 60) (>= (- b g) 60))))))))
        (true (>= n 30) "target orbit: ~D magenta pixels" n)))))

(defun line-style-checks (game png width height)
  "A dashed test line alternates 8 px lit and 6 px dark; 1 px and 2 px widths
measured across the line."
  (let* ((camera-fn (map-camera-fn 30))
         (y 12.5)
         (x0 6)
         (hook (test-line-hook (list x0 y) (list (- width 6) y))))
    (flet ((lit (pixels format x row)
             (destructuring-bind (r g b) (pixel-rgb pixels format width x row)
               (and (>= (- g r) 60) (>= (- b r) 60)))))
      (multiple-value-bind (pixels format) (capture-with-lines game png hook camera-fn)
       (let* ((runs (run-lengths (loop for x from (+ x0 2) below (- width 8) collect (lit pixels format x (floor y)))))
             (inner (butlast (rest runs))))
        (true (>= (length inner) 4) "dash runs: ~S" runs)
        (dolist (run inner)
          (if (car run)
              (true (<= 7 (cdr run) 9) "lit run ~D" (cdr run))
              (true (<= 5 (cdr run) 7) "dark run ~D" (cdr run))))))
      (flet ((across (w)
               (multiple-value-bind (pixels format)
                   (capture-with-lines game png (test-line-hook (list x0 y) (list (- width 6) y)
                                                                :dashed nil :width w)
                                       camera-fn)
                 (loop with x = (floor width 2)
                       with column = (loop for row from 3 to 22
                                           collect (destructuring-bind (r g b)
                                                       (pixel-rgb pixels format width x row)
                                                     (declare (ignore b r))
                                                     (lis::srgb-channel-linear (ash g 8) 8)))

                       for value in column
                       sum (/ value (float (max 1 (reduce #'max column))))))))
        (let ((one (across 1f0)) (two (across 2f0)))
          (true (< 0.8 one 1.3) "1 px line: ~,2F" one)
          (true (< 1.7 two 2.3) "2 px line: ~,2F" two))))))

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

;;; Objects (decision 124: Meshes of the places: models, scenery liveries,
;;; lights, far dots, the grapple's claws, floodlight and range finder; task 222).

(defun cpu-object-gpu (key)
  "An object-gpu without buffers: the CPU side of the draws."
  (let* ((def (lis::object-definition key))
         (model (lis::object-model key)))
    (lis::%make-object-gpu :key key :extent (lis::model-extent model)
                           :dot-radius (sqrt (/ (lis::shapes-quarter-area def) pi))
                           :beacons (lis::model-beacons model))))

(define-test object-uniforms-hand-computed
  ;; An object turned 90 degrees about z on the +x axis at 7,000 km, the Sun on
  ;; the same axis, the eye (3, 4, 0) m from it: every row against its hand value.
  (let* ((state (lis::make-render-state))
         (o (object-render-of state :k-17))
         (out (make-array lis::+ship-uniform-floats+ :element-type 'single-float :initial-element 0f0))
         (eye (lis::vec3 7d6 0d0 0d0))
         (sun (lis::vec3 1.5d11 0d0 0d0)))
    (replace (lis::object-render-r o) (lis::vec3 (- 7d6 3d0) (- 0d0 4d0) 0d0))
    (replace (lis::object-render-q o) (lis::quat (cos (/ pi 4)) 0d0 0d0 (sin (/ pi 4))))
    (setf (lis::object-render-sun-fraction o) 0.5d0
          (lis::object-render-drawn o) t)
    (replace (lis::render-state-sun state) sun)
    (setf (aref (lis::object-render-r o) 0) 7d6
          (aref (lis::object-render-r o) 1) 0d0)
    (replace eye (lis::vec3 (+ 7d6 3d0) 4d0 0d0))
    (lis::object-uniforms! out state o eye)
    (flet ((close-p (index expected)
             (true (< (abs (- (aref out index) expected)) (+ 1d-6 (* 1d-6 (abs expected))))
                   "float ~D: ~F against ~F" index (aref out index) expected)))
      ;; R = [0 -1 0; 1 0 0; 0 0 1].
      (loop for expected in '(0 -1 0 0  1 0 0 0  0 0 1 0) for i from 0 do (close-p i expected))
      ;; The CoM relative to the eye, then the glows' least sigma.
      (close-p 12 -3) (close-p 13 -4) (close-p 14 0) (close-p 15 (* 0.5 lis::*glow-min-pixels*))
      ;; No gimbal, identity trim.
      (loop for expected in '(0 0 0 0  1 0 0 0  0 1 0 0  0 0 1 0) for i from 16 do (close-p i expected))
      (let* ((distance (- 1.5d11 7d6))
             (irradiance (expt (/ lis::+au+ distance) 2))
             (cap (acos (/ lis::+radius-earth+ 7d6)))
             (earthshine (* lis::*earth-albedo* (expt (/ lis::+radius-earth+ 7d6) 2)
                            (/ (sin cap) cap) irradiance)))
        ;; The Sun along +x is body (0 -1 0); Earth along -x is body (0 1 0).
        (loop for expected in (list 0 -1 0 (* 0.5 irradiance)) for i from 32 do (close-p i expected))
        (loop for expected in (list 0 1 0 earthshine) for i from 36 do (close-p i expected)))
      (destructuring-bind (tr tg tb) lis::*earthshine-tint*
        (loop for expected in (list tr tg tb lis::*ship-ambient*) for i from 40 do (close-p i expected)))
      ;; The eye (3, 4, 0) from the object is body (4 -3 0); lights on; no emission.
      (loop for expected in '(4 -3 0 1  0 0 0 0) for i from 44 do (close-p i expected)))))

(define-test object-glows-and-kinds
  (let* ((sim (lis::new-game-sim))
         (state (sim-render-state sim))
         (gpu (cpu-object-gpu :tern-station))
         (o (object-render-of state :tern-station))
         (out (make-array (* 16 lis::+glow-floats+) :element-type 'single-float :initial-element 0f0))
         (eye (lis::vec3-add-scaled (lis::object-render-r o) (lis::vec3 1 0 0) 100d0))
         (camera (lis::make-camera)))
    (setf (lis::camera-orientation camera) (lis::quat 0 0 0 1))
    ;; Beacons: red, green, a strobe, four lamps. Strobe lit at the clock's 0, dark at 0.5 s.
    (setf (lis::render-state-lights-t state) 0d0)
    (is = 7 (lis::object-glows! out gpu state o eye nil 0))
    (setf (lis::render-state-lights-t state) 0.5d0)
    (is = 6 (lis::object-glows! out gpu state o eye nil 0))
    ;; The glows are at q p + r - eye, the red one first.
    (let ((p (second (first (lis::model-beacons (lis::object-model :tern-station))))))
      (let ((expected (lis::vec3-sub (lis::vec3-add (lis::quat-rotate (lis::object-render-q o) p)
                                                    (lis::object-render-r o))
                                     eye)))
        (dotimes (i 3)
          (true (< (abs (- (aref out i) (aref expected i))) 1d-3)))
        (true (> (aref out 4) 0))))
    ;; The strobe's offset per index: 0.5 s plus 0.37 x 4 = 1.98 s mod 1.2 s: lit at 0.78? dark.
    ;; A dot adds one glow, with the Sun's term and the sun-fraction; none in shadow.
    (let ((n (lis::object-glows! out gpu state o eye t 0)))
      (is = 7 n)
      (true (> (aref out (+ (* 6 lis::+glow-floats+) 4)) 0)))
    (setf (lis::object-render-sun-fraction o) 0d0)
    (is = 6 (lis::object-glows! out gpu state o eye t 0))
    (setf (lis::object-render-sun-fraction o) 1d0)
    ;; Mesh below the near plane's reach, mesh from 2 px, dot beyond it.
    (flet ((kind (distance)
             (replace eye (lis::vec3-add-scaled (lis::object-render-r o) (lis::vec3 1 0 0) distance))
             ;; Looking along -x at the object: its direction from the eye.
             (setf (lis::camera-orientation camera) (lis::quat 0 0 0 1))
             (lis::object-draw-kind gpu o camera '(320 200) eye)))
      ;; Orientation (0 0 0 1) turns x to -x about z: forward -x.
      (is eq :mesh (kind 150d0))
      (is eq :dot (kind 30000d0))
      (is eq :mesh (kind 1d0)))
    ;; Behind the eye: nothing.
    (replace eye (lis::vec3-add-scaled (lis::object-render-r o) (lis::vec3 1 0 0) -300d0))
    (is eq nil (lis::object-draw-kind gpu o camera '(320 200) eye))))

(define-test object-draws-cons-nothing
  ;; The CPU side of a frame's encode with the nine objects of Skua Yard:
  ;; uniforms, glows and draw kinds consing nothing.
  (let* ((sim (skua-yard-sim))
         (state (sim-render-state sim))
         (gpus (loop for def in (coerce lis::*objects* 'list) collect (cpu-object-gpu (lis::object-def-key def))))
         (out (make-array lis::+ship-uniform-floats+ :element-type 'single-float :initial-element 0f0))
         (glows (make-array (* 16 lis::+glow-floats+) :element-type 'single-float :initial-element 0f0))
         (camera (lis::make-camera))
         (eye (lis::vec3-add (lis::render-state-r state) (lis::vec3 0d0 123d0 77d0))))
    (flet ((frame ()
             (loop for i from 0 for gpu in gpus
                   for o = (svref (lis::render-state-objects state) i)
                   when (lis::object-render-drawn o)
                   do (lis::object-uniforms! out state o eye)
                      (lis::object-glows! glows gpu state o eye (evenp i) i)
                      (lis::object-draw-kind gpu o camera '(320 200) eye))))
      (frame)
      (true (> (count-if #'lis::object-render-drawn (lis::render-state-objects state)) 8))
      (is = 0 (least-bytes-consed (lambda () (dotimes (i 3000) (frame))))))))

(defun sim-at-offset (seconds)
  "The start state's sim at SECONDS after its start (its objects on rails; the
ship stays where it was: the camera-fns below fly the eye by an offset)."
  (let ((sim (lis::new-game-sim)))
    (incf (lis::sim-t sim) seconds)
    sim))

(defun object-sun-direction (state key)
  "The unit vector from the object KEY to the Sun at STATE."
  (lis::vec3-normalize (lis::vec3-sub (lis::render-state-sun state)
                                      (lis::object-render-r (object-render-of state key)))))

(defun object-eye-camera (key distance direction-fn &key (basis :world))
  "A camera-fn: the eye DISTANCE m from object KEY's CoM along the unit
DIRECTION-FN of (state object-render), looking at the CoM, any roll
(BASIS :world); BASIS :body: DIRECTION-FN returns the direction in the
object's body axes and the camera's right is its body y."
  (lambda (camera state)
    (let* ((o (object-render-of state key))
           (u (funcall direction-fn state o))
           (u (if (eq basis :body) (lis::quat-rotate (lis::object-render-q o) u) u))
           (eye (lis::vec3-add-scaled (lis::object-render-r o) u (float distance 1d0)))
           (x (lis::vec3-scale u -1d0))
           (hint (if (eq basis :body)
                     (lis::quat-rotate (lis::object-render-q o) (lis::vec3 0 1 0))
                     (if (< (abs (aref x 2)) 0.9d0) (lis::vec3 0 0 1) (lis::vec3 1 0 0))))
           (y (lis::vec3-normalize (if (eq basis :body)
                                       (lis::vec3-add-scaled hint x (- (lis::vec3-dot hint x)))
                                       (lis::vec3-cross hint x))))
           (z (lis::vec3-cross x y)))
      (lis::quat-from-basis! (lis::camera-orientation camera) x y z)
      (replace (lis::camera-offset camera) (lis::vec3-sub eye (lis::render-state-r state)))
      (setf (lis::camera-field-of-view camera) (float lis::*outside-field-of-view* 1d0)))))

(defun sunward-up-direction (state o)
  "The direction from object O to the Sun turned 45 degrees towards its
nadir: a view along its opposite looks up, the Sun within 60 degrees behind."
  (let* ((s (lis::vec3-normalize (lis::vec3-sub (lis::render-state-sun state) (lis::object-render-r o))))
         (nadir (lis::vec3-normalize (lis::vec3-scale (lis::object-render-r o) -1d0)))
         (p (lis::vec3-normalize (lis::vec3-add-scaled nadir s (- (lis::vec3-dot nadir s))))))
    (lis::vec3-add (lis::vec3-scale s (cos (deg 45))) (lis::vec3-scale p (sin (deg 45))))))

(defun sunlit-tern-offset ()
  "Seconds after the start when Tern Station is sunlit and the Sun is within 85
degrees of its nadir (a view away from the Sun, turned up, clears Earth)."
  (loop for dt from 0 by 30
        for sim = (sim-at-offset dt)
        for state = (sim-render-state sim)
        for o = (object-render-of state :tern-station)
        when (and (> (lis::object-render-sun-fraction o) 0.999d0)
                  (< (angle-between (object-sun-direction state :tern-station)
                                    (lis::vec3-scale (lis::object-render-r o) -1d0))
                     (deg 85)))
          return dt
        when (> dt 6000) do (error "No such time.")))

(defun shadowed-tern-offset ()
  (loop for dt from 0 by 30
        when (< (lis::object-render-sun-fraction
                 (object-render-of (sim-render-state (sim-at-offset dt)) :tern-station))
                1d-6)
          return (+ dt 1000)))

(defun object-bound-pixels (camera state key width height)
  "The projected centre and the angular bound's radius in pixels of object KEY."
  (let* ((o (object-render-of state key))
         (c (lis::vec3-sub (lis::object-render-r o) (lis::vec3-add (lis::render-state-r state)
                                                                   (lis::camera-offset camera))))
         (bound (lis::object-def-bound (lis::object-render-def o))))
    (multiple-value-bind (x y) (project camera c width height)
      (values x y (/ (* 0.5 height (tan (asin (/ bound (lis::vec3-norm c)))))
                     (tan (* 0.5d0 (lis::camera-field-of-view camera))))))))

(defun object-lamp-pixel (camera state key index width height)
  "The pixel (x y) of the INDEXth beacon of object KEY's model."
  (let* ((o (object-render-of state key))
         (beacon (nth index (lis::model-beacons (lis::object-model key))))
         (p (lis::vec3-add (lis::object-render-r o)
                           (lis::quat-rotate (lis::object-render-q o) (second beacon)))))
    (multiple-value-bind (x y)
        (project camera (lis::vec3-sub p (lis::vec3-add (lis::render-state-r state)
                                                        (lis::camera-offset camera)))
                 width height)
      (list (floor x) (floor y)))))

(defun object-checks (game png width height)
  (let ((format nil))
   (flet ((capture (sim camera-fn)
           (multiple-value-bind (path w h pixels pixel-format)
               (lis::capture-game-state game png :sim sim :frames 1 :camera-fn camera-fn)
             (declare (ignore path w h))
             (setf format pixel-format)
             pixels)))
    (with-canvas-values (game (lis::*star-radiance-limit* 0.0))
      ;; Tern Station from 150 m in sunlight, the Sun behind the camera.
      (let* ((sim (sim-at-offset (sunlit-tern-offset)))
             (state (sim-render-state sim))
             (camera-fn (object-eye-camera :tern-station 150
                                           #'sunward-up-direction))
             (camera (let ((c (lis::make-camera))) (funcall camera-fn c state) c))
             (pixels (capture sim camera-fn))
             (inside 0) (outside 0) (brightest-outside 0))
        (multiple-value-bind (cx cy radius) (object-bound-pixels camera state :tern-station width height)
          (dotimes (y height)
            (dotimes (x width)
              (let ((d (sqrt (+ (expt (- (+ x 0.5) cx) 2) (expt (- (+ y 0.5) cy) 2))))
                    (l (pixel-luminance pixels width x y)))
                (cond ((<= d radius) (when (> l 40) (incf inside)))
                      ((> d (+ radius 8))
                       (incf outside)
                       (setf brightest-outside (max brightest-outside l)))))))
          (true (plusp outside))
          (true (< brightest-outside 20) "Tern at 150 m: ~,1F outside the bound (radius ~,1F)"
                brightest-outside radius)
          ;; The first measured frame: 998 pixels above 40 (lavapipe), x 0.5 and x 2.
          (true (<= 499 inside 1996) "Tern at 150 m: ~D pixels above 40" inside)))
      ;; In Earth's shadow from 100 m, looking down body z at the wings: the
      ;; -y tip red, the +y tip green, the free fixture's lamp white.
      (let* ((sim (sim-at-offset (shadowed-tern-offset)))
             (state (sim-render-state sim))
             (camera-fn (object-eye-camera :tern-station 100 (lambda (state o)
                                                               (declare (ignore state o))
                                                               (lis::vec3 0 0 -1))
                                           :basis :body))
             (camera (let ((c (lis::make-camera))) (funcall camera-fn c state) c))
             (pixels (with-canvas-values (game (lis::*strobe-on* 0d0)) (capture sim camera-fn)))
             (beacons (lis::model-beacons (lis::object-model :tern-station))))
        (flet ((colour (kind &optional (n 0))
                 (let* ((at (loop for b in beacons for i from 0
                                  when (eq (first b) kind) collect i))
                        (xy (object-lamp-pixel camera state :tern-station (nth n at) width height)))
                   (pixel-rgb pixels format width (first xy) (second xy)))))
          (destructuring-bind (red green blue) (colour :red)
            (declare (ignore blue))
            (true (and (> red 150) (> red (+ green 50))) "-y tip: ~D ~D" red green))
          (destructuring-bind (red green blue) (colour :green)
            (declare (ignore blue))
            (true (and (> green 150) (> green (+ red 50))) "+y tip: ~D ~D" red green))
          (destructuring-bind (red green blue) (colour :lamp)
            (true (and (> red 150) (> green 150) (> blue 150)) "lamp: ~D ~D ~D" red green blue))))
      ;; Far: a dot from 30 km, none from 2,000 km (where Earth is in the
      ;; view, so against the pixels 6 to 10 px round it).
      (let* ((sim (sim-at-offset (sunlit-tern-offset)))
             (state (sim-render-state sim)))
        (dolist (distance '(30000 2000000))
          (let* ((camera-fn (object-eye-camera :tern-station distance #'sunward-up-direction))
                 (camera (let ((c (lis::make-camera))) (funcall camera-fn c state) c))
                 (pixels (capture sim camera-fn)))
            (multiple-value-bind (cx cy) (object-bound-pixels camera state :tern-station width height)
              (let ((best 0) (ring 0) (n 0))
                (loop for y from 0 below height
                      do (loop for x from 0 below width
                               for d = (sqrt (+ (expt (- (+ x 0.5) cx) 2) (expt (- (+ y 0.5) cy) 2)))
                               for l = (pixel-luminance pixels width x y)
                               do (cond ((<= d 2) (setf best (max best l)))
                                        ((<= 6 d 10) (incf ring l) (incf n)))))
                (setf ring (/ ring n))
                (if (< distance 100000)
                    (true (> best (+ ring 30)) "Tern at ~D m: ~,1F, ring ~,1F" distance best ring)
                    (true (< best (+ ring 5)) "Tern at ~D m: ~,1F, ring ~,1F" distance best ring)))))))
      ;; A latched Skua Yard 3 at its design pose from 40 m.
      (let* ((sim (skua-yard-sim))
             (index (position :skua-yard-3 lis::*objects* :key #'lis::object-def-key)))
        (lis::grapple-capture! sim index 0)
        (let* ((state (sim-render-state sim))
               (camera-fn (lambda (camera state)
                            (let* ((q (lis::render-state-q state))
                                   (axis (lis::quat-rotate q (lis::vec3 1 0 0)))
                                   (s (lis::vec3-normalize (lis::vec3-sub (lis::render-state-sun state)
                                                                          (lis::render-state-r state))))
                                   (u (lis::vec3-normalize (lis::vec3-add-scaled s axis (- (lis::vec3-dot s axis)))))
                                   (rock (lis::object-render-r (svref (lis::render-state-objects state) index)))
                                   (eye (lis::vec3-add-scaled (lis::render-state-r state) u 40d0))
                                   (x (lis::vec3-normalize (lis::vec3-sub rock eye)))
                                   (up (lis::quat-rotate q (lis::vec3 0 0 -1)))
                                   (y (lis::vec3-normalize (lis::vec3-cross (lis::vec3-scale up -1d0) x)))
                                   (z (lis::vec3-cross x y)))
                              (lis::quat-from-basis! (lis::camera-orientation camera) x y z)
                              (replace (lis::camera-offset camera) (lis::vec3-sub eye (lis::render-state-r state)))
                              (setf (lis::camera-field-of-view camera)
                                    (float lis::*outside-field-of-view* 1d0)))))
               (camera (let ((c (lis::make-camera))) (funcall camera-fn c state) c))
               (pixels (capture sim camera-fn))
               (o (svref (lis::render-state-objects state) index))
               (c (lis::vec3-sub (lis::object-render-r o)
                                 (lis::vec3-add (lis::render-state-r state) (lis::camera-offset camera)))))
          (multiple-value-bind (x y) (project camera c width height)
            (let* ((half (* 0.6 (/ (* 0.5 height (lis::object-def-bound (lis::object-render-def o)))
                                   (* (lis::vec3-norm c) (tan (* 0.5d0 (lis::camera-field-of-view camera)))))))
                   (values (box-pixels pixels width height (- x half) (- y half) (+ x half) (+ y half)))
                   (mean (/ (reduce #'+ values) (length values)))
                   (centre (pixel-luminance pixels width (floor x) (floor y)))
                   (background (/ (reduce #'+ (pixel-mode pixels width 0 height)) 3)))
              (true (> centre (+ background 20)) "the rock's centre: ~,1F" centre)
              (true (> mean (+ background 20)) "the rock's box: ~,1F over ~,1F" mean background)))))
      (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))))
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

;;; Slug text (src/text.lisp; decision 126: Cockpit structure and view in
;;; milestone 3): a white emissive H on a plane facing the camera.

(defun h-stem-em ()
  "Monaspace Neon's H left stem width in em, from its outline's crossings of
a horizontal line a quarter of the cap height up (all its segments are lines)."
  (let* ((loader (luv.slug:slug-font-loader (lis::font-path :mono)))
         (outline (luv.slug:slug-glyph-outline (luv.slug:load-slug-glyph #\H loader)))
         (y (* 0.25d0 (luv.slug:slug-font-cap-height loader)))
         (xs '()))
    (dolist (contour (luv.slug::slug-outline-contours outline))
      (dolist (curve contour)
        (let* ((a (luv.slug::slug-quadratic-start curve)) (b (luv.slug::slug-quadratic-end curve))
               (ya (luv.slug::slug-point-y a)) (yb (luv.slug::slug-point-y b)))
          (when (< (min ya yb) y (max ya yb))
            (push (+ (luv.slug::slug-point-x a)
                     (* (- (luv.slug::slug-point-x b) (luv.slug::slug-point-x a))
                        (/ (- y ya) (- yb ya))))
                  xs)))))
    (setf xs (sort xs #'<))
    (/ (- (second xs) (first xs)) (zpb-ttf:units/em loader))))

(defun text-checks (game png width height)
  ;; The H's stem width and tone, then E and 0 for coverage inside curves
  ;; and bars (task 281: horizontal gaps in the strokes).
  (dolist (char '(#\H #\E #\0))
    (text-char-checks game png width height char)))

(defun text-char-checks (game png width height char)
  (let* ((renderer (lis::game-renderer game))
         (atlas (lis::frame-renderer-text-atlas renderer))
         (batch (lis::frame-renderer-text-batch renderer))
         (fov (deg 40)) (distance 2d0)
         (cap-px 40)
         (pixels-per-metre (/ height (* 2 distance (tan (/ fov 2)))))
         (cap (/ cap-px pixels-per-metre))
         (font (lis::text-font-of atlas :mono))
         (scale (/ cap (lis::text-font-cap-em font)))
         (glyph (lis::text-glyph-for atlas :mono char))
         (factor 1.0)
         (camera-fn
           (lambda (camera state)
             (away-camera camera state)
             (setf (lis::camera-field-of-view camera) fov)
             (let* ((q (lis::camera-orientation camera))
                    (forward (lis::quat-rotate q (lis::vec3 1 0 0)))
                    (right (lis::quat-rotate q (lis::vec3 0 1 0)))
                    (up (lis::vec3-scale (lis::quat-rotate q (lis::vec3 0 0 1)) -1d0))
                    (middle-x (* 0.5 (+ (lis::text-glyph-min-x glyph) (lis::text-glyph-max-x glyph))))
                    (middle-y (* 0.5 (+ (lis::text-glyph-min-y glyph) (lis::text-glyph-max-y glyph))))
                    (origin (lis::vec3-add (lis::vec3-scale forward distance)
                                           (lis::vec3-add (lis::vec3-scale right (- (* middle-x scale)))
                                                          (lis::vec3-scale up (- (* middle-y scale)))))))
               (setf (lis::text-batch-count batch) 0)
               (lis::grid-run! atlas (lis::text-batch-region batch 4) 0
                               (lis::make-text-plane :origin (coerce origin 'list)
                                                     :right (coerce right 'list) :up (coerce up 'list))
                               (string char) :cap (float cap 1f0) :factor factor))))
         (saved-count (lis::text-batch-count batch))
         (saved-data (copy-seq (lis::text-batch-data batch)))
         pixels exposure)
    (with-canvas-values (game (lis::*bloom-strength* 0.0) (lis::*star-radiance-limit* 0.0))
      (lis::call-on-canvas-thread game (lambda () (setf (lis::frame-renderer-test-text renderer) batch)))
      (unwind-protect
           (setf pixels (nth-value 3 (lis::capture-game-state game png :frames 1 :camera-fn camera-fn))
                 exposure (lis::game-exposure game))
        ;; The cockpit's static text (plate, labels, placeholders) comes back.
        (lis::call-on-canvas-thread game (lambda () (setf (lis::game-camera-fn game) nil
                                                          (lis::frame-renderer-test-text renderer) nil
                                                          (lis::text-batch-count batch) saved-count)
                                           (replace (lis::text-batch-data batch) saved-data)))))
    (let* ((box-width (* (- (lis::text-glyph-max-x glyph) (lis::text-glyph-min-x glyph)) scale pixels-per-metre))
           (x0 (- (/ width 2) (/ box-width 2))) (x1 (+ x0 box-width))
           (y0 (- (/ height 2) (/ cap-px 2))) (y1 (+ y0 cap-px))
           (lit 0)
           (stem (* (h-stem-em) scale pixels-per-metre))
           (row (floor (+ (/ height 2) (* 0.25 cap-px)))))
      (dotimes (y height)
        (dotimes (x width)
          (when (> (pixel-luminance pixels width x y) 128)
            (incf lit)
            (true (and (<= (- x0 2) x (+ x1 2)) (<= (- y0 2) y (+ y1 2)))
                  "lit pixel ~D, ~D outside the H's box ~F-~F, ~F-~F" x y x0 x1 y0 y1))))
      (true (> lit (* 2 stem cap-px 0.8)) "~A: ~D lit pixels" char lit)
      (let* ((mid (floor (case char
                           (#\H (+ (/ height 2) (* 0.25 cap-px)))
                           (#\E (+ y0 (* 0.3 cap-px)))
                           (t (/ height 2)))))
             (start (position-if (lambda (x) (> (pixel-luminance pixels width x mid) 128))
                                 (loop for i below width collect i)))
             (run (and start (loop for x from start below width
                                   while (> (pixel-luminance pixels width x mid) 128) count t))))
        (true (and run (> run 2)) "~A: a stroke at row ~D, ~A px" char mid run)
        (when (and run (char= char #\H))
          (true (<= (abs (- run stem)) 1) "stem lit ~A px, outline ~,2F px" run stem)
          (let ((expected (lis::tone-map-srgb (/ factor exposure) exposure))
                (centre (pixel-colour pixels width (floor (+ x0 (/ stem 2))) row)))
            (dolist (byte centre)
              (true (<= (abs (- byte expected)) 3) "stem centre ~A, tone map ~D" centre expected))))
        ;; The left stroke's centre column, solid over the middle of the cap.
        (when run
          (let ((column (+ start (floor run 2))) (gaps 0))
            (loop for y from (ceiling (+ y0 (* 0.4 cap-px))) to (floor (+ y0 (* 0.6 cap-px)))
                  unless (> (pixel-luminance pixels width column y) 128) do (incf gaps))
            (true (zerop gaps) "~A: ~D gaps in the left stroke's centre column" char gaps)))
        ;; E: the top bar's centre row, solid across its length.
        (when (char= char #\E)
          (let ((y (ceiling (+ y0 (* 0.06 cap-px)))) (gaps 0))
            (loop for x from (ceiling (+ start 4)) to (floor (- x1 4))
                  unless (> (pixel-luminance pixels width x y) 128) do (incf gaps))
            (true (zerop gaps) "E: ~D gaps along the top bar" gaps))))
      (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream))))))

;;; The cab's shell and light (decision 126: Cockpit structure and view in
;;; milestone 3; src/cockpit.lisp): a lining pixel against the CPU twin of the
;;; light, the window showing the scene.

(defun cab-lining-hit (direction)
  "(values point normal) of the ray from the eye along DIRECTION (a unit vec3 in
body axes) on the lining cylinder, the normal inward."
  (let* ((dy (aref direction 1)) (dz (aref direction 2))
         (r lis::*cab-lining-radius*) (a (second lis::*cab-axis-offset*))
         (qa (+ (* dy dy) (* dz dz)))
         (qb (* -2 a dz)) (qc (- (* a a) (* r r)))
         (tt (/ (+ (- qb) (sqrt (- (* qb qb) (* 4 qa qc)))) (* 2 qa)))
         (p (lis::vec3-scale direction tt)))
    (values p (lis::vec3-scale (lis::vec3 0d0 (aref p 1) (- (aref p 2) a)) (/ -1 r)))))

(defun cockpit-checks (game png width height)
  (with-canvas-values (game (lis::*bloom-strength* 0.0))
    ;; The head turned right and up: the centre pixel is lining, lit by the
    ;; strips; its byte is the tone map of exposure x the twin's radiance.
    (let* ((yaw (deg 90)) (pitch (deg -35))
           (format nil)
           (pixels (multiple-value-bind (path w h pixels pixel-format)
                       (lis::capture-game-state game png :view :cockpit :head (list 90 -35 70)
                                                         :frames 2)
                     (declare (ignore path w h))
                     (setf format pixel-format)
                     pixels))
           (exposure (lis::game-exposure game))
           (direction (lis::vec3 (* (cos pitch) (cos yaw)) (* (cos pitch) (sin yaw)) (sin pitch))))
      (is = 4.0 exposure)
      (multiple-value-bind (p n) (cab-lining-hit direction)
        (multiple-value-bind (r g b) (lis::cab-irradiance p n)
          (let ((got (pixel-rgb pixels format width (floor width 2) (floor height 2)))
                (albedo (aref lis::*cab-materials* 0 0)))
            (loop for byte in got for x in (list r g b)
                  do (let ((expected (lis::tone-map-srgb (* albedo x) exposure)))
                       (true (<= (abs (- byte expected)) 4)
                             "lining pixel ~A, tone map of 4.0 x the twin ~D" got expected)))))))
    ;; The head on the nose: the window's centre shows the scene (space), not lining.
    (let* ((pixels (nth-value 3 (lis::capture-game-state game png :view :cockpit
                                                                  :head '(0 0 70) :frames 2)))
           ;; Off the boresight cross, which is white tape at the centre.
           (centre (pixel-luminance pixels width (+ (floor width 2) 30) (- (floor height 2) 20))))
      (true (< centre 40) "the window beside the cross: luminance ~D" centre))
    ;; Lit text: an "H" (kind 1) on a plane facing the eye, its crossbar at
    ;; the image's centre, takes the cab's irradiance at its glyph's corner.
    (let* ((renderer (lis::game-renderer game))
           (atlas (lis::frame-renderer-text-atlas renderer))
           (batch (lis::frame-renderer-text-batch renderer))
           (plate-count (lis::text-batch-count batch))
           (pitch (deg 30)) (distance 0.7d0) (cap 0.2d0)
           (direction (lis::vec3 (cos pitch) 0d0 (sin pitch)))
           (up (lis::vec3 (sin pitch) 0d0 (- (cos pitch))))
           (right (lis::vec3 0d0 1d0 0d0))
           (font (lis::text-font-of atlas :mono))
           (scale (/ cap (lis::text-font-cap-em font)))
           (glyph (lis::text-glyph-for atlas :mono #\H))
           (middle-x (* 0.5 (+ (lis::text-glyph-min-x glyph) (lis::text-glyph-max-x glyph))))
           (middle-y (* 0.5 (+ (lis::text-glyph-min-y glyph) (lis::text-glyph-max-y glyph))))
           (origin (lis::vec3-sub (lis::vec3-scale direction distance)
                                  (lis::vec3-add (lis::vec3-scale right (* middle-x scale))
                                                 (lis::vec3-scale up (* middle-y scale)))))
           pixels format)
      (setf (lis::text-batch-count batch) plate-count)
      (lis::grid-run! atlas (lis::text-batch-region batch 4) 0
                      (lis::make-text-plane :origin (coerce origin 'list) :right (coerce right 'list)
                                            :up (coerce up 'list))
                      "H" :cap (float cap 1f0) :kind :lit)
      (unwind-protect
           (multiple-value-bind (path w h got pixel-format)
               (lis::capture-game-state game png :view :cockpit :head '(0 30 70) :frames 2)
             (declare (ignore path w h))
             (setf pixels got format pixel-format))
        (let ((record (record-floats batch plate-count)))
          (setf (lis::text-batch-count batch) plate-count)
          (let* ((corner (lis::vec3 (nth 0 record) (nth 1 record) (nth 2 record)))
                 (exposure (lis::game-exposure game))
                 (got (pixel-rgb pixels format width (floor width 2) (floor height 2))))
            (multiple-value-bind (r g b) (lis::cab-irradiance corner (lis::vec3-scale direction -1d0))
              (loop for byte in got for x in (list r g b)
                    do (let ((expected (lis::tone-map-srgb x exposure)))
                         (true (<= (abs (- byte expected)) 4)
                               "lit text pixel ~A, tone map of the twin ~D" got expected))))))))

    ;; The glass: the window's pixel with and without it. Off the cross, along a
    ;; ray through (0.9, 0.3, 0.15), the scene is space (black) and the glass
    ;; adds its premultiplied reflection of the cab light, the twin's.
    (let* ((point (lis::vec3 0.9d0 0.3d0 0.15d0))
           (distance (lis::vec3-norm point))
           (yaw (atan 0.3d0 0.9d0)) (pitch (asin (/ 0.15d0 distance)))
           (head (list (/ (* yaw 180) pi) (/ (* pitch 180) pi) 70))
           (without (let ((lis::*cab-glass* nil))
                      (nth-value 3 (lis::capture-game-state game png :view :cockpit :head head :frames 2))))
           (fmt nil)
           (with (multiple-value-bind (path w h pixels pixel-format)
                     (lis::capture-game-state game png :view :cockpit :head head :frames 2)
                   (declare (ignore path w h))
                   (setf fmt pixel-format)
                   pixels))
           (exposure (lis::game-exposure game))
           (dark (pixel-rgb without fmt width (floor width 2) (floor height 2)))
           (got (pixel-rgb with fmt width (floor width 2) (floor height 2))))
      (true (every (lambda (byte) (< byte 12)) dark) "window without the glass: ~A" dark)
      (multiple-value-bind (r g b) (lis::cab-glass-colour point (lis::vec3 -1d0 0d0 0d0)
                                                          (max (/ 0.3d0 0.6d0) (/ 0.15d0 0.3d0)))
        (loop for byte in got for x in (list r g b) for base in dark
              do (let ((expected (lis::tone-map-srgb x exposure)))
                   (true (<= (abs (- byte expected)) 3)
                         "glass pixel ~A, tone map of the twin ~D (without ~A)" got expected dark)))))
    ;; The porthole: head yaw +90, pitch 40, the starboard porthole's centre
    ;; pixel is Earth, brighter than the lining beside it, and its centre is
    ;; the image's within 1 px.
    (multiple-value-bind (centre axis) (lis::cab-porthole-geometry 1)
      (declare (ignore axis))
      (let* ((yaw (deg 90)) (pitch (deg 40))
             (look (lis::vec3 (* (cos pitch) (cos yaw)) (* (cos pitch) (sin yaw)) (sin pitch)))
             (angle (acos (min 1d0 (lis::vec3-dot look (lis::vec3-normalize centre)))))
             (pixels-per-radian (/ (/ height 2) (tan (deg 35)))))
        (true (< (* angle pixels-per-radian) 1) "the porthole's centre ~,2F px off the image's"
              (* angle pixels-per-radian)))
      (let ((pixels (nth-value 3 (lis::capture-game-state game png :view :cockpit
                                                                    :head '(90 40 70) :frames 2)))
            (scene (nth-value 3 (lis::capture-game-state game png :view :cockpit :interior nil
                                                                   :head '(90 40 70) :frames 2))))
        (let ((got (pixel-luminance pixels width (floor width 2) (floor height 2)))
              (earth (pixel-luminance scene width (floor width 2) (floor height 2))))
          (true (> earth 20) "Earth through the porthole: ~D" earth)
          (true (<= (abs (- got earth)) 6) "porthole centre ~D, the scene alone ~D" got earth))))
    ;; The plate: its letters at least 3:1 darker than the steel (linear).
    (let* ((pitch (/ (* (atan 0.43d0 0.89d0) 180) pi))
           (pixels (nth-value 3 (lis::capture-game-state game png :view :cockpit
                                                                  :head (list 0 (- pitch) 20) :frames 2)))
           (values (sort (loop for y from (- (floor height 2) 12) to (+ (floor height 2) 12)
                               append (loop for x from (- (floor width 2) 90) to (+ (floor width 2) 90)
                                            collect (let ((byte (/ (pixel-luminance pixels width x y) 255.0)))
                                                      (expt (/ (+ byte 0.055) 1.055) 2.4))))
                         #'<))
           (letters (nth (floor (length values) 50) values))
           (steel (nth (floor (length values) 2) values)))
      (true (>= steel (* 3 letters)) "plate ~,5F, letters ~,5F" steel letters))
    (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))))

;;; The console (plan 31 piece 5; decision 126): tape labels and the screens'
;;; placeholders at 1280 x 800 in Earth's shadow, the beige bezel against the
;;; black ones. Each point is brought to the image's centre by the head.

(defparameter *console-poses*
  '((0 16 70) (0 0 70) (0 42 60) (-32 45 70) (32 45 70) (-55 30 70) (50 55 70))
  "Head poses (yaw pitch fov, degrees) the console's checks look from, to cover every label.")

(defun pose-pixel (point width height &optional (poses *console-poses*))
  "(values x y per pose) of the body POINT in the image of the *console-poses* head
that puts it nearest the centre, or nil outside every frame; PER is pixels per
tangent unit."
  (let ((best nil) (best-d 1e9))
    (dolist (pose poses)
      (destructuring-bind (yaw pitch fov) pose
        (multiple-value-bind (x y depth) (head-projection point yaw pitch)
          (let* ((per (/ (/ height 2) (tan (deg (/ fov 2)))))
                 (px (+ (/ width 2) (* x per))) (py (+ (/ height 2) (* y per)))
                 (d (+ (abs (- px (/ width 2))) (abs (- py (/ height 2))))))
            (when (and (> depth 0) (< 20 px (- width 20)) (< 20 py (- height 20)) (< d best-d))
              (setf best-d d best (list px py per pose)))))))
    (values-list best)))

(defun byte-linear (byte)
  (let ((x (/ byte 255.0)))
    (if (<= x 0.04045) (/ x 12.92) (expt (/ (+ x 0.055) 1.055) 2.4))))

(defun region-values (pixels format width x0 x1 y0 y1)
  "The linear luminance (mean of the three channels) of every pixel in the box."
  (loop for y from y0 to y1
        append (loop for x from x0 to x1
                     collect (/ (reduce #'+ (mapcar #'byte-linear (pixel-rgb pixels format width x y))) 3))))

(defun contrast-of (values)
  "(brightest 20 % mean) / median of VALUES, the median floored at a tiny radiance."
  (let* ((sorted (sort (copy-list values) #'<)) (n (length sorted))
         (top (subseq sorted (floor (* 0.8 n))))
         (median (nth (floor n 2) sorted)))
    (/ (/ (reduce #'+ top) (length top)) (max median 1e-5))))

(defun console-checks (game png width height)
  "Tape labels, placeholders and bezels from a few head poses at WIDTH x HEIGHT in
Earth's shadow; each thing is read where the pose puts it."
  (with-canvas-values (game (lis::*bloom-strength* 0.0))
    (let ((sim (start-sim-at 180)) (shots '()))
      (flet ((shot (pose)
               (or (cdr (assoc pose shots :test #'equal))
                   (let ((got (multiple-value-list
                               (lis::capture-game-state game png :sim sim :view :cockpit :readout nil
                                                                 :frames 1 :head pose))))
                     (push (cons pose got) shots)
                     got))))
        (flet ((ink-contrast (centre cap length &optional (poses *console-poses*))
                 ;; The box of CAP x LENGTH metres about CENTRE (a body point on a plane
                 ;; facing the eye), in the first pose that holds it.
                 (multiple-value-bind (x y per pose) (pose-pixel centre width height poses)
                   (when x
                     (destructuring-bind (path w h pixels format) (shot pose)
                       (declare (ignore path w h))
                       (let* ((scale (/ per (lis::vec3-norm centre))) ; per is px per tangent unit
                              (half-x (floor (* 0.5 length scale))) (half-y (max 2 (floor (* 0.5 cap scale)))))
                         (values (contrast-of (region-values pixels format width
                                                             (- (round x) half-x) (+ (round x) half-x)
                                                             (- (round y) half-y) (+ (round y) half-y)))
                                 pose)))))))
          ;; Tape labels: the text's box centred on its strip, the brightest fifth of
          ;; its pixels against the tape's median, at least 4.5:1.
          (dolist (label lis::*cockpit-labels*)
            (multiple-value-bind (centre) (lis::label-frame label)
              (multiple-value-bind (cap advance) (lis::label-metrics label)
                ;; A pose may hide the label behind a panel: the best pose that holds it.
                (let ((ratio nil) (seen nil))
                  (dolist (pose *console-poses*)
                    (multiple-value-bind (r got) (ink-contrast centre cap advance (list pose))
                      (when got
                        (setf seen t)
                        (when (or (null ratio) (> r ratio)) (setf ratio r)))))
                  (true seen "tape label ~S in no pose's frame" (getf label :text))
                  (when seen
                    (true (>= ratio 4.5) "tape label ~S: contrast ~,1F" (getf label :text) ratio))))))
          ;; Placeholders: the text's box on each screen's face, at least 7:1.
          (let* ((panel (lis::cockpit-panel :orbit)) (cap (lis::panel-grid-cap panel))
                 (origin (lis::panel-grid-origin panel cap))
                 (centre (multiple-value-bind (c n r u) (lis::panel-frame panel)
                           (declare (ignore c n))
                           (lis::vlin origin (* 0.5 (length lis::*placeholder-text*) lis::+text-cell-width+ cap) r
                                      (* 0.5 cap) u))))
            (multiple-value-bind (ratio pose)
                (ink-contrast centre cap (* (length lis::*placeholder-text*) lis::+text-cell-width+ cap))
              (true pose "ORBIT's placeholder in no frame")
              (when pose
                (true (>= ratio 7) "placeholder on the ORBIT face: contrast ~,1F" ratio))))
          ;; The beige bezel's pixels are warmer than the black ones.
          (flet ((warmth (point)
                   (multiple-value-bind (x y per pose) (pose-pixel point width height)
                     (declare (ignore per))
                     (when x
                       (destructuring-bind (path w h pixels format) (shot pose)
                         (declare (ignore path w h))
                         (destructuring-bind (red green blue) (pixel-rgb pixels format width (round x) (round y))
                           (declare (ignore green))
                           (- red blue)))))))
            (let* ((beige (lis::cockpit-panel :autopilot)) (black (lis::cockpit-panel :attitude))
                   (beige-point (multiple-value-bind (c n r u) (lis::panel-frame beige)
                                  (declare (ignore n u))
                                  (multiple-value-bind (left) (lis::panel-borders beige)
                                    (lis::vlin c (- (+ (* 0.5d0 (first (getf beige :size))) (* 0.5d0 left))) r))))
                   (black-point (multiple-value-bind (c n r u) (lis::panel-frame black)
                                  (declare (ignore n r))
                                  (multiple-value-bind (left right top) (lis::panel-borders black)
                                    (declare (ignore left right))
                                    (lis::vlin c (+ (* 0.5d0 (second (getf black :size))) (* 0.5d0 top)) u))))
                   (warm (warmth beige-point)) (cold (warmth black-point)))
              (true (and warm cold) "the bezels' points lie in a pose's frame")
              (when (and warm cold)
                (true (> warm 10) "the beige bezel: red - blue = ~D" warm)
                (true (> warm cold) "the beige bezel (~D) warmer than the black (~D)" warm cold)))))))
    (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))))

(define-test render-frames
  (let ((width 320) (height 200))
    ;; offscreen-frame decodes the Blue Marble on its canvas; this one shares
    ;; the assets tests' decode.
    (share-earth-pixels)
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
           (object-checks game png width height)
           (grapple-checks game png width height)
           (map-checks game png width height)
           (ribbon-checks game png width height)
           (line-style-checks game png width height)
           (target-orbit-checks game png width height)
           (marker-checks game png width height)
           (text-checks game png width height)
           (cockpit-checks game png width height)
           (sun-patch-checks game png width height)
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
               ;; 3 frames, not 6: the swapchain is replaced on the first
               ;; (task 258: Bring scripts/test back under its ~15 s budget).
               (lis::wait-for-frames game 3)
               (is equal size (luv:canvas-extent (lis::game-context game))))
             (true (<= (luv:canvas-frame-resource-count frames) images)
                   "~D frame slots after resizes, ~D images"
                   (luv:canvas-frame-resource-count frames) images)
             (true (<= (hash-table-count (lis::renderer-color-views renderer)) images)))
           (luv:resize-canvas (lis::game-canvas game) 1280 800)
           (lis::wait-for-frames game 3)
           (readout-checks game png 1280 800)
           (console-checks game png 1280 800)
           (lost-checks game png 1280 800)
           (fly-screenshot-checks game)))))))

;;; The target at the render time (decision 114: Targets and target holds in
;;; the sim; decision 143: The target at the render time: the sim's gravity
;;; and the jerk).

(define-test render-state-target
  ;; On rails: Kepler at t_r, half a 2 s coarse step back.
  (let ((t0 (lis::start-t)))
    (dolist (key '(:tern-station :skua-yard))
      (multiple-value-bind (sim clock)
          (stepped-sim :sim (let ((sim (lis::new-game-sim)))
                              (lis::sim-set-target! sim key)
                              sim)
                       :level 1000 :until (+ t0 2d0))
        (setf (lis::clock-accumulator clock) 1d0)
        (let ((state (lis::render-state-update! (lis::make-render-state) clock sim))
              (r (lis::make-vec3)) (v (lis::make-vec3))
              (relative (lis::make-target-relative)))
          (is eq key (lis::render-state-target state))
          (let ((rails (if (eq key :skua-yard)
                           (lis::place-rails (lis::place key))
                           (lis::object-rails (lis::sim-object sim key)))))
            (lis::rails-state-into! r v (lis::rails-segment-at rails (+ t0 1d0)) (+ t0 1d0))
            (let ((dr (lis::vec3-norm (lis::vec3-sub (lis::render-state-target-r state) r))))
              (true (< dr 1d-6) "~A ~,3E m" key dr))
            (lis::target-relative-into! relative (lis::render-state-r state) (lis::render-state-v state)
                                        (lis::render-state-q state) r v
                                        (lis::rails-rate-into! (lis::vec3-copy v) r
                                                               (lis::rails-segment-at rails (+ t0 1d0)))))
          (true (every (lambda (a b) (< (abs (- a b)) 1d-6))
                       relative (lis::render-state-target-relative state)))))))
  ;; Promoted (:j2): the extrapolation from the step's end against the
  ;; fine-stepped station at t_r, 1 s and 2 s back.
  (let* ((sim (ship-near :tern-station 1000d0))
         (t0 (lis::sim-t sim)))
    (setf (lis::sim-model sim) :j2)
    (lis::sim-set-target! sim :tern-station)
    (dolist (back '(1d0 2d0))
      (let ((reference (step-sim (lis::copy-sim sim) most-positive-fixnum 1 (- (+ t0 2d0) back))))
        (multiple-value-bind (stepped clock) (stepped-sim :sim (lis::copy-sim sim) :level 1000
                                                         :until (+ t0 2d0))
          (is = (+ t0 2d0) (lis::sim-t stepped))
          (setf (lis::clock-accumulator clock) (- 2d0 back))
          (let* ((state (lis::render-state-update! (lis::make-render-state) clock stepped))
                 (station (lis::object-state (lis::sim-object reference :tern-station)))
                 (dr (lis::vec3-norm (lis::vec3-sub (lis::render-state-target-r state)
                                                    (lis::ship-state-r station))))
                 (dv (lis::vec3-norm (lis::vec3-sub (lis::render-state-target-v state)
                                                    (lis::ship-state-v station)))))
            (is = (- (+ t0 2d0) back) (lis::render-state-t state))
            (true (< dr 1d-3) "~A s back: ~,3E m" back dr)
            (true (< dv 1d-3) "~A s back: ~,3E m/s" back dv)))))
    ;; No target: none.
    (lis::sim-set-target! sim nil)
    (multiple-value-bind (stepped clock) (stepped-sim :sim sim)
      (let ((state (lis::render-state-update! (lis::make-render-state) clock stepped)))
        (false (lis::render-state-target state))))))
;;; decision 148: The closing speed of a rails target: the position's rate.

(define-test render-state-target-closing-speed
  ;; Tern Station on rails from the start orbit: the closing speed against
  ;; the range's centred difference over ±1/64 s, the ship moved by
  ;; r + v τ + a τ²/2.
  (let* ((sim (lis::new-game-sim))
         (clock (lis::new-clock sim))
         (t0 (lis::sim-t sim))
         (h (/ 64d0))
         (ship (lis::sim-ship sim))
         (rails (lis::object-rails (lis::sim-object sim :tern-station)))
         (a (lis::gravity-into! (lis::make-vec3) (lis::ship-state-r ship) t0 :j2))
         (r (lis::make-vec3)) (v (lis::make-vec3)))
    (lis::sim-set-target! sim :tern-station)
    (is eq :rails (lis::object-status (lis::sim-object sim :tern-station)))
    (flet ((range (tau)
             (let ((time (+ t0 tau)))
               (lis::rails-state-into! r v (lis::rails-segment-at rails time) time)
               (lis::vec3-norm
                (map 'lis::vec3 (lambda (rt rs vs as) (- rt (+ rs (* tau (+ vs (* 0.5d0 tau as))))))
                     r (lis::ship-state-r ship) (lis::ship-state-v ship) a)))))
      (let* ((state (lis::render-state-update! (lis::make-render-state) clock sim))
             (closing (aref (lis::render-state-target-relative state) 1))
             (rate (/ (- (range (- h)) (range h)) (* 2 h))))
        (is eq :tern-station (lis::render-state-target state))
        (true (< (abs (- closing rate)) 1d-3) "CLS ~,6F, −d(range)/dt ~,6F m/s" closing rate)))))

;;; The grapple at work (decision 124; task 223): the range finder's spot and
;;; the floodlight's rows.

(defun spot-state (distance)
  "A render state with the Mule at the origin, axes the world's, its CoM at R,
and Skua Yard 3 drawn on the +x axis with its surface DISTANCE m past the tip."
  (let* ((state (lis::make-render-state))
         (def (lis::object-definition :skua-yard-3))
         (radius (lis::shape-radius (first (lis::object-def-shapes def))))
         (o (object-render-of state :skua-yard-3)))
    (setf (lis::render-state-com-valid state) t)
    (replace (lis::object-render-r o) (lis::vec3 (+ 16.6d0 distance radius) 0d0 0d0))
    (setf (lis::object-render-def o) def (lis::object-render-drawn o) t)
    state))

(define-test range-finder-spot-first-hit
  (let ((out (lis::make-vec3)))
    (let ((state (spot-state 50d0)))
      (true (lis::range-finder-spot out state))
      ;; The first hit, 50 m from the tip, pulled 0.05 m towards the ship.
      (true (close-to out (lis::vec3 (+ 16.6d0 (- 50d0 lis::*range-spot-pull*)) 0d0 0d0) 1d-3)
            "the spot at ~A" out)
      (is = 0 (least-bytes-consed (lambda () (dotimes (i 3000) (lis::range-finder-spot out state))))))
    ;; Beyond 500 m, and with nothing drawn, no spot.
    (false (lis::range-finder-spot out (spot-state 520d0)))
    (let ((state (spot-state 50d0)))
      (setf (lis::object-render-drawn (object-render-of state :skua-yard-3)) nil)
      (false (lis::range-finder-spot out state)))
    ;; Off the ray: the sphere 20 m to the side is missed.
    (let* ((state (spot-state 50d0))
           (o (object-render-of state :skua-yard-3)))
      (setf (aref (lis::object-render-r o) 1) 40d0)
      (false (lis::range-finder-spot out state)))
    ;; A box shape: the pod's near face, 50 m ahead, hit at its face.
    (let* ((state (lis::make-render-state))
           (def (lis::object-definition :pod-14))
           (o (object-render-of state :pod-14))
           (half (* 0.5d0 (aref (lis::shape-size (first (lis::object-def-shapes def))) 0))))
      (setf (lis::render-state-com-valid state) t
            (lis::object-render-def o) def (lis::object-render-drawn o) t)
      (replace (lis::object-render-r o) (lis::vec3 (+ 16.6d0 50d0 half) 0d0 0d0))
      (true (lis::range-finder-spot out state))
      (true (< (abs (- (aref out 0) (+ 16.6d0 (- 50d0 lis::*range-spot-pull*)))) 1d-3)
            "the pod's face at ~A" out))))

(define-test floodlight-rows
  ;; Zero while stowed, in the Mule's block and an object's; armed, the lamp
  ;; and axis in the block's own axes.
  (let* ((state (lis::make-render-state))
         (o (object-render-of state :k-17))
         (ship (make-array lis::+ship-uniform-floats+ :element-type 'single-float :initial-element 1f0))
         (object (make-array lis::+ship-uniform-floats+ :element-type 'single-float :initial-element 1f0))
         (grapple (lis::render-state-grapple state))
         (eye (lis::vec3 0d0 0d0 0d0)))
    (setf (lis::render-state-com-valid state) t
          (lis::render-state-r state) (lis::vec3 7d6 0d0 0d0)
          (lis::object-render-r o) (lis::vec3 (+ 7d6 30d0) 5d0 0d0)
          (lis::object-render-drawn o) t)
    (setf (lis::render-state-r state) (lis::vec3 7d6 0d0 0d0))
    (replace eye (lis::render-state-r state))
    (replace (lis::render-state-sun state) (lis::vec3 1.5d11 0d0 0d0))
    (lis::ship-uniforms! ship state eye (lis::vec3 0 0 0))
    (lis::object-uniforms! object state o eye)
    (loop for i from 52 below 60 do (is = 0 (aref ship i)) (is = 0 (aref object i)))
    (setf (lis::render-grapple-floodlight grapple) t)
    (lis::ship-uniforms! ship state eye (lis::vec3 0 0 0))
    (lis::object-uniforms! object state o eye)
    (loop for expected in (list 15.45 0 -0.32 lis::*floodlight-intensity* 1 0 0 0)
          for i from 52
          do (true (< (abs (- (aref ship i) expected)) 1d-5) "ship ~D: ~F" i (aref ship i)))
    ;; The object's axes are the world's: the lamp 30 m behind it, 5 m off.
    (loop for expected in (list (- 15.45 30) -5 -0.32 lis::*floodlight-intensity* 1 0 0 0)
          for i from 52
          do (true (< (abs (- (aref object i) expected)) 1d-5) "object ~D: ~F" i (aref object i)))))

(defun shadowed-yard-sim (offset)
  "The start orbit in Earth's shadow (u = 180 degrees), Skua Yard 3 promoted
with the ship's velocity at the body point OFFSET (from R), its axes the
world's (the Yard itself is never in Earth's shadow within days of the
start, so the rock is moved to the ship)."
  (let* ((sim (start-sim-at 180))
         (ship (lis::sim-ship sim))
         (com (lis::mass-properties-com (lis::ship-mass-properties (lis::ship-state-m-p ship))))
         (rock (lis::sim-object sim :skua-yard-3)))
    (true (lis::object-promote! rock (lis::sim-t sim)))
    (let ((state (lis::object-state rock)))
      (replace (lis::ship-state-r state)
               (lis::vec3-add (lis::ship-state-r ship)
                              (lis::quat-rotate (lis::ship-state-q ship)
                                                (lis::vec3-sub (lis::v3 offset) com))))
      (replace (lis::ship-state-v state) (lis::ship-state-v ship))
      (replace (lis::ship-state-q state) (lis::quat 1 0 0 0))
      (fill (lis::ship-state-w state) 0d0))
    sim))

(defun grapple-checks (game png width height)
  (flet ((capture (sim camera-fn)
           (multiple-value-bind (path w h pixels pixel-format)
               (lis::capture-game-state game png :sim sim :frames 1 :camera-fn camera-fn :readout nil)
             (declare (ignore path w h))
             (values pixels pixel-format)))
         (arm! (sim state) (setf (lis::grapple-state (lis::sim-grapple sim)) state)))
    (with-canvas-values (game (lis::*star-radiance-limit* 0.0))
      (let ((radius (lis::shape-radius (first (lis::object-def-shapes (lis::object-definition :skua-yard-3))))))
        ;; In Earth's shadow, Skua Yard 3's centre 10 m + R ahead of the tip.
        (flet ((rock-brightness (offset state)
                 (let* ((sim (shadowed-yard-sim offset))
                        (camera-fn (ship-view '(-1 0 0.5) 14 offset)))
                   (arm! sim state)
                   (let* ((render (sim-render-state sim))
                          (camera (posed-camera camera-fn render))
                          (pixels (capture sim camera-fn))
                          (o (object-render-of render :skua-yard-3))
                          (c (lis::vec3-sub (lis::object-render-r o)
                                            (lis::vec3-add (lis::render-state-r render)
                                                           (lis::camera-offset camera)))))
                     (multiple-value-bind (x y) (project camera c width height)
                       (let* ((half (* 0.8 (/ (* 0.5 height radius)
                                              (* (lis::vec3-norm c) (tan (* 0.5d0 (lis::camera-field-of-view camera)))))))
                              (values (box-pixels pixels width height (- x half) (- y half) (+ x half) (+ y half))))
                         (/ (reduce #'+ values) (max 1 (length values)))))))))
          (let* ((on-axis (list (+ 16.6d0 10d0 radius) 0d0 0d0))
                 (armed (rock-brightness on-axis :armed))
                 (stowed (rock-brightness on-axis :stowed)))
            (true (> armed (+ stowed 20)) "the rock 10 m ahead: ~,1F armed, ~,1F stowed" armed stowed))
          ;; 30 degrees off the beam's axis, as seen from the lamp: unlit.
          (let* ((lamp lis::*floodlight-position*)
                 (off-axis (list (+ (aref lamp 0) (* 30d0 (cos (deg 30))))
                                 (* 30d0 (sin (deg 30))) (aref lamp 2)))
                 (armed (rock-brightness off-axis :armed))
                 (stowed (rock-brightness off-axis :stowed)))
            (true (< (abs (- armed stowed)) 3) "30 degrees off the beam: ~,1F armed, ~,1F stowed" armed stowed))))
      ;; The range finder's spot on the rock: red while armed, absent stowed.
      (let* ((radius (lis::shape-radius (first (lis::object-def-shapes (lis::object-definition :skua-yard-3)))))
             (offset (list (+ 16.6d0 10d0 radius) 0d0 0d0))
             ;; From 4 m: the spot is 0.03 m, a fraction of a pixel from afar.
             (camera-fn (ship-view '(-1 0 0.5) 4 (list (- (+ 16.6d0 10d0) lis::*range-spot-pull*) 0d0 0d0))))
        (flet ((spot-pixel (state)
                 (let* ((sim (shadowed-yard-sim offset)))
                   (setf (lis::grapple-state (lis::sim-grapple sim)) state)
                   (let* ((render (sim-render-state sim))
                          (camera (posed-camera camera-fn render))
                          (spot (lis::make-vec3)))
                     (true (lis::range-finder-spot spot render))
                     (multiple-value-bind (pixels pixel-format) (capture sim camera-fn)
                       (multiple-value-bind (x y)
                           (project camera (lis::vec3-sub spot (lis::vec3-add (lis::render-state-r render)
                                                                              (lis::camera-offset camera)))
                                    width height)
                         (let ((best -255))
                           ;; The brightest red-over-green within 1 px of the spot.
                           (loop for dy from -1 to 1
                                 do (loop for dx from -1 to 1
                                          do (destructuring-bind (red green blue)
                                                 (pixel-rgb pixels pixel-format width (+ (floor x) dx) (+ (floor y) dy))
                                               (declare (ignore blue))
                                               (setf best (max best (- red green))))))
                           best)))))))
          (let ((armed (spot-pixel :armed)) (stowed (spot-pixel :stowed)))
            (true (> armed 50) "the spot while armed: red over green ~D" armed)
            (true (< stowed 20) "the spot while stowed: red over green ~D" stowed))))
      ;; The claws from 8 m on +y, the Sun behind the camera: the tip of the top
      ;; claw lit where armed, empty where stowed, and projected outward.
      (let* ((sim (lit-ship-sim))
             (state (sim-render-state sim))
             (r (lis::render-state-r state))
             (s (lis::vec3-normalize (lis::vec3-sub (lis::render-state-sun state) r))))
        (turn-ship! sim '(0 1 0) s)
        (let* ((camera-fn (ship-view '(0 1 0) 8 '(16.3d0 0d0 -0.4d0)))
               (state (sim-render-state sim))
               (camera (posed-camera camera-fn state))
               (out (make-array (* 3 lis::+instance-floats+) :element-type 'single-float))
               (tips '()) (levels '()))
          (dolist (angle '(0 30))
            (lis::ship-claw-instances! out angle)
            (let ((tip (lis::claw-tip out 0)))
              (multiple-value-bind (x y) (project camera (ship-point state camera tip) width height)
                (push (list x y) tips)
                (arm! sim (if (zerop angle) :stowed :armed))
                (let ((pixels (capture sim camera-fn)))
                  ;; The claw's last 0.2 m, a point back from the tip.
                  (let ((back (lis::vec3-sub tip (lis::vec3-scale (lis::vec3-sub tip (lis::vec3 16d0 0d0 -0.85d0))
                                                                    0.3d0))))
                    (multiple-value-bind (bx by) (project camera (ship-point state camera back) width height)
                      (push (/ (reduce #'+ (box-pixels pixels width height (- bx 1) (- by 1) (+ bx 1) (+ by 1)))
                               9)
                            levels)))))))
          (destructuring-bind (armed-tip stowed-tip) tips
            (true (> (- (second stowed-tip) (second armed-tip)) 1)
                  "the claw's tip moves up the image: y ~,1F to ~,1F" (second stowed-tip) (second armed-tip)))
          (true (> (first levels) (+ (second levels) 20))
                "the armed claw's pixels ~,1F, stowed ~,1F" (first levels) (second levels))))
      (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream))))
    (lis::game-load-state! game (lis::new-game-sim))))

(define-test render-cockpit-limb
  ;; The cockpit view with the head on the nose (yaw 0, pitch 0, 70°), the cab
  ;; exterior not drawn and no interior: Earth's limb on the nose column lies
  ;; 19.8° below the axis, f tan 19.8° below the centre, space above it
  ;; (decision 126: Cockpit structure and view in milestone 3).
  (let* ((width 320) (height 200)
         (f (/ (/ height 2) (tan (* 35 (/ pi 180)))))
         (expected (+ (/ height 2) (* f (tan (* 19.8d0 (/ pi 180)))))))
    (share-earth-pixels)
    (luv:call-with-sdl-main-thread
     (lambda ()
       (lis::with-capture-game (game :width width :height height :readout nil)
         (uiop:with-temporary-file (:pathname png :type "png")
           (let ((pixels (nth-value 3 (lis::capture-game-state game png :view :cockpit
                                                                        :head '(0 0 70)
                                                                        :interior nil :frames 2)))
                 (x (floor width 2)))
             (flet ((lum (y) (pixel-luminance pixels width x y)))
               ;; The brightest row of the column is the limb's rim, under
               ;; the halo's glow; space is dark above the halo.
               (let ((limb (loop with best = 0 with row = 0
                                 for y from (floor height 2) below height
                                 do (when (> (lum y) best) (setf best (lum y) row y))
                                 finally (return row))))
                 (true (< (abs (- limb expected)) 2) "limb at row ~D, expected ~,1F" limb expected)
                 (true (< (loop for y below (- limb 15) maximize (lum y)) 40)
                       "space above the limb is dark"))))
           (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))))))))

;;; The sunlit patch (plan 31 piece 6): the Sun at an elevation above the nose
;;; line through the window lights a patch on the deck, found by the twin.

(defun sun-body-direction (elevation)
  "The body direction of the Sun ELEVATION degrees above the nose line (z is down)."
  (lis::vec3 (cos (deg elevation)) 0d0 (- (sin (deg elevation)))))

(defun sun-sim (u elevation)
  "The start orbit at argument of latitude U, the ship turned so the Sun is at ELEVATION."
  (let* ((sim (start-sim-at u))
         (state (sim-render-state sim))
         (s (lis::vec3-normalize (lis::vec3-sub (lis::render-state-sun state)
                                                (lis::render-state-r state)))))
    (turn-ship! sim (coerce (sun-body-direction elevation) 'list) s)))

(defun deck-point-pose (point)
  "(values yaw pitch) in degrees of a head looking at POINT, or nil past its limits."
  (let ((yaw (* (/ 180 pi) (atan (aref point 1) (aref point 0))))
        (pitch (* (/ 180 pi) (asin (/ (aref point 2) (lis::vec3-norm point))))))
    (when (and (< (abs yaw) 145) (< pitch 75) (> pitch -55))
      (values yaw pitch))))

(defun find-deck-point (test)
  "A deck point (body axes from the eye, z +1.15) behind the eye a head can look
at for which TEST (a function of the point) holds."
  (loop for x from -2.0d0 to 0.8d0 by 0.05d0
        do (loop for y from -1.0d0 to 1.0d0 by 0.05d0
                 for p = (lis::vec3 x y 1.15d0)
                 when (and (deck-point-pose p) (funcall test p))
                   do (return-from find-deck-point p))))

(defun deck-pixel (game png sim point width height)
  "The centre pixel's rgb of the cockpit with the head aimed at POINT."
  (multiple-value-bind (yaw pitch) (deck-point-pose point)
    (multiple-value-bind (path w h pixels format)
        (lis::capture-game-state game png :sim sim :view :cockpit :readout nil :frames 2
                                          :head (list yaw pitch 40))
      (declare (ignore path w h))
      (pixel-rgb pixels format width (floor width 2) (floor height 2)))))

(defun sun-patch-checks (game png width height)
  (with-canvas-values (game (lis::*bloom-strength* 0.0))
    (let* ((s (sun-body-direction 30))
           (s5 (sun-body-direction 35))
           (lit (find-deck-point (lambda (p) (lis::sun-patch-p p s -0.03d0))))
           (flip (find-deck-point (lambda (p) (and (lis::sun-patch-p p s5 -0.03d0)
                                                   (not (lis::sun-patch-p p s 0.03d0)))))))
      (true lit "a lit deck point the head can see")
      (true flip "a deck point the 5 degree turn lights")
      (when (and lit flip)
        (let ((exposure 4.0)
              (up (lis::vec3 0d0 0d0 -1d0)))
          (flet ((led-level (point)
                   ;; The byte range of the unlit tread plate (diamonds 0.72..1).
                   (multiple-value-bind (r g b) (lis::cab-irradiance point up)
                     (declare (ignore r b))
                     (let ((albedo (aref lis::*cab-materials* 2 0)))
                       (values (- (lis::tone-map-srgb (* 0.72 0.9 albedo g) exposure) 4)
                               (+ (lis::tone-map-srgb (* 1.1 albedo g) exposure) 4))))))
            (let ((sim (sun-sim 60 30)))
              (let ((got (deck-pixel game png sim lit width height)))
                (true (> (second got) 200) "the patch's deck pixel ~A" got))
              ;; 0.05 m outside the patch: the LED level.
              (let* ((outside (loop for x from (aref lit 0) to 0.8d0 by 0.01d0
                                    for p = (lis::vec3 x (aref lit 1) 1.15d0)
                                    unless (lis::sun-patch-p p s 0.05d0) return p))
                     (got (and outside (deck-pixel game png sim outside width height))))
                (true (and outside (deck-point-pose outside)) "a point outside the patch")
                (when got
                  (multiple-value-bind (lo hi) (led-level outside)
                    (true (<= lo (second got) hi) "outside the patch: ~A, ~D..~D" got lo hi)))))
            ;; Turned 5 degrees the patch has moved: FLIP is unlit at 30, lit at 35.
            (let ((got (deck-pixel game png (sun-sim 60 30) flip width height)))
              (multiple-value-bind (lo hi) (led-level flip)
                (true (<= lo (second got) hi) "before the turn: ~A, ~D..~D" got lo hi)))
            (let ((got (deck-pixel game png (sun-sim 60 35) flip width height)))
              (true (> (second got) 200) "after the turn: ~A" got))
            ;; In Earth's shadow there is no patch.
            (let ((got (deck-pixel game png (sun-sim 180 30) lit width height)))
              (multiple-value-bind (lo hi) (led-level lit)
                (true (<= lo (second got) hi) "in shadow: ~A, ~D..~D" got lo hi)))))))))
