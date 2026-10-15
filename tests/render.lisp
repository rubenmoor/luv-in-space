(in-package #:luv-in-space.tests)

;;; render.lisp: the render state (decision 77: The frame in main.lisp:
;;; render state, capture of a given sim, part 4's placeholders and first
;;; asset), the meshes, and the frame on the GPU ("GPU tests").

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

;;; Meshes

(defun vertex-norms (data)
  (loop for i below (length data) by 3
        collect (sqrt (+ (expt (aref data i) 2)
                         (expt (aref data (+ i 1)) 2)
                         (expt (aref data (+ i 2)) 2)))))

(define-test uv-sphere-counts
  ;; A triangle list: two triangles per stack and slice, three floats each.
  (loop for (stacks slices) in '((1 3) (4 8) (32 64))
        for data = (lis::uv-sphere :stacks stacks :slices slices)
        do (is = (* stacks slices 6 3) (length data))
           (is eq 'single-float (array-element-type data))))

(define-test uv-sphere-unit-radius
  ;; Each position is also its normal, so this checks both.
  (let ((norms (vertex-norms (lis::uv-sphere :stacks 8 :slices 16))))
    (true (every (lambda (r) (< (abs (- r 1)) 1e-5)) norms))))

(define-test uv-sphere-poles
  (let* ((data (lis::uv-sphere :stacks 8 :slices 16))
         (ys (loop for i from 1 below (length data) by 3 collect (aref data i))))
    (is = 1.0 (reduce #'max ys))
    (is = -1.0 (reduce #'min ys))))

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

(defun nadir-camera (camera state)
  "Straight down from the ship, prograde up."
  (let* ((x (lis::vec3-normalize (lis::vec3-scale (lis::render-state-r state) -1d0)))
         (z (lis::vec3-normalize (lis::vec3-cross x (lis::vec3-cross (lis::render-state-v state) x))))
         (z (lis::vec3-scale z -1d0))
         (y (lis::vec3-cross z x)))
    (lis::quat-from-basis! (lis::camera-orientation camera) x y z)
    (fill (lis::camera-offset camera) 0d0)))

(defun away-camera (camera state)
  "From the ship, away from Earth and the Sun: a dark view."
  (let* ((r (lis::render-state-r state))
         (n (lis::vec3-normalize (lis::vec3-cross r (lis::render-state-sun state)))))
    (look-along! camera (lis::vec3-add (lis::vec3-scale n 2d0) (lis::vec3-normalize r)))))

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

(defun label-colour-bytes (format)
  "The overlay label's colour as the target stores it, in red, green, blue."
  (mapcar (lambda (x) (if (search "SRGB" (string format)) (srgb-byte x) (round (* 255 x))))
          '(1.0 0.75 0.2)))

(defun pixel-rgb (pixels format width x y)
  (multiple-value-bind (red blue) (pixel-red-blue pixels format width x y)
    (list red (aref pixels (+ 1 (* 4 (+ (* y width) x)))) blue)))

(define-test render-frames
  (let ((width 320) (height 200))
    (luv:call-with-sdl-main-thread
     (lambda ()
       (lis::with-capture-game (game :width width :height height)
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
           ;; The label is drawn after the tone map: exactly its colour (the
           ;; GPU's rounding aside), not bloomed. P's top row: dots 0-3 of 5,
           ;; 3 px each at the 16 px margin.
           (multiple-value-bind (path w h pixels format) (lis::capture-game-state game png :frames 1)
             (declare (ignore path w h))
             (let ((colour (label-colour-bytes format)))
               (dolist (x '(17 20 23 26))
                 (let ((pixel (pixel-rgb pixels format width x 17)))
                   (true (every (lambda (a b) (<= (abs (- a b)) 1)) colour pixel)
                         "label pixel ~D: ~A, not ~A" x pixel colour)))
               (false (equal colour (pixel-rgb pixels format width 29 17)))))
           (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))
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
           (is = 0 (lis:report-failed-live-artifacts game (make-broadcast-stream)))))))))
