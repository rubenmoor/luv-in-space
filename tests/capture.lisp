(in-package #:luv-in-space.tests)

;;; scripts/screenshot's capture: the start state on its own hidden canvas
;;; (decision 2: Screenshots and GPU test: offscreen capture on a hidden
;;; canvas, headless on lavapipe). The picture's checks are in tests/render.lisp.

(define-test offscreen-frame
  (uiop:with-temporary-file (:pathname png :type "png")
    (multiple-value-bind (path width height)
        (luv:call-with-sdl-main-thread
         (lambda () (lis::capture-scene png :frames 2 :width 320 :height 200)))
      (is = 320 width)
      (is = 200 height)
      (multiple-value-bind (png-width png-height signature-p) (png-size path)
        (true signature-p)
        (is = 320 png-width)
        (is = 200 png-height)))))

;;; scripts/fly's PNGs (decision 87), on render-frames' canvas at 1280 x 800.

(defun fly-screenshot-checks (game)
  "fly-screenshots of burn's record on GAME: three PNGs of 1280 x 800, none
fully black."
  (uiop:with-temporary-file (:pathname base)
   (let ((record (lis::run-flight "burn"))
         (dir (uiop:ensure-directory-pathname (format nil "~A.d" (uiop:native-namestring base)))))
    (unwind-protect
         (multiple-value-bind (paths pixels) (lis::fly-screenshots record dir :game game :frames 2)
           (is equal '("01-before.png" "02-during.png" "03-after.png")
               (mapcar #'file-namestring paths))
           (dolist (path paths)
             (multiple-value-bind (width height signature-p) (png-size path)
               (true signature-p)
               (is = 1280 width)
               (is = 800 height)))
           (loop for path in paths for p in pixels
                 do (true (find-if #'plusp p) "~A: fully black" (file-namestring path))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(defun pixels-differing (a b)
  "The share of pixels (4 bytes) where A and B differ."
  (/ (loop for i from 0 below (length a) by 4
           count (loop for j below 4 thereis (/= (aref a (+ i j)) (aref b (+ i j)))))
     (/ (length a) 4d0)))

(defun screen-state-checks (game)
  "fly-screenshots of a set-ap record, a shot in each view, on GAME at
1280 x 800 (decision 131): three PNGs, none fully black; the outside one with
the autopilot panel's box top right and text in it, its warp row unlike a
capture without the screen; the cockpit one unlike the outside one."
  (uiop:with-temporary-file (:pathname base)
    (let* ((record (lis::run-flight "((:autopilot :set-ap 500e3) (:warp :coast) (:wait :in-burn :apsis)
 (:screenshot \"o\") (:screenshot \"m\" :view :map) (:screenshot \"c\" :view :cockpit))"
                                    :model :point))
           (dir (uiop:ensure-directory-pathname (format nil "~A.s" (uiop:native-namestring base))))
           (entry (find :screenshot record :key #'lis::flight-entry-kind)))
      (unwind-protect
           (multiple-value-bind (paths pixels) (lis::fly-screenshots record dir :game game :frames 2)
             (is equal '("01-o.png" "02-m.png" "03-c.png") (mapcar #'file-namestring paths))
             (dolist (path paths)
               (multiple-value-bind (width height signature-p) (png-size path)
                 (true signature-p)
                 (is = 1280 width)
                 (is = 800 height)))
             (loop for path in paths for p in pixels
                   do (true (find-if #'plusp p) "~A: fully black" (file-namestring path)))
             (let* ((outside (first pixels))
                    (box-width (lis::readout-box-size nil lis::*autopilot-panel-chars*
                                                      lis::*autopilot-panel-chars*))
                    (x0 (- 1280 lis::*readout-margin-px* box-width))
                    (y0 lis::*readout-margin-px*))
               (true (< (abs (- (pixel-luminance outside 1280 (+ x0 2) (+ y0 2)) 34/3)) 1)
                     "the panel's box colour")
               (true (loop for y from y0 below (+ y0 160)
                           thereis (loop for x from x0 below (+ x0 box-width)
                                         thereis (> (pixel-luminance outside 1280 x y) 100)))
                     "text in the panel")
               (true (> (pixels-differing outside (third pixels)) 0.5d0))
               (true (> (nth (lis::clock-screen-level-index (lis::flight-entry-screen entry))
                             lis::*warp-levels*)
                        1))
               ;; the readout's warp row: the same sim without the screen reads WARP ×1
               (let ((plain (nth-value 3 (lis::capture-game-state
                                          game (merge-pathnames "plain.png" dir)
                                          :sim (lis::flight-entry-sim entry) :frames 2))))
                 (true (> (pixels-differing outside plain) 0)))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(define-test capture-cockpit-poses-home-head
  ;; capture-state :view :cockpit: the head at Home unless :head gives it, the
  ;; field of view snapped (decision 126: Cockpit structure and view in milestone 3).
  (uiop:with-temporary-file (:pathname png :type "png")
    (luv:call-with-sdl-main-thread
     (lambda ()
       (lis::with-capture-game (game :width 320 :height 200)
         (lis::cockpit-view-drag! (lis::game-cockpit-view game) 100d0 100d0)
         (lis::capture-game-state game png :view :cockpit :frames 2)
         (let ((view (lis::game-cockpit-view game)))
           (is eq :cockpit (lis::game-view game))
           (is = 0d0 (lis::cockpit-view-yaw view))
           (is = (* 16 (/ pi 180)) (lis::cockpit-view-pitch view))
           (is = (* 70 (/ pi 180)) (lis::cockpit-view-fov view)))
         (lis::capture-game-state game png :view :cockpit :head '(30 -5 50) :interior nil :frames 2)
         (let ((view (lis::game-cockpit-view game)))
           (true (< (abs (- (lis::cockpit-view-yaw view) (* 30 (/ pi 180)))) 1d-12))
           (true (< (abs (- (lis::cockpit-view-pitch view) (* -5 (/ pi 180)))) 1d-12))
           (true (< (abs (- (lis::cockpit-view-fov view) (* 50 (/ pi 180)))) 1d-12))
           (false (lis::game-interior-p game))))))))

(define-test capture-message-age
  ;; :message-age gives the newest SHIP entry's age in real seconds (decision 129).
  (uiop:with-temporary-file (:pathname png :type "png")
    (luv:call-with-sdl-main-thread
     (lambda ()
       (lis::with-capture-game (game :width 320 :height 200)
         (lis::call-on-canvas-thread
          game (lambda () (lis::clock-message! (lis::game-clock game) :grapple "LATCHED TERN STATION")))
         (lis::capture-game-state game png :frames 2 :message-age 40)
         (let ((ml (lis::game-message-line game)))
           (true (lis::message-line-entry ml))
           (true (> (- (lis::game-real-time game) (lis::message-line-seen ml)) 40d0)))
         (lis::capture-game-state game png :frames 2)
         (let ((ml (lis::game-message-line game)))
           (true (< (- (lis::game-real-time game) (lis::message-line-seen ml)) 30d0))))))))
