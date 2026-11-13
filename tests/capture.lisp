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
