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
