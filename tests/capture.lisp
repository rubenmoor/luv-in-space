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
