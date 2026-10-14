(in-package #:luv-in-space.tests)

;;; One frame offscreen on a hidden canvas: on the GPU with a display, on
;;; lavapipe without (decision 2: Screenshots and GPU test: offscreen capture
;;; on a hidden canvas, headless on lavapipe).

(defun png-size (pathname)
  "Width and height from a PNG's IHDR chunk."
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((header (make-array 24 :element-type '(unsigned-byte 8))))
      (read-sequence header in)
      (flet ((u32 (start) (reduce (lambda (a b) (+ (* a 256) b)) header
                                  :start start :end (+ start 4))))
        (values (u32 16) (u32 20)
                (equalp (subseq header 0 8) #(137 80 78 71 13 10 26 10)))))))

(define-test offscreen-frame
  (uiop:with-temporary-file (:pathname png :type "png")
    (multiple-value-bind (path width height pixels)
        (luv:call-with-sdl-main-thread
         (lambda () (lis::capture-scene png :frames 2 :width 320 :height 200)))
      (is = 320 width)
      (is = 200 height)
      (multiple-value-bind (png-width png-height signature-p) (png-size path)
        (true signature-p)
        (is = 320 png-width)
        (is = 200 png-height))
      ;; The Earth fills the centre; the background is near black.
      (let* ((offset (* 4 (+ (* 100 320) 160)))
             (sum (+ (aref pixels offset) (aref pixels (+ offset 1))
                     (aref pixels (+ offset 2)))))
        (true (> sum 60) "centre pixel ~D is background" sum)))))
