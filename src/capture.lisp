(in-package #:luv-in-space)

;;; Offscreen screenshots through luv's capture path (decision 2: Screenshots
;;; and GPU test: offscreen capture on a hidden canvas, headless on lavapipe).

(defparameter *screenshot-frames* 10
  "Frames drawn before the capture; the scene is static, so a few suffice.")

(defun screenshot (pathname &key (game *game*))
  "Write a PNG of GAME's scene at its canvas size; returns PATHNAME, width,
height, the packed pixels and their colour format."
  (multiple-value-bind (path pixels width height format)
      (luv:capture-application-screenshot game pathname :label "luv-in-space screenshot")
    (values path width height pixels format)))

(defun wait-for-frames (game count &key (timeout 60))
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        until (>= (game-frame-count game) count)
        do (when (> (get-internal-real-time) deadline)
             (error "Only ~D of ~D frames drawn in ~D s."
                    (game-frame-count game) count timeout))
           (sleep 0.01)))

(defun capture-scene (pathname &key (frames *screenshot-frames*) (width 1280) (height 800))
  "Open a hidden canvas, draw FRAMES frames, write a PNG and close it again.
Call on luv's SDL main thread (LUV:CALL-WITH-SDL-MAIN-THREAD)."
  (let ((game (start :visible nil :width width :height height)))
    (unwind-protect
         (progn (wait-for-frames game frames)
                (screenshot pathname :game game))
      (stop game))))

(defun screenshot-main (pathname &key (frames *screenshot-frames*))
  "Entry point of scripts/screenshot: write the PNG, report it, exit 0; exit 1 on error."
  (handler-case
      (multiple-value-bind (path width height)
          (luv:call-with-sdl-main-thread
           (lambda () (capture-scene pathname :frames frames)))
        (format t "~&~A ~Dx~D~%" (uiop:native-namestring path) width height)
        (finish-output)
        (uiop:quit 0))
    (error (condition)
      (format *error-output* "~&screenshot failed: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
