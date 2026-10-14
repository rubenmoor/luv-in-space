(in-package #:luv-in-space)

(defparameter *background* '(0.01 0.01 0.02)
  "Linear RGB the frame is cleared to.")

(defclass game ()
  ((canvas :initarg :canvas :reader game-canvas)
   (device :initarg :device :reader game-device)
   (context :initarg :context :reader game-context)
   (quit-requested-p :initform nil :accessor game-quit-requested-p)))

(defvar *game* nil)

(defun draw (game)
  (destructuring-bind (red green blue) *background*
    (luv:render-canvas-color (game-context game) red green blue)))

;; Runs on the canvas thread: only flag the quit; STOP runs on the caller's.
(defun handle-event (game event)
  (when (or (and (typep event 'luv:canvas-key-press-event)
                 (eq :escape (luv:canvas-key-event-key-name event)))
            (typep event 'luv:canvas-window-close-request-event))
    (setf (game-quit-requested-p game) t)))

(defun start (&key (visible t) (width 1280) (height 800))
  "Open the game window and start drawing; returns the game."
  (let* ((canvas (luv:make-sdl-canvas :title "Luv in Space" :width width :height height
                                      :visible-p visible))
         (_ (luv:open-canvas canvas))
         (device (luv:request-gpu-device luv:*gpu-provider*
                                         (luv:make-device-descriptor :label "luv-in-space")))
         (context (luv:make-canvas-context canvas luv:*gpu-provider*
                                           (luv:make-canvas-configuration :device device)))
         (game (make-instance 'game :canvas canvas :device device :context context)))
    (declare (ignore _))
    (setf (luv:canvas-event-handler canvas)
          (lambda (canvas event) (declare (ignore canvas)) (handle-event game event)))
    (setf (luv:canvas-clock canvas)
          (luv:make-cadence-clock (lambda (native timestamp)
                                    (declare (ignore native timestamp))
                                    (draw game))
                                  :frames-per-second 60))
    (setf *game* game)))

(defun running-p (&optional (game *game*))
  (and game
       (not (game-quit-requested-p game))
       (eq :open (luv:canvas-state (game-canvas game)))))

(defun stop (&optional (game *game*))
  "Close the window and release the GPU device."
  (when game
    (let ((canvas (game-canvas game)))
      (when (eq :open (luv:canvas-state canvas))
        (setf (luv:canvas-clock canvas) (luv:make-demand-clock)
              (luv:canvas-event-handler canvas) nil))
      (luv:close-canvas canvas)
      (luv:destroy (game-device game)))
    (when (eq game *game*)
      (setf *game* nil)))
  (values))

(defun run ()
  (let ((game (start)))
    (unwind-protect
         (loop while (running-p game) do (sleep 0.05))
      (stop game))))

(defun main ()
  "Entry point of the executable: play until Escape or the window closes."
  (luv:call-with-sdl-main-thread #'run)
  (uiop:quit 0))
