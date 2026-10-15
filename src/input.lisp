(in-package #:luv-in-space)

;;; Canvas events to the input state, and once per frame to the sim's
;;; controls (decision 81: Input in milestone 2: held keys per frame, taps,
;;; gating and the shared key map; docs/design.md "Controls"). Events only
;;; change the input state; input-frame! writes controls before the steps.

(defstruct input
  (held '() :type list)                 ; key names down now
  (pressed '() :type list)              ; pressed since the last frame
  (released '() :type list)             ; held keys released since the last frame
  (drag nil :type boolean)              ; right button down
  (pointer-dx 0d0 :type double-float)   ; summed since the last frame
  (pointer-dy 0d0 :type double-float)
  (wheel 0d0 :type double-float))       ; notches, up positive

(defun input-frame-keys (input)
  "The held keys plus each key pressed and released since the last frame
(the tap latch: held for one frame)."
  (let ((keys (input-held input)))
    (dolist (key (input-released input) keys)
      (when (member key (input-pressed input))
        (pushnew key keys)))))

(defun input-clear! (input)
  (setf (input-held input) '()
        (input-pressed input) '()
        (input-released input) '()
        (input-drag input) nil
        (input-pointer-dx input) 0d0
        (input-pointer-dy input) 0d0
        (input-wheel input) 0d0)
  input)

(defun quit-key-p (event)
  (let ((key (luv:canvas-key-event-key-name event)))
    (or (eq key :escape)
        (and (eq key :q) (member :control (luv:canvas-key-event-modifiers event))))))

(defun leave-relative-pointer-mode (game)
  (let ((canvas (game-canvas game)))
    (when (and canvas (eq :open (luv:canvas-state canvas)))
      (luv:set-canvas-relative-pointer-mode canvas nil))))

;; Runs on the canvas thread: only flag the quit; STOP runs on the caller's.
(defun handle-event (game event)
  (let ((input (game-input game)))
    (typecase event
      (luv:canvas-key-press-event
       (let ((key (luv:canvas-key-event-key-name event)))
         (cond ((quit-key-p event) (setf (game-quit-requested-p game) t))
               ((luv:canvas-key-event-repeat-p event))
               (t (pushnew key (input-held input))
                  (pushnew key (input-pressed input))))))
      (luv:canvas-key-release-event
       (let ((key (luv:canvas-key-event-key-name event)))
         (when (member key (input-held input))
           (setf (input-held input) (remove key (input-held input)))
           (pushnew key (input-released input)))))
      (luv:canvas-window-focus-lost-event
       (input-clear! input)
       (leave-relative-pointer-mode game))
      (luv:canvas-window-close-request-event
       (setf (game-quit-requested-p game) t)))))

(defun input-frame! (game elapsed)
  "Once per frame, before clock-frame!: the held and tapped keys to the
sim's controls; then the latches clear. ELAPSED: real seconds."
  (declare (ignore elapsed))
  (let ((input (game-input game)))
    (controls-apply-held-keys! (sim-controls (game-sim game)) (input-frame-keys input))
    (setf (input-pressed input) '()
          (input-released input) '()
          (input-pointer-dx input) 0d0
          (input-pointer-dy input) 0d0
          (input-wheel input) 0d0)
    input))
