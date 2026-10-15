(in-package #:luv-in-space)

;;; The game: the window, the sim and its clock, the camera and the frame
;;; (decision 77: The frame in main.lisp: render state, capture of a given
;;; sim, part 4's placeholders and first asset). The sim, the clock and the
;;; render state are touched only on the canvas thread.

(defclass game ()
  ((canvas :initarg :canvas :initform nil :reader game-canvas)
   (device :initarg :device :reader game-device)
   (context :initarg :context :reader game-context)
   (renderer :initarg :renderer :reader game-renderer)
   (sim :initarg :sim :reader game-sim)
   (clock :initarg :clock :reader game-clock)
   (start-checkpoint :initarg :start-checkpoint :reader game-start-checkpoint)
   (camera :initform (make-camera) :reader game-camera)
   (camera-fn :initform nil :accessor game-camera-fn
              :documentation "nil or (fn camera render-state), after the outside view's pose.")
   (outside-view :initform (new-outside-view) :reader game-outside-view
                 :documentation "The outside view's frame, angles and distance (camera.lisp); not in the sim.")
   (map-view :initform (new-map-view) :reader game-map-view
             :documentation "The map's focus, basis, angles and distance (camera.lisp); not in the sim.")
   (render-state :initform (make-render-state) :reader game-render-state)
   (exposure :initform nil :accessor game-exposure
             :documentation "The current exposure, nil before the first frame.")
   (frame-time :initform nil :accessor game-frame-time
               :documentation "luv's time of the last frame, nil before the first.")
   (frame-count :initform 0 :accessor game-frame-count)
   (input :initform (make-input) :reader game-input
          :documentation "Held keys, taps and the pointer (input.lisp); not in the sim.")
   (view :initform :outside :accessor game-view :documentation "The view: :outside.")
   (quit-requested-p :initform nil :accessor game-quit-requested-p)
   (stopped-p :initform nil :accessor game-stopped-p)))

(defvar *game* nil)

(defmethod luv:application-live-artifacts ((game game))
  (renderer-live-artifacts (game-renderer game)))

(defun failed-live-artifacts (&optional (game *game*))
  (and game
       (remove :failed (luv:application-live-artifacts game)
               :key #'luv:live-artifact-status :test-not #'eq)))

(defun report-failed-live-artifacts (&optional (game *game*) (stream *standard-output*))
  "Print each failed live artifact of GAME with its diagnostic; return their count.
A failed artifact keeps drawing its last good build."
  (let ((failed (failed-live-artifacts game)))
    (dolist (artifact failed)
      (format stream "Live artifact ~A FAILED (last good build still drawn):~%  ~A~%"
              (luv:live-artifact-label artifact)
              (luv:live-artifact-diagnostic artifact)))
    (length failed)))

(defun frame-elapsed! (game time)
  "Real seconds since the last frame by luv's frame TIME; 0 on the first."
  (let ((previous (game-frame-time game))
        (time (float time 1d0)))
    (setf (game-frame-time game) time)
    (if previous (max 0d0 (- time previous)) 0d0)))

(defun update-view! (game elapsed)
  "The render state from the clock and the sim, the outside view on it
(its distance eased over ELAPSED real seconds, snapped when nil), then the
game's camera-fn."
  (let ((state (game-render-state game))
        (camera (game-camera game)))
    (render-state-update! state (game-clock game) (game-sim game))
    (outside-camera-update! camera (game-outside-view game) state elapsed)
    (when (game-camera-fn game)
      (funcall (game-camera-fn game) camera state))))

(defun update-exposure! (game extent elapsed)
  "Ease the exposure toward the view's target over ELAPSED real seconds;
snap on the first frame and when ELAPSED is nil (decision 76)."
  (let ((target (exposure-target (game-camera game) (game-render-state game) :outside
                                 :aspect (/ (first extent) (second extent))))
        (current (game-exposure game)))
    (setf (game-exposure game)
          (if (and current elapsed) (exposure-ease current target elapsed) target))))

(defun encode-game-frame (game encoder target extent elapsed)
  "ELAPSED: real seconds since the last frame, nil to snap the exposure and
the outside view's distance."
  (update-view! game elapsed)
  (encode-frame (game-renderer game) encoder target extent
                (game-render-state game) (game-camera game)
                (update-exposure! game extent elapsed)))

(defun draw (game)
  (let ((context (game-context game)))
    (incf (game-frame-count game))
    ;; Rebuild pipelines whose shaders were redefined, before this frame uses them.
    (luv:refresh-application-live-artifacts game)
    (luv:present-canvas-frame
     context
     (lambda (texture encoder time)
       (let ((elapsed (frame-elapsed! game time))
             (extent (luv:canvas-extent context)))
         (note-swapchain-target (game-renderer game) texture extent)
         (input-frame! game elapsed)
         (clock-frame! (game-clock game) (game-sim game) elapsed)
         (unless (clock-paused (game-clock game))
           (incf (render-state-lights-t (game-render-state game)) elapsed))
         (encode-game-frame game encoder texture extent elapsed))))))

(defmethod luv:capture-canvas ((game game)) (game-canvas game))

;; The current state, not stepped; the exposure snaps.
(defmethod luv:encode-capture-frame ((game game) capture encoder target extent)
  (declare (ignore capture))
  (luv:refresh-application-live-artifacts game)
  (encode-game-frame game encoder target extent nil))

(defmethod luv:cleanup-capture ((game game) capture)
  (let ((target (luv:capture-target capture)))
    (when (and target (eq :open (luv:canvas-state (game-canvas game))))
      (call-on-canvas-thread game (lambda () (forget-target (game-renderer game) target))))))

(defun call-on-canvas-thread (game function)
  "Call FUNCTION on GAME's canvas thread, between frames; its first value."
  (let ((result nil))
    (luv:request-canvas-frame (game-canvas game)
                              (lambda (timestamp)
                                (declare (ignore timestamp))
                                (setf result (funcall function))))
    result))

(defun start (&key (visible t) (width 1280) (height 800) (sim (new-game-sim)) paused)
  "Open the game window on SIM (used as is, the start checkpoint taken of
it) and start drawing; PAUSED starts with the clock paused. Returns the game."
  (let* ((canvas (luv:make-sdl-canvas :title (window-title) :width width :height height
                                      :visible-p visible))
         (_ (luv:open-canvas canvas))
         (device (luv:request-gpu-device luv:*gpu-provider*
                                         (luv:make-device-descriptor :label "luv-in-space")))
         (context (luv:make-canvas-context canvas luv:*gpu-provider*
                                           (luv:make-canvas-configuration
                                            :device device
                                            :usage '(:render-attachment :copy-dst))))
         (clock (new-clock sim))
         (game (make-instance 'game :canvas canvas :device device :context context
                                    :renderer (make-frame-renderer device context)
                                    :sim sim :clock clock
                                    :start-checkpoint (take-checkpoint sim))))
    (declare (ignore _))
    (setf (clock-paused clock) (and paused t))
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

(defvar *stop-lock* (sb-thread:make-mutex :name "luv-in-space stop"))

(defun stop (&optional (game *game*))
  "Close the window and release the GPU device; again does nothing.
Not on the canvas thread."
  (sb-thread:with-mutex (*stop-lock*)
    (when game
      (stop-game game))))

(defun stop-game (game)
  (unless (game-stopped-p game)
    (setf (game-stopped-p game) t)
    (let ((canvas (game-canvas game)))
      (when (eq :open (luv:canvas-state canvas))
        (setf (luv:canvas-clock canvas) (luv:make-demand-clock)
              (luv:canvas-event-handler canvas) nil))
      (luv:close-canvas canvas)
      (destroy-frame-renderer (game-renderer game))
      (luv:destroy (game-device game))))
  (when (eq game *game*)
    (setf *game* nil))
  (values))

(defun play (&rest start-arguments)
  "START for the live image (./sly play): a watcher thread stops the game
once a quit is requested or the window closes, as RUN's loop does."
  (let ((game (apply #'start start-arguments)))
    (sb-thread:make-thread
     (lambda ()
       (loop while (and (eq game *game*) (running-p game)) do (sleep 0.05))
       (stop game))
     :name "luv-in-space quit watcher")
    game))

(defun run ()
  (let ((game (start)))
    (unwind-protect
         (loop while (running-p game) do (sleep 0.05))
      (stop game))))

(defun main ()
  "Entry point of the executable: play until Escape or the window closes."
  (luv:call-with-sdl-main-thread #'run)
  (uiop:quit 0))
