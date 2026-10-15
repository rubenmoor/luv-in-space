(in-package #:luv-in-space)

;;; Offscreen screenshots of a given state through luv's capture path
;;; (decision 2: Screenshots and GPU test: offscreen capture on a hidden
;;; canvas, headless on lavapipe; decision 77: The frame in main.lisp: render
;;; state, capture of a given sim, part 4's placeholders and first asset).

(defparameter *screenshot-frames* 10
  "Frames drawn before the capture; the game is frozen, so a few suffice.")

(defun screenshot (pathname &key (game *game*))
  "Write a PNG of GAME's scene at its canvas size; returns PATHNAME, width,
height, the packed pixels and their colour format."
  (multiple-value-bind (path pixels width height format)
      (luv:capture-application-screenshot game pathname :label "luv-in-space screenshot")
    (values path width height pixels format)))

(defun wait-for-frames (game count &key (timeout 60))
  "Wait until GAME has drawn COUNT more frames."
  (loop with target = (+ (game-frame-count game) count)
        with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        until (>= (game-frame-count game) target)
        do (when (> (get-internal-real-time) deadline)
             (error "Only ~D of ~D frames drawn in ~D s."
                    (- count (- target (game-frame-count game))) count timeout))
           (sleep 0.001)))

(defun game-load-state! (game sim)
  "GAME's sim becomes a copy of SIM, its clock restarted on it (×1,
unpaused); a frozen GAME (captures) takes no steps from it."
  (call-on-canvas-thread game (lambda ()
                                (clock-restart! (game-clock game) (game-sim game) sim))))

(defmacro with-capture-game ((game &key (width 1280) (height 800) sim (readout t)) &body body)
  "Run BODY with GAME on a hidden canvas of WIDTH x HEIGHT, frozen (no
steps, the clock not paused, so the readout reads WARP ×1) on a copy of
SIM (default the start state), the readout drawn unless READOUT is nil;
one canvas for several captures. Call on luv's SDL main thread
(LUV:CALL-WITH-SDL-MAIN-THREAD)."
  (let ((state (gensym "SIM")))
    `(let* ((,state ,sim)
            (,game (start :visible nil :width ,width :height ,height
                          :sim (if ,state (copy-sim ,state) (new-game-sim)) :frozen t
                          :readout ,readout)))
       (unwind-protect (progn ,@body)
         (stop ,game)))))

(defun game-set-view! (game view)
  "GAME in VIEW (:outside or :map), the map framed on the current state."
  (call-on-canvas-thread game (lambda ()
                                (when (eq view :map)
                                  (open-map-view! game))
                                (setf (game-view game) (ecase view ((:outside :map) view))
                                      (game-view-changed-p game) t))))

(defun capture-game-state (game pathname &key sim camera-fn (view :outside)
                                              (frames *screenshot-frames*)
                                              (readout (game-readout-p game))
                                              (death-elapsed 0d0))
  "Write a PNG of GAME showing SIM (default its current state) after FRAMES
frames in VIEW (:outside, or :map framed as on opening); CAMERA-FN (fn
camera render-state) sets a pose other than the view's; READOUT nil leaves
the readout out (default: as GAME draws it now). A lost sim shows the LOST
panel and the death glow DEATH-ELAPSED real seconds after the death
(default 0, the peak; decision 86). Returns as SCREENSHOT."
  (when sim
    (game-load-state! game sim))
  (setf (game-readout-p game) (and readout t)
        (game-death-elapsed game) (float death-elapsed 1d0))
  (game-set-view! game view)
  (setf (game-camera-fn game) camera-fn)
  (wait-for-frames game frames)
  (screenshot pathname :game game))

(defun capture-state (pathname sim &key camera-fn (view :outside) (frames *screenshot-frames*)
                                       (width 1280) (height 800) (readout t) (death-elapsed 0d0))
  "A PNG showing exactly SIM's state (a copy, the game frozen) on a hidden
canvas in VIEW (:outside or :map), with the readout unless READOUT is nil
(decision 85); a lost SIM with the LOST panel and the glow DEATH-ELAPSED
real seconds after the death (decision 86). Call on luv's SDL main thread."
  (with-capture-game (game :width width :height height :sim sim :readout readout)
    (capture-game-state game pathname :camera-fn camera-fn :view view :frames frames
                                      :death-elapsed death-elapsed)))

(defun capture-scene (pathname &key (frames *screenshot-frames*) (width 1280) (height 800))
  "The start state from the outside view: scripts/screenshot's picture."
  (capture-state pathname (new-game-sim) :frames frames :width width :height height))

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

;;; scripts/fly (decision 87: Flight scripts in milestone 2: additions, apsis
;;; conditions, live fly and scripts/fly): one PNG per :screenshot checkpoint.

(defun fly-screenshot-pathname (out-dir n label)
  (merge-pathnames (format nil "~2,'0D-~A.png" n
                           (substitute-if #\- (lambda (c) (not (or (alphanumericp c) (char= c #\-))))
                                          (string-downcase label)))
                   (uiop:ensure-directory-pathname out-dir)))

(defun fly-screenshots (record out-dir &key game (frames *screenshot-frames*)
                                            (width 1280) (height 800))
  "Write <OUT-DIR>/<nn>-<label>.png for each :screenshot entry of RECORD
(run-flight's), nn from 01: its sim in its view, the default outside pose,
the readout, a lost sim at the glow's peak; on GAME, else on one hidden
canvas of WIDTH x HEIGHT. Creates OUT-DIR; returns the pathnames and
their pixels (as SCREENSHOT's). Call on
luv's SDL main thread."
  (let ((shots (remove :screenshot record :key #'flight-entry-kind :test-not #'eq)))
    (ensure-directories-exist (uiop:ensure-directory-pathname out-dir))
    (flet ((capture-all (game)
             (loop for entry in shots
                   for n from 1
                   for path = (fly-screenshot-pathname out-dir n (flight-entry-label entry))
                   do (call-on-canvas-thread
                       game (lambda () (outside-view-reset! (game-outside-view game))))
                   collect path into paths
                   collect (nth-value 3 (capture-game-state
                                         game path :sim (flight-entry-sim entry)
                                                   :view (or (flight-entry-view entry) :outside)
                                                   :frames frames :readout t :death-elapsed 0d0))
                     into pixels
                   finally (return (values paths pixels)))))
      (if game
          (capture-all game)
          (with-capture-game (game :width width :height height)
            (capture-all game))))))

(defun fly-main (name out-dir)
  "Entry point of scripts/fly: run-flight of flights/NAME.sexp, its record
lines on stdout, the PNGs in OUT-DIR; exit 0, or 1 on a failure, a failed
expectation, a missing script or an error."
  (handler-case
      (let ((record (run-flight name)))
        (format t "~{~A~%~}" (flight-record-lines record))
        (finish-output)
        (dolist (path (luv:call-with-sdl-main-thread
                       (lambda () (fly-screenshots record out-dir))))
          (format t "~A~%" (uiop:native-namestring path)))
        (finish-output)
        (uiop:quit (if (flight-record-passed-p record) 0 1)))
    (error (condition)
      (format *error-output* "~&fly failed: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
