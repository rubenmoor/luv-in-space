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

(defun set-relative-pointer-mode (game enabled)
  (let ((canvas (game-canvas game)))
    (when (and canvas (eq :open (luv:canvas-state canvas)))
      (luv:set-canvas-relative-pointer-mode canvas enabled))))

;;; The key map (decision 29: Control scheme: key map, control modes,
;;; gamepad; decision 81): discrete commands, run at the press.

(defparameter *key-map*
  '((:f . :fine) (:z . :throttle-full) (:x . :throttle-cut)
    (:t . :rate-damping) (:r . :inertial-hold)
    (:|1| . :hold-prograde) (:|2| . :hold-retrograde)
    (:|3| . :hold-normal) (:|4| . :hold-anti-normal)
    (:|5| . :hold-radial-out) (:|6| . :hold-radial-in)
    (:|7| . :hold-target) (:|8| . :hold-anti-target)
    (:|9| . :hold-relative-prograde) (:|0| . :hold-relative-retrograde)
    (:|.| . :warp-up) (:|,| . :warp-down) (:/ . :warp-reset) (:p . :pause)
    (:c . :camera-frame) (:home . :camera-reset)
    (:m . :map) (:tab . :map-focus) (:page-up . :map-zoom-in) (:page-down . :map-zoom-out)
    (:g . :grapple) (:b . :transfer) (:y . :acknowledge)
    (:] . :target-next) (:[ . :target-previous) (:\\ . :target-clear)
    (:o . :autopilot-open) (:up . :autopilot-up) (:down . :autopilot-down)
    (:left . :autopilot-left) (:right . :autopilot-right) (:backspace . :autopilot-backspace)
    (:return . :restart) (:n . :new-game))
  "(key . command): luv's physical key names; run-command's commands.")

(defparameter *lost-commands* '(:restart :new-game)
  "Act only while lost; otherwise their keys are as if unbound, N translating (decision 86).")

(defparameter *flying-commands* '(:acknowledge)
  "Act at any time but lost (decision 113).")

(defparameter *target-commands* '(:target-next :target-previous :target-clear)
  "Act paused and at every level, not while lost or a flight runs (decision 114).")

(defparameter *autopilot-commands*
  '((:autopilot-open . :open) (:autopilot-up . :up) (:autopilot-down . :down)
    (:autopilot-left . :left) (:autopilot-right . :right) (:autopilot-return . :return)
    (:autopilot-backspace . :backspace))
  "(command . autopilot-key!'s): act paused and at every level, not while lost or
a flight runs; Return is :autopilot-return unless lost (decision 118).")

(defparameter *flight-commands*
  '(:fine :throttle-full :throttle-cut :rate-damping :inertial-hold
    :hold-prograde :hold-retrograde :hold-normal :hold-anti-normal
    :hold-radial-out :hold-radial-in :hold-target :hold-anti-target
    :hold-relative-prograde :hold-relative-retrograde :grapple :transfer)
  "Ignored while paused or lost (decision 81, decision 113); the rest always act (decision 88: Warp keys while paused).")

(defparameter *throttle-ramp-rate* 0.5d0 "Lever per real second, Shift-left / Control-left; decision 81.")
(defparameter *throttle-ramp-elapsed-cap* 0.1d0 "s: a frame's elapsed time the ramp uses at most; decision 81.")
(defparameter *throttle-up-key* :shift-left)
(defparameter *throttle-down-key* :control-left)
(defparameter *rcs-off-message* "RCS OFF ABOVE ×4" "decision 31: Warp choices within the time-warp rules.")
(defparameter *engine-locked-message* "ENGINE LOCKED: BERTHED" "decision 113.")

(defparameter *pilot-commands* (remove-if (lambda (c) (member c '(:grapple :transfer)))
                                          *flight-commands*)
  "Commands whose press, with the keys of *flight-keys* and the throttle
keys, disengages the autopilot (\"AUTOPILOT OFF: PILOT INPUT\"; decision 118).")

(defparameter *script-commands* '(:warp-up :warp-down :warp-reset :restart :new-game)
  "Ignored, with the flight commands, while a flight script runs: the script owns them (decision 87).")

(defun flight-keys-ignored-p (game)
  "Paused, lost or a flight running: flight keys do nothing (decision 81, decision 87)."
  (or (clock-paused (game-clock game)) (sim-lost-p (game-sim game)) (flight-running-p game)))

(defun run-command (game command)
  (let* ((sim (game-sim game))
         (clock (game-clock game))
         (controls (sim-controls sim)))
    (ecase command
      (:fine (controls-toggle-fine controls))
      (:throttle-full (controls-throttle-full controls))
      (:throttle-cut (controls-throttle-cut controls))
      (:rate-damping (controls-toggle-rate-damping controls))
      (:inertial-hold (controls-inertial-hold controls (ship-state-q (sim-ship sim))))
      (:hold-prograde (controls-set-hold controls :prograde))
      (:hold-retrograde (controls-set-hold controls :retrograde))
      (:hold-normal (controls-set-hold controls :normal))
      (:hold-anti-normal (controls-set-hold controls :anti-normal))
      (:hold-radial-out (controls-set-hold controls :radial-out))
      (:hold-radial-in (controls-set-hold controls :radial-in))
      (:hold-target (key-refusal! clock :target (sim-target-hold! sim :target)))
      (:hold-anti-target (key-refusal! clock :target (sim-target-hold! sim :anti-target)))
      (:hold-relative-prograde
       (key-refusal! clock :target (sim-target-hold! sim :relative-prograde)))
      (:hold-relative-retrograde
       (key-refusal! clock :target (sim-target-hold! sim :relative-retrograde)))
      (:warp-up (clock-warp-up clock sim))
      (:warp-down (clock-warp-down clock))
      (:warp-reset (clock-warp-reset clock))
      (:pause (clock-toggle-pause clock))
      (:camera-frame (unless (map-p game)
                       (outside-view-toggle-frame! (game-outside-view game) (game-render-state game))))
      (:camera-reset (if (map-p game)
                         (open-map-view! game)
                         (outside-view-reset! (game-outside-view game))))
      (:map (toggle-map! game))
      (:grapple (key-refusal! clock :grapple (grapple-key! sim)))
      (:transfer (key-refusal! clock :transfer (transfer-key! sim)))
      (:acknowledge (clock-acknowledge! clock))
      (:target-next (sim-cycle-target! sim 1))
      (:target-previous (sim-cycle-target! sim -1))
      (:target-clear (sim-set-target! sim nil))
      ((:autopilot-open :autopilot-up :autopilot-down :autopilot-left :autopilot-right
        :autopilot-return :autopilot-backspace)
       (autopilot-key! (clock-autopilot clock) clock sim
                       (cdr (assoc command *autopilot-commands*))))
      (:restart (game-return! game))
      (:new-game (game-new-game! game))
      (:map-focus (when (map-p game) (map-view-cycle-focus! (game-map-view game))))
      (:map-zoom-in (when (map-p game) (map-view-zoom! (game-map-view game) (/ *map-page-factor*))))
      (:map-zoom-out (when (map-p game) (map-view-zoom! (game-map-view game) *map-page-factor*))))))

(defun key-refusal! (clock key refusal)
  (when refusal
    (clock-message! clock key refusal)))

(defun map-p (game) (eq (game-view game) :map))

(defun open-map-view! (game)
  "Frame the map on the current state (decision 83: Map camera: framing on
opening, focus, zoom, drag and exposure)."
  (map-view-open! (game-map-view game)
                  (render-state-update! (game-render-state game) (game-clock game) (game-sim game))))

(defun toggle-map! (game)
  "M: :outside ↔ :map; opening frames the map, leaving keeps the outside
view as it was. The frame snaps the exposure (view-changed-p)."
  (if (map-p game)
      (setf (game-view game) :outside)
      (progn (open-map-view! game)
             (setf (game-view game) :map)))
  (setf (game-view-changed-p game) t))

(defun engine-key-p (key command)
  "KEY (its COMMAND) works the engine or the RCS: locked while berthed."
  (or (assoc key *flight-keys*)
      (eq command :throttle-full)
      (member key (list *throttle-up-key* *throttle-down-key*))))

(defun pilot-key-p (key command)
  "KEY (its COMMAND) is the pilot's: a rotation or translation key, a throttle
key or one of *pilot-commands*."
  (or (assoc key *flight-keys*)
      (member key (list *throttle-up-key* *throttle-down-key*))
      (member command *pilot-commands*)))

(defun key-pressed! (game key)
  "A new press of KEY: its command (Return and N only while lost; Return the
autopilot's otherwise), the locked engine's message for an engine or RCS key
while berthed, or the RCS-off message for a flight key above ×4. A pilot's
key that acts disengages an engaged autopilot first (decision 118)."
  (let ((command (cdr (assoc key *key-map*))))
    (when (and (eq command :restart) (not (sim-lost-p (game-sim game))))
      (setf command :autopilot-return))
    (when (and (member command *lost-commands*) (not (sim-lost-p (game-sim game))))
      (setf command nil))
    (when (and (assoc command *autopilot-commands*)
               (or (sim-lost-p (game-sim game)) (flight-running-p game)))
      (return-from key-pressed!))
    (when (and (sim-plan (game-sim game)) (pilot-key-p key command)
               (not (flight-keys-ignored-p game)))
      (autopilot-disengage! (game-clock game) (game-sim game) t))
    (when (and (member command *flying-commands*) (sim-lost-p (game-sim game)))
      (return-from key-pressed!))
    (when (and (member command *script-commands*) (flight-running-p game))
      (return-from key-pressed!))
    (when (and (member command *target-commands*)
               (or (sim-lost-p (game-sim game)) (flight-running-p game)))
      (return-from key-pressed!))
    (cond ((and (sim-berthed (game-sim game)) (engine-key-p key command))
           (unless (flight-keys-ignored-p game)
             (clock-message! (game-clock game) :engine-locked *engine-locked-message*)))
          ((and command (member command *flight-commands*))
           (unless (flight-keys-ignored-p game)
             (run-command game command)))
          (command (run-command game command))
          ((and (assoc key *flight-keys*)
                (not (flight-keys-ignored-p game))
                (clock-kinematic-p (game-clock game)))
           (clock-message! (game-clock game) :rcs-off *rcs-off-message*)))))

(defun window-title ()
  "The window's title: the main keys by QWERTY position (decision 81)."
  "Luv in Space · WASD QE turn · HN IK JL move · Shift/Ctrl throttle · Z/X full/cut · T damp · R 1–6 hold · 7–0 target holds · F fine · . , / warp · P pause · C/Home camera · M map · O autopilot · G grapple · B refuel · Y ack · [ ] \\ target · Esc quit")

;; Runs on the canvas thread: only flag the quit; STOP runs on the caller's.
(defun handle-event (game event)
  (let ((input (game-input game)))
    (typecase event
      (luv:canvas-key-press-event
       (let ((key (luv:canvas-key-event-key-name event)))
         (cond ((quit-key-p event) (setf (game-quit-requested-p game) t))
               ((luv:canvas-key-event-repeat-p event))
               (t (pushnew key (input-held input))
                  (pushnew key (input-pressed input))
                  (key-pressed! game key)))))
      (luv:canvas-key-release-event
       (let ((key (luv:canvas-key-event-key-name event)))
         (when (member key (input-held input))
           (setf (input-held input) (remove key (input-held input)))
           (pushnew key (input-released input)))))
      (luv:canvas-pointer-button-press-event
       (when (eq :right (luv:canvas-pointer-event-button event))
         (setf (input-drag input) t)
         (set-relative-pointer-mode game t)))
      (luv:canvas-pointer-button-release-event
       (when (and (eq :right (luv:canvas-pointer-event-button event)) (input-drag input))
         (setf (input-drag input) nil)
         (set-relative-pointer-mode game nil)))
      (luv:canvas-pointer-motion-event
       (when (input-drag input)
         (incf (input-pointer-dx input) (float (luv:canvas-pointer-event-delta-x event) 1d0))
         (incf (input-pointer-dy input) (float (luv:canvas-pointer-event-delta-y event) 1d0))))
      (luv:canvas-pointer-wheel-event
       (incf (input-wheel input) (float (luv:canvas-pointer-event-scroll-y event) 1d0)))
      (luv:canvas-window-focus-lost-event
       (input-clear! input)
       (set-relative-pointer-mode game nil))
      (luv:canvas-window-close-request-event
       (setf (game-quit-requested-p game) t)))))

(defun throttle-ramp! (controls keys elapsed)
  "Shift-left raises the lever, from off to 0.1; Control-left lowers it,
below 0.1 off (controls-set-throttle)."
  (let ((sign (- (if (member *throttle-up-key* keys) 1 0)
                 (if (member *throttle-down-key* keys) 1 0)))
        (throttle (controls-throttle controls)))
    (cond ((and (> sign 0) (= throttle 0d0))
           (controls-set-throttle controls *throttle-min*))
          ((/= sign 0)
           (controls-set-throttle
            controls (+ throttle (* sign *throttle-ramp-rate*
                                    (min (float elapsed 1d0) *throttle-ramp-elapsed-cap*))))))))

(defun pointer-frame! (game input)
  "The drag and wheel summed since the last frame to the current view
(decision 82; decision 83)."
  (let ((dx (input-pointer-dx input))
        (dy (input-pointer-dy input))
        (notches (input-wheel input)))
    (ecase (game-view game)
      (:outside
       (let ((view (game-outside-view game)))
         (unless (and (= dx 0d0) (= dy 0d0))
           (outside-view-drag! view dx dy))
         (unless (= notches 0d0)
           (outside-view-zoom! view notches (ship-state-m-p (sim-ship (game-sim game)))))))
      (:map
       (let ((view (game-map-view game)))
         (unless (and (= dx 0d0) (= dy 0d0))
           (map-view-drag! view dx dy))
         (unless (= notches 0d0)
           (map-view-zoom! view (expt *map-zoom-step* (- notches)))))))))

(defun input-frame! (game elapsed)
  "Once per frame, before clock-frame!: the held and tapped keys to the
sim's controls (none while paused, lost, berthed or above ×4; not written
while a flight runs, whose :keys own them, nor while the autopilot is
engaged, whose executor owns them) and the throttle ramp (not while paused,
lost, berthed, flying or engaged), a held Left or Right to the autopilot's
menu (autopilot-held!), the pointer to the view (always); then the latches
clear. ELAPSED: real seconds."
  (let* ((input (game-input game))
         (sim (game-sim game))
         (controls (sim-controls sim))
         (keys (input-frame-keys input))
         (engaged (and (sim-plan sim) t))
         (ignored (or (flight-keys-ignored-p game) (sim-berthed sim))))
    (unless (or (flight-running-p game) engaged)
      (controls-apply-held-keys! controls
                                 (if (or ignored (clock-kinematic-p (game-clock game))) '() keys)))
    (unless (or ignored engaged)
      (throttle-ramp! controls keys elapsed))
    (unless (or (sim-lost-p sim) (flight-running-p game))
      (let ((held (input-held input)))
        (autopilot-held! (clock-autopilot (game-clock game)) sim
                         (- (if (member :right held) 1 0) (if (member :left held) 1 0))
                         elapsed)))
    (pointer-frame! game input)
    (setf (input-pressed input) '()
          (input-released input) '()
          (input-pointer-dx input) 0d0
          (input-pointer-dy input) 0d0
          (input-wheel input) 0d0)
    input))
