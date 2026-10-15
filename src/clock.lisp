(in-package #:luv-in-space)

;;; The simulation state, the start state, the one step function and
;;; checkpoints (docs/architecture.md "The clock", "Integrator",
;;; "Checkpoints"; decision 72: Simulation state and the one step function
;;; in clock.lisp, decision 73: Warp levels above x4: entry, holds and
;;; lowering).

;;; The simulation state

(defstruct (sim (:constructor %make-sim (ship controls))
                (:copier nil))
  (t 0d0 :type double-float)                    ; s, on the 1/64 s grid
  (body :earth :type keyword)                   ; the central body
  (ship nil :type ship-state)
  (controls nil :type controls)
  (model :full :type keyword)                   ; gravity-into!'s
  (cutoff nil :type (or null double-float))     ; odometer value that cuts the throttle
  (loss nil)                                    ; the loss record, immutable
  (steps 0 :type fixnum))

(defun copy-sim (sim)
  "A deep copy: equalp to SIM, sharing no array."
  (sim-assign! (%make-sim (copy-ship-state (sim-ship sim)) (copy-controls (sim-controls sim)))
               sim))

(defun sim-assign! (out sim)
  "Copy SIM into OUT, consing nothing; OUT."
  (declare (type sim out sim))
  (ship-state-assign! (sim-ship out) (sim-ship sim))
  (controls-assign! (sim-controls out) (sim-controls sim))
  (setf (sim-t out) (sim-t sim)
        (sim-body out) (sim-body sim)
        (sim-model out) (sim-model sim)
        (sim-cutoff out) (sim-cutoff sim)
        (sim-loss out) (sim-loss sim)
        (sim-steps out) (sim-steps sim))
  out)

;;; The start (decision 38: Start orbit: polar, docs/design.md "The start")

(defparameter *start-orbit* '(:a 6778136.6d0 :e 0d0 :i 90d0 :raan 270d0 :u 0d0)
  "m and degrees at (start-t), decision 38.")

(defun new-game-sim ()
  "The start state: the polar 400 km orbit at (start-t), the prograde hold's
attitude and frame rate, settled, full tanks, throttle off."
  (destructuring-bind (&key a e i raan u) *start-orbit*
    (let ((time (start-t)))
      (multiple-value-bind (r v)
          (orbit-state (make-elliptic-orbit +mu-earth+ a e (deg->rad i) (deg->rad raan)
                                            0d0 (deg->rad u) time)
                       time)
        (multiple-value-bind (q w-t) (hold-attitude :prograde r v 0d0)
          (let ((controls (new-controls)))
            (setf (controls-settled controls) t)
            (let ((sim (%make-sim (make-ship-state r v q (quat-rotate (quat-conjugate q) w-t)
                                                   *propellant-full*)
                                  controls)))
              (setf (sim-t sim) time)
              sim)))))))

;;; The step (decision 72)

(defparameter *fine-levels-max* 4
  "The highest warp level with the fine step and attitude dynamics, decision 19: Time-warp rules.")
(declaim (type fixnum *fine-levels-max*))

(defvar *hold-controls*)

(defun kinematic-hold-attitude (q w-t time-cell r v)
  "rk4-step!'s attitude function for the hold of *hold-controls*: as
controls-attitude-fn's, without a closure per step."
  (declare (optimize speed) (ignore time-cell) (type quat q) (type vec3 w-t r v))
  (let ((controls *hold-controls*))
    (declare (type controls controls))
    (hold-attitude-into! q w-t (controls-hold controls) r v
                         (controls-roll-offset controls) (controls-inertial-target controls))))

(declaim (inline grid-ceiling-step))
(defun grid-ceiling-step (h)
  "h rounded up to a multiple of +fine-step+."
  (declare (type (double-float 0d0 1d6) h))
  (* +fine-step+ (fceiling (* h (/ +fine-step+)))))

(declaim (inline step-h))
(defun step-h (sim level until)
  "The h of SIM's next step at LEVEL with UNTIL, and whether the Δv cut-off
ends it; changes nothing (a cut due before the step counts as made)."
  (declare (type sim sim) (type fixnum level) (type (or null double-float) until))
  (let* ((ship (sim-ship sim))
         (controls (sim-controls sim))
         (time (sim-t sim))
         (coarse (> level *fine-levels-max*))
         (cutoff (sim-cutoff sim))
         (engine-on (and (> (ship-engine-force (controls-throttle controls)) 0d0)
                         (> (ship-state-m-p ship) 0d0)))
         (cut-after nil))
    (declare (type double-float time))
    (when (and cutoff engine-on (>= (ship-state-dv ship) cutoff))
      (setf cutoff nil engine-on nil))
    (let ((h (if coarse
                 (coarse-step (ship-state-r ship) time engine-on (sim-body sim))
                 +fine-step+)))
      (declare (type (double-float 0d0 1d6) h))
      ;; A coarse burn ends at the engine-only closed form, rounded up.
      (when (and coarse cutoff engine-on)
        (let* ((force (ship-engine-force (controls-throttle controls)))
               (mdot (ship-engine-mdot (controls-throttle controls)))
               (m0 (+ *ship-dry-mass* (ship-state-m-p ship)))
               (h-cut (grid-ceiling-step
                       (min 1d6 (* (/ m0 mdot)
                                   (- 1 (exp (- (/ (* (- cutoff (ship-state-dv ship)) mdot)
                                                   force)))))))))
          (declare (type double-float force mdot m0))
          (when (<= h-cut h)
            (setf h h-cut cut-after t))))
      (when (and until (> until time) (< (- until time) h))
        (setf h (- until time) cut-after nil))
      (values h cut-after))))

(defun sim-step-ticks! (sim level until)
  "sim-step!, returning h in 1/64 s ticks (a fixnum: nothing boxed) or nil."
  (declare (optimize speed) (type sim sim) (type fixnum level)
           (type (or null double-float) until) (inline flight-step! step-h))
  (when (sim-loss sim)
    (return-from sim-step-ticks! nil))
  (let* ((ship (sim-ship sim))
         (controls (sim-controls sim))
         (time (sim-t sim))
         (coarse (> level *fine-levels-max*))
         (r0 (make-vec3)) (v0 (make-vec3)) (q0 (make-quat)))
    (declare (type double-float time) (dynamic-extent r0 v0 q0))
    (flet ((cut! ()
             (controls-throttle-cut controls)
             (setf (sim-cutoff sim) nil)))
      (declare (inline cut!))
      (let ((cutoff (sim-cutoff sim)))
        (when (and cutoff
                   (> (ship-engine-force (controls-throttle controls)) 0d0)
                   (> (ship-state-m-p ship) 0d0)
                   (>= (ship-state-dv ship) cutoff))
          (cut!)))
      (when (and coarse (not (eq (controls-attitude-mode controls) :hold)))
        (controls-inertial-hold controls (ship-state-q ship)))
      (multiple-value-bind (h cut-after) (step-h sim level until)
        (declare (type (double-float 0d0 1d6) h))
        (replace r0 (ship-state-r ship))
        (replace v0 (ship-state-v ship))
        (replace q0 (ship-state-q ship))
        (if coarse
            (let ((*hold-controls* controls))
              (flight-step! ship controls h time (sim-model sim) #'kinematic-hold-attitude))
            (flight-step! ship controls h time (sim-model sim)))
        (setf (sim-t sim) (+ time h))
        (incf (sim-steps sim))
        (let ((cutoff (sim-cutoff sim)))
          (when (or cut-after (and cutoff (>= (ship-state-dv ship) cutoff)))
            (cut!)))
        (let ((ticks (truncate (* h (/ +fine-step+)))))
          (crash-test! sim r0 v0 q0 ticks)
          ticks)))))

(declaim (inline sim-step!))
(defun sim-step! (sim level &key until)
  "One step of SIM at warp LEVEL (×1, ×4, ×10 …) with every rule (decision
72): the fine step to ×4, above it the coarse step with kinematic attitude
(an inertial hold engaged first if no hold is), shortened to land on UNTIL
(a grid time; one not ahead is ignored) and on the Δv cut-off, then
flight-step! with SIM's model, then the crash test (a crash leaves SIM
lost at the impact, hazards.lisp). Returns h, or nil for a lost sim. Hot
callers declare it inline: a full call boxes h."
  (let ((ticks (sim-step-ticks! sim level until)))
    (and ticks (* ticks +fine-step+))))

;;; Checkpoints (docs/architecture.md "Checkpoints"); milestone 2 keeps one,
;;; the start state.

(defun take-checkpoint (sim) (copy-sim sim))

(defun restore-checkpoint! (sim checkpoint)
  "SIM becomes CHECKPOINT, consing nothing; SIM."
  (sim-assign! sim checkpoint))

(defun checkpoint-safe-p (sim &optional prediction)
  "Throttle off, not lost, no impact predicted (PREDICTION, the clock's, run
to its horizon; without it a fresh one, prediction.lisp)."
  (and (= (controls-throttle (sim-controls sim)) 0d0)
       (null (sim-loss sim))
       (not (prediction-impact-p sim prediction))))

;;; The clock: frames, warp levels, pause, the actual rate (docs/architecture.md
;;; "The clock", "Time warp"; decision 72, decision 19: Time-warp rules). Not
;;; part of the simulation state; checkpoints don't copy it.

(defparameter *warp-levels* '(1 4 10 100 1000 10000 100000 1000000)
  "In order; decision 19: Time-warp rules.")
(defparameter *frame-step-limit* 4096 "sim-step!s per frame at most; decision 16, decision 72.")
(defparameter *frame-step-time-limit* 0.004d0
  "s of wall clock per frame's steps, checked every 64 steps; nil: no limit
(tests). Decision 72.")
(defparameter *frame-elapsed-cap* 0.1d0 "s: a frame's elapsed time at most; decision 16.")
(defparameter *rate-meter-frames* 256 "Frames the rate meter keeps, more than a second's.")
(declaim (type fixnum *frame-step-limit* *rate-meter-frames*)
         (type (or null double-float) *frame-step-time-limit*)
         (type double-float *frame-elapsed-cap*))

(defconstant +time-check-interval+ 64)

(defstruct (clock (:constructor %make-clock (previous prediction rate-real rate-game))
                  (:copier nil))
  (level-index 0 :type fixnum)                  ; into *warp-levels*
  (accumulator 0d0 :type double-float)          ; game s owed to the sim
  (paused nil)
  (real-time 0d0 :type double-float)            ; summed elapsed: messages' stamps
  (rate-real nil :type (simple-array double-float (*))) ; per frame, a ring
  (rate-game nil :type (simple-array double-float (*)))
  (rate-index 0 :type fixnum)                   ; the next ring slot
  (message-list nil)                            ; (key text stamp), oldest first
  (held nil)                                    ; the last step coarse under a hold
  (previous nil :type ship-state)               ; the ship before the last step
  (previous-t 0d0 :type double-float)
  (frame-steps 0 :type fixnum)                  ; steps in the last frame
  (hook nil :type (or null function))           ; (fn clock sim) → a grid until or nil
  (prediction nil))

(defun new-clock (sim)
  "A clock at ×1, unpaused, its previous state SIM's."
  (let ((clock (%make-clock (copy-ship-state (sim-ship sim)) (new-prediction)
                            (make-array *rate-meter-frames* :element-type 'double-float
                                                            :initial-element 0d0)
                            (make-array *rate-meter-frames* :element-type 'double-float
                                                            :initial-element 0d0))))
    (setf (clock-previous-t clock) (sim-t sim))
    clock))

(defun clock-level (clock)
  "The warp level, ×1 … ×1,000,000."
  (the fixnum (nth (clock-level-index clock) *warp-levels*)))

(defun clock-kinematic-p (clock)
  "Above ×4: attitude kinematic, rotation and translation keys ignored (decision 31)."
  (> (clock-level clock) *fine-levels-max*))

(defun clock-previous-state (clock)
  "(values ship-state t) before the last step, for interpolation."
  (values (clock-previous clock) (clock-previous-t clock)))

;;; Messages (decision 31, decision 73): text and a real-time stamp from the
;;; summed elapsed; a new one replaces the live one of its key.

(defparameter *message-time* 3d0 "s of real time a message shows; decision 31.")
(declaim (type double-float *message-time*))

(defun clock-message! (clock key text)
  (setf (clock-message-list clock)
        (append (remove key (clock-message-list clock) :key #'first)
                (list (list key text (clock-real-time clock))))))

(defun clock-messages (clock)
  "The live messages' texts, oldest first."
  (setf (clock-message-list clock)
        (remove-if (lambda (m) (>= (- (clock-real-time clock) (third m)) *message-time*))
                   (clock-message-list clock)))
  (mapcar #'second (clock-message-list clock)))

;;; Warp rules (decision 19: Time-warp rules, decision 31, decision 73)

(defparameter *thrust-warp-limit* 10 "Manual throttle's highest level; decision 31.")
(defparameter *cutoff-warp-limit* 100 "With a Δv cut-off armed; decision 31.")
(defparameter *warp-rotation-limit* 0.01d0
  "rad/s: above ×4 without a hold, |ω| below it; decision 19.")
(defparameter *dropout-real-time* 3d0
  "s of real time an event stays ahead at the level; decision 19, decision 73.")
(declaim (type fixnum *thrust-warp-limit* *cutoff-warp-limit*)
         (type double-float *warp-rotation-limit* *dropout-real-time*))

(defun sim-engine-on-p (sim)
  "Thrust on: the throttle up with propellant left."
  (and (> (ship-engine-force (controls-throttle (sim-controls sim))) 0d0)
       (> (ship-state-m-p (sim-ship sim)) 0d0)))

(defun thrust-warp-limit (sim)
  (if (sim-cutoff sim) *cutoff-warp-limit* *thrust-warp-limit*))

(defun held-p (sim) (eq (controls-attitude-mode (sim-controls sim)) :hold))

(defun clock-dropout (clock sim)
  "(values Δ kind loss) to the prediction's earliest drop-out event, or nil."
  (multiple-value-bind (time kind) (prediction-dropout-event (clock-prediction clock))
    (when time
      (values (max 0d0 (- time (sim-t sim))) kind
              (nth-value 1 (prediction-event (clock-prediction clock) kind))))))

(defun warp-refusal (clock sim level)
  "The message refusing a warp up to LEVEL, or nil (decision 19, decision 73)."
  (multiple-value-bind (delta kind) (clock-dropout clock sim)
    (cond ((and (sim-engine-on-p sim) (> level (thrust-warp-limit sim)))
           (format nil "WARP REFUSED: ENGINE ON, LIMIT ×~:D" (thrust-warp-limit sim)))
          ((and (> level *fine-levels-max*) (not (held-p sim))
                (>= (vec3-norm (ship-state-w (sim-ship sim))) *warp-rotation-limit*))
           "WARP REFUSED: ROTATING, SET A HOLD")
          ((and delta (< delta (* *dropout-real-time* level)))
           (format nil "WARP REFUSED: ~A IN ~A" kind (format-duration delta))))))

(defun dropout-message (level delta loss)
  "decision 73's text for a drop-out to LEVEL, Δ ahead of LOSS's impact."
  (if (and loss (eq (loss-body loss) :moon))
      (format nil "WARP ×~:D: MOON IMPACT AHEAD" level)
      (let ((body (body (if loss (loss-body loss) :earth))))
        (format nil "WARP ×~:D: PERIAPSIS BELOW ~D KM IN ~A" level
                (round (- (body-crash-radius body) (body-radius body)) 1000)
                (format-duration delta)))))

(defun clock-check-level! (clock sim)
  "Before every step (decision 73): the thrust limit, ×4 for a hold released
above ×4, the highest level with the drop-out event ≥ 3 s of real time
ahead; each lowering writes its message."
  (flet ((lower! (level text)
           (when (< level (clock-level clock))
             (clock-set-level-index! clock (position level *warp-levels*))
             (clock-message! clock :lowered text))))
    (when (and (sim-engine-on-p sim) (> (clock-level clock) (thrust-warp-limit sim)))
      (lower! (thrust-warp-limit sim)
              (format nil "WARP ×~:D: ENGINE ON" (thrust-warp-limit sim))))
    (when (and (clock-held clock) (not (held-p sim)) (> (clock-level clock) *fine-levels-max*))
      (lower! *fine-levels-max* (format nil "WARP ×~D: HOLD RELEASED" *fine-levels-max*)))
    (multiple-value-bind (delta kind loss) (clock-dropout clock sim)
      (declare (ignore kind))
      (when (and delta (< delta (* *dropout-real-time* (clock-level clock))))
        (let ((level (or (find-if (lambda (l) (>= delta (* *dropout-real-time* l)))
                                  *warp-levels* :from-end t)
                         1)))
          (lower! level (dropout-message level delta loss)))))))

;;; Commands: between frames, so at a step boundary; never touching the sim.

(defun clock-set-level-index! (clock index)
  "Warp level INDEX; the accumulator scales by new/old, so the real time
owed stays."
  (let ((old (clock-level clock)))
    (setf (clock-level-index clock) index)
    (setf (clock-accumulator clock)
          (* (clock-accumulator clock) (/ (float (clock-level clock) 1d0) old)))
    clock))

(defun clock-warp-up (clock sim)
  "One level up unless refused (decision 19, decision 73): then the level
stays and the refusal is a message. True when raised. Entering above ×4
with no hold, the next step engages an inertial hold (sim-step!)."
  (let ((index (1+ (clock-level-index clock))))
    (when (< index (length *warp-levels*))
      (let* ((level (nth index *warp-levels*))
             (refusal (warp-refusal clock sim level)))
        (cond (refusal
               (clock-message! clock :refused refusal)
               nil)
              (t
               (when (<= (clock-level clock) *fine-levels-max*)
                 (setf (clock-held clock) nil))
               (clock-set-level-index! clock index)
               t))))))

(defun clock-warp-down (clock)
  (clock-set-level-index! clock (max (1- (clock-level-index clock)) 0)))

(defun clock-warp-reset (clock)
  "×1, never refused."
  (clock-set-level-index! clock 0))

(defun clock-toggle-pause (clock)
  "Pause or resume; the level is kept."
  (setf (clock-paused clock) (not (clock-paused clock)))
  clock)

;;; The frame

(defun clock-steps! (clock sim)
  "Steps while the accumulator holds the next h, within the frame's caps;
the rest dropped. The step count."
  (declare (optimize speed) (type clock clock) (type sim sim)
           (inline step-h sim-step-ticks!))
  (let ((steps 0)
        (deadline (and *frame-step-time-limit*
                       (+ (get-internal-real-time)
                          (the fixnum (ceiling (* *frame-step-time-limit*
                                                  internal-time-units-per-second))))))
        (previous (clock-previous clock))
        (hook (clock-hook clock)))
    (declare (type fixnum steps) (type (or null fixnum) deadline))
    (flet ((drop! ()
             (setf (clock-accumulator clock) 0d0)
             (return-from clock-steps! steps)))
      (loop
        (when (or (sim-loss sim)
                  (>= steps *frame-step-limit*)
                  (and deadline (> steps 0) (zerop (mod steps +time-check-interval+))
                       (> (get-internal-real-time) deadline)))
          (drop!))
        (clock-check-level! clock sim)
        (let* ((level (clock-level clock))
               (until (and hook (funcall hook clock sim))))
          (declare (type (or null double-float) until))
          (when (< (clock-accumulator clock) (the double-float (step-h sim level until)))
            (return steps))
          (ship-state-assign! previous (sim-ship sim))
          (setf (clock-previous-t clock) (sim-t sim))
          (let ((ticks (sim-step-ticks! sim level until)))
            (decf (clock-accumulator clock) (* (the fixnum ticks) +fine-step+))
            (setf (clock-held clock) (and (> level *fine-levels-max*) (held-p sim)))
            (incf steps)))))))

(defun rate-meter-record! (clock real game)
  (let ((i (clock-rate-index clock)))
    (setf (aref (clock-rate-real clock) i) real
          (aref (clock-rate-game clock) i) game
          (clock-rate-index clock) (mod (1+ i) *rate-meter-frames*))))

(defun clock-actual-rate (clock)
  "Game seconds per real second over the frames of the last second; 0d0
before any frame."
  (let ((real 0d0) (game 0d0) (n *rate-meter-frames*))
    (declare (type double-float real game))
    (loop for k from 1 to n
          for i = (mod (- (clock-rate-index clock) k) n)
          while (< real 1d0)
          do (incf real (aref (clock-rate-real clock) i))
             (incf game (aref (clock-rate-game clock) i)))
    (if (> real 0d0) (/ game real) 0d0)))

(defun clock-frame! (clock sim elapsed)
  "One frame of ELAPSED real seconds: the prediction updated, then the
accumulator fed min(ELAPSED, 0.1 s) × level and stepped down (the hook
before each step gives its until); none paused (the accumulator emptied)
or lost. The steps taken."
  (let ((elapsed (float elapsed 1d0))
        (start (sim-t sim)))
    (prediction-update! (clock-prediction clock) sim)
    (incf (clock-real-time clock) elapsed)
    (setf (clock-frame-steps clock)
          (cond ((or (clock-paused clock) (sim-loss sim))
                 (setf (clock-accumulator clock) 0d0)
                 0)
                (t
                 (incf (clock-accumulator clock)
                       (* (min elapsed *frame-elapsed-cap*) (clock-level clock)))
                 (clock-steps! clock sim))))
    (rate-meter-record! clock elapsed (- (sim-t sim) start))
    (clock-frame-steps clock)))

;;; Restart (decision 34: How death works): the LOST panel's Return and N.

(defun clock-restart! (clock sim checkpoint)
  "SIM restored to CHECKPOINT; CLOCK at ×1, unpaused, its accumulator,
messages and prediction cleared. SIM."
  (restore-checkpoint! sim checkpoint)
  (setf (clock-level-index clock) 0
        (clock-paused clock) nil
        (clock-accumulator clock) 0d0
        (clock-message-list clock) nil
        (clock-held clock) nil
        (clock-previous-t clock) (sim-t sim))
  (ship-state-assign! (clock-previous clock) (sim-ship sim))
  (prediction-invalidate! (clock-prediction clock))
  sim)
