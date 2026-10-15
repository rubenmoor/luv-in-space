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

(defun sim-step-ticks! (sim level until)
  "sim-step!, returning h in 1/64 s ticks (a fixnum: nothing boxed) or nil."
  (declare (optimize speed) (type sim sim) (type fixnum level)
           (type (or null double-float) until) (inline flight-step!))
  (when (sim-loss sim)
    (return-from sim-step-ticks! nil))
  (let* ((ship (sim-ship sim))
         (controls (sim-controls sim))
         (time (sim-t sim))
         (coarse (> level *fine-levels-max*))
         (cut-after nil))
    (declare (type double-float time))
    (flet ((engine-on-p ()
             (and (> (ship-engine-force (controls-throttle controls)) 0d0)
                  (> (ship-state-m-p ship) 0d0)))
           (cut! ()
             (controls-throttle-cut controls)
             (setf (sim-cutoff sim) nil)))
      (declare (inline engine-on-p cut!))
      (let ((cutoff (sim-cutoff sim)))
        (when (and cutoff (engine-on-p) (>= (ship-state-dv ship) cutoff))
          (cut!)))
      (when (and coarse (not (eq (controls-attitude-mode controls) :hold)))
        (controls-inertial-hold controls (ship-state-q ship)))
      (let ((h (if coarse
                   (coarse-step (ship-state-r ship) time (engine-on-p) (sim-body sim))
                   +fine-step+))
            (cutoff (sim-cutoff sim)))
        (declare (type (double-float 0d0 1d6) h))
        ;; A coarse burn ends at the engine-only closed form, rounded up.
        (when (and coarse cutoff (engine-on-p))
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
        (if coarse
            (let ((*hold-controls* controls))
              (flight-step! ship controls h time (sim-model sim) #'kinematic-hold-attitude))
            (flight-step! ship controls h time (sim-model sim)))
        ;; PLACEHOLDER(104): the crash test of hazards.lisp, from the step's start state.
        (setf (sim-t sim) (+ time h))
        (incf (sim-steps sim))
        (let ((cutoff (sim-cutoff sim)))
          (when (or cut-after (and cutoff (>= (ship-state-dv ship) cutoff)))
            (cut!)))
        (values (truncate (* h (/ +fine-step+))))))))

(declaim (inline sim-step!))
(defun sim-step! (sim level &key until)
  "One step of SIM at warp LEVEL (×1, ×4, ×10 …) with every rule (decision
72): the fine step to ×4, above it the coarse step with kinematic attitude
(an inertial hold engaged first if no hold is), shortened to land on UNTIL
(a grid time; one not ahead is ignored) and on the Δv cut-off, then
flight-step! with SIM's model. Returns h, or nil for a lost sim. Hot
callers declare it inline: a full call boxes h."
  (let ((ticks (sim-step-ticks! sim level until)))
    (and ticks (* ticks +fine-step+))))

;;; Checkpoints (docs/architecture.md "Checkpoints"); milestone 2 keeps one,
;;; the start state.

(defun take-checkpoint (sim) (copy-sim sim))

(defun restore-checkpoint! (sim checkpoint)
  "SIM becomes CHECKPOINT, consing nothing; SIM."
  (sim-assign! sim checkpoint))

(defun checkpoint-safe-p (sim)
  "Throttle off, not lost; PLACEHOLDER(105): no impact predicted."
  (and (= (controls-throttle (sim-controls sim)) 0d0)
       (null (sim-loss sim))))
