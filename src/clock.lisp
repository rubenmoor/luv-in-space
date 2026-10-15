(in-package #:luv-in-space)

;;; The simulation state, the start state, the one step function and
;;; checkpoints (docs/architecture.md "The clock", "Integrator",
;;; "Checkpoints"; decision 72: Simulation state and the one step function
;;; in clock.lisp, decision 73: Warp levels above x4: entry, holds and
;;; lowering).

;;; The simulation state

;;; The grapple (decision 112: Grapple, stack and berth in the sim: state,
;;; notices to the clock, the latest checkpoint).
(defstruct (grapple (:constructor make-grapple ()) (:copier nil))
  (state :stowed :type (member :stowed :armed :closing :latched))
  (object nil :type (or null fixnum))           ; the attached object's index
  (fixture nil :type (or null fixnum))          ; its fixture's index
  ;; Berthed: R in the station's axes from its CoM, and the ship's axes ->
  ;; the station's.
  (com (make-vec3) :type vec3 :read-only t)     ; m, its CoM in body axes from R
  (q (quat 1 0 0 0) :type quat :read-only t)    ; its body axes -> the ship's
  (closing-end 0d0 :type double-float))         ; s, :closing until then

(defun grapple-assign! (out grapple)
  (declare (type grapple out grapple))
  (setf (grapple-state out) (grapple-state grapple)
        (grapple-object out) (grapple-object grapple)
        (grapple-fixture out) (grapple-fixture grapple)
        (grapple-closing-end out) (grapple-closing-end grapple))
  (replace (grapple-com out) (grapple-com grapple))
  (replace (grapple-q out) (grapple-q grapple))
  out)

(defstruct (sim (:constructor %make-sim (ship controls objects grapple))
                (:copier nil))
  (t 0d0 :type double-float)                    ; s, on the 1/64 s grid
  (body :earth :type keyword)                   ; the central body
  (ship nil :type ship-state)
  (controls nil :type controls)
  (model :full :type keyword)                   ; gravity-into!'s
  (cutoff nil :type (or null double-float))     ; odometer value that cuts the throttle
  (loss nil)                                    ; the loss record, immutable
  (steps 0 :type fixnum)
  (objects #() :type simple-vector)             ; objects, in *objects*' order
  (grapple nil :type grapple)
  (berthed nil :type boolean)                   ; at the grapple's :berth fixture
  (transfer nil :type boolean)                  ; B: propellant flowing in
  (excluded nil :type (or null fixnum))         ; a released object's index, no contact
  (delivery nil :type list)                     ; nil or (load receiver fixture release-t), immutable
  (notices nil :type list)                      ; ((key text) …) of this step, oldest first
  (checkpoint-request nil :type boolean)        ; the clock takes a checkpoint when safe
  (target nil :type (or null keyword))          ; a place's or a towable object's key
  ;; The engaged burn plan, shared by copies, and its cursor (decision 115).
  (plan nil :type (or null burn-plan))
  (plan-index 0 :type fixnum)                   ; the current burn
  (plan-phase :done :type (member :pending :holding :burning :replan :done))
  (plan-rcs-target 0d0 :type double-float)      ; the RCS burn's odometer target
  ;; The autopilot's (decision 117): attitude cuts so far, replans so far,
  ;; the Δv a burn cut for its attitude resumes with (0: none), and the cut
  ;; that set :replan (plan-index the burn cut).
  (plan-retries 0 :type fixnum)
  (plan-replans 0 :type fixnum)
  (plan-resume 0d0 :type double-float)
  (plan-replan-t 0d0 :type double-float))

(defun copy-sim (sim)
  "A deep copy: equalp to SIM, sharing no array (the objects' definitions
and immutable parts shared)."
  (sim-assign! (%make-sim (copy-ship-state (sim-ship sim)) (copy-controls (sim-controls sim))
                          (map 'simple-vector #'copy-object (sim-objects sim))
                          (make-grapple))
               sim))

(defun sim-assign! (out sim)
  "Copy SIM into OUT, a sim with the same objects' definitions, consing
nothing; OUT."
  (declare (type sim out sim))
  (ship-state-assign! (sim-ship out) (sim-ship sim))
  (controls-assign! (sim-controls out) (sim-controls sim))
  (let ((out-objects (sim-objects out)) (objects (sim-objects sim)))
    (assert (= (length out-objects) (length objects)))
    (loop for o across out-objects
          for object across objects
          do (object-assign! o object)))
  (grapple-assign! (sim-grapple out) (sim-grapple sim))
  (setf (sim-t out) (sim-t sim)
        (sim-body out) (sim-body sim)
        (sim-model out) (sim-model sim)
        (sim-cutoff out) (sim-cutoff sim)
        (sim-loss out) (sim-loss sim)
        (sim-steps out) (sim-steps sim)
        (sim-berthed out) (sim-berthed sim)
        (sim-transfer out) (sim-transfer sim)
        (sim-excluded out) (sim-excluded sim)
        (sim-delivery out) (sim-delivery sim)
        (sim-notices out) (sim-notices sim)
        (sim-checkpoint-request out) (sim-checkpoint-request sim)
        (sim-target out) (sim-target sim)
        (sim-plan out) (sim-plan sim)
        (sim-plan-index out) (sim-plan-index sim)
        (sim-plan-phase out) (sim-plan-phase sim)
        (sim-plan-rcs-target out) (sim-plan-rcs-target sim)
        (sim-plan-retries out) (sim-plan-retries sim)
        (sim-plan-replans out) (sim-plan-replans sim)
        (sim-plan-resume out) (sim-plan-resume sim)
        (sim-plan-replan-t out) (sim-plan-replan-t sim))
  out)

(defparameter *delivery-wait* 600d0 "s from a delivery's release to the removal; decision 53.")
(declaim (type double-float *delivery-wait*))

(declaim (inline sim-delivery-due))
(defun sim-delivery-due (sim)
  "The pending delivery's due time, release + *delivery-wait* (on the grid), or nil."
  (let ((delivery (sim-delivery sim)))
    (and delivery (+ (the double-float (fourth delivery)) *delivery-wait*))))

(defun sim-notice! (sim key text)
  "A notice for the clock (decision 112): KEY its message's kind."
  (setf (sim-notices sim) (append (sim-notices sim) (list (list key text)))))

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
                                  controls
                                  (map 'simple-vector #'new-object *objects*)
                                  (make-grapple))))
              (setf (sim-t sim) time)
              (sim-recheck-objects! sim))))))))

;;; The objects: promotion and demotion (docs/architecture.md "Rails";
;;; decision 19: Time-warp rules, decision 110: Objects in the simulation:
;;; places.lisp's load position, the object struct and promotion checks).

(defparameter *promotion-distance* 100d3 "m; decision 19.")
(defparameter *demotion-distance* 200d3 "m; decision 19.")
(defparameter *promotion-closing-speed* 25d3
  "m/s, the closing speed a rails object's next check allows; decision 110.")
(defparameter *promotion-fast-ship* 12d3
  "m/s: a faster ship checks every object every step; decision 110.")
(declaim (type double-float *promotion-distance* *demotion-distance*
               *promotion-closing-speed* *promotion-fast-ship*))

(defun sim-object (sim key)
  "SIM's object of KEY."
  (or (find key (sim-objects sim) :key #'object-key)
      (error "No object ~S." key)))

(defun sim-update-objects! (sim)
  "Promotion and demotion at SIM's t: a promoted object beyond
*demotion-distance* of the ship goes back on rails; a rails object whose
next check is due is promoted within *promotion-distance*, else its next
check is t + (d − *promotion-distance*) / *promotion-closing-speed* rounded
down to the grid, or t while the ship is faster than *promotion-fast-ship*.
Conses nothing unless an object changes."
  (declare (optimize speed) (type sim sim) (inline rails-state-into!))
  (let* ((time (sim-t sim))
         (ship (sim-ship sim))
         (ship-r (ship-state-r ship))
         (fast (> (vec3-norm (ship-state-v ship)) *promotion-fast-ship*))
         (r (make-vec3)) (v (make-vec3)))
    (declare (type double-float time) (dynamic-extent r v))
    (flet ((distance (p)
             (declare (type vec3 p))
             (let ((dx (- (aref p 0) (aref ship-r 0)))
                   (dy (- (aref p 1) (aref ship-r 1)))
                   (dz (- (aref p 2) (aref ship-r 2))))
               (sqrt (the (double-float 0d0) (+ (* dx dx) (* dy dy) (* dz dz))))))
           (schedule! (object d)
             (declare (type object object) (type double-float d))
             (setf (object-next-check object)
                   (if fast
                       time
                       (+ time (* +fine-step+
                                  (the fixnum
                                       (floor (min 1d12 (/ (max 0d0 (- d *promotion-distance*))
                                                           (* *promotion-closing-speed*
                                                              +fine-step+)))))))))))
      (declare (inline distance schedule!))
      (loop for object across (sim-objects sim)
            do (case (object-status object)
                 (:promoted
                  (let ((d (distance (ship-state-r (object-state object)))))
                    (when (> d *demotion-distance*)
                      (object-demote! object time (sim-body sim))
                      (schedule! object d))))
                 (:rails
                  (when (<= (object-next-check object) time)
                    (let ((segment (rails-segment-at (object-rails object) time)))
                      ;; Past a crash end: removal is hazards.lisp's.
                      (unless (and (eq (rails-segment-end-kind segment) :crash)
                                   (> time (rails-segment-end segment)))
                        (rails-state-into! r v segment time)
                        (let ((d (distance r)))
                          (if (<= d *promotion-distance*)
                              (object-promote! object time)
                              (schedule! object d)))))))))))
  sim)

(defun sim-recheck-objects! (sim)
  "Every rails object's check due now, then sim-update-objects!: the start
state's first checks, or after the ship was placed; SIM."
  (loop for object across (sim-objects sim)
        when (eq (object-status object) :rails)
          do (setf (object-next-check object) (sim-t sim)))
  (sim-update-objects! sim))

;;; Targets (decision 114: Targets and target holds in the sim): a place of
;;; *places* or a towable object, by key.

(defun target-object-key (key)
  "The object key of target KEY, or nil for a point on rails (Skua Yard's
drop point)."
  (let ((place (find key *places* :key #'place-key)))
    (if place (place-object place) key)))

(defun target-name (key)
  "Target KEY's display name: its object's, else its place's."
  (let ((object (target-object-key key)))
    (if object
        (object-def-name (object-definition object))
        (place-name (place key)))))

(defun target-available-p (sim key)
  "KEY's object is neither removed nor latched (a drop point always is)."
  (let ((object (target-object-key key)))
    (or (null object)
        (not (member (object-status (sim-object sim object)) '(:removed :latched))))))

(defun target-candidates (sim)
  "The targets ] and [ cycle through: the places in *places*' order, then the
towable objects not already a place in *objects*' order; removed and latched
objects skipped."
  (remove-if-not (lambda (key) (target-available-p sim key))
                 (append (mapcar #'place-key *places*)
                         (loop for def in *objects*
                               for key = (object-def-key def)
                               when (and (eq (object-def-kind def) :towable)
                                         (not (find key *places* :key #'place-object)))
                                 collect key))))

(defun sim-set-target! (sim key)
  "SIM's target KEY, or none for nil (an active target hold released to
rate damping); KEY."
  (unless key
    (controls-release-target-hold (sim-controls sim)))
  (setf (sim-target sim) key))

(defun sim-target-hold! (sim kind)
  "7–0: controls-set-target-hold of KIND from the ship's attitude; without a
target refused: \"NO TARGET\", else nil (decision 114)."
  (if (sim-target sim)
      (progn (controls-set-target-hold (sim-controls sim) kind (ship-state-q (sim-ship sim)))
             nil)
      "NO TARGET"))

(defun sim-cycle-target! (sim direction)
  "The next candidate (DIRECTION +1, ]) or the previous (−1, [), wrapping;
from none or a target no longer a candidate the first or the last. The new
target's key, nil without candidates."
  (let* ((candidates (target-candidates sim))
         (n (length candidates))
         (i (position (sim-target sim) candidates)))
    (sim-set-target! sim (and (> n 0)
                              (nth (cond (i (mod (+ i direction) n))
                                         ((> direction 0) 0)
                                         (t (1- n)))
                                   candidates)))))

;; render-state-update! inlines it: a full call boxes TIME.
(declaim (sb-ext:maybe-inline target-state-into!))

(defun target-state-into! (r v sim &optional (time (sim-t sim)))
  "SIM's target's state at TIME (≤ the sim's t) into R, V (m, m/s,
Earth-centred): on rails exactly, a promoted object from its state at the
sim's t as r + v τ + a τ²/2 + j τ³/6, τ = TIME − t, a the sim's gravity
(J2 at most), j point-mass gravity's jerk (decision 143: The target at the
render time: the sim's gravity and the jerk). True, or nil without a target
or its state. Conses nothing."
  (declare (type vec3 r v) (type sim sim) (type double-float time)
           (inline rails-state-into!))
  (let ((key (sim-target sim)))
    (when key
      (let ((object-key (target-object-key key)))
        (if (null object-key)
            (let ((rails (place-rails (place key))))
              (rails-state-into! r v (rails-segment-at rails time) time)
              t)
            (let ((object (sim-object sim object-key))
                  (a (make-vec3)))
              (declare (dynamic-extent a))
              (case (object-status object)
                (:rails
                 (let ((segment (rails-segment-at (object-rails object) time)))
                   (unless (and (eq (rails-segment-end-kind segment) :crash)
                                (> time (rails-segment-end segment)))
                     (rails-state-into! r v segment time)
                     t)))
                ((:promoted :latched)
                 (let ((state (object-state object)))
                   (replace r (ship-state-r state))
                   (replace v (ship-state-v state)))
                 (let ((tau (- time (sim-t sim))))
                   (declare (type double-float tau))
                   (unless (= tau 0d0)
                     (gravity-into! a r (sim-t sim) (if (eq (sim-model sim) :full) :j2 (sim-model sim)))
                     (let* ((rn (vec3-norm r))
                            (k (/ (- +mu-earth+) (* rn rn rn)))
                            (rv (/ (* 3 (vec3-dot r v)) (* rn rn))))
                       (declare (type double-float rn k rv))
                       (dotimes (i 3)
                         (let ((ai (aref a i))
                               (ji (* k (- (aref v i) (* rv (aref r i))))))
                           (setf (aref r i) (+ (aref r i)
                                               (* tau (+ (aref v i)
                                                         (* tau (+ (* 0.5d0 ai)
                                                                   (* (/ tau 6) ji))))))
                                 (aref v i) (+ (aref v i) (* tau (+ ai (* 0.5d0 tau ji)))))))))
                   t))
                (t nil))))))))

(declaim (inline target-rails-segment))
(defun target-rails-segment (sim time)
  "The rails segment SIM's target is on at TIME, or nil: none, promoted or
latched."
  (let* ((key (sim-target sim))
         (object-key (and key (target-object-key key))))
    (cond ((null key) nil)
          ((null object-key) (rails-segment-at (place-rails (place key)) time))
          (t (let ((object (sim-object sim object-key)))
               (and (eq (object-status object) :rails)
                    (rails-segment-at (object-rails object) time)))))))

(defun target-check! (sim)
  "After a step's removals and captures: a target whose object is removed
or latched is cleared with the notice \"TARGET LOST: <NAME>\", an active
target hold released to rate damping."
  (let ((key (sim-target sim)))
    (when (and key (not (target-available-p sim key)))
      (sim-set-target! sim nil)
      (sim-notice! sim :target (format nil "TARGET LOST: ~A" (target-name key)))))
  sim)

;;; Planned burns (decision 115: Planned burns as scheduled controls in the
;;; sim): flown open-loop at each step's start; planning is the autopilot's.

(defparameter *plan-barrier* 60d0
  "s past a running job's snapshot, or past a cut that asks for a replan, the
clock steps the sim at most; decision 117.")
(defparameter *cut-error-min* 0.2d0
  "m/s: a cut's velocity error over max(it, *cut-error-fraction* of the burn's Δv) replans; decision 117.")
(defparameter *cut-error-fraction* 0.01d0 "decision 117.")
(defparameter *plan-max-replans* 2 "Replans a plan takes, then \"AUTOPILOT: GOAL MISSED\"; decision 117.")
(declaim (type double-float *plan-barrier* *cut-error-min* *cut-error-fraction*)
         (type fixnum *plan-max-replans*))

(defun sim-set-plan! (sim plan)
  "PLAN (a burn-plan, or nil) engaged from its first burn, the cursor's
retries, replans and resume 0; SIM."
  (setf (sim-plan sim) plan
        (sim-plan-index sim) 0
        (sim-plan-phase sim) (if (and plan (> (length (burn-plan-burns plan)) 0)) :pending :done)
        (sim-plan-rcs-target sim) 0d0
        (sim-plan-retries sim) 0
        (sim-plan-replans sim) 0
        (sim-plan-resume sim) 0d0
        (sim-plan-replan-t sim) 0d0)
  sim)

(declaim (inline plan-next-time))
(defun plan-next-time (sim)
  "The time of SIM's plan's next scheduled control: the current burn's
hold-at while :pending, its start while :holding (none while a resume waits
for the hold to settle), the cut + *plan-barrier* while :replan (the new
plan's installation); else nil."
  (let ((plan (sim-plan sim)))
    (when plan
      (let ((burn (svref (burn-plan-burns plan) (sim-plan-index sim))))
        (declare (type planned-burn burn))
        (case (sim-plan-phase sim)
          (:pending (planned-burn-hold-at burn))
          (:holding (and (= (sim-plan-resume sim) 0d0) (planned-burn-start burn)))
          (:replan (+ (sim-plan-replan-t sim) *plan-barrier*)))))))

(defun plan-cut-error (sim burn)
  "m/s between SIM's velocity now and BURN's shot after its cut, carried to
now by Kepler (decision 117)."
  (let* ((ship (sim-ship sim))
         (cut-t (planned-burn-cut-t burn))
         (v (if (= (sim-t sim) cut-t)
                (planned-burn-cut-v burn)
                (nth-value 1 (orbit-state (state->orbit (planned-burn-cut-r burn)
                                                        (planned-burn-cut-v burn)
                                                        (body-mu (body (sim-body sim))) cut-t)
                                          (sim-t sim))))))
    (vec3-norm (vec3-sub (ship-state-v ship) v))))

(defun plan-cut-missed-p (sim plan burn)
  "After BURN's cut: PLAN is the planner's (it has a goal measure) and SIM's
velocity is off its shot by more than max(*cut-error-min*,
*cut-error-fraction* of the burn's Δv)."
  (and (burn-plan-measure plan)
       (planned-burn-cut-v burn)
       (> (plan-cut-error sim burn)
          (max *cut-error-min* (* *cut-error-fraction* (planned-burn-dv burn))))))

(defun plan-step-controls! (sim)
  "At the start of SIM's step, before the cut-off check: from the current
burn's hold-at its inertial hold (roll offset 0); from its start the
throttle with sim-cutoff at odometer + Δv, or the RCS burn's ±x translation
key with its own odometer target; once cut (or the RCS target reached, the
key released) the plan's coast hold and the next burn :pending, :done after
the last. A burn cut for its attitude (plan-step-checks!) resumes with the
Δv it had left once its hold has settled. A planner's plan is measured at
each cut (plan-cut-missed-p): off its shot, the cursor goes :replan at the
burn cut, with the cut's t, for the clock to install a new plan
(autopilot-replan-due!); after *plan-max-replans* the plan ends with the
notice \"AUTOPILOT: GOAL MISSED\", the coast hold kept. Several at one t in
turn. SIM; conses nothing but at a measured cut."
  (declare (optimize speed) (type sim sim))
  (let ((plan (sim-plan sim)))
    (when plan
      (let* ((burns (burn-plan-burns plan))
             (controls (sim-controls sim))
             (ship (sim-ship sim))
             (time (sim-t sim)))
        (declare (type double-float time))
        (loop
          (let ((burn (svref burns (sim-plan-index sim)))
                (odometer (ship-state-dv ship)))
            (declare (type planned-burn burn) (type double-float odometer))
            (ecase (sim-plan-phase sim)
              ((:done :replan) (return))
              (:pending
               (when (< time (planned-burn-hold-at burn)) (return))
               (controls-engage-hold! controls :inertial (planned-burn-attitude burn))
               (setf (sim-plan-phase sim) :holding))
              (:holding
               (let ((resume (sim-plan-resume sim)))
                 (if (> resume 0d0)
                     (unless (controls-settled controls) (return))
                     (when (< time (planned-burn-start burn)) (return)))
                 (let ((rcs (planned-burn-rcs burn))
                       (dv (if (> resume 0d0) resume (planned-burn-dv burn))))
                   (if rcs
                       (progn (controls-set-translation-key controls :x rcs)
                              (setf (sim-plan-rcs-target sim) (+ odometer dv)))
                       (progn (controls-set-throttle controls (planned-burn-throttle burn))
                              (setf (sim-cutoff sim) (+ odometer dv))))))
               (setf (sim-plan-phase sim) :burning
                     (sim-plan-resume sim) 0d0))
              (:burning
               (if (planned-burn-rcs burn)
                   (progn (when (< odometer (sim-plan-rcs-target sim)) (return))
                          (controls-set-translation-key controls :x 0))
                   (let ((cutoff (sim-cutoff sim)))
                     (when (and cutoff (> (controls-throttle controls) 0d0) (< odometer cutoff))
                       (return))
                     (controls-throttle-cut controls)
                     (setf (sim-cutoff sim) nil)))
               (controls-engage-hold! controls (burn-plan-coast-hold plan))
               (cond ((plan-cut-missed-p sim plan burn)
                      (if (>= (sim-plan-replans sim) *plan-max-replans*)
                          (progn (sim-set-plan! sim nil)
                                 (sim-notice! sim :autopilot "AUTOPILOT: GOAL MISSED"))
                          (setf (sim-plan-phase sim) :replan
                                (sim-plan-replan-t sim) time))
                      (return))
                     ((< (1+ (sim-plan-index sim)) (length burns))
                      (setf (sim-plan-index sim) (1+ (sim-plan-index sim))
                            (sim-plan-phase sim) :pending))
                     (t (setf (sim-plan-phase sim) :done))))))))))
  sim)

(defun sim-install-plan! (sim plan)
  "PLAN replacing SIM's plan in :replan (decision 117): from its first burn,
the attempts and the coast hold kept, one replan more; SIM."
  (let ((retries (sim-plan-retries sim))
        (replans (sim-plan-replans sim))
        (hold (burn-plan-coast-hold (sim-plan sim))))
    (sim-set-plan! sim plan)
    (setf (sim-plan-retries sim) retries
          (sim-plan-replans sim) (1+ replans))
    (unless (eq hold (burn-plan-coast-hold plan))
      (controls-engage-hold! (sim-controls sim) (burn-plan-coast-hold plan))))
  sim)

(declaim (inline plan-rcs-burning-p))
(defun plan-rcs-burning-p (sim)
  "SIM's plan runs an RCS burn now."
  (let ((plan (sim-plan sim)))
    (and plan (eq (sim-plan-phase sim) :burning)
         (planned-burn-rcs (svref (burn-plan-burns plan) (sim-plan-index sim))))))

(defun plan-burn-due (sim)
  "The burn of SIM's plan that burns in its next step (plan-step-controls!
starts it then or it runs), or nil."
  (let ((plan (sim-plan sim)))
    (when plan
      (let ((burn (svref (burn-plan-burns plan) (sim-plan-index sim)))
            (resume (> (sim-plan-resume sim) 0d0)))
        (case (sim-plan-phase sim)
          (:burning burn)
          (:pending (and (>= (sim-t sim) (planned-burn-start burn)) burn))
          (:holding (and (if resume
                             (controls-settled (sim-controls sim))
                             (>= (sim-t sim) (planned-burn-start burn)))
                         burn)))))))

;;; The autopilot's execution rules (decision 117: Autopilot in milestone
;;; 3: planning jobs, goal measures, burns, execution rules in the step,
;;; trims): in the step, so they land on the grid; planning is autopilot.lisp's.

(defparameter *unsafe-orbit-margin* 25d3
  "m above the crash radius: a burn that would end below it aborts; decision 117.")
(defparameter *burn-attitude-limit* (deg->rad 5d0)
  "rad between the thrust axis and the burn's direction; decision 117.")
(declaim (type double-float *unsafe-orbit-margin* *burn-attitude-limit*))

(defun sim-plan-stop! (sim)
  "SIM's plan ended by an abort or a disengage: throttle off, no cut-off,
translation keys released, rate damping, sim-plan nil; SIM."
  (let ((controls (sim-controls sim)))
    (controls-throttle-cut controls)
    (fill (controls-translation-keys controls) 0)
    (setf (controls-attitude-mode controls) :rate-damping
          (controls-settled controls) nil
          (sim-cutoff sim) nil))
  (sim-set-plan! sim nil))

(defun sim-engage-plan! (sim plan)
  "PLAN engaged in SIM as the autopilot does (decision 117), and as its
shots start: throttle off, no cut-off, keys released, PLAN from its first
burn, its coast hold at once; SIM."
  (let ((controls (sim-controls sim)))
    (controls-throttle-cut controls)
    (fill (controls-rotation-keys controls) 0)
    (fill (controls-translation-keys controls) 0)
    (setf (sim-cutoff sim) nil)
    (sim-set-plan! sim plan)
    (controls-engage-hold! controls (burn-plan-coast-hold plan)))
  sim)

(defun plan-abort! (sim reason)
  "An abort: sim-plan-stop! and the notice \"AUTOPILOT ABORT: REASON\"."
  (sim-plan-stop! sim)
  (sim-notice! sim :autopilot (format nil "AUTOPILOT ABORT: ~A" reason))
  sim)

(declaim (inline periapsis-after))
(defun periapsis-after (r v axis dv mu)
  "The osculating periapsis radius of R and V + DV AXIS about MU, any
conic. Conses nothing."
  (declare (type vec3 r v axis) (type double-float dv mu))
  (let* ((vx (+ (aref v 0) (* dv (aref axis 0))))
         (vy (+ (aref v 1) (* dv (aref axis 1))))
         (vz (+ (aref v 2) (* dv (aref axis 2))))
         (rx (aref r 0)) (ry (aref r 1)) (rz (aref r 2))
         (hx (- (* ry vz) (* rz vy)))
         (hy (- (* rz vx) (* rx vz)))
         (hz (- (* rx vy) (* ry vx)))
         (h2 (+ (* hx hx) (* hy hy) (* hz hz)))
         (energy (- (* 0.5d0 (+ (* vx vx) (* vy vy) (* vz vz)))
                    (/ mu (sqrt (+ (* rx rx) (* ry ry) (* rz rz))))))
         (e (sqrt (the (double-float 0d0)
                       (max 0d0 (+ 1 (/ (* 2 energy h2) (* mu mu))))))))
    (/ (/ h2 mu) (+ 1 e))))

(defun plan-propellant-left (sim burn dv)
  "kg SIM's plan needs from now: DV left of the current BURN, then the
later burns', each by the rocket equation at the mass after the earlier."
  (declare (optimize speed) (type sim sim) (type planned-burn burn) (type double-float dv))
  (let* ((burns (burn-plan-burns (sim-plan sim)))
         (mass (+ *ship-dry-mass* (ship-state-m-p (sim-ship sim))
                  (with-stack-load (load sim) (stack-load-mass load))))
         (start mass))
    (declare (type double-float mass start))
    (flet ((burn! (dv isp)
             (declare (type double-float dv isp))
             (setf mass (* mass (exp (- (/ dv (* isp +g0+))))))))
      (burn! dv (planned-burn-isp burn))
      (loop for i of-type fixnum from (1+ (sim-plan-index sim)) below (length burns)
            do (let ((later (svref burns i)))
                 (declare (type planned-burn later))
                 (burn! (planned-burn-dv later) (planned-burn-isp later)))))
    (- start mass)))

(defun plan-step-checks! (sim coarse)
  "decision 117's rules at SIM's step start, after plan-step-controls!
(COARSE: a step above ×4). After the last cut the plan completes: the
notice \"AUTOPILOT: PLAN COMPLETE\", a checkpoint request, sim-plan nil, the
coast hold kept. During a burn, with Δv left the cut-off's (the RCS
target's) less the odometer, in order: an abort \"UNSAFE ORBIT\" when the
periapsis after cutting now is at or above the crash radius +
*unsafe-orbit-margin* but the one after the Δv left applied along the
thrust axis is not; \"PROPELLANT LOW\" when m_p is below the plan's
propellant left and half its reserve; on a fine step the thrust axis more
than *burn-attitude-limit* off the burn's cuts it to :holding at its
attitude, resuming with the Δv left once settled, the second time an abort
\"ATTITUDE ERROR\". SIM; conses nothing but an abort's or completion's
notice."
  (declare (optimize speed) (type sim sim) (type boolean coarse))
  (let ((plan (sim-plan sim)))
    (when plan
      (case (sim-plan-phase sim)
        (:done
         (sim-set-plan! sim nil)
         (sim-notice! sim :autopilot "AUTOPILOT: PLAN COMPLETE")
         (setf (sim-checkpoint-request sim) t))
        (:burning
         (let* ((burn (svref (burn-plan-burns plan) (sim-plan-index sim)))
                (rcs (planned-burn-rcs burn))
                (ship (sim-ship sim))
                (controls (sim-controls sim))
                (target (if rcs (sim-plan-rcs-target sim) (sim-cutoff sim)))
                (axis (make-vec3)) (aim (make-vec3)) (x (make-vec3)))
           (declare (type planned-burn burn) (dynamic-extent axis aim x))
           (when target
             (let* ((left (max 0d0 (- (the double-float target) (ship-state-dv ship))))
                    (body (body (sim-body sim)))
                    (floor (+ (body-crash-radius body) *unsafe-orbit-margin*))
                    (r (ship-state-r ship)) (v (ship-state-v ship)))
               (declare (type double-float left floor))
               (vec3-set! x 1d0 0d0 0d0)
               (quat-rotate! axis (ship-state-q ship) x)
               (when (eql rcs -1) (vec3-scale! axis axis -1d0))
               (cond ((and (>= (periapsis-after r v axis 0d0 (body-mu body)) floor)
                           (< (periapsis-after r v axis left (body-mu body)) floor))
                      (plan-abort! sim "UNSAFE ORBIT"))
                     ((< (ship-state-m-p ship)
                         (+ (plan-propellant-left sim burn left)
                            (* 0.5d0 (burn-plan-reserve-mass plan))))
                      (plan-abort! sim "PROPELLANT LOW"))
                     ((and (not coarse)
                           (progn (quat-rotate! aim (planned-burn-attitude burn) x)
                                  (quat-rotate! x (ship-state-q ship) x)
                                  (< (vec3-dot x aim) (cos *burn-attitude-limit*))))
                      (if (> (sim-plan-retries sim) 0)
                          (plan-abort! sim "ATTITUDE ERROR")
                          (progn
                            (if rcs
                                (controls-set-translation-key controls :x 0)
                                (controls-throttle-cut controls))
                            (controls-engage-hold! controls :inertial (planned-burn-attitude burn))
                            (setf (sim-cutoff sim) nil
                                  (sim-plan-phase sim) :holding
                                  (sim-plan-resume sim) left)
                            (incf (sim-plan-retries sim)))))))))))))
  sim)

(defun contact-abort! (sim)
  "After a contact answered in SIM's step with a plan engaged: abort
\"BERTHED\", \"LATCHED\" (a capture) or \"CONTACT\"."
  (plan-abort! sim (cond ((sim-berthed sim) "BERTHED")
                         ((eq (grapple-state (sim-grapple sim)) :closing) "LATCHED")
                         (t "CONTACT"))))

;;; The step (decision 72)

(defparameter *fine-levels-max* 4
  "The highest warp level with the fine step and attitude dynamics, decision 19: Time-warp rules.")
(declaim (type fixnum *fine-levels-max*))

(defvar *hold-controls*)

(defun kinematic-hold-attitude (q w-t time-cell r v)
  "rk4-step!'s attitude function for the hold of *hold-controls*: as
controls-attitude-fn's, without a closure per step."
  (declare (optimize speed) (type quat q) (type vec3 w-t r v)
           (type (simple-array double-float (1)) time-cell))
  (let ((controls *hold-controls*))
    (declare (type controls controls))
    (hold-attitude-into! q w-t (controls-hold controls) r v
                         (controls-roll-offset controls) (controls-inertial-target controls)
                         (controls-target-state controls) (controls-target-attitude controls)
                         (- (aref time-cell 0) (aref (controls-target-state controls) 9)))))

(declaim (inline grid-ceiling-step))
(defun grid-ceiling-step (h)
  "h rounded up to a multiple of +fine-step+."
  (declare (type (double-float 0d0 1d6) h))
  (* +fine-step+ (fceiling (* h (/ +fine-step+)))))

(declaim (inline step-h))
(defun step-h (sim level until)
  "The h of SIM's next step at LEVEL with UNTIL, and whether the Δv cut-off
ends it; it lands on a pending delivery's due time and the plan's next
hold-at or start (plan-next-time) as on UNTIL; changes
nothing (a cut due before the step counts as made)."
  (declare (type sim sim) (type fixnum level) (type (or null double-float) until))
  (let* ((ship (sim-ship sim))
         (controls (sim-controls sim))
         (time (sim-t sim))
         (coarse (> level *fine-levels-max*))
         (cutoff (sim-cutoff sim))
         (engine-on (and (> (ship-engine-force (controls-throttle controls)) 0d0)
                         (> (ship-state-m-p ship) 0d0)
                         (not (sim-berthed sim))))
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
               (m0 (+ *ship-dry-mass* (ship-state-m-p ship)
                         (with-stack-load (load sim) (stack-load-mass load))))
               (h-cut (grid-ceiling-step
                       (min 1d6 (* (/ m0 mdot)
                                   (- 1 (exp (- (/ (* (- cutoff (ship-state-dv ship)) mdot)
                                                   force)))))))))
          (declare (type double-float force mdot m0))
          (when (<= h-cut h)
            (setf h h-cut cut-after t))))
      (when (and until (> until time) (< (- until time) h))
        (setf h (- until time) cut-after nil))
      (let ((due (sim-delivery-due sim)))
        (when (and due (> due time) (< (- due time) h))
          (setf h (- due time) cut-after nil)))
      (let ((next (plan-next-time sim)))
        (when (and next (> next time) (< (- next time) h))
          (setf h (- next time) cut-after nil)))
      (values h cut-after))))

(defvar *contact-redo* nil
  "True while hazards.lisp redoes a step to a contact's grid point: no save, no contact test.")

(declaim (ftype (function (sim fixnum (or null double-float)) (values null &optional))
                contact-save!)
         (ftype (function (sim) (values sim &optional))
                berth-hold! latched-load-update! exclusion-update! delivery-update!)
         (ftype (function (sim double-float) (values sim &optional)) transfer-step!)
         (ftype (function (sim fixnum fixnum) (values (or null fixnum) &optional))
                contact-test!))

(defun sim-step-ticks! (sim level until)
  "sim-step!, returning h in 1/64 s ticks (a fixnum: nothing boxed; 0 for a
contact answered at the step's start) or nil."
  (declare (optimize speed) (type sim sim) (type fixnum level)
           (type (or null double-float) until)
           (inline flight-step! step-h object-step! target-state-into!))
  (when (sim-loss sim)
    (return-from sim-step-ticks! nil))
  (unless *contact-redo*
    (contact-save! sim level until))
  (when (sim-plan sim)
    (plan-step-controls! sim)
    (plan-step-checks! sim (> level *fine-levels-max*)))
  (let* ((ship (sim-ship sim))
         (controls (sim-controls sim))
         (time (sim-t sim))
         (coarse (> level *fine-levels-max*))
         (model (sim-model sim))
         (objects (sim-objects sim))
         (r0 (make-vec3)) (v0 (make-vec3)) (q0 (make-quat))
         (third-bodies (make-array 18 :element-type 'double-float))
         ;; The step's end t, then each object's r and v at its start.
         (before (make-array (1+ (* 6 (length objects))) :element-type 'double-float)))
    (declare (type double-float time) (type keyword model)
             (dynamic-extent r0 v0 q0 third-bodies before))
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
      ;; A target hold's target at the step's start (decision 114), with a
      ;; rails target's drift (decision 144).
      (when (controls-target-hold-p controls)
        (if (target-state-into! r0 v0 sim time)
            (let ((segment (target-rails-segment sim time)))
              (controls-target-state-set! controls r0 v0 time)
              (when segment
                (let ((s (controls-target-state controls)))
                  (rails-rate-into! v0 r0 segment)
                  (dotimes (i 3)
                    (setf (aref s (+ i 10)) (- (aref v0 i) (aref s (+ i 3))))))))
            (controls-release-target-hold controls)))
      (when (and coarse (not (sim-berthed sim))
                 (not (eq (controls-attitude-mode controls) :hold)))
        (controls-inertial-hold controls (ship-state-q ship)))
      (multiple-value-bind (h cut-after) (step-h sim level until)
        (declare (type (double-float 0d0 1d6) h))
        (copy-doubles! r0 (ship-state-r ship) 3)
        (copy-doubles! v0 (ship-state-v ship) 3)
        (copy-doubles! q0 (ship-state-q ship) 4)
        (when (eq model :full)
          (third-bodies-step-into! third-bodies time h))
        ;; Berthed (decision 112): not integrated, set from the station below.
        (cond ((sim-berthed sim)
               (fill (controls-duties controls) 0d0)
               (when (sim-transfer sim)
                 (transfer-step! sim h)))
              (coarse
               (let ((*hold-controls* controls))
                 (with-stack-load (load sim)
                   (flight-step! ship controls h time model #'kinematic-hold-attitude
                                 third-bodies load))))
              (t
               (with-stack-load (load sim)
                 (flight-step! ship controls h time model nil third-bodies load))))
        (loop for object across objects
              for i of-type fixnum from 1 by 6
              when (eq (object-status object) :promoted)
                do (let ((state (object-state object)))
                     (copy-doubles! before (ship-state-r state) 3 i)
                     (copy-doubles! before (ship-state-v state) 3 (+ i 3))
                     (object-step! object h time model third-bodies)))
        (if (sim-berthed sim)
            (berth-hold! sim)
            (latched-load-update! sim))
        (setf (sim-t sim) (+ time h))
        (let ((grapple (sim-grapple sim)))
          (when (and (eq (grapple-state grapple) :closing)
                     (>= (sim-t sim) (grapple-closing-end grapple)))
            (setf (grapple-state grapple) :latched)))
        (incf (sim-steps sim))
        (let ((cutoff (sim-cutoff sim)))
          (when (or cut-after (and cutoff (>= (ship-state-dv ship) cutoff)))
            (cut!)))
        (setf (aref before 0) (sim-t sim))
        (let* ((ticks (truncate (* h (/ +fine-step+))))
               (lost (crash-test! sim r0 v0 q0 ticks)))
          (object-crash-tests! sim before ticks)
          (let ((contact (and (not lost) (not *contact-redo*) (contact-test! sim level ticks))))
            (unless (or lost contact)
              (when (sim-excluded sim)
                (exclusion-update! sim))
              (when (sim-delivery sim)
                (delivery-update! sim))
              (sim-update-objects! sim))
            (when (sim-target sim)
              (target-check! sim))
            (when (and contact (sim-plan sim) (not (sim-loss sim)))
              (contact-abort! sim))
            (or contact ticks)))))))

(declaim (inline sim-step!))
(defun sim-step! (sim level &key until)
  "One step of SIM at warp LEVEL (×1, ×4, ×10 …) with every rule (decision
72): the plan's controls first (plan-step-controls!) and the autopilot's
rules (plan-step-checks!), the fine step to
×4, above it the coarse step with kinematic attitude (an inertial hold
engaged first if no hold is, unless berthed), shortened to land on UNTIL
(a grid time; one not ahead is ignored), on the plan's next hold-at or
start and on the Δv cut-off, then flight-step! with SIM's model and the stack's mass (its
latched load's too) and each promoted object's object-step! (the Moon and
the Sun read once for all), the berthed ship held at its station
(berth-hold!, no flight-step!; the transfer's propellant first,
transfer-step!) or a latched load written from the stack
(latched-load-update!), the claws latched at their closing end, then the
crash test (a crash
leaves SIM lost at the impact, hazards.lisp), the objects' crash tests and
rails removals (object-crash-tests!), contact with promoted objects
(contact-test!: a bounce redoes the step to the grid point before it, a
fast one kills), then, unless lost, the excluded object's check
(exclusion-update!), a pending delivery's (delivery-update!: the step
lands on its due time), promotion and demotion, and last the target's
check (target-check!) and a contact's abort of the plan (contact-abort!).
Returns h (0 for a contact answered at the step's start), or nil for a
lost sim. Hot callers declare it inline: a full
call boxes h."
  (let ((ticks (sim-step-ticks! sim level until)))
    (and ticks (* ticks +fine-step+))))

;;; Checkpoints (docs/architecture.md "Checkpoints"): the game's start and the
;;; clock's latest (clock-checkpoint, decision 112).

(defun take-checkpoint (sim) (copy-sim sim))

(defun restore-checkpoint! (sim checkpoint)
  "SIM becomes CHECKPOINT, consing nothing; SIM."
  (sim-assign! sim checkpoint))

(defun checkpoint-safe-p (sim &optional prediction)
  "Throttle off, not lost, no impact and no contact predicted (PREDICTION,
the clock's, run to its horizon; without it a fresh one, prediction.lisp)."
  (and (= (controls-throttle (sim-controls sim)) 0d0)
       (null (sim-loss sim))
       (not (prediction-dropout-p sim prediction))))

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
  (prediction nil)
  (game-t 0d0 :type double-float)               ; the sim's t last seen: the log's stamps
  (log nil :type list)                          ; the SHIP log, (t text), newest first
  (log-read nil)                                ; the log's newest entry when Y was pressed
  (checkpoint nil)                              ; the latest checkpoint, a sim, or nil
  ;; The autopilot's preset menu, its planning job and the plan last
  ;; engaged (autopilot-model, autopilot.lisp; decision 118).
  (autopilot nil))

(defun new-clock (sim)
  "A clock at ×1, unpaused, its previous state SIM's, no checkpoint; its
prediction two periods ahead while coasting (decision 116)."
  (let ((clock (%make-clock (copy-ship-state (sim-ship sim)) (new-prediction :periods 2)
                            (make-array *rate-meter-frames* :element-type 'double-float
                                                            :initial-element 0d0)
                            (make-array *rate-meter-frames* :element-type 'double-float
                                                            :initial-element 0d0))))
    (setf (clock-previous-t clock) (sim-t sim)
          (clock-game-t clock) (sim-t sim)
          (clock-autopilot clock) (make-autopilot-model))
    clock))

(defun clock-level (clock)
  "The warp level, ×1 … ×1,000,000."
  (the fixnum (nth (clock-level-index clock) *warp-levels*)))

(defun clock-kinematic-p (clock)
  "Above ×4: attitude kinematic, rotation and translation keys ignored (decision 31)."
  (> (clock-level clock) *fine-levels-max*))

;;; Messages (decision 31, decision 73): text and a real-time stamp from the
;;; summed elapsed; a new one replaces the live one of its key.

(defparameter *message-time* 3d0 "s of real time a message shows; decision 31.")
(declaim (type double-float *message-time*))

(defparameter *log-length* 200 "Entries the SHIP log keeps; decision 112.")
(declaim (type fixnum *log-length*))

(defun clock-message! (clock key text)
  "TEXT shown as the message of KEY, and in the SHIP log at the game t."
  (setf (clock-message-list clock)
        (append (remove key (clock-message-list clock) :key #'first)
                (list (list key text (clock-real-time clock)))))
  (let ((log (cons (list (clock-game-t clock) text) (clock-log clock))))
    (when (> (length log) *log-length*)
      (setf log (subseq log 0 *log-length*)))
    (setf (clock-log clock) log)))

(defun clock-log-unread (clock)
  "The SHIP log's entries newer than the read mark, newest first."
  (let ((mark (member (clock-log-read clock) (clock-log clock) :test #'eq)))
    (ldiff (clock-log clock) mark)))

(defun clock-acknowledge! (clock)
  "Y: the SHIP log read."
  (setf (clock-log-read clock) (first (clock-log clock))))

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
  "Thrust on: the throttle up with propellant left, not berthed."
  (and (> (ship-engine-force (controls-throttle (sim-controls sim))) 0d0)
       (> (ship-state-m-p (sim-ship sim)) 0d0)
       (not (sim-berthed sim))))

(defun thrust-warp-limit (sim)
  (if (sim-cutoff sim) *cutoff-warp-limit* *thrust-warp-limit*))

(defun held-p (sim) (eq (controls-attitude-mode (sim-controls sim)) :hold))

(defun clock-dropout (clock sim)
  "(values Δ kind data) to the prediction's earliest drop-out event, or nil;
DATA as prediction-event's second value."
  (multiple-value-bind (time kind) (prediction-dropout-event (clock-prediction clock) sim)
    (when time
      (values (max 0d0 (- time (sim-t sim))) kind
              (nth-value 1 (prediction-event (clock-prediction clock) kind))))))

(defun warp-refusal (clock sim level)
  "The message refusing a warp up to LEVEL, or nil (decision 19, decision
73; an RCS burn's ×4, decision 117)."
  (multiple-value-bind (delta kind) (clock-dropout clock sim)
    (cond ((and (> level *fine-levels-max*)
                (let ((burn (plan-burn-due sim))) (and burn (planned-burn-rcs burn))))
           (format nil "WARP REFUSED: RCS BURN, LIMIT ×~D" *fine-levels-max*))
          ((and (sim-engine-on-p sim) (> level (thrust-warp-limit sim)))
           (format nil "WARP REFUSED: ENGINE ON, LIMIT ×~:D" (thrust-warp-limit sim)))
          ((and (> level *fine-levels-max*) (not (held-p sim))
                (>= (vec3-norm (ship-state-w (sim-ship sim))) *warp-rotation-limit*))
           "WARP REFUSED: ROTATING, SET A HOLD")
          ((and delta (< delta (* *dropout-real-time* level)))
           (format nil "WARP REFUSED: ~A IN ~A" kind (format-duration delta))))))

(defun dropout-message (level delta kind &optional loss)
  "The text for a drop-out to LEVEL, Δ ahead of the event of KIND: decision
73's for an :impact (LOSS its record), decision 113's for a :contact,
decision 117's for a :burn."
  (cond ((member kind '(:contact :burn))
         (format nil "WARP ×~:D: ~A IN ~A" level kind (format-duration delta)))
        ((and loss (eq (loss-body loss) :moon))
         (format nil "WARP ×~:D: MOON IMPACT AHEAD" level))
        (t
         (let ((body (body (if loss (loss-body loss) :earth))))
           (format nil "WARP ×~:D: PERIAPSIS BELOW ~D KM IN ~A" level
                   (round (- (body-crash-radius body) (body-radius body)) 1000)
                   (format-duration delta))))))

(defun clock-check-level! (clock sim)
  "Before every step (decision 73): the thrust limit, ×4 for a hold released
above ×4, the highest level with the drop-out event ≥ 3 s of real time
ahead; each lowering writes its message. A plan's burn due in the step
(plan-burn-due) counts from that step: ×4 on the RCS, *cutoff-warp-limit*
on the engine (decision 117)."
  (flet ((lower! (level text)
           (when (< level (clock-level clock))
             (clock-set-level-index! clock (position level *warp-levels*))
             (clock-message! clock :lowered text))))
    (when (and (sim-engine-on-p sim) (> (clock-level clock) (thrust-warp-limit sim)))
      (lower! (thrust-warp-limit sim)
              (format nil "WARP ×~:D: ENGINE ON" (thrust-warp-limit sim))))
    (when (> (clock-level clock) *fine-levels-max*)
      (let ((burn (plan-burn-due sim)))
        (when burn
          (if (planned-burn-rcs burn)
              (lower! *fine-levels-max* (format nil "WARP ×~D: RCS BURN" *fine-levels-max*))
              (when (> (clock-level clock) *cutoff-warp-limit*)
                (lower! *cutoff-warp-limit*
                        (format nil "WARP ×~:D: ENGINE ON" *cutoff-warp-limit*)))))))
    (when (and (clock-held clock) (not (held-p sim)) (> (clock-level clock) *fine-levels-max*))
      (lower! *fine-levels-max* (format nil "WARP ×~D: HOLD RELEASED" *fine-levels-max*)))
    (multiple-value-bind (delta kind data) (clock-dropout clock sim)
      (when (and delta (< delta (* *dropout-real-time* (clock-level clock))))
        (let ((level (or (find-if (lambda (l) (>= delta (* *dropout-real-time* l)))
                                  *warp-levels* :from-end t)
                         1)))
          (lower! level (dropout-message level delta kind (and (eq kind :impact) data))))))))

;;; The rate meter: real and game seconds per frame, emptied at each warp
;;; change and pause or resume, so it measures only the current level.

(defun rate-meter-reset! (clock)
  (fill (clock-rate-real clock) 0d0)
  (fill (clock-rate-game clock) 0d0)
  (setf (clock-rate-index clock) 0))

;;; Commands: between frames, so at a step boundary; never touching the sim.

(defun clock-set-level-index! (clock index)
  "Warp level INDEX; the accumulator scales by new/old, so the real time
owed stays."
  (let ((old (clock-level clock)))
    (unless (= index (clock-level-index clock))
      (rate-meter-reset! clock))
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
  (rate-meter-reset! clock)
  (setf (clock-paused clock) (not (clock-paused clock)))
  clock)

;;; Engaging and disengaging the autopilot (decision 117), between steps.

(defun autopilot-engage! (clock sim plan)
  "PLAN flown by SIM from now (sim-engage-plan!), CLOCK's model :engaged with
it kept for the map; SIM."
  (autopilot-engaged! (clock-autopilot clock) plan)
  (sim-engage-plan! sim plan))

(defun autopilot-disengage! (clock sim &optional pilot)
  "Backspace, or PILOT input: SIM's plan stopped (sim-plan-stop!: throttle
off, rate damping) with the message \"AUTOPILOT OFF\" (\"AUTOPILOT OFF:
PILOT INPUT\"), CLOCK keeping the plan for the map. True when one was
engaged."
  (when (sim-plan sim)
    (let ((text (if pilot "AUTOPILOT OFF: PILOT INPUT" "AUTOPILOT OFF")))
      (sim-plan-stop! sim)
      (autopilot-stopped! (clock-autopilot clock) text)
      (clock-message! clock :autopilot text))
    t))

;;; The step and the frame

(defun clock-take-notices! (clock sim)
  "After a step (decision 112): SIM's notices to clock-message! and the SHIP
log, a checkpoint request to clock-checkpoint when checkpoint-safe-p on the
clock's prediction; both cleared from SIM."
  (loop for (key text) in (sim-notices sim)
        do (clock-message! clock key text)
           (when (eq key :autopilot)
             (autopilot-notice! (clock-autopilot clock) text)))
  (setf (sim-notices sim) nil)
  (when (sim-checkpoint-request sim)
    (setf (sim-checkpoint-request sim) nil)
    (when (checkpoint-safe-p sim (clock-prediction clock))
      (setf (clock-checkpoint clock) (take-checkpoint sim)))))

(declaim (inline clock-step!))
(defun clock-step! (clock sim &optional budget)
  "One step of SIM under CLOCK (decision 72, decision 73): the lowering
rules, the hook's until, sim-step! at the level after the hook (a hook
that changes the controls or the level applies clock-check-level! again
itself), the previous state and
the held flag kept. With BUDGET (game s, the frame's accumulator) no step
when the next h exceeds it. While the autopilot's job runs, no step past
its barrier (autopilot-barrier): the step lands on it, there the
accumulator is emptied. The step's 1/64 s ticks, or nil when none was
taken (lost, over BUDGET or at the barrier). A plan's cut that asks for a
replan starts the replan job after its step; at the cut + *plan-barrier*
its result is installed (autopilot-replan-due!), the job finished at once
without BUDGET (headless), else the sim waits there for the frames'
work."
  (declare (type clock clock) (type sim sim) (type (or null double-float) budget)
           (inline step-h sim-step-ticks!))
  (when (sim-loss sim)
    (return-from clock-step! nil))
  (setf (clock-game-t clock) (sim-t sim))
  (when (and (eq (sim-plan-phase sim) :replan)
             (not (autopilot-replan-due! (clock-autopilot clock) clock sim (null budget))))
    (setf (clock-accumulator clock) 0d0)
    (return-from clock-step! nil))
  (let ((barrier (autopilot-barrier (clock-autopilot clock))))
    (declare (type (or null double-float) barrier))
    (when (and barrier (>= (sim-t sim) barrier))
      (setf (clock-accumulator clock) 0d0)
      (return-from clock-step! nil))
    (clock-check-level! clock sim)
    (let* ((hook (clock-hook clock))
           (until (let ((until (and hook (funcall hook clock sim))))
                    (declare (type (or null double-float) until))
                    (if (and barrier (or (null until) (<= until (sim-t sim)) (< barrier until)))
                        barrier
                        until)))
           (level (clock-level clock)))         ; after the hook: its warp acts now
      (declare (type (or null double-float) until))
      (unless (and budget (< budget (the double-float (step-h sim level until))))
        (ship-state-assign! (clock-previous clock) (sim-ship sim))
        (setf (clock-previous-t clock) (sim-t sim))
        (let ((ticks (sim-step-ticks! sim level until)))
          (setf (clock-held clock) (and (> level *fine-levels-max*) (held-p sim))
                (clock-game-t clock) (sim-t sim))
          (when (eq (sim-plan-phase sim) :replan)
            (autopilot-replan-start! (clock-autopilot clock) sim))
          (when (or (sim-notices sim) (sim-checkpoint-request sim))
            (clock-take-notices! clock sim))
          ticks)))))

(defun clock-steps! (clock sim)
  "Steps while the accumulator holds the next h, within the frame's caps;
the rest dropped. The step count."
  (declare (optimize speed) (type clock clock) (type sim sim)
           (inline clock-step!))
  (let ((steps 0)
        (deadline (and *frame-step-time-limit*
                       (+ (get-internal-real-time)
                          (the fixnum (ceiling (* *frame-step-time-limit*
                                                  internal-time-units-per-second)))))))
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
        (let ((ticks (clock-step! clock sim (clock-accumulator clock))))
          (unless ticks
            (return steps))
          (decf (clock-accumulator clock) (* (the fixnum ticks) +fine-step+))
          (incf steps))))))

(defun rate-meter-record! (clock real game)
  (let ((i (clock-rate-index clock)))
    (setf (aref (clock-rate-real clock) i) real
          (aref (clock-rate-game clock) i) game
          (clock-rate-index clock) (mod (1+ i) *rate-meter-frames*))))

(defun clock-actual-rate (clock)
  "Game seconds per real second over the frames of the last second; nil
until the meter holds a second since the last warp change or pause."
  (let ((real 0d0) (game 0d0) (n *rate-meter-frames*))
    (declare (type double-float real game))
    (loop for k from 1 to n
          for i = (mod (- (clock-rate-index clock) k) n)
          while (< real 1d0)
          do (incf real (aref (clock-rate-real clock) i))
             (incf game (aref (clock-rate-game clock) i)))
    (and (>= real 1d0) (/ game real))))

(defun clock-frame! (clock sim elapsed)
  "One frame of ELAPSED real seconds: the prediction updated (while the
autopilot plans or replans with at most half of *prediction-work-per-frame*,
the replan job then the menu's the rest, paused too; decision 117), then the accumulator fed min(ELAPSED,
0.1 s) × level and stepped down (the hook before each step gives its
until); none paused (the accumulator emptied) or lost. The steps taken."
  (let ((elapsed (float elapsed 1d0))
        (start (sim-t sim))
        (model (clock-autopilot clock))
        (prediction (clock-prediction clock)))
    (if (or (autopilot-planning-p model) (autopilot-replanning-p model sim))
        (progn (prediction-update! prediction sim (floor *prediction-work-per-frame* 2))
               (let ((left (- *prediction-work-per-frame* (prediction-last-work prediction))))
                 (decf left (autopilot-replan-work! model sim left))
                 (autopilot-frame! model clock sim left)))
        (progn (prediction-update! prediction sim)
               (autopilot-frame! model clock sim 0)))
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
    (setf (clock-game-t clock) (sim-t sim))
    (clock-frame-steps clock)))

;;; Restart (decision 34: How death works): the LOST panel's Return and N.

(defun clock-restart! (clock sim checkpoint)
  "SIM restored to CHECKPOINT; CLOCK at ×1, unpaused, its accumulator,
messages, prediction and the autopilot's model (autopilot-reset!) cleared; its latest
checkpoint and the SHIP log kept. SIM."
  (restore-checkpoint! sim checkpoint)
  (setf (clock-game-t clock) (sim-t sim))
  (rate-meter-reset! clock)
  (autopilot-reset! (clock-autopilot clock))
  (setf (clock-level-index clock) 0
        (clock-paused clock) nil
        (clock-accumulator clock) 0d0
        (clock-message-list clock) nil
        (clock-held clock) nil
        (clock-previous-t clock) (sim-t sim))
  (ship-state-assign! (clock-previous clock) (sim-ship sim))
  (prediction-invalidate! (clock-prediction clock))
  sim)
