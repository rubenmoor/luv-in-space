(in-package #:luv-in-space.tests)

;;; prediction.lisp: the milestone-2 events by sim-step! on a copy
;;; (docs/architecture.md "Testing" → "Warp, events and collisions",
;;; Prediction; decision 75: Prediction in milestone 2: the step function on
;;; a copy).

(defun run-prediction (sim &rest args)
  (lis::prediction-run! (apply #'lis::new-prediction args) sim))

(defun prediction-nodes (pred)
  "The stored nodes from the head, as a fresh array."
  (subseq (lis::prediction-nodes pred)
          (* 7 (lis::prediction-head pred)) (* 7 (lis::prediction-count pred))))

(defun last-node (pred)
  "(values t r v) of PRED's last node."
  (let ((nodes (lis::prediction-nodes pred))
        (k (* 7 (1- (lis::prediction-count pred)))))
    (values (aref nodes k) (subseq nodes (+ k 1) (+ k 4)) (subseq nodes (+ k 4) (+ k 7)))))

(defun prediction-events (pred)
  (loop for i below (lis::prediction-event-count pred)
        collect (list (svref (lis::prediction-event-kinds pred) i)
                      (aref (lis::prediction-event-times pred) i))))

(defun r-dot-v (sim)
  (let ((ship (lis::sim-ship sim)))
    (lis::vec3-dot (lis::ship-state-r ship) (lis::ship-state-v ship))))

(define-test prediction-grid-ceiling
  (let ((t0 (lis::start-t)))
    (is = t0 (lis::grid-ceiling t0))
    (is = (+ t0 1/64) (lis::grid-ceiling (+ t0 1d-6)))
    (is = (+ t0 1/64) (lis::grid-ceiling (+ t0 1/64)))))

(define-test prediction-coasting-bit-for-bit
  ;; :full at ×1,000: one orbit ahead the prediction is the flight.
  (let* ((sim (lis::new-game-sim))
         (pred (run-prediction sim))
         (period (lis::osculating-period sim)))
    (true (lis::prediction-complete-p pred))
    (false (lis::prediction-event pred :impact))
    (multiple-value-bind (end r v) (last-node pred)
      (true (>= (- end (lis::sim-t sim)) period))
      (let ((flown (step-sim (lis::copy-sim sim) 100000 1000 end)))
        (is = end (lis::sim-t flown))
        (true (equalp r (lis::ship-state-r (lis::sim-ship flown))))
        (true (equalp v (lis::ship-state-v (lis::sim-ship flown))))))
    ;; Frames of 500 steps.
    (let ((framed (lis::new-prediction))
          (frames 0))
      (let ((lis::*prediction-work-per-frame* 500))
        (loop do (lis::prediction-update! framed sim)
                 (incf frames)
                 (true (<= (lis::prediction-last-work framed) 500))
              until (lis::prediction-complete-p framed)))
      (true (> frames 5))
      (is = 1 (lis::prediction-restarts framed))
      (true (equalp (prediction-nodes pred) (prediction-nodes framed)))
      (is equal (prediction-events pred) (prediction-events framed)))))

(defun apsis-sim ()
  "A :point orbit with e 0.1, periapsis 400 km, a quarter past it."
  (let* ((q (+ lis::+radius-earth+ 400d3))
         (orbit (lis::make-elliptic-orbit lis::+mu-earth+ (/ q 0.9d0) 0.1d0 (lis::deg->rad 60)
                                          1d0 2d0 (/ pi 2) (lis::start-t))))
    (values (orbit-sim orbit) orbit)))

(define-test prediction-apsides
  (multiple-value-bind (sim orbit) (apsis-sim)
    ;; The apoapsis comes first.
    (let ((pred (run-prediction sim))
          (start (lis::sim-t sim)))
      (dolist (kind '(:apoapsis :periapsis))
        (let ((predicted (lis::prediction-event pred kind))
              (kepler (lis::orbit-next-apsis orbit start kind)))
          (true predicted)
          (when predicted
            (true (< (abs (- predicted kepler)) 1d-3)
                  "~A ~,6F s from Kepler" kind (- predicted kepler))
            ;; The flight: coarse to 20 s before (the inertial hold it engages
            ;; makes ×1 steps cost the mixer), then ×1 to the grid point after;
            ;; r·v changes sign over the last fine step.
            (let ((after (lis::grid-ceiling predicted))
                  (before-sign nil))
              (step-sim sim 100000 1000 (- after 20))
              (step-sim sim 100000 1 (- after 1/64))
              (setf before-sign (plusp (r-dot-v sim)))
              (step-sim sim 1 1 after)
              (is = after (lis::sim-t sim))
              (true (> (- after predicted) -1d-9))
              (is eq (eq kind :apoapsis) before-sign)
              (is eq (eq kind :periapsis) (plusp (r-dot-v sim))))))))))

(define-test prediction-impact
  (let* ((start (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3))))
         (pred (run-prediction start)))
    (multiple-value-bind (time loss) (lis::prediction-event pred :impact)
      (true time)
      (when time
        (is eq :earth (lis::loss-body loss))
        (is = time (lis::loss-time loss))
        (is = time (lis::prediction-dropout-event pred))
        (is eq :impact (nth-value 1 (lis::prediction-dropout-event pred)))
        (let ((coarse (fly-until-lost (lis::copy-sim start) 1000 (+ time 10))))
          (true (< (abs (- (lis::loss-time (lis::sim-loss coarse)) time)) 1d-2)))
        (let ((fine (fly-until-lost (lis::copy-sim start) 1 (+ time 10))))
          (true (lis::sim-lost-p fine))
          (true (< (abs (- (lis::loss-time (lis::sim-loss fine)) time)) 0.05d0)
                "×1: ~,6F s from the prediction" (- (lis::loss-time (lis::sim-loss fine)) time)))))
    (false (lis::checkpoint-safe-p start))
    (true (lis::checkpoint-safe-p (lis::new-game-sim)))))

(define-test prediction-cutoff
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-model sim) :point
          (lis::sim-cutoff sim) 25d0)
    (lis::controls-throttle-full (lis::sim-controls sim))
    (let* ((pred (run-prediction sim))
           (cut (lis::prediction-event pred :cutoff))
           (apoapsis (lis::prediction-event pred :apoapsis)))
      (true cut)
      (true apoapsis)
      (when (and cut apoapsis)
        (loop while (lis::sim-cutoff sim) do (step-sim sim 1 100))
        (is = cut (lis::sim-t sim))
        (step-sim sim 100000 100 (lis::grid-ceiling apoapsis))
        (let ((p (lis::prediction-position! (lis::make-vec3) pred apoapsis)))
          (true (< (abs (- (lis::vec3-norm p)
                           (lis::vec3-norm (lis::ship-state-r (lis::sim-ship sim)))))
                   1d0)))))))

(defun frames (pred sim n)
  "N frames of two fine steps of SIM, each followed by prediction-update!."
  (dotimes (i n pred)
    (step-sim sim 2 1)
    (lis::prediction-update! pred sim)))

(define-test prediction-restart-rules
  (flet ((restarts-after (change)
           (let ((sim (lis::new-game-sim))
                 (pred (lis::new-prediction)))
             (setf (lis::sim-model sim) :point)
             (lis::prediction-run! pred sim)
             (frames pred sim 2)
             (let ((before (lis::prediction-restarts pred)))
               (funcall change sim)
               (lis::prediction-update! pred sim)
               (- (lis::prediction-restarts pred) before)))))
    (is = 1 (restarts-after (lambda (sim) (lis::controls-set-throttle (lis::sim-controls sim) 0.5))))
    (is = 1 (restarts-after (lambda (sim) (lis::controls-set-hold (lis::sim-controls sim) :retrograde))))
    (is = 1 (restarts-after (lambda (sim) (incf (aref (lis::ship-state-r (lis::sim-ship sim)) 0) 2d0))))
    (is = 0 (restarts-after (lambda (sim) (incf (aref (lis::ship-state-r (lis::sim-ship sim)) 0) 0.1d0)))))
  ;; Coasting 100 frames: no restart, the tail grows.
  (let* ((sim (lis::new-game-sim))
         (pred (lis::new-prediction)))
    (setf (lis::sim-model sim) :point)
    (lis::prediction-run! pred sim)
    (multiple-value-bind (end r) (last-node pred)
      (frames pred sim 100)
      (is = 1 (lis::prediction-restarts pred))
      (true (> (last-node pred) end))
      (let ((p (lis::prediction-position! (lis::make-vec3) pred end)))
        (true (equalp r p)))))
  ;; Throttle on: a restart each call.
  (let* ((sim (lis::new-game-sim))
         (pred (run-prediction sim)))
    (lis::controls-set-throttle (lis::sim-controls sim) 0.5)
    (frames pred sim 3)
    (is = 4 (lis::prediction-restarts pred))))

(define-test prediction-death-and-restore
  (let* ((start (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3))))
         (checkpoint (lis::take-checkpoint start))
         (pred (run-prediction start)))
    (fly-until-lost start 1000 (+ (lis::start-t) 20000))
    (lis::prediction-update! pred start)
    (is = 2 (lis::prediction-restarts pred))
    (true (lis::prediction-complete-p pred))
    (false (lis::prediction-event pred :impact))
    (lis::prediction-update! pred start)
    (is = 2 (lis::prediction-restarts pred))
    (lis::restore-checkpoint! start checkpoint)
    (lis::prediction-update! pred start)
    (is = 3 (lis::prediction-restarts pred))))

(define-test prediction-budget-and-consing
  (let* ((sim (lis::new-game-sim))
         (pred (lis::new-prediction :periods 2)))
    (lis::prediction-update! pred sim)
    (is = lis::*prediction-work-per-frame* (lis::prediction-last-work pred))
    (false (lis::prediction-complete-p pred))
    (loop repeat 20 until (lis::prediction-complete-p pred)
          do (lis::prediction-update! pred sim))
    (true (lis::prediction-complete-p pred))
    (is = 1 (lis::prediction-restarts pred))
    (let ((consed (least-bytes-consed
                   (lambda ()
                     (lis::prediction-invalidate! pred)
                     (lis::prediction-run! pred sim)))))
      (true (< consed 1024) "~D bytes" consed))
    (is = 4 (lis::prediction-restarts pred))))

;;; The contact drop-out (docs/architecture.md "Time warp", "Trajectory
;;; prediction"; decision 43: Warp drop-outs for contact and life support,
;;; decision 113: Grapple and contact details the design leaves open).

(defun intercept! (sim key v-rel duration)
  "SIM's ship (:point) placed to coast into object KEY's CoM DURATION s
from now at V-REL (inertial) relative to it, both on Kepler conics; KEY
put on J2-free rails when on rails. (values sim index)."
  (let* ((object (lis::sim-object sim key))
         (index (position object (lis::sim-objects sim)))
         (t0 (lis::sim-t sim))
         (t1 (+ t0 duration))
         (mu lis::+mu-earth+))
    (multiple-value-bind (r v) (object-rv object t0)
      (let ((orbit (lis::state->orbit r v mu t0)))
        (when (eq (lis::object-status object) :rails)
          (setf (lis::object-rails object) (lis::make-rails orbit t0 :j2 nil)))
        (multiple-value-bind (r1 v1) (lis::orbit-state orbit t1)
          (multiple-value-bind (r0 v0)
              (lis::orbit-state (lis::state->orbit r1 (lis::vec3-add v1 v-rel) mu t1) t0)
            (setf (lis::sim-model sim) :point)
            (lis::ship-state-assign! (lis::sim-ship sim)
                                     (lis::make-ship-state r0 v0 (lis::quat 1 0 0 0)
                                                           (lis::make-vec3) 28000d0))
            (lis::sim-recheck-objects! sim)
            (values sim index)))))))

(defun approach-sim ()
  "The ship 1,800 s from Skua Yard 3 (50 t, promoted by hand), closing at
2 m/s radially at the end: ~5 km away now. (values sim index)."
  (let ((sim (contact-scene :skua-yard-3 (lis::vec3 -100 0 0))))
    (intercept! sim :skua-yard-3 (lis::vec3 -2 0 0) 1800d0)))

(define-test prediction-contact-event
  (multiple-value-bind (sim index) (approach-sim)
    (let* ((ship (lis::sim-ship sim))
           (object (svref (lis::sim-objects sim) index))
           (d (lis::vec3-norm (lis::vec3-sub (lis::ship-state-r (lis::object-state object))
                                             (lis::ship-state-r ship))))
           (pred (run-prediction sim)))
      (true (< 4000 d 7000) "~,1F m" d)
      (multiple-value-bind (time object-index) (lis::prediction-event pred :contact)
        (true time)
        (is eql index object-index)
        ;; The spheres meet a little before the CoMs: ~(bounds) / 2 m/s.
        (when time
          (true (< 1780 (- time (lis::sim-t sim)) 1800) "~,3F s" (- time (lis::sim-t sim)))
          (is = time (lis::prediction-dropout-event pred))
          (is eq :contact (nth-value 1 (lis::prediction-dropout-event pred)))))
      ;; The copy's contact death is no :impact.
      (true (lis::sim-lost-p (lis::prediction-sim pred)))
      (false (lis::prediction-event pred :impact))
      (false (lis::checkpoint-safe-p sim))
      (let ((consed (least-bytes-consed (lambda ()
                                          (lis::prediction-invalidate! pred)
                                          (lis::prediction-run! pred sim)))))
        (true (< consed 1024) "~D bytes" consed)))))

(define-test prediction-contact-none
  ;; Berthed, latched, just released, overlapping at the start (bouncing,
  ;; or dying): no :contact event.
  (flet ((events (sim)
           (let ((pred (run-prediction sim)))
             (list (lis::prediction-event pred :contact) (lis::prediction-event pred :impact)))))
    (dolist (key '(:tern-station :skua-yard-3))
      (let ((sim (attached-scene key)))
        (is equal '(nil nil) (events sim) "~A attached" key)
        (false (lis::grapple-key! sim))
        (is equal '(nil nil) (events sim) "~A released" key)))
    (dolist (speed '(0.5d0 1.01d0))
      (is equal '(nil nil) (events (aft-scene speed)) "~F m/s" speed))))

(define-test prediction-contact-rails
  ;; Tern Station on rails ~180 km away, promoted on the copy: seen.
  (multiple-value-bind (sim index) (intercept! (lis::new-game-sim) :tern-station
                                               (lis::vec3 0 0 50) 3000d0)
    (is eq :rails (lis::object-status (svref (lis::sim-objects sim) index)))
    (let ((pred (run-prediction sim)))
      (multiple-value-bind (time object-index) (lis::prediction-event pred :contact)
        (true time)
        (is eql index object-index)
        (when time
          (true (< 2998 (- time (lis::sim-t sim)) 3000) "~,3F s" (- time (lis::sim-t sim))))))))

(define-test clock-dropout-to-contact
  ;; At ×1,000 through clock-frame!: every step above ×1 at least 3 s ×
  ;; level before the event (less 0.05 s), ×1 at it, a message at each
  ;; lowering; warp up refused within 3 s of it.
  (let* ((sim (approach-sim))
         (event (lis::prediction-event (run-prediction (lis::copy-sim sim)) :contact))
         (clock (clock-at sim 1000))
         (record '())
         (messages '())
         (refused nil)
         (lis::*frame-step-time-limit* nil))
    (setf (lis::clock-hook clock)
          (lambda (clock s)
            (push (cons (lis::sim-t s) (lis::clock-level clock)) record)
            (when (and (not refused) (< 0 (- event (lis::sim-t s)) 3))
              (setf refused t)
              (false (lis::clock-warp-up clock s))
              (true (find-if (lambda (m) (search "WARP REFUSED: CONTACT IN " m))
                             (lis::clock-messages clock))))
            (setf messages (union messages (lis::clock-messages clock) :test #'string=))
            nil))
    (loop repeat 100000
          until (or (lis::sim-lost-p sim) (> (lis::sim-t sim) (+ event 5)))
          do (lis::clock-frame! clock sim 1/60))
    (true refused)
    (is = 0 (count-if (lambda (r)
                        (and (> (cdr r) 1) (< (- event (car r)) (- (* 3 (cdr r)) 0.05d0))))
                      record))
    (is = 1 (cdr (find-if (lambda (r) (<= (car r) event)) record)))
    (dolist (level '(100 10 4 1))
      (true (find-if (lambda (m) (search (format nil "WARP ×~:D: CONTACT IN " level) m)) messages)
            "×~D in ~S" level messages))))

(define-test clock-checkpoint-contact
  ;; A checkpoint request with a contact predicted takes none.
  (let* ((sim (approach-sim))
         (clock (lis::new-clock sim)))
    (setf (lis::sim-checkpoint-request sim) t)
    (lis::clock-step! clock sim)
    (false (lis::sim-checkpoint-request sim))
    (false (lis::clock-checkpoint clock))))

;;; Milestone 3's form (docs/architecture.md "Trajectory prediction";
;;; decision 116: Prediction in milestone 3: horizon, target nodes, closest
;;; approach, relative nodes, budget): the plan flown by the copy, the
;;; target's nodes, the restart rules and the work budget.

;;; The time per work unit, outside scripts/test (2026-10-15, :full, two
;;; periods; task 198, then after task 260's cuts: the start orbit 1.13, then
;;; 0.65 us per unit, with Tern Station targeted 0.68, then 0.45 us, at Skua
;;; Yard with Skua Yard 3 targeted 0.66, then 0.59 us, 9.2 per step):
#+(or)
(flet ((ns-per-work (sim)
         (let ((pred (lis::new-prediction :periods 2)) (work 0))
           (lis::prediction-run! pred sim)
           (let ((start (get-internal-real-time)))
             (dotimes (i 10)
               (lis::prediction-invalidate! pred)
               (lis::prediction-run! pred sim)
               (incf work (lis::prediction-last-work pred)))
             (round (* 1d9 (/ (- (get-internal-real-time) start) internal-time-units-per-second))
                    work)))))
  (list (ns-per-work (lis::new-game-sim))
        (ns-per-work (let ((s (lis::new-game-sim))) (lis::sim-set-target! s :tern-station) s))
        (ns-per-work (let ((s (skua-yard-sim))) (lis::sim-set-target! s :skua-yard-3) s))))

(defun tern-beside! (sim)
  "Tern Station promoted 1 km behind SIM's ship on its velocity, on J2-free
rails through that state, and targeted; SIM."
  (let* ((object (lis::sim-object sim :tern-station))
         (ship (lis::sim-ship sim))
         (t0 (lis::sim-t sim))
         (v (lis::ship-state-v ship))
         (r (lis::vec3-sub (lis::ship-state-r ship) (lis::vec3-scale (lis::vec3-normalize v) 1000d0))))
    (setf (lis::object-rails object)
          (lis::make-rails (lis::state->orbit r v lis::+mu-earth+ t0) t0 :j2 nil))
    (lis::object-promote! object t0)
    (lis::sim-set-target! sim :tern-station)
    sim))

(defun target-nodes (pred)
  "The stored target nodes from the head, as a fresh array."
  (subseq (lis::prediction-target-nodes pred)
          (* 6 (lis::prediction-head pred)) (* 6 (lis::prediction-target-count pred))))

(defun prediction-event-list (pred)
  (loop for i below (lis::prediction-event-count pred)
        collect (list (svref (lis::prediction-event-kinds pred) i)
                      (aref (lis::prediction-event-times pred) i)
                      (aref (lis::prediction-event-values pred) i))))

(define-test prediction-plan-and-target
  ;; Tern Station beside the ship, the Hohmann plan: every node and target
  ;; node is the flight's at ×1,000, bit for bit; budgeted updates equal it.
  (let* ((sim (tern-beside! (plan-sim)))
         (burns (lis::burn-plan-burns (lis::sim-plan sim)))
         (pred (run-prediction sim :periods 2))
         (n (lis::prediction-node-count pred)))
    (true (lis::prediction-complete-p pred))
    (is = n (lis::prediction-target-count pred))
    (is equal (list (lis::planned-burn-start (aref burns 0)) (lis::planned-burn-start (aref burns 1)))
        (loop for e in (prediction-event-list pred) when (eq (first e) :burn-start) collect (second e)))
    (is equal '(0d0 1d0)
        (loop for e in (prediction-event-list pred) when (eq (first e) :burn-start) collect (third e)))
    (is = 2 (count :cutoff (prediction-event-list pred) :key #'first))
    (multiple-value-bind (time datum value) (lis::prediction-event pred :burn-start)
      (is = (lis::planned-burn-start (aref burns 0)) time)
      (is = 0d0 datum)
      (is = 0d0 value))
    (let ((flown (lis::copy-sim sim))
          (r (lis::make-vec3)) (v (lis::make-vec3))
          (rt (lis::make-vec3)) (vt (lis::make-vec3))
          (mismatches 0))
      (loop for i from 1 below n
            do (step-sim flown 1 1000)
               (let ((time (lis::prediction-node pred i r v)))
                 (lis::prediction-target-node pred i rt vt)
                 (multiple-value-bind (fr fv) (values (lis::vec3-copy (lis::ship-state-r (lis::sim-ship flown)))
                                                      (lis::vec3-copy (lis::ship-state-v (lis::sim-ship flown))))
                   (let ((tr (lis::make-vec3)) (tv (lis::make-vec3)))
                     (lis::target-state-into! tr tv flown)
                     (unless (and (= time (lis::sim-t flown)) (equalp r fr) (equalp v fv)
                                  (equalp rt tr) (equalp vt tv))
                       (incf mismatches))))))
      (is = 0 mismatches)
      ;; Promoted beside the ship, demoted once the Hohmann carries it off.
      (is eq :rails (lis::object-status (lis::sim-object flown :tern-station)))
      (is eq :done (lis::sim-plan-phase flown)))
    (let ((framed (lis::new-prediction :periods 2))
          (frames 0))
      (loop do (lis::prediction-update! framed sim 300)
               (incf frames)
               (true (<= (lis::prediction-last-work framed) 300))
            until (lis::prediction-complete-p framed))
      (true (> frames 10))
      (is = 1 (lis::prediction-restarts framed))
      (true (equalp (prediction-nodes pred) (prediction-nodes framed)))
      (true (equalp (target-nodes pred) (target-nodes framed)))
      (is equal (prediction-event-list pred) (prediction-event-list framed)))))

(define-test prediction-horizons
  ;; Coasting two periods (the clock's), under thrust one.
  (flet ((span (sim)
           (let* ((pred (lis::prediction-run! (lis::new-prediction :periods 2) sim))
                  (period (lis::osculating-period sim))
                  (n (lis::prediction-node-count pred)))
             (list (/ (- (lis::prediction-node pred (- n 1)) (lis::sim-t sim)) period)
                   (/ (- (lis::prediction-node pred (- n 2)) (lis::sim-t sim)) period)))))
    (let ((sim (lis::new-game-sim)))
      (setf (lis::sim-model sim) :point)
      (destructuring-bind (last before) (span sim)
        (true (and (>= last 2) (< before 2)) "~F ~F" last before))
      (lis::controls-set-throttle (lis::sim-controls sim) 0.1d0)
      (setf (lis::sim-cutoff sim) 5d0)
      (destructuring-bind (last before) (span sim)
        (true (and (>= last 1) (< before 1)) "~F ~F" last before)))
    (is = 2 (lis::prediction-periods (lis::clock-prediction (lis::new-clock (lis::new-game-sim)))))))

(define-test prediction-target-restarts
  ;; A target or plan change restarts, the target moved 2 m by hand too.
  (flet ((restarts-after (change)
           (let ((sim (tern-beside! (plan-sim)))
                 (pred (lis::new-prediction)))
             (lis::prediction-run! pred sim)
             (frames pred sim 2)
             (let ((before (lis::prediction-restarts pred)))
               (funcall change sim)
               (lis::prediction-update! pred sim)
               (- (lis::prediction-restarts pred) before))))
         (tern-r (sim)
           (lis::ship-state-r (lis::object-state (lis::sim-object sim :tern-station)))))
    (is = 0 (restarts-after (lambda (sim) sim)))
    (is = 1 (restarts-after (lambda (sim) (lis::sim-set-target! sim :fulmar-depot))))
    (is = 1 (restarts-after (lambda (sim) (lis::sim-set-target! sim nil))))
    (is = 1 (restarts-after (lambda (sim) (lis::sim-set-plan! sim (hohmann-plan sim)))))
    (is = 1 (restarts-after (lambda (sim) (lis::sim-set-plan! sim nil))))
    (is = 1 (restarts-after (lambda (sim) (incf (aref (tern-r sim) 1) 2d0))))
    (is = 0 (restarts-after (lambda (sim) (incf (aref (tern-r sim) 1) 0.1d0))))))

(define-test prediction-target-cons-nothing
  ;; Tern Station on rails 2,370 km ahead: a two-period prediction conses
  ;; nothing after the first.
  (let ((sim (lis::new-game-sim))
        (pred (lis::new-prediction :periods 2)))
    (setf (lis::sim-model sim) :j2)
    (lis::sim-set-target! sim :tern-station)
    (lis::prediction-run! pred sim)
    (is = (lis::prediction-node-count pred) (lis::prediction-target-count pred))
    (let ((consed (least-bytes-consed (lambda ()
                                        (lis::prediction-invalidate! pred)
                                        (lis::prediction-run! pred sim)))))
      (true (< consed 1024) "~D bytes" consed))))

(define-test prediction-work-budget
  ;; At Skua Yard, nine objects promoted and Skua Yard 3 targeted: each
  ;; update's work within the budget, ten per step.
  (let ((sim (skua-yard-sim))
        (pred (lis::new-prediction :periods 2))
        (budget lis::*prediction-work-per-frame*))
    (setf (lis::sim-model sim) :j2)
    (lis::sim-set-target! sim :skua-yard-3)
    (lis::prediction-update! pred sim)
    (is = (* 10 (lis::prediction-last-steps pred)) (lis::prediction-last-work pred))
    (true (>= (lis::prediction-last-work pred) (- budget 11)))
    (loop repeat 100
          until (lis::prediction-complete-p pred)
          do (lis::prediction-update! pred sim)
             (true (<= (lis::prediction-last-work pred) budget)))
    (true (lis::prediction-complete-p pred))))

(define-test prediction-rcs-burn
  ;; A planned RCS burn: the copy flies it at ×4, fine steps, and marks its
  ;; start.
  (let* ((sim (lis::new-game-sim))
         (t0 (lis::sim-t sim)))
    (setf (lis::sim-model sim) :point)
    (lis::sim-set-plan!
     sim (lis::make-burn-plan
          (vector (lis::make-planned-burn :trim (+ t0 30) (+ t0 60)
                                          (lis::quat-copy (lis::ship-state-q (lis::sim-ship sim)))
                                          0d0 1 0.3d0))))
    (let* ((pred (run-prediction sim))
           (n (lis::prediction-node-count pred))
           (times (loop for i below n collect (lis::prediction-node pred i))))
      (is = (+ t0 60) (lis::prediction-event pred :burn-start))
      (is eq :done (lis::sim-plan-phase (lis::prediction-sim pred)))
      ;; From the burn's start, fine steps until the burn ends.
      (let ((burn (member (+ t0 60) times)))
        (true burn)
        (true (every (lambda (a b) (= (- b a) 1/64)) burn (subseq burn 1 64)))))))

;;; Closest approach and relative nodes (decision 116), against brute force
;;; and Kepler.

(defun rails-target! (sim key r v)
  "Object KEY on J2-free rails through R, V at SIM's t, targeted; its orbit."
  (let* ((t0 (lis::sim-t sim))
         (orbit (lis::state->orbit r v lis::+mu-earth+ t0)))
    (setf (lis::object-rails (lis::sim-object sim key)) (lis::make-rails orbit t0 :j2 nil))
    (lis::sim-set-target! sim key)
    orbit))

(defun kepler-target! (sim key)
  "Object KEY on J2-free rails through its state now, targeted; its orbit."
  (multiple-value-bind (r v) (object-rv (lis::sim-object sim key) (lis::sim-t sim))
    (rails-target! sim key r v)))

(defun co-orbital-state (sim distance)
  "(values r v) on the Kepler orbit of SIM's ship, DISTANCE m ahead of it."
  (let* ((ship (lis::sim-ship sim))
         (t0 (lis::sim-t sim)))
    (lis::orbit-state (lis::state->orbit (lis::ship-state-r ship) (lis::ship-state-v ship)
                                         lis::+mu-earth+ t0)
                      (+ t0 (/ distance (lis::vec3-norm (lis::ship-state-v ship)))))))

(defun fulmar-sim ()
  "The start orbit, :point, raised by 30 m/s prograde to an apoapsis near
Fulmar Depot's 500 km."
  (let* ((sim (lis::new-game-sim))
         (v (lis::ship-state-v (lis::sim-ship sim))))
    (setf (lis::sim-model sim) :point)
    (lis::vec3-add-scaled! v v (lis::vec3-normalize v) 30d0)
    sim))

(defun events-of (pred &rest kinds)
  "(time value) of PRED's events of KINDS, in order."
  (loop for (kind time value) in (prediction-event-list pred)
        when (member kind kinds) collect (list time value)))

(defun scanned-approaches (pred orbit)
  "The interior local minima (time distance) of the range between PRED's
stored ship path and ORBIT's Kepler state, sampled every 1/4 s, each
refined to the vertex of the parabola through its three samples. (Sampled
every 1/16 s without the parabola until task 261: 4x the samples.)"
  (let* ((n (lis::prediction-node-count pred))
         (t0 (lis::prediction-node pred 0))
         (t1 (lis::prediction-node pred (1- n)))
         (h 0.25d0)
         (p (lis::make-vec3))
         (minima '()))
    (flet ((range (time)
             (lis::prediction-position! p pred time)
             (lis::vec3-norm (lis::vec3-sub! p p (lis::orbit-state orbit time)))))
      (loop with a = (range t0) and b = (range (+ t0 h))
            for k from 2 to (floor (- t1 t0) h)
            for time = (+ t0 (* k h))
            for c = (range time)
            do (when (and (< b a) (<= b c))
                 (let ((vertex (+ (- time h) (/ (* h 0.5d0 (- a c)) (+ a (* -2 b) c)))))
                   (push (list vertex (range vertex)) minima)))
               (setf a b b c)))
    (nreverse minima)))

(defun check-approaches (sim orbit label)
  (let* ((pred (run-prediction sim :periods 2))
         (found (events-of pred :closest-approach))
         (scanned (scanned-approaches pred orbit)))
    (true scanned "~A: a minimum to compare" label)
    (is = (length scanned) (length found) "~A: ~S ~S" label scanned found)
    (loop for (time distance) in scanned
          for (found-time found-distance) in found
          do (true (< (abs (- time found-time)) 0.05d0) "~A: ~,4F s off" label (- found-time time))
             (true (< (abs (- distance found-distance)) (max 1d0 (* 1d-3 distance)))
                   "~A: ~,3F m off at ~,1F m" label (- found-distance distance) distance))
    (let ((least (reduce #'min found :key #'second :initial-value most-positive-double-float)))
      (multiple-value-bind (time distance) (lis::prediction-closest-approach pred)
        (is = least distance)
        (is = (first (find least found :key #'second)) time)))
    pred))

(define-test prediction-closest-approach
  ;; Fulmar Depot from the start orbit raised 30 m/s, two-body; Tern
  ;; Station 10 km ahead on the start orbit after a 5 m/s radial burn. (From
  ;; the start orbit itself neither range has a minimum in two periods.)
  (let ((sim (fulmar-sim)))
    (check-approaches sim (kepler-target! sim :fulmar-depot) "Fulmar Depot"))
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim))
         (v (lis::ship-state-v ship)))
    (setf (lis::sim-model sim) :point)
    (let ((orbit (multiple-value-call #'rails-target! sim :tern-station
                   (co-orbital-state sim 10000d0))))
      (lis::vec3-add-scaled! v v (lis::vec3-normalize (lis::ship-state-r ship)) 5d0)
      (check-approaches sim orbit "Tern Station"))))

(define-test prediction-co-orbital
  ;; Tern Station 1 km ahead on the ship's orbit: no local minimum.
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-model sim) :point)
    (multiple-value-call #'rails-target! sim :tern-station (co-orbital-state sim 1000d0))
    (let ((pred (run-prediction sim :periods 2)))
      (true (lis::prediction-complete-p pred))
      (false (events-of pred :closest-approach))
      (false (lis::prediction-closest-approach pred)))))

(define-test prediction-relative-nodes
  ;; A target on the start orbit turned 1° in Ω, two-body: the nodes where
  ;; Kepler crosses its plane, alternating.
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim))
         (t0 (lis::sim-t sim))
         (orbit (lis::state->orbit (lis::ship-state-r ship) (lis::ship-state-v ship)
                                   lis::+mu-earth+ t0))
         (turn (lis::quat-from-axis-angle (lis::vec3 0 0 1) (lis::deg->rad 1d0))))
    (setf (lis::sim-model sim) :point)
    (let ((normal (multiple-value-bind (r v) (lis::orbit-state orbit (+ t0 2000))
                    (let ((r (lis::quat-rotate turn r)) (v (lis::quat-rotate turn v)))
                      (rails-target! sim :tern-station r v)
                      (lis::vec3-cross r v)))))
      (flet ((height (time) (lis::vec3-dot (lis::orbit-state orbit time) normal)))
        (let* ((pred (run-prediction sim :periods 2))
               (t1 (lis::prediction-node pred (1- (lis::prediction-node-count pred))))
               (found (loop for (kind time value) in (prediction-event-list pred)
                            when (member kind '(:relative-ascending :relative-descending))
                              collect (list kind time value)))
               (kepler (loop for start from t0 below t1
                             for end = (min t1 (1+ start))
                             when (/= (signum (height start)) (signum (height end)))
                               collect (let ((rising (minusp (height start)))
                                             (a start) (b end))
                                         (loop while (> (- b a) 1d-6)
                                               do (let ((m (* 0.5d0 (+ a b))))
                                                    (if (eq (minusp (height m)) rising)
                                                        (setf a m)
                                                        (setf b m))))
                                         (list (if rising :relative-ascending :relative-descending)
                                               a)))))
          (true (>= (length kepler) 3))
          (is = (length kepler) (length found))
          (loop for (kind time) in kepler
                for (found-kind found-time angle) in found
                do (is eq kind found-kind)
                   (true (< (abs (- time found-time)) 1d-3) "~,6F s off" (- found-time time))
                   (true (< (abs (- angle (lis::deg->rad 1d0))) 1d-4) "~F rad" angle))
          (true (loop for (a b) on found while b never (eq (first a) (first b))))
          (multiple-value-bind (time angle) (lis::prediction-relative-node pred :descending)
            (is = (second (find :relative-descending found :key #'first)) time)
            (true angle))))))
  ;; The coplanar start with Tern Station: none.
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-model sim) :j2)
    (lis::sim-set-target! sim :tern-station)
    (let ((pred (run-prediction sim :periods 2)))
      (is = (lis::prediction-node-count pred) (lis::prediction-target-count pred))
      (false (events-of pred :relative-ascending :relative-descending))
      (false (lis::prediction-relative-node pred :ascending)))))

(define-test prediction-events-ordered
  ;; Fulmar Depot targeted: apsides and approaches in time order, dropped
  ;; once the flown t passes them.
  (let* ((sim (fulmar-sim))
         (pred (lis::new-prediction :periods 2)))
    (kepler-target! sim :fulmar-depot)
    (lis::prediction-run! pred sim)
    (let ((events (prediction-event-list pred)))
      (true (>= (length events) 5))
      (true (apply #'<= (mapcar #'second events)))
      (let ((passed (second (third events))))
        (step-sim sim 100000 1000 (+ passed 1))
        (lis::prediction-update! pred sim)
        (is = 1 (lis::prediction-restarts pred))
        (let ((now (prediction-event-list pred)))
          (true (every (lambda (e) (> (second e) (lis::sim-t sim))) now))
          (true (apply #'<= (mapcar #'second now)))
          (is equal (nthcdr 3 events) (subseq now 0 (- (length events) 3))))))))
