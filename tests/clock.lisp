(in-package #:luv-in-space.tests)

;;; clock.lisp: the simulation state, the start state, sim-step! and
;;; checkpoints (docs/architecture.md "The clock", "Checkpoints"; decision
;;; 72: Simulation state and the one step function in clock.lisp).

(defun step-sim (sim n level &optional until)
  "N sim-step!s of SIM at LEVEL, fewer if it reaches UNTIL or is lost; SIM."
  (declare (type fixnum n level) (inline lis::sim-step!))
  (dotimes (i n sim)
    (when (or (and until (>= (lis::sim-t sim) until)) (lis::sim-loss sim))
      (return sim))
    (lis::sim-step! sim level :until until)))

(defun on-grid-p (time)
  (zerop (mod (rational time) 1/64)))

(defun angle-between (a b)
  (atan (lis::vec3-norm (lis::vec3-cross a b)) (lis::vec3-dot a b)))

(define-test clock-start-state
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim))
         (r (lis::ship-state-r ship))
         (v (lis::ship-state-v ship))
         (q (lis::ship-state-q ship))
         (orbit (lis::state->orbit r v lis::+mu-earth+ (lis::sim-t sim)))
         (n (lis::orbit-n orbit)))
    (is = (lis::start-t) (lis::sim-t sim))
    (is eq :earth (lis::sim-body sim))
    (is eq :full (lis::sim-model sim))
    (is = 28000d0 (lis::ship-state-m-p ship))
    (true (< (relative-error (lis::orbit-a orbit) 6778136.6d0) 1d-9))
    (true (< (lis::orbit-e orbit) 1d-12))
    (true (< (relative-error (lis::orbit-i orbit) (/ pi 2)) 1d-9))
    (true (< (relative-error (lis::orbit-raan orbit) (* 3/2 pi)) 1d-9))
    ;; u = ω + θ; ω = 0 for a circle.
    (true (< (abs (lis::wrap-signed (+ (lis::orbit-argp orbit)
                                       (lis::orbit-true-anomaly orbit (lis::sim-t sim)))))
             1d-9))
    (true (< (angle-between (lis::quat-rotate q (lis::vec3 1 0 0)) v) 1d-12))
    (true (< (angle-between (lis::quat-rotate q (lis::vec3 0 0 -1)) r) 1d-12))
    (true (< (relative-error (lis::vec3-norm (lis::ship-state-w ship)) n) 1d-12))
    (let ((c (lis::sim-controls sim)))
      (is eq :hold (lis::controls-attitude-mode c))
      (is eq :prograde (lis::controls-hold c))
      (true (lis::controls-settled c))
      (is = 0d0 (lis::controls-throttle c)))
    (false (lis::sim-cutoff sim))
    (false (lis::sim-loss sim))
    (true (lis::checkpoint-safe-p sim))))

(define-test clock-copy-and-assign
  (let* ((sim (lis::new-game-sim))
         (copy (lis::copy-sim sim)))
    (true (equalp sim copy))
    (false (eq (lis::sim-ship sim) (lis::sim-ship copy)))
    (false (eq (lis::sim-controls sim) (lis::sim-controls copy)))
    (step-sim copy 3 1)
    (lis::controls-throttle-full (lis::sim-controls copy))
    (setf (lis::sim-cutoff copy) 10d0)
    (false (equalp sim copy))
    (false (lis::checkpoint-safe-p copy))
    (lis::sim-assign! copy sim)
    (true (equalp sim copy))
    (false (eq (lis::sim-ship sim) (lis::sim-ship copy)))))

(defun grappled-sim ()
  "The start state with every field of decision 112 set."
  (let* ((sim (lis::new-game-sim))
         (grapple (lis::sim-grapple sim)))
    (setf (lis::grapple-state grapple) :closing
          (lis::grapple-object grapple) 1
          (lis::grapple-fixture grapple) 0
          (lis::grapple-closing-end grapple) (+ (lis::sim-t sim) 2)
          (lis::sim-berthed sim) t
          (lis::sim-transfer sim) t
          (lis::sim-excluded sim) 2
          (lis::sim-delivery sim) (list 4 3 0 (lis::sim-t sim))
          (lis::sim-notices sim) (list (list :removal "K-17 BURNED UP"))
          (lis::sim-checkpoint-request sim) t)
    (replace (lis::grapple-com grapple) '(17.2d0 0.1d0 -0.2d0))
    (replace (lis::grapple-q grapple) '(0.6d0 0.8d0 0d0 0d0))
    sim))

(define-test clock-copy-grapple-fields
  (let* ((sim (grappled-sim))
         (copy (lis::copy-sim sim))
         (fresh (lis::new-game-sim)))
    (true (equalp sim copy))
    (false (eq (lis::sim-grapple sim) (lis::sim-grapple copy)))
    (false (equalp sim fresh))
    (setf (aref (lis::grapple-com (lis::sim-grapple copy)) 0) 17.3d0)
    (false (equalp sim copy))
    (is = 0 (least-bytes-consed (lambda () (lis::sim-assign! copy sim))))
    (true (equalp sim copy))
    (is = 0 (least-bytes-consed (lambda () (lis::sim-assign! copy fresh))))
    (true (equalp copy fresh))))

(define-test clock-notices-and-log
  ;; K-17 burns up on rails 2,603 s out: the prediction's copy sees it
  ;; first and says nothing; the flown step's notice is a message and a
  ;; log entry at its t, once.
  (let* ((sim (k-17-sim 90d3))
         (clock (clock-at sim 1000))
         (end (k-17-crash-end sim)))
    (lis::prediction-run! (lis::clock-prediction clock) sim)
    (true (lis::object-removed-p
           (lis::sim-object (lis::prediction-sim (lis::clock-prediction clock)) :k-17)))
    (false (lis::sim-notices (lis::prediction-sim (lis::clock-prediction clock))))
    (false (lis::clock-log clock))
    (loop until (lis::object-removed-p (lis::sim-object sim :k-17))
          do (lis::clock-step! clock sim))
    (true (>= (lis::sim-t sim) end))
    (false (lis::sim-notices sim))
    (is equal '("K-17 BURNED UP") (lis::clock-messages clock))
    (is equal (list (list (lis::sim-t sim) "K-17 BURNED UP")) (lis::clock-log clock))
    (dotimes (i 100) (lis::clock-step! clock sim))
    (is = 1 (length (lis::clock-log clock)))
    ;; The read mark; the log keeps the last 200.
    (is = 1 (length (lis::clock-log-unread clock)))
    (lis::clock-acknowledge! clock)
    (false (lis::clock-log-unread clock))
    (dotimes (i 250) (lis::clock-message! clock :refused (format nil "M~D" i)))
    (is = 200 (length (lis::clock-log clock)))
    (is = 200 (length (lis::clock-log-unread clock)))
    (is equal "M249" (second (first (lis::clock-log clock))))))

(define-test clock-checkpoint-request
  (let* ((sim (lis::new-game-sim))
         (clock (lis::new-clock sim)))
    (setf (lis::sim-checkpoint-request sim) t)
    (lis::clock-step! clock sim)
    (false (lis::sim-checkpoint-request sim))
    (true (equalp sim (lis::clock-checkpoint clock)))
    (false (eq sim (lis::clock-checkpoint clock)))
    ;; Not with the throttle on: the old one stays.
    (let ((old (lis::clock-checkpoint clock)))
      (lis::controls-throttle-full (lis::sim-controls sim))
      (setf (lis::sim-checkpoint-request sim) t)
      (lis::clock-step! clock sim)
      (false (lis::sim-checkpoint-request sim))
      (is eq old (lis::clock-checkpoint clock))
      ;; clock-restart! keeps it.
      (lis::clock-restart! clock sim old)
      (is eq old (lis::clock-checkpoint clock))
      (true (equalp sim old)))))

(define-test clock-grid-and-until
  ;; Blocks of 50 steps per level; targets an odd number of ticks ahead.
  (let ((sim (lis::new-game-sim))
        (levels #(1 1000 4 100000 10 1))
        (target nil) (landed 0) (off-grid 0) (overshot 0))
    (setf (lis::sim-model sim) :point)
    (dotimes (i 1000)
      (unless target
        (setf target (+ (lis::sim-t sim) (/ (1+ (* 2 (mod (* i 37) 300))) 64d0))))
      (step-sim sim 1 (aref levels (mod (floor i 50) 6)) target)
      (unless (on-grid-p (lis::sim-t sim)) (incf off-grid))
      (cond ((= (lis::sim-t sim) target) (incf landed) (setf target nil))
            ((> (lis::sim-t sim) target) (incf overshot) (setf target nil))))
    (is = 0 off-grid)
    (is = 0 overshot)
    (true (> landed 20) "~D landed" landed)
    (is = 1000 (lis::sim-steps sim))))

(defun cutoff-burn (level)
  "A prograde full-throttle burn from the start with a 25 m/s cut-off at
LEVEL (:point): (values cut time since the start, Δv, the last fine step's
engine Δv)."
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim)))
    (setf (lis::sim-model sim) :point
          (lis::sim-cutoff sim) 25d0)
    (lis::controls-throttle-full (lis::sim-controls sim))
    (loop while (lis::sim-cutoff sim)
          do (step-sim sim 1 level))
    (is = 0d0 (lis::controls-throttle (lis::sim-controls sim)))
    (values (- (lis::sim-t sim) (lis::start-t))
            (lis::ship-state-dv ship)
            (/ (* lis::*engine-thrust* lis::+fine-step+)
               (+ lis::*ship-dry-mass* (lis::ship-state-m-p ship))))))

(define-test clock-cutoff
  (multiple-value-bind (t1 dv1 fine-dv) (cutoff-burn 1)
    (multiple-value-bind (t100 dv100) (cutoff-burn 100)
      (true (on-grid-p t1))
      (true (on-grid-p t100))
      (dolist (dv (list dv1 dv100))
        (true (<= 25d0 dv) "~F" dv)
        (true (< dv (+ 25d0 fine-dv)) "~F" dv))
      (true (<= (abs (- t1 t100)) 1/64) "~F ~F" t1 t100))))

(defun mixed-run (sim n)
  "N steps alternating 200 at ×1 and 200 at ×1,000, with a burn, a held
pitch key (releasing the hold, so the next coarse step engages an
inertial hold) and a cut-off; SIM."
  (dotimes (i n sim)
    (let ((c (lis::sim-controls sim)))
      (case i
        (20 (lis::controls-set-throttle c 0.5d0))
        (60 (lis::controls-throttle-cut c))
        (80 (lis::controls-set-rotation-key c :pitch 1))
        (120 (lis::controls-set-rotation-key c :pitch 0))
        (500 (lis::controls-throttle-full c)
         (setf (lis::sim-cutoff sim) (+ (lis::ship-state-dv (lis::sim-ship sim)) 5d0)))))
    (step-sim sim 1 (if (< (mod i 400) 200) 1 1000))))

(define-test clock-checkpoint
  (let* ((sim (step-sim (lis::new-game-sim) 50 1))
         (checkpoint (lis::take-checkpoint sim))
         (flown (lis::copy-sim (mixed-run sim 2000))))
    (true (eq :inertial (lis::controls-hold (lis::sim-controls flown))))
    (false (equalp flown checkpoint))
    (let ((consed (least-bytes-consed
                   (lambda () (lis::restore-checkpoint! sim checkpoint)))))
      (is = 0 consed))
    (true (equalp sim checkpoint))
    (mixed-run sim 2000)
    (true (equalp sim flown))))

(define-test clock-polar-node
  ;; docs/architecture.md "Rails with J2", decision 38: Start orbit: polar.
  (let* ((sim (lis::new-game-sim))
         (end (+ (lis::start-t) (* 7 lis::+day+))))
    (setf (lis::sim-model sim) :j2)
    (loop while (< (lis::sim-t sim) end)
          do (step-sim sim 1000 1000 end))
    (is = end (lis::sim-t sim))
    (let* ((ship (lis::sim-ship sim))
           (orbit (lis::state->orbit (lis::ship-state-r ship) (lis::ship-state-v ship)
                                     lis::+mu-earth+ (lis::sim-t sim))))
      (true (< (abs (- (lis::rad->deg (lis::orbit-raan orbit)) 270)) 1d-6)
            "Ω ~F°" (lis::rad->deg (lis::orbit-raan orbit)))
      (true (< (abs (lis::orbit-j2-rates orbit :earth)) 1d-15)))))

(define-test clock-step-cons-nothing
  (let* ((a (lis::new-game-sim))
         (b (lis::copy-sim a)))
    ;; The first run fills the ephemeris cache.
    (dotimes (i 3) (step-sim (step-sim a 1667 1) 1667 1000))
    (let ((consed (least-bytes-consed
                   (lambda () (step-sim (step-sim b 1667 1) 1667 1000)))))
      (true (< consed 1024) "~D bytes" consed))
    (true (equalp a b))))

;;; The clock: frames, warp levels, pause, the actual rate (docs/architecture.md
;;; "The clock", "Drift bounds" through the clock; decision 72).

(defun drift-sim ()
  "The circular test orbit of \"Drift bounds\" as a sim: :point, direct
mode, no keys, at rest."
  (let ((sim (lis::new-game-sim)))
    (multiple-value-bind (r v) (lis::orbit-state (drift-orbit 0d0) (lis::start-t))
      (lis::ship-state-assign! (lis::sim-ship sim)
                               (lis::make-ship-state r v (lis::quat 1 0 0 0) (lis::make-vec3)
                                                     28000d0)))
    (setf (lis::sim-model sim) :point
          (lis::controls-attitude-mode (lis::sim-controls sim)) :direct)
    sim))

(defun drift-period () (lis::orbit-period (drift-orbit 0d0)))

(defun level-index (level) (position level lis::*warp-levels*))

(defun clock-run (sim level end frames &key (hook-until nil))
  "A clock at LEVEL flies SIM to END, fed the elapsed times FRAMES in turn
(cycled); before every step SIM is compared with a direct sim-step! loop
at LEVEL. (values mismatches checks steps-per-frame clock)."
  (let ((direct (lis::copy-sim sim))
        (clock (lis::new-clock sim))
        (mismatches 0) (checks 0) (per-frame '())
        (lis::*frame-step-time-limit* nil)
        ;; The drift orbit has no drop-out event; a short prediction saves time.
        (lis::*prediction-work-per-frame* 64))
    (declare (inline lis::sim-step!))
    (setf (lis::clock-hook clock)
          (lambda (clock s)
            (declare (ignore clock))
            (loop while (< (lis::sim-t direct) (lis::sim-t s))
                  do (lis::sim-step! direct level :until hook-until))
            (incf checks)
            (unless (equalp direct s) (incf mismatches))
            hook-until))
    (lis::clock-set-level-index! clock (level-index level))
    (loop with pending = frames
          while (< (lis::sim-t sim) end)
          do (when (null pending) (setf pending frames))
             (push (lis::clock-frame! clock sim (pop pending)) per-frame))
    (loop while (< (lis::sim-t direct) (lis::sim-t sim))
          do (lis::sim-step! direct level :until hook-until))
    (unless (equalp direct sim) (incf mismatches))
    (values mismatches checks (nreverse per-frame) clock)))

(define-test clock-levels-equal-direct-steps
  ;; Shortened as plan 15 allows, to keep the part's tests under 1.5 s: ×1
  ;; and ×4 over 120 s (not 600 s), the coarse levels over 3 orbits (not 10;
  ;; 5 until task 261 cut it for scripts/test's budget; ×1,000,000 still
  ;; fills two frames at the cap).
  (dolist (level lis::*warp-levels*)
    (let* ((sim (drift-sim))
           (end (+ (lis::sim-t sim) (if (<= level 4) 120d0 (* 3 (drift-period))))))
      (multiple-value-bind (mismatches checks per-frame) (clock-run sim level end '(1/60))
        (is = 0 mismatches "×~D" level)
        (true (> checks 100) "×~D: ~D checks" level checks)
        (true (>= (lis::sim-t sim) end))
        (when (= level 1000000)
          (true (every (lambda (n) (= n 4096)) (butlast per-frame)) "~A" per-frame))))))

(defparameter *irregular-frames*
  '(0.016d0 0.5d0 0.007d0 0.0331d0 1d-4 0.02d0 0.1d0 0.25d0 0.0123d0 0.04d0)
  "Elapsed times; 0.5 and 0.25 are clamped to 0.1.")

(define-test clock-frame-rate-changes-nothing
  (dolist (level '(1 4 1000))
    (let ((end (+ (lis::start-t) (if (<= level 4) 60d0 (drift-period)))))
      (dolist (frames (list '(1/60) '(1/144) *irregular-frames*))
        (let ((sim (drift-sim)))
          (is = 0 (clock-run sim level end frames) "×~D ~A" level frames))))))

(define-test clock-warp-change
  (let* ((sim (drift-sim))
         (clock (lis::new-clock sim))
         (lis::*frame-step-time-limit* nil))
    (dotimes (i 10) (lis::clock-frame! clock sim 1/60))
    (let ((copy (lis::copy-sim sim))
          (t0 (lis::sim-t sim)))
      (dotimes (i 3)
        (true (lis::clock-warp-up clock sim))
        (true (equalp copy sim)))
      (is = 100 (lis::clock-level clock))
      (true (lis::clock-kinematic-p clock))
      ;; ≤ 1/64 s owed at ×1 is ≤ 1.6 s at ×100: 10 to 11.6 s, five 2 s steps.
      (is = 5 (lis::clock-frame! clock sim 0.1d0))
      (is = (* 5 (lis::coarse-step (lis::ship-state-r (lis::sim-ship copy)) t0 nil :earth))
          (- (lis::sim-t sim) t0))
      (lis::clock-warp-reset clock)
      (is = 1 (lis::clock-level clock))
      (lis::clock-warp-down clock)
      (is = 1 (lis::clock-level clock)))))

(define-test clock-warp-round-trip
  ;; ×1 → ×1,000 → ×1 over half an orbit, against ×1 throughout (one orbit
  ;; until task 261 halved it for scripts/test's budget: the ×1 reference
  ;; dominates).
  (let* ((reference (drift-sim))
         (sim (drift-sim))
         (clock (lis::new-clock sim))
         (end (lis::grid-ceiling (+ (lis::start-t) (* 0.5d0 (drift-period)))))
         (lis::*frame-step-time-limit* nil))
    (dotimes (i 60) (lis::clock-frame! clock sim 1/60))
    (lis::clock-set-level-index! clock (level-index 1000))
    (loop while (< (lis::sim-t sim) (- end 100)) do (lis::clock-frame! clock sim 1/60))
    (lis::clock-warp-reset clock)
    (loop while (< (lis::sim-t sim) end) do (lis::clock-frame! clock sim 0.1d0))
    (step-sim reference most-positive-fixnum 1 (lis::sim-t sim))
    (is = (lis::sim-t sim) (lis::sim-t reference))
    (let ((d (lis::vec3-norm (lis::vec3-sub (lis::ship-state-r (lis::sim-ship sim))
                                            (lis::ship-state-r (lis::sim-ship reference))))))
      (true (< d 5d0) "~F m" d))))

(define-test clock-pause
  (let* ((sim (drift-sim))
         (clock (lis::new-clock sim))
         (lis::*frame-step-time-limit* nil))
    (lis::clock-warp-up clock sim)
    (lis::clock-frame! clock sim 1/60)
    (let ((copy (lis::copy-sim sim)))
      (lis::clock-toggle-pause clock)
      (dotimes (i 100)
        (is = 0 (lis::clock-frame! clock sim 1/60)))
      (true (equalp copy sim))
      (is = 0d0 (lis::clock-accumulator clock))
      (lis::clock-toggle-pause clock)
      (is = 4 (lis::clock-level clock))
      (true (> (lis::clock-frame! clock sim 1/60) 0)))))

(define-test clock-actual-rate
  (let* ((sim (drift-sim))
         (clock (lis::new-clock sim))
         (h (lis::coarse-step (lis::ship-state-r (lis::sim-ship sim)) (lis::sim-t sim) nil :earth))
         (expected (* 4096 h 60))
         (lis::*frame-step-time-limit* nil)
         (lis::*prediction-work-per-frame* 64))
    (lis::clock-set-level-index! clock (level-index 1000000))
    (dotimes (i 59) (lis::clock-frame! clock sim 1/60))
    (false (lis::clock-actual-rate clock) "under a second metered")
    (dotimes (i 2) (lis::clock-frame! clock sim 1/60))
    (let ((rate (lis::clock-actual-rate clock)))
      (true (< (relative-error rate expected) 0.01) "~F against ~F" rate expected))))

;;; Warp refusals, per-step lowering, drop-outs, messages, restart
;;; (docs/architecture.md "Time warp", "Warp, events and collisions";
;;; decision 73: Warp levels above x4: entry, holds and lowering).

(defun clock-at (sim level)
  (let ((clock (lis::new-clock sim)))
    (lis::clock-set-level-index! clock (level-index level))
    clock))

(defun messages-p (clock &rest texts)
  "CLOCK's live messages are TEXTS."
  (equal texts (lis::clock-messages clock)))

(define-test clock-warp-refusals
  (let ((lis::*frame-step-time-limit* nil))
    ;; Thrust: ×10 manual, ×100 with a cut-off.
    (let* ((sim (lis::new-game-sim))
           (clock (clock-at sim 10)))
      (lis::controls-throttle-full (lis::sim-controls sim))
      (false (lis::clock-warp-up clock sim))
      (is = 10 (lis::clock-level clock))
      (true (messages-p clock "WARP REFUSED: ENGINE ON, LIMIT ×10"))
      (setf (lis::sim-cutoff sim) 100d0)
      (true (lis::clock-warp-up clock sim))
      (is = 100 (lis::clock-level clock))
      (false (lis::clock-warp-up clock sim))
      (is = 100 (lis::clock-level clock))
      (true (messages-p clock "WARP REFUSED: ENGINE ON, LIMIT ×100")))
    ;; Rotating under rate damping: refused at 0.02 rad/s, allowed at 0.005
    ;; with an inertial hold engaged at the first coarse step.
    (let* ((sim (lis::new-game-sim))
           (controls (lis::sim-controls sim))
           (w (lis::ship-state-w (lis::sim-ship sim)))
           (clock (clock-at sim 4)))
      (lis::controls-toggle-rate-damping controls)
      (replace w '(0d0 0d0 0.02d0))
      (false (lis::clock-warp-up clock sim))
      (is = 4 (lis::clock-level clock))
      (true (messages-p clock "WARP REFUSED: ROTATING, SET A HOLD"))
      (replace w '(0d0 0d0 0.005d0))
      (true (lis::clock-warp-up clock sim))
      (is = 10 (lis::clock-level clock))
      (loop while (= (lis::sim-steps sim) 0) do (lis::clock-frame! clock sim 1/60))
      (is eq :hold (lis::controls-attitude-mode controls))
      (is eq :inertial (lis::controls-hold controls))
      (is = 10 (lis::clock-level clock)))
    ;; An impact 20 s ahead: ×4 → ×10 refused (30 s > 20 s).
    (let* ((orbit (dive-orbit (- *earth-crash-radius* 50d3)))
           (impact (lis::orbit-radius-crossings orbit *earth-crash-radius* (lis::start-t)))
           (sim (orbit-sim orbit (lis::grid-ceiling (- impact 20d0))))
           (clock (clock-at sim 4)))
      (lis::prediction-run! (lis::clock-prediction clock) sim)
      (false (lis::clock-warp-up clock sim))
      (is = 4 (lis::clock-level clock))
      (true (messages-p clock "WARP REFUSED: IMPACT IN 20 S") "~A" (lis::clock-messages clock)))))

(define-test clock-warp-lowering
  (let ((lis::*frame-step-time-limit* nil))
    ;; Throttle up at ×100 without a cut-off: ×10 at the next step.
    (let* ((sim (lis::new-game-sim))
           (clock (clock-at sim 100)))
      (dotimes (i 3) (lis::clock-frame! clock sim 1/60))
      (is = 100 (lis::clock-level clock))
      (lis::controls-throttle-full (lis::sim-controls sim))
      (lis::clock-frame! clock sim 1/60)
      (is = 10 (lis::clock-level clock))
      (true (messages-p clock "WARP ×10: ENGINE ON")))
    ;; Releasing the hold at ×1,000: ×4.
    (let* ((sim (lis::new-game-sim))
           (clock (clock-at sim 1000)))
      (dotimes (i 3) (lis::clock-frame! clock sim 1/60))
      (is = 1000 (lis::clock-level clock))
      (lis::controls-set-hold (lis::sim-controls sim) :prograde)
      (is eq :rate-damping (lis::controls-attitude-mode (lis::sim-controls sim)))
      (lis::clock-frame! clock sim 1/60)
      (is = 4 (lis::clock-level clock))
      (true (messages-p clock "WARP ×4: HOLD RELEASED")))))

(define-test clock-dropout-to-impact
  ;; Periapsis 50 km below the crash radius, from 1 h before the impact at
  ;; ×1,000,000 (docs/architecture.md "Crash detection", the clock case).
  (let* ((orbit (dive-orbit (- *earth-crash-radius* 50d3)))
         (expected (lis::orbit-radius-crossings orbit *earth-crash-radius* (lis::start-t)))
         (sim (orbit-sim orbit (lis::grid-ceiling (- expected 3600d0))))
         (clock (clock-at sim 1000000))
         (record '())
         (messages '())
         (lis::*frame-step-time-limit* nil))
    (setf (lis::clock-hook clock)
          (lambda (clock s)
            (push (cons (lis::sim-t s) (lis::clock-level clock)) record)
            (setf messages (union messages (lis::clock-messages clock) :test #'string=))
            nil))
    (loop repeat 100000
          until (lis::sim-lost-p sim)
          do (lis::clock-frame! clock sim 1/60))
    (let ((loss (lis::sim-loss sim)))
      (true loss)
      (when loss
        (is = 1 (lis::clock-level clock))
        (is = 1 (cdr (first record)))
        ;; Δ to the flown impact; the prediction's is off by under 0.05 s.
        (let ((late (count-if (lambda (r)
                                (and (> (cdr r) 1)
                                     (< (- (lis::loss-time loss) (car r)) (- (* 3 (cdr r)) 0.05d0))))
                              record)))
          (is = 0 late))
        (true (find 1000 record :key #'cdr))
        (true (< (abs (- (lis::loss-time loss) expected)) 1d-2)
              "~,6F s from the Kepler crossing" (- (lis::loss-time loss) expected))
        (multiple-value-bind (latitude longitude)
            (lis::latitude-longitude :earth (lis::orbit-state orbit expected) expected)
          (true (< (deg-diff (lis::loss-latitude loss) latitude) 0.01d0))
          (true (< (deg-diff (lis::loss-longitude loss) longitude) 0.01d0)))
        (dolist (level '(1000 100 10 4 1))
          (true (find-if (lambda (m)
                           (search (format nil "WARP ×~:D: PERIAPSIS BELOW 100 KM IN " level) m))
                         messages)
                "×~D in ~S" level messages))))))

(define-test clock-messages-expire
  (let* ((sim (lis::new-game-sim))
         (clock (clock-at sim 10)))
    (lis::controls-throttle-full (lis::sim-controls sim))
    (lis::clock-warp-up clock sim)
    (is = 1 (length (lis::clock-messages clock)))
    (lis::clock-warp-up clock sim)
    (is = 1 (length (lis::clock-messages clock)) "a refusal replaces a refusal")
    ;; Throttle off: the prediction no longer restarts every frame.
    (lis::controls-throttle-cut (lis::sim-controls sim))
    (lis::clock-toggle-pause clock)
    (dotimes (i 29) (lis::clock-frame! clock sim 0.1d0))
    (is = 1 (length (lis::clock-messages clock)))
    (dotimes (i 2) (lis::clock-frame! clock sim 0.1d0))
    (is = 0 (length (lis::clock-messages clock)))))

(define-test clock-restart
  (let* ((sim (lis::new-game-sim))
         (checkpoint (lis::take-checkpoint sim))
         (clock (clock-at sim 100))
         (lis::*frame-step-time-limit* nil))
    (dotimes (i 10) (lis::clock-frame! clock sim 1/60))
    (lis::vec3-scale! (lis::ship-state-v (lis::sim-ship sim))
                      (lis::ship-state-v (lis::sim-ship sim)) 0.5d0)
    (fly-until-lost sim 1000 (+ (lis::sim-t sim) 1000))
    (true (lis::sim-lost-p sim))
    (lis::clock-frame! clock sim 1/60)
    (lis::clock-toggle-pause clock)
    (lis::clock-restart! clock sim checkpoint)
    (true (equalp sim (lis::new-game-sim)))
    (is = 1 (lis::clock-level clock))
    (false (lis::clock-paused clock))
    (is = 0d0 (lis::clock-accumulator clock))
    (false (lis::clock-messages clock))
    (let* ((fresh (lis::new-game-sim))
           (fresh-clock (lis::new-clock fresh)))
      (dotimes (i 1000)
        (lis::clock-frame! clock sim 1/60)
        (lis::clock-frame! fresh-clock fresh 1/60))
      (true (equalp sim fresh)))))

;;; The objects in the sim: promotion, demotion, stepping (docs/architecture.md
;;; "Rails", "Determinism"; decision 110: Objects in the simulation:
;;; places.lisp's load position, the object struct and promotion checks).

(defun object-rv (object time)
  (let ((r (lis::make-vec3)) (v (lis::make-vec3)))
    (lis::object-state-into! r v (lis::make-quat) (lis::make-vec3) object time)
    (values r v)))

(defun place-ship! (sim r v)
  "SIM's ship at R, V, at rest in direct mode, the objects rechecked; SIM."
  (lis::ship-state-assign! (lis::sim-ship sim)
                           (lis::make-ship-state r v (lis::quat 1 0 0 0) (lis::make-vec3) 28000d0))
  (setf (lis::controls-attitude-mode (lis::sim-controls sim)) :direct)
  (lis::sim-recheck-objects! sim))

(defun ship-near (key offset &key (dv 0d0))
  "The start state with the ship OFFSET m ahead of object KEY along its
velocity, moving with it plus DV m/s along it."
  (let ((sim (lis::new-game-sim)))
    (multiple-value-bind (r v) (object-rv (lis::sim-object sim key) (lis::sim-t sim))
      (let ((u (lis::vec3-normalize v)))
        (place-ship! sim (lis::vec3-add-scaled r u offset) (lis::vec3-add-scaled v u dv))))))

(defun promoted-keys (sim)
  (loop for object across (lis::sim-objects sim)
        when (lis::object-promoted-p object) collect (lis::object-key object)))

(define-test clock-objects-start-schedule
  ;; Tern Station ~2,370 km ahead: its first check ~90 s out, then none
  ;; promoted for 1,000 s.
  (let* ((sim (lis::new-game-sim))
         (start (lis::sim-t sim))
         (tern (lis::sim-object sim :tern-station))
         (d (lis::vec3-norm (lis::vec3-sub (object-rv tern start)
                                           (lis::ship-state-r (lis::sim-ship sim))))))
    (is equal (map 'list #'lis::object-key (lis::sim-objects sim))
        (mapcar #'lis::object-def-key lis::*objects*))
    (true (< 2300d3 d 2450d3) "~,1F km" (/ d 1000))
    (is = (+ start (/ (floor (* 64 (/ (- d 100d3) 25d3))) 64)) (lis::object-next-check tern))
    (true (every (lambda (o) (and (eq :rails (lis::object-status o))
                                  (> (lis::object-next-check o) start)))
                 (lis::sim-objects sim)))
    (loop until (>= (lis::sim-t sim) (+ start 1000d0))
          do (step-sim sim 1 1000 (+ start 1000d0))
             (false (promoted-keys sim)))))

(define-test clock-promote-demote
  ;; Promoted at t, demoted after one step: undisturbed, back on the rails
  ;; it had; disturbed (a contact), on rails reproducing the integrated
  ;; state within 1 mm.
  (dolist (disturbed '(nil t))
    (let* ((sim (ship-near :tern-station 1000d0))
           (tern (lis::sim-object sim :tern-station))
           (rails (lis::object-def-rails (lis::object-def-of tern))))
      (is equal '(:tern-station) (promoted-keys sim))
      (is equalp rails (lis::object-rails tern))
      (setf (lis::object-disturbed tern) disturbed)
      (let ((reference (step-sim (lis::copy-sim sim) 1 1)))
        ;; The ship 300 km up: beyond 200 km after the step.
        (let ((r (lis::ship-state-r (lis::sim-ship sim))))
          (lis::vec3-add-scaled! r r (lis::vec3-normalize r) 300d3))
        (step-sim sim 1 1)
        (is eq :rails (lis::object-status tern))
        (false (lis::object-disturbed tern))
        (is eq (lis::object-def-attitude (lis::object-def-of tern)) (lis::object-attitude tern))
        (if disturbed
            (let ((integrated (lis::object-state (lis::sim-object reference :tern-station))))
              (false (eq rails (lis::object-rails tern)))
              (multiple-value-bind (r v) (object-rv tern (lis::sim-t sim))
                (let ((dr (lis::vec3-norm (lis::vec3-sub r (lis::ship-state-r integrated))))
                      (dv (lis::vec3-norm (lis::vec3-sub v (lis::ship-state-v integrated)))))
                  (true (< dr 1d-3) "~,3E m" dr)
                  (true (< dv 1d-6) "~,3E m/s" dv))))
            (is eq rails (lis::object-rails tern)))
        (true (> (lis::object-next-check tern) (lis::sim-t sim)))))))

(define-test clock-demoted-towable-attitude
  ;; Disturbed, K-17 spinning about +x keeps a :spin; tumbling, an
  ;; :inertial at its q.
  (flet ((demoted (w)
           (let* ((sim (ship-near :k-17 1000d0))
                  (k-17 (lis::sim-object sim :k-17))
                  (state (lis::object-state k-17)))
             (setf (lis::object-disturbed k-17) t)
             (when w (replace (lis::ship-state-w state) w))
             (let ((r (lis::ship-state-r (lis::sim-ship sim))))
               (lis::vec3-add-scaled! r r (lis::vec3-normalize r) 300d3))
             (step-sim sim 1 1)
             (values (lis::object-attitude k-17) (lis::ship-state-q state) (lis::sim-t sim)))))
    (multiple-value-bind (mode q time) (demoted nil)
      (is eq :spin (lis::attitude-kind mode))
      (is equalp q (lis::attitude-q mode))
      (is = time (lis::attitude-epoch mode))
      (true (< (abs (- (lis::attitude-rate mode) (lis::deg->rad 0.3d0))) 1d-12))
      (true (< (angle-between (lis::attitude-axis mode) (lis::vec3 1 0 0)) 1d-9)))
    (multiple-value-bind (mode q) (demoted '(0.01d0 0.01d0 0d0))
      (is eq :inertial (lis::attitude-kind mode))
      (is equalp q (lis::attitude-q mode)))))

(defun berth-object-alone (object start end)
  "A berth OBJECT's promoted state flown alone by rk4-step! at the fine step
from START to END under :full, the Moon and the Sun read by the step."
  (declare (type double-float start end) (inline lis::rk4-step!))
  (let* ((def (lis::object-def-of object))
         (state (lis::copy-ship-state (lis::object-state object)))
         (zero (lis::make-vec3))
         (lis::*object-attitude* (lis::object-attitude object)))
    (loop for time of-type double-float from start below end by lis::+fine-step+
          do (lis::rk4-step! state lis::+fine-step+ time :full zero zero 0d0 (lis::object-def-mass def)
                             (lis::object-def-tensor def) (lis::object-def-tensor-inverse def)
                             #'lis::object-kinematic-attitude))
    state))

(define-test clock-tern-beside-parked-ship
  ;; Promoted beside a parked ship (1 km ahead, :full, ×4 steps), Tern
  ;; Station flies as a reference integration of the station alone: 30 min
  ;; (planned 1 h; both read 0 m, so half the run checks the same, task 258:
  ;; Bring scripts/test back under its ~15 s budget). 0.3 s at the fine step.
  (let* ((sim (ship-near :tern-station 1000d0))
         (tern (lis::sim-object sim :tern-station))
         (end (+ (lis::sim-t sim) 1800d0))
         (state (berth-object-alone tern (lis::sim-t sim) end)))
    (step-sim sim most-positive-fixnum 4 end)
    (is = end (lis::sim-t sim))
    (is equal '(:tern-station) (promoted-keys sim))
    (let ((d (lis::vec3-norm (lis::vec3-sub (lis::ship-state-r (lis::object-state tern))
                                            (lis::ship-state-r state)))))
      (true (< d 1d0) "~,3E m" d))))

(define-test clock-objects-leave-the-ship-alone
  ;; With objects on rails, none promoted, the ship flies as without them.
  (let* ((with (lis::new-game-sim))
         (without (lis::copy-sim with)))
    (setf (lis::sim-objects without) #())
    (mixed-run with 2000)
    (mixed-run without 2000)
    (false (promoted-keys with))
    (is equalp (lis::sim-ship without) (lis::sim-ship with))
    (is = (lis::sim-t without) (lis::sim-t with))))

(define-test clock-objects-frame-rates-and-checkpoint
  ;; 30 m/s prograde from 1 km behind Tern Station and 200 m off its plane
  ;; (passing it, not hitting it): promoted, then beyond 200 km within the
  ;; orbit and demoted onto its own rails; every frame rate and a restore
  ;; give the same objects bit for bit.
  (flet ((departing ()
           (let* ((sim (ship-near :tern-station -1000d0 :dv 30d0))
                  (r (lis::vec3-copy (lis::ship-state-r (lis::sim-ship sim))))
                  (v (lis::vec3-copy (lis::ship-state-v (lis::sim-ship sim)))))
             (place-ship! sim (lis::vec3-add-scaled r (lis::vec3-normalize (lis::vec3-cross r v)) 200d0)
                          v))))
    (dolist (level '(4 1000))
      (let ((end (+ (lis::start-t) (if (= level 4) 30d0 (* 1.2d0 (drift-period))))))
        (dolist (frames (list '(1/60) *irregular-frames*))
          (let ((sim (departing)))
            (is equal '(:tern-station) (promoted-keys sim))
            (is = 0 (clock-run sim level end frames) "×~D ~A" level frames)
            (when (= level 1000)
              (is eq :rails (lis::object-status (lis::sim-object sim :tern-station)))
              (true (eq (lis::object-def-rails (lis::object-def-of
                                                 (lis::sim-object sim :tern-station)))
                         (lis::object-rails (lis::sim-object sim :tern-station)))))))))
    (let* ((sim (step-sim (departing) 100 1000))
           (checkpoint (lis::take-checkpoint sim))
           (flown (lis::copy-sim (step-sim sim 3000 1000))))
      (is eq :rails (lis::object-status (lis::sim-object flown :tern-station)))
      (is = 0 (least-bytes-consed (lambda () (lis::restore-checkpoint! sim checkpoint))))
      (true (equalp sim checkpoint))
      (step-sim sim 3000 1000)
      (true (equalp sim flown)))))

(defun tern-pass-sim ()
  "The start state coasted to 20 h at ×1,000,000 (the steps the state
allows), past the pass ~20 km under Tern Station at 12.6 h. (values sim
promoted), PROMOTED the times Tern Station was promoted."
  (let* ((sim (lis::new-game-sim))
         (end (+ (lis::sim-t sim) 72000d0))
         (tern (lis::sim-object sim :tern-station))
         (promoted '()))
    (loop until (>= (lis::sim-t sim) end)
          do (lis::sim-step! sim 1000000 :until end)
             (when (lis::object-promoted-p tern)
               (push (lis::sim-t sim) promoted)))
    (values sim (nreverse promoted))))

(define-test clock-tern-pass-keeps-rails
  ;; The pass promotes Tern Station (12.58–13.92 h); it is demoted onto the
  ;; rails it had (task 210 measured 555 km off them at 20 h when re-railed
  ;; on its osculating elements). ~25 ms.
  (multiple-value-bind (sim promoted) (tern-pass-sim)
    (let ((tern (lis::sim-object sim :tern-station))
          (hours (mapcar (lambda (time) (/ (- time (lis::start-t)) 3600)) promoted)))
      (true (and promoted (< 12.5 (first hours) 12.7) (< 13.8 (car (last hours)) 14.0))
            "~,2F–~,2F h" (first hours) (car (last hours)))
      (is eq :rails (lis::object-status tern))
      (is eq (lis::object-def-rails (lis::object-def-of tern)) (lis::object-rails tern))
      (multiple-value-bind (r v) (object-rv tern (lis::sim-t sim))
        (declare (ignore v))
        (let ((r0 (lis::make-vec3)) (v0 (lis::make-vec3)))
          (lis::rails-state-into! r0 v0 (first (lis::object-def-rails (lis::object-def-of tern)))
                                  (lis::sim-t sim))
          (is equalp r0 r))))))

(defun skua-yard-sim ()
  "The ship at Skua Yard's drop point at the start, its nine objects promoted."
  (let ((sim (lis::new-game-sim))
        (r (lis::make-vec3)) (v (lis::make-vec3)))
    (lis::place-target-state-into! r v (lis::place :skua-yard) sim)
    (place-ship! sim r v)))

(define-test clock-skua-yard-cons-nothing
  (let* ((a (skua-yard-sim))
         (b (lis::copy-sim a)))
    (is equal '(:highport :skua-yard-1 :skua-yard-2 :skua-yard-3 :skua-yard-4 :skua-yard-5
                :skua-yard-6 :pod-14 :rock-mover)
        (promoted-keys a))
    ;; 10,002 steps: least-bytes-consed runs its thunk three times.
    (dotimes (i 3) (step-sim a 3334 1))
    (is = 9 (length (promoted-keys a)))
    (let ((consed (least-bytes-consed (lambda () (step-sim b 3334 1)))))
      (true (< consed 1024) "~D bytes" consed))
    (true (equalp a b))
    (let ((start (skua-yard-sim)))
      (is = 0 (least-bytes-consed (lambda () (lis::sim-assign! b start))))
      (true (equalp b start)))))

;;; Targets (decision 114: Targets and target holds in the sim)

(defparameter *target-order*
  '(:tern-station :fulmar-depot :k-17 :highport :skua-yard
    :skua-yard-1 :skua-yard-2 :skua-yard-3 :skua-yard-4 :skua-yard-5 :skua-yard-6 :pod-14))

(define-test clock-target-cycle
  (let ((sim (lis::new-game-sim)))
    (is equal *target-order* (lis::target-candidates sim))
    (false (lis::sim-target sim))
    (is eq :tern-station (lis::sim-cycle-target! sim 1))
    (is eq :pod-14 (lis::sim-cycle-target! sim -1))
    (is eq :tern-station (lis::sim-cycle-target! sim 1))
    (is eq :fulmar-depot (lis::sim-cycle-target! sim 1))
    (false (lis::sim-set-target! sim nil))
    (is eq :pod-14 (lis::sim-cycle-target! sim -1))
    (is eq :tern-station (lis::sim-cycle-target! sim 1))
    ;; A removed K-17 and a latched rock skipped.
    (setf (lis::object-status (lis::sim-object sim :k-17)) :removed
          (lis::object-status (lis::sim-object sim :skua-yard-2)) :latched)
    (is equal (remove-if (lambda (key) (member key '(:k-17 :skua-yard-2))) *target-order*)
        (lis::target-candidates sim))
    (lis::sim-set-target! sim :fulmar-depot)
    (is eq :highport (lis::sim-cycle-target! sim 1))
    (lis::sim-set-target! sim :skua-yard-3)
    (is eq :skua-yard-1 (lis::sim-cycle-target! sim -1))))

(define-test clock-target-copied
  (let* ((sim (lis::new-game-sim))
         (copy (lis::copy-sim sim)))
    (lis::sim-set-target! sim :highport)
    (false (equalp sim copy))
    (is eq :highport (lis::sim-target (lis::copy-sim sim)))
    (is = 0 (least-bytes-consed (lambda () (lis::sim-assign! copy sim))))
    (is eq :highport (lis::sim-target copy))
    (true (equalp sim copy))))

(define-test clock-target-state
  (let ((sim (lis::new-game-sim))
        (r (lis::make-vec3)) (v (lis::make-vec3)))
    (false (lis::target-state-into! r v sim))
    (dolist (key '(:tern-station :skua-yard :pod-14))
      (lis::sim-set-target! sim key)
      (true (lis::target-state-into! r v sim))
      (multiple-value-bind (r1 v1)
          (if (eq key :pod-14)
              (object-rv (lis::sim-object sim :pod-14) (lis::sim-t sim))
              (let ((r1 (lis::make-vec3)) (v1 (lis::make-vec3)))
                (lis::place-target-state-into! r1 v1 (lis::place key) sim)
                (values r1 v1)))
        (is equalp r1 r "~A" key)
        (is equalp v1 v "~A" key)))))

(define-test clock-target-lost
  ;; K-17 burns up on rails while targeted: cleared once, with the notice.
  (let ((sim (k-17-sim 90d3)))
    (lis::sim-set-target! sim :k-17)
    (loop until (lis::object-removed-p (lis::sim-object sim :k-17))
          do (step-sim sim 1 1000))
    (false (lis::sim-target sim))
    (is equal '("K-17 BURNED UP" "TARGET LOST: K-17") (mapcar #'second (lis::sim-notices sim)))
    (step-sim sim 10 1000)
    (is = 1 (count "TARGET LOST: K-17" (lis::sim-notices sim) :key #'second :test #'equal)))
  ;; A latched rock: its name.
  (let ((sim (lis::new-game-sim)))
    (lis::sim-set-target! sim :skua-yard-4)
    (setf (lis::object-status (lis::sim-object sim :skua-yard-4)) :latched)
    (lis::target-check! sim)
    (false (lis::sim-target sim))
    (is equal '((:target "TARGET LOST: SKUA YARD 4")) (lis::sim-notices sim))))

;;; Planned burns (decision 115: Planned burns as scheduled controls in the
;;; sim)

(defun burn-duration (m0 dv throttle)
  "s of an engine burn of DV from mass M0 at THROTTLE."
  (let ((mdot (lis::ship-engine-mdot throttle))
        (force (lis::ship-engine-force throttle)))
    (* (/ m0 mdot) (- 1 (exp (- (/ (* dv mdot) force)))))))

(defun grid-round (time) (/ (round (* time 64)) 64d0))

(defun hohmann-plan (sim)
  "SIM's two-burn Hohmann (point mass) from its circular orbit to 400 km
higher, each full-throttle burn centred on its node, its hold 8 s before;
(values plan r2)."
  (let* ((mu lis::+mu-earth+)
         (ship (lis::sim-ship sim))
         (t0 (lis::sim-t sim))
         (r1 (lis::vec3-norm (lis::ship-state-r ship)))
         (r2 (+ r1 400d3))
         (at (/ (+ r1 r2) 2))
         (dv1 (* (sqrt (/ mu r1)) (- (sqrt (/ r2 at)) 1)))
         (dv2 (* (sqrt (/ mu r2)) (- 1 (sqrt (/ r1 at)))))
         (m0 (+ lis::*ship-dry-mass* (lis::ship-state-m-p ship)))
         (m1 (* m0 (exp (- (/ (* dv1 (lis::ship-engine-mdot 1d0)) lis::*engine-thrust*)))))
         (node1 (+ t0 60))
         (node2 (+ node1 (* pi (sqrt (/ (expt at 3) mu)))))
         (burns '()))
    (multiple-value-bind (r v)
        (lis::orbit-state (lis::state->orbit (lis::ship-state-r ship) (lis::ship-state-v ship) mu t0)
                          node1)
      (flet ((burn! (label node r v m dv)
               (let ((start (grid-round (- node (/ (burn-duration m dv 1d0) 2)))))
                 (push (lis::make-planned-burn label (- start 8) start
                                               (lis::hold-attitude :prograde r v 0d0)
                                               1d0 nil dv)
                       burns))))
        (burn! :raise node1 r v m0 dv1)
        (let ((v1 (lis::vec3-add v (lis::vec3-scale (lis::vec3-normalize v) dv1))))
          (multiple-value-bind (r v) (lis::orbit-state (lis::state->orbit r v1 mu node1) node2)
            (burn! :circularise node2 r v m1 dv2)))))
    (values (lis::make-burn-plan (coerce (reverse burns) 'simple-vector)) r2)))

(defun plan-sim ()
  "The start's sim, :point, with hohmann-plan engaged; (values sim r2)."
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-model sim) :point)
    (multiple-value-bind (plan r2) (hohmann-plan sim)
      (lis::sim-set-plan! sim plan)
      (values sim r2))))

(defun fly-plan (sim level-fn &optional (stop (lambda (sim) (eq (lis::sim-plan-phase sim) :done))))
  "SIM's steps at (LEVEL-FN SIM) until STOP; the transitions, oldest first:
(:hold t i) at the step's start that engaged burn i's inertial hold, (:on t
i throttle) that started it, (:cut t i dv-before cutoff dv-after) at the
step's end that cut it, (:coast t i hold) that engaged the coast hold."
  (let ((events '()))
    (loop until (funcall stop sim)
          do (let* ((c (lis::sim-controls sim))
                    (t0 (lis::sim-t sim))
                    (i (lis::sim-plan-index sim))
                    (phase (lis::sim-plan-phase sim))
                    (throttle (lis::controls-throttle c))
                    (cutoff (lis::sim-cutoff sim))
                    (dv (lis::ship-state-dv (lis::sim-ship sim))))
               (step-sim sim 1 (funcall level-fn sim))
               (let ((phase1 (lis::sim-plan-phase sim))
                     (i1 (lis::sim-plan-index sim)))
                 (when (and (eq phase :pending) (member phase1 '(:holding :burning)))
                   (push (list :hold t0 i (lis::controls-hold c)) events))
                 (when (and (member phase '(:pending :holding)) (eq phase1 :burning))
                   (push (list :on t0 i (lis::controls-throttle c) dv) events))
                 (when (and (> throttle 0d0) (= (lis::controls-throttle c) 0d0))
                   (push (list :cut (lis::sim-t sim) i dv cutoff
                               (lis::ship-state-dv (lis::sim-ship sim)))
                         events))
                 (when (and (eq phase :burning) (or (/= i i1) (eq phase1 :done)))
                   (push (list :coast t0 i (lis::controls-hold c)) events)))))
    (nreverse events)))

(defun hohmann-run (level-fn)
  "hohmann-plan flown at LEVEL-FN to :done, the transitions checked;
(values the cut times, the final orbit's periapsis and apoapsis radii, r2)."
  (multiple-value-bind (sim r2) (plan-sim)
    (let* ((burns (lis::burn-plan-burns (lis::sim-plan sim)))
           (events (fly-plan sim level-fn))
           (cuts '())
           (start-dv 0d0))
      (is equal '(:hold :on :cut :coast :hold :on :cut :coast) (mapcar #'first events))
      (dolist (e events)
        (destructuring-bind (kind time i &rest data) e
          (let ((burn (aref burns i)))
            (ecase kind
              (:hold (is = (lis::planned-burn-hold-at burn) time)
               (is eq :inertial (first data)))
              (:on (is = (lis::planned-burn-start burn) time)
               ;; full, lowered so the burn lasts whole ticks (decision 165)
               (true (< (- 1d0 1d-3) (first data) 1d0) "~F" (first data))
               (setf start-dv (second data)))
              (:cut (destructuring-bind (dv-before cutoff dv-after) data
                      (true (on-grid-p time))
                      (true (< dv-before cutoff) "~F ~F" dv-before cutoff)
                      (true (<= cutoff dv-after (+ cutoff 0.1d0)) "~F ~F" cutoff dv-after)
                      ;; the cut on its tick's end: the planned Δv, no tick's more
                      (true (< (abs (- (- dv-after start-dv) (lis::planned-burn-dv burn))) 1d-4)
                            "~F of ~F" (- dv-after start-dv) (lis::planned-burn-dv burn))
                      (push time cuts)))
              (:coast (is eq :prograde (first data)))))))
      (is eq :done (lis::sim-plan-phase sim))
      (is eq :hold (lis::controls-attitude-mode (lis::sim-controls sim)))
      (false (lis::sim-cutoff sim))
      (let* ((ship (lis::sim-ship sim))
             (orbit (lis::state->orbit (lis::ship-state-r ship) (lis::ship-state-v ship)
                                       lis::+mu-earth+ (lis::sim-t sim))))
        (values (nreverse cuts) (lis::orbit-periapsis-radius orbit)
                (lis::orbit-apoapsis-radius orbit) r2)))))

(define-test clock-plan-hohmann
  ;; ×1 from each hold to its cut, coasting at ×1,000 between; and ×1,000 throughout.
  (multiple-value-bind (cuts1 rp1 ra1 r2)
      (hohmann-run (lambda (sim)
                     (let ((next (lis::plan-next-time sim)))
                       (if (or (member (lis::sim-plan-phase sim) '(:holding :burning))
                               (and next (<= next (lis::sim-t sim))))
                           1 1000))))
    (multiple-value-bind (cuts1000 rp1000 ra1000) (hohmann-run (constantly 1000))
      (loop for a in cuts1 for b in cuts1000
            do (true (<= (abs (- a b)) 1/64) "~F ~F" a b))
      (dolist (radius (list rp1 ra1 rp1000 ra1000))
        (true (< (relative-error radius r2) 1d-2) "~,1F km from ~,1F km"
              (/ radius 1000) (/ r2 1000))))))

(define-test clock-plan-rcs
  ;; 0.3 m/s along +x, no gravity: the velocity change is the burn's.
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim))
         (t0 (lis::sim-t sim)))
    (setf (lis::sim-model sim) :none)
    (lis::sim-set-plan!
     sim (lis::make-burn-plan
          (vector (lis::make-planned-burn :trim t0 (+ t0 2) (lis::quat-copy (lis::ship-state-q ship))
                                          0d0 1 0.3d0))))
    (step-sim sim 129 1)
    (is eq :burning (lis::sim-plan-phase sim))
    (is = 1 (aref (lis::controls-translation-keys (lis::sim-controls sim)) 0))
    (let ((v0 (lis::vec3-copy (lis::ship-state-v ship)))
          (step-dv 0d0))
      (loop while (eq (lis::sim-plan-phase sim) :burning)
            do (let ((v (lis::vec3-copy (lis::ship-state-v ship))))
                 (step-sim sim 1 1)
                 (setf step-dv (max step-dv (lis::vec3-norm (lis::vec3-sub (lis::ship-state-v ship) v))))))
      (is eq :done (lis::sim-plan-phase sim))
      (is equalp #(0 0 0) (lis::controls-translation-keys (lis::sim-controls sim)))
      (is eq :prograde (lis::controls-hold (lis::sim-controls sim)))
      ;; The last step ran with the key released.
      (let ((delivered (lis::vec3-norm (lis::vec3-sub (lis::ship-state-v ship) v0))))
        (true (< (abs (- delivered 0.3d0)) step-dv) "~F m/s, a step ~F m/s" delivered step-dv)))))

(define-test clock-plan-cursor-copied
  ;; Mid-burn: a copy, an assigned sim and a restored checkpoint fly the same burns.
  (let ((sim (plan-sim)))
    (fly-plan sim (constantly 1000) (lambda (sim) (eq (lis::sim-plan-phase sim) :burning)))
    (step-sim sim 1 1000)
    (is eq :burning (lis::sim-plan-phase sim))
    (let* ((copy (lis::copy-sim sim))
           (assigned (lis::sim-assign! (lis::new-game-sim) sim))
           (checkpoint (lis::take-checkpoint sim))
           (restored (lis::new-game-sim)))
      (true (equalp copy sim))
      (true (equalp assigned sim))
      (is eq (lis::sim-plan sim) (lis::sim-plan copy))
      (fly-plan sim (constantly 1000))
      (lis::restore-checkpoint! restored checkpoint)
      (dolist (other (list copy assigned restored))
        (fly-plan other (constantly 1000))
        (true (equalp other sim))))))

(define-test clock-plan-cons-nothing
  (let* ((sim (plan-sim))
         (out (lis::new-game-sim)))
    (is = 0 (least-bytes-consed (lambda () (lis::sim-assign! out sim))))
    (fly-plan sim (constantly 1000))
    (step-sim sim 100 1000)
    (true (< (least-bytes-consed (lambda () (step-sim sim 10000 1000)) 1) 1024))
    (is eq :done (lis::sim-plan-phase sim))))

;;; The autopilot's execution rules in the step and the clock (decision 117:
;;; Autopilot in milestone 3: planning jobs, goal measures, burns, execution
;;; rules in the step, trims).

(defun notice-texts (sim) (mapcar #'second (lis::sim-notices sim)))

(defun check-aborted (sim reason)
  "SIM's last step aborted its plan for REASON: the notice, throttle off,
keys released, rate damping, no plan, t on the grid."
  (let ((controls (lis::sim-controls sim)))
    (true (member (format nil "AUTOPILOT ABORT: ~A" reason) (notice-texts sim) :test #'string=)
          "~S" (notice-texts sim))
    (is = 0d0 (lis::controls-throttle controls))
    (false (lis::sim-cutoff sim))
    (is equalp #(0 0 0) (lis::controls-translation-keys controls))
    (is eq :rate-damping (lis::controls-attitude-mode controls))
    (false (lis::sim-plan sim))
    (true (on-grid-p (lis::sim-t sim)))))

(defun step-until (sim level stop &optional (limit 100000))
  "Steps of SIM at LEVEL until (STOP SIM), at most LIMIT; SIM."
  (loop repeat limit
        until (funcall stop sim)
        do (step-sim sim 1 level))
  (true (funcall stop sim))
  sim)

(defun burning-plan-sim ()
  "plan-sim (hohmann-plan, :point) at ×1,000 to the first hold, at ×1 into its burn."
  (let ((sim (plan-sim)))
    (fly-plan sim (lambda (s) (if (eq (lis::sim-plan-phase s) :pending) 1000 1))
              (lambda (s) (eq (lis::sim-plan-phase s) :burning)))
    (step-sim sim 64 1)
    (is eq :burning (lis::sim-plan-phase sim))
    sim))

(define-test autopilot-propellant-low
  ;; Drained mid-burn below the burns left: the abort at that step.
  (let* ((sim (burning-plan-sim))
         (t0 (lis::sim-t sim)))
    (setf (lis::ship-state-m-p (lis::sim-ship sim)) 50d0)
    (step-sim sim 1 1)
    (is = (+ t0 1/64) (lis::sim-t sim))
    (check-aborted sim "PROPELLANT LOW")))

(define-test autopilot-attitude-error
  ;; ×1: a yaw rate of 0.1 rad/s mid-burn cuts it; the inertial hold settles
  ;; and it resumes with the Δv it had left; a second time aborts. A copy
  ;; and a restored checkpoint taken after the cut fly on bit for bit.
  (let* ((sim (burning-plan-sim))
         (ship (lis::sim-ship sim))
         (controls (lis::sim-controls sim))
         (cutoff (lis::sim-cutoff sim)))
    (setf (aref (lis::ship-state-w ship) 2) 0.1d0)
    (step-until sim 1 (lambda (s) (eq (lis::sim-plan-phase s) :holding)) 256)
    (is = 0d0 (lis::controls-throttle controls))
    (is eq :inertial (lis::controls-hold controls))
    (is = 1 (lis::sim-plan-retries sim))
    (let ((left (lis::sim-plan-resume sim)))
      (true (< 0d0 left))
      (true (< (abs (- left (- cutoff (lis::ship-state-dv ship)))) 0.1d0) "~F m/s left" left)
      (let* ((copy (lis::copy-sim sim))
             (restored (lis::restore-checkpoint! (lis::new-game-sim) (lis::take-checkpoint sim))))
        (true (equalp copy sim))
        (dolist (s (list sim copy restored))
          (step-until s 1 (lambda (s) (eq (lis::sim-plan-phase s) :burning)) 20000))
        (true (lis::controls-settled controls))
        (true (equalp copy sim))
        (true (equalp restored sim)))
      ;; Resumed: the cut-off re-armed at the odometer + the Δv left.
      (is = 0d0 (lis::sim-plan-resume sim))
      (true (< (abs (- (lis::sim-cutoff sim) (+ (lis::ship-state-dv ship) left))) 0.1d0)))
    (setf (aref (lis::ship-state-w ship) 2) 0.1d0)
    (step-until sim 1 (lambda (s) (null (lis::sim-plan s))) 256)
    (check-aborted sim "ATTITUDE ERROR")))

(define-test autopilot-unsafe-orbit
  ;; A retrograde burn whose full Δv takes the periapsis to 110 km aborts
  ;; at its first step, the periapsis then above the crash radius + 25 km.
  (let* ((sim (model-sim :point))
         (ship (lis::sim-ship sim))
         (t0 (lis::sim-t sim))
         (r1 (lis::vec3-norm (lis::ship-state-r ship)))
         (dv (- (circular-speed 400d3)
                (nth-value 1 (transfer-speeds (+ lis::+radius-earth+ 110d3) r1))))
         (q (lis::hold-attitude :retrograde (lis::ship-state-r ship) (lis::ship-state-v ship) 0d0))
         (floor (+ *earth-crash-radius* 25d3)))
    (replace (lis::ship-state-q ship) q)
    (fill (lis::ship-state-w ship) 0d0)
    (lis::sim-engage-plan! sim (lis::make-burn-plan
                                (vector (lis::make-planned-burn :apsis t0 (+ t0 1/2) q 1d0 nil dv))))
    (flet ((periapsis ()
             (lis::orbit-periapsis-radius
              (lis::state->orbit (lis::ship-state-r ship) (lis::ship-state-v ship)
                                 lis::+mu-earth+ (lis::sim-t sim)))))
      (step-until sim 1 (lambda (s) (eq (lis::sim-plan-phase s) :holding)) 4)
      (step-sim sim 31 1)
      ;; The impulsive check fails at the burn's start: no thrust delivered.
      (let ((t1 (lis::sim-t sim)))
        (is = (+ t0 1/2) t1)
        (step-sim sim 1 1)
        (check-aborted sim "UNSAFE ORBIT")
        (true (< (lis::ship-state-dv ship) 1d-3))
        (true (>= (periapsis) floor) "~,1F km" (/ (- (periapsis) lis::+radius-earth+) 1000))))))

(define-test autopilot-contact
  ;; A bounce in the step aborts the plan engaged.
  (let* ((sim (aft-scene 0.5d0))
         (t0 (lis::sim-t sim)))
    (lis::sim-set-plan! sim (lis::make-burn-plan
                             (vector (lis::make-planned-burn :apsis (+ t0 600) (+ t0 700)
                                                             (lis::quat 1 0 0 0) 1d0 nil 10d0))))
    (step-until sim 1 (lambda (s) (null (lis::sim-plan s))) 128)
    (check-aborted sim "CONTACT")))

(define-test autopilot-coasting-cons-nothing
  ;; 10,000 coarse steps engaged before the hold-at cons nothing.
  (let* ((sim (model-sim :point))
         (t0 (lis::sim-t sim)))
    (lis::sim-engage-plan! sim (lis::make-burn-plan
                                (vector (lis::make-planned-burn :apsis (+ t0 1d7) (+ t0 1.1d7)
                                                                (lis::quat 1 0 0 0) 1d0 nil 10d0))))
    (step-sim sim 100 1000)
    (true (< (least-bytes-consed (lambda () (step-sim sim 10000 1000)) 1) 1024))
    (is eq :pending (lis::sim-plan-phase sim))))

(defun eager-clock (sim record)
  "A clock at ×1,000,000 for SIM whose hook warps up as far as allowed
before each step and pushes (t level phase due-burn index) onto the car of
the cons RECORD, the live messages it has seen onto its cdr."
  (let ((clock (clock-at sim 1000000)))
    (setf (lis::clock-hook clock)
          (lambda (clock s)
            (setf (cdr record) (union (cdr record) (lis::clock-messages clock) :test #'string=))
            (loop while (lis::clock-warp-up clock s))
            (lis::clock-check-level! clock s)
            (push (list (lis::sim-t s) (lis::clock-level clock) (lis::sim-plan-phase s)
                        (lis::plan-burn-due s) (and (lis::sim-plan s) (lis::sim-plan-index s)))
                  (car record))
            nil))
    clock))

(defun log-texts (clock) (mapcar #'second (lis::clock-log clock)))

(define-test autopilot-flown-through-clock
  ;; The Hohmann 400 → 800 km planned and engaged, flown through
  ;; clock-frame! by a pilot warping up as far as allowed: before each
  ;; hold-at Δ ≥ 3 s × level (less 0.05 s), ×1 at it, ≤ ×100 while the
  ;; engine burns; completion's checkpoint and notice once.
  (let* ((lis::*frame-step-time-limit* nil)
         (sim (model-sim :point))
         (plan (plan-now sim :change-orbit '(800d3)))
         (record (list nil))
         (clock (eager-clock sim record)))
    (lis::autopilot-engage! clock sim plan)
    (is eq plan (lis::autopilot-model-flown (lis::clock-autopilot clock)))
    (loop repeat 20000
          while (lis::sim-plan sim)
          do (lis::clock-frame! clock sim 1/60))
    (false (lis::sim-plan sim))
    (dolist (step (car record))
      (destructuring-bind (time level phase due index) step
        (when index
          (let ((hold-at (lis::planned-burn-hold-at (svref (lis::burn-plan-burns plan) index))))
            (when (and (eq phase :pending) (> level 1))
              (true (>= (- hold-at time) (- (* 3 level) 0.05d0)) "×~D ~,2F s before" level
                    (- hold-at time))
              (when (= time hold-at) (is = 1 level)))))
        (when due (true (<= level 100) "×~D burning" level))))
    (loop for burn across (lis::burn-plan-burns plan)
          do (true (find (lis::planned-burn-hold-at burn) (car record) :key #'first)))
    ;; Coasting the 2,900 s transfer the pilot reaches ×100 at most.
    (dolist (level '(10 4 1))
      (true (find-if (lambda (m) (search (format nil "WARP ×~:D: BURN IN " level) m)) (cdr record))
            "×~D in ~S" level (remove-if-not (lambda (m) (search "BURN IN" m)) (cdr record))))
    (is = 1 (count "AUTOPILOT: PLAN COMPLETE" (log-texts clock) :test #'string=))
    (let ((checkpoint (lis::clock-checkpoint clock)))
      (true checkpoint)
      (when checkpoint
        (is = 0d0 (lis::controls-throttle (lis::sim-controls checkpoint)))
        (false (lis::sim-plan checkpoint))
        (is eq :prograde (lis::controls-hold (lis::sim-controls checkpoint)))
        (true (lis::checkpoint-safe-p checkpoint))))
    (is eq plan (lis::autopilot-model-flown (lis::clock-autopilot clock)))))

(define-test autopilot-rcs-burn-warp
  ;; A 0.3 m/s RCS burn: warp ≤ ×4 while it runs, with its message; warp up
  ;; refused then.
  (let* ((lis::*frame-step-time-limit* nil)
         (sim (model-sim :point))
         (ship (lis::sim-ship sim))
         (t0 (lis::sim-t sim))
         (start (+ t0 100))
         (orbit (lis::state->orbit (lis::ship-state-r ship) (lis::ship-state-v ship)
                                   lis::+mu-earth+ t0))
         (q (multiple-value-bind (r v) (lis::orbit-state orbit start)
              (lis::hold-attitude :prograde r v 0d0)))
         (record (list nil))
         (clock (eager-clock sim record)))
    (lis::autopilot-engage! clock sim (lis::make-burn-plan
                                       (vector (lis::make-planned-burn :trim (+ t0 30) start q
                                                                       0d0 1 0.3d0))))
    (loop repeat 20000
          while (lis::sim-plan sim)
          do (lis::clock-frame! clock sim 1/60))
    (false (lis::sim-plan sim))
    (let ((burning (remove :burning (car record) :key #'third :test-not #'eq)))
      (true (> (length burning) 100))
      (dolist (step burning)
        (true (<= (second step) 4) "×~D" (second step))))
    (true (find "WARP ×4: RCS BURN" (cdr record) :test #'string=) "~S" (cdr record))
    (true (find "WARP REFUSED: RCS BURN, LIMIT ×4" (cdr record) :test #'string=))
    (true (find "AUTOPILOT: PLAN COMPLETE" (log-texts clock) :test #'string=))))

(define-test autopilot-disengage
  (let* ((sim (plan-sim))
         (clock (lis::new-clock sim)))
    (lis::autopilot-engage! clock sim (lis::sim-plan sim))
    (true (lis::autopilot-disengage! clock sim t))
    (false (lis::sim-plan sim))
    (is eq :rate-damping (lis::controls-attitude-mode (lis::sim-controls sim)))
    (true (messages-p clock "AUTOPILOT OFF: PILOT INPUT"))
    (false (lis::autopilot-disengage! clock sim))
    (true (lis::autopilot-model-flown (lis::clock-autopilot clock)))))

;;; The preset menu's planning through the clock (decision 117, decision
;;; 118): the barrier, the budget split, engaging, staleness, Backspace.

(defun menu-key (clock sim &rest keys)
  (dolist (key keys)
    (lis::autopilot-key! (lis::clock-autopilot clock) clock sim key)))

(defun menu-state (clock) (lis::autopilot-model-state (lis::clock-autopilot clock)))

(defun menu-orbit! (clock km)
  "The ORBIT row selected with KM, its INC kept."
  (let ((model (lis::clock-autopilot clock)))
    (setf (lis::autopilot-model-selection model) :change-orbit
          (cdr (assoc :change-orbit (lis::autopilot-model-values model))) km)))

(defun frames-while-planning (clock sim &optional (limit 200))
  "clock-frame!s of 1/60 s while CLOCK's model plans, at most LIMIT; their
count and the largest sim t seen while planning."
  (let ((frames 0) (latest (lis::sim-t sim)))
    (loop while (and (eq :planning (menu-state clock)) (< frames limit))
          do (lis::clock-frame! clock sim 1/60)
             (incf frames)
             (when (eq :planning (menu-state clock))
               (setf latest (max latest (lis::sim-t sim)))))
    (values frames latest)))

(define-test autopilot-menu-planning-barrier
  ;; ×1,000,000: Return plans ORBIT 800 KM over frames, the prediction and
  ;; the job within the frame's budget, the sim held at the snapshot's
  ;; t + 60 s; the plan equalp to autopilot-plan-now's from the snapshot.
  (let* ((lis::*frame-step-time-limit* nil)
         (sim (model-sim :point))
         (clock (clock-at sim 1000000))
         (model (lis::clock-autopilot clock)))
    (menu-key clock sim :open)
    (menu-orbit! clock 800)
    (let ((snapshot (lis::copy-sim sim))
          (job-work 0))
      (menu-key clock sim :return)
      (is eq :planning (menu-state clock))
      (is string= "PLANNING…" (lis::autopilot-model-text model))
      (lis::clock-frame! clock sim 1/60)
      (is = (+ (lis::sim-t snapshot) 60) (lis::sim-t sim))
      (true (<= (lis::prediction-last-work (lis::clock-prediction clock)) 2048))
      (setf job-work (lis::autopilot-job-work (lis::autopilot-model-job model)))
      (true (<= (+ job-work (lis::prediction-last-work (lis::clock-prediction clock))) 4096))
      (multiple-value-bind (frames latest) (frames-while-planning clock sim)
        (true (> frames 2))
        (is = (+ (lis::sim-t snapshot) 60) latest))
      (is eq :shown (menu-state clock))
      (true (equalp (lis::autopilot-plan-now snapshot :change-orbit (list 800000d0 nil))
                    (lis::autopilot-model-plan model)))
      ;; Warped past its first hold-at − 10 s: stale, re-planned, engaged.
      (loop repeat 10 do (lis::clock-frame! clock sim 1/60))
      (let ((old (lis::autopilot-model-plan model)))
        (true (> (lis::sim-t sim)
                 (- (lis::planned-burn-hold-at (svref (lis::burn-plan-burns old) 0)) 10)))
        (menu-key clock sim :return)
        (true (find "PLAN STALE: REPLANNING" (log-texts clock) :test #'string=))
        (is eq :planning (menu-state clock))
        (frames-while-planning clock sim)
        (is eq :engaged (menu-state clock))
        (true (lis::sim-plan sim))
        (false (eq old (lis::sim-plan sim)))
        (is eq (lis::sim-plan sim) (lis::autopilot-model-flown model))))))

(define-test autopilot-menu-engage-and-backspace
  ;; Paused: planning acts, a second Return engages; Backspace in each state.
  (let* ((sim (model-sim :point))
         (clock (lis::new-clock sim))
         (t0 (lis::sim-t sim)))
    (lis::clock-toggle-pause clock)
    (menu-key clock sim :backspace)
    (is eq :closed (menu-state clock))
    (menu-key clock sim :open)
    (is eq :menu (menu-state clock))
    (menu-key clock sim :backspace)
    (is eq :closed (menu-state clock))
    (menu-key clock sim :open)
    (menu-orbit! clock 800)
    (menu-key clock sim :return)
    (menu-key clock sim :backspace)
    (is eq :menu (menu-state clock))
    (menu-key clock sim :return)
    (frames-while-planning clock sim)
    (is = t0 (lis::sim-t sim))
    (is eq :shown (menu-state clock))
    (menu-key clock sim :backspace)
    (is eq :menu (menu-state clock))
    (false (lis::autopilot-model-plan (lis::clock-autopilot clock)))
    (menu-key clock sim :return)
    (frames-while-planning clock sim)
    (let ((plan (lis::autopilot-model-plan (lis::clock-autopilot clock))))
      (menu-key clock sim :return)
      (is eq :engaged (menu-state clock))
      (is eq plan (lis::sim-plan sim)))
    (menu-key clock sim :backspace)
    (is eq :off (menu-state clock))
    (false (lis::sim-plan sim))
    (is string= "AUTOPILOT OFF" (lis::autopilot-model-text (lis::clock-autopilot clock)))
    ;; Off: Return re-plans the row and engages once shown.
    (menu-key clock sim :return)
    (is eq :planning (menu-state clock))
    (frames-while-planning clock sim)
    (is eq :engaged (menu-state clock))
    (true (lis::sim-plan sim))
    (menu-key clock sim :backspace)
    (menu-key clock sim :backspace)
    (is eq :closed (menu-state clock))
    ;; Refused: 300 kg for 2,000 km; Backspace back to the menu.
    (setf (lis::ship-state-m-p (lis::sim-ship sim)) 300d0)
    (menu-key clock sim :open)
    (menu-orbit! clock 2000)
    (menu-key clock sim :return)
    (frames-while-planning clock sim)
    (is eq :refused (menu-state clock))
    (true (eql 0 (search "NEEDS " (lis::autopilot-model-text (lis::clock-autopilot clock)))))
    (true (find-if (lambda (m) (eql 0 (search "PLAN REFUSED: NEEDS " m))) (log-texts clock)))
    (menu-key clock sim :backspace)
    (is eq :menu (menu-state clock))))

(define-test autopilot-menu-abort-and-restart
  ;; An abort leaves the model :off with its text; a restart closes it.
  (let* ((sim (plan-sim))
         (clock (lis::new-clock sim)))
    (lis::autopilot-engage! clock sim (lis::sim-plan sim))
    (is eq :engaged (menu-state clock))
    (lis::sim-notice! sim :autopilot "AUTOPILOT ABORT: CONTACT")
    (lis::sim-plan-stop! sim)
    (lis::clock-take-notices! clock sim)
    (is eq :off (menu-state clock))
    (is string= "AUTOPILOT ABORT: CONTACT" (lis::autopilot-model-text (lis::clock-autopilot clock)))
    (lis::clock-restart! clock sim (lis::new-game-sim))
    (is eq :closed (menu-state clock))
    (false (lis::autopilot-model-flown (lis::clock-autopilot clock)))))

;;; Measurement at each cut, replans and trims (decision 117, task 205).

(defun crafted-plan (plan i &key (turn 0d0) (dv 0d0))
  "PLAN with burn I's attitude turned by TURN (rad) about its orbit's normal
and its Δv changed by DV, its shot's cut state and the goal measure kept."
  (let* ((burns (copy-seq (lis::burn-plan-burns plan)))
         (b (svref burns i))
         (r (lis::planned-burn-cut-r b))
         (v (lis::planned-burn-cut-v b))
         (n (lis::vec3-normalize (lis::vec3-cross r v))))
    (setf (svref burns i)
          (lis::make-planned-burn (lis::planned-burn-label b) (lis::planned-burn-hold-at b)
                                  (lis::planned-burn-start b)
                                  (lis::quat-mul (lis::quat-from-axis-angle n turn)
                                                 (lis::planned-burn-attitude b))
                                  (lis::planned-burn-throttle b) (lis::planned-burn-rcs b)
                                  (+ (lis::planned-burn-dv b) dv)
                                  :cut-t (lis::planned-burn-cut-t b) :cut-r r :cut-v v
                                  :isp (lis::planned-burn-isp b)))
    (lis::make-burn-plan burns :preset (lis::burn-plan-preset plan) :goal (lis::burn-plan-goal plan)
                               :goal-orbit (lis::burn-plan-goal-orbit plan)
                               :reserve-dv (lis::burn-plan-reserve-dv plan)
                               :reserve-mass (lis::burn-plan-reserve-mass plan)
                               :measure (lis::burn-plan-measure plan))))

(defun burn-level (sim)
  "×1 from a hold-at to the cut, else ×1,000."
  (let ((next (lis::plan-next-time sim)))
    (if (or (member (lis::sim-plan-phase sim) '(:holding :burning))
            (and next (<= next (lis::sim-t sim))))
        1 1000)))

(defun replan-run (sim plan &key frames)
  "PLAN engaged in SIM and flown through a clock at burn-level, by
clock-step!, with FRAMES by clock-frame!s of 1/60 s while a replan runs, to
its end. (values clock seen messages): SEEN ((t phase plan replans
replan-t) …) before each step that saw a new plan or a new phase, oldest
first; MESSAGES the autopilot's, in order (the log fills with warp's)."
  (let ((lis::*frame-step-time-limit* nil)
        (clock (lis::new-clock sim))
        (seen '())
        (messages '()))
    (flet ((message! ()
             (let ((text (second (assoc :autopilot (lis::clock-message-list clock)))))
               (when (and text (not (equal text (first messages))))
                 (push text messages)))))
      (setf (lis::clock-hook clock)
            (lambda (clock s)
              (let ((last (first seen)))
                (unless (and last (eq (third last) (lis::sim-plan s))
                             (eq (second last) (lis::sim-plan-phase s)))
                  (push (list (lis::sim-t s) (lis::sim-plan-phase s) (lis::sim-plan s)
                              (lis::sim-plan-replans s) (lis::sim-plan-replan-t s))
                        seen)))
              (message!)
              (lis::clock-set-level-index! clock (level-index (burn-level s)))
              nil))
      (lis::autopilot-engage! clock sim plan)
      (loop repeat 1000000
            while (lis::sim-plan sim)
            do (if (and frames (eq (lis::sim-plan-phase sim) :replan))
                   (lis::clock-frame! clock sim 1/60)
                   (lis::clock-step! clock sim)))
      (message!))
    (false (lis::sim-plan sim))
    (values clock (reverse seen) (reverse messages))))

(defun goal-score (sim measure)
  "MEASURE's largest residual (1: the tolerance) over a period from SIM."
  (let ((pred (lis::prediction-shoot! (lis::new-prediction) (lis::copy-sim sim)
                                      (+ (lis::sim-t sim) (lis::osculating-period sim)))))
    (lis::prediction-extend! pred most-positive-fixnum)
    (nth-value 1 (lis::goal-residuals! (make-array 4 :element-type 'double-float)
                                       measure pred (lis::sim-t sim)))))

(defun installs (seen plan)
  "The (t plan replans) of each plan installed after PLAN in SEEN."
  (loop for (time nil p replans) in seen
        for previous = plan then current
        for current = (or p previous)
        when (and p (not (eq p previous)))
          collect (list time p replans)))

(define-test autopilot-replan
  ;; The :point Hohmann 400 → 800 km, its first burn turned 3° in pitch
  ;; (5.8 m/s off its shot): :replan at its cut, the second burn re-shot and
  ;; installed at exactly the cut + 60 s by clock-step! and through
  ;; clock-frame!s alike; the goal met after it; twice the same sim.
  (let* ((start (model-sim :point))
         (plan (plan-now start :change-orbit '(800d3)))
         (crafted (crafted-plan plan 0 :turn (lis::deg->rad 3d0)))
         (runs (loop for frames in '(nil nil t)
                     collect (let ((sim (lis::copy-sim start)))
                               (multiple-value-bind (clock seen messages)
                                   (replan-run sim crafted :frames frames)
                                 (declare (ignore clock))
                                 (list sim seen messages))))))
    (destructuring-bind ((sim seen messages) (again &rest again-run) (framed framed-seen &rest framed-run))
        runs
      (declare (ignore again-run framed-run))
      (let ((cut (find :replan seen :key #'second))
            (new (installs seen crafted)))
        (true cut)
        (is = 1 (length new))
        (when (and cut new)
          (let ((cut-t (fifth cut))
                (installed (second (first new))))
            ;; The cut's step (×1) started at t_cut.
            (is = (+ cut-t 1/64) (first cut))
            (is = (+ cut-t 60) (first (first new)))
            (is = 1 (third (first new)))
            (is equal '(:circularise) (map 'list #'lis::planned-burn-label
                                           (lis::burn-plan-burns installed)))
            (let ((framed-new (installs framed-seen crafted)))
              (is = 1 (length framed-new))
              (is = (+ cut-t 60) (first (first framed-new)))
              (true (equalp installed (second (first framed-new)))))))
        (is equal '("AUTOPILOT: REPLANNED" "AUTOPILOT: PLAN COMPLETE") messages)
        (let ((score (goal-score sim (lis::burn-plan-measure plan))))
          (true (<= score 1d0) "~,3F of the tolerance" score))
        (true (equalp again sim))
        (true (equalp framed sim))))))

(define-test autopilot-trim
  ;; The Hohmann's last burn short by what leaves its periapsis 0.5 % low:
  ;; :replan at its cut, one :trim burn, the goal then met. A turn under
  ;; the threshold (0.1°, 0.2 m/s of 1.1) replans nothing.
  (let* ((start (model-sim :point))
         (plan (plan-now start :change-orbit '(800d3)))
         (r2 (+ lis::+radius-earth+ 800d3))
         (short (- (circular-speed 800d3) (nth-value 1 (transfer-speeds (* 0.995d0 r2) r2))))
         (sim (lis::copy-sim start)))
    (multiple-value-bind (clock seen messages) (replan-run sim (crafted-plan plan 1 :dv (- short)))
      (declare (ignore clock))
      (let ((new (installs seen plan)))
        (is = 2 (length new))
        (when (= 2 (length new))
          (is equal '(:trim) (map 'list #'lis::planned-burn-label
                                  (lis::burn-plan-burns (second (second new))))))
        (is equal '("AUTOPILOT: REPLANNED" "AUTOPILOT: PLAN COMPLETE") messages)
        (let ((score (goal-score sim (lis::burn-plan-measure plan))))
          (true (<= score 1d0) "~,3F of the tolerance" score))))
    (let ((sim (lis::copy-sim start)))
      (multiple-value-bind (clock seen messages)
          (replan-run sim (crafted-plan plan 0 :turn (lis::deg->rad 0.1d0)))
        (declare (ignore clock))
        (false (find :replan seen :key #'second))
        (is = 1 (length (installs seen plan)))
        (is equal '("AUTOPILOT: PLAN COMPLETE") messages)))))

(define-test autopilot-goal-missed
  ;; A third error: after two replans the cut ends the plan, "AUTOPILOT:
  ;; GOAL MISSED", the coast hold kept, no checkpoint asked for.
  (let* ((sim (model-sim :point))
         (plan (crafted-plan (plan-now sim :change-orbit '(800d3)) 0 :turn (lis::deg->rad 3d0)))
         (clock (lis::new-clock sim)))
    (lis::autopilot-engage! clock sim plan)
    (setf (lis::sim-plan-replans sim) 2)
    (step-until sim 1 (lambda (s) (or (null (lis::sim-plan s)) (lis::sim-notices s))) 1000000)
    (false (lis::sim-plan sim))
    (is equal '("AUTOPILOT: GOAL MISSED") (notice-texts sim))
    (false (lis::sim-checkpoint-request sim))
    (is eq :hold (lis::controls-attitude-mode (lis::sim-controls sim)))
    (is eq :prograde (lis::controls-hold (lis::sim-controls sim)))
    (lis::clock-take-notices! clock sim)
    (is eq :closed (menu-state clock))))

;;; A rendezvous's terminal phase and station-keeping (decision 120:
;;; Rendezvous terminal phase and station-keeping in the step). Fine steps
;;; only; ×1 and ×4 take the same steps.

(defun terminal-sim (target &key (number 1) (time (lis::start-t)) burn)
  "The start state at TIME under :j2, the ship at TARGET's aim point NUMBER
(range 100 m) at its velocity, nose prograde, full tanks, the objects
rechecked, engaged with a rendezvous plan to it: with BURN (an :arrival
planned-burn) pending, else in :terminal from TIME."
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-t sim) time
          (lis::sim-model sim) :j2)
    (let ((fixture (lis::rendezvous-stop sim target)))
      (multiple-value-bind (r v) (lis::aim-point sim target fixture 100d0 number time)
        (lis::ship-state-assign! (lis::sim-ship sim)
                                 (lis::make-ship-state r v (lis::hold-attitude :prograde r v 0d0)
                                                       (lis::make-vec3) lis::*propellant-full*)))
      (lis::sim-recheck-objects! sim)
      (lis::sim-engage-plan!
       sim (lis::make-burn-plan
            (vector (or burn (lis::make-planned-burn :arrival (- time 10) (- time 5) (lis::quat 1 0 0 0)
                                                     0.1d0 nil 0d0)))
            :rendezvous (lis::make-rendezvous-part target fixture 100d0 number 1 (lis::make-vec3)
                                                   time '())))
      (unless burn
        (setf (lis::sim-plan-phase sim) :terminal
              (lis::sim-plan-terminal-t sim) time))
      sim)))

(defun ship-arm ()
  "m: the grapple's tip ahead of the CoM along x, full tanks."
  (- (aref lis::*grapple-tip* 0)
     (aref (lis::mass-properties-com (lis::ship-mass-properties lis::*propellant-full*)) 0)))

(defun station-sim (target &optional (time (lis::start-t)))
  "terminal-sim with the tip at TARGET's stop point, moving with it, the
nose prograde: on station at the first step, then the :target hold's slew."
  (let* ((sim (terminal-sim target :time time))
         (fixture (lis::rendezvous-part-fixture (lis::burn-plan-rendezvous (lis::sim-plan sim)))))
    (multiple-value-bind (s sv) (stop-point sim target fixture time)
      (let ((rt (object-com sim target time)))
        (replace (lis::ship-state-r (lis::sim-ship sim))
                 (lis::vec3-add-scaled s (lis::vec3-normalize (lis::vec3-sub rt s)) (- (ship-arm))))
        (replace (lis::ship-state-v (lis::sim-ship sim)) sv)))
    (lis::sim-recheck-objects! sim)))

(defun terminal-relative (sim)
  "(values p u): the controlled point less the stop point, their relative velocity."
  (let ((p (lis::make-vec3)) (u (lis::make-vec3)))
    (true (lis::terminal-relative-into! p u sim (lis::burn-plan-rendezvous (lis::sim-plan sim))))
    (values p u)))

(defun controlled-point (sim)
  "The controlled point, Earth-centred (task 270's Skua Yard approach)."
  (let ((part (lis::burn-plan-rendezvous (lis::sim-plan sim))))
    (lis::vec3-add (terminal-relative sim)
                   (stop-point sim (lis::rendezvous-part-target part) (lis::rendezvous-part-fixture part)
                               (lis::sim-t sim)))))

(defun station-bounds (sim seconds &optional (level 4) (step (lambda (sim level) (step-sim sim 1 level))))
  "SIM stepped SECONDS at LEVEL in :station: (values the largest |p| and |u|)."
  (let ((end (+ (lis::sim-t sim) seconds)) (p-max 0d0) (u-max 0d0))
    (loop while (< (lis::sim-t sim) end)
          do (funcall step sim level)
             (is eq :station (lis::sim-plan-phase sim))
             (multiple-value-bind (p u) (terminal-relative sim)
               (setf p-max (max p-max (lis::vec3-norm p)) u-max (max u-max (lis::vec3-norm u)))))
    (values p-max u-max)))

(define-test terminal-approach-tern
  ;; From aim point (1) 1 km behind a promoted Tern Station at ×1, each
  ;; step's notices and checkpoint request handed to a clock, then 120 s of
  ;; station-keeping at ×4 (one orbit, 0.87 m and 0.011 m/s at most, takes
  ;; 1.8 s: task 270 moves it to the slow command). The closing speed exceeds |p| / 100 s by up to 0.20 m/s
  ;; between 600 and 360 m, where the RCS's 0.05 m/s² cannot follow w's
  ;; decay and the engine waits for 0.3 m/s (decision 162: The terminal
  ;; approach's bounds: the closing speed's lag and the controlled point).
  (let* ((sim (terminal-sim :tern-station))
         (clock (lis::new-clock sim))
         (t0 (lis::sim-t sim))
         (dv0 (lis::ship-state-dv (lis::sim-ship sim)))
         (tern (lis::sim-object sim :tern-station))
         (excess 0d0) (nearest most-positive-double-float))
    (is equal '(:tern-station) (promoted-keys sim))
    (loop until (or (eq (lis::sim-plan-phase sim) :station) (> (lis::sim-t sim) (+ t0 1800)))
          do (step-sim sim 1 1)
             (lis::clock-take-notices! clock sim)
             (multiple-value-bind (p u) (terminal-relative sim)
               (let ((n (lis::vec3-norm p)))
                 (setf excess (max excess (- (- (/ (lis::vec3-dot p u) n)) (/ n 100))))))
             (setf nearest (min nearest (lis::vec3-norm (lis::vec3-sub (lis::ship-state-r (lis::object-state tern))
                                                                (lis::ship-state-r (lis::sim-ship sim)))))))
    (is eq :station (lis::sim-plan-phase sim))
    (true (<= (- (lis::sim-t sim) t0) 1800d0) "~,1F s" (- (lis::sim-t sim) t0))
    (true (<= excess 0.3d0) "~,3F m/s" excess)
    (true (>= nearest 100d0) "~,1F m" nearest)
    (let ((dv (- (lis::ship-state-dv (lis::sim-ship sim)) dv0)))
      (true (<= dv 20d0) "~,2F m/s" dv))
    (is = 1 (count "ON STATION: TERN STATION" (log-texts clock) :test #'string=))
    (let ((checkpoint (lis::clock-checkpoint clock)))
      (true checkpoint)
      (is eq :station (lis::sim-plan-phase checkpoint)))
    (multiple-value-bind (p-max u-max)
        (station-bounds sim 120d0 4 (lambda (sim level)
                                      (step-sim sim 1 level)
                                      (lis::clock-take-notices! clock sim)))
      (true (<= p-max 5d0) "~,2F m" p-max)
      (true (<= u-max 0.05d0) "~,3F m/s" u-max))
    (is = 1 (count "ON STATION: TERN STATION" (log-texts clock) :test #'string=))
    (true (lis::sim-plan sim))))

(define-test terminal-station-keeping
  ;; Fulmar Depot's A (Sun-pointing) and K-17's fixture (spinning) for 150
  ;; s each, not the plan's quarter orbit (1.1 m and 0.6 m at most over it;
  ;; task 270 for the slow command); from the stop point, so the
  ;; :target hold's slew comes first.
  (dolist (target '(:fulmar-depot :k-17))
    (let ((sim (station-sim target (+ (lis::start-t) 3600d0))))
      (step-sim sim 1 1)
      (is eq :station (lis::sim-plan-phase sim))
      (multiple-value-bind (p-max u-max) (station-bounds sim 150d0)
        (true (<= p-max 5d0) "~A ~,2F m" target p-max)
        (true (<= u-max 0.05d0) "~A ~,3F m/s" target u-max)))))

(define-test terminal-warp
  (let ((lis::*frame-step-time-limit* nil))
    ;; The :arrival cut at ×100 (put back before each step: the burn's
    ;; drop-out lowers it) starts :terminal; the next step's check lowers
    ;; to ×4 with its message.
    (let* ((t0 (+ (lis::start-t) 60d0))
           (sim (terminal-sim :tern-station
                              :burn (lis::make-planned-burn :arrival t0 (+ t0 10) (lis::quat 1 0 0 0)
                                                            0.1d0 nil 0.05d0)))
           (clock (clock-at sim 100)))
      (loop until (eq (lis::sim-plan-phase sim) :terminal)
            do (lis::clock-set-level-index! clock (level-index 100))
               (lis::clock-step! clock sim))
      (let ((start (lis::sim-plan-terminal-t sim)))
        (true (on-grid-p start))
        (true (<= (+ t0 10) start (lis::sim-t sim))))
      (lis::clock-step! clock sim)
      (is = 4 (lis::clock-level clock))
      (true (messages-p clock "WARP ×4: TERMINAL PHASE"))
      ;; Above ×4 refused.
      (false (lis::clock-warp-up clock sim))
      (is = 4 (lis::clock-level clock))
      (true (member "WARP REFUSED: STATION-KEEPING, LIMIT ×4" (lis::clock-messages clock) :test #'string=)))
    ;; A coarse step releases the keys and the throttle.
    (let* ((sim (terminal-sim :tern-station))
           (controls (lis::sim-controls sim)))
      (step-sim sim 64 1)
      (replace (lis::controls-translation-keys controls) '(1 -1 1))
      (setf (lis::controls-throttle controls) 0.1d0)
      (step-sim sim 1 1000)
      (is equalp #(0 0 0) (lis::controls-translation-keys controls))
      (is = 0d0 (lis::controls-throttle controls))
      (is eq :terminal (lis::sim-plan-phase sim)))))

(define-test terminal-aborts
  ;; K-17 removed in :station: "TARGET LOST" at the next step.
  (let ((sim (station-sim :k-17)))
    (step-sim sim 1 1)
    (is eq :station (lis::sim-plan-phase sim))
    (setf (lis::sim-notices sim) nil)
    (setf (lis::object-status (lis::sim-object sim :k-17)) :removed)
    (step-sim sim 1 1)
    (check-aborted sim "TARGET LOST")
    (true (member "TARGET LOST: K-17" (notice-texts sim) :test #'string=)))
  ;; *terminal-timeout* at 60 s: the step starting at the start + 60 s aborts.
  (let ((lis::*terminal-timeout* 60d0)
        (sim (terminal-sim :tern-station)))
    (step-sim sim (* 60 64) 1)
    (is = (+ (lis::start-t) 60d0) (lis::sim-t sim))
    (is eq :terminal (lis::sim-plan-phase sim))
    (step-sim sim 1 1)
    (check-aborted sim "APPROACH TIMED OUT")))

(define-test terminal-determinism
  ;; 15 s of the approach at ×4 through clock-frame! at two frame rates,
  ;; each step compared with direct steps; a restored checkpoint flies the same.
  (dolist (frames (list '(1/60) *irregular-frames*))
    (let ((sim (terminal-sim :tern-station)))
      (is = 0 (clock-run sim 4 (+ (lis::start-t) 15d0) frames) "~A" frames)))
  (let* ((sim (step-sim (terminal-sim :tern-station) 640 4))
         (checkpoint (lis::take-checkpoint sim))
         (flown (lis::copy-sim (step-sim sim 1280 4))))
    (lis::restore-checkpoint! sim checkpoint)
    (true (equalp (step-sim sim 1280 4) flown))))

(define-test terminal-cons-nothing
  (let ((sim (station-sim :tern-station)))
    (step-sim sim 640 4)
    (is eq :station (lis::sim-plan-phase sim))
    (setf (lis::sim-notices sim) nil)
    (let ((consed (least-bytes-consed (lambda () (step-sim sim 10000 4)) 1)))
      (true (< consed 1024) "~D bytes" consed))
    (is eq :station (lis::sim-plan-phase sim))))
