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
        (lis::*prediction-steps-per-frame* 64))
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
  ;; and ×4 over 120 s (not 600 s), the coarse levels over 5 orbits (not 10).
  (dolist (level lis::*warp-levels*)
    (let* ((sim (drift-sim))
           (end (+ (lis::sim-t sim) (if (<= level 4) 120d0 (* 5 (drift-period))))))
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
  ;; ×1 → ×1,000 → ×1 over one orbit, against ×1 throughout.
  (let* ((reference (drift-sim))
         (sim (drift-sim))
         (clock (lis::new-clock sim))
         (end (lis::grid-ceiling (+ (lis::start-t) (drift-period))))
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
         (lis::*prediction-steps-per-frame* 64))
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
  ;; Promoted at t, demoted after one step: the rails state is the
  ;; integrated one within 1 mm.
  (let* ((sim (ship-near :tern-station 1000d0))
         (tern (lis::sim-object sim :tern-station))
         (reference (step-sim (lis::copy-sim sim) 1 1)))
    (is equal '(:tern-station) (promoted-keys sim))
    (is equalp (lis::object-def-rails (lis::object-def-of tern)) (lis::object-rails tern))
    ;; The ship 300 km up: beyond 200 km after the step.
    (let ((r (lis::ship-state-r (lis::sim-ship sim))))
      (lis::vec3-add-scaled! r r (lis::vec3-normalize r) 300d3))
    (step-sim sim 1 1)
    (is eq :rails (lis::object-status tern))
    (is eq (lis::object-def-attitude (lis::object-def-of tern)) (lis::object-attitude tern))
    (let ((integrated (lis::object-state (lis::sim-object reference :tern-station))))
      (multiple-value-bind (r v) (object-rv tern (lis::sim-t sim))
        (let ((dr (lis::vec3-norm (lis::vec3-sub r (lis::ship-state-r integrated))))
              (dv (lis::vec3-norm (lis::vec3-sub v (lis::ship-state-v integrated)))))
          (true (< dr 1d-3) "~,3E m" dr)
          (true (< dv 1d-6) "~,3E m/s" dv))))
    (true (> (lis::object-next-check tern) (lis::sim-t sim)))))

(define-test clock-demoted-towable-attitude
  ;; K-17 spinning about +x keeps a :spin; tumbling, an :inertial at its q.
  (flet ((demoted (w)
           (let* ((sim (ship-near :k-17 1000d0))
                  (k-17 (lis::sim-object sim :k-17))
                  (state (lis::object-state k-17)))
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
  ;; Station flies as a reference integration of the station alone: 1 h.
  ;; 0.5 s, over the planned ~0.4 s, kept at the fine step the plan asks for.
  (let* ((sim (ship-near :tern-station 1000d0))
         (tern (lis::sim-object sim :tern-station))
         (end (+ (lis::sim-t sim) 3600d0))
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
  ;; orbit and demoted; every frame rate and a restore give the same
  ;; objects bit for bit.
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
              (false (eq (lis::object-def-rails (lis::object-def-of
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
