(in-package #:luv-in-space.tests)

;;; clock.lisp: the simulation state, the start state, sim-step! and
;;; checkpoints (docs/architecture.md "The clock", "Checkpoints"; decision
;;; 72: Simulation state and the one step function in clock.lisp).

(defun step-sim (sim n level &optional until)
  "N sim-step!s of SIM at LEVEL, fewer if it reaches UNTIL; SIM."
  (declare (type fixnum n level) (inline lis::sim-step!))
  (dotimes (i n sim)
    (when (and until (>= (lis::sim-t sim) until))
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
    (let ((consed (let ((before (sb-ext:get-bytes-consed)))
                    (lis::restore-checkpoint! sim checkpoint)
                    (- (sb-ext:get-bytes-consed) before))))
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
    (step-sim (step-sim a 5000 1) 5000 1000)
    (let ((consed (let ((before (sb-ext:get-bytes-consed)))
                    (step-sim (step-sim b 5000 1) 5000 1000)
                    (- (sb-ext:get-bytes-consed) before))))
      (true (< consed 1024) "~D bytes" consed))
    (true (equalp a b))))
