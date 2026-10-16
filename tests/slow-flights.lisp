;;; The slow tests (scripts/test-slow; decision 123: Milestone 3's flight
;;; scripts run in scripts/test-slow, not scripts/test): milestone 3's flight
;;; scripts, each flown twice bit for bit, and the long runs plan 27 left for
;;; the slow command. Helpers from luv-in-space/tests.

(defpackage #:luv-in-space.slow-tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:lis #:luv-in-space) (#:lt #:luv-in-space.tests)))

(in-package #:luv-in-space.slow-tests)

(declaim (notinline make-instance))

(defun fly-script (name)
  "Fly flights/NAME.sexp twice: passed, the end sims equalp and the records
equal. Prints its line: game time, steps, wall time, PASSED or FAILED.
Returns the record and the end sim."
  (let ((start (lis::flight-start-sim (lis::read-flight name)))
        (wall (get-internal-real-time)))
    (multiple-value-bind (record sim) (lis::run-flight name)
      (multiple-value-bind (again again-sim) (lis::run-flight name)
        (let ((passed (and (lis::flight-record-passed-p record)
                           (equalp sim again-sim)
                           (lt::record-equal record again))))
          (true (lis::flight-record-passed-p record) "~A:~%~A" name (lt::script-record-lines record))
          (true (equalp sim again-sim) "~A: end sims differ" name)
          (true (lt::record-equal record again) "~A: records differ" name)
          (format t "~&~28A game ~10,1F s  steps ~9D  wall ~5,1F s (twice)  ~:[FAILED~;PASSED~]~%"
                  name (- (lis::sim-t sim) (lis::sim-t start))
                  (- (lis::sim-steps sim) (lis::sim-steps start))
                  (/ (- (get-internal-real-time) wall) internal-time-units-per-second)
                  passed)
          (finish-output)
          (values record sim))))))

;;; The approach and rendezvous scripts (docs/architecture.md "Scripts by
;;; milestone"). skua-yard-apogee-approach is left out until task 274
;;; (it picks aim point 1 and passes Skua Yard 1 at 265 m).

(define-test approach-tern
  (multiple-value-bind (record sim) (fly-script "approach-tern")
    (declare (ignore record))
    (true (lis::sim-berthed sim))))

(define-test approach-k-17
  ;; The stack's mass is the ship's plus K-17's 2,500 kg exactly.
  (multiple-value-bind (record sim) (fly-script "approach-k-17")
    (declare (ignore record))
    (is = (+ (lis::mass-properties-mass
              (lis::ship-mass-properties (lis::ship-state-m-p (lis::sim-ship sim)) nil))
             2500d0)
        (lis::with-stack-properties (properties sim)
          (lis::mass-properties-mass properties)))))

(define-test rendezvous-fulmar-phasing
  (fly-script "rendezvous-fulmar-phasing"))

(define-test rendezvous-skua-yard
  (fly-script "rendezvous-skua-yard"))

;;; plan 27's runs marked for the slow command (tests/autopilot.lisp).

(define-test rendezvous-search-highport-phases
  ;; rendezvous-search-highport (phases 0 and 4) at the odd phases from
  ;; Fulmar Depot's 500 km: <= 2,758 m/s. Not phases 2 (2,784.6) and 6
  ;; (2,770.7): task 275 finds why.
  (dolist (k '(1 3 5 7))
    (let* ((sim (lt::highport-phase-sim k))
           (candidates (lt::search-now sim :highport)))
      (lt::check-candidates sim candidates lis::*rendezvous-limit*)
      (true (<= (lis::rendezvous-candidate-total (svref candidates 0)) 2758d0)
            "phase ~D: ~,1F m/s" k (lis::rendezvous-candidate-total (svref candidates 0))))))

(define-test rendezvous-plan-places
  ;; rendezvous-plan-highport's check for the other four places.
  (lt::rendezvous-places '(:tern-station :fulmar-depot :k-17 :skua-yard)))

;;; The preset scripts: every Earth-orbit place by the autopilot alone from
;;; the start state (plan 28's Done when). preset-fulmar-depot is left out
;;; until task 277 (ON STATION while the ship still turns: 0.452 m/s at the
;;; tip), preset-skua-yard until task 278 (the stop point's rails velocity
;;; ~1.05 m/s off its position's rate: APPROACH TIMED OUT at 103 m).

(define-test preset-tern-station
  (fly-script "preset-tern-station"))

(define-test preset-k-17
  (fly-script "preset-k-17"))

(define-test preset-highport
  (fly-script "preset-highport"))

;;; plan 27 piece 5's terminal tests at full length (task 270: Slow tests:
;;; full-length station-keeping and the Skua Yard 5 terminal approach),
;;; shortened in scripts/test (tests/clock.lisp).

(defun rails-period (key)
  "s: the period of KEY's first rails segment."
  (lis::orbit-period (lis::rails-segment-orbit (first (lis::object-def-rails (lis::object-definition key))))))

(defun check-station (target time seconds level)
  "From TARGET's stop point at TIME, SECONDS of station-keeping at LEVEL:
|p| <= 5 m and |u| <= 0.05 m/s at every step."
  (let ((sim (lt::station-sim target time)))
    (lt::step-sim sim 1 1)
    (is eq :station (lis::sim-plan-phase sim))
    (multiple-value-bind (p-max u-max) (lt::station-bounds sim seconds level)
      (true (<= p-max 5d0) "~A x~D ~,2F m" target level p-max)
      (true (<= u-max 0.05d0) "~A x~D ~,3F m/s" target level u-max))))

(define-test station-keeping-tern-orbit
  ;; One orbit at Tern Station at x1 and x4 (0.87 m, 0.011 m/s at most).
  (dolist (level '(1 4))
    (check-station :tern-station (lis::start-t) (rails-period :tern-station) level)))

(define-test station-keeping-quarter-orbit
  ;; Fulmar Depot's A (Sun-pointing; 1.02 m, 0.010 m/s) and K-17's fixture
  ;; (spinning; 0.61 m, 0.008 m/s), a quarter orbit each at x4.
  (dolist (target '(:fulmar-depot :k-17))
    (check-station target (+ (lis::start-t) 3600d0) (/ (rails-period target) 4) 4)))

(defun skua-apogee ()
  "Skua Yard 5's first apogee, on the grid."
  (lt::grid-round (lis::orbit-next-apsis (lis::rails-segment-orbit
                                          (first (lis::object-def-rails (lis::object-definition :skua-yard-5))))
                                         (lis::start-t) :apoapsis)))

(define-test terminal-approach-skua-yard
  ;; From aim point (2) above Skua Yard 5 at its first apogee: the
  ;; controlled point's path (the checked segment's, 563.0 m from Skua
  ;; Yard 1) keeps 560 m from Skua Yard 1's and POD 14's CoM; the CoM, 6.3
  ;; m behind the tip, comes to 557 m of Skua Yard 1 (decision 162: The
  ;; terminal approach's bounds: the closing speed's lag and the controlled
  ;; point).
  (let ((sim (lt::terminal-sim :skua-yard-5 :number 2 :time (skua-apogee)))
        (nearest (list (cons :skua-yard-1 most-positive-double-float)
                       (cons :pod-14 most-positive-double-float))))
    (loop until (or (eq (lis::sim-plan-phase sim) :station) (null (lis::sim-plan sim)))
          do (lt::step-sim sim 1 1)
             (let ((point (lt::controlled-point sim)))
               (dolist (entry nearest)
                 (setf (cdr entry)
                       (min (cdr entry)
                            (lis::vec3-norm (lis::vec3-sub point (lt::object-com sim (car entry)
                                                                                 (lis::sim-t sim)))))))))
    (is eq :station (lis::sim-plan-phase sim))
    (dolist (entry nearest)
      (true (>= (cdr entry) 560d0) "~A ~,1F m" (car entry) (cdr entry)))))
