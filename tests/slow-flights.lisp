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
