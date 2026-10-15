(in-package #:luv-in-space.tests)

;;; flights.lisp: the safe reader and the step parser; prediction-apsis
;;; (docs/architecture.md "Scripted runs"; decision 87: Flight scripts in
;;; milestone 2: additions, apsis conditions, live fly and scripts/fly).

(defun script-error-of (source)
  "The flight-script-error reading SOURCE signals, or nil."
  (handler-case (progn (lis::read-flight source) nil)
    (lis::flight-script-error (e) e)))

(defun script-error-text (source)
  (let ((e (script-error-of source)))
    (and e (princ-to-string e))))

(define-test flights-reader-rejects
  (let ((packages (length (list-all-packages))))
    (true (script-error-of "((:throttle #.(+ 1 0)))"))
    (true (script-error-of "(#S(lis::flight-step))"))
    (true (search "FOO IS NOT A KEYWORD" (script-error-text "((:throttle 1.0)
 (:hold foo))")))
    (true (search "SECOND FORM" (script-error-text "((:throttle 1.0)) ((:throttle 0))")))
    (true (search "NOT BEFORE MILESTONE 3" (script-error-text "((:autopilot :hohmann 800e3))")))
    (true (search "NOT BEFORE MILESTONE 3" (script-error-text "((:start :state (:date \"2040-01-01\")))")))
    (true (search "NOT BEFORE MILESTONE 5" (script-error-text "((:wait :quest-step \"2.3\"))")))
    (true (search "NOT BEFORE MILESTONE 4" (script-error-text "((:expect :soi :moon))")))
    (true (search ":BOGUS" (script-error-text "((:bogus 1))")))
    (true (search "THROTTLE" (script-error-text "((:throttle 0.05))")))
    (true (script-error-of "((:expect :apoapsis))"))
    (true (script-error-of "((:wait :altitude-below 120e3 :before 10))"))
    (true (script-error-of "((:throttle 1.0) (:start :new-game))"))
    (true (search "NO SCRIPT" (script-error-text "no-such-script")))
    ;; The line of the offending step.
    (let ((e (script-error-of "(; a comment
  (:throttle 1.0)
  (:after 10 :throttle 0)
  (:grapple :arm))")))
      (true e)
      (when e
        (is = 4 (lis::flight-script-error-line e))
        (true (search "LINE 4" (princ-to-string e)))))
    (is = packages (length (list-all-packages)))))

(define-test flights-reader-parses
  (let ((steps (lis::read-flight "((:start :new-game)
 (:at 0.01 :warp 1)
 (:after-dv 9.10 :throttle 0)
 (:after 10 :throttle 0)
 (:after 300)
 (:after-dv 25.0 :throttle 0)
 (:keys (:w :h) :for 2)
 (:fine t)
 (:hold nil)
 (:wait :apoapsis :before 18.5)
 (:wait :date \"2040-01-01T01:00:00\" :within 7200)
 (:expect :lost :crash)
 (:screenshot \"falling\" :view :map))")))
    (is = 13 (length steps))
    (destructuring-bind (start at bare after pass dv keys fine hold apsis date lost shot) steps
      (is eq :start (lis::flight-step-name start))
      (is = 1/64 (lis::flight-step-at at))
      (is equal '(1) (lis::flight-step-args at))
      (is eq :action (lis::flight-step-kind bare))
      (is = 0 (lis::flight-step-after bare))
      (false (lis::flight-step-at bare))
      (is eql 9.1d0 (first (lis::flight-step-args bare)))
      (is = 10 (lis::flight-step-after after))
      (is eq :pass (lis::flight-step-kind pass))
      (is = 300 (lis::flight-step-after pass))
      (is equal '(25d0) (lis::flight-step-args dv))
      (is equal '((:w :h)) (lis::flight-step-args keys))
      (is = 2 (lis::flight-step-for keys))
      (is equal '(t) (lis::flight-step-args fine))
      (is equal '(nil) (lis::flight-step-args hold))
      (is eq :wait (lis::flight-step-kind apsis))
      (is = 18.5 (lis::flight-step-before apsis))
      (is = (* 30 86400) (lis::flight-step-within apsis))
      (is = (lis::utc->grid-t 2040 1 1 1 0 0) (first (lis::flight-step-args date)))
      (is = 7200 (lis::flight-step-within date))
      (is eq :expect (lis::flight-step-kind lost))
      (is equal '(:crash) (lis::flight-step-args lost))
      (is eq :map (lis::flight-step-view shot))
      (is equal '("falling") (lis::flight-step-args shot))))
  ;; grid-ceiling: 0 stays, anything above rounds up.
  (is = 0 (lis::flight-step-at (first (lis::read-flight "((:at 0 :throttle 1.0))"))))
  (is eq :outside (lis::flight-step-view (first (lis::read-flight "((:screenshot \"x\"))")))))

(define-test flights-prediction-apsis
  ;; Coasting after a small prograde burn: the Hermite's apoapsis.
  (let* ((sim (lis::new-game-sim))
         (v (lis::ship-state-v (lis::sim-ship sim))))
    (lis::vec3-scale! v v 1.003d0)
    (let ((pred (run-prediction sim)))
      (multiple-value-bind (time radius) (lis::prediction-apsis pred :apoapsis)
        (true time)
        (when time
          (is = (lis::prediction-event pred :apoapsis) time)
          (let ((flown (step-sim (lis::copy-sim sim) 100000 1000 time)))
            (is = time (lis::sim-t flown))
            (true (< (abs (- radius (lis::vec3-norm (lis::ship-state-r (lis::sim-ship flown)))))
                     1d-3)))
          (true (< 400d3 (- radius lis::+radius-earth+) 500d3))))))
  ;; An impact first: the osculating periapsis of the impact state.
  (let* ((pred (run-prediction (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3)))))
         (impact (lis::prediction-event pred :impact)))
    (true impact)
    (when impact
      (multiple-value-bind (time radius) (lis::prediction-apsis pred :periapsis)
        (true (> time impact))
        (true (< (abs (- radius (- *earth-crash-radius* 50d3))) 1d0)
              "periapsis ~,3F m off" (- radius (- *earth-crash-radius* 50d3)))))))
