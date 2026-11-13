;;; approach-k-17: the last 100 m to Derelict K-17's fixture by the
;;; :approach pilot, latched (docs/architecture.md "Scripts by milestone",
;;; "The terminal approach"; decision 122: Flight scripts in milestone 3).
;;;
;;; Closed form: the grapple's tip 100 m out on K-17's axis at its motion
;;; (it spins 0.3 deg/s about the axis, so the pin stands still); the pilot
;;; closes at (range - 20 m) / 30 s, at most 0.1 m/s, holds at 20 m, then
;;; 0.1 m/s to contact: ~1,000 s. The stack's mass is the ship's plus
;;; K-17's 2,500 kg exactly (tests/slow-flights.lisp checks it).
;;;
;;; Screenshots (scripts/fly; PNGs not committed, decision 87; decision 131),
;;; none changes the flown states:
;;; - start (cockpit): K-17's fixture ahead at 100 m through the window.
;;; - start-outside (outside): K-17 ahead of the ship.
;;; - approach-20m-cockpit (cockpit): the grapple camera on the TARGET screen at the 20 m hold.
;;; - approach-20m-outside (outside): the ship 20 m from K-17.
;;; - latched (cockpit): the grapple camera, GRPL green, on the pin.
;;; - latched-outside (outside): the stack with K-17 latched.
((:start :state (:orbit (:on-axis :k-17 100)))
 (:screenshot "start" :view :cockpit)
 (:screenshot "start-outside")
 (:approach :k-17 :screenshots (:cockpit :outside))
 (:screenshot "latched" :view :cockpit)
 (:screenshot "latched-outside")
 (:expect :latched :k-17)
 (:expect :propellant-above 27900))
