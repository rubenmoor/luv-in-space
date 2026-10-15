;;; approach-k-17: the last 100 m to Derelict K-17's fixture by the
;;; :approach pilot, latched (docs/architecture.md "Scripts by milestone",
;;; "The terminal approach"; decision 122: Flight scripts in milestone 3).
;;;
;;; Closed form: the grapple's tip 100 m out on K-17's axis at its motion
;;; (it spins 0.3 deg/s about the axis, so the pin stands still); the pilot
;;; closes at (range - 20 m) / 30 s, at most 0.1 m/s, holds at 20 m, then
;;; 0.1 m/s to contact: ~1,000 s. The stack's mass is the ship's plus
;;; K-17's 2,500 kg exactly (tests/slow-flights.lisp checks it).
((:start :state (:orbit (:on-axis :k-17 100)))
 (:approach :k-17)
 (:expect :latched :k-17)
 (:expect :propellant-above 27900))
