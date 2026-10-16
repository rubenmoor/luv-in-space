;;; preset-skua-yard: "Rendezvous: Skua Yard" from the start state by the
;;; autopilot alone (docs/architecture.md "Scripts by milestone";
;;; docs/design.md "Earth-orbit points of interest", "Scripted runs").
;;; Skua Yard 3 holds the drop point at the start, so the stop point is the
;;; yard's front (decision 121: Skua Yard's stop point while the drop point
;;; is occupied).
;;;
;;; Closed form: Highport's orbit, 30 km behind it: 2,762 m/s as Highport,
;;; < 5 m/s more for the yard. Δv: (2,762 + 5) x 1.03 + 3 m/s for the clear
;;; pass + 20 m/s of terminal phase: 2,873 m/s. Within the 48 h limit and
;;; the terminal phase: 50 h. Clearance: every object of the yard and POD 14
;;; >= the largest bound (the rock mover's 29.2 m) + 200 m, until the aim
;;; point; no :only, the stop point is no object's.
((:start :new-game)
 (:autopilot :rendezvous :skua-yard)
 (:warp :coast)
 (:wait :autopilot-done :within 259200)
 (:expect :on-station :skua-yard)
 (:expect :elapsed-below 180000)
 (:expect :dv-below 2873)
 (:expect :clearance-above 229.2 :until :aim-point))
