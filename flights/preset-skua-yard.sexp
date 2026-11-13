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
;;;
;;; Screenshots (scripts/fly; PNGs not committed, decision 87), each at a
;;; condition wait only, so the states equal the script's without them:
;;; - start (cockpit, t 0): the cab in the start orbit, screens idle.
;;; - plan (map, engaged): the predicted trajectory, the target's orbit in magenta, burn markers with their Δv, the dashed path after the first burn.
;;; - plan-panel (outside): the autopilot panel top right with the burns, the message line.
;;; - departure (outside, in the departure burn): the engine lit (or the RCS).
;;; - departure-cockpit (cockpit): ATTITUDE on the burn attitude, the AUTOPILOT screen in the burn.
;;; - arrival (map, in the arrival burn): the closest approach at the place.
;;; - terminal (cockpit): the place through the window, TARGET's range closing.
;;; - terminal-outside (outside): the place lit near the ship.
;;; - on-station (outside): the place beside the ship.
;;; - on-station-cockpit (cockpit): the screens at the stop.
;;; - on-station-map (map): the ship at the place.
;;; Its plan has :departure and :arrival, no :phasing; the arrival burn starts
;;; inside 1,000 m, so the terminal shots wait on :terminal (the yard's rocks in
;;; their nets).
((:start :new-game)
 (:screenshot "start" :view :cockpit)
 (:autopilot :rendezvous :skua-yard)
 (:warp :coast)
 (:screenshot "plan" :view :map)
 (:screenshot "plan-panel")
 (:wait :in-burn :departure)
 (:screenshot "departure")
 (:screenshot "departure-cockpit" :view :cockpit)
 (:wait :in-burn :arrival)
 (:screenshot "arrival" :view :map)
 (:wait :terminal)
 (:screenshot "terminal" :view :cockpit)
 (:screenshot "terminal-outside")
 (:wait :autopilot-done :within 259200)
 (:screenshot "on-station")
 (:screenshot "on-station-cockpit" :view :cockpit)
 (:screenshot "on-station-map" :view :map)
 (:expect :on-station :skua-yard)
 (:expect :elapsed-below 180000)
 (:expect :dv-below 2873)
 (:expect :clearance-above 229.2 :until :aim-point))
