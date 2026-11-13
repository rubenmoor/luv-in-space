;;; preset-k-17: "Rendezvous: Derelict K-17" from the start state by the
;;; autopilot alone (docs/architecture.md "Scripts by milestone";
;;; docs/design.md "Earth-orbit points of interest", "Scripted runs").
;;;
;;; Closed form: 400 km circular, K-17 at 800 km, 270 deg ahead at the
;;; start. The Hohmann 400 -> 800 km: 109.1 + 107.6 m/s, 2,946 s; the ship
;;; gains 29.6 deg/orbit on ~263 deg: ~14 h. Δv: 217 m/s + 3 m/s for the
;;; clear pass (decision 59), + 3 %, + 20 m/s of terminal phase: 246.6 m/s.
;;; Until the aim point >= its bound 8.7 m + 200 m from its CoM.
;;;
;;; Screenshots (scripts/fly; PNGs not committed, decision 87), each at a
;;; condition wait only, so the states equal the script's without them:
;;; - start (cockpit, t 0): the cab in the start orbit, screens idle.
;;; - plan (map, engaged): the predicted trajectory, the target's orbit in magenta, burn markers with their Δv, the dashed path after the first burn.
;;; - plan-panel (outside): the autopilot panel top right with the burns, the message line.
;;; - phasing (map, in the phasing burn): the burn marker under the ship.
;;; - departure (outside, in the departure burn): the engine lit (or the RCS).
;;; - departure-cockpit (cockpit): ATTITUDE on the burn attitude, the AUTOPILOT screen in the burn.
;;; - arrival (map, in the arrival burn): the closest approach at the place.
;;; - terminal (cockpit): the place through the window, TARGET's range closing.
;;; - terminal-outside (outside): the place lit near the ship.
;;; - on-station (outside): the place beside the ship.
;;; - on-station-cockpit (cockpit): the screens at the stop.
;;; - on-station-map (map): the ship at the place.
;;; Its plan has a :phasing burn first, then :departure and :arrival; the arrival
;;; burn starts inside 1,000 m, so the terminal shots wait on :terminal.
((:start :new-game)
 (:screenshot "start" :view :cockpit)
 (:autopilot :rendezvous :k-17)
 (:warp :coast)
 (:screenshot "plan" :view :map)
 (:screenshot "plan-panel")
 (:wait :in-burn :phasing)
 (:screenshot "phasing" :view :map)
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
 (:expect :on-station :k-17)
 (:expect :elapsed-below 180000)
 (:expect :dv-below 246.6)
 (:expect :clearance-above 209 :only (:k-17) :until :aim-point))
