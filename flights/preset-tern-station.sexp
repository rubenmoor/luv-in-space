;;; preset-tern-station: "Rendezvous: Tern Station" from the start state by
;;; the autopilot alone (docs/architecture.md "Scripts by milestone";
;;; docs/design.md "Earth-orbit points of interest", "Scripted runs").
;;;
;;; Closed form: 400 km circular (7,668.6 m/s), Tern Station at 420 km,
;;; 20 deg ahead at the start. The Hohmann 400 -> 420 km: 5.5 + 5.5 m/s,
;;; 2,780 s, during which the station moves 179.3 deg, so the phase angle is
;;; 0.7 deg; the ship gains 1.0 deg/orbit on it, so A waits ~19 h. Δv: 11
;;; m/s + 3 m/s for a transfer that passes the station clear (decision 59),
;;; + 3 % for finite burns, + 20 m/s of terminal phase: 34.3 m/s. Until the
;;; aim point >= its bound 40 m + 200 m from its CoM.
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
;;; Its plan has :departure and :arrival, no :phasing; the terminal shots wait for
;;; the range under 1,000 m (just after the arrival cut).
((:start :new-game)
 (:screenshot "start" :view :cockpit)
 (:autopilot :rendezvous :tern-station)
 (:warp :coast)
 (:screenshot "plan" :view :map)
 (:screenshot "plan-panel")
 (:wait :in-burn :departure)
 (:screenshot "departure")
 (:screenshot "departure-cockpit" :view :cockpit)
 (:wait :in-burn :arrival)
 (:screenshot "arrival" :view :map)
 (:wait :range-below 1000)
 (:screenshot "terminal" :view :cockpit)
 (:screenshot "terminal-outside")
 (:wait :autopilot-done :within 259200)
 (:screenshot "on-station")
 (:screenshot "on-station-cockpit" :view :cockpit)
 (:screenshot "on-station-map" :view :map)
 (:expect :on-station (:tern-station :berth))
 (:expect :elapsed-below 86400)
 (:expect :dv-below 34.3)
 (:expect :clearance-above 240 :only (:tern-station) :until :aim-point))
