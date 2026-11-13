;;; preset-fulmar-depot: "Rendezvous: Fulmar Depot" (fixture A) from the
;;; start state by the autopilot alone (docs/architecture.md "Scripts by
;;; milestone"; docs/design.md "Earth-orbit points of interest", "Scripted
;;; runs").
;;;
;;; Closed form: 400 km circular, Fulmar Depot at 500 km, 180 deg ahead at
;;; the start. The Hohmann 400 -> 500 km: 27.4 + 27.3 m/s, 2,807.6 s; the
;;; phase angle 1.96 deg, so the ship gains 7.8 deg/orbit on 178 deg:
;;; ~36 h. Δv: 56 m/s + 3 m/s for the clear pass (decision 59), + 3 %,
;;; + 20 m/s of terminal phase: 80.7 m/s. Until the aim point >= its bound
;;; 33.7 m + 200 m from its CoM.
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
 (:autopilot :rendezvous (:fulmar-depot :a))
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
 (:expect :on-station (:fulmar-depot :a))
 (:expect :elapsed-below 180000)
 (:expect :dv-below 80.7)
 (:expect :clearance-above 234 :only (:fulmar-depot) :until :aim-point))
