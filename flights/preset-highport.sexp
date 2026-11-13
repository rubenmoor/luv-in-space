;;; preset-highport: "Rendezvous: Highport" (the plant's berth) from the
;;; start state by the autopilot alone (docs/architecture.md "Scripts by
;;; milestone"; docs/design.md "Earth-orbit points of interest", "Scripted
;;; runs").
;;;
;;; Closed form: 400 km circular to Highport's 1,000 x 70,000 km (a
;;; 41,878.1 km, e 0.82382, period 23.7 h) in the same plane: 2,762 m/s by
;;; Hohmann-like transfers; the 48 h search does the phasing. Δv: 2,762 +
;;; 3 m/s for the clear pass (decision 59), + 3 %, + 20 m/s of terminal
;;; phase: 2,868 m/s. Within the 48 h limit and the terminal phase: 50 h.
;;; Until the aim point >= its bound 73 m + 200 m from its CoM.
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
;;; inside 1,000 m, so the terminal shots wait on :terminal.
((:start :new-game)
 (:screenshot "start" :view :cockpit)
 (:autopilot :rendezvous (:highport :plant))
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
 (:expect :on-station (:highport :plant))
 (:expect :elapsed-below 180000)
 (:expect :dv-below 2868)
 (:expect :clearance-above 273 :only (:highport) :until :aim-point))
