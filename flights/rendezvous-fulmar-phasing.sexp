;;; rendezvous-fulmar-phasing: "Rendezvous: Fulmar Depot" (fixture A) from
;;; the worst phase, 296 deg short of the Hohmann's (docs/architecture.md
;;; "Scripts by milestone"; docs/design.md "Scripted runs"; decision 59:
;;; Rendezvous transfers pass the target clear).
;;;
;;; Closed form: 400 km circular (7,668.6 m/s), Fulmar Depot at 500 km. The
;;; Hohmann 400 -> 500 km: 27.4 + 27.3 m/s, 2,807.6 s, during which the
;;; depot moves 178.04 deg, so the phase angle is 1.96 deg; the ship starts
;;; with the depot 297.96 deg ahead, 296 deg short of it. A drift orbit
;;; makes it up within the 48 h limit: the search's 95.5 m/s impulsive with
;;; the phasing (decision 159); + 3 % for finite burns and 20 m/s of
;;; terminal phase: 118 m/s (decision 174; 126 in decision 171, 114 in
;;; decision 59). Planned at :j2 94.7 m/s, re-timed after the search's
;;; transfer passed the depot 1.7 m inside its clearance (task 276). The
;;; final transfer passes the depot >= its bound 33.7 m + 200 m.
;;;
;;; Screenshots (scripts/fly; PNGs not committed, decision 87; decision 131),
;;; each after a step or a condition wait, so the states equal the script's without them:
;;; - start (map): the ship and the depot's orbit, the depot far ahead.
;;; - plan (map, engaged): the trajectory with the phasing and the burn markers.
;;; - departure (map, in the departure burn): the burn marker under the ship.
;;; - arrival (map, in the arrival burn): the closest approach at the depot.
;;; - on-station (outside): Fulmar Depot with TANKER 2 beside the ship.
;;; - on-station-cockpit (cockpit): the screens at the stop.
((:start :state (:date "2040-01-01"
                 :orbit (:circular 400e3 :lead (:fulmar-depot 297.96))))
 (:screenshot "start" :view :map)
 (:autopilot :rendezvous (:fulmar-depot :a))
 (:warp :coast)
 (:screenshot "plan" :view :map)
 (:wait :in-burn :departure)
 (:screenshot "departure" :view :map)
 (:wait :in-burn :arrival)
 (:screenshot "arrival" :view :map)
 (:wait :autopilot-done :within 259200)
 (:screenshot "on-station")
 (:screenshot "on-station-cockpit" :view :cockpit)
 (:expect :on-station (:fulmar-depot :a))
 (:expect :elapsed-below 175620)         ; 48 h 47 min
 (:expect :dv-below 118)
 (:expect :clearance-above 234 :only (:fulmar-depot) :until :aim-point))
