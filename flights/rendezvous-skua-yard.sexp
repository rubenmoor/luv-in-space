;;; rendezvous-skua-yard: "Rendezvous: Skua Yard 5" after leaving Highport's
;;; plant an hour before perigee (docs/architecture.md "Scripts by
;;; milestone"; docs/design.md "Scripted runs"; decision 163: Rendezvous:
;;; Skua Yard 5's clearance is decision 59's, not the design's 500 m).
;;;
;;; Closed form: Highport and Skua Yard share the 23.7 h orbit (perigee
;;; radius 7,378 km, a 41,880 km); the yard trails Highport by 29-31 s, so
;;; one phasing revolution costs 2 x 0.11 m/s (320 / N m/s per half period
;;; at perigee); + 20 m/s of terminal phase at most: 21 m/s. One phasing
;;; revolution (23.7 h), the wait for perigee and the terminal phase: 26 h.
;;;
;;; Clearance: every object but the target stays >= its bound + 200 m away
;;; (decision 59), one expectation each. Not Highport: the engage comes 360 s
;;; after the release at 0.2 m/s, ~72 m from the plant (the end-to-end test
;;; rendezvous-end-to-end-skua-yard-5 holds it clear from the first burn on).
;;;
;;; Screenshots (scripts/fly; PNGs not committed, decision 87; decision 131),
;;; each after a step or a condition wait, so the states equal the script's without them:
;;; - berthed (outside): the ship at Highport's plant.
;;; - released (outside): the ship clear of the plant after the 0.2 m/s push.
;;; - plan (map, engaged): the trajectory, the yard's orbit, burn markers.
;;; - departure (outside, in the departure burn).
;;; - terminal (cockpit): the yard's rocks in their nets through the window.
;;; - on-station (outside): Skua Yard 5 beside the ship.
;;; - on-station-map (map): the ship at the yard.
((:start :state (:date (:periapsis :highport :before 3600)
                 :berthed (:highport :plant)))
 (:screenshot "berthed")
 (:release)
 (:translate :aft :dv 0.2)
 (:screenshot "released")
 (:after 300)
 (:autopilot :rendezvous :skua-yard-5)
 (:warp :coast)
 (:screenshot "plan" :view :map)
 (:wait :in-burn :departure)
 (:screenshot "departure")
 (:wait :terminal)
 (:screenshot "terminal" :view :cockpit)
 (:wait :autopilot-done :within 259200)
 (:screenshot "on-station")
 (:screenshot "on-station-map" :view :map)
 (:expect :on-station :skua-yard-5)
 (:expect :elapsed-below 93600)
 (:expect :dv-below 21)
 (:expect :clearance-above 202.4 :only (:skua-yard-1))
 (:expect :clearance-above 202.7 :only (:skua-yard-2))
 (:expect :clearance-above 202.9 :only (:skua-yard-3))
 (:expect :clearance-above 203.2 :only (:skua-yard-4))
 (:expect :clearance-above 203.7 :only (:skua-yard-6))
 (:expect :clearance-above 203.1 :only (:pod-14))
 (:expect :clearance-above 229.2 :only (:rock-mover)))
