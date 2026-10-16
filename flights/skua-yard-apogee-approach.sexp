;;; skua-yard-apogee-approach: "Rendezvous: Skua Yard 5" from 1 km behind it
;;; at apogee, where the yard is crowded: the ship starts 334 m behind Skua
;;; Yard 1 and must pass it clear (docs/architecture.md "Scripts by
;;; milestone"; docs/design.md "Scripted runs", "The terminal path and
;;; crowded targets"; decision 59: Rendezvous transfers pass the target
;;; clear).
;;;
;;; Closed form: Highport's 23.7 h orbit, apogee radius 76,380 km (a
;;; 41,880 km, perigee 7,378 km), where the yard moves 0.96 km/s; 1 km
;;; behind at its velocity the ship stays co-orbital, so the least-Δv plan
;;; within the 48 h limit waits for a cheap departure (< 0.1 m/s) and may
;;; arrive anywhere on the orbit: away from apogee the yard spreads out and
;;; aim point (1) is clear. + 20 m/s of terminal phase: 21 m/s. The 48 h
;;; limit and the terminal phase: 50 h.
;;;
;;; Clearance: every object but the target stays >= its bound + 200 m away
;;; (decision 59), one expectation each.
((:start :state (:date (:apoapsis :highport)
                 :orbit (:near :skua-yard-5 :behind 1000)))
 (:autopilot :rendezvous :skua-yard-5)
 (:warp :coast)
 (:wait :autopilot-done :within 259200)
 (:expect :on-station :skua-yard-5)
 (:expect :elapsed-below 180000)
 (:expect :dv-below 21)
 (:expect :clearance-above 202.4 :only (:skua-yard-1))
 (:expect :clearance-above 202.7 :only (:skua-yard-2))
 (:expect :clearance-above 202.9 :only (:skua-yard-3))
 (:expect :clearance-above 203.2 :only (:skua-yard-4))
 (:expect :clearance-above 203.7 :only (:skua-yard-6))
 (:expect :clearance-above 203.1 :only (:pod-14))
 (:expect :clearance-above 229.2 :only (:rock-mover)))
