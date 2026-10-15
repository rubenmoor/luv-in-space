;;; skua-yard-apogee-approach: "Rendezvous: Skua Yard 5" from 1 km behind it
;;; near apogee, where the yard is crowded: the plan must pick aim point (2)
;;; and keep clear of Skua Yard 1 and POD 14 (docs/architecture.md "Scripts
;;; by milestone"; docs/design.md "Scripted runs", "The terminal path and
;;; crowded targets").
;;;
;;; Closed form: Highport's 23.7 h orbit, apogee radius 76,380 km (a
;;; 41,880 km, perigee 7,378 km), where the yard moves 0.96 km/s; 1 km
;;; behind at its velocity the ship stays co-orbital. The close grid flies
;;; <= 0.4 periods (~10 h) and the terminal phase; aim point (2) lies above
;;; the target, its path >= 560 m from Skua Yard 1's and POD 14's CoM.
;;;
;;; Fails as of 2026-10-15 (aim point 1, Skua Yard 1 at 265 m, 33 h at x4):
;;; task 274 fixes the plan or the expectations; until then it is not in
;;; scripts/test-slow.
((:start :state (:date (:apoapsis :highport)
                 :orbit (:near :skua-yard-5 :behind 1000)))
 (:autopilot :rendezvous :skua-yard-5)
 (:warp :coast)
 (:wait :autopilot-done :within 259200)
 (:expect :aim-point 2)
 (:expect :clearance-above 560 :except (:skua-yard-5))
 (:expect :on-station :skua-yard-5))
