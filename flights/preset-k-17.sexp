;;; preset-k-17: "Rendezvous: Derelict K-17" from the start state by the
;;; autopilot alone (docs/architecture.md "Scripts by milestone";
;;; docs/design.md "Earth-orbit points of interest", "Scripted runs").
;;;
;;; Closed form: 400 km circular, K-17 at 800 km, 270 deg ahead at the
;;; start. The Hohmann 400 -> 800 km: 109.1 + 107.6 m/s, 2,946 s; the ship
;;; gains 29.6 deg/orbit on ~263 deg: ~14 h. Δv: 217 m/s + 3 m/s for the
;;; clear pass (decision 59), + 3 %, + 20 m/s of terminal phase: 246.6 m/s.
;;; Until the aim point >= its bound 8.7 m + 200 m from its CoM.
((:start :new-game)
 (:autopilot :rendezvous :k-17)
 (:warp :coast)
 (:wait :autopilot-done :within 259200)
 (:expect :on-station :k-17)
 (:expect :elapsed-below 180000)
 (:expect :dv-below 246.6)
 (:expect :clearance-above 209 :only (:k-17) :until :aim-point))
