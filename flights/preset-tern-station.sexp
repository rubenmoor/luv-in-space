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
((:start :new-game)
 (:autopilot :rendezvous :tern-station)
 (:warp :coast)
 (:wait :autopilot-done :within 259200)
 (:expect :on-station (:tern-station :berth))
 (:expect :elapsed-below 86400)
 (:expect :dv-below 34.3)
 (:expect :clearance-above 240 :only (:tern-station) :until :aim-point))
