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
((:start :new-game)
 (:autopilot :rendezvous (:fulmar-depot :a))
 (:warp :coast)
 (:wait :autopilot-done :within 259200)
 (:expect :on-station (:fulmar-depot :a))
 (:expect :elapsed-below 180000)
 (:expect :dv-below 80.7)
 (:expect :clearance-above 234 :only (:fulmar-depot) :until :aim-point))
