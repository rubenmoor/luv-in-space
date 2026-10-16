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
((:start :new-game)
 (:autopilot :rendezvous (:highport :plant))
 (:warp :coast)
 (:wait :autopilot-done :within 259200)
 (:expect :on-station (:highport :plant))
 (:expect :elapsed-below 180000)
 (:expect :dv-below 2868)
 (:expect :clearance-above 273 :only (:highport) :until :aim-point))
