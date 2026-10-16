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
((:start :state (:date "2040-01-01"
                 :orbit (:circular 400e3 :lead (:fulmar-depot 297.96))))
 (:autopilot :rendezvous (:fulmar-depot :a))
 (:warp :coast)
 (:wait :autopilot-done :within 259200)
 (:expect :on-station (:fulmar-depot :a))
 (:expect :elapsed-below 175620)         ; 48 h 47 min
 (:expect :dv-below 118)
 (:expect :clearance-above 234 :only (:fulmar-depot) :until :aim-point))
