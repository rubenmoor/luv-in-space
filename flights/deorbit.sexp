;;; deorbit: turn retrograde, burn the periapsis down to 50 km, 50 km below
;;; the crash radius (R + 100 km), fall, crash at the predicted time, see
;;; the glow and the LOST panel, restart at the start state
;;; (docs/architecture.md "Scripts by milestone"; docs/design.md "Dying and
;;; what follows"; decision 34: How death works).
;;;
;;; Closed form: mu 3.986004418e14 m^3/s^2, r 6,778,136.6 m (R 6,378,136.6
;;; + 400 km), v_circ 7,668.56 m/s; the 400 x 50 km ellipse (a 6,603,136.6
;;; m) has 7,566.26 m/s at its apoapsis: 102.30 m/s retrograde (35.7 s at
;;; v_e 4,410 m/s).
;;;
;;; Render it: scripts/fly deorbit <dir>.
((:hold :retrograde)
 (:wait :settled :within 120)
 (:screenshot "retrograde")
 (:warp :coast)
 (:throttle 1.0)
 (:after-dv 102.3 :throttle 0)
 (:expect :periapsis-below 100e3)
 (:screenshot "falling" :view :map)
 (:wait :altitude-below 150e3)
 (:screenshot "entry")
 (:wait :impact)
 (:expect :lost :crash)
 (:expect :impact-as-predicted 0.1)
 (:screenshot "lost")
 (:restart)
 (:expect :start-state)
 (:screenshot "restarted"))
