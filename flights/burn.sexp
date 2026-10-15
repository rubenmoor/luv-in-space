;;; burn: a 10 s full-throttle prograde burn from the start orbit at x1,
;;; seen from outside before and during it, the raised orbit on the map
;;; after (docs/architecture.md "Scripts by milestone").
;;;
;;; Closed form: mu 3.986004418e14 m^3/s^2, r 6,378,136.6 + 400 km =
;;; 6,778,136.6 m, v_circ 7,668.56 m/s; 101.8 kN at v_e 4,410 m/s burns
;;; 23.08 kg/s, so 10 s takes 35,961 to 35,730.2 kg (230.8 kg) and gives
;;; 4,410 ln(35,961/35,730.2) = 28.40 m/s: apoapsis 501.3 km two-body.
;;;
;;; Render it: scripts/fly burn <dir>.
((:screenshot "before")
 (:after 10 :throttle 1.0)              ; the start state's prograde hold
 (:after 5 :screenshot "during")
 (:after 5 :throttle 0)
 ;; The prediction's apoapsis (decision 87): 494.6 km measured, J2 taking
 ;; ~7 km off the two-body 501.3 km; 495 +- 10 km.
 (:expect :apoapsis-above 485e3)
 (:expect :apoapsis-below 505e3)
 (:expect :periapsis-above 380e3)
 (:expect :periapsis-below 405e3)
 (:expect :propellant-above 27764.0)    ; 28,000 - 230.8 = 27,769.2 kg, less 5 (the test: +- 5)
 (:screenshot "after" :view :map))
