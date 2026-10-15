;;; raise-orbit: a Hohmann transfer from the 400 km start orbit to a
;;; circular 800 km one, both burns under (:warp :coast) (x100 while the
;;; engine runs), cut by :after-dv; the map shows the ring, the transfer
;;; ellipse and the raised ring (docs/architecture.md "Scripts by
;;; milestone"; decision 87: Flight scripts in milestone 2).
;;;
;;; Closed form: mu 3.986004418e14 m^3/s^2, r1 6,778,136.6 m (R 6,378,136.6
;;; + 400 km), r2 7,178,136.6 m, a 6,978,136.6 m: v1 7,668.56 m/s, first
;;; burn 109.12 m/s (38.1 s, 35,961 to 35,082 kg at v_e 4,410 m/s), half
;;; the transfer 2,900.6 s, second burn 107.56 m/s (36.6 s), centred on the
;;; apoapsis by :before. tests/flights.lisp flies it two-body (:model
;;; :point): both apsides 800 +- 4 km, the 400 km raise within 1 %.
;;;
;;; Render it: scripts/fly raise-orbit <dir>.
((:warp :coast)
 (:screenshot "start" :view :map)
 (:throttle 1.0)                        ; the start state's prograde hold
 (:after-dv 109.1 :throttle 0)
 ;; Apsides from the prediction (decision 87). J2 in the polar orbit puts
 ;; the true apsis radii ~7 km under the two-body 800 km (measured 792.6
 ;; km after the first burn, 792.1 and 799.7 after the second), so the
 ;; lower bounds are 785 km, not 792.
 (:expect :apoapsis-above 785e3)
 (:expect :apoapsis-below 808e3)
 (:expect :periapsis-above 385e3)
 (:expect :periapsis-below 405e3)
 (:screenshot "transfer" :view :map)
 (:wait :apoapsis :before 18.3)         ; half the second burn: 36.6 s flown
 (:throttle 1.0)
 (:after-dv 107.6 :throttle 0)
 (:expect :apoapsis-above 785e3)
 (:expect :apoapsis-below 808e3)
 (:expect :periapsis-above 785e3)
 (:expect :periapsis-below 808e3)
 (:screenshot "raised" :view :map)
 (:screenshot "raised-outside"))
