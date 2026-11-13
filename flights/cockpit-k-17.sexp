;;; cockpit-k-17: the rendezvous and capture flown in the cockpit view,
;;; "Flying from the cockpit alone" (docs/design.md "The cockpit" →
;;; "Interaction"; docs/architecture.md "Scripts by milestone"; decision 131:
;;; Flight screenshots and live runs in milestone 3).
;;;
;;; Closed form: preset-k-17's rendezvous from the start state (Δv 246.6 m/s
;;; at most), then approach-k-17's last 100 m: the stack's mass is the ship's
;;; plus K-17's 2,500 kg exactly (tests/slow-flights.lisp checks it); the
;;; approach spends a few m/s of RCS, so the Δv bound is preset-k-17's + 2.
;;;
;;; Every screenshot is the cockpit view (scripts/fly; PNGs not committed,
;;; decision 87), each after a step or a condition wait:
;;; - start: the cab in the start orbit, screens idle.
;;; - plan (engaged): the AUTOPILOT screen's plan and orbit plot.
;;; - departure (in the departure burn): ATTITUDE on the burn attitude.
;;; - arrival (in the arrival burn): the AUTOPILOT screen at the closest approach.
;;; - terminal: K-17 through the window, TARGET's range closing.
;;; - on-station: the screens at the stop, 100 m from K-17.
;;; - approach-20m-cockpit: the grapple camera on the TARGET screen at the 20 m hold.
;;; - latched: the grapple camera, GRPL green, K-17 on the pin.
((:start :new-game)
 (:screenshot "start" :view :cockpit)
 (:autopilot :rendezvous :k-17)
 (:warp :coast)
 (:screenshot "plan" :view :cockpit)
 (:wait :in-burn :departure)
 (:screenshot "departure" :view :cockpit)
 (:wait :in-burn :arrival)
 (:screenshot "arrival" :view :cockpit)
 (:wait :terminal)
 (:screenshot "terminal" :view :cockpit)
 (:wait :autopilot-done :within 259200)
 (:screenshot "on-station" :view :cockpit)
 (:expect :on-station :k-17)
 (:approach :k-17 :screenshots (:cockpit))
 (:wait :latched :k-17)
 (:screenshot "latched" :view :cockpit)
 (:expect :latched :k-17)
 (:expect :dv-below 248.6))
