;;; approach-tern: the last 100 m to Tern Station's berth by the :approach
;;; pilot, from the stop point the autopilot leaves (docs/architecture.md
;;; "Scripts by milestone", "The terminal approach"; decision 122: Flight
;;; scripts in milestone 3).
;;;
;;; Closed form: the grapple's tip 100 m out on the berth's axis at the
;;; station's motion; the pilot closes at (range - 20 m) / 30 s, at most
;;; 0.1 m/s (80 m in ~800 s plus the slowing), holds at 20 m, then 20 m at
;;; 0.1 m/s (200 s): ~1,000-1,100 s, a few m/s of RCS. Berthing needs 0.01-0.30
;;; m/s closing, <= 0.30 m off the pin, <= 10 deg, <= 0.5 deg/s.
;;;
;;; Screenshots (scripts/fly; PNGs not committed, decision 87; decision 131),
;;; none changes the flown states:
;;; - start (cockpit): the station's berth ahead at 100 m through the window.
;;; - start-outside (outside): the station ahead of the ship.
;;; - approach-20m-cockpit (cockpit): the grapple camera on the TARGET screen at the 20 m hold.
;;; - approach-20m-outside (outside): the ship 20 m from the berth.
;;; - berthed (cockpit): the grapple camera, GRPL green, on the berth.
;;; - berthed-outside (outside): the stack at the berth.
((:start :state (:orbit (:on-axis (:tern-station :berth) 100)))
 (:screenshot "start" :view :cockpit)
 (:screenshot "start-outside")
 (:approach (:tern-station :berth) :screenshots (:cockpit :outside))
 (:screenshot "berthed" :view :cockpit)
 (:screenshot "berthed-outside")
 (:expect :berthed (:tern-station :berth))
 (:expect :propellant-above 27900))
