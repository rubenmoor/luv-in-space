(in-package #:luv-in-space.tests)

;;; autopilot.lisp: burns and first guesses (docs/architecture.md "Testing"
;;; → "Autopilot"; decision 117: Autopilot in milestone 3: planning jobs,
;;; goal measures, burns, execution rules in the step, trims).

(defun elements-sim (a e i raan argp mean-anomaly &key (model :point))
  "The start's sim, MODEL, on the orbit of these elements (m, rad) at its t."
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim))
         (t0 (lis::sim-t sim)))
    (setf (lis::sim-model sim) model)
    (multiple-value-bind (r v)
        (lis::orbit-state (lis::make-elliptic-orbit lis::+mu-earth+ a e i raan argp mean-anomaly t0)
                          t0)
      (replace (lis::ship-state-r ship) r)
      (replace (lis::ship-state-v ship) v))
    sim))

(defun first-guess (sim preset goal)
  (multiple-value-bind (plan reason) (lis::autopilot-first-guess sim preset goal)
    (true plan "~A: ~A" preset reason)
    plan))

(defun plan-burn (plan i) (svref (lis::burn-plan-burns plan) i))

(define-test autopilot-finite-burn
  (let ((ve (* lis::*engine-isp* lis::+g0+)))
    ;; docs/design.md "Mass budget and Δv": 56 m/s wet 20 s, 100 m/s with 50 t 83 s.
    (loop for (dv mass seconds) in '((56d0 35961d0 20d0) (100d0 85961d0 83d0))
          do (multiple-value-bind (start duration throttle rcs isp propellant)
                 (lis::finite-burn 1000d0 (lis::vec3 0 dv 0) mass)
               (true (<= (abs (- duration seconds)) 0.5d0) "~F s for ~F m/s" duration dv)
               (is = 1d0 throttle)
               (false rcs)
               (is = lis::*engine-isp* isp)
               (true (< (relative-error propellant (* mass (- 1 (exp (- (/ dv ve)))))) 1d-12))
               (true (on-grid-p start))
               (true (<= (abs (- (+ start (/ duration 2)) 1000d0)) 1/64))))
    ;; 2 m/s with 100 t: the throttle that makes 10 s; wet: the 10 % floor.
    (multiple-value-bind (start duration throttle) (lis::finite-burn 1000d0 (lis::vec3 2 0 0) 135961d0)
      (true (< (abs (- duration 10d0)) 1d-9) "~F s" duration)
      (true (< 0.2d0 throttle 0.3d0) "throttle ~F" throttle)
      (true (<= (abs (- (+ start 5d0) 1000d0)) 1/64)))
    (multiple-value-bind (start duration throttle) (lis::finite-burn 1000d0 (lis::vec3 2 0 0) 35961d0)
      (declare (ignore start))
      (is = lis::*throttle-min* throttle)
      (true (< 6d0 duration 8d0) "~F s" duration))
    ;; 0.3 m/s retrograde: the RCS, nose prograde, so no slew from the coast hold.
    (let* ((sim (lis::new-game-sim))
           (ship (lis::sim-ship sim))
           (r (lis::ship-state-r ship))
           (v (lis::ship-state-v ship))
           (dv (lis::vec3-scale (lis::vec3-normalize v) -0.3d0)))
      (multiple-value-bind (start duration throttle rcs isp) (lis::finite-burn 1000d0 dv 35961d0 :prograde v)
        (is = -1 rcs)
        (is = 0d0 throttle)
        (is = lis::*rcs-isp* isp)
        (true (< (relative-error duration
                                 (/ (* 35961d0 (- 1 (exp (- (/ 0.3d0 (* 360 lis::+g0+))))))
                                    (/ 1780d0 (* 360 lis::+g0+))))
                 1d-12))
        (true (<= (abs (- (+ start (/ duration 2)) 1000d0)) 1/64))
        (let ((q (lis::burn-attitude dv r v rcs)))
          (true (< (lis::slew-estimate (lis::hold-attitude :prograde r v 0d0) q
                                       (lis::mass-properties-inertia
                                        (lis::ship-mass-properties 28000d0)))
                   1d-3)))))))

(define-test autopilot-burn-attitude
  (let ((r (lis::vec3 7d6 0 0))
        (v (lis::vec3 0 7.5d3 0)))
    (flet ((axes (dv)
             (let ((q (lis::burn-attitude dv r v)))
               (values (lis::quat-rotate q (lis::vec3 1 0 0))
                       (lis::quat-rotate q (lis::vec3 0 0 -1))))))
      ;; The nose along Δv, the cab radial out.
      (let ((dv (lis::vec3 10 40 15)))
        (multiple-value-bind (nose cab) (axes dv)
          (true (< (lis::vec3-norm (lis::vec3-sub nose (lis::vec3-normalize dv))) 1d-12))
          (let* ((out (lis::vec3 1 0 0))
                 (expected (lis::vec3-normalize
                            (lis::vec3-add-scaled out nose (- (lis::vec3-dot out nose))))))
            (true (< (lis::vec3-norm (lis::vec3-sub cab expected)) 1d-12)))))
      ;; Within 10° of radial: the cab towards the normal.
      (multiple-value-bind (nose cab) (axes (lis::vec3 40 2 0))
        (true (< (abs (lis::vec3-dot nose cab)) 1d-12))
        (true (< (lis::vec3-norm (lis::vec3-sub cab (lis::vec3 0 0 1))) 1d-12))))))

(define-test autopilot-slew-and-lead
  ;; decision 31: 180° wet ~40 s, with 100 t ~59 s.
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim))
         (r (lis::ship-state-r ship))
         (v (lis::ship-state-v ship))
         (from (lis::hold-attitude :prograde r v 0d0))
         (to (lis::hold-attitude :retrograde r v 0d0)))
    (loop for (load seconds) in (list (list nil 40d0)
                                      (list (lis::stack-design-pose
                                             (lis::object-definition :skua-yard-5))
                                            59d0))
          for slew = (lis::slew-estimate
                      from to (lis::mass-properties-inertia (lis::ship-mass-properties 28000d0 load)))
          do (true (<= (abs (- slew seconds)) 2d0) "~F s" slew)
             (is = (max 60d0 (+ slew 10d0)) (lis::burn-lead slew)))
    (is = 60d0 (lis::burn-lead 0d0))))

(define-test autopilot-hohmann-guess
  ;; 400 → 800 km from the start, :point: the vis-viva closed form.
  (let* ((sim (let ((sim (lis::new-game-sim))) (setf (lis::sim-model sim) :point) sim))
         (t0 (lis::sim-t sim))
         (mu lis::+mu-earth+)
         (r1 (lis::vec3-norm (lis::ship-state-r (lis::sim-ship sim))))
         (r2 (+ lis::+radius-earth+ 800d3))
         (plan (first-guess sim :change-orbit '(800d3)))
         (one (plan-burn plan 0))
         (two (plan-burn plan 1)))
    (is equal '(:departure :circularise) (map 'list #'lis::planned-burn-label
                                              (lis::burn-plan-burns plan)))
    (is eq :change-orbit (lis::burn-plan-preset plan))
    (let ((dv1 (* (sqrt (/ mu r1)) (- (sqrt (/ (* 2 r2) (+ r1 r2))) 1)))
          (dv2 (* (sqrt (/ mu r2)) (- 1 (sqrt (/ (* 2 r1) (+ r1 r2)))))))
      (true (< (relative-error (lis::planned-burn-dv one) dv1) 1d-9))
      (true (< (relative-error (lis::planned-burn-dv two) dv2) 1d-9))
      (true (< (relative-error (+ (lis::planned-burn-dv one) (lis::planned-burn-dv two))
                               (+ dv1 dv2))
               1d-9)))
    (true (>= (lis::planned-burn-hold-at one) (+ t0 lis::*plan-min-lead*)))
    (loop for burn across (lis::burn-plan-burns plan)
          do (true (on-grid-p (lis::planned-burn-hold-at burn)))
             (true (on-grid-p (lis::planned-burn-start burn)))
             (is = 1d0 (lis::planned-burn-throttle burn)))
    ;; The second node half a transfer period after the first.
    (true (<= (abs (- (- (lis::planned-burn-cut-t two) (lis::planned-burn-cut-t one))
                      (* pi (sqrt (/ (expt (/ (+ r1 r2) 2) 3) mu)))))
              1/64))
    (let ((goal (lis::burn-plan-goal-orbit plan)))
      (true (< (relative-error (lis::orbit-a goal) r2) 1d-9))
      (true (< (lis::orbit-e goal) 1d-9)))
    ;; Propellant by the rocket equation, burn after burn; the reserve's floor.
    (let* ((ve (* lis::*engine-isp* lis::+g0+))
           (m0 (+ lis::*ship-dry-mass* 28000d0))
           (end (* m0 (exp (- (/ (+ (lis::planned-burn-dv one) (lis::planned-burn-dv two)) ve))))))
      (is = 20d0 (lis::burn-plan-reserve-dv plan))
      (true (< (relative-error (lis::burn-plan-reserve-mass plan)
                               (* end (- 1 (exp (- (/ 20d0 ve))))))
               1d-9)))
    (is = 20d0 (lis::plan-reserve-dv 999d0))
    (is = 40d0 (lis::plan-reserve-dv 2000d0))))

(define-test autopilot-apsis-guesses
  ;; 400 × 1,000 km, :point: set Pe 300 km and circularise, both at apoapsis.
  (let* ((rp (+ lis::+radius-earth+ 400d3))
         (ra (+ lis::+radius-earth+ 1000d3))
         (sim (elements-sim (/ (+ rp ra) 2) (/ (- ra rp) (+ ra rp)) (/ pi 2) (* 3/2 pi) 0d0 0.3d0)))
    (let* ((plan (first-guess sim :set-periapsis 300d3))
           (burn (plan-burn plan 0))
           (goal (lis::burn-plan-goal-orbit plan)))
      (is eq :apsis (lis::planned-burn-label burn))
      (true (< (abs (- (lis::vec3-norm (lis::planned-burn-cut-r burn)) ra)) 1d0))
      (true (< (abs (- (lis::orbit-periapsis-radius goal) (+ lis::+radius-earth+ 300d3))) 1d0))
      (true (< (abs (- (lis::orbit-apoapsis-radius goal) ra)) 1d0)))
    (let* ((plan (first-guess sim :circularise :apoapsis))
           (goal (lis::burn-plan-goal-orbit plan)))
      (is eq :circularise (lis::planned-burn-label (plan-burn plan 0)))
      (true (< (lis::orbit-e goal) 1d-9))
      (true (< (abs (- (lis::orbit-a goal) ra)) 1d0)))))

(define-test autopilot-plane-guess
  ;; e = 0.1, ω 30°: 89° from 90° at the equatorial node at the larger radius,
  ;; 2 v sin(Δi/2) with v the horizontal speed there, Ω kept.
  (let* ((mu lis::+mu-earth+)
         (a 7.5d6) (e 0.1d0) (argp (lis::deg->rad 30d0)) (raan (* 3/2 pi))
         (sim (elements-sim a e (/ pi 2) raan argp 0d0))
         (plan (first-guess sim :change-plane (lis::deg->rad 89d0)))
         (burn (plan-burn plan 0))
         (p (* a (- 1 (* e e))))
         (far (/ p (+ 1 (* e (cos (- pi argp))))))
         (r (lis::planned-burn-cut-r burn))
         (after (lis::state->orbit r (lis::planned-burn-cut-v burn) mu 0d0)))
    (is eq :plane (lis::planned-burn-label burn))
    (true (< (abs (- (lis::vec3-norm r) far)) 100d0) "~,1F km, not ~,1F km"
          (/ (lis::vec3-norm r) 1000) (/ far 1000))
    (true (< (relative-error (lis::planned-burn-dv burn)
                             (* 2 (/ (sqrt (* mu p)) (lis::vec3-norm r)) (sin (lis::deg->rad 0.5d0))))
             1d-6))
    (true (< (abs (- (lis::orbit-i after) (lis::deg->rad 89d0))) 1d-6))
    (true (< (abs (- (lis::orbit-raan after) raan)) 1d-6))
    ;; Without a target the target's plane is refused.
    (multiple-value-bind (plan reason) (lis::autopilot-first-guess sim :change-plane :target)
      (false plan)
      (is string= "NO TARGET" reason))))
