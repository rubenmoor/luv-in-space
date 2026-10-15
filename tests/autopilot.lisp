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

;;; The planning job, shooting and the orbit manoeuvres (decision 117):
;;; each plan flown by sim-step! meets its goal measure; Δv near the
;;; impulsive figure.
;;;
;;; Time per plan (outside scripts/test; record in docs/architecture.md
;;; "Testing"):
;;; (dolist (case '((:point :change-orbit (800d3)) (:j2 :change-orbit (800d3))
;;;                 (:full :change-orbit (800d3)) (:j2 :change-plane 1.5533430342749532d0)
;;;                 (:full :set-apoapsis 1000d3)))
;;;   (destructuring-bind (model preset goal) case
;;;     (let* ((sim (let ((s (lis::new-game-sim))) (setf (lis::sim-model s) model) s))
;;;            (job (lis::new-autopilot-job sim))
;;;            (start (get-internal-real-time)))
;;;       (dotimes (i 10) (lis::autopilot-plan-now sim preset goal job))
;;;       (format t "~&~A ~A ~,1F ms, ~D steps~%" model preset
;;;               (/ (- (get-internal-real-time) start) (/ internal-time-units-per-second 100))
;;;               (lis::autopilot-job-iteration job)))))

(defun model-sim (model)
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-model sim) model)
    sim))

(defun plan-now (sim preset goal &optional job)
  (multiple-value-bind (plan reason)
      (apply #'lis::autopilot-plan-now sim preset goal (and job (list job)))
    (true plan "~A ~A: ~A" preset goal reason)
    plan))

(defun plan-dv (plan)
  (reduce #'+ (lis::burn-plan-burns plan) :key #'lis::planned-burn-dv))

(defun flown-score (sim plan level measure)
  "MEASURE's largest residual (1: the tolerance) on SIM flown with PLAN by
sim-step! at LEVEL to its last cut, then a period on a shot from there."
  (let* ((copy (lis::shot-sim-assign! (lis::copy-sim sim) sim plan))
         (burns (lis::burn-plan-burns plan))
         (cut (lis::planned-burn-cut-t (svref burns (1- (length burns))))))
    (step-sim copy 1000000 level cut)
    (is = cut (lis::sim-t copy))
    (is = 0d0 (lis::controls-throttle (lis::sim-controls copy)))
    ;; The shot's cut state, bit for bit.
    (let ((last (svref burns (1- (length burns)))))
      (true (and (equalp (lis::planned-burn-cut-r last) (lis::ship-state-r (lis::sim-ship copy)))
                 (equalp (lis::planned-burn-cut-v last) (lis::ship-state-v (lis::sim-ship copy))))
            "×~D: the cut state differs from the shot's" level))
    (let ((pred (lis::prediction-shoot! (lis::new-prediction) copy
                                        (+ cut (lis::osculating-period copy)))))
      (lis::prediction-extend! pred most-positive-fixnum)
      (nth-value 1 (lis::goal-residuals! (make-array 4 :element-type 'double-float)
                                         measure pred cut)))))

(defun check-plan (sim preset goal impulsive &optional job)
  "PRESET with GOAL planned from SIM: its Δv within 3 % of IMPULSIVE, flown
at ×1,000 and ×100 within its goal's tolerances; the plan."
  (let* ((job (or job (lis::new-autopilot-job sim)))
         (plan (plan-now sim preset goal job))
         (measure (lis::autopilot-job-measure job)))
    (when plan
      (true (< (relative-error (plan-dv plan) impulsive) 0.03)
            "~A: ~,2F m/s, impulsive ~,2F" preset (plan-dv plan) impulsive)
      (dolist (level '(1000 100))
        (let ((score (flown-score sim plan level measure)))
          (true (<= score 1d0) "~A at ×~D: ~,3F of the tolerance" preset level score)))
      (loop for burn across (lis::burn-plan-burns plan)
            do (true (on-grid-p (lis::planned-burn-cut-t burn)))))
    plan))

(defun circular-speed (altitude)
  (sqrt (/ lis::+mu-earth+ (+ lis::+radius-earth+ altitude))))

(defun transfer-speeds (r1 r2)
  "(values at r1 at r2) on the ellipse between radii R1 and R2."
  (let ((a (/ (+ r1 r2) 2)) (mu lis::+mu-earth+))
    (values (sqrt (* mu (- (/ 2 r1) (/ a)))) (sqrt (* mu (- (/ 2 r2) (/ a)))))))

(define-test autopilot-plan-apsides
  ;; :point. Circularise at apoapsis after a 50 m/s prograde kick.
  (let* ((sim (model-sim :point))
         (ship (lis::sim-ship sim))
         (v (lis::ship-state-v ship)))
    (lis::vec3-add-scaled! v v (lis::vec3-normalize v) 50d0)
    (let* ((orbit (lis::state->orbit (lis::ship-state-r ship) v lis::+mu-earth+ (lis::sim-t sim)))
           (ra (lis::orbit-apoapsis-radius orbit))
           (va (nth-value 1 (transfer-speeds (lis::orbit-periapsis-radius orbit) ra))))
      (check-plan sim :circularise :apoapsis (- (sqrt (/ lis::+mu-earth+ ra)) va))))
  ;; Set Ap 1,000 km from the start.
  (let ((r1 (+ lis::+radius-earth+ 400d3)) (r2 (+ lis::+radius-earth+ 1000d3)))
    (check-plan (model-sim :point) :set-apoapsis 1000d3
                (- (transfer-speeds r1 r2) (circular-speed 400d3))))
  ;; Set Pe 300 km from 400 × 1,000 km, at apoapsis.
  (let* ((rp (+ lis::+radius-earth+ 400d3))
         (ra (+ lis::+radius-earth+ 1000d3))
         (sim (elements-sim (/ (+ rp ra) 2) (/ (- ra rp) (+ ra rp)) (/ pi 2) (* 3/2 pi) 0d0 0.3d0)))
    (check-plan sim :set-periapsis 300d3
                (- (nth-value 1 (transfer-speeds rp ra))
                   (nth-value 1 (transfer-speeds (+ lis::+radius-earth+ 300d3) ra))))))

(define-test autopilot-plan-hohmann
  ;; 400 → 800 km, :point and once :full; with INC 89.5° under :j2, merged
  ;; into the burn at 800 km.
  (let* ((r1 (+ lis::+radius-earth+ 400d3))
         (r2 (+ lis::+radius-earth+ 800d3))
         (v1 (circular-speed 400d3))
         (v2 (circular-speed 800d3)))
    (multiple-value-bind (p a) (transfer-speeds r1 r2)
      (let ((impulsive (+ (- p v1) (- v2 a))))
        (dolist (model '(:point :full))
          (let ((plan (check-plan (model-sim model) :change-orbit '(800d3) impulsive)))
            (is equal '(:departure :circularise)
                (map 'list #'lis::planned-burn-label (lis::burn-plan-burns plan)))
            (is eq :change-orbit (lis::burn-plan-preset plan)))))
      (let* ((di (lis::deg->rad 0.5d0))
             (impulsive (+ (- p v1) (sqrt (+ (* a a) (* v2 v2) (* -2 a v2 (cos di))))))
             (plan (check-plan (model-sim :j2) :change-orbit (list 800d3 (lis::deg->rad 89.5d0))
                               impulsive)))
        (true (< (abs (- (lis::orbit-i (lis::burn-plan-goal-orbit plan)) (lis::deg->rad 89.5d0)))
                 (lis::deg->rad 0.05d0)))))))

(define-test autopilot-plan-planes
  ;; :j2. To 89° at an equatorial node; to Tern Station's plane turned 1°
  ;; in Ω (a crafted rails orbit) at a relative node: 2 v sin(Δi/2).
  (let ((impulsive (* 2 (circular-speed 400d3) (sin (lis::deg->rad 0.5d0)))))
    (check-plan (model-sim :j2) :change-plane (lis::deg->rad 89d0) impulsive)
    (let* ((sim (model-sim :j2))
           (turn (lis::quat-from-axis-angle (lis::vec3 0 0 1) (lis::deg->rad 1d0))))
      (multiple-value-bind (r v) (object-rv (lis::sim-object sim :tern-station) (lis::sim-t sim))
        (rails-target! sim :tern-station (lis::quat-rotate turn r) (lis::quat-rotate turn v)))
      (let ((plan (check-plan sim :change-plane :target impulsive)))
        (is eq :plane (lis::planned-burn-label (plan-burn plan 0)))))))

(define-test autopilot-plan-job
  ;; The job in 300-unit calls: each within its budget, the plan equalp to
  ;; one planned at once (:j2, a Newton step).
  (let* ((sim (model-sim :j2))
         (goal '(800d3))
         (whole (plan-now sim :change-orbit goal))
         (job (lis::new-autopilot-job sim))
         (calls 0))
    (lis::autopilot-job-start! job sim :change-orbit goal)
    (loop for stage = (lis::autopilot-job-work! job 300)
          do (incf calls)
             (true (<= (lis::autopilot-job-work job) 300))
          until (member stage '(:done :refused)))
    (is eq :done (lis::autopilot-job-stage job))
    (true (>= (lis::autopilot-job-iteration job) 1))
    (true (> calls 20))
    (true (equalp whole (lis::autopilot-job-plan job))))
  ;; The minimum-norm step of x + y = 2: (−1, −1).
  (let ((out (make-array 2 :element-type 'double-float)))
    (lis::minimum-norm-step! out (make-array 2 :element-type 'double-float :initial-element 1d0)
                             (make-array 1 :element-type 'double-float :initial-element 2d0) 1 2)
    (true (< (abs (+ (aref out 0) 1)) 1d-9))
    (true (< (abs (+ (aref out 1) 1)) 1d-9))))

(define-test autopilot-replan-job
  ;; After the :point Hohmann's last cut, flown as shot: a replan finds the
  ;; goal met, no plan. With the velocity there 0.5 % short of the goal's
  ;; periapsis: one :trim burn, the plan's preset and measure, the goal met
  ;; on its shot; in 300-unit calls the same plan.
  (let* ((sim (model-sim :point))
         (plan (plan-now sim :change-orbit '(800d3)))
         (last (plan-burn plan 1))
         (r2 (+ lis::+radius-earth+ 800d3))
         (short (- (circular-speed 800d3) (nth-value 1 (transfer-speeds (* 0.995d0 r2) r2)))))
    (lis::sim-engage-plan! sim plan)
    (step-sim sim 1000000 1000 (lis::planned-burn-cut-t last))
    (lis::plan-step-controls! sim)
    (is eq :done (lis::sim-plan-phase sim))
    (setf (lis::sim-plan-phase sim) :replan
          (lis::sim-plan-index sim) 1
          (lis::sim-plan-replan-t sim) (lis::sim-t sim))
    (multiple-value-bind (replan reason) (lis::autopilot-replan-now sim)
      (false replan)
      (false reason))
    (let ((v (lis::ship-state-v (lis::sim-ship sim))))
      (lis::vec3-add-scaled! v v (lis::vec3-normalize v) (- short)))
    (let* ((job (lis::new-autopilot-job sim))
           (trim (lis::autopilot-replan-now sim job)))
      (true trim)
      (when trim
        (is equal '(:trim) (map 'list #'lis::planned-burn-label (lis::burn-plan-burns trim)))
        (is eq :change-orbit (lis::burn-plan-preset trim))
        (is eq (lis::burn-plan-measure plan) (lis::burn-plan-measure trim))
        (true (<= (nth-value 1 (lis::job-shot-residuals! job (make-array 4 :element-type 'double-float)))
                  1d0))
        (lis::autopilot-replan-job-start! job sim)
        (loop for stage = (lis::autopilot-job-work! job 300)
              do (true (<= (lis::autopilot-job-work job) 300))
              until (member stage '(:done :refused)))
        (true (equalp trim (lis::autopilot-job-plan job)))))))

(define-test autopilot-plan-refusals
  (flet ((refused (sim preset goal)
           (multiple-value-bind (plan reason) (lis::autopilot-plan-now sim preset goal)
             (false plan)
             reason)))
    ;; Past the menu's range, the shot finds the floor.
    (is string= "BELOW 150 KM" (refused (model-sim :point) :set-periapsis 120d3))
    (let ((sim (model-sim :point)))
      (setf (lis::ship-state-m-p (lis::sim-ship sim)) 300d0)
      (let ((reason (refused sim :change-orbit '(2000d3))))
        (true (and (stringp reason) (eql 0 (search "NEEDS " reason))
                   (string= " KG" (subseq reason (- (length reason) 3)))
                   (> (parse-integer reason :start 6 :junk-allowed t) 300))
              "~A" reason)))
    (is string= "NO TARGET" (refused (model-sim :point) :change-plane :target))
    (is string= "GOAL OUT OF RANGE" (refused (model-sim :point) :change-orbit '(400000d3)))))

;;; The preset menu (decision 118)

(define-test autopilot-menu-lines
  ;; The rows from the start orbit (400 km, 90°), the selection marked;
  ;; PLANE TARGET only with a target; each step of each kind.
  (let* ((sim (model-sim :point))
         (clock (lis::new-clock sim))
         (model (lis::clock-autopilot clock)))
    (lis::autopilot-key! model clock sim :open)
    (multiple-value-bind (lines index) (lis::autopilot-menu-lines model sim)
      (true (member (first lines) '("> CIRCULARISE AP" "> CIRCULARISE PE") :test #'string=))
      (is equal '("  SET AP 400 KM" "  SET PE 400 KM" "  ORBIT 400 KM" "  ORBIT INC KEEP"
                  "  PLANE 90.0°")
          (subseq lines 1 6))
      (is = 0 index)
      (dolist (line lines) (false (lis::glyph-missing line) "~S" line)))
    (dolist (key '(:right :down :down :down :down :down :left :left :up :right :right))
      (lis::autopilot-key! model clock sim key))
    (is equal '("> ORBIT INC 91.0°" "  PLANE 89.0°")
        (subseq (lis::autopilot-menu-lines model sim) 4 6))
    (lis::sim-set-target! sim :tern-station)
    (is string= "  PLANE TARGET" (seventh (lis::autopilot-menu-lines model sim)))
    (lis::autopilot-key! model clock sim :down)
    (lis::autopilot-key! model clock sim :down)
    (is equal '(:change-plane :target)
        (multiple-value-list (funcall (lis::autopilot-row-goal (lis::autopilot-selected-row model sim))
                                      model)))
    (dotimes (i 6) (lis::autopilot-key! model clock sim :down))
    (is eq :circularise (lis::autopilot-model-selection model) "past the five RDV rows")
    ;; 12 rows with Tern (a place) targeted: up 8 times wraps to ORBIT INC
    (dotimes (i 8) (lis::autopilot-key! model clock sim :up))
    (dotimes (i 40000) (lis::autopilot-step-value! model sim 1 10))
    (is string= "> ORBIT INC 180.0°" (fifth (lis::autopilot-menu-lines model sim)))
    (lis::autopilot-key! model clock sim :up)
    (dotimes (i 4000) (lis::autopilot-step-value! model sim 1 10))
    (is string= "> ORBIT 300,000 KM" (fourth (lis::autopilot-menu-lines model sim)))
    (dotimes (i 4000) (lis::autopilot-step-value! model sim -1 10))
    (is string= "> ORBIT 150 KM" (fourth (lis::autopilot-menu-lines model sim)))
    (false (lis::glyph-missing "> ORBIT 300,000 KM PLANNING… PLAN STALE: REPLANNING"))
    (false (lis::glyph-missing "PLAN REFUSED: NEEDS 1234 KG"))))

(define-test autopilot-plan-summary
  ;; The :point Hohmann 400 → 800 km against its burns, before engaging and
  ;; once engaged at its second burn.
  (let* ((sim (model-sim :point))
         (plan (plan-now sim :change-orbit '(800d3)))
         (burns (lis::burn-plan-burns plan))
         (one (svref burns 0)) (two (svref burns 1))
         (m-p (lis::ship-state-m-p (lis::sim-ship sim)))
         (m0 (+ lis::*ship-dry-mass* m-p))
         (ve (* lis::*engine-isp* lis::+g0+))
         (needed (* m0 (- 1 (exp (- (/ (+ (lis::planned-burn-dv one) (lis::planned-burn-dv two))
                                         ve))))))
         (summary (lis::autopilot-plan-summary plan sim)))
    (flet ((near (a b) (< (abs (- a b)) (* 1d-9 (max 1d0 (abs b))))))
      (is = 2 (getf summary :burns))
      (is = 0 (getf summary :index))
      (true (near (getf summary :total-dv) (plan-dv plan)))
      (true (near (getf summary :propellant) needed))
      (true (near (getf summary :left) (- m-p needed)))
      (is = (- (lis::planned-burn-start one) (lis::sim-t sim)) (getf summary :next-in))
      (is = (lis::planned-burn-dv one) (getf summary :next-dv))
      (is = (- (lis::planned-burn-cut-t one) (lis::planned-burn-start one))
          (getf summary :next-duration))
      (is = (lis::planned-burn-cut-t two) (getf summary :arrival))
      (lis::sim-engage-plan! sim plan)
      (setf (lis::sim-plan-index sim) 1
            (lis::sim-t sim) (+ (lis::planned-burn-start two) 1))
      (let ((later (lis::autopilot-plan-summary plan sim)))
        (is = 1 (getf later :index))
        (is = 0d0 (getf later :next-in))
        (is = (lis::planned-burn-dv two) (getf later :next-dv))))))

;;; Rendezvous: aim points and the terminal check (docs/design.md "The
;;; terminal path and crowded targets"; decision 119: Rendezvous planning in
;;; milestone 3: Lambert, search, aim and stop points, checks, shooting,
;;; burns).

(defun first-apsis (key kind)
  (lis::orbit-next-apsis (lis::rails-segment-orbit (first (lis::object-def-rails (lis::object-definition key))))
                         (lis::start-t) kind))

(defun rendezvous-aim (sim target t-arr)
  (multiple-value-list (lis::rendezvous-aim sim target (lis::rendezvous-stop sim target) 100d0 t-arr)))

(defun aim-segment-distance (sim target number key t-arr)
  "m from KEY's CoM to TARGET's terminal segment from aim point NUMBER at T-ARR."
  (let ((fixture (lis::rendezvous-stop sim target)))
    (multiple-value-bind (stop) (stop-point sim target fixture t-arr)
      (lis::segment-distance (object-com sim key t-arr)
                             (lis::aim-point sim target fixture 100d0 number t-arr) stop))))

(define-test rendezvous-aim-point-1
  ;; (1): the target's own rails state 1 km / |v_T| earlier.
  (let* ((sim (lis::new-game-sim))
         (t-arr (+ (lis::start-t) 7200d0))
         (fixture (lis::rendezvous-stop sim :tern-station)))
    (multiple-value-bind (rt vt) (object-com sim :tern-station t-arr)
      (multiple-value-bind (r v) (lis::aim-point sim :tern-station fixture 100d0 1 t-arr)
        (multiple-value-bind (r1 v1) (rails-point (lis::object-def-rails (object-def* :tern-station))
                                                  (- t-arr (/ 1000d0 (lis::vec3-norm vt))))
          (true (< (lis::vec3-norm (lis::vec3-sub r r1)) 1d-6))
          (is equalp v1 v))
        ;; 1 km at the Kepler speed; the rails move ~0.1 % slower under J2.
        (true (< (abs (- (lis::vec3-norm (lis::vec3-sub r rt)) 1000d0)) 2d0))))))

(define-test rendezvous-aim-skua-yard-5
  ;; At its first apogee the stop point is 102.6 m behind Skua Yard 5 in
  ;; the line: (1) runs through Skua Yard 1, (2) comes down from 1 km above.
  ;; The yard's 665.6 m gap (decision 133: The places' data: rock radii,
  ;; the yard's attitude, display names and the target state) puts Skua
  ;; Yard 1 563.0 m from it, not the plan's 565.
  (let* ((sim (lis::new-game-sim))
         (apogee (first-apsis :skua-yard-5 :apoapsis))
         (perigee (first-apsis :skua-yard-5 :periapsis))
         (fixture (lis::rendezvous-stop sim :skua-yard-5)))
    (true (< (aim-segment-distance sim :skua-yard-5 1 :skua-yard-1 apogee) 1d0))
    (destructuring-bind (number r &rest more) (rendezvous-aim sim :skua-yard-5 apogee)
      (declare (ignore more))
      (is = 2 number)
      (multiple-value-bind (r2) (lis::aim-point sim :skua-yard-5 fixture 100d0 2 apogee)
        (is equalp r2 r))
      (let ((d1 (aim-segment-distance sim :skua-yard-5 2 :skua-yard-1 apogee))
            (d14 (aim-segment-distance sim :skua-yard-5 2 :pod-14 apogee)))
        (true (>= d1 562d0) "Skua Yard 1 ~,1F m" d1)
        (true (>= d14 765d0) "POD 14 ~,1F m" d14))
      (let ((nearest (loop for object across (lis::sim-objects sim)
                           unless (eq (lis::object-key object) :skua-yard-5)
                             minimize (lis::vec3-norm (lis::vec3-sub (object-com sim (lis::object-key object) apogee)
                                                                     r)))))
        (true (>= nearest 1140d0) "aim point ~,1F m" nearest))
      ;; Above the stop point: away from Earth.
      (multiple-value-bind (stop) (stop-point sim :skua-yard-5 fixture apogee)
        (true (> (lis::vec3-dot (lis::vec3-sub r stop) (lis::vec3-normalize stop)) 999d0))))
    ;; At perigee the stop point is ahead: (1) runs through the target.
    (true (< (aim-segment-distance sim :skua-yard-5 1 :skua-yard-5 perigee) 1d0))
    (is = 2 (first (rendezvous-aim sim :skua-yard-5 perigee)))))

(define-test rendezvous-aim-keeps-1
  ;; Tern Station and K-17 keep (1) along their orbits; Fulmar Depot's fixture
  ;; A turns with the Sun, so (1) passes it clear over part of each orbit
  ;; only, (2) elsewhere.
  (let ((sim (lis::new-game-sim)))
    (dolist (target '(:tern-station :k-17))
      (loop for hours from 0 to 48 by 7
            do (is = 1 (first (rendezvous-aim sim target (+ (lis::start-t) (* hours 3600d0)))))))
    (is = 1 (first (rendezvous-aim sim :fulmar-depot (+ (lis::start-t) (* 12 3600d0)))))
    (is = 2 (first (rendezvous-aim sim :fulmar-depot (lis::start-t))))))

(define-test rendezvous-aim-skua-yard-front
  ;; The front: (1), back on the drop point's rails, ends on Skua Yard 3.
  (let* ((sim (lis::new-game-sim))
         (apogee (first-apsis :skua-yard-3 :apoapsis)))
    (is eq :front (lis::rendezvous-stop sim :skua-yard))
    (is = 2 (first (rendezvous-aim sim :skua-yard apogee)))))

(defun crafted-object (key r v time)
  "A 1 m towable object on the Kepler rails of R, V at TIME."
  (lis::new-object
   (lis::make-object-def key (string key) :towable 1000d0 (lis::mat3 1 0 0 0 1 0 0 0 1) '() '()
                         (lis::make-attitude :inertial) 1d0
                         (lis::make-rails (lis::state->orbit r v lis::+mu-earth+ time) time))))

(define-test rendezvous-aim-refusals
  ;; Four crafted objects, one in the middle of each segment ((3) has two
  ;; sides): "NO CLEAR APPROACH".
  (let* ((sim (lis::new-game-sim))
         (t-arr (+ (lis::start-t) 7200d0))
         (fixture (lis::rendezvous-stop sim :tern-station))
         (stop (stop-point sim :tern-station fixture t-arr))
         (crafted (loop for (number side) in '((1 1) (2 1) (3 1) (3 -1))
                        for i from 0
                        collect (multiple-value-bind (r v)
                                    (lis::aim-point sim :tern-station fixture 100d0 number t-arr side)
                                  (crafted-object (intern (format nil "CRAFTED-~D" i) :keyword)
                                                  (lis::vec3-lerp r stop 0.5d0) v t-arr)))))
    (loop for object in crafted
          for count from 1
          do (setf (lis::sim-objects sim) (concatenate 'simple-vector (lis::sim-objects sim) (list object)))
             (is equal (if (= count 4) '(nil "NO CLEAR APPROACH") (list (min 3 (1+ count))))
                 (subseq (rendezvous-aim sim :tern-station t-arr) 0 (if (= count 4) 2 1))))
    (is equal '(nil "NO FREE FIXTURE")
        (multiple-value-list (lis::rendezvous-stop sim :fulmar-depot :fixture "B")))
    (is equal '(nil "RANGE TOO SHORT")
        (multiple-value-list (lis::rendezvous-stop sim :tern-station :range 50)))
    (setf (lis::object-status (lis::sim-object sim :k-17)) :removed)
    (is equal '(nil "NO TARGET") (multiple-value-list (lis::rendezvous-stop sim :k-17)))))

;;; Rendezvous: the search (docs/design.md "The search"; decision 119,
;;; decision 157: The rendezvous search as built: J2-secular coasts, the
;;; grid's aim points, phasing points, seeds, refinement, pruning, units).
;;; Impulsive, on J2-secular rails, against "The quest line's rendezvous" and "The terminal path and
;;; crowded targets" (± max(1 m/s, 0.5 %) unless stated).
;;;
;;; Wall time per search (outside scripts/test; record in docs/architecture.md
;;; "Testing"):
;;; (dolist (case (list (list (lis::new-game-sim) :tern-station 86400)
;;;                     (list (lis::new-game-sim) :k-17 86400)
;;;                     (list (lis::new-game-sim) :fulmar-depot 129600)
;;;                     (list (fulmar-phasing-sim) :fulmar-depot 172800)
;;;                     (list (highport-phase-sim 0) :highport 172800)
;;;                     (list (skua-plant-sim) :skua-yard-5 172800)
;;;                     (list (apogee-approach-sim) :skua-yard-5 172800)))
;;;   (destructuring-bind (sim target limit) case
;;;     (let ((start (get-internal-run-time)))
;;;       (dotimes (i 5) (lis::rendezvous-search-now sim target :limit limit))
;;;       (format t "~&~A ~,1F ms~%" target
;;;               (/ (- (get-internal-run-time) start) (/ internal-time-units-per-second 200))))))

(defun search-now (sim target &rest keys)
  (multiple-value-bind (candidates reason) (apply #'lis::rendezvous-search-now sim target keys)
    (true candidates "~A: ~A" target reason)
    candidates))

(defun impulse (candidate label)
  (find label (lis::rendezvous-candidate-impulses candidate) :key #'lis::rendezvous-impulse-label))

(defun impulse-dv (candidate label)
  (let ((impulse (impulse candidate label)))
    (and impulse (lis::vec3-norm (lis::rendezvous-impulse-dv impulse)))))

(defun hours-after (sim time) (/ (- time (lis::sim-t sim)) 3600))

(defun check-candidates (sim candidates limit)
  "Every candidate within its limit and clear; the first node after the lead."
  (loop for c across candidates
        for first = (lis::rendezvous-impulse-time (svref (lis::rendezvous-candidate-impulses c) 0))
        do (true (>= (lis::rendezvous-candidate-margin c) 0d0))
           (true (<= (lis::rendezvous-candidate-arrival c) (+ (lis::sim-t sim) limit)))
           (true (>= first (+ (lis::sim-t sim) lis::*plan-min-lead* lis::*search-node-lead*)))))

(defun near-dv (dv reference)
  (<= (abs (- dv reference)) (max 1d0 (* 0.005d0 reference))))

(defun hohmann-dv (r1 r2)
  (multiple-value-bind (p a) (transfer-speeds r1 r2)
    (+ (abs (- p (sqrt (/ lis::+mu-earth+ r1)))) (abs (- (sqrt (/ lis::+mu-earth+ r2)) a)))))

(defun circular-beside (sim key altitude angle)
  "(values r v): circular at ALTITUDE in KEY's plane, ANGLE rad ahead of it."
  (multiple-value-bind (r v) (object-com sim key (lis::sim-t sim))
    (let* ((h (lis::vec3-normalize (lis::vec3-cross r v)))
           (dir (lis::rotate-about (lis::vec3-normalize r) h angle))
           (radius (+ lis::+radius-earth+ altitude)))
      (values (lis::vec3-scale dir radius)
              (lis::vec3-scale (lis::vec3-normalize (lis::vec3-cross h dir))
                               (sqrt (/ lis::+mu-earth+ radius)))))))

(defun sim-at (time)
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-t sim) time)
    sim))

(defun fulmar-phasing-sim ()
  "rendezvous-fulmar-phasing's start (docs/architecture.md \"Scripts by
milestone\"; plan 28 builds it as a stated state): 400 km circular, Fulmar
Depot 297.96° ahead, 296° short of the Hohmann's phase."
  (let ((sim (lis::new-game-sim)))
    (multiple-value-bind (r v) (circular-beside sim :fulmar-depot 400d3 (lis::deg->rad -297.96d0))
      (place-ship! sim r v))))

(defun highport-orbit* ()
  (lis::rails-segment-orbit (first (lis::object-def-rails (lis::object-definition :highport)))))

(defun highport-phase-sim (k)
  "Fulmar Depot's 500 km, 5° ahead of it, at phase K/8 of Highport's period."
  (let ((sim (sim-at (lis::grid-round (+ (lis::start-t) (* k (/ (lis::orbit-period (highport-orbit*)) 8)))))))
    (multiple-value-bind (r v) (circular-beside sim :fulmar-depot 500d3 (lis::deg->rad 5d0))
      (place-ship! sim r v))))

(defun highport-apsis (kind)
  (lis::grid-round (lis::orbit-next-apsis (highport-orbit*) (lis::start-t) kind)))

(defun skua-plant-sim ()
  "rendezvous-skua-yard's state as the autopilot plans: released at
Highport's plant 1 h before perigee, 0.2 m/s along the plant's axis and 300 s
on (by a line, plan 28 builds the stated state)."
  (let* ((sim (sim-at (- (highport-apsis :periapsis) 3300d0)))
         (highport (lis::sim-object sim :highport))
         (fixture (find "plant" (lis::object-def-fixtures (lis::object-def-of highport))
                        :key #'lis::fixture-name :test #'string=))
         (r (lis::make-vec3)) (v (lis::make-vec3)) (q (lis::make-quat)) (w (lis::make-vec3)))
    (lis::object-state-into! r v q w highport (lis::sim-t sim))
    (let ((axis (lis::quat-rotate q (lis::fixture-axis fixture))))
      (place-ship! sim (lis::vec3-add r (lis::vec3-add-scaled (lis::quat-rotate q (lis::fixture-pin fixture))
                                                              axis 68d0))
                   (lis::vec3-add-scaled v axis 0.2d0)))))

(defun apogee-approach-sim ()
  "skua-yard-apogee-approach's start: at Highport's apoapsis, 1 km behind
Skua Yard 5 on its rails, at its velocity."
  (let ((sim (sim-at (highport-apsis :apoapsis))))
    (multiple-value-bind (r v) (object-com sim :skua-yard-5 (lis::sim-t sim))
      (place-ship! sim (object-com sim :skua-yard-5 (- (lis::sim-t sim) (/ 1000d0 (lis::vec3-norm v))))
                   v))
    sim))

(define-test rendezvous-search-tern
  ;; From the start: A, waiting 12.6 h (the coasts at the sim's mean motion,
  ;; decision 159: the 400 km start's mean a lies 9.7 km lower, so the ship
  ;; reaches Tern sooner than the design's ~19 h), 11.41 m/s (11.4 ± 1),
  ;; passing ≥ 239.8 m from Tern's CoM. 24 h, not 48: the same winner at
  ;; half the time (~55 ms).
  (let* ((sim (lis::new-game-sim))
         (candidates (search-now sim :tern-station :limit 86400d0))
         (best (svref candidates 0)))
    (check-candidates sim candidates 86400d0)
    (is eq :a (lis::rendezvous-candidate-family best))
    (true (<= 11 (hours-after sim (lis::rendezvous-impulse-time (impulse best :departure))) 14))
    (true (near-dv (lis::rendezvous-candidate-total best) 11.4d0) "~,3F m/s" (lis::rendezvous-candidate-total best))
    (true (>= (lis::rendezvous-candidate-target-distance best) 239.8d0))
    (is = 1 (lis::rendezvous-candidate-aim best))
    (true (<= (length candidates) lis::*search-candidates*))))

(define-test rendezvous-search-k-17-fulmar
  ;; From 400 km: K-17 219.5 m/s passing ≥ 208.5 m (24 h: it arrives at
  ;; 19.5 h); Fulmar Depot at 36 h, where its fixture A points within 45° of
  ;; nadir and aim point (2) lies 1 km below the stop point (decision 156),
  ;; so the Hohmann to that radius (~55.4), not "The terminal path"'s 56.75
  ;; to (1); passing ≥ 233.7 m.
  (let* ((sim (lis::new-game-sim))
         (k-17 (svref (search-now sim :k-17 :limit 86400d0) 0)))
    (true (near-dv (lis::rendezvous-candidate-total k-17) 219.5d0)
          "~,3F m/s" (lis::rendezvous-candidate-total k-17))
    (true (>= (lis::rendezvous-candidate-target-distance k-17) 208.5d0)))
  (let* ((sim (lis::new-game-sim))
         (candidates (search-now sim :fulmar-depot :limit 129600d0))
         (best (svref candidates 0))
         (r0 (lis::vec3-norm (lis::ship-state-r (lis::sim-ship sim)))))
    (check-candidates sim candidates 129600d0)
    (is = 2 (lis::rendezvous-candidate-aim best))
    (true (near-dv (lis::rendezvous-candidate-total best)
                   (hohmann-dv r0 (lis::vec3-norm (lis::rendezvous-candidate-aim-r best))))
          "~,3F m/s" (lis::rendezvous-candidate-total best))
    (true (<= (lis::rendezvous-candidate-total best) (+ 56.75d0 1)))
    (true (>= (lis::rendezvous-candidate-target-distance best) 233.7d0))))

(define-test rendezvous-search-fulmar-phasing
  ;; 296° short: B, the phasing ≤ 33 m/s, ≤ 96 m/s in all (95.5: at the
  ;; sim's mean motion the 400 km start lags further, decision 159; the
  ;; design's 89.3 took the osculating a's), the aim point within 48 h, the
  ;; final transfer passing ≥ 233.7 m.
  (let* ((sim (fulmar-phasing-sim))
         (candidates (search-now sim :fulmar-depot))
         (best (svref candidates 0)))
    (check-candidates sim candidates lis::*rendezvous-limit*)
    (is eq :b (lis::rendezvous-candidate-family best))
    (true (<= (impulse-dv best :phasing) 33d0) "phasing ~,2F m/s" (impulse-dv best :phasing))
    (true (<= (lis::rendezvous-candidate-total best) 96d0) "~,2F m/s" (lis::rendezvous-candidate-total best))
    (true (>= (lis::rendezvous-candidate-target-distance best) 233.7d0))))

(define-test rendezvous-search-highport
  ;; From Fulmar Depot's 500 km at phases 0 and 4/8 of Highport's period:
  ;; ≤ 2,758 m/s. Two of the plan's eight (~70 ms each); all eight for task
  ;; 162's slow command: (dotimes (k 8) …).
  (dolist (k '(0 4))
    (let* ((sim (highport-phase-sim k))
           (candidates (search-now sim :highport)))
      (check-candidates sim candidates lis::*rendezvous-limit*)
      (true (<= (lis::rendezvous-candidate-total (svref candidates 0)) 2758d0)
            "phase ~D: ~,1F m/s" k (lis::rendezvous-candidate-total (svref candidates 0))))))

(define-test rendezvous-search-skua-yard
  ;; From Highport's plant 1 h before perigee (decision 159: the ship's
  ;; coast at the sim's mean motion against the yard's rails): an A leaving
  ;; at 2.4 h, a 19.6 h transfer, 1.43 m/s, arriving within 26 h; the
  ;; design's B with one revolution (0.22 m/s at perigee) no longer comes
  ;; first nor among the eight. The winner keeps ≥ 500 m from every object
  ;; but the target.
  (let* ((sim (skua-plant-sim))
         (candidates (search-now sim :skua-yard-5))
         (best (svref candidates 0)))
    (check-candidates sim candidates lis::*rendezvous-limit*)
    (true (<= (lis::rendezvous-candidate-total best) 1.5d0) "~,3F m/s" (lis::rendezvous-candidate-total best))
    (true (<= (hours-after sim (lis::rendezvous-candidate-arrival best)) 26))
    (true (>= (lis::rendezvous-candidate-other-distance best) 500d0)
          "~,1F m" (lis::rendezvous-candidate-other-distance best))))

(define-test rendezvous-search-apogee-approach
  ;; 1 km behind Skua Yard 5 at apogee at its velocity: both integrated, the
  ;; ship stays co-orbital (decision 159), so at 48 h it waits (an A of
  ;; 0.04 m/s over 20 h); within 6 h a close-grid transfer (under 0.4 target
  ;; periods) of under 0.1 m/s.
  (let* ((sim (apogee-approach-sim))
         (best (svref (search-now sim :skua-yard-5 :limit 21600d0) 0)))
    (true (< (- (lis::rendezvous-candidate-arrival best)
                (lis::rendezvous-impulse-time (impulse best :departure)))
             (* 0.4d0 (lis::orbit-period (highport-orbit*)))))
    (true (< (lis::rendezvous-candidate-total best) 0.1d0) "~,3F m/s" (lis::rendezvous-candidate-total best))))

(define-test rendezvous-search-slices
  ;; In 300-unit calls, each within its budget: the same candidates as at
  ;; once; through the job's :search stage too (Tern, 12 h).
  (let* ((sim (lis::new-game-sim))
         (whole (search-now sim :tern-station :limit 43200d0))
         (fixture (lis::rendezvous-stop sim :tern-station))
         (search (lis::new-rendezvous-search))
         (calls 0))
    (lis::rendezvous-search-start! search sim :tern-station fixture 100d0 :limit 43200d0)
    (loop until (lis::rendezvous-search-work! search 300)
          do (incf calls)
             (true (<= (lis::rendezvous-search-work search) 300)))
    (true (> calls 50))
    (true (equalp whole (lis::rendezvous-search-result search)))
    (let ((job (lis::new-autopilot-job sim)))
      (lis::autopilot-job-start! job sim :circularise :apoapsis)
      (setf (lis::autopilot-job-search job)
            (lis::rendezvous-search-start! (lis::new-rendezvous-search) sim :tern-station fixture 100d0
                                           :limit 43200d0))
      (loop for stage = (lis::autopilot-job-work! job 300)
            do (true (<= (lis::autopilot-job-work job) 300))
            until (member stage '(:done :refused)))
      (is eq :done (lis::autopilot-job-stage job))
      (true (equalp whole (lis::rendezvous-search-result (lis::autopilot-job-search job)))))))

;;; Rendezvous: the job (docs/design.md "Rendezvous: search, phasing and the
;;; approach"; decision 119, decision 158: The rendezvous job as built: the
;;; lead, the transfer re-guess, step halving, the planning world, the checks).
;;;
;;; Time per plan (outside scripts/test; record in docs/architecture.md
;;; "Testing"):
;;; (dolist (case (list (list :point (lis::new-game-sim) '(:tern-station))
;;;                     (list :j2 (lis::new-game-sim) '(:tern-station))
;;;                     (list :point (lis::new-game-sim) '(:k-17 :limit 86400d0))))
;;;   (destructuring-bind (model sim goal) case
;;;     (setf (lis::sim-model sim) model)
;;;     (let ((start (get-internal-run-time)))
;;;       (dotimes (i 5) (lis::autopilot-plan-now sim :rendezvous goal))
;;;       (format t "~&~A ~S ~,1F ms~%" model goal
;;;               (/ (- (get-internal-run-time) start) (/ internal-time-units-per-second 200))))))

(defun rendezvous-now (sim goal &optional (job (lis::new-autopilot-job sim)))
  "SIM's rendezvous with GOAL planned at once: (values plan job)."
  (multiple-value-bind (plan reason) (lis::autopilot-plan-now sim :rendezvous goal job)
    (true plan "~S: ~A" goal reason)
    (values plan job)))

(defun plan-labels (plan) (map 'list #'lis::planned-burn-label (lis::burn-plan-burns plan)))

(defun job-search-total (job)
  "The search's impulsive total of JOB's chosen candidate."
  (lis::rendezvous-candidate-total (svref (lis::autopilot-job-candidates job) (lis::autopilot-job-index job))))

(defun check-pass (plan key least)
  "PLAN's shot path keeps LEAST m from KEY's CoM: the pass, or never promoted (farther)."
  (let ((pass (cdr (assoc key (lis::rendezvous-part-passes (lis::burn-plan-rendezvous plan))))))
    (true (or (null pass) (>= pass least)) "~A ~,1F m" key pass)))

(defun check-arrival (job)
  "JOB's final shot meets the aim point within 100 m and 0.1 m/s at the :arrival cut."
  (let ((largest (nth-value 1 (lis::rendezvous-residuals! job (make-array 6 :element-type 'double-float)))))
    (true (<= largest 1d0) "~,3F of the tolerance" largest)))

(define-test rendezvous-plan-tern
  ;; From the start state, 18 h (the same plan as 48 h at a third of the
  ;; time): :departure and :arrival to aim point (1), the shot path ≥ 239.8 m
  ;; from Tern's CoM. Tern is promoted 100 km out, hours before the slow
  ;; arrival, and leaves its rails; the transfer is re-timed to it (decision
  ;; 161): :point within 3 % of the search's 11.52 (11.76; 22.36 with the
  ;; search's times), :j2 ≤ 20 m/s (19.35; 28.45 with the search's times;
  ;; the search's 11.41 is a Kepler figure J2's physics does not reach).
  (dolist (model '(:point :j2))
    (let ((sim (model-sim model)))
      (multiple-value-bind (plan job) (rendezvous-now sim '(:tern-station :limit 64800d0))
        (when plan
          (is equal '(:departure :arrival) (plan-labels plan))
          (is = 1 (lis::rendezvous-plan-aim-point plan))
          (true (plusp (lis::autopilot-job-retimes job)))
          (check-arrival job)
          (check-pass plan :tern-station 239.8d0)
          (true (if (eq model :point)
                    (< (relative-error (plan-dv plan) (job-search-total job)) 0.03)
                    (<= (plan-dv plan) 20d0))
                "~A ~,2F m/s, search ~,2F" model (plan-dv plan) (job-search-total job))
          (let ((part (lis::burn-plan-rendezvous plan)))
            (is eq :tern-station (lis::rendezvous-part-target part))
            (is = 100d0 (lis::rendezvous-part-range part))))))))

(define-test rendezvous-plan-k-17
  ;; :point, 18 h: ≥ 208.5 m from K-17's CoM, within 3 % of 219.5 m/s.
  (multiple-value-bind (plan job) (rendezvous-now (model-sim :point) '(:k-17 :limit 64800d0))
    (when plan
      (check-arrival job)
      (check-pass plan :k-17 208.5d0)
      (true (< (relative-error (plan-dv plan) 219.5d0) 0.03) "~,2F m/s" (plan-dv plan)))))

(define-test rendezvous-plan-plane
  ;; The start turned 0.5° in Ω (:point): the plan begins with :plane, its
  ;; total within 3 % of that plane change plus the coplanar plan.
  (let* ((sim (model-sim :point))
         (ship (lis::sim-ship sim))
         (turn (lis::quat-from-axis-angle (lis::vec3 0 0 1) (lis::deg->rad 0.5d0)))
         (coplanar (rendezvous-now (model-sim :point) '(:tern-station :limit 64800d0))))
    (place-ship! sim (lis::quat-rotate turn (lis::ship-state-r ship))
                 (lis::quat-rotate turn (lis::ship-state-v ship)))
    (multiple-value-bind (plan job) (rendezvous-now sim '(:tern-station :limit 64800d0))
      (when (and plan coplanar)
        (is eq :plane (first (plan-labels plan)))
        (check-arrival job)
        (let ((expected (+ (lis::planned-burn-dv (plan-burn plan 0)) (plan-dv coplanar))))
          (true (< (relative-error (plan-dv plan) expected) 0.03)
                "~,2F m/s, expected ~,2F" (plan-dv plan) expected))))))

(defun kepler-object (key r v time)
  "A 1 m towable object on the conic rails (no J2: as :point flies it) of R, V at TIME."
  (lis::new-object
   (lis::make-object-def key (string key) :towable 1000d0 (lis::mat3 1 0 0 0 1 0 0 0 1) '() '()
                         (lis::make-attitude :inertial) 1d0
                         (lis::make-rails (lis::state->orbit r v lis::+mu-earth+ time) time :j2 nil))))

(defun sim-with (sim &rest objects)
  "A copy of SIM with OBJECTS added, rechecked."
  (let ((copy (lis::copy-sim sim)))
    (setf (lis::sim-objects copy) (concatenate 'simple-vector (lis::sim-objects copy) objects))
    (lis::sim-recheck-objects! copy)
    copy))

(define-test rendezvous-plan-blocked
  ;; Tern Station, :point, 18 h. An object the search does not see (*search-ignored*)
  ;; crossing the ship's coast at the first candidate's :departure start
  ;; blocks it: the second is shot and chosen. One 150 m behind the ship on
  ;; its orbit blocks every candidate: "NO CLEAR TRANSFER" (two kept, not
  ;; eight, for the time: each is coasted and shot).
  (let* ((sim (model-sim :point))
         (goal '(:tern-station :limit 64800d0))
         (first (rendezvous-now sim goal))
         (start (lis::planned-burn-start (find :departure (lis::burn-plan-burns first)
                                               :key #'lis::planned-burn-label)))
         (coast (lis::shot-sim-assign! (lis::copy-sim sim) sim nil)))
    (loop while (< (lis::sim-t coast) start) do (lis::sim-step-ticks! coast lis::*prediction-level* start))
    (let* ((r (lis::ship-state-r (lis::sim-ship coast)))
           (v (lis::vec3-add-scaled (lis::ship-state-v (lis::sim-ship coast)) (lis::vec3-normalize r) 300d0))
           (lis::*search-ignored* '(:crossing :behind)))
      (multiple-value-bind (plan job) (rendezvous-now (sim-with sim (kepler-object :crossing r v start)) goal)
        (when plan
          (is = 1 (lis::autopilot-job-index job))
          (check-arrival job)
          (check-pass plan :crossing 201d0)))
      (let* ((ship (lis::sim-ship sim))
             (v0 (lis::ship-state-v ship))
             (behind (lis::vec3-add-scaled (lis::ship-state-r ship) (lis::vec3-normalize v0) -150d0)))
        (is equal '(nil "NO CLEAR TRANSFER")
            (let ((lis::*search-candidates* 2))
              (multiple-value-list
               (lis::autopilot-plan-now (sim-with sim (kepler-object :behind behind v0 (lis::sim-t sim)))
                                        :rendezvous goal))))))))

(define-test rendezvous-plan-refusals
  ;; 300 kg for Highport: "NEEDS <n> KG", n the burns', the reserve and the
  ;; terminal phase's 20 m/s on the RCS from the mass after the burns. No
  ;; target for RDV TARGET: "NO TARGET"; the stop point's refusals.
  (let* ((sim (model-sim :point))
         (job (lis::new-autopilot-job sim)))
    (setf (lis::ship-state-m-p (lis::sim-ship sim)) 300d0)
    (let* ((reason (nth-value 1 (lis::autopilot-plan-now sim :rendezvous '(:highport) job)))
           (burns (lis::autopilot-job-propellant job))
           (end-mass (- (+ lis::*ship-dry-mass* 300d0) burns))
           (terminal (lis::propellant-for end-mass 20d0 lis::*rcs-isp*))
           (reserve (lis::propellant-for end-mass (lis::plan-reserve-dv (job-search-total job))
                                         lis::*engine-isp*)))
      (is equal (format nil "NEEDS ~D KG" (ceiling (+ burns terminal reserve))) reason)
      (true (> terminal 0d0))))
  (let ((sim (model-sim :point)))
    (is equal '(nil "NO TARGET")
        (multiple-value-list (lis::autopilot-plan-now sim :rendezvous '(:target))))
    (is equal '(nil "NO FREE FIXTURE")
        (multiple-value-list (lis::autopilot-plan-now sim :rendezvous '((:fulmar-depot "B")))))
    (is equal '(nil "RANGE TOO SHORT")
        (multiple-value-list (lis::autopilot-plan-now sim :rendezvous '(:tern-station :range 50))))))

(define-test rendezvous-plan-job
  ;; In 300-unit calls, each within its budget: equalp to the plan at once
  ;; (Tern, 18 h).
  (let* ((sim (model-sim :point))
         (goal '(:tern-station :limit 64800d0))
         (whole (rendezvous-now sim goal))
         (job (lis::new-autopilot-job sim))
         (calls 0))
    (lis::autopilot-job-start! job sim :rendezvous goal)
    (loop for stage = (lis::autopilot-job-work! job 300)
          do (incf calls)
             (true (<= (lis::autopilot-job-work job) 300))
          until (member stage '(:done :refused)))
    (true (> calls 100))
    (true (equalp whole (lis::autopilot-job-plan job)))))

(define-test rendezvous-menu-rows
  ;; The five RDV rows after plan 26's, in *places*' order; RDV TARGET only
  ;; with a target not a place; Right ×3 → 84 H, 240 H the top, 12 H the
  ;; bottom; the goal; engaging a rendezvous sets the sim's target.
  (let* ((sim (model-sim :point))
         (clock (lis::new-clock sim))
         (model (lis::clock-autopilot clock)))
    (lis::autopilot-key! model clock sim :open)
    (is equal '("  RDV TERN STATION 48 H" "  RDV FULMAR DEPOT 48 H" "  RDV DERELICT K-17 48 H"
                "  RDV HIGHPORT 48 H" "  RDV SKUA YARD 48 H")
        (subseq (lis::autopilot-menu-lines model sim) 6))
    (lis::sim-set-target! sim :tern-station)
    (is = 12 (length (lis::autopilot-menu-lines model sim)))
    (lis::sim-set-target! sim :skua-yard-5)
    (is string= "  RDV TARGET 48 H" (car (last (lis::autopilot-menu-lines model sim))))
    (dolist (line (lis::autopilot-menu-lines model sim)) (false (lis::glyph-missing line) "~S" line))
    (setf (lis::autopilot-model-selection model) :rdv-k-17)
    (dotimes (i 3) (lis::autopilot-key! model clock sim :right))
    (flet ((k-17-line () (find "RDV DERELICT" (lis::autopilot-menu-lines model sim) :test #'search)))
    (is string= "> RDV DERELICT K-17 84 H" (k-17-line))
    (is equal (list :rendezvous (list :k-17 :limit (* 84 3600d0)))
        (multiple-value-list (funcall (lis::autopilot-row-goal (lis::autopilot-selected-row model sim)) model)))
    (dotimes (i 30) (lis::autopilot-step-value! model sim 1 1))
    (is string= "> RDV DERELICT K-17 240 H" (k-17-line))
    (dotimes (i 30) (lis::autopilot-step-value! model sim -1 1))
    (is string= "> RDV DERELICT K-17 12 H" (k-17-line)))
    (setf (lis::autopilot-model-selection model) :rdv-target)
    (is equal (list :target :limit (* 48 3600d0))
        (nth-value 1 (funcall (lis::autopilot-row-goal (lis::autopilot-selected-row model sim)) model)))
    (lis::sim-set-target! sim nil)
    (let* ((burn (lis::make-planned-burn :departure (+ (lis::sim-t sim) 600) (+ (lis::sim-t sim) 700)
                                         (lis::quat 1 0 0 0) 1d0 nil 1d0))
           (plan (lis::make-burn-plan (vector burn)
                                      :rendezvous (lis::make-rendezvous-part :k-17 nil 100d0 1 1 (lis::make-vec3)
                                                                             0d0 '()))))
      (lis::autopilot-engage! clock sim plan)
      (is eq :k-17 (lis::sim-target sim)))))
