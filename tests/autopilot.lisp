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
          (rest lines))
      (is = 0 index)
      (dolist (line lines) (false (lis::glyph-missing line) "~S" line)))
    (dolist (key '(:right :up :left :left :up :right :right))
      (lis::autopilot-key! model clock sim key))
    (is equal '("> ORBIT INC 91.0°" "  PLANE 89.0°")
        (last (lis::autopilot-menu-lines model sim) 2))
    (lis::sim-set-target! sim :tern-station)
    (is string= "  PLANE TARGET" (car (last (lis::autopilot-menu-lines model sim))))
    (lis::autopilot-key! model clock sim :down)
    (lis::autopilot-key! model clock sim :down)
    (is equal '(:change-plane :target)
        (multiple-value-list (funcall (lis::autopilot-row-goal (lis::autopilot-selected-row model sim))
                                      model)))
    (lis::autopilot-key! model clock sim :down)
    (is eq :circularise (lis::autopilot-model-selection model))
    (dotimes (i 3) (lis::autopilot-key! model clock sim :up))
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
