(in-package #:luv-in-space)

;;; The autopilot (docs/design.md "The autopilot"; decision 117: Autopilot
;;; in milestone 3: planning jobs, goal measures, burns, execution rules in
;;; the step, trims): finite burns built on impulsive nodes, and the first
;;; guesses of the orbit manoeuvres from a snapshot of the sim.

(defparameter *burn-min-duration* 10d0
  "s: a burn shorter at full throttle runs at the throttle that makes it this; decision 117.")
(defparameter *rcs-burn-max-dv* 0.5d0 "m/s: below it a burn runs on the RCS along ±x; decision 117.")
(defparameter *radial-burn-angle* (deg->rad 10d0)
  "rad: Δv this close to radial turns the cab towards the normal; decision 117.")
(defparameter *burn-min-lead* 60d0 "s: hold-at at least this before the start; decision 31.")
(defparameter *slew-margin* 10d0 "s: added to the slew estimate for the lead; decision 31.")
(defparameter *plan-min-lead* 120d0 "s: a plan's first hold-at after its snapshot; decision 117.")
(defparameter *reserve-fraction* 0.02d0 "Of the plan's Δv, the reserve; decision 32.")
(defparameter *reserve-min-dv* 20d0 "m/s, the reserve's floor; decision 32.")
(defparameter *circular-guess-e* 1d-3
  "Below this e an orbit has no apsis to burn at: the burn goes at the earliest node.")
(defparameter *node-tie* 1d-3 "Relative: two nodes' radii this close are a tie, the earlier wins.")
(declaim (type double-float *burn-min-duration* *rcs-burn-max-dv* *radial-burn-angle*
               *burn-min-lead* *slew-margin* *plan-min-lead* *reserve-fraction*
               *reserve-min-dv* *circular-guess-e* *node-tie*))

(defun grid-round (time)
  "TIME on the nearest 1/64 s grid point."
  (* +fine-step+ (fround time +fine-step+)))

;;; Burns

(defun finite-burn (node-t dv-vec mass &key (isp *engine-isp*) (thrust *engine-thrust*) prograde)
  "The finite burn of DV-VEC centred on NODE-T from MASS (kg):
(values start duration throttle rcs isp propellant). t = m₀ (1 − e^(−Δv /
v_e)) / ṁ at full throttle; under *burn-min-duration* the throttle that
makes it, at least *throttle-min*. Under *rcs-burn-max-dv* the RCS along ±x
(throttle 0), RCS the sign that keeps the nose within 90° of PROGRADE (a
vector; +1 without). START = NODE-T − duration / 2 on the grid."
  (let* ((dv (vec3-norm dv-vec))
         (rcs (when (< dv *rcs-burn-max-dv*)
                (if (and prograde (< (vec3-dot dv-vec prograde) 0d0)) -1 1)))
         (isp (if rcs *rcs-isp* (float isp 1d0)))
         (thrust (if rcs (rcs-translation-capability 0 (make-vec3)) (float thrust 1d0)))
         (ve (* isp +g0+))
         (propellant (* mass (- 1 (exp (- (/ dv ve))))))
         (full (/ propellant (/ thrust ve)))
         (throttle (cond (rcs 0d0)
                         ((>= full *burn-min-duration*) 1d0)
                         (t (max *throttle-min* (/ full *burn-min-duration*)))))
         (duration (if rcs full (/ full throttle))))
    (values (grid-round (- node-t (/ duration 2))) duration throttle rcs isp propellant)))

(defun burn-attitude (dv-vec r v &optional rcs)
  "The burn's attitude (body -> inertial): the nose along DV-VEC (against it
for RCS −1; along V for a zero Δv), the cab (−z) radial out from R, towards
the normal R × V when the nose lies within *radial-burn-angle* of radial."
  (let* ((x (vec3-normalize (if (> (vec3-norm dv-vec) 0d0) dv-vec v)))
         (out (vec3-normalize r))
         (cab (if (> (abs (vec3-dot x out)) (cos *radial-burn-angle*))
                  (vec3-normalize (vec3-cross r v))
                  out))
         (z (vec3-normalize (vec3-scale (vec3-add-scaled cab x (- (vec3-dot cab x))) -1d0))))
    (when (eql rcs -1)
      (vec3-scale! x x -1d0))
    (quat-from-basis x (vec3-cross z x) z)))

(defun slew-estimate (from to inertia)
  "s: the bang-bang slew from the attitude FROM to TO (quats, body ->
inertial) at *hold-torque-fraction* of the couple about the rotation axis
and at most *damping-rate*, INERTIA the stack's about its CoM (docs/design.md
\"Control modes\", decision 70)."
  (let* ((e (quat-mul (quat-conjugate from) to))
         (axis (vec3 (aref e 1) (aref e 2) (aref e 3)))
         (s (vec3-norm axis))
         (angle (* 2 (atan s (abs (aref e 0))))))
    (if (= s 0d0)
        0d0
        (let ((i-axis (mat3*vec3 inertia (vec3-scale axis (/ s))))
              (alpha most-positive-double-float)
              (rate *damping-rate*))
          (dotimes (i 3)
            (let ((need (abs (aref i-axis i))))
              (when (> need 0d0)
                (setf alpha (min alpha (/ (* *hold-torque-fraction* (rcs-couple i)) need))))))
          (if (<= angle (/ (* rate rate) alpha))
              (* 2 (sqrt (/ angle alpha)))
              (+ (/ angle rate) (/ rate alpha)))))))

(defun burn-lead (slew)
  "s from hold-at to the start: max(*burn-min-lead*, SLEW + *slew-margin*); decision 31."
  (max *burn-min-lead* (+ slew *slew-margin*)))

(defun propellant-for (mass dv isp)
  "kg of propellant for DV (m/s) from MASS at ISP (s), the rocket equation."
  (* mass (- 1 (exp (- (/ dv (* isp +g0+)))))))

(defun plan-reserve-dv (total-dv)
  "m/s: max(*reserve-fraction* of TOTAL-DV, *reserve-min-dv*); decision 32."
  (max (* *reserve-fraction* total-dv) *reserve-min-dv*))

(defun sim-stack-load (sim)
  "SIM's latched load as a fresh stack-load, or nil (none, or berthed)."
  (let* ((grapple (sim-grapple sim))
         (index (grapple-object grapple)))
    (when (and index (not (sim-berthed sim)))
      (make-stack-load (object-def-of (svref (sim-objects sim) index))
                       (vec3-copy (grapple-com grapple)) (quat-copy (grapple-q grapple))))))

(defun coast-attitude (orbit time)
  "The prograde hold's attitude on ORBIT at TIME."
  (multiple-value-bind (r v) (orbit-state orbit time)
    (hold-attitude :prograde r v 0d0)))

(defun build-burn (label node-t r v dv-vec m-p load previous orbit)
  "The planned-burn of DV-VEC at NODE-T, the ship there at R, V with M-P kg
of propellant and the stack-load LOAD: centred, its attitude, hold-at
start − (burn-lead slew) on the grid, the slew from PREVIOUS (the last
burn's attitude) or, without one, from the prograde hold on ORBIT at
hold-at; the cut state the node's after an impulsive Δv. (values burn
propellant)."
  (let ((mass (+ *ship-dry-mass* m-p (stack-load-mass load))))
    (multiple-value-bind (start duration throttle rcs isp propellant)
        (finite-burn node-t dv-vec mass :prograde v)
      (declare (ignore duration))
      (let ((q (burn-attitude dv-vec r v rcs))
            (inertia (mass-properties-inertia (ship-mass-properties m-p load)))
            (hold-at (- start *burn-min-lead*)))
        ;; The coast hold's attitude is taken at hold-at: twice settles it.
        (dotimes (pass (if previous 1 2))
          (let ((slew (slew-estimate (or previous (coast-attitude orbit hold-at)) q inertia)))
            (setf hold-at (- start (grid-ceiling (burn-lead slew))))))
        (values (make-planned-burn label hold-at start q throttle rcs (vec3-norm dv-vec)
                                   :cut-t node-t :cut-r (vec3-copy r)
                                   :cut-v (vec3-add v dv-vec) :isp isp)
                propellant)))))

;;; The coast shot (decision 117): the snapshot coasting two osculating
;;; periods, for the apsis and node times.

(defun shot-sim-assign! (out snapshot plan)
  "OUT: SNAPSHOT as the autopilot engages PLAN (sim-engage-plan!, decision
117); without a plan the controls as they were, the throttle off. OUT."
  (sim-assign! out snapshot)
  (if plan
      (sim-engage-plan! out plan)
      (progn (controls-throttle-cut (sim-controls out))
             (setf (sim-cutoff out) nil)
             (sim-set-plan! out nil)))
  out)

(defun coast-shot-start! (pred scratch sim)
  "PRED restarted as SIM's coast shot through the sim SCRATCH: no plan, no
throttle, two osculating periods (one unbound: to its capacity)."
  (shot-sim-assign! scratch sim nil)
  (let ((period (osculating-period scratch)))
    (prediction-shoot! pred scratch (if period
                                        (+ (sim-t scratch) (* 2 period))
                                        most-positive-double-float))))

(defun autopilot-coast-shot (sim)
  "SIM's coasting path: a copy without its plan, throttle or cut-off, run
two osculating periods by the prediction."
  (prediction-extend! (coast-shot-start! (new-prediction) (copy-sim sim) sim)
                      most-positive-fixnum))

(defun shot-event-times (shot kind)
  "The times of SHOT's events of KIND, in order."
  (loop for i below (prediction-event-count shot)
        when (eq (svref (prediction-event-kinds shot) i) kind)
          collect (aref (prediction-event-times shot) i)))

(defun shot-equator-times (shot)
  "The times SHOT's path crosses the equatorial plane (z = 0), in order,
each bisected on the Hermite to *apsis-time-tolerance*."
  (let ((nodes (prediction-nodes shot))
        (c (make-array 12 :element-type 'double-float))
        (p (make-vec3)) (dp (make-vec3))
        (times '()))
    (loop for i from (prediction-head shot) below (1- (prediction-count shot))
          for z0 = (aref nodes (+ (* +node-size+ i) 3))
          for z1 = (aref nodes (+ (* +node-size+ (1+ i)) 3))
          when (or (and (< z0 0d0) (>= z1 0d0)) (and (> z0 0d0) (<= z1 0d0)))
            do (let* ((h (interval-hermite! c shot i))
                      (rising (< z0 0d0))
                      (lo 0d0) (hi 1d0)
                      (tolerance (/ *apsis-time-tolerance* (* 4 h))))
                 (loop while (> (- hi lo) tolerance)
                       do (let ((s (* 0.5d0 (+ lo hi))))
                            (hermite-point! p dp c s)
                            (if (eq (< (aref p 2) 0d0) rising) (setf lo s) (setf hi s))))
                 (push (+ (node-t nodes i) (* 0.5d0 (+ lo hi) h)) times)))
    (nreverse times)))

(defun shot-target-normal (shot time)
  "The unit angular momentum of SHOT's target at TIME (the Hermite of its
nodes, the nearest end outside them), or nil without its state."
  (let ((count (prediction-target-count shot))
        (head (prediction-head shot)))
    (when (> count (1+ head))
      (let* ((nodes (prediction-nodes shot))
             (i (max head (min (- count 2)
                               (or (node-interval shot time)
                                   (if (< time (node-t nodes head)) head (- count 2))))))
             (c (make-array 12 :element-type 'double-float))
             (p (make-vec3)) (dp (make-vec3))
             (h (interval-hermite! c shot i t)))
        (hermite-point! p dp c (max 0d0 (min 1d0 (/ (- time (node-t nodes i)) h))))
        (vec3-normalize (vec3-cross p dp))))))

;;; Planes

(defun rotate-about (v axis angle)
  "V turned by ANGLE about the unit AXIS (Rodrigues)."
  (let ((c (cos angle)) (s (sin angle)))
    (vec3-add (vec3-add (vec3-scale v c) (vec3-scale (vec3-cross axis v) s))
              (vec3-scale axis (* (vec3-dot axis v) (- 1 c))))))

(defun plane-angle-to-normal (r v h-goal)
  "rad about r̂ that turns the plane of R, V towards the unit normal H-GOAL."
  (let ((h (vec3-normalize (vec3-cross r v)))
        (out (vec3-normalize r)))
    (atan (vec3-dot (vec3-cross h h-goal) out) (vec3-dot h h-goal))))

(defun plane-angle-to-inclination (r v inclination)
  "rad about r̂, the least turn, that gives the plane of R, V the INCLINATION;
the nearest reachable one where r̂'s latitude forbids it."
  (let* ((h (vec3-normalize (vec3-cross r v)))
         (out (vec3-normalize r))
         (a (aref h 2))
         (b (aref (vec3-cross out h) 2))
         (radius (sqrt (+ (* a a) (* b b)))))
    (if (< radius 1d-12)
        0d0
        (let* ((delta (atan b a))
               (spread (acos (max -1d0 (min 1d0 (/ (cos inclination) radius)))))
               (one (wrap-signed (+ delta spread)))
               (two (wrap-signed (- delta spread))))
          (if (<= (abs one) (abs two)) one two)))))

(defun plane-goal-angle (shot r v goal time)
  "rad about r̂ for GOAL, an inclination (rad) or :target (the target's
plane at TIME from SHOT)."
  (if (eq goal :target)
      (let ((h-goal (shot-target-normal shot time)))
        (if h-goal (plane-angle-to-normal r v h-goal) 0d0))
      (plane-angle-to-inclination r v (float goal 1d0))))

(defun assemble-burn-plan (burns preset goal m-p-end load)
  "The burn-plan of BURNS (a simple-vector): PRESET, GOAL, the goal orbit
from the last burn's cut state, the reserve (decision 32) in m/s and in kg
at the end mass, M-P-END kg of propellant left with LOAD."
  (let* ((last (svref burns (1- (length burns))))
         (total (reduce #'+ burns :key #'planned-burn-dv))
         (reserve (plan-reserve-dv total))
         (end-mass (+ *ship-dry-mass* m-p-end (stack-load-mass load))))
    (make-burn-plan burns
                    :preset preset :goal goal
                    :goal-orbit (state->orbit (planned-burn-cut-r last) (planned-burn-cut-v last)
                                              +mu-earth+ (planned-burn-cut-t last))
                    :reserve-dv reserve
                    :reserve-mass (propellant-for end-mass reserve *engine-isp*))))

;;; First guesses (decision 117): from the snapshot's osculating elements,
;;; the apsis and node times from its coast shot.

(defun tangential-velocity (r v speed)
  "SPEED along the in-plane horizontal of R, V (ĥ × r̂)."
  (let ((h (vec3-cross r v)))
    (vec3-scale (vec3-normalize (vec3-cross h r)) speed)))

(defun autopilot-first-guess (sim preset goal &optional shot)
  "The first guess of PRESET with GOAL from the snapshot SIM: (values plan
nil), or (values nil reason). PRESET and GOAL:
:circularise, :apoapsis or :periapsis (the apsis it burns at);
:set-apoapsis, :set-periapsis, the altitude (m), burning at the opposite apsis;
:change-orbit, (altitude inclination): a Hohmann to the circular altitude,
  the inclination (rad, :target or nil) merged into the burn at the larger radius;
:change-plane, an inclination (rad, Ω kept) at an equatorial node or :target,
  the target's plane at a relative node, the cheaper (larger radius) of the next two.
SHOT: SIM's coast shot (autopilot-coast-shot) if already run."
  (let* ((ship (sim-ship sim))
         (t0 (sim-t sim))
         (mu +mu-earth+)
         (orbit (state->orbit (ship-state-r ship) (ship-state-v ship) mu t0)))
    (unless (and (eq (sim-body sim) :earth) (elliptic-p orbit))
      (return-from autopilot-first-guess (values nil "NOT IN EARTH ORBIT")))
    (when (and (eq preset :change-plane) (eq goal :target) (null (sim-target sim)))
      (return-from autopilot-first-guess (values nil "NO TARGET")))
    (let* ((shot (or shot (autopilot-coast-shot sim)))
           (load (sim-stack-load sim))
           (m-p (ship-state-m-p ship))
           (period (orbit-period orbit))
           (circular (< (orbit-e orbit) *circular-guess-e*))
           (earliest (+ t0 *plan-min-lead*))
           (burns '())
           (previous nil))
      (labels ((burn-at (label node-t r v dv)
                 ;; A burn after those collected, not kept.
                 (build-burn label node-t r v dv m-p load previous orbit))
               (keep! (burn propellant)
                 (push burn burns)
                 (setf previous (planned-burn-attitude burn))
                 (decf m-p propellant))
               (candidates (times)
                 ;; TIMES, then the last one a period on, twice more.
                 (let ((last (or (car (last times)) t0)))
                   (append times (list (+ last period) (+ last (* 2 period))))))
               (first-node (times dv-fn label)
                 ;; The first of TIMES whose burn holds at or after
                 ;; EARLIEST, or with a circular orbit the earliest node:
                 ;; (values node burn propellant r v dv).
                 (if circular
                     (let ((node (+ earliest *burn-min-lead*)))
                       (loop
                         (multiple-value-bind (r v) (orbit-state orbit node)
                           (let ((dv (funcall dv-fn r v node)))
                             (multiple-value-bind (burn propellant) (burn-at label node r v dv)
                               (let ((short (- earliest (planned-burn-hold-at burn))))
                                 (when (<= short 0d0)
                                   (return (values node burn propellant r v dv)))
                                 (incf node (max short +fine-step+))))))))
                     (dolist (node (candidates times)
                                   (return-from autopilot-first-guess
                                     (values nil "NO NODE AHEAD")))
                       (multiple-value-bind (r v) (orbit-state orbit node)
                         (let ((dv (funcall dv-fn r v node)))
                           (multiple-value-bind (burn propellant) (burn-at label node r v dv)
                             (when (>= (planned-burn-hold-at burn) earliest)
                               (return (values node burn propellant r v dv)))))))))
               (apsis-times (kind)
                 (shot-event-times shot kind))
               (finish ()
                 (values (assemble-burn-plan (coerce (reverse burns) 'simple-vector)
                                             preset goal m-p load)
                         nil)))
        (ecase preset
          (:circularise
           (check-type goal (member :apoapsis :periapsis))
           (multiple-value-bind (node burn propellant)
               (first-node (apsis-times goal)
                           (lambda (r v node)
                             (declare (ignore node))
                             (vec3-sub (tangential-velocity r v (sqrt (/ mu (vec3-norm r)))) v))
                           :circularise)
             (declare (ignore node))
             (keep! burn propellant)
             (finish)))
          ((:set-apoapsis :set-periapsis)
           (let ((goal-r (+ +radius-earth+ (float goal 1d0))))
             (multiple-value-bind (node burn propellant)
                 (first-node (apsis-times (if (eq preset :set-apoapsis) :periapsis :apoapsis))
                             (lambda (r v node)
                               (declare (ignore node))
                               (let ((rn (vec3-norm r)))
                                 (vec3-sub (tangential-velocity
                                            r v (sqrt (* mu (- (/ 2 rn) (/ 2 (+ rn goal-r))))))
                                           v)))
                             :apsis)
               (declare (ignore node))
               (keep! burn propellant)
               (finish))))
          (:change-orbit
           (destructuring-bind (altitude &optional inclination) goal
             (let* ((r2 (+ +radius-earth+ (float altitude 1d0)))
                    (raise (>= r2 (orbit-a orbit))))
               (flet ((target-v (r v node speed plane)
                        ;; The circular or transfer velocity, turned into the goal plane.
                        (let ((v1 (tangential-velocity r v speed)))
                          (if (and plane inclination)
                              (rotate-about v1 (vec3-normalize r)
                                            (plane-goal-angle shot r v inclination node))
                              v1))))
                 (multiple-value-bind (node1 burn1 propellant1 r1 v1 dv1)
                     (first-node (apsis-times (if raise :periapsis :apoapsis))
                                 (lambda (r v node)
                                   (let ((rn (vec3-norm r)))
                                     (vec3-sub (target-v r v node
                                                         (sqrt (* mu (- (/ 2 rn) (/ 2 (+ rn r2)))))
                                                         (not raise))
                                               v)))
                                 :departure)
                   (keep! burn1 propellant1)
                   (let*((transfer (state->orbit r1 (vec3-add v1 dv1) mu node1))
                          (node2 (grid-round (+ node1 (/ (orbit-period transfer) 2)))))
                     (multiple-value-bind (r v) (orbit-state transfer node2)
                       (let ((dv (vec3-sub (target-v r v node2 (sqrt (/ mu (vec3-norm r))) raise) v)))
                         (multiple-value-bind (burn propellant)
                             (burn-at :circularise node2 r v dv)
                           (keep! burn propellant))))
                     (finish)))))))
          (:change-plane
           (let ((times (if (eq goal :target)
                            (sort (append (shot-event-times shot :relative-ascending)
                                          (shot-event-times shot :relative-descending))
                                  #'<)
                            (shot-equator-times shot)))
                 (found '()))
             ;; The first two nodes whose burns hold after EARLIEST.
             (dolist (node (if times
                               (append times (list (+ (car (last times)) (/ period 2))
                                                   (+ (car (last times)) period)))
                               '()))
               (multiple-value-bind (r v) (orbit-state orbit node)
                 (let ((dv (vec3-sub (rotate-about v (vec3-normalize r)
                                                   (plane-goal-angle shot r v goal node))
                                     v)))
                   (multiple-value-bind (burn propellant) (burn-at :plane node r v dv)
                     (when (>= (planned-burn-hold-at burn) earliest)
                       (push (list (vec3-norm r) burn propellant) found)
                       (when (= (length found) 2) (return)))))))
             (when (null found)
               (return-from autopilot-first-guess (values nil "NO NODE AHEAD")))
             (destructuring-bind (a &optional b) (reverse found)
               (let ((pick (if (and b (> (- (first b) (first a)) (* *node-tie* (first a))))
                               b
                               a)))
                 (keep! (second pick) (third pick))
                 (finish))))))))))

;;; Shooting (decision 117): each burn's Δv in its node's LVLH axes (T, N,
;;; R) refined by Newton on shots, sim-step! runs of a snapshot copy with
;;; the candidate plan, the goal measured over one period after the last cut.

(defparameter *plan-floor-margin* 50d3
  "m above Earth's crash radius: no node of a plan below it; decision 32.")
(defparameter *plan-moon-margin* 10d3 "m above the Moon's radius; decision 32.")
(defparameter *goal-altitude-max* 300000d3
  "m: a higher goal is refused; milestone 3 keeps apoapses below the Moon's sphere, decision 117.")
(defparameter *apsis-tolerance-fraction* 1d-3 "Of the goal radius; decision 117.")
(defparameter *apsis-tolerance-min* 1d3 "m; decision 117.")
(defparameter *plane-tolerance* (deg->rad 0.01d0) "rad; decision 117.")
(defparameter *plan-iterations* 8 "Newton steps at most, then \"NOT CONVERGED\"; decision 117.")
(defparameter *shot-perturbation* 1d0
  "m/s: the Jacobian's finite difference; well above the cut's grain (a
1/64 s tick at full throttle, 0.044 m/s wet).")
(defparameter *plan-max-burns* 4 "Burns a job refines.")
(defparameter *plan-max-residuals* 4 "Goal residuals a job measures.")
(defparameter *shot-cap-margin* 600d0
  "s: a shot whose plan never ends (no propellant) stops this after its last burn's planned end.")
(declaim (type double-float *plan-floor-margin* *plan-moon-margin* *goal-altitude-max*
               *apsis-tolerance-fraction* *apsis-tolerance-min* *plane-tolerance*
               *shot-perturbation* *shot-cap-margin*)
         (type fixnum *plan-iterations* *plan-max-burns* *plan-max-residuals*))

(defun apsis-tolerance (radius)
  "m: max(0.1 % of RADIUS, 1 km); decision 117."
  (max (* *apsis-tolerance-fraction* radius) *apsis-tolerance-min*))

(defun lvlh-axes (r v)
  "(values t n r) unit vectors at R, V: in-plane horizontal, orbit normal, radial out."
  (let* ((n (vec3-normalize (vec3-cross r v)))
         (out (vec3-normalize r)))
    (values (vec3-cross n out) n out)))

(defun burn-dv-vector (burn)
  "BURN's Δv vector: its nose's direction (against it for RCS −1) times its Δv."
  (vec3-scale (quat-rotate (planned-burn-attitude burn) (vec3 1 0 0))
              (if (eql (planned-burn-rcs burn) -1)
                  (- (planned-burn-dv burn))
                  (planned-burn-dv burn))))

;;; Goal measures over a shot (decision 117)

(defun shot-measure (shot start)
  "Over SHOT's nodes from the one at START to its end: (values r-max r-min
mean-i mean-h end r0 ex ey), the largest and smallest |r| (nodes and apsis
events), the time-weighted means of the osculating inclination, the unit
angular momentum (normalised), |r| and the eccentricity vector, this along
r̂ at START and ĥ × r̂ there; nil without two nodes from START."
  (let ((i0 (node-interval shot start))
        (count (prediction-count shot))
        (nodes (prediction-nodes shot)))
    (when (and i0 (< (1+ i0) count))
      (let ((r-max 0d0) (r-min most-positive-double-float)
            (sum-i 0d0) (sum-r 0d0) (sum-h (make-vec3)) (sum-e (make-vec3)) (span 0d0)
            (h (make-vec3)) (r (make-vec3)) (v (make-vec3)) (e (make-vec3))
            (x (make-vec3)) (y (make-vec3))
            (last-i 0d0) (last-r 0d0) (last-h (make-vec3)) (last-e (make-vec3))
            (end (node-t nodes (1- count))))
        (loop for i from i0 below count
              for k = (* +node-size+ i)
              do (replace r nodes :start2 (+ k 1) :end2 (+ k 4))
                 (replace v nodes :start2 (+ k 4) :end2 (+ k 7))
                 (vec3-normalize! h (vec3-cross! h r v))
                 (when (= i i0)
                   (vec3-normalize! x r)
                   (vec3-cross! y h x))
                 (let* ((rn (vec3-norm r))
                        (inclination (acos (max -1d0 (min 1d0 (aref h 2))))))
                   ;; e = ((v² − μ/r) r − (r · v) v) / μ
                   (vec3-scale! e r (- (vec3-dot v v) (/ +mu-earth+ rn)))
                   (vec3-add-scaled! e e v (- (vec3-dot r v)))
                   (vec3-scale! e e (/ +mu-earth+))
                   (setf r-max (max r-max rn) r-min (min r-min rn))
                   (when (> i i0)
                     (let ((dt (* 0.5d0 (- (node-t nodes i) (node-t nodes (1- i))))))
                       (incf span (* 2 dt))
                       (incf sum-i (* dt (+ inclination last-i)))
                       (incf sum-r (* dt (+ rn last-r)))
                       (dotimes (j 3)
                         (incf (aref sum-h j) (* dt (+ (aref h j) (aref last-h j))))
                         (incf (aref sum-e j) (* dt (+ (aref e j) (aref last-e j)))))))
                   (setf last-i inclination last-r rn)
                   (replace last-h h)
                   (replace last-e e)))
        (dotimes (n (prediction-event-count shot))
          (let ((time (aref (prediction-event-times shot) n)))
            (when (and (member (svref (prediction-event-kinds shot) n) '(:apoapsis :periapsis))
                       (<= start time end)
                       (prediction-position! r shot time))
              (let ((rn (vec3-norm r)))
                (setf r-max (max r-max rn) r-min (min r-min rn))))))
        (values r-max r-min (/ sum-i span) (vec3-normalize! sum-h sum-h) end
                (/ sum-r span) (/ (vec3-dot sum-e x) span) (/ (vec3-dot sum-e y) span))))))

(defstruct (goal-measure (:constructor make-goal-measure (r-max r-min plane &optional steer))
                         (:copier nil))
  (r-max nil :type (or null double-float) :read-only t) ; m: the largest |r| asked
  (r-min nil :type (or null double-float) :read-only t) ; m: the smallest
  (plane nil :type (or null double-float (eql :target)) :read-only t) ; an inclination or the target's
  ;; What Newton steers the radii by: :apsides (the largest and smallest
  ;; |r|), :circle (the mean |r| and eccentricity vector, two burns or
  ;; more), :shape (the eccentricity vector: one burn keeps its point).
  (steer :apsides :type (member :apsides :circle :shape) :read-only t))

(defun goal-residuals! (out measure shot start &optional newton)
  "OUT: MEASURE's residuals on SHOT from START, each over its tolerance (|x|
≤ 1 within it); (values count largest), a large one without a measure.
NEWTON: the residuals Newton steers by (goal-measure-steer): for a
circular goal the mean eccentricity vector times the radius, with two burns
the mean |r| too (smooth, and blind to where the window starts, where under
J2 the largest and smallest |r| jump between near-equal extrema), instead of
the largest and smallest |r|."
  (multiple-value-bind (r-max r-min mean-i mean-h end r0 ex ey) (shot-measure shot start)
    (let ((n 0)
          (high (goal-measure-r-max measure))
          (low (goal-measure-r-min measure))
          (plane (goal-measure-plane measure)))
      (flet ((put (x) (setf (aref out n) (if r-max x 1d3)) (incf n)))
        (cond ((and newton (not (eq (goal-measure-steer measure) :apsides)))
               (let ((tolerance (apsis-tolerance high)))
                 (when (eq (goal-measure-steer measure) :circle)
                   (put (and r0 (/ (- r0 high) tolerance))))
                 (put (and ex (/ (* ex high) tolerance)))
                 (put (and ey (/ (* ey high) tolerance)))))
              (t (when high (put (and r-max (/ (- r-max high) (apsis-tolerance high)))))
                 (when low (put (and r-min (/ (- r-min low) (apsis-tolerance low)))))))
        (cond ((null plane))
              ((null mean-i) (put nil))
              ((eq plane :target)
               (let ((h-goal (shot-target-normal shot (* 0.5d0 (+ start end)))))
                 (put (if h-goal
                          (/ (atan (vec3-norm (vec3-cross mean-h h-goal)) (vec3-dot mean-h h-goal))
                             *plane-tolerance*)
                          1d3))))
              (t (put (/ (- mean-i plane) *plane-tolerance*)))))
      (values n (loop for i below n maximize (abs (aref out i)))))))

;;; Newton's minimum-norm step

(defun minimum-norm-step! (out jacobian residuals m n)
  "OUT (N): the least-norm δ with J δ = −RESIDUALS, J the M × N JACOBIAN
(row-major, M ≤ N): δ = −Jᵀ (J Jᵀ)⁻¹ r, Gaussian elimination with partial
pivoting; a singular J J ᵀ is regularised. OUT."
  (let ((a (make-array (list m (1+ m)) :element-type 'double-float :initial-element 0d0))
        (y (make-array m :element-type 'double-float :initial-element 0d0)))
    (dotimes (i m)
      (dotimes (j m)
        (setf (aref a i j) (loop for k below n
                                 sum (* (aref jacobian (+ (* i n) k)) (aref jacobian (+ (* j n) k))))))
      (setf (aref a i m) (- (aref residuals i))))
    (let ((scale (loop for i below m maximize (aref a i i))))
      (dotimes (i m)
        (incf (aref a i i) (* 1d-12 (max scale 1d-300)))))
    (dotimes (col m)
      (let ((pivot (loop with best = col
                         for row from col below m
                         when (> (abs (aref a row col)) (abs (aref a best col))) do (setf best row)
                         finally (return best))))
        (dotimes (j (1+ m))
          (rotatef (aref a col j) (aref a pivot j)))
        (loop for row from (1+ col) below m
              do (let ((f (/ (aref a row col) (aref a col col))))
                   (loop for j from col to m
                         do (decf (aref a row j) (* f (aref a col j))))))))
    (loop for i from (1- m) downto 0
          do (setf (aref y i) (/ (- (aref a i m)
                                    (loop for j from (1+ i) below m sum (* (aref a i j) (aref y j))))
                                 (aref a i i))))
    (dotimes (k n out)
      (setf (aref out k) (loop for i below m sum (* (aref jacobian (+ (* i n) k)) (aref y i)))))))

;;; The planning job (decision 117): resumable, on a snapshot, in the
;;; prediction's work units.

(defstruct (autopilot-job (:constructor %make-autopilot-job (snapshot scratch coast shot))
                          (:copier nil))
  (snapshot nil :type sim)                      ; the flown sim at the request
  (scratch nil :type sim)                       ; the snapshot with the candidate, for a shot
  (coast nil :type prediction)                  ; the snapshot coasting: nodes, coast radii
  (shot nil :type prediction)                   ; the last shot, the final plan's at the end
  (preset nil :type (or null keyword))
  (goal nil)
  (stage :done :type (member :coast :guess :iterate :check :done :refused))
  (iteration 0 :type fixnum)                    ; Newton steps taken
  (column -1 :type fixnum)                      ; the parameter being perturbed, −1 the base shot
  (plan nil :type (or null burn-plan))          ; the result, :done
  (reason nil :type (or null string))           ; the refusal, :refused
  (candidate nil :type (or null burn-plan))     ; the plan being shot
  (propellant 0d0 :type double-float)           ; kg the candidate's burns need
  (measure nil :type (or null goal-measure))
  (burn-count 0 :type fixnum)
  (burn-labels (make-array *plan-max-burns* :initial-element :trim) :type simple-vector)
  ;; Per burn its node's t and the state there before it (r, v).
  (node-times (make-array *plan-max-burns* :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))
  (node-states (make-array (* 6 *plan-max-burns*) :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))
  (params (make-array (* 3 *plan-max-burns*) :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))       ; per burn Δv along T, N, R
  (delta (make-array (* 3 *plan-max-burns*) :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))
  (residual-count 0 :type fixnum)
  (residuals (make-array *plan-max-residuals* :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))       ; the base shot's
  (column-residuals (make-array *plan-max-residuals* :element-type 'double-float
                                                     :initial-element 0d0)
   :type (simple-array double-float (*)))
  (jacobian (make-array (* *plan-max-residuals* 3 *plan-max-burns*) :element-type 'double-float
                                                                    :initial-element 0d0)
   :type (simple-array double-float (*)))       ; row-major, residuals × parameters
  (work 0 :type fixnum))                        ; units done in the last call

(defun new-autopilot-job (sim)
  "A job for sims with SIM's objects: its snapshot, scratch and two predictions."
  (%make-autopilot-job (copy-sim sim) (copy-sim sim) (new-prediction) (new-prediction)))

(defun goal-altitude (preset goal)
  "The goal altitude (m) of PRESET and GOAL, or nil."
  (case preset
    ((:set-apoapsis :set-periapsis) (float goal 1d0))
    (:change-orbit (float (first goal) 1d0))))

(defun goal-plane (preset goal)
  "The plane PRESET and GOAL ask for: an inclination (rad), :target or nil."
  (let ((plane (case preset
                 (:change-orbit (second goal))
                 (:change-plane goal))))
    (if (realp plane) (float plane 1d0) plane)))

(defun job-refuse! (job reason)
  (setf (autopilot-job-stage job) :refused
        (autopilot-job-reason job) reason
        (autopilot-job-candidate job) nil)
  job)

(defun autopilot-job-start! (job sim preset goal)
  "JOB planning PRESET with GOAL (autopilot-first-guess's) from a snapshot
of SIM; refused at once outside Earth orbit, for a goal above
*goal-altitude-max* or a target's plane without a target. JOB."
  (let ((snapshot (autopilot-job-snapshot job)))
    (sim-assign! snapshot sim)
    (setf (autopilot-job-preset job) preset
          (autopilot-job-goal job) goal
          (autopilot-job-iteration job) 0
          (autopilot-job-column job) -1
          (autopilot-job-plan job) nil
          (autopilot-job-reason job) nil
          (autopilot-job-candidate job) nil
          (autopilot-job-measure job) nil
          (autopilot-job-burn-count job) 0
          (autopilot-job-work job) 0
          (autopilot-job-stage job) :coast)
    (let* ((ship (sim-ship snapshot))
           (orbit (state->orbit (ship-state-r ship) (ship-state-v ship) +mu-earth+ (sim-t snapshot)))
           (altitude (goal-altitude preset goal)))
      (cond ((not (and (eq (sim-body snapshot) :earth) (elliptic-p orbit)))
             (job-refuse! job "NOT IN EARTH ORBIT"))
            ((and altitude (> altitude *goal-altitude-max*))
             (job-refuse! job "GOAL OUT OF RANGE"))
            ((and (eq (goal-plane preset goal) :target) (null (sim-target snapshot)))
             (job-refuse! job "NO TARGET"))
            (t (coast-shot-start! (autopilot-job-coast job) (autopilot-job-scratch job) snapshot))))
    job))

(defun job-candidate! (job)
  "JOB's candidate plan from its parameters: each burn built (build-burn)
on its node with Δv = T t̂ + N n̂ + R r̂ there, the propellant after the
earlier burns, re-centred; the propellant needed. JOB."
  (let* ((snapshot (autopilot-job-snapshot job))
         (ship (sim-ship snapshot))
         (orbit (state->orbit (ship-state-r ship) (ship-state-v ship) +mu-earth+ (sim-t snapshot)))
         (load (sim-stack-load snapshot))
         (m-p (ship-state-m-p ship))
         (n (autopilot-job-burn-count job))
         (burns (make-array n))
         (states (autopilot-job-node-states job))
         (params (autopilot-job-params job))
         (propellant 0d0)
         (previous nil))
    (dotimes (i n)
      (let ((r (subseq states (* 6 i) (+ (* 6 i) 3)))
            (v (subseq states (+ (* 6 i) 3) (+ (* 6 i) 6))))
        (multiple-value-bind (ta na ra) (lvlh-axes r v)
          (let ((dv (vec3-add (vec3-add (vec3-scale ta (aref params (* 3 i)))
                                        (vec3-scale na (aref params (+ (* 3 i) 1))))
                              (vec3-scale ra (aref params (+ (* 3 i) 2))))))
            (multiple-value-bind (burn needed)
                (build-burn (svref (autopilot-job-burn-labels job) i) (aref (autopilot-job-node-times job) i)
                            r v dv m-p load previous orbit)
              (setf (svref burns i) burn
                    previous (planned-burn-attitude burn))
              (decf m-p needed)
              (incf propellant needed))))))
    (setf (autopilot-job-candidate job)
          (assemble-burn-plan burns (autopilot-job-preset job) (autopilot-job-goal job) m-p load)
          (autopilot-job-propellant job) propellant)
    job))

(defun job-shoot! (job &optional column)
  "Start JOB's shot of its candidate, COLUMN's parameter perturbed by
*shot-perturbation* (the candidate rebuilt either way); horizon the last
cut plus a period, capped *shot-cap-margin* past the last burn's planned end."
  (let ((params (autopilot-job-params job)))
    (when column (incf (aref params column) *shot-perturbation*))
    (job-candidate! job)
    (when column (decf (aref params column) *shot-perturbation*)))
  (let* ((plan (autopilot-job-candidate job))
         (burns (burn-plan-burns plan))
         (last (svref burns (1- (length burns))))
         (cap (+ (planned-burn-cut-t last)
                 (* 3 (- (planned-burn-cut-t last) (planned-burn-start last)))
                 *shot-cap-margin*)))
    (prediction-shoot! (autopilot-job-shot job)
                       (shot-sim-assign! (autopilot-job-scratch job) (autopilot-job-snapshot job) plan)
                       cap :after-plan t)
    (setf (autopilot-job-column job) (or column -1))
    job))

(defun shot-last-cut (job)
  "The t of the last burn's cut in JOB's shot, or nil if the shot ended first."
  (prediction-cut-time (autopilot-job-shot job) (1- (autopilot-job-burn-count job))))

(defun job-shot-residuals! (job out)
  "OUT: the Newton residuals of JOB's finished shot (goal-residuals!);
(values count largest), LARGEST the goal measure's, large for a shot lost
or cut short."
  (let* ((cut (shot-last-cut job))
         (shot (autopilot-job-shot job))
         (start (if (and cut (null (sim-loss (prediction-sim shot)))) cut most-positive-double-float))
         (measure (autopilot-job-measure job))
         (largest (nth-value 1 (goal-residuals! out measure shot start))))
    (values (goal-residuals! out measure shot start t) largest)))

(defun job-guess! (job)
  "From the coast shot: the first guess, its burns' nodes and LVLH Δv, the
goal measure, the propellant check; then the base shot."
  (let ((snapshot (autopilot-job-snapshot job))
        (coast (autopilot-job-coast job))
        (preset (autopilot-job-preset job))
        (goal (autopilot-job-goal job)))
    (multiple-value-bind (guess reason) (autopilot-first-guess snapshot preset goal coast)
      (unless guess
        (return-from job-guess! (job-refuse! job reason)))
      (let* ((burns (burn-plan-burns guess))
             (n (length burns))
             (states (autopilot-job-node-states job))
             (params (autopilot-job-params job)))
        (assert (<= n *plan-max-burns*))
        (setf (autopilot-job-burn-count job) n)
        (dotimes (i n)
          (let* ((burn (svref burns i))
                 (dv (burn-dv-vector burn))
                 (r (planned-burn-cut-r burn))
                 (v (vec3-sub (planned-burn-cut-v burn) dv)))
            (setf (svref (autopilot-job-burn-labels job) i) (planned-burn-label burn)
                  (aref (autopilot-job-node-times job) i) (planned-burn-cut-t burn))
            (replace states r :start1 (* 6 i))
            (replace states v :start1 (+ (* 6 i) 3))
            (multiple-value-bind (ta na ra) (lvlh-axes r v)
              (setf (aref params (* 3 i)) (vec3-dot dv ta)
                    (aref params (+ (* 3 i) 1)) (vec3-dot dv na)
                    (aref params (+ (* 3 i) 2)) (vec3-dot dv ra)))))
        ;; The radius at the first node on the coast shot: circularise's goal,
        ;; the apsis a set Ap / Pe keeps.
        (let* ((node-t (aref (autopilot-job-node-times job) 0))
               (p (make-vec3))
               (here (if (prediction-position! p coast node-t)
                         (vec3-norm p)
                         (vec3-norm (planned-burn-cut-r (svref burns 0)))))
               (altitude (goal-altitude preset goal))
               (radius (and altitude (+ +radius-earth+ altitude)))
               (plane (goal-plane preset goal)))
          (setf (autopilot-job-measure job)
                (ecase preset
                  (:circularise (make-goal-measure here here nil :shape))
                  ((:set-apoapsis :set-periapsis)
                   (make-goal-measure (max here radius) (min here radius) nil))
                  (:change-orbit (make-goal-measure radius radius plane :circle))
                  (:change-plane (make-goal-measure nil nil plane)))))
        (job-candidate! job)
        (let ((needed (+ (autopilot-job-propellant job)
                         (burn-plan-reserve-mass (autopilot-job-candidate job)))))
          (when (> needed (ship-state-m-p (sim-ship snapshot)))
            (return-from job-guess!
              (job-refuse! job (format nil "NEEDS ~D KG" (ceiling needed))))))
        (setf (autopilot-job-stage job) :iterate)
        (job-shoot! job)))))

(defun job-shot-done! (job)
  "JOB's shot finished: a base shot converged goes to :check, else the
Jacobian's columns one shot each, then the minimum-norm step and the next
base shot; past *plan-iterations* steps \"NOT CONVERGED\"."
  (let ((column (autopilot-job-column job))
        (n-params (* 3 (autopilot-job-burn-count job))))
    (if (< column 0)
        (multiple-value-bind (m largest) (job-shot-residuals! job (autopilot-job-residuals job))
          (setf (autopilot-job-residual-count job) m)
          (cond ((or (<= largest 1d0)
                     ;; A crash or an abort: :check refuses it.
                     (sim-loss (prediction-sim (autopilot-job-shot job)))
                     (prediction-abort (autopilot-job-shot job)))
                 (setf (autopilot-job-stage job) :check))
                ((or (>= (autopilot-job-iteration job) *plan-iterations*)
                     (null (shot-last-cut job)))
                 (job-refuse! job "NOT CONVERGED"))
                (t (job-shoot! job 0))))
        (let ((out (autopilot-job-column-residuals job))
              (base (autopilot-job-residuals job))
              (jacobian (autopilot-job-jacobian job))
              (m (autopilot-job-residual-count job)))
          (job-shot-residuals! job out)
          (dotimes (i m)
            (setf (aref jacobian (+ (* i n-params) column))
                  (/ (- (aref out i) (aref base i)) *shot-perturbation*)))
          (if (< (1+ column) n-params)
              (job-shoot! job (1+ column))
              (let ((step (autopilot-job-delta job))
                    (params (autopilot-job-params job)))
                (minimum-norm-step! step jacobian base m n-params)
                (dotimes (k n-params)
                  (incf (aref params k) (aref step k)))
                (incf (autopilot-job-iteration job))
                (job-shoot! job)))))))

(defun shot-refusal (job)
  "The refusal of JOB's final shot and candidate, or nil: a node below
Earth's crash radius + *plan-floor-margin* (or a crash, or the shot's
\"UNSAFE ORBIT\" abort), within the Moon's radius + *plan-moon-margin*, the
propellant and reserve over what is on board; another abort of the shot
its reason."
  (let* ((shot (autopilot-job-shot job))
         (nodes (prediction-nodes shot))
         (floor (+ (body-crash-radius (body :earth)) *plan-floor-margin*))
         (near (+ +radius-moon+ *plan-moon-margin*))
         (far (- +moon-distance-min+ near))
         (abort (prediction-abort shot))
         (r (make-vec3)) (moon (make-vec3)) (moon-v (make-vec3)))
    (cond ((or (sim-loss (prediction-sim shot))
               (and abort (search "UNSAFE ORBIT" abort))
               (loop for i from (prediction-head shot) below (prediction-count shot)
                     for k = (* +node-size+ i)
                     do (replace r nodes :start2 (+ k 1) :end2 (+ k 4))
                     thereis (< (vec3-norm r) floor)))
           (format nil "BELOW ~D KM" (round (- floor +radius-earth+) 1000)))
          ((loop for i from (prediction-head shot) below (prediction-count shot)
                 for k = (* +node-size+ i)
                 do (replace r nodes :start2 (+ k 1) :end2 (+ k 4))
                 thereis (and (> (vec3-norm r) far)
                              (progn (body-state-into moon moon-v :moon (node-t nodes i) :earth)
                                     (< (vec3-norm (vec3-sub! moon moon r)) near))))
           "TOO NEAR THE MOON")
          (t (let ((needed (+ (autopilot-job-propellant job)
                              (burn-plan-reserve-mass (autopilot-job-candidate job)))))
               (cond ((> needed (ship-state-m-p (sim-ship (autopilot-job-snapshot job))))
                      (format nil "NEEDS ~D KG" (ceiling needed)))
                     (abort (subseq abort (length "AUTOPILOT ABORT: ")))))))))

(defun shot-cut-plan (plan shot)
  "PLAN with each burn's cut state the SHOT's (the copy's t, r, v at its
cut) and the goal orbit from the last."
  (let* ((nodes (prediction-nodes shot))
         (burns (map 'simple-vector
                     (lambda (burn i)
                       (let* ((time (prediction-cut-time shot i))
                              (k (* +node-size+ (node-interval shot time))))
                         (make-planned-burn (planned-burn-label burn) (planned-burn-hold-at burn)
                                            (planned-burn-start burn) (planned-burn-attitude burn)
                                            (planned-burn-throttle burn) (planned-burn-rcs burn)
                                            (planned-burn-dv burn)
                                            :cut-t time
                                            :cut-r (subseq nodes (+ k 1) (+ k 4))
                                            :cut-v (subseq nodes (+ k 4) (+ k 7))
                                            :isp (planned-burn-isp burn))))
                     (burn-plan-burns plan)
                     (loop for i below (length (burn-plan-burns plan)) collect i)))
         (last (svref burns (1- (length burns)))))
    (make-burn-plan burns :coast-hold (burn-plan-coast-hold plan)
                          :preset (burn-plan-preset plan) :goal (burn-plan-goal plan)
                          :goal-orbit (state->orbit (planned-burn-cut-r last) (planned-burn-cut-v last)
                                                    +mu-earth+ (planned-burn-cut-t last))
                          :reserve-dv (burn-plan-reserve-dv plan)
                          :reserve-mass (burn-plan-reserve-mass plan))))

(defun job-check! (job)
  "The refusals on JOB's final shot, else the plan with the shot's cut states: :done."
  (let ((reason (shot-refusal job)))
    (if reason
        (job-refuse! job reason)
        (setf (autopilot-job-plan job) (shot-cut-plan (autopilot-job-candidate job)
                                                      (autopilot-job-shot job))
              (autopilot-job-stage job) :done))
    job))

(defun autopilot-job-work! (job budget)
  "At most BUDGET work units (the prediction's, decision 116) of JOB; its
stage: :coast, :guess, :iterate, :check, then :done (autopilot-job-plan)
or :refused (autopilot-job-reason)."
  (let ((left budget))
    (flet ((advance (pred)
             (prediction-extend! pred left)
             (decf left (prediction-last-work pred))
             (prediction-complete-p pred)))
      (loop
        (ecase (autopilot-job-stage job)
          ((:done :refused) (return))
          (:coast (if (advance (autopilot-job-coast job))
                      (setf (autopilot-job-stage job) :guess)
                      (return)))
          (:guess (job-guess! job))
          (:iterate (if (advance (autopilot-job-shot job))
                        (job-shot-done! job)
                        (return)))
          (:check (job-check! job)))))
    (setf (autopilot-job-work job) (- budget left))
    (autopilot-job-stage job)))

(defun autopilot-plan-now (sim preset goal &optional (job (new-autopilot-job sim)))
  "PRESET with GOAL planned from SIM at once (headless, tests, flight
scripts): (values plan nil) or (values nil reason)."
  (autopilot-job-start! job sim preset goal)
  (autopilot-job-work! job most-positive-fixnum)
  (values (autopilot-job-plan job) (autopilot-job-reason job)))
