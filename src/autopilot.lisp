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
  ;; A replan (decision 117): the plan flown and its first burn left to re-shoot.
  (base nil :type (or null burn-plan))
  (from 0 :type fixnum)
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
          (autopilot-job-base job) nil
          (autopilot-job-from job) 0
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

(defun replan-remaining-guess (snapshot base from)
  "The first guess re-shooting BASE's burns from FROM on, from SNAPSHOT: each
on its old node (the middle of its start and cut) with its old Δv vector,
the node's state by Kepler from SNAPSHOT's osculating orbit after the
earlier burns applied at their nodes."
  (let* ((ship (sim-ship snapshot))
         (mu +mu-earth+)
         (coast (state->orbit (ship-state-r ship) (ship-state-v ship) mu (sim-t snapshot)))
         (orbit coast)
         (load (sim-stack-load snapshot))
         (m-p (ship-state-m-p ship))
         (old-burns (burn-plan-burns base))
         (burns '())
         (previous nil))
    (loop for i from from below (length old-burns)
          for old = (svref old-burns i)
          for node = (grid-round (* 0.5d0 (+ (planned-burn-start old) (planned-burn-cut-t old))))
          do (multiple-value-bind (r v) (orbit-state orbit node)
               (let ((dv (burn-dv-vector old)))
                 (multiple-value-bind (burn propellant)
                     (build-burn (planned-burn-label old) node r v dv m-p load previous coast)
                   (push burn burns)
                   (setf previous (planned-burn-attitude burn)
                         orbit (state->orbit r (vec3-add v dv) mu node))
                   (decf m-p propellant)))))
    (assemble-burn-plan (coerce (nreverse burns) 'simple-vector)
                        (burn-plan-preset base) (burn-plan-goal base) m-p load)))

(defun trim-goal (measure coast start)
  "The preset and goal whose first guess starts a trim for MEASURE missed on
COAST from START: a plane alone by its plane change; set Ap / Pe by the
apsis further off its tolerance; a circular goal by circularising at the
apsis nearer its radius."
  (multiple-value-bind (r-max r-min) (shot-measure coast start)
    (let ((high (goal-measure-r-max measure))
          (low (goal-measure-r-min measure)))
      (cond ((null high)
             (values :change-plane (goal-measure-plane measure)))
            ((eq (goal-measure-steer measure) :apsides)
             (if (>= (abs (/ (- r-max high) (apsis-tolerance high)))
                     (abs (/ (- r-min low) (apsis-tolerance low))))
                 (values :set-apoapsis (- high +radius-earth+))
                 (values :set-periapsis (- low +radius-earth+))))
            (t (values :circularise
                       (if (< (abs (- r-max high)) (abs (- r-min high))) :apoapsis :periapsis)))))))

(defun replan-guess (job)
  "JOB's first guess as a replan: the base's burns left, or after its last
one the goal measured on the coast shot from the snapshot and, missed, a
trim's guess. (values plan reason trim): no plan and no reason when the goal
is met."
  (let* ((snapshot (autopilot-job-snapshot job))
         (coast (autopilot-job-coast job))
         (base (autopilot-job-base job))
         (from (autopilot-job-from job))
         (measure (burn-plan-measure base)))
    (if (< from (length (burn-plan-burns base)))
        (values (replan-remaining-guess snapshot base from) nil nil)
        (if (<= (nth-value 1 (goal-residuals! (autopilot-job-residuals job) measure coast
                                               (sim-t snapshot)))
                1d0)
            (values nil nil nil)
            (multiple-value-bind (preset goal) (trim-goal measure coast (sim-t snapshot))
              (multiple-value-bind (plan reason) (autopilot-first-guess snapshot preset goal coast)
                (values plan reason t)))))))

(defun job-guess! (job)
  "From the coast shot: the first guess (a replan's: replan-guess), its
burns' nodes and LVLH Δv, the goal measure (a replan's the base's), the
propellant check; then the base shot. A replan whose goal is met is :done
without a plan."
  (let ((snapshot (autopilot-job-snapshot job))
        (coast (autopilot-job-coast job))
        (preset (autopilot-job-preset job))
        (goal (autopilot-job-goal job))
        (base (autopilot-job-base job))
        (trim nil))
    (multiple-value-bind (guess reason)
        (if base
            (multiple-value-bind (plan reason trimming) (replan-guess job)
              (setf trim trimming)
              (values plan reason))
            (autopilot-first-guess snapshot preset goal coast))
      (unless guess
        (return-from job-guess!
          (if reason
              (job-refuse! job reason)
              (progn (setf (autopilot-job-plan job) nil
                           (autopilot-job-stage job) :done)
                     job))))
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
            (setf (svref (autopilot-job-burn-labels job) i) (if trim :trim (planned-burn-label burn))
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
                (if base
                    (burn-plan-measure base)
                    (ecase preset
                      (:circularise (make-goal-measure here here nil :shape))
                      ((:set-apoapsis :set-periapsis)
                       (make-goal-measure (max here radius) (min here radius) nil))
                      (:change-orbit (make-goal-measure radius radius plane :circle))
                      (:change-plane (make-goal-measure nil nil plane))))))
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

(defun shot-cut-plan (plan shot measure)
  "PLAN with each burn's cut state the SHOT's (the copy's t, r, v at its
cut), the goal orbit from the last and the goal MEASURE (measured at each
cut in flight)."
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
                          :reserve-mass (burn-plan-reserve-mass plan)
                          :measure measure)))

(defun job-check! (job)
  "The refusals on JOB's final shot, else the plan with the shot's cut states: :done."
  (let ((reason (shot-refusal job)))
    (if reason
        (job-refuse! job reason)
        (setf (autopilot-job-plan job) (shot-cut-plan (autopilot-job-candidate job)
                                                      (autopilot-job-shot job)
                                                      (autopilot-job-measure job))
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

(defun autopilot-replan-job-start! (job sim)
  "JOB re-planning SIM's plan, in :replan after a cut (decision 117): the
burns after the one cut re-shot from a snapshot of SIM, or after the last
one a :trim burn when the goal measure misses; same preset and goal. JOB."
  (let ((plan (sim-plan sim)))
    (autopilot-job-start! job sim (burn-plan-preset plan) (burn-plan-goal plan))
    (setf (autopilot-job-base job) plan
          (autopilot-job-from job) (1+ (sim-plan-index sim)))
    job))

(defun autopilot-replan-now (sim &optional (job (new-autopilot-job sim)))
  "SIM's replan at once: (values plan reason); neither when the goal is met."
  (autopilot-replan-job-start! job sim)
  (autopilot-job-work! job most-positive-fixnum)
  (values (autopilot-job-plan job) (autopilot-job-reason job)))

;;; Rendezvous: the stop point's choice, aim points and the terminal check
;;; (docs/design.md "The terminal path and crowded targets"; decision 119:
;;; Rendezvous planning in milestone 3: Lambert, search, aim and stop points,
;;; checks, shooting, burns, decision 59: Rendezvous transfers pass the target
;;; clear). Stop points are places.lisp's.

(defparameter *aim-distance* 1000d0 "m: an aim point from the target or the stop point; decision 119.")
(defparameter *terminal-check-step* 300d0 "s between the terminal check's times; decision 119.")
(defparameter *terminal-check-span* 1800d0 "s after the arrival the terminal check covers; decision 119.")
(defparameter *inward-axis-cos* (cos (deg->rad 45d0))
  "A fixture axis within 45° of nadir points inward: aim point (2) below the stop point.")

(defun rendezvous-stop (sim target &key fixture (range *stop-range*))
  "The stop point's choice for a rendezvous with TARGET (FIXTURE a fixture's
name or nil, RANGE m): (values fixture nil), the fixture as
rendezvous-fixture gives it, or (values nil reason)."
  (multiple-value-bind (chosen reason) (rendezvous-fixture sim target fixture)
    (let ((reason (or reason (stop-range-refusal chosen (float range 1d0)))))
      (if reason (values nil reason) (values chosen nil)))))

(defun segment-distance (p a b)
  "m: from P to the segment A–B."
  (let* ((ab (vec3-sub b a))
         (l2 (vec3-dot ab ab))
         (s (if (> l2 0d0) (max 0d0 (min 1d0 (/ (vec3-dot (vec3-sub p a) ab) l2))) 0d0)))
    (vec3-norm (vec3-sub p (vec3-add-scaled a ab s)))))

(defun to-terminal-frame! (out q p origin)
  "OUT = q* (P − ORIGIN): P in the frame Q at ORIGIN; OUT."
  (let ((c (quat-conjugate q)))
    (quat-rotate! out c (vec3-sub! out p origin))))

(defun terminal-path-margin (sim target fixture stop aim t-arr)
  "m to spare along the terminal segment from AIM to STOP (positions at
T-ARR, Earth-centred), fixed in the terminal frame: its distance from the
target's CoM less *target-clearance*, and from every other object neither
removed nor latched less its bound + *object-clearance*, those at T-ARR and
every *terminal-check-step* to T-ARR + *terminal-check-span*. Below 0: not
clear."
  (let* ((object (target-object sim target))
         (rt (make-vec3)) (vt (make-vec3)) (q (make-quat)) (w (make-vec3))
         (r (make-vec3)) (v (make-vec3)) (a (make-vec3)) (s (make-vec3))
         (margin most-positive-double-float))
    (stop-target-state-into! rt vt sim target fixture t-arr)
    (terminal-frame-into! q w object rt vt t-arr)
    (to-terminal-frame! a q aim rt)
    (to-terminal-frame! s q stop rt)
    (when object
      (setf margin (- (segment-distance (make-vec3) a s) *target-clearance*)))
    (loop for k from 0 to (round *terminal-check-span* *terminal-check-step*)
          for time = (+ t-arr (* k *terminal-check-step*))
          do (stop-target-state-into! rt vt sim target fixture time)
             (terminal-frame-into! q w object rt vt time)
             (loop for other across (sim-objects sim)
                   unless (or (eq other object)
                              (member (object-status other) '(:removed :latched)))
                     do (when (object-planning-state-into! r v other sim time)
                          (to-terminal-frame! r q r rt)
                          (setf margin (min margin (- (segment-distance r a s)
                                                      (object-def-bound (object-def-of other))
                                                      *object-clearance*))))))
    margin))

(defun terminal-path-clear-p (sim target fixture stop aim t-arr)
  "The terminal segment from AIM to STOP keeps its clearances
(terminal-path-margin)."
  (>= (terminal-path-margin sim target fixture stop aim t-arr) 0d0))

(defun aim-point (sim target fixture range number t-arr &optional (side 1))
  "Aim point NUMBER at T-ARR: (values r v) (m, m/s, Earth-centred). (1) the
target's own rails state *aim-distance* / |v_T| earlier, on its orbit; (2)
*aim-distance* from the stop point along the target's radius, outward, inward
when the fixture's axis is within 45° of nadir; (3) out of the plane, along
SIDE (±1) times the target's angular momentum. (2) and (3) move with the
terminal frame: v_T + ω × their offset from the target."
  (let ((rt (make-vec3)) (vt (make-vec3)) (r (make-vec3)) (v (make-vec3)))
    (stop-target-state-into! rt vt sim target fixture t-arr)
    (if (= number 1)
        (stop-target-state-into! r v sim target fixture
                                 (- t-arr (/ *aim-distance* (vec3-norm vt))))
        (let ((object (target-object sim target))
              (q (make-quat)) (w (make-vec3))
              (offset (if (= number 2)
                          (vec3-normalize rt)
                          (vec3-scale (vec3-normalize (vec3-cross rt vt)) (float side 1d0)))))
          (terminal-frame-into! q w object rt vt t-arr)
          (when (and (= number 2) (typep fixture 'fixture)
                     (< (vec3-dot (quat-rotate q (fixture-axis fixture)) offset)
                        (- *inward-axis-cos*)))
            (vec3-scale! offset offset -1d0))
          (stop-point-into! r v sim target fixture (float range 1d0) t-arr)
          (vec3-add-scaled! r r offset *aim-distance*)
          (vec3-add! v vt (vec3-cross w (vec3-sub r rt)))))
    (values r v)))

(defun rendezvous-aim (sim target fixture range t-arr)
  "The first aim point whose terminal path is clear (decision 119): (values
number r v side), SIDE the out-of-plane side of (3) (1 otherwise), or
(values nil \"NO CLEAR APPROACH\"). (3) tries the clearer side first."
  (let ((stop (make-vec3)) (stop-v (make-vec3)))
    (stop-point-into! stop stop-v sim target fixture (float range 1d0) t-arr)
    (flet ((margin (number side)
             (multiple-value-bind (r v) (aim-point sim target fixture range number t-arr side)
               (values (terminal-path-margin sim target fixture stop r t-arr) r v))))
      (loop for number in '(1 2)
            do (multiple-value-bind (m r v) (margin number 1)
                 (when (>= m 0d0)
                   (return-from rendezvous-aim (values number r v 1)))))
      (multiple-value-bind (m+ r+ v+) (margin 3 1)
        (multiple-value-bind (m- r- v-) (margin 3 -1)
          (cond ((and (>= m+ m-) (>= m+ 0d0)) (values 3 r+ v+ 1))
                ((>= m- 0d0) (values 3 r- v- -1))
                (t (values nil "NO CLEAR APPROACH"))))))))

;;; The preset menu (decision 118: Autopilot preset menu and input in
;;; milestone 3): the clock's model (clock-autopilot, not in the sim or
;;; checkpoints), driven by O, the arrows, Return and Backspace; drawn by the
;;; cockpit and the overlay.

(defparameter *plan-stale-lead* 10d0 "s before the first hold-at a shown plan goes stale; decision 117.")
(defparameter *plan-stale-distance* 1d0 "m off the job's final shot: stale; decision 117.")
(defparameter *menu-altitude-step* 10 "km a step; decision 118.")
(defparameter *menu-altitude-min* 150 "km; decision 118.")
(defparameter *menu-altitude-max* 300000 "km; decision 118.")
(defparameter *menu-inclination-step* 1 "Half degrees a step (0.5°); decision 118.")
(defparameter *menu-inclination-max* 360 "Half degrees (180°); decision 118.")
(defparameter *menu-repeat-rate* 8 "Steps per real second while Left or Right is held; decision 118.")
(defparameter *menu-fast-after* 1 "s held: then each step ×*menu-fast-factor*; decision 118.")
(defparameter *menu-fast-factor* 10 "decision 118.")
(declaim (type double-float *plan-stale-lead* *plan-stale-distance*)
         (type fixnum *menu-altitude-step* *menu-altitude-min* *menu-altitude-max*
               *menu-inclination-step* *menu-inclination-max* *menu-repeat-rate*
               *menu-fast-after* *menu-fast-factor*))

(defstruct (autopilot-row (:constructor make-autopilot-row (key kind init text goal &key offered))
                          (:copier nil))
  (key nil :type keyword :read-only t)
  ;; What Left/Right do: :altitude (km), :inclination (half degrees),
  ;; :inclination-keep ((half degrees . the start's): KEEP at the start),
  ;; :apsis (:apoapsis ↔ :periapsis), :none.
  (kind :none :type (member :altitude :inclination :inclination-keep :apsis :none) :read-only t)
  (init nil :type function :read-only t)        ; (sim orbit) → the value on opening
  (text nil :type function :read-only t)        ; (value) → the row's text
  (goal nil :type function :read-only t)        ; (model) → (values preset goal)
  (offered nil :type (or null function) :read-only t)) ; (sim) → listed; nil: always

(defun round-to (x step)
  "X rounded to a multiple of STEP, an integer."
  (* step (round x step)))

(defun menu-altitude (radius)
  "km: RADIUS's altitude rounded to *menu-altitude-step*, within the menu's range."
  (max *menu-altitude-min*
       (min *menu-altitude-max*
            (round-to (/ (- radius +radius-earth+) 1000) *menu-altitude-step*))))

(defun menu-inclination (orbit)
  "Half degrees: ORBIT's inclination rounded to *menu-inclination-step*."
  (round-to (* 2 (rad->deg (orbit-i orbit))) *menu-inclination-step*))

(defun format-half-degrees (half)
  (format nil "~,1F°" (/ half 2)))

(defun model-value (model key)
  (cdr (assoc key (autopilot-model-values model))))

(defun half-degrees->rad (half) (deg->rad (/ half 2d0)))

(defun orbit-goal (model)
  (let ((inc (model-value model :orbit-inc)))
    (values :change-orbit
            (list (* 1000d0 (model-value model :change-orbit))
                  (and (/= (car inc) (cdr inc)) (half-degrees->rad (car inc)))))))

(defparameter *autopilot-rows*
  (list
   (make-autopilot-row :circularise :apsis
                       (lambda (sim orbit)
                         (let ((ap (orbit-next-apsis orbit (sim-t sim) :apoapsis))
                               (pe (orbit-next-apsis orbit (sim-t sim) :periapsis)))
                           (if (and ap (or (null pe) (< ap pe))) :apoapsis :periapsis)))
                       (lambda (value) (format nil "CIRCULARISE ~:[PE~;AP~]" (eq value :apoapsis)))
                       (lambda (model) (values :circularise (model-value model :circularise))))
   (make-autopilot-row :set-apoapsis :altitude
                       (lambda (sim orbit)
                         (declare (ignore sim))
                         (menu-altitude (or (orbit-apoapsis-radius orbit) (orbit-q orbit))))
                       (lambda (km) (format nil "SET AP ~:D KM" km))
                       (lambda (model)
                         (values :set-apoapsis (* 1000d0 (model-value model :set-apoapsis)))))
   (make-autopilot-row :set-periapsis :altitude
                       (lambda (sim orbit)
                         (declare (ignore sim))
                         (menu-altitude (orbit-q orbit)))
                       (lambda (km) (format nil "SET PE ~:D KM" km))
                       (lambda (model)
                         (values :set-periapsis (* 1000d0 (model-value model :set-periapsis)))))
   (make-autopilot-row :change-orbit :altitude
                       (lambda (sim orbit)
                         (declare (ignore sim))
                         (menu-altitude (orbit-a orbit)))
                       (lambda (km) (format nil "ORBIT ~:D KM" km))
                       #'orbit-goal)
   (make-autopilot-row :orbit-inc :inclination-keep
                       (lambda (sim orbit)
                         (declare (ignore sim))
                         (let ((half (menu-inclination orbit))) (cons half half)))
                       (lambda (inc)
                         (if (= (car inc) (cdr inc))
                             "ORBIT INC KEEP"
                             (format nil "ORBIT INC ~A" (format-half-degrees (car inc)))))
                       #'orbit-goal)
   (make-autopilot-row :change-plane :inclination
                       (lambda (sim orbit)
                         (declare (ignore sim))
                         (menu-inclination orbit))
                       (lambda (half) (format nil "PLANE ~A" (format-half-degrees half)))
                       (lambda (model)
                         (values :change-plane (half-degrees->rad (model-value model :change-plane)))))
   (make-autopilot-row :plane-target :none
                       (lambda (sim orbit) (declare (ignore sim orbit)) nil)
                       (lambda (value) (declare (ignore value)) "PLANE TARGET")
                       (lambda (model) (declare (ignore model)) (values :change-plane :target))
                       :offered (lambda (sim) (and (sim-target sim) t))))
  "The menu's rows in order (decision 118); task 161 appends its rendezvous rows.")

(defstruct (autopilot-model (:constructor make-autopilot-model ()) (:copier nil))
  (open nil :type boolean)                      ; the menu shown: state not :closed
  (rows (copy-list *autopilot-rows*) :type list)
  (selection :circularise :type keyword)        ; the selected row's key
  (values '() :type list)                       ; (key . value) per row
  (job nil :type (or null autopilot-job))       ; made at the first plan, reused
  (replan-job nil :type (or null autopilot-job)) ; made at the first replan, reused
  (replan nil :type (or null burn-plan))        ; the plan it re-plans, nil once installed
  (replan-t 0d0 :type double-float)             ; its cut
  (row nil :type (or null keyword))             ; the row the job plans, or the plan's
  (plan nil :type (or null burn-plan))          ; the plan shown
  (flown nil :type (or null burn-plan))         ; the plan last engaged: the map's, grey after
  (engage nil :type boolean)                    ; engage the job's plan once shown
  (engaged nil :type boolean)                   ; one of its plans in the sim (mirrored)
  (state :closed :type (member :closed :menu :planning :shown :refused :engaged :off))
  (text nil :type (or null string))             ; the refusal or the abort, or a state's text
  (held 0 :type fixnum)                         ; −1 Left, +1 Right held since its press, 0
  (held-time 0d0 :type double-float)            ; real s held
  (held-steps 0 :type fixnum))                  ; repeats made while held

(defun sim-earth-orbit (sim)
  "SIM's osculating orbit about Earth; a hyperbola as it is."
  (let ((ship (sim-ship sim)))
    (state->orbit (ship-state-r ship) (ship-state-v ship) +mu-earth+ (sim-t sim))))

(defun autopilot-offered-rows (model sim)
  "MODEL's rows SIM offers, in order."
  (remove-if-not (lambda (row)
                   (let ((offered (autopilot-row-offered row)))
                     (or (null offered) (funcall offered sim))))
                 (autopilot-model-rows model)))

(defun autopilot-selected-row (model sim)
  "The selected row, or the first offered when it is not offered."
  (let ((rows (autopilot-offered-rows model sim)))
    (or (find (autopilot-model-selection model) rows :key #'autopilot-row-key)
        (first rows))))

(defun autopilot-init-values! (model sim)
  "Each row's value from SIM's current orbit (decision 118)."
  (let ((orbit (sim-earth-orbit sim)))
    (unless (elliptic-p orbit)
      ;; Out of orbit: a circular orbit at the ship's radius for the values.
      (let ((r (vec3-norm (ship-state-r (sim-ship sim)))))
        (setf orbit (make-elliptic-orbit +mu-earth+ r 0d0 (orbit-i orbit) 0d0 0d0 0d0 (sim-t sim)))))
    (setf (autopilot-model-values model)
          (mapcar (lambda (row)
                    (cons (autopilot-row-key row) (funcall (autopilot-row-init row) sim orbit)))
                  (autopilot-model-rows model))))
  model)

(defun autopilot-step-value! (model sim direction factor)
  "The selected row's value DIRECTION (±1) steps of FACTOR, within its range."
  (let* ((row (autopilot-selected-row model sim))
         (cell (and row (assoc (autopilot-row-key row) (autopilot-model-values model)))))
    (when cell
      (flet ((clamp (x low high) (max low (min high x))))
        (ecase (autopilot-row-kind row)
          (:altitude
           (setf (cdr cell) (clamp (+ (cdr cell) (* direction factor *menu-altitude-step*))
                                   *menu-altitude-min* *menu-altitude-max*)))
          (:inclination
           (setf (cdr cell) (clamp (+ (cdr cell) (* direction factor *menu-inclination-step*))
                                   0 *menu-inclination-max*)))
          (:inclination-keep
           (setf (cdr cell) (cons (clamp (+ (cadr cell) (* direction factor *menu-inclination-step*))
                                         0 *menu-inclination-max*)
                                  (cddr cell))))
          (:apsis
           (setf (cdr cell) (if (eq (cdr cell) :apoapsis) :periapsis :apoapsis)))
          (:none)))))
  model)

(defun autopilot-set-state! (model state &optional text)
  (setf (autopilot-model-state model) state
        (autopilot-model-open model) (not (eq state :closed))
        (autopilot-model-text model) text)
  model)

(defun autopilot-drop! (model sim)
  "A shown, refused or planning plan dropped back to the menu (the job
abandoned); :off back to the menu."
  (when (member (autopilot-model-state model) '(:planning :shown :refused :off))
    (setf (autopilot-model-plan model) nil
          (autopilot-model-engage model) nil)
    (autopilot-set-state! model (if (sim-plan sim) :engaged :menu)))
  model)

(defun autopilot-plan! (model clock sim &optional engage)
  "A job for the selected row from SIM now: :planning \"PLANNING…\"; ENGAGE:
engaged once shown. A refusal at the start shows at once."
  (let ((row (autopilot-selected-row model sim)))
    (when row
      (multiple-value-bind (preset goal) (funcall (autopilot-row-goal row) model)
        (let ((job (or (autopilot-model-job model)
                       (setf (autopilot-model-job model) (new-autopilot-job sim)))))
          (autopilot-job-start! job sim preset goal)
          (setf (autopilot-model-row model) (autopilot-row-key row)
                (autopilot-model-plan model) nil
                (autopilot-model-engage model) engage)
          (autopilot-set-state! model :planning "PLANNING…")
          (autopilot-job-poll! model clock sim)))))
  model)

(defun autopilot-plan-stale-p (model sim)
  "MODEL's shown plan is stale for SIM (decision 117): its first hold-at −
*plan-stale-lead* passed, or SIM's r more than *plan-stale-distance* off
the job's final shot at SIM's t."
  (let* ((plan (autopilot-model-plan model))
         (shot (autopilot-job-shot (autopilot-model-job model)))
         (time (sim-t sim))
         (p (make-vec3)))
    (or (> time (- (planned-burn-hold-at (svref (burn-plan-burns plan) 0)) *plan-stale-lead*))
        (not (prediction-position! p shot time))
        (> (vec3-norm (vec3-sub! p p (ship-state-r (sim-ship sim)))) *plan-stale-distance*))))

(defun autopilot-engage-shown! (model clock sim)
  "The shown plan engaged, or when stale \"PLAN STALE: REPLANNING\" and a new
job engaged once shown."
  (if (autopilot-plan-stale-p model sim)
      (progn (clock-message! clock :autopilot "PLAN STALE: REPLANNING")
             (autopilot-plan! model clock sim t))
      (autopilot-engage! clock sim (autopilot-model-plan model))))

(defun autopilot-job-poll! (model clock sim)
  "While :planning, the job's end: :shown (engaged with the engage flag), or
:refused with the message \"PLAN REFUSED: <REASON>\"."
  (when (eq (autopilot-model-state model) :planning)
    (let ((job (autopilot-model-job model)))
      (case (autopilot-job-stage job)
        (:done
         (setf (autopilot-model-plan model) (autopilot-job-plan job))
         (autopilot-set-state! model :shown)
         (when (autopilot-model-engage model)
           (setf (autopilot-model-engage model) nil)
           (autopilot-engage-shown! model clock sim)))
        (:refused
         (let ((reason (autopilot-job-reason job)))
           (setf (autopilot-model-engage model) nil)
           (autopilot-set-state! model :refused reason)
           (clock-message! clock :autopilot-refused (format nil "PLAN REFUSED: ~A" reason)))))))
  model)

(defun autopilot-key! (model clock sim key)
  "A menu command (decision 118), between frames: :open (O: open or close),
:up, :down (select, wrapping), :left, :right (one step of the value; held:
autopilot-held!), :return (plan; engage a shown plan; after a disengage or
abort re-plan and engage once shown), :backspace (disengage a running plan,
else drop a shown, refused or planning one, else close). The arrows and
Return act with the menu open."
  (autopilot-sync! model sim)
  (let ((state (autopilot-model-state model))
        (open (autopilot-model-open model)))
    (ecase key
      (:open
       (if open
           (progn (setf (autopilot-model-plan model) nil
                        (autopilot-model-engage model) nil)
                  (autopilot-set-state! model :closed))
           (progn (autopilot-init-values! model sim)
                  (autopilot-set-state! model (if (sim-plan sim) :engaged :menu)))))
      ((:up :down)
       (when open
         (autopilot-drop! model sim)
         (let* ((rows (autopilot-offered-rows model sim))
                (i (or (position (autopilot-row-key (autopilot-selected-row model sim)) rows
                                 :key #'autopilot-row-key)
                       0)))
           (setf (autopilot-model-selection model)
                 (autopilot-row-key (nth (mod (+ i (if (eq key :down) 1 -1)) (length rows)) rows))))))
      ((:left :right)
       (when open
         (autopilot-drop! model sim)
         (let ((direction (if (eq key :right) 1 -1)))
           (autopilot-step-value! model sim direction 1)
           (setf (autopilot-model-held model) direction
                 (autopilot-model-held-time model) 0d0
                 (autopilot-model-held-steps model) 0))))
      (:return
       (when open
         (case state
           ((:menu :refused :engaged) (autopilot-plan! model clock sim))
           (:off (autopilot-plan! model clock sim t))
           (:shown (autopilot-engage-shown! model clock sim)))))
      (:backspace
       (cond ((sim-plan sim) (autopilot-disengage! clock sim))
             ((member state '(:planning :shown :refused))
              (autopilot-drop! model sim))
             (open (autopilot-set-state! model :closed))))))
  model)

(defun autopilot-held! (model sim direction elapsed)
  "Once per frame while the arrow of DIRECTION (±1) pressed last is held,
ELAPSED real s: *menu-repeat-rate* steps a second after the press's own,
each ×*menu-fast-factor* after *menu-fast-after* s (decision 118); another
DIRECTION or 0 ends the repeat."
  (if (or (/= direction (autopilot-model-held model)) (= direction 0)
          (not (autopilot-model-open model)))
      (setf (autopilot-model-held model) 0)
      (let* ((time (incf (autopilot-model-held-time model) (float elapsed 1d0)))
             (due (floor (+ (* time *menu-repeat-rate*) 1d-9))))
        (loop while (< (autopilot-model-held-steps model) due)
              do (let ((k (incf (autopilot-model-held-steps model))))
                   (autopilot-step-value! model sim direction
                                          (if (> k (* *menu-fast-after* *menu-repeat-rate*))
                                              *menu-fast-factor*
                                              1))))))
  model)

(defun autopilot-engaged! (model plan)
  "PLAN engaged (autopilot-engage!): kept for the map, :engaged."
  (setf (autopilot-model-flown model) plan
        (autopilot-model-plan model) nil
        (autopilot-model-engage model) nil
        (autopilot-model-engaged model) t)
  (autopilot-set-state! model :engaged))

(defun autopilot-stopped! (model text &optional (state :off))
  "The engaged plan ended with TEXT: STATE (:off after a disengage or abort,
:closed after completion) unless a new plan is planned or shown."
  (setf (autopilot-model-engaged model) nil)
  (unless (member (autopilot-model-state model) '(:planning :shown :refused))
    (autopilot-set-state! model state text))
  model)

(defun autopilot-notice! (model text)
  "A sim notice of key :autopilot (clock-take-notices!): an abort leaves
:off with TEXT, completion or a missed goal :closed."
  (when (autopilot-model-engaged model)
    (autopilot-stopped! model text (if (search "ABORT" text) :off :closed)))
  model)

(defun autopilot-sync! (model sim)
  "The engaged flag mirrored from SIM: a plan gone without a notice (a
restart, a script) ends as completion does."
  (when (and (autopilot-model-engaged model) (null (sim-plan sim)))
    (autopilot-stopped! model nil :closed))
  model)

(defun autopilot-planning-p (model)
  (eq (autopilot-model-state model) :planning))

(defun autopilot-barrier (model)
  "The t the clock steps the sim to at most while MODEL's job runs (its
snapshot's + *plan-barrier*), or nil."
  (when (autopilot-planning-p model)
    (+ (sim-t (autopilot-job-snapshot (autopilot-model-job model))) *plan-barrier*)))

;;; Replans (decision 117): a cut off its shot leaves the sim's plan in
;;; :replan; the clock runs a job from that step's sim and installs its
;;; result at the cut + *plan-barrier*, the same in every mode.

(defun autopilot-replan-start! (model sim)
  "After a step leaving SIM's plan in :replan: MODEL's replan job started
from SIM, unless already for this plan and cut."
  (unless (and (eq (autopilot-model-replan model) (sim-plan sim))
               (= (autopilot-model-replan-t model) (sim-plan-replan-t sim)))
    (let ((job (or (autopilot-model-replan-job model)
                   (setf (autopilot-model-replan-job model) (new-autopilot-job sim)))))
      (autopilot-replan-job-start! job sim)
      (setf (autopilot-model-replan model) (sim-plan sim)
            (autopilot-model-replan-t model) (sim-plan-replan-t sim))))
  model)

(defun autopilot-replanning-p (model sim)
  "MODEL's replan job runs for SIM's plan."
  (and (eq (sim-plan-phase sim) :replan)
       (eq (autopilot-model-replan model) (sim-plan sim))
       (not (member (autopilot-job-stage (autopilot-model-replan-job model)) '(:done :refused)))))

(defun autopilot-replan-work! (model sim budget)
  "The replan job's share of the frame: at most BUDGET units while it runs;
the units done."
  (if (autopilot-replanning-p model sim)
      (let ((job (autopilot-model-replan-job model)))
        (autopilot-job-work! job budget)
        (autopilot-job-work job))
      0))

(defun autopilot-replan-due! (model clock sim finish)
  "Before a step of SIM in :replan (clock-step!): true to step on. Before
the cut + *plan-barrier* the job runs on; there, its job done (or FINISH:
done at once) its result installed (sim-install-plan!, the message
\"AUTOPILOT: REPLANNED\"), no plan the goal met (the plan :done, completing
in the step), a refusal an abort with its reason; nil while it still runs."
  (autopilot-replan-start! model sim)
  (let ((job (autopilot-model-replan-job model)))
    (cond ((< (sim-t sim) (+ (sim-plan-replan-t sim) *plan-barrier*)) t)
          ((and (not finish) (autopilot-replanning-p model sim)) nil)
          (t
           (autopilot-job-work! job most-positive-fixnum)
           (setf (autopilot-model-replan model) nil)
           (ecase (autopilot-job-stage job)
             (:done
              (let ((plan (autopilot-job-plan job)))
                (if plan
                    (progn (sim-install-plan! sim plan)
                           (setf (autopilot-model-flown model) plan)
                           (clock-message! clock :autopilot "AUTOPILOT: REPLANNED"))
                    (setf (sim-plan-phase sim) :done))))
             (:refused (plan-abort! sim (autopilot-job-reason job))))
           t))))

(defun autopilot-frame! (model clock sim budget)
  "Once per frame (clock-frame!): the job's BUDGET work units, its end polled;
the engaged flag mirrored."
  (when (autopilot-planning-p model)
    (autopilot-job-work! (autopilot-model-job model) budget)
    (autopilot-job-poll! model clock sim))
  (autopilot-sync! model sim))

(defun autopilot-menu-lines (model sim)
  "The offered rows' texts with their values, the selection marked \"> \", the
others \"  \"; second value the selection's index (decision 118)."
  (let* ((rows (autopilot-offered-rows model sim))
         (selected (autopilot-selected-row model sim)))
    (values (mapcar (lambda (row)
                      (concatenate 'string (if (eq row selected) "> " "  ")
                                   (funcall (autopilot-row-text row)
                                            (model-value model (autopilot-row-key row)))))
                    rows)
            (position selected rows))))

(defun autopilot-plan-summary (plan sim)
  "PLAN against SIM now, for the instruments (docs/design.md \"Instruments\",
Burn): a plist :burns (count), :index (the current burn: SIM's cursor when
PLAN is engaged, else 0), :total-dv (m/s, every burn), :propellant (kg
the burns from the current one need, rocket equation from the stack's mass
now), :left (kg after them), :next-in (s to the current burn's start, 0
once started), :next-dv, :next-duration (s, start to cut), :arrival (the last
cut's t). Pure; conses only its result."
  (let* ((burns (burn-plan-burns plan))
         (engaged (eq (sim-plan sim) plan))
         (index (if engaged (sim-plan-index sim) 0))
         (burn (svref burns index))
         (m-p (ship-state-m-p (sim-ship sim)))
         (mass (+ *ship-dry-mass* m-p (with-stack-load (load sim) (stack-load-mass load))))
         (end mass))
    (loop for i from index below (length burns)
          for b = (svref burns i)
          do (setf end (* end (exp (- (/ (planned-burn-dv b) (* (planned-burn-isp b) +g0+)))))))
    (list :burns (length burns)
          :index index
          :total-dv (loop for b across burns sum (planned-burn-dv b))
          :propellant (- mass end)
          :left (- m-p (- mass end))
          :next-in (max 0d0 (- (planned-burn-start burn) (sim-t sim)))
          :next-dv (planned-burn-dv burn)
          :next-duration (- (planned-burn-cut-t burn) (planned-burn-start burn))
          :arrival (planned-burn-cut-t (svref burns (1- (length burns)))))))

(defun autopilot-reset! (model)
  "MODEL as new (clock-restart!): closed, no plan or job running, the values
and the job's buffers kept."
  (setf (autopilot-model-plan model) nil
        (autopilot-model-flown model) nil
        (autopilot-model-replan model) nil
        (autopilot-model-row model) nil
        (autopilot-model-engage model) nil
        (autopilot-model-engaged model) nil
        (autopilot-model-held model) 0)
  (autopilot-set-state! model :closed))
