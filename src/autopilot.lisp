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
(defparameter *rendezvous-halvings* 4
  "A rendezvous's Newton step that worsens the largest residual is halved back at most this often.")
(defparameter *rendezvous-stall-tolerance* 3d0
  "Tolerances a stalled rendezvous Newton accepts its best shot within: the
:departure's cut lands on a 1/64 s tick (0.044 m/s at full throttle), which
moves the arrival 100–300 m on an hour's transfer; decision 164.")
(defparameter *shot-perturbation* 1d0
  "m/s: the Jacobian's finite difference; well above the cut's grain (a
1/64 s tick at full throttle, 0.044 m/s wet).")
(defparameter *plan-max-burns* 4 "Burns a job refines.")
(defparameter *plan-max-residuals* 6 "Goal residuals a job measures: a rendezvous's position and velocity.")
(defparameter *retime-pool* 8 "A re-timing's cheapest scan points, refined and checked in order.")
(defparameter *shot-cap-margin* 600d0
  "s: a shot whose plan never ends (no propellant) stops this after its last burn's planned end.")
(declaim (type double-float *plan-floor-margin* *plan-moon-margin* *goal-altitude-max*
               *apsis-tolerance-fraction* *apsis-tolerance-min* *plane-tolerance*
               *shot-perturbation* *shot-cap-margin* *rendezvous-stall-tolerance*)
         (type fixnum *plan-iterations* *rendezvous-halvings* *plan-max-burns* *plan-max-residuals*
               *retime-pool*))

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

(defstruct (autopilot-job (:constructor %make-autopilot-job (snapshot scratch coast shot lead research lead-scan))
                          (:copier nil))
  (snapshot nil :type sim)                      ; the flown sim at the request
  (scratch nil :type sim)                       ; the snapshot with the candidate, for a shot
  ;; A rendezvous: the snapshot flying the candidate's fixed burns to
  ;; lead-t, where its shots start (job-lead-start!), their cut states, its
  ;; path's clearances, an abort.
  (lead nil :type sim)
  (lead-t 0d0 :type double-float)
  (lead-cuts '() :type list)                    ; per fixed burn (t r v), in order
  (lead-scan nil :type prediction)              ; only its clearance buffers
  (lead-abort nil :type (or null string))
  (research nil :type sim)                      ; the lead's world at lead-t, for planning
  (researched nil :type boolean)                ; research holds it
  (coast nil :type prediction)                  ; the snapshot coasting: nodes, coast radii
  (shot nil :type prediction)                   ; the last shot, the final plan's at the end
  (preset nil :type (or null keyword))
  (goal nil)
  (stage :done :type (member :coast :search :guess :lead :retime :iterate :check :done :refused))
  (search nil)                                  ; a rendezvous-search, run after :coast
  ;; A replan (decision 117): the plan flown and its first burn left to re-shoot.
  (base nil :type (or null burn-plan))
  (from 0 :type fixnum)
  ;; A rendezvous (decision 119): its stop point, the :plane guess, the
  ;; search's candidates, the one shot and its aim point.
  (target nil :type (or null keyword))
  (fixture nil)
  (range 0d0 :type double-float)
  (plane nil :type (or null planned-burn))
  (candidates #() :type simple-vector)
  (index 0 :type fixnum)
  (aim 1 :type fixnum)
  (side 1 :type fixnum)
  (fixed 0 :type fixnum)                        ; leading burns Newton keeps (:plane, :phasing)
  ;; The re-timing (job-retime-start!): the reference coast's first t
  ;; within *promotion-distance* of the target, nil if none.
  (retime-approach nil :type (or null double-float))
  (retimes 0 :type fixnum)
  (retime-cursor 0 :type fixnum)                ; −1 while the reference coast runs
  (retime-gap 0d0 :type double-float)           ; s from the :departure's hold-at to its node
  (retime-centre (make-array 3 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (3)))       ; the departure, transfer time and period scanned about
  ;; the scan's cheapest points, (total departure transfer-time) each, in order
  (retime-pool (make-array (* 3 *retime-pool*) :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))
  (retime-count 0 :type fixnum)
  (retime-point (make-array 3 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (3)))       ; the one refined, then the one chosen
  (retime-chosen nil :type boolean)
  (retime-own 0d0 :type double-float)           ; m/s the candidate's own times cost in the model
  (last-largest 0d0 :type double-float)         ; a rendezvous: the accepted shot's largest residual
  (base-params (make-array (* 3 *plan-max-burns*) :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))       ; … and its parameters
  (tolerance 1d0 :type double-float)            ; the largest residual accepted, more once stalled
  (fraction 1d0 :type double-float)             ; of the last step applied
  (halvings 0 :type fixnum)
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
  "A job for sims with SIM's objects: its snapshot, scratch, lead, research
and two predictions."
  (let* ((empty (make-array 0 :element-type 'double-float))
         (job (%make-autopilot-job (copy-sim sim) (copy-sim sim) (new-prediction) (new-prediction)
                                   (copy-sim sim) (copy-sim sim)
                                   (%make-prediction empty empty empty #() #() empty))))
    (setf (prediction-sim (autopilot-job-lead-scan job)) (autopilot-job-lead job))
    job))

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

(defun autopilot-job-start! (job sim preset goal &key base (from 0))
  "JOB planning PRESET with GOAL (autopilot-first-guess's, or :rendezvous's
(target &key range limit)) from a snapshot of SIM, or re-planning BASE's
burns FROM on; refused at once outside Earth orbit, for a goal above
*goal-altitude-max*, a target's plane without a target or a rendezvous's
stop point (job-rendezvous-start!). JOB."
  (let ((snapshot (autopilot-job-snapshot job)))
    (sim-assign! snapshot sim)
    (setf (autopilot-job-preset job) preset
          (autopilot-job-goal job) goal
          (autopilot-job-base job) base
          (autopilot-job-from job) from
          (autopilot-job-target job) nil
          (autopilot-job-fixture job) nil
          (autopilot-job-plane job) nil
          (autopilot-job-candidates job) #()
          (autopilot-job-index job) 0
          (autopilot-job-aim job) 1
          (autopilot-job-side job) 1
          (autopilot-job-fixed job) 0
          (autopilot-job-retimes job) 0
          (autopilot-job-researched job) nil
          (autopilot-job-lead-cuts job) '()
          (autopilot-job-lead-abort job) nil
          (autopilot-job-iteration job) 0
          (autopilot-job-column job) -1
          (autopilot-job-plan job) nil
          (autopilot-job-reason job) nil
          (autopilot-job-candidate job) nil
          (autopilot-job-measure job) nil
          (autopilot-job-burn-count job) 0
          (autopilot-job-work job) 0
          (autopilot-job-search job) nil
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
            ((and (eq preset :rendezvous) (not (job-rendezvous-start! job))))
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
cut plus a period, capped *shot-cap-margin* past the last burn's planned end
(a rendezvous: the cap, its clearance window from the first burn's start to
the last's)."
  (let ((params (autopilot-job-params job)))
    (when column (incf (aref params column) *shot-perturbation*))
    (job-candidate! job)
    (when column (decf (aref params column) *shot-perturbation*)))
  (let* ((plan (autopilot-job-candidate job))
         (burns (burn-plan-burns plan))
         (last (svref burns (1- (length burns))))
         (rendezvous (eq (autopilot-job-preset job) :rendezvous))
         (cap (+ (planned-burn-cut-t last)
                 (* 3 (- (planned-burn-cut-t last) (planned-burn-start last)))
                 *shot-cap-margin*))
         (shot (prediction-shoot! (autopilot-job-shot job)
                                  (shot-sim-assign! (autopilot-job-scratch job)
                                                    (if rendezvous
                                                        (autopilot-job-lead job)
                                                        (autopilot-job-snapshot job))
                                                    plan)
                                  cap :after-plan (not rendezvous))))
    (when rendezvous
      (let ((scratch (prediction-sim shot)))
        (setf (sim-plan-index scratch) (autopilot-job-fixed job)))
      (prediction-clearance! shot (planned-burn-start (svref burns 0)) (planned-burn-start last)))
    (setf (autopilot-job-column job) (or column -1))
    job))

(defun shot-first-burn (job)
  "The burn JOB's shots start from: a rendezvous's first free one (its lead
flies the fixed), else 0."
  (if (eq (autopilot-job-preset job) :rendezvous) (autopilot-job-fixed job) 0))

(defun shot-last-cut (job)
  "The t of the last burn's cut in JOB's shot, or nil if the shot ended first."
  (prediction-cut-time (autopilot-job-shot job)
                       (- (autopilot-job-burn-count job) 1 (shot-first-burn job))))

(defun job-shot-residuals! (job out)
  "OUT: the Newton residuals of JOB's finished shot (goal-residuals!);
(values count largest), LARGEST the goal measure's, large for a shot lost
or cut short; a rendezvous's rendezvous-residuals!."
  (when (eq (autopilot-job-preset job) :rendezvous)
    (return-from job-shot-residuals! (rendezvous-residuals! job out)))
  (let* ((cut (shot-last-cut job))
         (shot (autopilot-job-shot job))
         (start (if (and cut (null (sim-loss (prediction-sim shot)))) cut most-positive-double-float))
         (measure (autopilot-job-measure job))
         (largest (nth-value 1 (goal-residuals! out measure shot start))))
    (values (goal-residuals! out measure shot start t) largest)))

(defun replan-remaining-guess (snapshot base from)
  "The first guess re-shooting BASE's burns from FROM on, from SNAPSHOT: each
on its old node (the middle of its start and cut; a rendezvous's :arrival
on the aim point's t) with its old Δv vector, the node's state by Kepler
from SNAPSHOT's osculating orbit after the earlier burns applied at their
nodes."
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
          for node = (if (and (eq (planned-burn-label old) :arrival) (burn-plan-rendezvous base))
                         (rendezvous-part-arrival (burn-plan-rendezvous base))
                         (grid-round (* 0.5d0 (+ (planned-burn-start old) (planned-burn-cut-t old)))))
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

(defparameter *rendezvous-terminal-dv* 20d0
  "m/s the propellant check keeps for a rendezvous's terminal phase, on the RCS; decision 119.")
(declaim (type double-float *rendezvous-terminal-dv*))

(defun job-propellant-needed (job)
  "kg JOB's candidate needs: its burns', a rendezvous's terminal allowance
(*rendezvous-terminal-dv* at the RCS's Isp from the mass after them) and
the reserve."
  (let* ((candidate (autopilot-job-candidate job))
         (burns (autopilot-job-propellant job))
         (snapshot (autopilot-job-snapshot job))
         (end-mass (- (+ *ship-dry-mass* (ship-state-m-p (sim-ship snapshot))
                         (stack-load-mass (sim-stack-load snapshot)))
                      burns)))
    (+ burns
       (if (eq (autopilot-job-preset job) :rendezvous)
           (propellant-for end-mass *rendezvous-terminal-dv* *rcs-isp*)
           0d0)
       (burn-plan-reserve-mass candidate))))

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
        (rendezvous (eq (autopilot-job-preset job) :rendezvous))
        (trim nil))
    (multiple-value-bind (guess reason)
        (cond (base
               (multiple-value-bind (plan reason trimming) (replan-guess job)
                 (setf trim trimming)
                 (values plan reason)))
              (rendezvous (rendezvous-guess job))
              (t (autopilot-first-guess snapshot preset goal coast)))
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
        (setf (autopilot-job-burn-count job) n
              (autopilot-job-tolerance job) 1d0
              ;; a rendezvous's Newton: the :departure's and :arrival's Δv
              (autopilot-job-fixed job)
              (if rendezvous
                  (or (position :departure burns :key #'planned-burn-label) (1- n))
                  0))
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
                (cond (rendezvous nil)
                      (base (burn-plan-measure base))
                      (t (ecase preset
                           (:circularise (make-goal-measure here here nil :shape))
                           ((:set-apoapsis :set-periapsis)
                            (make-goal-measure (max here radius) (min here radius) nil))
                           (:change-orbit (make-goal-measure radius radius plane :circle))
                           (:change-plane (make-goal-measure nil nil plane)))))))
        (job-candidate! job)
        (let ((needed (job-propellant-needed job)))
          (when (> needed (ship-state-m-p (sim-ship snapshot)))
            (return-from job-guess!
              (job-refuse! job (format nil "NEEDS ~D KG" (ceiling needed))))))
        (if rendezvous
            (job-lead-start! job)
            (progn (setf (autopilot-job-stage job) :iterate)
                   (job-shoot! job)))))))

(defun job-shot-done! (job)
  "JOB's shot finished: a base shot converged goes to :check, else the
Jacobian's columns one shot each, then the minimum-norm step and the next
base shot; past *plan-iterations* steps \"NOT CONVERGED\" (a rendezvous:
the next candidate, job-next-candidate!). A rendezvous's Newton stalled
(its step halved *rendezvous-halvings* times without gain, or past
*plan-iterations*) takes its best shot within *rendezvous-stall-tolerance*
(shot again if it was not the last). The parameters are the burns' after
the fixed ones (autopilot-job-fixed)."
  (let* ((column (autopilot-job-column job))
         (n-params (* 3 (autopilot-job-burn-count job)))
         (first (* 3 (autopilot-job-fixed job)))
         (free (- n-params first))
         (rendezvous (eq (autopilot-job-preset job) :rendezvous)))
    (if (< column 0)
        (multiple-value-bind (m largest) (job-shot-residuals! job (autopilot-job-residuals job))
          (setf (autopilot-job-residual-count job) m)
          (cond ((job-retime-due-p job) (job-retime-start! job))
                ((and rendezvous
                      (> (autopilot-job-iteration job) 0)
                      (> largest (autopilot-job-last-largest job))
                      (< (autopilot-job-halvings job) *rendezvous-halvings*))
                 ;; worse: back half the step off and shoot again
                 (let ((half (* 0.5d0 (autopilot-job-fraction job)))
                       (step (autopilot-job-delta job))
                       (params (autopilot-job-params job)))
                   (dotimes (k free)
                     (decf (aref params (+ first k)) (* half (aref step k))))
                   (setf (autopilot-job-fraction job) half)
                   (incf (autopilot-job-halvings job))
                   (job-shoot! job)))
                ((or (<= largest (autopilot-job-tolerance job))
                     ;; A crash or an abort: :check refuses it.
                     (sim-loss (prediction-sim (autopilot-job-shot job)))
                     (prediction-abort (autopilot-job-shot job)))
                 (setf (autopilot-job-stage job) :check))
                ((and rendezvous
                      (or (>= (autopilot-job-iteration job) *plan-iterations*)
                          (and (> (autopilot-job-iteration job) 0)
                               (> largest (autopilot-job-last-largest job)))))
                 (job-stalled! job largest))
                ((or (>= (autopilot-job-iteration job) *plan-iterations*)
                     (null (shot-last-cut job)))
                 (if rendezvous
                     (job-next-candidate! job "NOT CONVERGED")
                     (job-refuse! job "NOT CONVERGED")))
                (t (setf (autopilot-job-last-largest job) largest)
                   (replace (autopilot-job-base-params job) (autopilot-job-params job))
                   (job-shoot! job first))))
        (let ((out (autopilot-job-column-residuals job))
              (base (autopilot-job-residuals job))
              (jacobian (autopilot-job-jacobian job))
              (m (autopilot-job-residual-count job)))
          (job-shot-residuals! job out)
          (dotimes (i m)
            (setf (aref jacobian (+ (* i free) (- column first)))
                  (/ (- (aref out i) (aref base i)) *shot-perturbation*)))
          (if (< (1+ column) n-params)
              (job-shoot! job (1+ column))
              (let ((step (autopilot-job-delta job))
                    (params (autopilot-job-params job)))
                (minimum-norm-step! step jacobian base m free)
                (dotimes (k free)
                  (incf (aref params (+ first k)) (aref step k)))
                (setf (autopilot-job-fraction job) 1d0
                      (autopilot-job-halvings job) 0)
                (incf (autopilot-job-iteration job))
                (job-shoot! job)))))))

(defun job-stalled! (job largest)
  "JOB's rendezvous Newton stalled, its last shot's largest residual
LARGEST: its best shot (the last, or the last step's base shot again)
accepted within *rendezvous-stall-tolerance*, else the next candidate
\"NOT CONVERGED\"."
  (let* ((last (or (= (autopilot-job-iteration job) 0)
                   (<= largest (autopilot-job-last-largest job))))
         (best (if last largest (autopilot-job-last-largest job))))
    (cond ((or (> (autopilot-job-tolerance job) 1d0) (> best *rendezvous-stall-tolerance*))
           (job-next-candidate! job "NOT CONVERGED"))
          (t (setf (autopilot-job-tolerance job) *rendezvous-stall-tolerance*)
             (if last
                 (setf (autopilot-job-stage job) :check)
                 (progn (replace (autopilot-job-params job) (autopilot-job-base-params job))
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
          (t (let ((needed (job-propellant-needed job)))
               (cond ((> needed (ship-state-m-p (sim-ship (autopilot-job-snapshot job))))
                      (format nil "NEEDS ~D KG" (ceiling needed)))
                     (abort (subseq abort (length "AUTOPILOT ABORT: ")))))))))

(defun shot-cut-plan (plan shot measure &optional rendezvous lead-cuts)
  "PLAN with each burn's cut state the SHOT's (the copy's t, r, v at its
cut; the burns before the shot's start LEAD-CUTS' (t r v)), the goal orbit
from the last, the goal MEASURE (measured at each cut in flight) and the
RENDEZVOUS part."
  (let* ((nodes (prediction-nodes shot))
         (first (length lead-cuts))
         (burns (map 'simple-vector
                     (lambda (burn i)
                       (flet ((cut (time r v)
                                (make-planned-burn (planned-burn-label burn) (planned-burn-hold-at burn)
                                                   (planned-burn-start burn) (planned-burn-attitude burn)
                                                   (planned-burn-throttle burn) (planned-burn-rcs burn)
                                                   (planned-burn-dv burn)
                                                   :cut-t time :cut-r r :cut-v v
                                                   :isp (planned-burn-isp burn))))
                         (if (< i first)
                             (destructuring-bind (time r v) (nth i lead-cuts)
                               (cut time (vec3-copy r) (vec3-copy v)))
                             (let* ((time (prediction-cut-time shot (- i first)))
                                    (k (* +node-size+ (node-interval shot time))))
                               (cut time (subseq nodes (+ k 1) (+ k 4)) (subseq nodes (+ k 4) (+ k 7)))))))
                     (burn-plan-burns plan)
                     (loop for i below (length (burn-plan-burns plan)) collect i)))
         (last (svref burns (1- (length burns)))))
    (make-burn-plan burns :coast-hold (burn-plan-coast-hold plan)
                          :preset (burn-plan-preset plan) :goal (burn-plan-goal plan)
                          :goal-orbit (state->orbit (planned-burn-cut-r last) (planned-burn-cut-v last)
                                                    +mu-earth+ (planned-burn-cut-t last))
                          :reserve-dv (burn-plan-reserve-dv plan)
                          :reserve-mass (burn-plan-reserve-mass plan)
                          :measure measure
                          :rendezvous rendezvous)))

(defun job-check! (job)
  "The refusals on JOB's final shot, else the plan with the shot's cut
states: :done. A rendezvous's shot also passes the path check
(shot-path-margin); a candidate failing it or refused but for propellant
gives way to the next (job-next-candidate!)."
  (let ((reason (or (let ((abort (autopilot-job-lead-abort job)))
                      (and abort (subseq abort (length "AUTOPILOT ABORT: "))))
                    (shot-refusal job)))
        (rendezvous (eq (autopilot-job-preset job) :rendezvous)))
    (cond ((and reason (or (not rendezvous) (eql 0 (search "NEEDS" reason))))
           (job-refuse! job reason))
          (reason (job-next-candidate! job reason))
          ((not rendezvous)
           (setf (autopilot-job-plan job) (shot-cut-plan (autopilot-job-candidate job)
                                                         (autopilot-job-shot job)
                                                         (autopilot-job-measure job))
                 (autopilot-job-stage job) :done))
          (t (multiple-value-bind (margin passes) (shot-path-margin job)
               (if (< margin 0d0)
                   (job-next-candidate! job "NO CLEAR TRANSFER")
                   (setf (autopilot-job-plan job)
                         (shot-cut-plan (autopilot-job-candidate job) (autopilot-job-shot job)
                                        :aim-point (job-rendezvous-part job passes)
                                        (autopilot-job-lead-cuts job))
                         (autopilot-job-stage job) :done)))))
    job))

(defun autopilot-job-work! (job budget)
  "At most BUDGET work units (the prediction's, decision 116) of JOB; its
stage: :coast, with a search :search (rendezvous-search-work!), :guess,
:iterate, :check, then :done (autopilot-job-plan) or :refused
(autopilot-job-reason)."
  (let ((left budget))
    (flet ((advance (pred)
             (prediction-extend! pred left)
             (decf left (prediction-last-work pred))
             (prediction-complete-p pred)))
      (loop
        (ecase (autopilot-job-stage job)
          ((:done :refused) (return))
          (:coast (cond ((not (advance (autopilot-job-coast job))) (return))
                        ((and (eq (autopilot-job-preset job) :rendezvous)
                              (null (autopilot-job-base job)))
                         (job-rendezvous-search! job))
                        (t (setf (autopilot-job-stage job)
                                 (if (autopilot-job-search job) :search :guess)))))
          (:search (multiple-value-bind (done units)
                       (rendezvous-search-advance! (autopilot-job-search job) left)
                     (decf left units)
                     (if done
                         (setf (autopilot-job-stage job) :guess)
                         (return))))
          (:guess (job-guess! job))
          (:lead (multiple-value-bind (done units) (job-lead-advance! job left)
                   (decf left units)
                   (if done (job-lead-done! job) (return))))
          (:retime (multiple-value-bind (done units) (job-retime-advance! job left)
                     (decf left units)
                     (if done (job-retime-done! job) (return))))
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
    (autopilot-job-start! job sim (burn-plan-preset plan) (burn-plan-goal plan)
                          :base plan :from (1+ (sim-plan-index sim)))))

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

;;; Rendezvous: the search (docs/design.md "The search"; decision 119,
;;; decision 59, decision 157: The rendezvous search as built: J2-secular
;;; coasts, the grid's aim points, phasing points, seeds, refinement,
;;; pruning, units). Conics on J2-secular rails: the ship's coasts, the
;;; target's and every other object's alike. A resumable job of items in a
;;; fixed order, each charged its work units (decision 116) and started only
;;; while its units fit the call's budget, so any slicing gives the same
;;; candidates.

(defparameter *rendezvous-limit* (* 48 3600d0) "s: the aim point within it of planning; decision 119.")
(defparameter *rendezvous-limit-step* (* 12 3600d0) "s; decision 119.")
(defparameter *rendezvous-limit-max* (* 240 3600d0) "s; decision 119.")
(defparameter *search-node-lead* 180d0
  "s after *plan-min-lead*: the first node, room for its hold, slew and half its burn.")
(defparameter *search-departures* 64
  "Departures per ship period, phasing points of the next revolution; decision 119.")
(defparameter *search-transfers* '(0.4d0 1.5d0 32) "Transfer times: target periods from, to, steps; decision 119.")
(defparameter *search-close-transfers* '(0.02d0 0.4d0 16) "The close grid's; decision 119.")
(defparameter *search-close-distance* 50d3 "m from the aim point: the close grid too; decision 119.")
(defparameter *phasing-max-revolutions* 32 "decision 119.")
(defparameter *phasing-apoapsis-max* 150000d3 "m from Earth's centre; decision 54.")
(defparameter *aim-bucket* 600d0 "s: the grid's aim point is rendezvous-aim's choice at each multiple.")
(defparameter *phasing-min-dv* 0.05d0 "m/s: a smaller phasing burn keeps the ship's orbit, A's.")
(defparameter *scan-samples* 256 "Per coast; decision 119.")
(defparameter *scan-chunk* 8 "Samples an item scans.")
(defparameter *scan-refine-distance* 10d3 "m: a sampled minimum under it is refined; decision 119.")
(defparameter *scan-time-tolerance* 1d-2 "s: a refined minimum's golden section.")
(defparameter *search-seeds* 3 "Refined per group; decision 119.")
(defparameter *search-pool* 64 "Grid points kept per group, the least Δv first, for its seeds.")
(defparameter *search-seed-scans* 24 "Clearance scans a group spends finding its seeds.")
(defparameter *search-time-tolerance* 1d0 "s: the golden section of departure and transfer time.")
(defparameter *search-boundary-steps* 6 "Bisections from an unclear optimum back to its clear seed.")
(defparameter *search-candidates* 8 "Clear candidates kept for the shooting; decision 119.")
(defparameter *search-tie-dv* 1d0 "m/s: within it of the least the earlier arrival wins; decision 119.")
(defparameter *search-group-margin* 0.05d0
  "A group whose grid least lies this fraction over the 8th candidate is not refined.")
;; Work units (decision 116), measured against a coast step (0.65 µs,
;; decision 149): docs/architecture.md "Trajectory prediction".
(defparameter *lambert-work* 1 "Units per grid point: its Lambert solve and aim point.")
(defparameter *scan-evals-per-unit* 4 "Rails evaluations per unit.")
(defparameter *aim-work* 12 "Units per exact evaluation: aim-point and coast-lambert-into!.")
(defparameter *aim-bucket-work* 40 "Units per aim bucket (rendezvous-aim).")
(defparameter *phasing-point-work* 8 "Units per phasing point.")
(declaim (type double-float *rendezvous-limit* *rendezvous-limit-step* *rendezvous-limit-max*
               *search-node-lead* *search-close-distance* *phasing-apoapsis-max* *aim-bucket*
               *phasing-min-dv*
               *scan-refine-distance* *scan-time-tolerance* *search-time-tolerance*
               *search-tie-dv* *search-group-margin*)
         (type fixnum *search-departures* *phasing-max-revolutions* *scan-samples* *scan-chunk*
               *search-seeds* *search-pool* *search-seed-scans* *search-boundary-steps*
               *search-candidates* *lambert-work* *scan-evals-per-unit* *aim-work*
               *aim-bucket-work* *phasing-point-work*))

(defconstant +golden+ (/ (- (sqrt 5d0) 1) 2) "1/φ.")

(defvar *coast-j2* t
  "The search's coasts drift with J2's secular rates: nil when its sim's model is :point.")

;; decision 159: The rendezvous search's coasts at the sim's mean motion.
(defvar *coast-delta* nil
  "m: a search's short-period a term (the snapshot's osculating a less its
mean, j2-mean-semi-major-axis), kept by every coast it derives from its own
rails' states, which are no osculating states; nil: from the state itself.")

(defun coast-rails (r v time)
  "The rails of the search's coast from R, V at TIME about Earth
(state-rails): J2's secular rates (*coast-j2*) at the mean motion the
integrated motion keeps (the osculating a less *coast-delta*), so the ship
drifts as the sim flies it (the osculating a's mean motion is off by up to
0.2 %: ~1,000 km in 19 h at 400 km)."
  (state-rails r v time :j2 *coast-j2* :delta *coast-delta*))

(defun state-coast-delta (r v)
  "m: the short-period a term of the integrated state R, V (*coast-delta*'s)."
  (- (orbit-a (state->orbit r v +mu-earth+ 0d0)) (j2-mean-semi-major-axis r v)))

(defun model-coast-j2 (sim)
  "*coast-j2* for a search from SIM: a :point sim's ship coasts on conics."
  (not (eq (sim-model sim) :point)))

(defun rails-drift-rates (rails)
  "(values u̇ Ω̇) rad/s: J2's secular drift of RAILS' argument of latitude
(ω̇ + Ṁ − n) and node against its conic."
  (let ((segment (first rails)))
    (values (+ (rails-segment-argp-rate segment)
               (- (rails-segment-mean-rate segment) (orbit-n (rails-segment-orbit segment))))
            (rails-segment-raan-rate segment))))

(declaim (inline rotate-into!))
(defun rotate-into! (out v axis angle)
  "OUT = V turned by ANGLE about the unit AXIS (Rodrigues); OUT may be V."
  (declare (type vec3 out v axis) (type double-float angle))
  (let* ((c (cos angle)) (s (sin angle))
         (d (* (vec3-dot axis v) (- 1 c))))
    (with-vec3 (((x y z) v) ((ax ay az) axis))
      (vec3-set! out
                 (+ (* x c) (* s (- (* ay z) (* az y))) (* ax d))
                 (+ (* y c) (* s (- (* az x) (* ax z))) (* ay d))
                 (+ (* z c) (* s (- (* ax y) (* ay x))) (* az d))))))

(declaim (inline drift-into!))
(defun drift-into! (out v normal du dnode)
  "OUT = V turned by DU about NORMAL, then by DNODE about z: a conic's
state moved on as its rails' drift."
  (declare (type vec3 out v normal) (type double-float du dnode))
  (rotate-into! out v normal du)
  (unless (zerop dnode)
    (rotate-into! out out (load-time-value (vec3 0 0 1) t) dnode))
  out)

(defun rails-radii (rails)
  "(values q Q) of RAILS' first conic, Q infinite for a hyperbola."
  (let ((orbit (rails-segment-orbit (first rails))))
    (values (orbit-q orbit) (or (orbit-apoapsis-radius orbit) most-positive-double-float))))

(declaim (sb-ext:maybe-inline arc-below-p))
(defun arc-below-p (r1 v1 r2 v2 floor)
  "The conic arc from R1, V1 to R2, V2, prograde and under one revolution,
passes its periapsis below the radius FLOOR."
  (declare (type vec3 r1 v1 r2 v2) (type double-float floor))
  (let* ((mu +mu-earth+)
         (h (with-vec3 (((ax ay az) r1) ((bx by bz) v1))
              (sqrt (+ (expt (- (* ay bz) (* az by)) 2) (expt (- (* az bx) (* ax bz)) 2)
                       (expt (- (* ax by) (* ay bx)) 2)))))
         (p (/ (* h h) mu))
         (n1 (vec3-norm r1)) (n2 (vec3-norm r2)))
    (flet ((anomaly (r v n)
             ;; e cos ν = p / r − 1, e sin ν = (r · v) h / (μ r)
             (let ((nu (atan (/ (* (vec3-dot r v) h) (* mu n)) (- (/ p n) 1))))
               (if (minusp nu) (+ nu +two-pi+) nu))))
      (let* ((nu1 (anomaly r1 v1 n1))
             (nu2 (anomaly r2 v2 n2))
             (e-cos (- (/ p n1) 1))
             (e-sin (/ (* (vec3-dot r1 v1) h) (* mu n1)))
             (e (sqrt (+ (* e-cos e-cos) (* e-sin e-sin)))))
        (and (< (/ p (+ 1 e)) floor)
             (>= (+ nu1 (mod (- nu2 nu1) +two-pi+)) +two-pi+))))))

;;; The objects the search clears and its arcs' scan

(defstruct (search-object (:constructor make-search-object (key rails clearance target q big-q))
                          (:copier nil))
  (key nil :type keyword :read-only t)
  (rails nil :type list :read-only t)           ; planning rails (object-planning-rails)
  (clearance 0d0 :type double-float :read-only t) ; m: its bound + *object-clearance*
  (target nil :type boolean :read-only t)
  (q 0d0 :type double-float :read-only t)       ; its conic's radii
  (big-q 0d0 :type double-float :read-only t))

(defvar *search-ignored* '()
  "Object keys the search's Kepler filter leaves out, so only the shot-path
check sees them (tests of its fallback).")

(defun search-objects (sim target)
  "The objects a rendezvous with TARGET from SIM must clear, the target first:
neither removed nor latched nor in *search-ignored*, on their planning rails."
  (let ((object (target-object sim target))
        (out '()))
    (loop for other across (sim-objects sim)
          unless (or (member (object-status other) '(:removed :latched))
                     (member (object-key other) *search-ignored*))
            do (let ((rails (object-planning-rails other sim)))
                 (multiple-value-bind (q big-q) (rails-radii rails)
                   (let ((entry (make-search-object (object-key other) rails
                                                    (+ (object-def-bound (object-def-of other))
                                                       *object-clearance*)
                                                    (eq other object) q big-q)))
                     (if (eq other object)
                         (setf out (cons entry out))
                         (setf out (append out (list entry))))))))
    (coerce out 'simple-vector)))

(defstruct (clearance-scan (:constructor %make-clearance-scan (coasts objects least last1 last2))
                           (:copier nil))
  (coasts #() :type simple-vector)              ; per coast (start end rails)
  (objects #() :type simple-vector)             ; the search-objects near its radii
  (coast 0 :type fixnum)
  (sample 0 :type fixnum)                       ; the coast's next sample
  (least nil :type (simple-array double-float (*))) ; m per object, CoM to CoM
  (last1 nil :type (simple-array double-float (*))) ; at the sample before
  (last2 nil :type (simple-array double-float (*))) ; and the one before it
  (pending '() :type list))                     ; (object coast lo hi) minima to refine

(defun make-clearance-scan (coasts objects)
  "The scan of the arc COASTS (a list of (start end rails)) against the
OBJECTS whose radii come within *scan-refine-distance* and their clearance
of a coast's."
  (let* ((near (remove-if-not
                (lambda (object)
                  (some (lambda (coast)
                          (multiple-value-bind (q big-q) (rails-radii (third coast))
                            (let ((pad (+ *scan-refine-distance* (search-object-clearance object))))
                              (and (<= (- q pad) (search-object-big-q object))
                                   (<= (search-object-q object) (+ big-q pad))))))
                        coasts))
                (coerce objects 'list)))
         (n (length near)))
    (flet ((doubles () (make-array n :element-type 'double-float
                                     :initial-element most-positive-double-float)))
      (%make-clearance-scan (coerce coasts 'simple-vector) (coerce near 'simple-vector)
                            (doubles) (doubles) (doubles)))))

(defun scan-done-p (scan)
  (and (null (clearance-scan-pending scan))
       (>= (clearance-scan-coast scan) (length (clearance-scan-coasts scan)))))

(defun scan-margin (scan)
  "m to spare: the least over the objects of the distance less the clearance."
  (loop for object across (clearance-scan-objects scan)
        for least across (clearance-scan-least scan)
        minimize (- least (search-object-clearance object)) into margin
        finally (return (if (zerop (length (clearance-scan-objects scan)))
                            most-positive-double-float
                            margin))))

(defun scan-target-distance (scan)
  "m: the least distance from the target's CoM, or nil (none near)."
  (let ((i (position-if #'search-object-target (clearance-scan-objects scan))))
    (and i (aref (clearance-scan-least scan) i))))

(defun scan-other-distance (scan)
  "m: the least distance from another object's CoM, or nil (none near)."
  (loop for object across (clearance-scan-objects scan)
        for least across (clearance-scan-least scan)
        unless (search-object-target object) minimize least into m and count t into n
        finally (return (and (plusp n) m))))

(declaim (inline scan-object-distance scan-coast-time))
(defun scan-object-distance (object ship-r time)
  "m from SHIP-R to OBJECT at TIME. Conses nothing."
  (declare (type search-object object) (type vec3 ship-r) (type double-float time)
           (inline rails-state-into!))
  (let ((r (make-vec3)) (v (make-vec3)))
    (declare (dynamic-extent r v))
    (rails-state-into! r v (rails-segment-at (search-object-rails object) time) time)
    (vec3-norm (vec3-sub! r r ship-r))))

(defun scan-coast-time (coast i)
  (declare (type list coast) (type fixnum i))
  (let ((start (first coast)) (end (second coast)))
    (declare (type double-float start end))
    (+ start (* (- end start) (/ (float i 1d0) *scan-samples*)))))

(defun golden-steps (width tolerance)
  "Golden-section steps that shrink WIDTH below TOLERANCE."
  (if (<= width tolerance)
      0
      (ceiling (log (/ width tolerance)) (log (/ +golden+)))))

(defun scan-item-units (scan)
  "The units of SCAN's next item."
  (let ((pending (first (clearance-scan-pending scan))))
    (ceiling (if pending
                 (* 2 (+ 2 (golden-steps (- (fourth pending) (third pending)) *scan-time-tolerance*)))
                 (* *scan-chunk* (1+ (length (clearance-scan-objects scan)))))
             *scan-evals-per-unit*)))

(defun scan-refine! (scan object-index coast lo hi)
  "The least distance of object OBJECT-INDEX over [LO, HI] on COAST by golden section."
  (let* ((object (svref (clearance-scan-objects scan) object-index))
         (rails (third coast))
         (r (make-vec3)) (v (make-vec3)))
    (flet ((f (time)
             (rails-state-into! r v (first rails) time)
             (scan-object-distance object r time)))
      (let* ((a lo) (b hi)
             (c (- b (* +golden+ (- b a)))) (d (+ a (* +golden+ (- b a))))
             (fc (f c)) (fd (f d)))
        (dotimes (i (golden-steps (- hi lo) *scan-time-tolerance*))
          (if (< fc fd)
              (setf b d d c fd fc c (- b (* +golden+ (- b a))) fc (f c))
              (setf a c c d fc fd d (+ a (* +golden+ (- b a))) fd (f d))))
        (let ((least (clearance-scan-least scan)))
          (setf (aref least object-index) (min (aref least object-index) fc fd)))))))

(defun scan-item! (scan)
  "SCAN's next item: a pending minimum refined, else the next *scan-chunk*
samples of the coast, each local minimum under *scan-refine-distance*
queued; its units."
  (declare (optimize speed) (type clearance-scan scan) (inline rails-state-into!))
  (let ((units (scan-item-units scan))
        (pending (pop (clearance-scan-pending scan))))
    (if pending
        (destructuring-bind (index coast lo hi) pending
          (scan-refine! scan index coast lo hi))
        (let* ((coast (svref (clearance-scan-coasts scan) (clearance-scan-coast scan)))
               (objects (clearance-scan-objects scan))
               (least (clearance-scan-least scan))
               (last1 (clearance-scan-last1 scan))
               (last2 (clearance-scan-last2 scan))
               (r (make-vec3)) (v (make-vec3)))
          (declare (dynamic-extent r v))
          (loop repeat *scan-chunk*
                for i of-type fixnum = (clearance-scan-sample scan)
                while (<= i *scan-samples*)
                do (let ((time (scan-coast-time coast i)))
                     (rails-state-into! r v (first (third coast)) time)
                     (dotimes (k (length objects))
                       (let ((d (scan-object-distance (svref objects k) r time)))
                         (setf (aref least k) (min (aref least k) d))
                         (when (and (>= i 2) (< (aref last1 k) (aref last2 k))
                                    (<= (aref last1 k) d) (< (aref last1 k) *scan-refine-distance*))
                           (setf (clearance-scan-pending scan)
                                 (append (clearance-scan-pending scan)
                                         (list (list k coast (scan-coast-time coast (- i 2)) time)))))
                         (setf (aref last2 k) (if (= i 0) most-positive-double-float (aref last1 k))
                               (aref last1 k) d))))
                   (incf (clearance-scan-sample scan)))
          (when (> (clearance-scan-sample scan) *scan-samples*)
            (setf (clearance-scan-sample scan) 0)
            (incf (clearance-scan-coast scan)))))
    units))

;;; The search's state

(defstruct (rendezvous-impulse (:constructor make-rendezvous-impulse (label time r v dv))
                               (:copier nil))
  (label nil :type keyword :read-only t)        ; :phasing, :departure, :arrival
  (time 0d0 :type double-float :read-only t)    ; the node
  (r nil :type vec3 :read-only t)               ; the state there before it
  (v nil :type vec3 :read-only t)
  (dv nil :type vec3 :read-only t))

(defstruct (rendezvous-candidate (:constructor make-rendezvous-candidate) (:copier nil))
  (family :a :type (member :a :b) :read-only t)
  (revolutions 0 :type fixnum :read-only t)     ; B: N
  (direction nil :type (member nil :catch-up :fall-back) :read-only t)
  (impulses #() :type simple-vector :read-only t)
  (arrival 0d0 :type double-float :read-only t) ; the aim point's t
  (aim 1 :type fixnum :read-only t)             ; its number, (1)–(3)
  (side 1 :type fixnum :read-only t)            ; (3)'s side
  (aim-r nil :type vec3 :read-only t)
  (aim-v nil :type vec3 :read-only t)
  (total 0d0 :type double-float :read-only t)   ; m/s: every impulse and a :plane's
  (margin 0d0 :type double-float :read-only t)  ; m the arc keeps beyond the clearances
  (target-distance nil :type (or null double-float) :read-only t) ; m from the target's CoM
  (other-distance nil :type (or null double-float) :read-only t)) ; from any other's, if near

(defstruct (search-group (:constructor make-search-group) (:copier nil))
  (family :a :type (member :a :b))
  (revolutions 0 :type fixnum)
  (direction nil)
  (phasing nil)                                 ; B: the phasing impulse
  (cost 0d0 :type double-float)                 ; m/s: the phasing's and a :plane's
  (rails nil :type list)                        ; the coast the departures leave from
  (drift 0d0 :type double-float)                ; its rails' u̇ and Ω̇ (rails-drift-rates)
  (node-drift 0d0 :type double-float)
  (bound 0d0 :type double-float)                ; m/s its totals are at least
  (first-departure 0d0 :type double-float)
  (step 0d0 :type double-float)
  (count 0 :type fixnum)
  (min-departure 0d0 :type double-float)
  (pool-count 0 :type fixnum)                   ; the least-Δv grid points, sorted
  (pool-total (make-array *search-pool* :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))
  (pool-departure (make-array *search-pool* :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))
  (pool-tof (make-array *search-pool* :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))
  (pool-k (make-array *search-pool* :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (pool-j (make-array *search-pool* :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (walk 0 :type fixnum)                         ; the next pool entry for a seed
  (scans 0 :type fixnum)
  (seeds '() :type list))                       ; (pool-index total departure tof aim side margin distances)

(defstruct (search-refine (:constructor make-search-refine) (:copier nil))
  "One seed's refinement: golden sections in departure then transfer time on
Δv alone, the optimum scanned, bisected back towards the seed while unclear,
the aim point checked at its arrival."
  (phase :departure :type (member :departure :tof :scan :boundary :aim :rescan))
  (aim 1 :type fixnum) (side 1 :type fixnum)
  (a 0d0 :type double-float) (b 0d0 :type double-float)
  (c 0d0 :type double-float) (d 0d0 :type double-float)
  (fc 0d0 :type double-float) (fd 0d0 :type double-float)
  (next :c :type (member :c :d :step))          ; the golden point to evaluate next
  (best-departure 0d0 :type double-float) (best-tof 0d0 :type double-float)
  (best-total 0d0 :type double-float)
  (clear-departure 0d0 :type double-float) (clear-tof 0d0 :type double-float) ; known clear
  (clear-margin 0d0 :type double-float)
  (clear-distance nil :type list)                ; (target-distance other-distance)
  (far-departure 0d0 :type double-float) (far-tof 0d0 :type double-float) ; known unclear
  (steps 0 :type fixnum)
  (probe-departure 0d0 :type double-float) (probe-tof 0d0 :type double-float))

(defstruct (rendezvous-search (:constructor %make-rendezvous-search) (:copier nil))
  (sim nil)                                     ; the snapshot, read only
  (target nil) (fixture nil) (range 100d0 :type double-float)
  (ship-rails nil :type list)                   ; the ship's coast before its first burn
  (normal nil :type (or null vec3))             ; Lambert's prograde sense
  (delta nil :type (or null double-float))      ; its coasts' *coast-delta*
  (ship-period 0d0 :type double-float)
  (earliest 0d0 :type double-float)             ; the first node
  (deadline 0d0 :type double-float)             ; the latest aim point
  (plane-dv 0d0 :type double-float)
  (target-rails nil :type list) (target-shift 0d0 :type double-float)
  (target-period 0d0 :type double-float)
  (transfers nil :type (or null (simple-array double-float (*)))) ; the grid's, close ones after
  (main-transfers 0 :type fixnum)
  (objects #() :type simple-vector)
  (aim-numbers #() :type simple-vector)         ; per bucket: (number side) or nil
  (aim-offsets nil :type (or null (simple-array double-float (*)))) ; per bucket 6, LVLH
  (phasing-best nil :type (or null simple-vector)) ; per (N, direction): (cost k period)
  (stage :done :type (member :aims :phasing :group :grid :seeds :refine :finish :done))
  (cursor 0 :type fixnum)
  (groups '() :type list)                       ; left to search, the current first
  (refine nil :type (or null search-refine))
  (scan nil :type (or null clearance-scan))
  (scan-key nil)                                ; what the scan is for
  (candidates '() :type list)                   ; accepted, in the order found
  (result #() :type simple-vector)              ; :done: the best 8 in order
  (work 0 :type fixnum))                        ; units in the last call

(defun search-floor () (+ (body-crash-radius (body :earth)) *plan-floor-margin*))

(declaim (sb-ext:maybe-inline search-target-into! search-aim-into! pool-insert!))
(defun search-target-into! (r v search time)
  "The target's state (what the stop point moves with) at TIME on the
search's planning rails. Conses nothing."
  (declare (type vec3 r v) (type double-float time))
  (let ((time (+ time (rendezvous-search-target-shift search))))
    (rails-state-into! r v (rails-segment-at (rendezvous-search-target-rails search) time) time)))

(defun search-aim-into! (r v search time)
  "The grid's aim point at TIME into R, V: its bucket's number, (1) exactly
on the target's rails, (2) and (3) by the bucket's offsets in the target's
LVLH axes; nil when the bucket has no clear approach. Conses nothing."
  (declare (type vec3 r v) (type double-float time))
  (let* ((numbers (rendezvous-search-aim-numbers search))
         (bucket (max 0 (min (1- (length numbers))
                             (floor (- time (rendezvous-search-earliest search)) *aim-bucket*))))
         (entry (svref numbers bucket)))
    (when entry
      (if (= (first entry) 1)
          ;; the bucket's target speed for the shift
          (search-target-into! r v search (- time (/ *aim-distance*
                                                     (aref (rendezvous-search-aim-offsets search)
                                                           (* 6 bucket)))))
          (let ((offsets (rendezvous-search-aim-offsets search))
                (ta (make-vec3)) (na (make-vec3)) (ra (make-vec3)))
            (declare (dynamic-extent ta na ra))
            (search-target-into! r v search time)
            (vec3-normalize! na (vec3-cross! na r v))
            (vec3-normalize! ra r)
            (vec3-cross! ta na ra)
            (let ((k (* 6 bucket)))
              (vec3-add-scaled! r r ta (aref offsets k))
              (vec3-add-scaled! r r na (aref offsets (+ k 1)))
              (vec3-add-scaled! r r ra (aref offsets (+ k 2)))
              (vec3-add-scaled! v v ta (aref offsets (+ k 3)))
              (vec3-add-scaled! v v na (aref offsets (+ k 4)))
              (vec3-add-scaled! v v ra (aref offsets (+ k 5))))))
      t)))

(defun periapsis-grid-start (rails earliest step)
  "The first time at or after EARLIEST on a grid of STEP through RAILS'
periapsis passage: an eccentric coast's departures include its periapsis."
  (let ((tp (orbit-tp (rails-segment-orbit (first rails)))))
    (+ tp (* step (ceiling (- earliest tp) step)))))

(defun transfer-grid (period close)
  "Transfer times (s) for a target of PERIOD: *search-transfers*' steps, then
with CLOSE *search-close-transfers*'."
  (flet ((steps (spec)
           (destructuring-bind (from to n) spec
             (loop for j below n collect (* period (+ from (* (- to from) (/ j (float (1- n) 1d0)))))))))
    (coerce (append (steps *search-transfers*) (and close (steps *search-close-transfers*)))
            '(simple-array double-float (*)))))

(defun rendezvous-search-start! (search sim target fixture range
                                 &key r v time earliest (limit *rendezvous-limit*) (plane-dv 0d0))
  "SEARCH for a rendezvous with TARGET at FIXTURE (rendezvous-stop's) and
RANGE from SIM, its aim point within LIMIT s of SIM's t: the ship coasting
from R, V at TIME (default SIM's ship now; after a :plane burn its cut),
its first node at EARLIEST or later (default SIM's t + *plan-min-lead* +
*search-node-lead*), PLANE-DV (m/s) added to every total. SEARCH."
  (let* ((ship (sim-ship sim))
         (r (or r (vec3-copy (ship-state-r ship))))
         (v (or v (vec3-copy (ship-state-v ship))))
         (time (float (or time (sim-t sim)) 1d0))
         (delta (and (model-coast-j2 sim)
                     (elliptic-p (state->orbit (ship-state-r ship) (ship-state-v ship) +mu-earth+ 0d0))
                     (state-coast-delta (ship-state-r ship) (ship-state-v ship))))
         (ship-rails (let ((*coast-j2* (model-coast-j2 sim)) (*coast-delta* delta))
                       (coast-rails r v time)))
         (object (target-object sim target))
         (target-rails (if object
                           (object-planning-rails object sim)
                           (place-rails (place target))))
         (target-period (orbit-period (rails-segment-orbit (first target-rails))))
         (earliest (float (or earliest (+ (sim-t sim) *plan-min-lead* *search-node-lead*)) 1d0))
         (deadline (+ (sim-t sim) (float limit 1d0)))
         (buckets (1+ (max 0 (floor (- deadline earliest) *aim-bucket*)))))
    (setf (rendezvous-search-sim search) sim
          (rendezvous-search-target search) target
          (rendezvous-search-fixture search) fixture
          (rendezvous-search-range search) (float range 1d0)
          (rendezvous-search-ship-rails search) ship-rails
          (rendezvous-search-normal search) (vec3-normalize (vec3-cross r v))
          (rendezvous-search-delta search) delta
          (rendezvous-search-ship-period search)
          (or (orbit-period (rails-segment-orbit (first ship-rails))) 0d0)
          (rendezvous-search-earliest search) earliest
          (rendezvous-search-deadline search) deadline
          (rendezvous-search-plane-dv search) (float plane-dv 1d0)
          (rendezvous-search-target-rails search) target-rails
          (rendezvous-search-target-shift search) (if (eq fixture :front) *yard-front-lead* 0d0)
          (rendezvous-search-target-period search) (or target-period 0d0)
          (rendezvous-search-transfers search) (transfer-grid (or target-period 0d0) t)
          (rendezvous-search-main-transfers search) (third *search-transfers*)
          (rendezvous-search-objects search) (search-objects sim target)
          (rendezvous-search-aim-numbers search) (make-array buckets :initial-element nil)
          (rendezvous-search-aim-offsets search)
          (make-array (* 6 buckets) :element-type 'double-float :initial-element 0d0)
          (rendezvous-search-phasing-best search)
          (make-array (* 2 *phasing-max-revolutions*) :initial-element nil)
          (rendezvous-search-stage search) (if (and target-period
                                                     (> (rendezvous-search-ship-period search) 0d0))
                                                :aims
                                                :finish)
          (rendezvous-search-cursor search) 0
          (rendezvous-search-groups search) '()
          (rendezvous-search-refine search) nil
          (rendezvous-search-scan search) nil
          (rendezvous-search-candidates search) '()
          (rendezvous-search-result search) #()
          (rendezvous-search-work search) 0)
    search))

(defun new-rendezvous-search () (%make-rendezvous-search))

;;; Stage :aims: rendezvous-aim at each bucket, its offsets in LVLH

(defun search-aim-bucket! (search i)
  (let* ((time (+ (rendezvous-search-earliest search) (* i *aim-bucket*)))
         (sim (rendezvous-search-sim search)))
    (multiple-value-bind (number r v side)
        (rendezvous-aim sim (rendezvous-search-target search) (rendezvous-search-fixture search)
                        (rendezvous-search-range search) time)
      (when number
        (let ((rt (make-vec3)) (vt (make-vec3))
              (offsets (rendezvous-search-aim-offsets search))
              (k (* 6 i)))
          (search-target-into! rt vt search time)
          (multiple-value-bind (ta na ra) (lvlh-axes rt vt)
            (let ((dr (vec3-sub r rt)) (dv (vec3-sub v vt)))
              ;; (1): the target's speed for its shift; (2), (3): the offsets
              (setf (aref offsets k) (if (= number 1) (vec3-norm vt) (vec3-dot dr ta))
                    (aref offsets (+ k 1)) (vec3-dot dr na)
                    (aref offsets (+ k 2)) (vec3-dot dr ra)
                    (aref offsets (+ k 3)) (vec3-dot dv ta)
                    (aref offsets (+ k 4)) (vec3-dot dv na)
                    (aref offsets (+ k 5)) (vec3-dot dv ra))))
          (setf (svref (rendezvous-search-aim-numbers search) i) (list number side)))))))

;;; Stage :phasing: per phasing point, the cheapest one of each (N, direction)

(defun target-lead (search r time)
  "s: how long ago the target passed the direction of R along its orbit, in [0, T_t)."
  (let ((rt (make-vec3)) (vt (make-vec3)))
    (search-target-into! rt vt search time)
    (let* ((orbit (state->orbit rt vt +mu-earth+ time))
           (e (orbit-e orbit)))
      (multiple-value-bind (p q) (perifocal-axes (orbit-i orbit) (orbit-raan orbit) (orbit-argp orbit))
        (let* ((theta (atan (vec3-dot r q) (vec3-dot r p)))
               (ea (true->eccentric theta e))
               (m (- ea (* e (sin ea))))
               (n (orbit-n orbit)))
          (/ (mod (- (orbit-mean-anomaly orbit time) m) +two-pi+) n))))))

(defun rails-period (rails)
  "s: one revolution in argument of latitude on RAILS' first segment (its
mean motion and ω̇), as the search's coasts and the objects move."
  (let ((segment (first rails)))
    (/ +two-pi+ (+ (rails-segment-mean-rate segment) (rails-segment-argp-rate segment)))))

(defun phasing-speed (r v period)
  "m/s at R along V whose coast (coast-rails) revolves in PERIOD s, or nil:
the Kepler a of PERIOD, corrected three times by the coast's own rate (a ∝
T^(2/3))."
  (let* ((rn (vec3-norm r))
         (vn (vec3-norm v))
         (a (expt (* +mu-earth+ (expt (/ period +two-pi+) 2)) 1/3)))
    (flet ((speed (a)
             (let ((s2 (* +mu-earth+ (- (/ 2 rn) (/ a)))))
               (and (> s2 0d0) (sqrt s2)))))
      (dotimes (i 3 (speed a))
        (let ((s (speed a)))
          (unless s (return nil))
          (let ((coast (rails-period (coast-rails r (vec3-scale v (/ s vn)) 0d0))))
            (unless (> coast 0d0) (return nil))
            (setf a (* a (expt (/ period coast) 2/3)))))))))

(defun phasing-orbit-ok-p (r v-new)
  "The phasing orbit from R, V-NEW is bound, its periapsis above the floor,
its apoapsis within *phasing-apoapsis-max*."
  (let ((orbit (state->orbit r v-new +mu-earth+ 0d0)))
    (and (elliptic-p orbit)
         (>= (orbit-q orbit) (search-floor))
         (<= (orbit-apoapsis-radius orbit) *phasing-apoapsis-max*))))

(defun search-phasing-point! (search k)
  "Phasing point K: for each N and direction its phasing burn's Δv, kept per
(N, direction) where least (ties to the earlier point)."
  (let* ((period (rendezvous-search-ship-period search))
         (tt (rails-period (rendezvous-search-target-rails search)))
         (step (/ period *search-departures*))
         (time (+ (periapsis-grid-start (rendezvous-search-ship-rails search)
                                        (rendezvous-search-earliest search) step)
                  (* k step)))
         (deadline (rendezvous-search-deadline search))
         (best (rendezvous-search-phasing-best search))
         (r (make-vec3)) (v (make-vec3)))
    (rails-state-into! r v (first (rendezvous-search-ship-rails search)) time)
    (let ((lead (target-lead search r time))
          (vn (vec3-norm v)))
      (loop for n from 1 to *phasing-max-revolutions*
            do (loop for direction in '(:catch-up :fall-back)
                     for d from 0
                     for t-ph = (if (eq direction :catch-up)
                                    (- tt (/ lead n))
                                    (+ tt (/ (- tt lead) n)))
                     do (when (and (> t-ph 0d0)
                                   (<= (+ time (* n t-ph)) deadline)
                                   (<= (+ time (* (- n 0.5d0) t-ph) (* (first *search-close-transfers*) tt))
                                       deadline))
                          (let ((speed (phasing-speed r v t-ph)))
                            (when speed
                              (let* ((v-new (vec3-scale v (/ speed vn)))
                                     (cost (abs (- speed vn)))
                                     (i (+ (* 2 (1- n)) d))
                                     (old (svref best i)))
                                (when (and (or (null old) (< cost (first old)))
                                           (phasing-orbit-ok-p r v-new))
                                  (setf (svref best i)
                                        (list cost time t-ph (vec3-copy r) (vec3-copy v) v-new))))))))))))

(defun energy-bound (rails target-rails)
  "m/s at least to go from RAILS' conic to TARGET-RAILS': the energy step
over the escape speed at the lower periapsis (Δε = v · Δv)."
  (let ((one (rails-segment-orbit (first rails)))
        (two (rails-segment-orbit (first target-rails))))
    (/ (abs (- (orbit-energy one) (orbit-energy two)))
       (sqrt (/ (* 2 +mu-earth+) (min (orbit-q one) (orbit-q two)))))))

(defun make-coast-group (rails &rest keys)
  "A search-group leaving from RAILS, its drift rates set."
  (multiple-value-bind (du dnode) (rails-drift-rates rails)
    (apply #'make-search-group :rails rails :drift du :node-drift dnode keys)))

(defun search-groups (search)
  "The groups to search: A, then B per (N, direction) by their least total,
the phasing Δv and energy-bound's (decision 157)."
  (let* ((plane (rendezvous-search-plane-dv search))
         (period (rendezvous-search-ship-period search))
         (earliest (rendezvous-search-earliest search))
         (deadline (rendezvous-search-deadline search))
         (tt (rendezvous-search-target-period search))
         (target-rails (rendezvous-search-target-rails search))
         (step (/ period *search-departures*))
         (ship-rails (rendezvous-search-ship-rails search))
         (first (periapsis-grid-start ship-rails earliest step))
         (a (make-coast-group ship-rails :family :a :cost plane
                                         :bound (+ plane (energy-bound ship-rails target-rails))
                                         :first-departure first :step step
                                         :count (max 0 (1+ (floor (- deadline first
                                                                     (* (first *search-close-transfers*) tt))
                                                                  step)))
                                         :min-departure earliest))
         ;; a (N, direction) whose cheapest phasing burn is under
         ;; *phasing-min-dv*: the ship's own orbit phases so, A's
         (bs (loop for entry across (rendezvous-search-phasing-best search)
                   for i from 0
                   when (and entry (>= (first entry) *phasing-min-dv*))
                     collect (destructuring-bind (cost time t-ph r v v-new) entry
                               (let* ((n (1+ (floor i 2)))
                                      (end (+ time (* n t-ph)))
                                      (rails (coast-rails r v-new time)))
                                 (make-coast-group
                                  rails
                                  :family :b :revolutions n
                                  :direction (if (evenp i) :catch-up :fall-back)
                                  :phasing (make-rendezvous-impulse :phasing time r v (vec3-sub v-new v))
                                  :cost (+ plane cost)
                                  :bound (+ plane cost (energy-bound rails target-rails))
                                  :first-departure (- end (/ t-ph 2))
                                  :step (/ t-ph *search-departures*)
                                  :count *search-departures*
                                  :min-departure (+ time (/ t-ph 2))))))))
    (cons a (stable-sort bs #'< :key #'search-group-bound))))

;;; Stage :grid: one departure an item

(defun pool-insert! (group total departure tof k j)
  "GROUP's pool of the least-Δv grid points: TOTAL's place, after equal ones."
  (declare (type search-group group) (type double-float total departure tof) (type fixnum k j))
  (let ((count (search-group-pool-count group))
        (totals (search-group-pool-total group))
        (departures (search-group-pool-departure group))
        (tofs (search-group-pool-tof group))
        (ks (search-group-pool-k group))
        (js (search-group-pool-j group)))
    (when (or (< count *search-pool*) (< total (aref totals (1- count))))
      (let ((i (min count (1- *search-pool*))))
        (loop while (and (> i 0) (< total (aref totals (1- i))))
              do (setf (aref totals i) (aref totals (1- i))
                       (aref departures i) (aref departures (1- i))
                       (aref tofs i) (aref tofs (1- i))
                       (aref ks i) (aref ks (1- i))
                       (aref js i) (aref js (1- i)))
                 (decf i))
        (setf (aref totals i) total
              (aref departures i) departure
              (aref tofs i) tof
              (aref ks i) k
              (aref js i) j
              (search-group-pool-count group) (min *search-pool* (1+ count)))))))

(defun search-grid-departure! (search group k)
  "Departure K of GROUP against every transfer time that fits (the close
ones within *search-close-distance* of the aim point): each a Lambert
solve to the grid's aim point, kept in the pool by its total Δv; its units."
  (declare (optimize speed) (type rendezvous-search search) (type search-group group) (type fixnum k)
           (inline search-aim-into! search-target-into! drift-into! lambert-into! arc-below-p
                   pool-insert! rails-state-into!))
  (let* ((t-d (+ (search-group-first-departure group) (* k (search-group-step group))))
         (deadline (rendezvous-search-deadline search))
         (transfers (rendezvous-search-transfers search))
         (normal (rendezvous-search-normal search))
         (floor (search-floor))
         (cost (search-group-cost group))
         (drift (search-group-drift group))
         (node-drift (search-group-node-drift group))
         (r-d (make-vec3)) (v-d (make-vec3)) (r-a (make-vec3)) (v-a (make-vec3))
         (v1 (make-vec3)) (v2 (make-vec3)) (d1 (make-vec3)) (d2 (make-vec3))
         (units 1))
    (declare (type double-float t-d deadline floor cost drift node-drift) (type fixnum units)
             (type (simple-array double-float (*)) transfers) (type vec3 normal)
             (dynamic-extent r-d v-d r-a v-a v1 v2 d1 d2))
    (rails-state-into! r-d v-d (first (search-group-rails group)) t-d)
    (let ((close (if (search-aim-into! r-a v-a search t-d)
                     (<= (vec3-norm (vec3-sub! r-a r-a r-d)) *search-close-distance*)
                     nil)))
      (loop for j of-type fixnum below (if close (length transfers) (rendezvous-search-main-transfers search))
            for tof of-type double-float = (aref transfers j)
            for t-a of-type double-float = (+ t-d tof)
            when (<= t-a deadline)
              do (incf units *lambert-work*)
                 ;; the Lambert end set back by the rails' drift over TOF
                 (when (and (search-aim-into! r-a v-a search t-a)
                            (drift-into! r-a r-a normal (- (* drift tof)) (- (* node-drift tof)))
                            (lambert-into! v1 v2 r-d r-a tof +mu-earth+ normal)
                            (not (arc-below-p r-d v1 r-a v2 floor)))
                   (drift-into! v2 v2 normal (* drift tof) (* node-drift tof))
                   (pool-insert! group
                                 (+ cost (vec3-norm (vec3-sub! d1 v1 v-d))
                                    (vec3-norm (vec3-sub! d2 v-a v2)))
                                 t-d tof k j))))
    units))

;;; Exact evaluations and their scans

(defparameter *coast-lambert-tolerance* 1d-3 "m: coast-lambert-into!'s end against its aim.")
(defparameter *coast-lambert-iterations* 4)

(defun coast-lambert-into! (v1 v2 r-d t-d r-a tof normal group)
  "V1 at R-D (T-D) of the conic whose rails (coast-rails) reach R-A TOF s
later, V2 their velocity there: Lambert to R-A set back by GROUP's drift,
then the end's miss on the rails added to the aim, *coast-lambert-iterations*
times at most; true, or nil (no solution, or the arc below the floor)."
  (let ((aim (make-vec3)) (r (make-vec3)) (miss (make-vec3)))
    (drift-into! aim r-a normal (- (* (search-group-drift group) tof))
                 (- (* (search-group-node-drift group) tof)))
    (dotimes (i *coast-lambert-iterations* t)
      (unless (and (lambert-into! v1 v2 r-d aim tof +mu-earth+ normal)
                   (not (arc-below-p r-d v1 aim v2 (search-floor))))
        (return nil))
      (let ((rails (coast-rails r-d v1 t-d)))
        (rails-state-into! r v2 (first rails) (+ t-d tof)))
      (vec3-sub! miss r-a r)
      (when (< (vec3-norm miss) *coast-lambert-tolerance*)
        (return t))
      (vec3-add! aim aim miss))))

(defun search-evaluate (search group t-d tof aim side)
  "The transfer leaving GROUP's coast at T-D for TOF s to aim point AIM
(SIDE) by aim-point: (values total impulses aim-r aim-v), or nil (outside
its times, no solution, below the floor)."
  (let ((t-a (+ t-d tof))
        (sim (rendezvous-search-sim search)))
    (when (and (> tof 0d0) (>= t-d (search-group-min-departure group))
               (<= t-a (rendezvous-search-deadline search)))
      (multiple-value-bind (r-a v-a)
          (aim-point sim (rendezvous-search-target search) (rendezvous-search-fixture search)
                     (rendezvous-search-range search) aim t-a side)
        (let ((r-d (make-vec3)) (v-d (make-vec3)) (v1 (make-vec3)) (v2 (make-vec3)))
          (rails-state-into! r-d v-d (first (search-group-rails group)) t-d)
          (when (coast-lambert-into! v1 v2 r-d t-d r-a tof (rendezvous-search-normal search) group)
            (let* ((dv1 (vec3-sub v1 v-d))
                   (dv2 (vec3-sub v-a v2))
                   (impulses (append (and (search-group-phasing group)
                                          (list (search-group-phasing group)))
                                     (list (make-rendezvous-impulse :departure t-d r-d v-d dv1)
                                           (make-rendezvous-impulse :arrival t-a (vec3-copy r-a) v2 dv2)))))
              (values (+ (search-group-cost group) (vec3-norm dv1) (vec3-norm dv2))
                      (coerce impulses 'simple-vector) r-a v-a))))))))

(defun impulses-scan (search impulses)
  "The clearance scan of the arc of IMPULSES: from the first to the last,
each coast on its rails (coast-rails) after its impulse."
  (make-clearance-scan
   (loop for (one two) on (coerce impulses 'list)
         while two
         collect (list (rendezvous-impulse-time one) (rendezvous-impulse-time two)
                       (coast-rails (rendezvous-impulse-r one)
                                     (vec3-add (rendezvous-impulse-v one) (rendezvous-impulse-dv one))
                                     (rendezvous-impulse-time one))))
   (rendezvous-search-objects search)))

(defun search-scan-start! (search key impulses)
  (setf (rendezvous-search-scan search) (impulses-scan search impulses)
        (rendezvous-search-scan-key search) key))

(defun search-scan-result (search key)
  "The finished scan for KEY: (values margin (target-distance
other-distance) t), cleared; nil if none."
  (let ((scan (rendezvous-search-scan search)))
    (when (and scan (equal key (rendezvous-search-scan-key search)) (scan-done-p scan))
      (setf (rendezvous-search-scan search) nil
            (rendezvous-search-scan-key search) nil)
      (values (scan-margin scan) (list (scan-target-distance scan) (scan-other-distance scan)) t))))

(defun search-bucket-aim (search time)
  "The bucket's (number side) at TIME, or nil."
  (let ((numbers (rendezvous-search-aim-numbers search)))
    (svref numbers (max 0 (min (1- (length numbers))
                               (floor (- time (rendezvous-search-earliest search)) *aim-bucket*))))))

;;; Stage :seeds: the pool walked in order, each clear point not beside a
;;; seed taken, up to *search-seeds*

(defun pool-beside-p (group i seed)
  (let ((ks (search-group-pool-k group)) (js (search-group-pool-j group))
        (main (third *search-transfers*)))
    (and (<= (abs (- (aref ks i) (aref ks seed))) 1)
         (<= (abs (- (aref js i) (aref js seed))) 1)
         (eq (< (aref js i) main) (< (aref js seed) main)))))

(defun search-seeds-item! (search group)
  "The next item of GROUP's seed walk; true when it is over."
  (let ((i (search-group-walk group)))
    (multiple-value-bind (margin distance scanned) (search-scan-result search (list :seed i))
      (when scanned
        (let ((t-d (aref (search-group-pool-departure group) i))
              (tof (aref (search-group-pool-tof group) i))
              (aim (search-bucket-aim search (+ (aref (search-group-pool-departure group) i)
                                                (aref (search-group-pool-tof group) i)))))
          (when (>= margin 0d0)
            (setf (search-group-seeds group)
                  (append (search-group-seeds group)
                          (list (list i (search-evaluate search group t-d tof (first aim) (second aim))
                                      t-d tof (first aim) (second aim) margin distance))))))
        (incf (search-group-scans group))
        (incf (search-group-walk group))
        (return-from search-seeds-item! nil)))
    ;; the next entry not beside a seed
    (loop while (and (< i (search-group-pool-count group))
                     (some (lambda (seed) (pool-beside-p group i (first seed))) (search-group-seeds group)))
          do (incf i))
    (setf (search-group-walk group) i)
    (if (or (>= i (search-group-pool-count group))
            (>= (length (search-group-seeds group)) *search-seeds*)
            (>= (search-group-scans group) *search-seed-scans*))
        t
        (let* ((t-d (aref (search-group-pool-departure group) i))
               (tof (aref (search-group-pool-tof group) i))
               (aim (search-bucket-aim search (+ t-d tof)))
               (impulses (and aim (nth-value 1 (search-evaluate search group t-d tof
                                                                (first aim) (second aim))))))
          (if impulses
              (search-scan-start! search (list :seed i) impulses)
              (incf (search-group-walk group)))
          nil))))

;;; Stage :refine: one seed after another

(defun search-refine-start (group seed)
  "SEED's refinement (a seed: (pool-index total departure tof aim side margin
distance)), its departure's golden section first."
  (destructuring-bind (i total t-d tof aim side margin distance) seed
    (declare (ignore i))
    (make-search-refine :phase :departure :aim aim :side side
                        :a (- t-d (search-group-step group)) :b (+ t-d (search-group-step group))
                        :next :c
                        :best-departure t-d :best-tof tof :best-total total
                        :clear-departure t-d :clear-tof tof :clear-margin margin
                        :clear-distance distance)))

(defun transfer-step (search tof)
  "s between TOF's grid neighbours (the close grid's below 0.4 periods)."
  (let ((tt (rendezvous-search-target-period search)))
    (flet ((spacing (spec) (destructuring-bind (from to n) spec (/ (* tt (- to from)) (1- n)))))
      (if (< tof (* tt (first *search-transfers*)))
          (spacing *search-close-transfers*)
          (spacing *search-transfers*)))))

(defun refine-golden-item! (search group refine)
  "One golden-section evaluation of REFINE's current dimension; at its end
the next phase."
  (let ((departure (eq (search-refine-phase refine) :departure)))
    (flet ((f (x)
             (let ((total (search-evaluate search group
                                           (if departure x (search-refine-best-departure refine))
                                           (if departure (search-refine-best-tof refine) x)
                                           (search-refine-aim refine) (search-refine-side refine))))
               (when (and total (< total (search-refine-best-total refine)))
                 (setf (search-refine-best-total refine) total)
                 (if departure
                     (setf (search-refine-best-departure refine) x)
                     (setf (search-refine-best-tof refine) x)))
               (or total most-positive-double-float))))
      (let ((a (search-refine-a refine)) (b (search-refine-b refine)))
        (ecase (search-refine-next refine)
          (:c (setf (search-refine-c refine) (- b (* +golden+ (- b a)))
                    (search-refine-fc refine) (f (search-refine-c refine))
                    (search-refine-next refine) :d))
          (:d (setf (search-refine-d refine) (+ a (* +golden+ (- b a)))
                    (search-refine-fd refine) (f (search-refine-d refine))
                    (search-refine-next refine) :step))
          (:step
           (if (< (search-refine-fc refine) (search-refine-fd refine))
               (setf (search-refine-b refine) (search-refine-d refine)
                     (search-refine-d refine) (search-refine-c refine)
                     (search-refine-fd refine) (search-refine-fc refine)
                     (search-refine-c refine) (- (search-refine-b refine)
                                                 (* +golden+ (- (search-refine-b refine) a)))
                     (search-refine-fc refine) (f (search-refine-c refine)))
               (setf (search-refine-a refine) (search-refine-c refine)
                     (search-refine-c refine) (search-refine-d refine)
                     (search-refine-fc refine) (search-refine-fd refine)
                     (search-refine-d refine) (+ (search-refine-a refine)
                                                 (* +golden+ (- b (search-refine-a refine))))
                     (search-refine-fd refine) (f (search-refine-d refine))))))
        (when (and (eq (search-refine-next refine) :step)
                   (< (- (search-refine-b refine) (search-refine-a refine)) *search-time-tolerance*))
          (if departure
              (let ((tof (search-refine-best-tof refine)))
                (setf (search-refine-phase refine) :tof
                      (search-refine-a refine) (- tof (transfer-step search tof))
                      (search-refine-b refine) (+ tof (transfer-step search tof))
                      (search-refine-next refine) :c))
              (setf (search-refine-phase refine) :scan)))))))

(defun refine-probe! (search group refine phase departure tof)
  "Evaluate REFINE's probe at DEPARTURE, TOF with its aim point and start
its scan for PHASE; nil (no scan) when infeasible."
  (setf (search-refine-probe-departure refine) departure
        (search-refine-probe-tof refine) tof)
  (let ((impulses (nth-value 1 (search-evaluate search group departure tof
                                                (search-refine-aim refine)
                                                (search-refine-side refine)))))
    (when impulses
      (search-scan-start! search (list phase) impulses)
      t)))

(defun search-accept! (search group refine)
  "REFINE's clear point as a candidate, unless one of the group lies within
*search-time-tolerance* of it."
  (let ((t-d (search-refine-clear-departure refine))
        (tof (search-refine-clear-tof refine)))
    (unless (find-if (lambda (c)
                       (and (eq (rendezvous-candidate-family c) (search-group-family group))
                            (= (rendezvous-candidate-revolutions c) (search-group-revolutions group))
                            (eq (rendezvous-candidate-direction c) (search-group-direction group))
                            (let ((departure (rendezvous-impulse-time
                                              (find :departure (rendezvous-candidate-impulses c)
                                                    :key #'rendezvous-impulse-label))))
                              (and (< (abs (- departure t-d)) *search-time-tolerance*)
                                   (< (abs (- (- (rendezvous-candidate-arrival c) departure) tof))
                                      *search-time-tolerance*)))))
                     (rendezvous-search-candidates search))
      (multiple-value-bind (total impulses r-a v-a)
          (search-evaluate search group t-d tof (search-refine-aim refine) (search-refine-side refine))
        (when total
          (setf (rendezvous-search-candidates search)
                (append (rendezvous-search-candidates search)
                        (list (make-rendezvous-candidate
                               :family (search-group-family group)
                               :revolutions (search-group-revolutions group)
                               :direction (search-group-direction group)
                               :impulses impulses :arrival (+ t-d tof)
                               :aim (search-refine-aim refine) :side (search-refine-side refine)
                               :aim-r r-a :aim-v v-a :total total
                               :margin (search-refine-clear-margin refine)
                               :target-distance (first (search-refine-clear-distance refine))
                               :other-distance (second (search-refine-clear-distance refine)))))))))))

(defun refine-item! (search group refine)
  "REFINE's next item: (values done units), DONE once the seed is."
  (let ((aim (eq (search-refine-phase refine) :aim)))
    (values (refine-phase-item! search group refine)
            (if aim (+ *aim-bucket-work* *aim-work*) *aim-work*))))

(defun refine-phase-item! (search group refine)
  "The item of REFINE's phase; true when the seed is done."
  (ecase (search-refine-phase refine)
    ((:departure :tof) (refine-golden-item! search group refine) nil)
    (:scan
     (multiple-value-bind (margin distance scanned) (search-scan-result search (list :scan))
       (cond ((not scanned)
              (if (and (= (search-refine-best-departure refine) (search-refine-clear-departure refine))
                       (= (search-refine-best-tof refine) (search-refine-clear-tof refine)))
                  (setf (search-refine-phase refine) :aim)
                  (unless (refine-probe! search group refine :scan (search-refine-best-departure refine)
                                         (search-refine-best-tof refine))
                    (setf (search-refine-phase refine) :aim)))
              nil)
             ((>= margin 0d0)
              (setf (search-refine-clear-departure refine) (search-refine-best-departure refine)
                    (search-refine-clear-tof refine) (search-refine-best-tof refine)
                    (search-refine-clear-margin refine) margin
                    (search-refine-clear-distance refine) distance
                    (search-refine-phase refine) :aim)
              nil)
             (t (setf (search-refine-far-departure refine) (search-refine-best-departure refine)
                      (search-refine-far-tof refine) (search-refine-best-tof refine)
                      (search-refine-steps refine) 0
                      (search-refine-phase refine) :boundary)
                nil))))
    (:boundary
     (multiple-value-bind (margin distance scanned) (search-scan-result search (list :boundary))
       (when scanned
         (if (>= margin 0d0)
             (setf (search-refine-clear-departure refine) (search-refine-probe-departure refine)
                   (search-refine-clear-tof refine) (search-refine-probe-tof refine)
                   (search-refine-clear-margin refine) margin
                   (search-refine-clear-distance refine) distance)
             (setf (search-refine-far-departure refine) (search-refine-probe-departure refine)
                   (search-refine-far-tof refine) (search-refine-probe-tof refine)))
         (incf (search-refine-steps refine)))
       (if (>= (search-refine-steps refine) *search-boundary-steps*)
           (setf (search-refine-phase refine) :aim)
           (unless scanned
             (unless (refine-probe! search group refine :boundary
                                    (* 0.5d0 (+ (search-refine-clear-departure refine)
                                                (search-refine-far-departure refine)))
                                    (* 0.5d0 (+ (search-refine-clear-tof refine)
                                                (search-refine-far-tof refine))))
               ;; infeasible counts as unclear
               (setf (search-refine-far-departure refine) (search-refine-probe-departure refine)
                     (search-refine-far-tof refine) (search-refine-probe-tof refine))
               (incf (search-refine-steps refine)))))
       nil))
    (:aim
     ;; rendezvous-aim at the exact arrival: the same aim point keeps it,
     ;; another is evaluated and scanned again, none drops it
     (let* ((t-a (+ (search-refine-clear-departure refine) (search-refine-clear-tof refine)))
            (sim (rendezvous-search-sim search)))
       (multiple-value-bind (number r v side)
           (rendezvous-aim sim (rendezvous-search-target search) (rendezvous-search-fixture search)
                           (rendezvous-search-range search) t-a)
         (declare (ignore r v))
         (cond ((null number) t)
               ((and (= number (search-refine-aim refine)) (= side (search-refine-side refine)))
                (search-accept! search group refine)
                t)
               (t (setf (search-refine-aim refine) number
                        (search-refine-side refine) side
                        (search-refine-phase refine) :rescan)
                  (not (refine-probe! search group refine :rescan
                                      (search-refine-clear-departure refine)
                                      (search-refine-clear-tof refine))))))))
    (:rescan
     (multiple-value-bind (margin distance) (search-scan-result search (list :rescan))
       (when (>= margin 0d0)
         (setf (search-refine-clear-margin refine) margin
               (search-refine-clear-distance refine) distance)
         (search-accept! search group refine))
       t))))

;;; The work loop

(defun search-bound (search)
  "The 8th least total among the candidates so far, or infinity."
  (let ((totals (sort (mapcar #'rendezvous-candidate-total (rendezvous-search-candidates search)) #'<)))
    (or (nth (1- *search-candidates*) totals) most-positive-double-float)))

(defun search-order (candidates)
  "CANDIDATES in decision 119's order: again and again the earliest arrival
among those within *search-tie-dv* of the least total left."
  (let ((left (copy-list candidates)) (out '()))
    (loop while left
          do (let* ((least (reduce #'min left :key #'rendezvous-candidate-total))
                    (pick (reduce (lambda (a b)
                                    (if (< (rendezvous-candidate-arrival b) (rendezvous-candidate-arrival a))
                                        b a))
                                  (remove-if (lambda (c) (> (rendezvous-candidate-total c)
                                                            (+ least *search-tie-dv*)))
                                             left))))
               (push pick out)
               (setf left (remove pick left))))
    (nreverse out)))

(defun search-item-units (search)
  "The most units SEARCH's next item takes."
  (let ((scan (rendezvous-search-scan search)))
    (if (and scan (not (scan-done-p scan)))
        (scan-item-units scan)
        (ecase (rendezvous-search-stage search)
          (:aims *aim-bucket-work*)
          (:phasing *phasing-point-work*)
          (:grid (1+ (* *lambert-work* (length (rendezvous-search-transfers search)))))
          ((:seeds :refine) (+ *aim-bucket-work* *aim-work*))
          ((:group :finish :done) 1)))))

(defun search-item! (search)
  "SEARCH's next item; its units."
  (let ((scan (rendezvous-search-scan search))
        (group (first (rendezvous-search-groups search)))
        (units *aim-work*))
    (if (and scan (not (scan-done-p scan)))
        (scan-item! scan)
        (ecase (rendezvous-search-stage search)
          (:aims
           (let ((i (rendezvous-search-cursor search)))
             (search-aim-bucket! search i)
             (when (>= (incf (rendezvous-search-cursor search))
                       (length (rendezvous-search-aim-numbers search)))
               (setf (rendezvous-search-cursor search) 0
                     (rendezvous-search-stage search) :phasing))
             *aim-bucket-work*))
          (:phasing
           (search-phasing-point! search (rendezvous-search-cursor search))
           (when (>= (incf (rendezvous-search-cursor search)) *search-departures*)
             (setf (rendezvous-search-groups search) (search-groups search)
                   (rendezvous-search-stage search) :group))
           *phasing-point-work*)
          (:group
           (cond ((null group) (setf (rendezvous-search-stage search) :finish))
                 ((>= (search-group-bound group) (search-bound search))
                  ;; B's groups come by their bound: none after can do better
                  (setf (rendezvous-search-groups search) '()
                        (rendezvous-search-stage search) :finish))
                 (t (setf (rendezvous-search-cursor search) 0
                          (rendezvous-search-stage search) :grid)))
           1)
          (:grid
           (let ((units (search-grid-departure! search group (rendezvous-search-cursor search))))
             (when (>= (incf (rendezvous-search-cursor search)) (search-group-count group))
               (setf (rendezvous-search-stage search)
                     (if (and (> (search-group-pool-count group) 0)
                              (let ((bound (search-bound search)))
                                (or (= bound most-positive-double-float)
                                    (<= (aref (search-group-pool-total group) 0)
                                        (* (+ 1 *search-group-margin*) bound)))))
                         :seeds
                         (progn (pop (rendezvous-search-groups search)) :group))))
             units))
          (:seeds
           (when (search-seeds-item! search group)
             (if (search-group-seeds group)
                 (setf (rendezvous-search-refine search)
                       (search-refine-start group (first (search-group-seeds group)))
                       (rendezvous-search-stage search) :refine)
                 (progn (pop (rendezvous-search-groups search))
                        (setf (rendezvous-search-stage search) :group))))
           *aim-work*)
          (:refine
           (multiple-value-bind (done spent) (refine-item! search group (rendezvous-search-refine search))
             (setf units spent)
             (when done
               (pop (search-group-seeds group))
               (if (search-group-seeds group)
                   (setf (rendezvous-search-refine search)
                         (search-refine-start group (first (search-group-seeds group))))
                   (progn (setf (rendezvous-search-refine search) nil)
                          (pop (rendezvous-search-groups search))
                          (setf (rendezvous-search-stage search) :group)))))
           units)
          (:finish
           (let ((ordered (search-order (rendezvous-search-candidates search))))
             (setf (rendezvous-search-result search)
                   (coerce (subseq ordered 0 (min *search-candidates* (length ordered))) 'simple-vector)
                   (rendezvous-search-stage search) :done))
           1)
          (:done 0)))))

(defun rendezvous-search-work! (search budget)
  "At most BUDGET units of SEARCH (an item whose most does not fit waits for
the next call; a call that fits none takes one); true once :done
(rendezvous-search-result: the best *search-candidates* clear candidates in
order)."
  (let ((left budget)
        (*coast-j2* (model-coast-j2 (rendezvous-search-sim search)))
        (*coast-delta* (rendezvous-search-delta search)))
    (loop until (or (eq (rendezvous-search-stage search) :done) (<= left 0))
          do (let ((most (search-item-units search)))
               (when (and (> most left) (< left budget))
                 (return))
               (decf left (search-item! search))))
    (setf (rendezvous-search-work search) (- budget left))
    (eq (rendezvous-search-stage search) :done)))

(defun rendezvous-search-advance! (search budget)
  "The job's :search stage: (values done units) of at most BUDGET units."
  (if (plusp budget)
      (values (rendezvous-search-work! search budget) (rendezvous-search-work search))
      (values (eq (rendezvous-search-stage search) :done) 0)))

(defun rendezvous-search-now (sim target &rest keys &key fixture (range *stop-range*) &allow-other-keys)
  "The search for TARGET from SIM at once (FIXTURE a name, or the target's
first free one; the other KEYS rendezvous-search-start!'s): (values
candidates nil), a simple-vector in order, or (values nil reason)."
  (multiple-value-bind (chosen reason) (rendezvous-stop sim target :fixture fixture :range range)
    (if reason
        (values nil reason)
        (let ((search (new-rendezvous-search)))
          (apply #'rendezvous-search-start! search sim target chosen range
                 (loop for (key value) on keys by #'cddr
                       unless (member key '(:fixture :range)) append (list key value)))
          (rendezvous-search-work! search most-positive-fixnum)
          (let ((result (rendezvous-search-result search)))
            (if (plusp (length result))
                (values result nil)
                (values nil "NO CLEAR TRANSFER")))))))

;;; Rendezvous: the job (docs/design.md "Rendezvous: search, phasing and the
;;; approach"; decision 119, decision 158: The rendezvous job as built: the
;;; lead, the transfer re-guess, step halving, the planning world, the
;;; checks): [:plane] [:phasing] :departure :arrival from the search's
;;; candidates in order, the fixed burns flown by the lead, the :departure's
;;; and :arrival's Δv shot from it to the aim point, the path checked; the
;;; terminal phase is the step's.

(defparameter *rendezvous-plane-min* (deg->rad 0.01d0)
  "rad between the ship's plane and the target's: a :plane burn first; decision 119.")
(defparameter *rendezvous-position-tolerance* 100d0 "m at the :arrival cut; decision 119.")
(defparameter *rendezvous-velocity-tolerance* 0.1d0 "m/s at the :arrival cut; decision 119.")
(declaim (type double-float *rendezvous-plane-min* *rendezvous-position-tolerance*
               *rendezvous-velocity-tolerance*))

(defun rendezvous-goal-parts (goal sim)
  "A rendezvous GOAL, (<target, (target fixture-name) or :target (SIM's)>
&key range limit): (values target fixture-name range limit)."
  (destructuring-bind (spec &key (range *stop-range*) (limit *rendezvous-limit*)) goal
    (let ((target (if (consp spec) (first spec) spec)))
      (values (if (eq target :target) (sim-target sim) target)
              (and (consp spec) (second spec))
              (float range 1d0) (float limit 1d0)))))

(defun job-rendezvous-start! (job)
  "JOB's rendezvous from its snapshot: the stop point's choice (a replan's
its base's), the snapshot's target set to it; refused \"NO TARGET\", \"NO
FREE FIXTURE\", \"RANGE TOO LONG\" or \"RANGE TOO SHORT\"; true when on."
  (let* ((snapshot (autopilot-job-snapshot job))
         (base (and (autopilot-job-base job) (burn-plan-rendezvous (autopilot-job-base job)))))
    (if base
        (setf (autopilot-job-target job) (rendezvous-part-target base)
              (autopilot-job-fixture job) (rendezvous-part-fixture base)
              (autopilot-job-range job) (rendezvous-part-range base)
              (autopilot-job-aim job) (rendezvous-part-aim base)
              (autopilot-job-side job) (rendezvous-part-side base))
        (multiple-value-bind (target name range) (rendezvous-goal-parts (autopilot-job-goal job) snapshot)
          (unless target
            (job-refuse! job "NO TARGET")
            (return-from job-rendezvous-start! nil))
          (multiple-value-bind (fixture reason) (rendezvous-stop snapshot target :fixture name :range range)
            (when reason
              (job-refuse! job reason)
              (return-from job-rendezvous-start! nil))
            (setf (autopilot-job-target job) target
                  (autopilot-job-fixture job) fixture
                  (autopilot-job-range job) range))))
    (sim-set-target! snapshot (autopilot-job-target job))
    (shot-sim-assign! (autopilot-job-lead job) snapshot nil)
    t))

(defun plane-angle (r v rt vt)
  "rad between the planes of R, V and RT, VT."
  (let ((h (vec3-cross r v)) (ht (vec3-cross rt vt)))
    (atan (vec3-norm (vec3-cross h ht)) (vec3-dot h ht))))

(defun job-rendezvous-search! (job)
  "After JOB's coast shot: with the planes ≥ *rendezvous-plane-min* apart
the :plane burn's first guess (the target's plane at the cheaper relative
node), the search from its cut, its first node ≥ the cut + *plan-min-lead*
+ *search-node-lead*; else from the snapshot. Stage :search."
  (let* ((snapshot (autopilot-job-snapshot job))
         (ship (sim-ship snapshot))
         (target (autopilot-job-target job))
         (fixture (autopilot-job-fixture job))
         (range (autopilot-job-range job))
         (limit (nth-value 3 (rendezvous-goal-parts (autopilot-job-goal job) snapshot)))
         (search (new-rendezvous-search))
         (rt (make-vec3)) (vt (make-vec3)))
    (stop-target-state-into! rt vt snapshot target :drop-point (sim-t snapshot))
    (if (>= (plane-angle (ship-state-r ship) (ship-state-v ship) rt vt) *rendezvous-plane-min*)
        (multiple-value-bind (plan reason)
            (autopilot-first-guess snapshot :change-plane :target (autopilot-job-coast job))
          (unless plan
            (return-from job-rendezvous-search! (job-refuse! job reason)))
          (let* ((burn (svref (burn-plan-burns plan) 0))
                 (cut (planned-burn-cut-t burn)))
            (setf (autopilot-job-plane job) burn)
            (rendezvous-search-start! search snapshot target fixture range
                                      :r (vec3-copy (planned-burn-cut-r burn))
                                      :v (vec3-copy (planned-burn-cut-v burn))
                                      :time cut :earliest (+ cut *plan-min-lead* *search-node-lead*)
                                      :limit limit :plane-dv (planned-burn-dv burn))))
        (rendezvous-search-start! search snapshot target fixture range :limit limit))
    (setf (autopilot-job-search job) search
          (autopilot-job-stage job) :search)
    job))

(defun rendezvous-guess (job)
  "The first guess of JOB's current candidate (autopilot-job-index of the
search's result): the :plane burn, then each impulse built (build-burn) on
its node with the propellant after the earlier; its aim point JOB's.
(values plan nil), or (values nil \"NO CLEAR TRANSFER\") past the last."
  (let ((candidates (rendezvous-search-result (autopilot-job-search job)))
        (index (autopilot-job-index job)))
    (setf (autopilot-job-candidates job) candidates)
    (if (>= index (length candidates))
        (values nil "NO CLEAR TRANSFER")
        (let* ((candidate (svref candidates index))
               (snapshot (autopilot-job-snapshot job))
               (ship (sim-ship snapshot))
               (orbit (state->orbit (ship-state-r ship) (ship-state-v ship) +mu-earth+ (sim-t snapshot)))
               (load (sim-stack-load snapshot))
               (m-p (ship-state-m-p ship))
               (plane (autopilot-job-plane job))
               (burns '())
               (previous nil))
          (flet ((add! (label node r v dv)
                   (multiple-value-bind (burn propellant)
                       (build-burn label node r v dv m-p load previous orbit)
                     (push burn burns)
                     (setf previous (planned-burn-attitude burn))
                     (decf m-p propellant))))
            (when plane
              (let ((dv (burn-dv-vector plane)))
                (add! :plane (planned-burn-cut-t plane) (planned-burn-cut-r plane)
                      (vec3-sub (planned-burn-cut-v plane) dv) dv)))
            (loop for impulse across (rendezvous-candidate-impulses candidate)
                  do (add! (rendezvous-impulse-label impulse) (rendezvous-impulse-time impulse)
                           (rendezvous-impulse-r impulse) (rendezvous-impulse-v impulse)
                           (rendezvous-impulse-dv impulse))))
          (setf (autopilot-job-aim job) (rendezvous-candidate-aim candidate)
                (autopilot-job-side job) (rendezvous-candidate-side candidate))
          (values (assemble-burn-plan (coerce (nreverse burns) 'simple-vector)
                                      :rendezvous (autopilot-job-goal job) m-p load)
                  nil)))))

(defparameter *shot-lead* 600d0
  "s before a rendezvous candidate's first hold-at its shots start, from the lead's coast.")
(declaim (type double-float *shot-lead*))

(defun job-planning-sim (job)
  "The sim JOB's aim points and the target's planning states come from: once
its lead is at lead-t, the lead's world there (the research copy: a
promotion on the way may have re-railed the target); before, the search's
sim (the snapshot); a replan's snapshot without a search."
  (cond ((autopilot-job-researched job) (autopilot-job-research job))
        ((autopilot-job-search job) (rendezvous-search-sim (autopilot-job-search job)))
        (t (autopilot-job-snapshot job))))

(defun job-lead-start! (job)
  "Stage :lead: JOB's lead from the snapshot, its candidate engaged, to fly
the fixed burns and coast to *shot-lead* before the first free burn's
hold-at; its cuts, clearance window (from the first burn's start) and abort
reset."
  (let* ((lead (autopilot-job-lead job))
         (snapshot (autopilot-job-snapshot job))
         (burns (burn-plan-burns (autopilot-job-candidate job)))
         (free (svref burns (autopilot-job-fixed job))))
    (shot-sim-assign! lead snapshot (autopilot-job-candidate job))
    (setf (autopilot-job-researched job) nil
          (autopilot-job-lead-t job)
          (max (sim-t snapshot) (grid-round (- (planned-burn-hold-at free) *shot-lead*)))
          (autopilot-job-lead-cuts job) '()
          (autopilot-job-lead-abort job) nil
          (autopilot-job-stage job) :lead)
    (prediction-clearance! (autopilot-job-lead-scan job)
                           (planned-burn-start (svref burns 0)) (autopilot-job-lead-t job))
    job))

(defun job-lead-advance! (job budget)
  "JOB's lead stepped towards lead-t while its steps' work
(prediction-step-work) fits BUDGET, as prediction-extend! steps a shot (at
*prediction-level*, an RCS burn at *prediction-rcs-level*): each burn's cut
(t r v) kept, the clearances scanned, an abort kept.
(values done units)."
  (let ((lead (autopilot-job-lead job))
        (scan (autopilot-job-lead-scan job))
        (until (autopilot-job-lead-t job))
        (units 0))
    (loop while (and (< (sim-t lead) until) (null (sim-loss lead)))
          do (let ((work (prediction-step-work lead)))
               (when (> (+ units work) budget) (return))
               (when (sim-plan lead)
                 (let ((index (sim-plan-index lead))
                       (burning (eq (sim-plan-phase lead) :burning)))
                   (plan-step-controls! lead)
                   (when (and burning (or (/= index (sim-plan-index lead))
                                          (not (eq (sim-plan-phase lead) :burning))))
                     (setf (autopilot-job-lead-cuts job)
                           (append (autopilot-job-lead-cuts job)
                                   (list (list (sim-t lead) (vec3-copy (ship-state-r (sim-ship lead)))
                                               (vec3-copy (ship-state-v (sim-ship lead))))))))))
               (contact-starts! scan lead)
               (let ((ticks (sim-step-ticks! lead (if (plan-rcs-burning-p lead)
                                                      *prediction-rcs-level*
                                                      *prediction-level*)
                                             until)))
                 (let ((notice (find :autopilot (sim-notices lead) :key #'first)))
                   (when (and notice (eql 0 (search "AUTOPILOT ABORT" (second notice))))
                     (setf (autopilot-job-lead-abort job) (second notice))))
                 (setf (sim-notices lead) nil
                       (sim-checkpoint-request lead) nil)
                 (unless (eql ticks 0)
                   (clearance-step! scan lead)))
               (incf units work)))
    (values (or (>= (sim-t lead) until) (and (sim-loss lead) t)) units)))

(defun job-lead-done! (job)
  "JOB's lead at lead-t: its world there the planning sim (job-planning-sim),
the transfer guessed again from it (job-transfer-guess!) before the
candidate's first shot, then that shot."
  (shot-sim-assign! (autopilot-job-research job) (autopilot-job-lead job) nil)
  (setf (autopilot-job-researched job) t)
  (when (= (autopilot-job-iteration job) 0)
    (job-transfer-guess! job))
  (setf (autopilot-job-stage job) :iterate)
  (job-shoot! job))

(defparameter *transfer-guess-ratio* 1.5d0
  "The re-guessed transfer is kept at most this times the search's Δv …")
(defparameter *transfer-guess-slack* 2d0 "… plus this, m/s.")
(declaim (type double-float *transfer-guess-ratio* *transfer-guess-slack*))

(defun job-params-dv (job from below)
  "m/s: the Δv of JOB's burns FROM to BELOW by their parameters."
  (let ((params (autopilot-job-params job)))
    (loop for i from from below below
          sum (sqrt (+ (expt (aref params (* 3 i)) 2) (expt (aref params (+ (* 3 i) 1)) 2)
                       (expt (aref params (+ (* 3 i) 2)) 2))))))

(defun job-transfer-guess! (job)
  "JOB's :departure and :arrival guessed again from where its lead's ship
is: the scratch coasted from the lead to the :departure's node, the
transfer there to the aim point at the :arrival's node by
coast-lambert-into! in the ship's plane (the search's kept when it has
none, or costs over *transfer-guess-ratio* times the search's plus
*transfer-guess-slack*); their node states and LVLH Δv."
  (let* ((n (autopilot-job-burn-count job))
         (d (autopilot-job-fixed job))
         (labels (autopilot-job-burn-labels job)))
    (when (and (= n (+ d 2)) (eq (svref labels d) :departure) (eq (svref labels (1+ d)) :arrival))
      (let* ((times (autopilot-job-node-times job))
             (t-d (setf (aref times d) (grid-round (aref times d))))
             (t-a (aref times (1+ d)))
             (scratch (shot-sim-assign! (autopilot-job-scratch job) (autopilot-job-lead job) nil))
             (planning (job-planning-sim job))
             (*coast-j2* (model-coast-j2 planning))
             (*coast-delta* nil))
        (loop while (and (< (sim-t scratch) t-d) (null (sim-loss scratch)))
              do (sim-step-ticks! scratch *prediction-level* t-d))
        (multiple-value-bind (r-a v-a)
            (aim-point planning (autopilot-job-target job) (autopilot-job-fixture job)
                       (autopilot-job-range job) (autopilot-job-aim job) t-a (autopilot-job-side job))
          (let* ((ship (sim-ship scratch))
                 (r-d (vec3-copy (ship-state-r ship)))
                 (v-d (vec3-copy (ship-state-v ship)))
                 (normal (vec3-normalize (vec3-cross r-d v-d)))
                 ;; in the ship's plane: near 180° an offset out of it
                 ;; would tilt the transfer's; Newton's N terms take it
                 (r-in (vec3-add-scaled r-a normal (- (vec3-dot r-a normal))))
                 (v1 (make-vec3)) (v2 (make-vec3))
                 (group (make-coast-group (coast-rails r-d v-d t-d))))
            (when (and (= (sim-t scratch) t-d)
                       (coast-lambert-into! v1 v2 r-d t-d r-in (- t-a t-d) normal group)
                       ;; near 180° Lambert's branch can be dear: the search's then
                       (<= (+ (vec3-norm (vec3-sub v1 v-d)) (vec3-norm (vec3-sub v-a v2)))
                           (+ (* *transfer-guess-ratio* (job-params-dv job d (+ d 2)))
                              *transfer-guess-slack*)))
              (let ((states (autopilot-job-node-states job))
                    (params (autopilot-job-params job)))
                (loop for i in (list d (1+ d))
                      for r in (list r-d r-in)
                      for v in (list v-d v2)
                      for dv in (list (vec3-sub v1 v-d) (vec3-sub v-a v2))
                      do (replace states r :start1 (* 6 i))
                         (replace states v :start1 (+ (* 6 i) 3))
                         (multiple-value-bind (ta na ra) (lvlh-axes r v)
                           (setf (aref params (* 3 i)) (vec3-dot dv ta)
                                 (aref params (+ (* 3 i) 1)) (vec3-dot dv na)
                                 (aref params (+ (* 3 i) 2)) (vec3-dot dv ra))))))))))
    job))

;;; Re-timing (decision 161, refining decision 158): the target, promoted
;;; 100 km out, leaves its rails while the transfer flies (its rails carry
;;; J2's secular rates at the mean motion, the integration moves on); the
;;; shot's burn times are fixed, so the transfer is re-timed to the target
;;; as the flight meets it.

(defparameter *retime-residual* 10d0
  "A candidate's first shot missing its largest residual by more (in
tolerances) re-times the transfer.")
(defparameter *retime-max* 2 "Re-timings per candidate.")
(defparameter *retime-departures* 16
  "Departures each side of the candidate's, 1/(2 × this) of the ship's period apart.")
(defparameter *retime-transfers* 4 "Transfer times each side of the candidate's …")
(defparameter *retime-transfer-step* (/ 1d0 16) "… this fraction of it apart.")
(defparameter *retime-approach-samples* 32
  "Samples of a transfer for its promotion of the target, then bisection.")
(declaim (type double-float *retime-residual* *retime-transfer-step*)
         (type fixnum *retime-max* *retime-departures* *retime-transfers* *retime-approach-samples*))

(defun job-arrival-aim (job time &optional epoch)
  "(values r v): JOB's aim point at TIME: the target's planning state
(job-planning-sim), or from EPOCH on its coast from its planning state there
(state-rails at the planning sim's model, as a promoted object moves), plus
the aim point's offset (job-aim-offset)."
  (let ((planning (job-planning-sim job))
        (target (autopilot-job-target job))
        (r (make-vec3)) (v (make-vec3)))
    (if epoch
        (progn (stop-target-state-into! r v planning target :drop-point epoch)
               (rails-state-into! r v (first (state-rails r v epoch :j2 (model-coast-j2 planning))) time))
        (stop-target-state-into! r v planning target :drop-point time))
    (multiple-value-bind (dr dv) (job-aim-offset job time)
      (values (vec3-add! r r dr) (vec3-add! v v dv)))))

(defun job-retime-due-p (job)
  "True when JOB's first shot of the candidate (a :departure and an
:arrival left) misses by more than *retime-residual*, at most *retime-max*
times."
  (let ((n (autopilot-job-burn-count job))
        (d (autopilot-job-fixed job))
        (labels (autopilot-job-burn-labels job)))
    (and (eq (autopilot-job-preset job) :rendezvous)
         (= (autopilot-job-iteration job) 0)
         (< (autopilot-job-retimes job) *retime-max*)
         (= n (+ d 2)) (eq (svref labels d) :departure) (eq (svref labels (1+ d)) :arrival)
         (> (loop for i below (autopilot-job-residual-count job)
                  maximize (abs (aref (autopilot-job-residuals job) i)))
            *retime-residual*))))

(defun job-retime-start! (job)
  "Stage :retime: first the reference coast, the lead's ship coasting on
JOB's shot (no plan) past the latest arrival scanned; then the transfer's
departure and transfer time scanned about the candidate's,
*retime-departures* each side over half the ship's period,
*retime-transfers* each side; the cheapest *retime-pool* refined by golden
section and checked in order (job-retime-advance!)."
  (let* ((lead (autopilot-job-lead job))
         (ship (sim-ship lead))
         (d (autopilot-job-fixed job))
         (times (autopilot-job-node-times job))
         (centre (autopilot-job-retime-centre job))
         (period (or (orbit-period (state->orbit (ship-state-r ship) (ship-state-v ship) +mu-earth+
                                                 (sim-t lead)))
                     0d0))
         (tof (- (aref times (1+ d)) (aref times d))))
    (setf (aref centre 0) (aref times d)
          (aref centre 1) tof
          (aref centre 2) period
          (autopilot-job-retime-count job) 0
          (autopilot-job-retime-chosen job) nil
          (autopilot-job-retime-gap job)
          (- (aref times d) (planned-burn-hold-at (svref (burn-plan-burns (autopilot-job-candidate job)) d)))
          (autopilot-job-retime-cursor job) -1
          (autopilot-job-stage job) :retime)
    (prediction-shoot! (autopilot-job-shot job)
                       (shot-sim-assign! (autopilot-job-scratch job) lead nil)
                       (+ (aref times d) (* 0.5d0 period)
                          (* tof (+ 1d0 (* *retime-transfers* *retime-transfer-step*)))
                          *shot-cap-margin*))
    (incf (autopilot-job-retimes job))
    job))

(defun retime-earliest (job)
  "The earliest :departure node of a re-timing: *plan-min-lead* +
*search-node-lead* after the snapshot or the last fixed burn's cut, and
its hold-at after the lead's t (the reference coast's start)."
  (let ((d (autopilot-job-fixed job)))
    (max (+ (if (> d 0)
                (aref (autopilot-job-node-times job) (1- d))
                (sim-t (autopilot-job-snapshot job)))
            *plan-min-lead* *search-node-lead*)
         (+ (autopilot-job-lead-t job) (autopilot-job-retime-gap job) 1d0))))

(defun coast-approach (job)
  "The first t JOB's reference coast comes within *promotion-distance* of
the target, or nil."
  (let ((coast (autopilot-job-shot job))
        (r (make-vec3)) (rt (make-vec3)))
    (loop for i from (prediction-head coast) below (prediction-count coast)
          for k = (* +node-size+ i)
          do (replace r (prediction-nodes coast) :start2 (+ k 1) :end2 (+ k 4))
          when (and (prediction-target-node coast (- i (prediction-head coast)) rt)
                    (<= (vec3-norm (vec3-sub! r r rt)) *promotion-distance*))
            return (node-t (prediction-nodes coast) i))))

(defun transfer-approach (job r-d v1 t-d t-a)
  "The first t the transfer from R-D, V1 at T-D (coast-rails) comes within
*promotion-distance* of the target on its planning rails: sampled, then
bisected; T-A if not before."
  (let* ((planning (job-planning-sim job))
         (target (autopilot-job-target job))
         (rails (first (coast-rails r-d v1 t-d)))
         (r (make-vec3)) (v (make-vec3)) (rt (make-vec3)) (vt (make-vec3)))
    (flet ((near-p (time)
             (rails-state-into! r v rails time)
             (stop-target-state-into! rt vt planning target :drop-point time)
             (<= (vec3-norm (vec3-sub! r r rt)) *promotion-distance*)))
      (let ((step (/ (- t-a t-d) *retime-approach-samples*)))
        (loop for k from 1 to *retime-approach-samples*
              for hi = (+ t-d (* k step))
              when (near-p hi)
                do (let ((lo (- hi step)))
                     (dotimes (i 12)
                       (let ((mid (* 0.5d0 (+ lo hi))))
                         (if (near-p mid) (setf hi mid) (setf lo mid))))
                     (return hi))
              finally (return t-a))))))

(defun model-error-into! (dr dv coast t1 t2 j2)
  "DR, DV: the state of the reference COAST at T2 less its coast
(state-rails, J2's secular rates if J2) from its state at T1 (clamped to
COAST's start): what the planning model misses over T1 to T2 near this
orbit. DR."
  (let ((r (make-vec3)) (v (make-vec3))
        (t1 (max t1 (prediction-now coast))))
    (prediction-state! r v coast t1)
    (rails-state-into! r v (first (state-rails r v t1 :j2 j2)) t2)
    (prediction-state! dr dv coast t2)
    (vec3-sub! dr dr r)
    (vec3-sub! dv dv v)
    dr))

(defun retime-aim (job t-d t-a epoch)
  "(values r v): JOB's aim point at T-A as the flight meets it, for a
transfer leaving at T-D: job-arrival-aim from the target's EPOCH (its
promotion, or the planning sim's t when promoted there; nil on rails), under
J2 corrected by the model's error near this orbit (model-error-into!) from
EPOCH less from T-D, so the transfer's and the target's misses cancel."
  (multiple-value-bind (r v) (job-arrival-aim job t-a epoch)
    (let ((j2 (model-coast-j2 (job-planning-sim job)))
          (coast (autopilot-job-shot job))
          (dr (make-vec3)) (dv (make-vec3)))
      (when j2
        (when epoch
          (model-error-into! dr dv coast epoch t-a j2)
          (vec3-add! r r dr)
          (vec3-add! v v dv))
        (model-error-into! dr dv coast t-d t-a j2)
        (vec3-sub! r r dr)
        (vec3-sub! v v dv))
      (values r v))))

(defun retime-evaluate (job t-d tof)
  "The transfer from JOB's reference coast at T-D for TOF s to its aim
point (retime-aim), in the ship's plane (job-transfer-guess!): a target on
rails is promoted where the coast or the transfer (transfer-approach, once
more after the first solve) first comes within *promotion-distance*.
(values total r-d v-d dv1 r-a v2 dv2 epoch), or nil (before retime-earliest,
past the limit or the coast, no solution, below the floor)."
  (let* ((planning (job-planning-sim job))
         (coast (autopilot-job-shot job))
         (object (target-object planning (autopilot-job-target job)))
         (t-d (grid-round t-d))
         (t-a (+ t-d tof)))
    (when (and (> tof 0d0) (>= t-d (retime-earliest job))
               (<= t-a (min (node-t (prediction-nodes coast) (1- (prediction-count coast)))
                            (+ (sim-t (autopilot-job-snapshot job))
                               (nth-value 3 (rendezvous-goal-parts (autopilot-job-goal job) planning))))))
      (let* ((*coast-j2* (model-coast-j2 planning))
             (*coast-delta* nil)
             (approach (autopilot-job-retime-approach job))
             (r-d (make-vec3)) (v-d (make-vec3)) (v1 (make-vec3)) (v2 (make-vec3))
             (normal (make-vec3))
             (epoch (cond ((null object) nil)
                          ((eq (object-status object) :promoted) (sim-t planning))
                          ((and approach (<= approach t-d)) approach)
                          (t t-d))))
        (prediction-state! r-d v-d coast t-d)
        (vec3-normalize! normal (vec3-cross! normal r-d v-d))
        (flet ((solve ()
                 (multiple-value-bind (r-a v-a) (retime-aim job t-d t-a epoch)
                   (vec3-add-scaled! r-a r-a normal (- (vec3-dot r-a normal)))
                   (and (coast-lambert-into! v1 v2 r-d t-d r-a tof normal
                                             (make-coast-group (coast-rails r-d v-d t-d)))
                        (list r-a v-a)))))
          (let ((aim (solve)))
            (when (and aim object (eq (object-status object) :rails) (= epoch t-d))
              (setf epoch (transfer-approach job r-d v1 t-d t-a)
                    aim (solve)))
            (when aim
              (destructuring-bind (r-a v-a) aim
                (let ((dv1 (vec3-sub v1 v-d)) (dv2 (vec3-sub v-a v2)))
                  (values (+ (vec3-norm dv1) (vec3-norm dv2)) r-d v-d dv1 r-a v2 dv2 epoch))))))))))

(defparameter *retime-check-samples* 64
  "Samples of a re-timed transfer's distance from the target, the least then refined.")
(defparameter *retime-check-work* 40 "Units per clearance check of a re-timed transfer.")
(defparameter *retime-gain* 1d0
  "m/s a re-timed transfer must save, in the model, over the candidate's own times.")
(defparameter *retime-clearance-margin* 50d0
  "m a re-timed transfer keeps beyond the clearance, for its model's error (the shot checks).")
(declaim (type fixnum *retime-check-samples* *retime-check-work*)
         (type double-float *retime-gain* *retime-clearance-margin*))

(defun retime-margin (job t-d tof)
  "m the re-timed transfer at T-D, TOF keeps from the target's CoM beyond
its bound + *object-clearance* (decision 59), as retime-aim models both
(under J2 each corrected by the reference coast's model error from its
epoch); nil if infeasible. Sampled, the least refined by golden section."
  (multiple-value-bind (total r-d v-d dv1 r-a v2 dv2 epoch) (retime-evaluate job t-d tof)
    (declare (ignore r-a v2 dv2))
    (when total
      (let* ((planning (job-planning-sim job))
             (target (autopilot-job-target job))
             (object (target-object planning target))
             (coast (autopilot-job-shot job))
             (j2 (model-coast-j2 planning))
             (*coast-j2* j2)
             (*coast-delta* nil)
             (t-d (grid-round t-d))
             (t-a (+ t-d tof))
             (ship (first (coast-rails r-d (vec3-add v-d dv1) t-d)))
             (r (make-vec3)) (v (make-vec3)) (p (make-vec3)) (q (make-vec3)))
        (unless object
          (return-from retime-margin most-positive-double-float))
        (flet ((reference (time)
                 (let ((time (max time (prediction-now coast))))
                   (prediction-state! r v coast time)
                   (first (state-rails r v time :j2 j2)))))
          (let* ((ship-ref (and j2 (reference t-d)))
                 (target-model (and epoch (progn (stop-target-state-into! r v planning target :drop-point epoch)
                                                 (first (state-rails r v epoch :j2 j2)))))
                 (target-ref (and j2 epoch (reference epoch))))
            (flet ((distance (time)
                     ;; ship less target, each with its model's error added
                     (rails-state-into! p q ship time)
                     (if (and epoch (>= time epoch))
                         (rails-state-into! r v target-model time)
                         (stop-target-state-into! r v planning target :drop-point time))
                     (vec3-sub! p p r)
                     (when j2
                       (prediction-state! r v coast time)
                       (vec3-add! p p r)
                       (rails-state-into! r v ship-ref time)
                       (vec3-sub! p p r)
                       (when (and epoch (>= time epoch))
                         (prediction-state! r v coast time)
                         (vec3-sub! p p r)
                         (rails-state-into! r v target-ref time)
                         (vec3-add! p p r)))
                     (vec3-norm p)))
              (let* ((step (/ tof *retime-check-samples*))
                     (k (loop with best = 0 with least = most-positive-double-float
                              for i from 0 to *retime-check-samples*
                              for d = (distance (+ t-d (* i step)))
                              when (< d least) do (setf least d best i)
                              finally (return best)))
                     (a (+ t-d (* (max 0 (1- k)) step)))
                     (b (min t-a (+ t-d (* (1+ k) step))))
                     (c (- b (* +golden+ (- b a)))) (e (+ a (* +golden+ (- b a))))
                     (fc (distance c)) (fe (distance e)))
                (dotimes (i (golden-steps (- b a) *scan-time-tolerance*))
                  (if (< fc fe)
                      (setf b e e c fe fc c (- b (* +golden+ (- b a))) fc (distance c))
                      (setf a c c e fc fe e (+ a (* +golden+ (- b a))) fe (distance e))))
                (- (min fc fe (distance t-d) (distance t-a))
                   (object-def-bound (object-def-of object)) *object-clearance*)))))))))

(defun retime-pool-insert! (job total t-d tof)
  "TOTAL at T-D, TOF into JOB's pool of the cheapest, in order."
  (let* ((pool (autopilot-job-retime-pool job))
         (count (autopilot-job-retime-count job))
         (at (loop for i below count
                   when (< total (aref pool (* 3 i))) return i
                   finally (return count))))
    (when (< at *retime-pool*)
      (replace pool pool :start1 (* 3 (1+ at)) :start2 (* 3 at)
                         :end2 (* 3 (min count (1- *retime-pool*))))
      (setf (aref pool (* 3 at)) total
            (aref pool (+ (* 3 at) 1)) t-d
            (aref pool (+ (* 3 at) 2)) tof
            (autopilot-job-retime-count job) (min *retime-pool* (1+ count))))))

(defun retime-golden! (job departure)
  "Golden section of the point being refined (retime-point) in its
DEPARTURE (else its transfer time) over one scan step each side, to
*search-time-tolerance*; the point moved to the least found."
  (let* ((centre (autopilot-job-retime-centre job))
         (point (autopilot-job-retime-point job))
         (width (if departure
                    (/ (aref centre 2) (* 2 *retime-departures*))
                    (* (aref centre 1) *retime-transfer-step*)))
         (x0 (aref point (if departure 1 2)))
         (a (- x0 width)) (b (+ x0 width)))
    (flet ((f (x)
             (let* ((t-d (if departure x (aref point 1)))
                    (tof (if departure (aref point 2) x))
                    (total (retime-evaluate job t-d tof)))
               (when (and total (< total (aref point 0)))
                 (setf (aref point 0) total (aref point 1) t-d (aref point 2) tof))
               (or total most-positive-double-float))))
      (let* ((c (- b (* +golden+ (- b a)))) (d (+ a (* +golden+ (- b a))))
             (fc (f c)) (fd (f d)))
        (dotimes (i (golden-steps (- b a) *search-time-tolerance*))
          (if (< fc fd)
              (setf b d d c fd fc c (- b (* +golden+ (- b a))) fc (f c))
              (setf a c c d fc fd d (+ a (* +golden+ (- b a))) fd (f d))))))))

(defun retime-rows () (1+ (* 2 *retime-departures*)))

(defun retime-item-units (job)
  "The units of JOB's next re-timing item: a scan row's, a golden
section's or a check's."
  (let ((cursor (autopilot-job-retime-cursor job))
        (centre (autopilot-job-retime-centre job)))
    (if (< cursor (retime-rows))
        (* *aim-work* (1+ (* 2 *retime-transfers*)))
        (case (mod (- cursor (retime-rows)) 3)
          (0 (* *aim-work* (+ 2 (golden-steps (/ (aref centre 2) *retime-departures*)
                                              *search-time-tolerance*))))
          (1 (* *aim-work* (+ 2 (golden-steps (* 2 (aref centre 1) *retime-transfer-step*)
                                              *search-time-tolerance*))))
          (t (* 2 *retime-check-work*))))))

(defun retime-item! (job)
  "JOB's re-timing item at its cursor: a scan row into the pool; per pool
point its golden sections in departure and transfer time, then the check:
the refined point, else the scan's, chosen when clear by
*retime-clearance-margin* and *retime-gain* cheaper than the candidate's
own times (retime-own); the pool ends at the first that is not cheaper."
  (let* ((cursor (autopilot-job-retime-cursor job))
         (centre (autopilot-job-retime-centre job))
         (point (autopilot-job-retime-point job))
         (pool (autopilot-job-retime-pool job)))
    (if (< cursor (retime-rows))
        (let ((t-d (+ (aref centre 0) (* (- cursor *retime-departures*)
                                         (/ (aref centre 2) (* 2 *retime-departures*))))))
          (loop for j from (- *retime-transfers*) to *retime-transfers*
                for tof = (* (aref centre 1) (+ 1d0 (* j *retime-transfer-step*)))
                for total = (retime-evaluate job t-d tof)
                when total do (retime-pool-insert! job total t-d tof)))
        (multiple-value-bind (k phase) (floor (- cursor (retime-rows)) 3)
          (case phase
            (0 (replace point pool :start2 (* 3 k) :end2 (+ (* 3 k) 3))
               (retime-golden! job t))
            (1 (retime-golden! job nil))
            (t (let ((margin (retime-margin job (aref point 1) (aref point 2))))
                 (unless (and margin (>= margin *retime-clearance-margin*))
                   (replace point pool :start2 (* 3 k) :end2 (+ (* 3 k) 3))
                   (setf margin (retime-margin job (aref point 1) (aref point 2))))
                 (cond ((>= (aref point 0) (- (autopilot-job-retime-own job) *retime-gain*))
                        ;; the rest cost more: keep the candidate's times
                        (setf (autopilot-job-retime-count job) k))
                       ((and margin (>= margin *retime-clearance-margin*))
                        (setf (autopilot-job-retime-chosen job) t))))))))
    (incf (autopilot-job-retime-cursor job))))

(defun job-retime-advance! (job budget)
  "JOB's re-timing within BUDGET units: the reference coast as a shot
extends; then the candidate's own times in the model (retime-own), and
unless they cost no more than its guess and *retime-gain*, items while
their units fit (a call that fits none takes one) until a clear point is
chosen or the pool is spent. (values done units)."
  (let ((units 0)
        (coast (autopilot-job-shot job)))
    (when (< (autopilot-job-retime-cursor job) 0)
      (prediction-extend! coast budget)
      (setf units (prediction-last-work coast))
      (unless (prediction-complete-p coast)
        (return-from job-retime-advance! (values nil units)))
      (let ((centre (autopilot-job-retime-centre job))
            (d (autopilot-job-fixed job)))
        (setf (autopilot-job-retime-cursor job) 0
              (autopilot-job-retime-approach job) (coast-approach job)
              (autopilot-job-retime-own job)
              (or (retime-evaluate job (aref centre 0) (aref centre 1)) most-positive-double-float))
        ;; the model prices the candidate's own times at its guess: no scan
        (when (<= (autopilot-job-retime-own job) (+ (job-params-dv job d (+ d 2)) *retime-gain*))
          (setf (autopilot-job-retime-cursor job) (retime-rows)))))
    (flet ((done-p ()
             (or (autopilot-job-retime-chosen job)
                 (>= (autopilot-job-retime-cursor job)
                     (+ (retime-rows) (* 3 (autopilot-job-retime-count job)))))))
      (loop until (done-p)
            do (let ((most (retime-item-units job)))
                 (when (and (> (+ units most) budget) (> units 0))
                   (return))
                 (retime-item! job)
                 (incf units most)))
      (values (done-p) units))))

(defun job-retime-done! (job)
  "The chosen re-timed transfer as JOB's :departure and :arrival (their
nodes, states and LVLH Δv), then shot; none chosen, the candidate shot as
it was, not re-timed again."
  (let ((point (autopilot-job-retime-point job))
        (d (autopilot-job-fixed job)))
    (multiple-value-bind (total r-d v-d dv1 r-a v2 dv2)
        (and (autopilot-job-retime-chosen job)
             (retime-evaluate job (aref point 1) (aref point 2)))
      (when total
        (let ((times (autopilot-job-node-times job))
              (states (autopilot-job-node-states job))
              (params (autopilot-job-params job)))
          (setf (aref times d) (grid-round (aref point 1))
                (aref times (1+ d)) (+ (aref times d) (aref point 2)))
          (loop for i in (list d (1+ d))
                for r in (list r-d r-a)
                for v in (list v-d v2)
                for dv in (list dv1 dv2)
                do (replace states r :start1 (* 6 i))
                   (replace states v :start1 (+ (* 6 i) 3))
                   (multiple-value-bind (ta na ra) (lvlh-axes r v)
                     (setf (aref params (* 3 i)) (vec3-dot dv ta)
                           (aref params (+ (* 3 i) 1)) (vec3-dot dv na)
                           (aref params (+ (* 3 i) 2)) (vec3-dot dv ra))))))))
  (unless (autopilot-job-retime-chosen job)
    (setf (autopilot-job-retimes job) *retime-max*))
  (setf (autopilot-job-stage job) :iterate)
  (job-shoot! job))

(defun job-next-candidate! (job reason)
  "JOB's current candidate dropped: the next one guessed and shot, past the
last \"NO CLEAR TRANSFER\"; a replan (one plan only) refused with REASON."
  (if (and (null (autopilot-job-base job))
           (< (1+ (autopilot-job-index job)) (length (autopilot-job-candidates job))))
      (setf (autopilot-job-index job) (1+ (autopilot-job-index job))
            (autopilot-job-retimes job) 0
            (autopilot-job-iteration job) 0
            (autopilot-job-column job) -1
            (autopilot-job-stage job) :guess)
      (job-refuse! job (if (autopilot-job-base job) reason "NO CLEAR TRANSFER")))
  job)

(defun job-aim-offset (job time)
  "(values dr dv): JOB's aim point at TIME less the target's planning state
there (stop-target-state-into!, the drop point itself for Skua Yard), from
its snapshot."
  (let ((snapshot (job-planning-sim job))
        (rt (make-vec3)) (vt (make-vec3)))
    (multiple-value-bind (r v)
        (aim-point snapshot (autopilot-job-target job) (autopilot-job-fixture job)
                   (autopilot-job-range job) (autopilot-job-aim job) time (autopilot-job-side job))
      (stop-target-state-into! rt vt snapshot (autopilot-job-target job) :drop-point time)
      (values (vec3-sub r rt) (vec3-sub v vt)))))

(defun rendezvous-residuals! (job out)
  "OUT: at JOB's shot's :arrival cut the ship's position and velocity less
the aim point's (the shot's target there plus job-aim-offset), over
*rendezvous-position-tolerance* and *rendezvous-velocity-tolerance*; the
velocity alone when no :departure is left to steer the position (a replan
after it). (values count largest); large for a shot lost or cut short."
  (let* ((shot (autopilot-job-shot job))
         (n (autopilot-job-burn-count job))
         (cut (shot-last-cut job))
         (m (if (find :departure (autopilot-job-burn-labels job) :end n) 6 3))
         (i (and cut (node-interval shot cut))))
    (if (or (null i) (sim-loss (prediction-sim shot)))
        (progn (fill out 1d3 :end m)
               (values m 1d3))
        (let ((r (make-vec3)) (v (make-vec3)) (rt (make-vec3)) (vt (make-vec3))
              (nodes (prediction-nodes shot)))
          (replace r nodes :start2 (+ (* +node-size+ i) 1))
          (replace v nodes :start2 (+ (* +node-size+ i) 4))
          (unless (prediction-target-node shot (- i (prediction-head shot)) rt vt)
            (stop-target-state-into! rt vt (job-planning-sim job) (autopilot-job-target job)
                                     :drop-point cut))
          (multiple-value-bind (dr dv) (job-aim-offset job cut)
            (dotimes (j 3)
              (when (= m 6)
                (setf (aref out j) (/ (- (aref r j) (aref rt j) (aref dr j))
                                      *rendezvous-position-tolerance*)))
              (setf (aref out (+ j (- m 3))) (/ (- (aref v j) (aref vt j) (aref dv j))
                                                *rendezvous-velocity-tolerance*))))
          (values m (loop for j below m maximize (abs (aref out j))))))))

(defun shot-path-margin (job)
  "The path check of JOB's lead and shot (decisions 58, 59): over their
clearance windows the least of each promoted object's distance (the target's too) less
its bound + *object-clearance*: (values margin passes), PASSES (key . m)
per object it came near."
  (let* ((shot (autopilot-job-shot job))
         (objects (sim-objects (prediction-sim shot)))
         (least (prediction-clearance-least shot))
         (lead (prediction-clearance-least (autopilot-job-lead-scan job)))
         (margin most-positive-double-float)
         (passes '()))
    (loop for i below (min (length least) (length objects))
          for d = (if (< i (length lead)) (min (aref least i) (aref lead i)) (aref least i))
          when (< d most-positive-double-float)
            do (let ((object (svref objects i)))
                 (push (cons (object-key object) d) passes)
                 (setf margin (min margin (- d (object-def-bound (object-def-of object))
                                             *object-clearance*)))))
    (values margin (nreverse passes))))

(defun job-rendezvous-part (job passes)
  "JOB's rendezvous-part for its plan: the stop point's choice, the aim
point (its offset from the target at the :arrival node), PASSES."
  (let ((arrival (aref (autopilot-job-node-times job) (1- (autopilot-job-burn-count job)))))
    (make-rendezvous-part (autopilot-job-target job) (autopilot-job-fixture job)
                          (autopilot-job-range job) (autopilot-job-aim job) (autopilot-job-side job)
                          (job-aim-offset job arrival) arrival passes)))

(defun rendezvous-plan-aim-point (plan)
  "The number of PLAN's aim point, (1)–(3), or nil for a plan not a rendezvous."
  (let ((rendezvous (burn-plan-rendezvous plan)))
    (and rendezvous (rendezvous-part-aim rendezvous))))

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
  ;; :apsis (:apoapsis ↔ :periapsis), :hours (a rendezvous's limit), :none.
  (kind :none :type (member :altitude :inclination :inclination-keep :apsis :hours :none)
        :read-only t)
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

(defparameter *orbit-rows*
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
  "The orbit rows in order (decision 118).")

(defun rendezvous-row (key name target &optional offered)
  "The row \"RDV <NAME> <h> H\" of KEY: a rendezvous with TARGET (a key, or
:target: the sim's) within its limit in hours (decision 119)."
  (make-autopilot-row key :hours
                      (lambda (sim orbit)
                        (declare (ignore sim orbit))
                        (round *rendezvous-limit* 3600))
                      (lambda (hours) (format nil "RDV ~A ~D H" name hours))
                      (lambda (model)
                        (values :rendezvous (list target :limit (* 3600d0 (model-value model key)))))
                      :offered offered))

(defparameter *autopilot-rows*
  (append *orbit-rows*
          (loop for place in *places*
                collect (rendezvous-row (intern (format nil "RDV-~A" (place-key place)) :keyword)
                                        (place-name place) (place-key place)))
          (list (rendezvous-row :rdv-target "TARGET" :target
                                (lambda (sim)
                                  (let ((target (sim-target sim)))
                                    (and target (not (find target *places* :key #'place-key))))))))
  "The menu's rows in order (decision 118): the orbit rows, then \"RDV
<PLACE>\" for each of *places* and \"RDV TARGET\" with a target not a place
(decision 119).")

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
          (:hours
           (let ((step (round *rendezvous-limit-step* 3600)))
             (setf (cdr cell) (clamp (+ (cdr cell) (* direction factor step))
                                     step (round *rendezvous-limit-max* 3600)))))
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
         (job (autopilot-model-job model))
         ;; a rendezvous's shots start late (job-lead-start!): its coast
         (shot (if (burn-plan-rendezvous plan) (autopilot-job-coast job) (autopilot-job-shot job)))
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
