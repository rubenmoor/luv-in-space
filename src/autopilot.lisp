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

(defun autopilot-coast-shot (sim)
  "SIM's coasting path: a copy without its plan, throttle or cut-off, run
two osculating periods by the prediction."
  (let ((copy (copy-sim sim)))
    (sim-set-plan! copy nil)
    (controls-throttle-cut (sim-controls copy))
    (setf (sim-cutoff copy) nil)
    (prediction-run! (new-prediction :periods 2) copy)))

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
                 (let* ((burns (coerce (reverse burns) 'simple-vector))
                        (last (svref burns (1- (length burns))))
                        (total (reduce #'+ burns :key #'planned-burn-dv))
                        (reserve (plan-reserve-dv total))
                        (end-mass (+ *ship-dry-mass* m-p (stack-load-mass load))))
                   (values (make-burn-plan burns
                                           :preset preset :goal goal
                                           :goal-orbit (state->orbit (planned-burn-cut-r last)
                                                                     (planned-burn-cut-v last)
                                                                     mu (planned-burn-cut-t last))
                                           :reserve-dv reserve
                                           :reserve-mass (propellant-for end-mass reserve
                                                                         *engine-isp*))
                           nil))))
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
