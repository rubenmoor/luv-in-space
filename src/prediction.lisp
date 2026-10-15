(in-package #:luv-in-space)

;;; Trajectory prediction: sim-step! on a copy of the sim at a coarse level,
;;; every step's t, r, v kept (and the target's r, v), and the events found
;;; on them (docs/architecture.md "Trajectory prediction"; decision 75:
;;; Prediction in milestone 2: the step function on a copy, decision 20:
;;; Trajectory prediction on the frame thread, decision 116: Prediction in
;;; milestone 3: horizon, target nodes, closest approach, relative nodes,
;;; budget).

(defparameter *prediction-level* 1000
  "The warp level the copy steps at: coarse, kinematic, decision 75.")
(defparameter *prediction-rcs-level* 4
  "The copy's level while a planned RCS burn runs: fine, keys on; decision 115.")
(defparameter *prediction-work-per-frame* 4096
  "Work per prediction-update!: per copy step 1, plus 1 per promoted object
stepped, plus 1 for a rails target (decision 116). As many as the clock's
steps per frame, so warp never outruns it: ~2.7 ms at the start orbit
(0.65 us per unit, :full), ~2.4 ms at Skua Yard (0.59 us; tests/prediction.lisp;
decision 149).")
(defparameter *prediction-capacity* 20000
  "Stored steps, the horizon's last bound; decision 75.")
(defparameter *prediction-deviation* 1d0
  "m: the flown r (or target) farther than this from the stored path restarts; decision 75, 116.")
(defparameter *prediction-event-capacity* 128 "Events kept ahead of the flown t; decision 116.")
(defparameter *apsis-time-tolerance* 1d-6 "s, the apsides' root; decision 75.")
(declaim (type fixnum *prediction-level* *prediction-rcs-level* *prediction-work-per-frame*
               *prediction-capacity* *prediction-event-capacity*)
         (type double-float *prediction-deviation* *apsis-time-tolerance*))

(defconstant +node-size+ 7)                     ; t, r, v
(defconstant +target-node-size+ 6)              ; the target's r, v

(defstruct (prediction (:constructor %make-prediction (nodes target-nodes event-times event-kinds
                                                        event-objects event-values))
                       (:copier nil))
  (sim nil :type (or null sim))                 ; the copy that steps ahead
  (nodes nil :type (simple-array double-float (*)))
  (target-nodes nil :type (simple-array double-float (*))) ; per node the target's r, v
  (head 0 :type fixnum)                         ; the node at or before the flown t
  (count 0 :type fixnum)                        ; nodes stored
  (target-count 0 :type fixnum)                 ; nodes from 0 with the target's state
  (periods 1 :type (integer 1 2))               ; the coasting horizon in osculating periods
  (until nil :type (or null double-float))      ; a shot's horizon, instead of the periods
  (until-plan nil :type boolean)                ; the horizon: the copy's plan done + a period
  (end 0d0 :type double-float)                  ; the horizon's t
  (state :empty :type (member :empty :running :complete))
  (now 0d0 :type double-float)                  ; the flown t at the last update
  (event-times nil :type (simple-array double-float (*)))
  (event-kinds nil :type simple-vector)
  (event-count 0 :type fixnum)
  (event-objects nil :type simple-vector)       ; :contact: the object's index
  (event-values nil :type (simple-array double-float (*)))
  (cut-times (make-array 16 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (16)))      ; the copy's burns' cuts, in order
  (cut-count 0 :type fixnum)
  (contact-starts (make-array 0 :element-type 'double-float) ; a step's start: ship, candidates
   :type (simple-array double-float (*)))
  ;; A rendezvous shot's path check (decision 119): over [from, until] the
  ;; least distance from the ship to each promoted object's CoM, per index.
  (clearance-from 0d0 :type double-float)
  (clearance-until -1d0 :type double-float)
  (clearance-least (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)))
  (impact nil)                                  ; the copy's loss record
  (abort nil)                                   ; the copy's plan's abort notice, a string
  (signature (new-controls) :type controls)     ; the controls it was made with
  (signature-cutoff nil :type (or null double-float))
  (signature-model :full :type keyword)
  (signature-target nil :type (or null keyword))
  (signature-plan nil :type (or null burn-plan))
  (signature-plan-index 0 :type fixnum)
  (signature-plan-phase :done :type keyword)
  (signature-plan-rcs-target 0d0 :type double-float)
  (source-loss nil)                             ; the flown sim's loss at the restart
  (restarts 0 :type fixnum)
  (last-steps 0 :type fixnum)                   ; sim-step!s in the last call
  (last-work 0 :type fixnum))                   ; their work, *prediction-work-per-frame*'s unit

(defun new-prediction (&key (periods 1) until)
  "An empty prediction; PERIODS its coasting horizon (2 for the clock's: the
map), or UNTIL a fixed t (a shot's)."
  (let ((p (%make-prediction
            (make-array (* +node-size+ *prediction-capacity*) :element-type 'double-float
                                                               :initial-element 0d0)
            (make-array (* +target-node-size+ *prediction-capacity*) :element-type 'double-float
                                                                      :initial-element 0d0)
            (make-array *prediction-event-capacity* :element-type 'double-float
                                                    :initial-element 0d0)
            (make-array *prediction-event-capacity* :initial-element nil)
            (make-array *prediction-event-capacity* :initial-element nil)
            (make-array *prediction-event-capacity* :element-type 'double-float
                                                    :initial-element 0d0))))
    (setf (prediction-periods p) periods
          (prediction-until p) (and until (float until 1d0)))
    p))

(defun grid-ceiling (time)
  "The first 1/64 s grid point at or after TIME: where a wait on an event
ends (decision 75)."
  (declare (type double-float time))
  (* +fine-step+ (fceiling (* time (/ +fine-step+)))))

;;; Nodes

(declaim (inline node-t))
(defun node-t (nodes i)
  (declare (type (simple-array double-float (*)) nodes) (type fixnum i))
  (aref nodes (* +node-size+ i)))

(defun push-node! (pred sim)
  "Append SIM's t, r, v; nil when the buffer is full."
  (declare (optimize speed) (type prediction pred) (type sim sim))
  (let ((nodes (prediction-nodes pred))
        (head (prediction-head pred)))
    (when (and (>= (prediction-count pred) *prediction-capacity*) (> head 0))
      (replace nodes nodes :start2 (* +node-size+ head))
      (let ((targets (prediction-target-nodes pred)))
        (replace targets targets :start2 (* +target-node-size+ head)))
      (decf (prediction-count pred) head)
      (setf (prediction-target-count pred) (max 0 (- (prediction-target-count pred) head))
            (prediction-head pred) 0))
    (let ((i (prediction-count pred)))
      (when (< i *prediction-capacity*)
        (let ((k (* +node-size+ i))
              (ship (sim-ship sim)))
          (setf (aref nodes k) (sim-t sim))
          (copy-doubles! nodes (ship-state-r ship) 3 (+ k 1))
          (copy-doubles! nodes (ship-state-v ship) 3 (+ k 4)))
        (setf (prediction-count pred) (1+ i))
        t))))

(defun push-target! (pred sim)
  "After push-node!: SIM's target's r, v at its t for the last node, while
the target nodes are contiguous and SIM has a target with a state. The work:
1 for a rails evaluation, else 0. Conses nothing."
  (declare (optimize speed) (type prediction pred) (type sim sim)
           (inline target-state-into! target-rails-segment))
  (let ((i (1- (prediction-count pred))))
    (if (and (sim-target sim) (= (prediction-target-count pred) i))
        (let ((r (make-vec3)) (v (make-vec3))
              (time (sim-t sim)))
          (declare (dynamic-extent r v))
          (cond ((target-state-into! r v sim time)
                 (let ((k (* +target-node-size+ i))
                       (targets (prediction-target-nodes pred)))
                   (replace targets r :start1 k)
                   (replace targets v :start1 (+ k 3)))
                 (setf (prediction-target-count pred) (1+ i))
                 (if (target-rails-segment sim time) 1 0))
                (t 0)))
        0)))

(defun node-interval (pred time)
  "The index i ≥ head with t_i ≤ TIME < t_(i+1), the last node if TIME is
its t; nil outside the stored nodes."
  (declare (optimize speed) (type prediction pred) (type double-float time))
  (let ((nodes (prediction-nodes pred))
        (lo (prediction-head pred))
        (hi (1- (prediction-count pred))))
    (declare (type fixnum lo hi))
    (when (or (< hi lo) (< time (node-t nodes lo)) (> time (node-t nodes hi)))
      (return-from node-interval nil))
    (when (= time (node-t nodes hi))
      (return-from node-interval hi))
    ;; t_lo ≤ time < t_hi
    (loop while (> (- hi lo) 1)
          do (let ((mid (ash (+ lo hi) -1)))
               (if (<= (node-t nodes mid) time) (setf lo mid) (setf hi mid))))
    lo))

(defun interval-hermite! (c pred i &optional target)
  "C: the Hermite of the stored path (TARGET: the target's) over [t_i,
t_(i+1)]; its h."
  (declare (optimize speed) (type hermite c) (type prediction pred) (type fixnum i))
  (let* ((nodes (prediction-nodes pred))
         (h (- (node-t nodes (1+ i)) (node-t nodes i)))
         (data (if target (prediction-target-nodes pred) nodes))
         (size (if target +target-node-size+ +node-size+))
         (k0 (+ (* size i) (if target 0 1))) (k1 (+ k0 size)))
    (declare (type (simple-array double-float (*)) data) (type fixnum size k0 k1))
    (dotimes (j 3)
      (let* ((a (aref data (+ k0 j))) (b (aref data (+ k1 j)))
             (m0 (* h (aref data (+ k0 3 j)))) (m1 (* h (aref data (+ k1 3 j)))))
        (setf (aref c j) a
              (aref c (+ 3 j)) m0
              (aref c (+ 6 j)) (- (* 3 (- b a)) (* 2 m0) m1)
              (aref c (+ 9 j)) (+ (* 2 (- a b)) m0 m1))))
    h))

(defun path-position! (out pred time target)
  "OUT: the stored path's (TARGET: the target's) position at TIME, the
Hermite between nodes; nil outside it."
  (declare (type vec3 out) (type prediction pred) (type double-float time))
  (let ((i (node-interval pred time))
        (nodes (prediction-nodes pred)))
    (cond ((null i) nil)
          ((and target (>= i (prediction-target-count pred))) nil)
          ((= time (node-t nodes i))
           (if target
               (let ((k (* +target-node-size+ i)))
                 (replace out (prediction-target-nodes pred) :start2 k :end2 (+ k 3)))
               (replace out nodes :start2 (+ (* +node-size+ i) 1) :end2 (+ (* +node-size+ i) 4))))
          ((and target (>= (1+ i) (prediction-target-count pred))) nil)
          (t
           (let ((c (make-array 12 :element-type 'double-float))
                 (dp (make-vec3)))
             (declare (dynamic-extent c dp))
             (let ((h (interval-hermite! c pred i target)))
               (hermite-point! out dp c (/ (- time (node-t nodes i)) h)))
             out)))))

(defun prediction-position! (out pred time)
  "OUT: the stored path's position at TIME (the Hermite between nodes); nil
outside it."
  (path-position! out pred time nil))

(defun prediction-state! (r v pred time)
  "R, V: the stored path's position and velocity at TIME, the Hermite
between nodes; nil outside it."
  (declare (type vec3 r v) (type prediction pred) (type double-float time))
  (let ((i (node-interval pred time)))
    (when i
      (let ((c (make-array 12 :element-type 'double-float))
            (nodes (prediction-nodes pred)))
        (declare (dynamic-extent c))
        (if (or (= time (node-t nodes i)) (>= (1+ i) (prediction-count pred)))
            (let ((k (* +node-size+ i)))
              (replace r nodes :start2 (+ k 1) :end2 (+ k 4))
              (replace v nodes :start2 (+ k 4) :end2 (+ k 7)))
            (let ((h (interval-hermite! c pred i)))
              (hermite-point! r v c (/ (- time (node-t nodes i)) h))
              (vec3-scale! v v (/ h))))
        r))))

(defun prediction-target-position! (out pred time)
  "OUT: the target's stored position at TIME (the Hermite between its
nodes); nil outside them or without a target."
  (path-position! out pred time t))

(defun prediction-node-count (pred)
  "The nodes stored from the one at or before the flown t."
  (- (prediction-count pred) (prediction-head pred)))

(defun prediction-node (pred i &optional r v)
  "Node I (0: at or before the flown t): its t, its r and v into R, V if given."
  (let ((k (* +node-size+ (+ (prediction-head pred) i)))
        (nodes (prediction-nodes pred)))
    (when r (replace r nodes :start2 (+ k 1) :end2 (+ k 4)))
    (when v (replace v nodes :start2 (+ k 4) :end2 (+ k 7)))
    (aref nodes k)))

(defun prediction-target-node (pred i &optional r v)
  "The target at node I: its t, its r and v into R, V if given; nil without
the target's state there."
  (let ((j (+ (prediction-head pred) i)))
    (when (< j (prediction-target-count pred))
      (let ((k (* +target-node-size+ j))
            (targets (prediction-target-nodes pred)))
        (when r (replace r targets :start2 k :end2 (+ k 3)))
        (when v (replace v targets :start2 (+ k 3) :end2 (+ k 6)))
        (node-t (prediction-nodes pred) j)))))

;;; Events: kept in time order, dropped once the flown t passes them; each
;;; with a double value (decision 116).

(defun push-event! (pred kind time &optional object (value 0d0))
  "KIND at TIME with VALUE (a :contact with its OBJECT's index), unless the
flown t is there already; inserted in time order (a step's events are found
out of order), after those at the same time."
  (let ((n (prediction-event-count pred))
        (times (prediction-event-times pred))
        (kinds (prediction-event-kinds pred))
        (objects (prediction-event-objects pred))
        (amounts (prediction-event-values pred)))
    (declare (type fixnum n) (type double-float time)
             (type (simple-array double-float (*)) times amounts))
    (when (and (< n *prediction-event-capacity*) (> time (prediction-now pred)))
      (let ((i n))
        (declare (type fixnum i))
        (loop while (and (> i 0) (> (aref times (1- i)) time))
              do (setf (aref times i) (aref times (1- i))
                       (svref kinds i) (svref kinds (1- i))
                       (svref objects i) (svref objects (1- i))
                       (aref amounts i) (aref amounts (1- i)))
                 (decf i))
        (setf (aref times i) time
              (svref kinds i) kind
              (svref objects i) object
              (aref amounts i) value
              (prediction-event-count pred) (1+ n))))))

(defun drop-past-events! (pred)
  (let* ((times (prediction-event-times pred))
         (kinds (prediction-event-kinds pred))
         (objects (prediction-event-objects pred))
         (amounts (prediction-event-values pred))
         (n (prediction-event-count pred))
         (now (prediction-now pred))
         (k (loop for i of-type fixnum below n
                  while (<= (aref times i) now)
                  finally (return i))))
    (declare (type (simple-array double-float (*)) times amounts) (type fixnum n k)
             (type double-float now))
    (when (> k 0)
      (replace times times :start2 k :end2 n)
      (replace kinds kinds :start2 k :end2 n)
      (replace objects objects :start2 k :end2 n)
      (replace amounts amounts :start2 k :end2 n)
      (setf (prediction-event-count pred) (- n k)))))

(defun apsis-search! (pred i)
  "An apsis between nodes i and i + 1 from r·v's sign change: pushed with
its time, bisected on the Hermite to *apsis-time-tolerance*."
  (declare (optimize speed) (type prediction pred) (type fixnum i))
  (let* ((nodes (prediction-nodes pred))
         (k0 (* +node-size+ i)) (k1 (+ k0 +node-size+))
         (f0 (+ (* (aref nodes (+ k0 1)) (aref nodes (+ k0 4)))
                (* (aref nodes (+ k0 2)) (aref nodes (+ k0 5)))
                (* (aref nodes (+ k0 3)) (aref nodes (+ k0 6)))))
         (f1 (+ (* (aref nodes (+ k1 1)) (aref nodes (+ k1 4)))
                (* (aref nodes (+ k1 2)) (aref nodes (+ k1 5)))
                (* (aref nodes (+ k1 3)) (aref nodes (+ k1 6))))))
    (declare (type double-float f0 f1))
    (when (or (and (< f0 0d0) (>= f1 0d0))
              (and (> f0 0d0) (<= f1 0d0)))
      (let ((c (make-array 12 :element-type 'double-float))
            (p (make-vec3)) (dp (make-vec3)))
        (declare (dynamic-extent c p dp))
        (let* ((h (interval-hermite! c pred i))
               (rising (< f0 0d0))
               (lo 0d0) (hi 1d0)
               (tolerance (/ *apsis-time-tolerance* (* 4 h))))
          (declare (type double-float h lo hi tolerance))
          (loop while (> (- hi lo) tolerance)
                do (let ((s (* 0.5d0 (+ lo hi))))
                     (hermite-point! p dp c s)
                     (if (eq (< (vec3-dot p dp) 0d0) rising)
                         (setf lo s)
                         (setf hi s))))
          (push-event! pred (if rising :periapsis :apoapsis)
                       (+ (node-t nodes i) (* 0.5d0 (+ lo hi) h))))))))

;;; The target (decision 116): closest approach where r_rel · v_rel goes
;;; from − to +, relative nodes where r · (r_T × v_T) changes sign, both
;;; bisected on the ship's and the target's Hermites.

(defparameter *relative-node-min-inclination* (deg->rad 0.01d0)
  "rad: planes closer than this give no relative nodes; decision 116.")
(defparameter *closest-approach-min-cosine* 1d-9
  "A local minimum of the range counts only where, at one of its step's
nodes, |r̂_rel · v_rel| / |v_rel| reaches this: a constant range (co-orbital,
1 km) gives minima from rounding noise at ~1e-12; decision 147.")
(declaim (type double-float *relative-node-min-inclination* *closest-approach-min-cosine*))

(defun target-search! (pred i)
  "Between nodes i and i + 1, both with the target's state: a
:closest-approach (value the distance) at a local minimum of the range, a
:relative-ascending or :relative-descending (value the planes' angle) where
the ship crosses the target's plane, each bisected to
*apsis-time-tolerance*. Conses nothing."
  (declare (optimize speed) (type prediction pred) (type fixnum i))
  (let* ((nodes (prediction-nodes pred))
         (targets (prediction-target-nodes pred))
         (k0 (* +node-size+ i)) (k1 (+ k0 +node-size+))
         (j0 (* +target-node-size+ i)) (j1 (+ j0 +target-node-size+))
         (t0 (node-t nodes i)))
    (declare (type fixnum k0 k1 j0 j1))
    (flet ((range-rate (k j)
             ;; r_rel · v_rel at a node
             (let ((sum 0d0))
               (declare (type double-float sum))
               (dotimes (a 3 sum)
                 (incf sum (* (- (aref targets (+ j a)) (aref nodes (+ k 1 a)))
                              (- (aref targets (+ j 3 a)) (aref nodes (+ k 4 a))))))))
           (clear-p (f k j)
             ;; |r_rel · v_rel| ≥ cos_min |r_rel| |v_rel|
             (let ((rr 0d0) (vv 0d0))
               (declare (type double-float rr vv))
               (dotimes (a 3)
                 (let ((dr (- (aref targets (+ j a)) (aref nodes (+ k 1 a))))
                       (dv (- (aref targets (+ j 3 a)) (aref nodes (+ k 4 a)))))
                   (incf rr (* dr dr))
                   (incf vv (* dv dv))))
               (>= (* f f) (* *closest-approach-min-cosine* *closest-approach-min-cosine* rr vv))))
           (height (k j)
             ;; r · (r_T × v_T) at a node
             (let ((x (aref targets j)) (y (aref targets (+ j 1))) (z (aref targets (+ j 2)))
                   (u (aref targets (+ j 3))) (v (aref targets (+ j 4))) (w (aref targets (+ j 5))))
               (+ (* (aref nodes (+ k 1)) (- (* y w) (* z v)))
                  (* (aref nodes (+ k 2)) (- (* z u) (* x w)))
                  (* (aref nodes (+ k 3)) (- (* x v) (* y u)))))))
      (let* ((f0 (range-rate k0 j0)) (f1 (range-rate k1 j1))
             (g0 (height k0 j0)) (g1 (height k1 j1))
             (approach (and (< f0 0d0) (>= f1 0d0)
                            (or (clear-p f0 k0 j0) (clear-p f1 k1 j1))))
             (node (or (and (< g0 0d0) (>= g1 0d0)) (and (> g0 0d0) (<= g1 0d0)))))
        (declare (type double-float f0 f1 g0 g1))
        (when (or approach node)
          (let ((cs (make-array 12 :element-type 'double-float))
                (ct (make-array 12 :element-type 'double-float))
                (cr (make-array 12 :element-type 'double-float))
                (p (make-vec3)) (dp (make-vec3)) (pt (make-vec3)) (dpt (make-vec3))
                (n (make-vec3)) (nt (make-vec3)))
            (declare (dynamic-extent cs ct cr p dp pt dpt n nt))
            (let* ((h (interval-hermite! cs pred i))
                   (tolerance (/ *apsis-time-tolerance* (* 4 h))))
              (declare (type double-float h tolerance))
              (interval-hermite! ct pred i t)
              (dotimes (a 12) (setf (aref cr a) (- (aref ct a) (aref cs a))))
              (macrolet ((bisect ((s rising) test)
                           ;; the root of TEST's sign change on [0, 1], RISING from −
                           `(let ((lo 0d0) (hi 1d0))
                              (declare (type double-float lo hi))
                              (loop while (> (- hi lo) tolerance)
                                    do (let ((,s (* 0.5d0 (+ lo hi))))
                                         (if (eq (< ,test 0d0) ,rising) (setf lo ,s) (setf hi ,s))))
                              (* 0.5d0 (+ lo hi)))))
                (when approach
                  (let ((s (bisect (s t) (progn (hermite-point! p dp cr s) (vec3-dot p dp)))))
                    (hermite-point! p dp cr s)
                    (push-event! pred :closest-approach (+ t0 (* s h)) nil (vec3-norm p))))
                (when node
                  (let* ((rising (< g0 0d0))
                         (s (bisect (s rising)
                                    (progn (hermite-point! p dp cs s)
                                           (hermite-point! pt dpt ct s)
                                           (vec3-dot p (vec3-cross! n pt dpt))))))
                    (hermite-point! p dp cs s)
                    (hermite-point! pt dpt ct s)
                    (vec3-cross! n p dp)
                    (vec3-cross! nt pt dpt)
                    (let* ((cosine (vec3-dot n nt))
                           (sine (vec3-norm (vec3-cross! p n nt)))
                           (angle (atan sine (abs cosine))))
                      (when (>= angle *relative-node-min-inclination*)
                        (push-event! pred (if rising :relative-ascending :relative-descending)
                                     (+ t0 (* s h)) nil angle)))))))))))))

;;; Contact (decision 43:Warp drop-outs for contact and life support,
;;; decision 113): the stack's and a candidate's bounding spheres coming to
;;; meet on a step's relative Hermite, as contact-find!'s broad phase.

(defun contact-starts! (pred sim)
  "Before a step of PRED's copy SIM: its t, the ship's r, v, then per
object a flag (a contact candidate, contact-candidate-p) and its r, v.
Conses nothing once the buffer fits SIM's objects."
  (declare (optimize speed) (type prediction pred) (type sim sim))
  (let* ((objects (sim-objects sim))
         (size (* 7 (1+ (length objects))))
         (buffer (prediction-contact-starts pred))
         (ship (sim-ship sim)))
    (unless (= (length buffer) size)
      (setf buffer (make-array size :element-type 'double-float :initial-element 0d0)
            (prediction-contact-starts pred) buffer))
    (setf (aref buffer 0) (sim-t sim))
    (copy-doubles! buffer (ship-state-r ship) 3 1)
    (copy-doubles! buffer (ship-state-v ship) 3 4)
    (loop for object across objects
          for i of-type fixnum from 0
          for k of-type fixnum from 7 by 7
          do (cond ((contact-candidate-p sim object i)
                    (let ((state (object-state object)))
                      (setf (aref buffer k) 1d0)
                      (copy-doubles! buffer (ship-state-r state) 3 (+ k 1))
                      (copy-doubles! buffer (ship-state-v state) 3 (+ k 4))))
                   (t (setf (aref buffer k) 0d0))))
    nil))

(defun contact-crossing! (pred sim)
  "After a step of PRED's copy SIM (contact-starts! before it): of
the objects a candidate at both ends, the first whose bounding sphere and
the stack's come to meet within the step, by hermite-crossing on their
relative Hermite against the sum of bounds (stack-bound at the end), not
already meeting at its start: a :contact event at that time. Conses
nothing."
  (declare (optimize speed) (type prediction pred) (type sim sim) (inline stack-bound))
  (let* ((objects (sim-objects sim))
         (buffer (prediction-contact-starts pred))
         (t0 (if (> (length buffer) 0) (aref buffer 0) (sim-t sim)))
         (h (- (sim-t sim) t0)))
    (declare (type double-float t0 h))
    (flet ((candidate-p (i)
             (declare (type fixnum i))
             (and (= (aref buffer (* 7 (1+ i))) 1d0)
                  (contact-candidate-p sim (svref objects i) i))))
      (declare (inline candidate-p))
      (unless (and (> h 0d0)
                   (= (length buffer) (* 7 (1+ (length objects))))
                   (loop for i of-type fixnum below (length objects) thereis (candidate-p i)))
        (return-from contact-crossing! nil))
      (with-stack-properties (properties sim load)
        (let ((bound (stack-bound (mass-properties-com properties) load))
              (ship (sim-ship sim))
              (c (make-array 12 :element-type 'double-float))
              (p0 (make-vec3)) (u0 (make-vec3)) (p1 (make-vec3)) (u1 (make-vec3))
              (best 2d0) (best-i -1))
          (declare (dynamic-extent c p0 u0 p1 u1) (type double-float bound best)
                   (type fixnum best-i))
          (dotimes (i (length objects))
            (when (candidate-p i)
              (let ((k (* 7 (1+ i)))
                    (state (object-state (svref objects i))))
                (dotimes (j 3)
                  (setf (aref p0 j) (- (aref buffer (+ k 1 j)) (aref buffer (+ 1 j)))
                        (aref u0 j) (- (aref buffer (+ k 4 j)) (aref buffer (+ 4 j)))
                        (aref p1 j) (- (aref (ship-state-r state) j) (aref (ship-state-r ship) j))
                        (aref u1 j) (- (aref (ship-state-v state) j) (aref (ship-state-v ship) j))))
                (hermite-set! c p0 u0 p1 u1 h)
                (let ((s (hermite-crossing c (+ bound (object-def-bound
                                                       (object-def-of (svref objects i))))
                                           (hermite-sag c))))
                  (when (< 0d0 s best)
                    (setf best s best-i i))))))
          (when (>= best-i 0)
            (push-event! pred :contact (+ t0 (* best h)) best-i)))))))

(defun hermite-least (c lo hi)
  "The least |p(s)| on C over [LO, HI] ⊆ [0, 1]: the ends, or where p·p'
goes from − to + between them, bisected. Conses nothing."
  (declare (optimize speed) (type hermite c) (type double-float lo hi) (inline hermite-point!))
  (let ((p (make-vec3)) (dp (make-vec3)))
    (declare (dynamic-extent p dp))
    (flet ((at (s) (hermite-point! p dp c s) (values (vec3-norm p) (vec3-dot p dp))))
      (multiple-value-bind (d0 g0) (at lo)
        (multiple-value-bind (d1 g1) (at hi)
          (if (and (< g0 0d0) (> g1 0d0))
              (let ((a lo) (b hi))
                (declare (type double-float a b))
                (loop repeat 50
                      do (let ((s (* 0.5d0 (+ a b))))
                           (if (< (nth-value 1 (at s)) 0d0) (setf a s) (setf b s))))
                (min d0 d1 (the double-float (at (* 0.5d0 (+ a b))))))
              (min d0 d1)))))))

(defun clearance-step! (pred sim)
  "After a step of PRED's copy SIM (contact-starts! before it), the part of
it within PRED's clearance window: each object a candidate at both ends, the
least distance from the ship on their relative Hermite kept per index.
Conses nothing."
  (declare (optimize speed) (type prediction pred) (type sim sim) (inline hermite-set!))
  (let* ((objects (sim-objects sim))
         (buffer (prediction-contact-starts pred))
         (least (prediction-clearance-least pred))
         (t0 (if (> (length buffer) 0) (aref buffer 0) (sim-t sim)))
         (t1 (sim-t sim))
         (h (- t1 t0))
         (from (max t0 (prediction-clearance-from pred)))
         (until (min t1 (prediction-clearance-until pred))))
    (declare (type double-float t0 t1 h from until))
    (when (and (> h 0d0) (<= from until)
               (= (length buffer) (* 7 (1+ (length objects))))
               (= (length least) (length objects)))
      (let ((ship (sim-ship sim))
            (c (make-array 12 :element-type 'double-float))
            (p0 (make-vec3)) (u0 (make-vec3)) (p1 (make-vec3)) (u1 (make-vec3)))
        (declare (dynamic-extent c p0 u0 p1 u1))
        (dotimes (i (length objects))
          (let ((k (* 7 (1+ i)))
                (object (svref objects i)))
            (when (and (= (aref buffer k) 1d0) (contact-candidate-p sim object i))
              (let ((state (object-state object)))
                (dotimes (j 3)
                  (setf (aref p0 j) (- (aref buffer (+ k 1 j)) (aref buffer (+ 1 j)))
                        (aref u0 j) (- (aref buffer (+ k 4 j)) (aref buffer (+ 4 j)))
                        (aref p1 j) (- (aref (ship-state-r state) j) (aref (ship-state-r ship) j))
                        (aref u1 j) (- (aref (ship-state-v state) j) (aref (ship-state-v ship) j))))
                (hermite-set! c p0 u0 p1 u1 h)
                (setf (aref least i)
                      (min (aref least i) (hermite-least c (/ (- from t0) h) (/ (- until t0) h))))))))))
    nil))

(defun prediction-clearance! (pred from until)
  "PRED's clearance window [FROM, UNTIL] (after a restart, before its
steps), its least distances reset; PRED."
  (let ((n (length (sim-objects (prediction-sim pred)))))
    (unless (= (length (prediction-clearance-least pred)) n)
      (setf (prediction-clearance-least pred) (make-array n :element-type 'double-float)))
    (fill (prediction-clearance-least pred) most-positive-double-float)
    (setf (prediction-clearance-from pred) (float from 1d0)
          (prediction-clearance-until pred) (float until 1d0))
    pred))

;;; Restart and extension

(defun signature-matches-p (pred sim)
  "SIM's controls, cut-off, model, target, plan (eq) and its cursor are
those PRED was made with (decision 75, decision 116)."
  (declare (type prediction pred) (type sim sim))
  (let ((a (prediction-signature pred))
        (b (sim-controls sim)))
    (and (= (controls-throttle a) (controls-throttle b))
         (eq (controls-attitude-mode a) (controls-attitude-mode b))
         (eq (controls-hold a) (controls-hold b))
         (= (controls-roll-offset a) (controls-roll-offset b))
         (equalp (controls-inertial-target a) (controls-inertial-target b))
         (equalp (controls-rotation-keys a) (controls-rotation-keys b))
         (equalp (controls-translation-keys a) (controls-translation-keys b))
         (eq (controls-fine a) (controls-fine b))
         (eql (prediction-signature-cutoff pred) (sim-cutoff sim))
         (eq (prediction-signature-model pred) (sim-model sim))
         (eq (prediction-signature-target pred) (sim-target sim))
         (eq (prediction-signature-plan pred) (sim-plan sim))
         (= (prediction-signature-plan-index pred) (sim-plan-index sim))
         (eq (prediction-signature-plan-phase pred) (sim-plan-phase sim))
         (= (prediction-signature-plan-rcs-target pred) (sim-plan-rcs-target sim)))))

(defun osculating-period (sim)
  "2π √(a³/μ) of SIM's ship about its body; nil unless bound."
  (declare (type sim sim))
  (let* ((ship (sim-ship sim))
         (r (vec3-norm (ship-state-r ship)))
         (v (ship-state-v ship))
         (mu (body-mu (body (sim-body sim))))
         (inverse-a (- (/ 2 r) (/ (vec3-dot v v) mu))))
    (and (> inverse-a 0d0)
         (* 2 pi (sqrt (/ (expt (/ inverse-a) 3) mu))))))

(defun prediction-restart! (pred sim)
  "Start PRED over from SIM."
  (declare (type prediction pred) (type sim sim))
  (if (prediction-sim pred)
      (sim-assign! (prediction-sim pred) sim)
      (setf (prediction-sim pred) (copy-sim sim)))
  (controls-assign! (prediction-signature pred) (sim-controls sim))
  (setf (prediction-signature-cutoff pred) (sim-cutoff sim)
        (prediction-signature-model pred) (sim-model sim)
        (prediction-signature-target pred) (sim-target sim)
        (prediction-signature-plan pred) (sim-plan sim)
        (prediction-signature-plan-index pred) (sim-plan-index sim)
        (prediction-signature-plan-phase pred) (sim-plan-phase sim)
        (prediction-signature-plan-rcs-target pred) (sim-plan-rcs-target sim)
        (prediction-source-loss pred) (sim-loss sim)
        (prediction-head pred) 0
        (prediction-count pred) 0
        (prediction-target-count pred) 0
        (prediction-event-count pred) 0
        (prediction-cut-count pred) 0
        (prediction-impact pred) nil
        (prediction-abort pred) nil
        (prediction-clearance-from pred) 0d0
        (prediction-clearance-until pred) -1d0
        (prediction-state pred) :running)
  (incf (prediction-restarts pred))
  (push-node! pred (prediction-sim pred))
  (push-target! pred (prediction-sim pred))
  pred)

(defun prediction-invalidate! (pred)
  "The next update restarts (after a restore)."
  (setf (prediction-state pred) :empty)
  pred)

(declaim (inline thrust-like-p))
(defun thrust-like-p (sim)
  "SIM's prediction runs as under thrust: the throttle up, or a rendezvous's
:terminal or :station (decision 120)."
  (or (> (controls-throttle (sim-controls sim)) 0d0)
      (plan-terminal-p sim)))

(defun prediction-stale-p (pred sim)
  "True when PRED must restart from SIM (decision 75's rules, decision 116's:
the target's path off by more than *prediction-deviation*, and every frame
as under thrust, thrust-like-p); else moves its head to SIM's t."
  (declare (type prediction pred) (type sim sim)
           (inline target-state-into!))
  (when (or (eq (prediction-state pred) :empty)
            (thrust-like-p sim)
            (not (eq (sim-loss sim) (prediction-source-loss pred)))
            (not (signature-matches-p pred sim)))
    (return-from prediction-stale-p t))
  (let ((i (node-interval pred (sim-t sim))))
    (when (null i)
      (return-from prediction-stale-p t))
    (let ((p (make-vec3)) (r (make-vec3)) (v (make-vec3)))
      (declare (dynamic-extent p r v))
      (prediction-position! p pred (sim-t sim))
      (vec3-sub! p p (ship-state-r (sim-ship sim)))
      (when (> (vec3-norm p) *prediction-deviation*)
        (return-from prediction-stale-p t))
      (let ((flown (target-state-into! r v sim (sim-t sim)))
            (stored (prediction-target-position! p pred (sim-t sim))))
        (unless (eq (not flown) (not stored))
          (return-from prediction-stale-p t))
        (when (and flown (> (vec3-norm (vec3-sub! p p r)) *prediction-deviation*))
          (return-from prediction-stale-p t))))
    (setf (prediction-head pred) i)
    nil))

(declaim (inline prediction-step-work))
(defun prediction-step-work (sim)
  "The most work a step of the copy SIM can take: 1, 1 per promoted object,
1 with a target (its rails evaluation)."
  (declare (type sim sim))
  (let ((work (if (sim-target sim) 2 1)))
    (declare (type fixnum work))
    (loop for object across (sim-objects sim)
          when (eq (object-status object) :promoted)
            do (incf work))
    work))

(defun prediction-extend! (pred budget)
  "sim-step!s of the copy toward the horizon, each stored with its target's
state and its events, while their work stays within BUDGET; PRED. The
copy's plan flies at *prediction-rcs-level* while an RCS burn runs, else at
*prediction-level* (decision 115)."
  (declare (optimize speed) (type prediction pred) (type fixnum budget)
           (inline sim-step-ticks! prediction-step-work plan-rcs-burning-p))
  (let ((sim (prediction-sim pred))
        (steps 0)
        (work 0))
    (declare (type fixnum steps work))
    (loop
      (when (or (sim-loss sim)
                (>= (sim-t sim) (prediction-end pred))
                (and (>= (prediction-count pred) *prediction-capacity*)
                     (= (prediction-head pred) 0)))
        (setf (prediction-state pred) :complete)
        (return))
      (when (> (+ work (prediction-step-work sim)) budget)
        (return))
      (incf work (- (prediction-step-work sim) (if (sim-target sim) 1 0)))
      ;; The plan's controls at the step's start, as the step itself would
      ;; set them (it does again, a no-op): the level, the cut-off and
      ;; :burn-start see this step's.
      (when (sim-plan sim)
        (let ((index (sim-plan-index sim))
              (burning (eq (sim-plan-phase sim) :burning)))
          (plan-step-controls! sim)
          (let ((moved (/= index (sim-plan-index sim))))
            (when (and burning (or moved (not (eq (sim-plan-phase sim) :burning)))
                       (< (prediction-cut-count pred) 16))
              (setf (aref (prediction-cut-times pred) (prediction-cut-count pred)) (sim-t sim))
              (incf (prediction-cut-count pred)))
            (when (and (eq (sim-plan-phase sim) :burning) (or (not burning) moved))
              (push-event! pred :burn-start (sim-t sim) nil
                           (float (sim-plan-index sim) 1d0)))))
        ;; A shot's horizon: the last cut plus one osculating period.
        (when (and (prediction-until-plan pred) (eq (sim-plan-phase sim) :done))
          (let ((period (osculating-period sim)))
            (setf (prediction-until-plan pred) nil
                  (prediction-end pred) (if period
                                            (+ (sim-t sim) (the double-float period))
                                            most-positive-double-float)))))
      (contact-starts! pred sim)
      (let* ((cutoff (sim-cutoff sim))
             (zero (eql (sim-step-ticks! sim (if (plan-rcs-burning-p sim)
                                                 *prediction-rcs-level*
                                                 *prediction-level*)
                                         nil)
                        0)))
        ;; The copy's notices and requests are nobody's (decision 112),
        ;; but a plan's abort is the shot's.
        (when (sim-notices sim)
          (let ((notice (find :autopilot (sim-notices sim) :key #'first)))
            (when (and notice (eql 0 (search "AUTOPILOT ABORT" (the string (second notice)))))
              (setf (prediction-abort pred) (second notice)))))
        (setf (sim-notices sim) nil
              (sim-checkpoint-request sim) nil)
        (incf steps)
        ;; A contact answered at the step's start: its node replaced.
        (when (and zero (> (prediction-count pred) (prediction-head pred)))
          (decf (prediction-count pred))
          (setf (prediction-target-count pred)
                (min (prediction-target-count pred) (prediction-count pred))))
        (unless (push-node! pred sim)
          (setf (prediction-state pred) :complete)
          (return))
        (incf work (the fixnum (push-target! pred sim)))
        (unless zero
          (apsis-search! pred (- (prediction-count pred) 2))
          (when (= (prediction-target-count pred) (prediction-count pred))
            (target-search! pred (- (prediction-count pred) 2)))
          (contact-crossing! pred sim)
          (when (<= (prediction-clearance-from pred) (prediction-clearance-until pred))
            (clearance-step! pred sim)))
        ;; A contact death is no :impact: its spheres' meeting came first.
        (cond ((and (sim-loss sim) (not (eq (loss-cause (sim-loss sim)) :contact)))
               (setf (prediction-impact pred) (sim-loss sim))
               (push-event! pred :impact (loss-time (sim-loss sim))))
              ((and cutoff (null (sim-cutoff sim)))
               (push-event! pred :cutoff (sim-t sim))))))
    (setf (prediction-last-steps pred) steps
          (prediction-last-work pred) work)
    pred))

(defun prediction-refresh! (pred sim)
  "Restart PRED from SIM if stale; the horizon from SIM's t: prediction-periods
osculating periods coasting, one as under thrust (thrust-like-p; decision
116, decision 120)."
  (declare (type prediction pred) (type sim sim))
  (when (prediction-stale-p pred sim)
    (prediction-restart! pred sim))
  (let ((period (osculating-period sim))
        (periods (if (thrust-like-p sim) 1 (prediction-periods pred))))
    (setf (prediction-now pred) (sim-t sim)
          (prediction-end pred) (cond ((prediction-until pred))
                                      (period (+ (sim-t sim) (* periods period)))
                                      (t most-positive-double-float)))
    (when (and (eq (prediction-state pred) :complete)
               (null (sim-loss (prediction-sim pred)))
               (< (sim-t (prediction-sim pred)) (prediction-end pred)))
      (setf (prediction-state pred) :running)))
  (drop-past-events! pred)
  pred)

(defun prediction-update! (pred sim &optional (budget *prediction-work-per-frame*))
  "Once per frame: restart from the flown SIM when stale (decision 75,
decision 116), else extend the tail; at most BUDGET work. PRED."
  (prediction-refresh! pred sim)
  (prediction-extend! pred budget))

(defun prediction-shoot! (pred sim until &key after-plan)
  "Restart PRED from SIM as a shot (decision 117): its horizon the fixed
time UNTIL; AFTER-PLAN: once the copy's plan is done, that t plus one
osculating period instead (UNTIL then a cap). Run it by prediction-extend!;
PRED."
  (prediction-restart! pred sim)
  (setf (prediction-now pred) (sim-t sim)
        (prediction-end pred) (float until 1d0)
        (prediction-until-plan pred) (and after-plan (sim-plan sim) t))
  pred)

(defun prediction-cut-time (pred i)
  "The t of the copy's I-th burn cut (from 0) since the restart, or nil."
  (and (< i (prediction-cut-count pred)) (aref (prediction-cut-times pred) i)))

(defun prediction-run! (pred sim)
  "PRED updated from SIM and run to its horizon (tests, scripts); PRED."
  (prediction-refresh! pred sim)
  (prediction-extend! pred most-positive-fixnum))

(defun prediction-complete-p (pred)
  (eq (prediction-state pred) :complete))

;;; Queries

(defun prediction-event (pred kind)
  "The first event of KIND (:apoapsis, :periapsis, :impact, :cutoff,
:contact, :burn-start, :closest-approach, :relative-ascending,
:relative-descending) after the flown t: (values time datum value), the
datum the copy's loss record for :impact, the object's index in
sim-objects for :contact, else the event's double value (:burn-start: the
burn's index, :closest-approach: the distance, a relative node: the planes'
angle); nil without one."
  (let ((times (prediction-event-times pred))
        (kinds (prediction-event-kinds pred))
        (amounts (prediction-event-values pred)))
    (dotimes (i (prediction-event-count pred) nil)
      (when (and (eq (svref kinds i) kind) (> (aref times i) (prediction-now pred)))
        (return (values (aref times i)
                        (case kind
                          (:impact (prediction-impact pred))
                          (:contact (svref (prediction-event-objects pred) i))
                          (t (aref amounts i)))
                        (aref amounts i)))))))

(defun prediction-apsis (pred kind)
  "(values time radius) of the next :apoapsis or :periapsis of PRED: on the
stored Hermite, or with an impact before it the osculating apsis of the
impact state (decision 87); nil if neither is known."
  (check-type kind (member :apoapsis :periapsis))
  (let ((time (prediction-event pred kind))
        (impact (prediction-event pred :impact)))
    (cond ((and time (or (null impact) (< time impact)))
           (let ((p (make-vec3)))
             (prediction-position! p pred time)
             (values time (vec3-norm p))))
          (impact
           (let* ((sim (prediction-sim pred))
                  (ship (sim-ship sim))
                  (orbit (state->orbit (ship-state-r ship) (ship-state-v ship)
                                       (body-mu (body (sim-body sim))) (sim-t sim)))
                  (time (orbit-next-apsis orbit (sim-t sim) kind)))
             (when time
               (values time (if (eq kind :apoapsis)
                                (orbit-apoapsis-radius orbit)
                                (orbit-periapsis-radius orbit)))))))))

(defun prediction-closest-approach (pred)
  "(values time distance) of the least :closest-approach after the flown t,
or nil (decision 116)."
  (let ((times (prediction-event-times pred))
        (kinds (prediction-event-kinds pred))
        (amounts (prediction-event-values pred))
        (best nil) (distance nil))
    (dotimes (i (prediction-event-count pred))
      (when (and (eq (svref kinds i) :closest-approach)
                 (> (aref times i) (prediction-now pred))
                 (or (null distance) (< (aref amounts i) distance)))
        (setf best (aref times i) distance (aref amounts i))))
    (and best (values best distance))))

(defun prediction-relative-node (pred kind)
  "(values time angle) of the next relative node, KIND :ascending or
:descending, the angle between the planes in rad; nil without one."
  (check-type kind (member :ascending :descending))
  (multiple-value-bind (time angle)
      (prediction-event pred (if (eq kind :ascending) :relative-ascending :relative-descending))
    (and time (values time angle))))

(defparameter *dropout-events*'(:impact :contact)
  "The events that drop warp: decision 75's, decision 43's contact.")

(defun prediction-dropout-event (pred &optional sim)
  "(values time kind) of the earliest drop-out event, or nil; with SIM (the
flown sim) also its plan's next hold-at while :pending, kind :burn
(decision 117), from sim-plan directly."
  (let ((best nil) (best-kind nil))
    (dolist (kind *dropout-events*)
      (let ((time (prediction-event pred kind)))
        (when (and time (or (null best) (< time best)))
          (setf best time best-kind kind))))
    (let ((plan (and sim (sim-plan sim))))
      (when (and plan (eq (sim-plan-phase sim) :pending))
        (let ((time (planned-burn-hold-at (svref (burn-plan-burns plan) (sim-plan-index sim)))))
          (when (or (null best) (< time best))
            (setf best time best-kind :burn)))))
    (and best (values best best-kind))))

(defun prediction-dropout-p (sim &optional pred)
  "True when PRED, updated from SIM and run to its horizon, meets a crash or
a contact; without PRED a fresh one."
  (let ((pred (or pred (new-prediction))))
    (prediction-run! pred sim)
    (and (prediction-dropout-event pred) t)))
