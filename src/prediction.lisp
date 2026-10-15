(in-package #:luv-in-space)

;;; Trajectory prediction, milestone 2's form: sim-step! on a copy of the
;;; sim at a coarse level, every step's t, r, v kept, and the events found
;;; on them (docs/architecture.md "Trajectory prediction"; decision 75:
;;; Prediction in milestone 2: the step function on a copy, decision 20:
;;; Trajectory prediction on the frame thread).

(defparameter *prediction-level* 1000
  "The warp level the copy steps at: coarse, kinematic, decision 75.")
(defparameter *prediction-steps-per-frame* 4096
  "sim-step!s per prediction-update!, about 1 ms; decision 72.")
(defparameter *prediction-capacity* 20000
  "Stored steps, the horizon's last bound; decision 75.")
(defparameter *prediction-deviation* 1d0
  "m: the flown r farther than this from the stored path restarts; decision 75.")
(defparameter *prediction-event-capacity* 64 "Events kept ahead of the flown t.")
(defparameter *apsis-time-tolerance* 1d-6 "s, the apsides' root; decision 75.")
(declaim (type fixnum *prediction-level* *prediction-steps-per-frame* *prediction-capacity*
               *prediction-event-capacity*)
         (type double-float *prediction-deviation* *apsis-time-tolerance*))

(defconstant +node-size+ 7)                     ; t, r, v

(defstruct (prediction (:constructor %make-prediction (nodes event-times event-kinds
                                                        event-objects))
                       (:copier nil))
  (sim nil :type (or null sim))                 ; the copy that steps ahead
  (nodes nil :type (simple-array double-float (*)))
  (head 0 :type fixnum)                         ; the node at or before the flown t
  (count 0 :type fixnum)                        ; nodes stored
  (periods 1 :type (integer 1 2))               ; the horizon in osculating periods
  (end 0d0 :type double-float)                  ; the horizon's t
  (state :empty :type (member :empty :running :complete))
  (now 0d0 :type double-float)                  ; the flown t at the last update
  (event-times nil :type (simple-array double-float (*)))
  (event-kinds nil :type simple-vector)
  (event-count 0 :type fixnum)
  (event-objects nil :type simple-vector)       ; :contact: the object's index
  (contact-starts (make-array 0 :element-type 'double-float) ; a step's start: ship, candidates
   :type (simple-array double-float (*)))
  (impact nil)                                  ; the copy's loss record
  (signature (new-controls) :type controls)     ; the controls it was made with
  (signature-cutoff nil :type (or null double-float))
  (signature-model :full :type keyword)
  (source-loss nil)                             ; the flown sim's loss at the restart
  (restarts 0 :type fixnum)
  (last-steps 0 :type fixnum))                  ; sim-step!s in the last call

(defun new-prediction (&key (periods 1))
  "An empty prediction; PERIODS 2 for the map."
  (let ((p (%make-prediction
            (make-array (* +node-size+ *prediction-capacity*) :element-type 'double-float
                                                               :initial-element 0d0)
            (make-array *prediction-event-capacity* :element-type 'double-float
                                                    :initial-element 0d0)
            (make-array *prediction-event-capacity* :initial-element nil)
            (make-array *prediction-event-capacity* :initial-element nil))))
    (setf (prediction-periods p) periods)
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
      (decf (prediction-count pred) head)
      (setf (prediction-head pred) 0))
    (let ((i (prediction-count pred)))
      (when (< i *prediction-capacity*)
        (let ((k (* +node-size+ i))
              (ship (sim-ship sim)))
          (setf (aref nodes k) (sim-t sim))
          (replace nodes (ship-state-r ship) :start1 (+ k 1))
          (replace nodes (ship-state-v ship) :start1 (+ k 4)))
        (setf (prediction-count pred) (1+ i))
        t))))

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

(defun interval-hermite! (c pred i)
  "C: the Hermite of the stored path over [t_i, t_(i+1)]; its h."
  (declare (optimize speed) (type hermite c) (type prediction pred) (type fixnum i))
  (let* ((nodes (prediction-nodes pred))
         (k0 (* +node-size+ i)) (k1 (+ k0 +node-size+))
         (h (- (aref nodes k1) (aref nodes k0))))
    (dotimes (j 3)
      (let* ((a (aref nodes (+ k0 1 j))) (b (aref nodes (+ k1 1 j)))
             (m0 (* h (aref nodes (+ k0 4 j)))) (m1 (* h (aref nodes (+ k1 4 j)))))
        (setf (aref c j) a
              (aref c (+ 3 j)) m0
              (aref c (+ 6 j)) (- (* 3 (- b a)) (* 2 m0) m1)
              (aref c (+ 9 j)) (+ (* 2 (- a b)) m0 m1))))
    h))

(defun prediction-position! (out pred time)
  "OUT: the stored path's position at TIME (the Hermite between nodes); nil
outside it."
  (declare (type vec3 out) (type prediction pred) (type double-float time))
  (let ((i (node-interval pred time))
        (nodes (prediction-nodes pred)))
    (cond ((null i) nil)
          ((= time (node-t nodes i))
           (replace out nodes :start2 (+ (* +node-size+ i) 1) :end2 (+ (* +node-size+ i) 4)))
          (t
           (let ((c (make-array 12 :element-type 'double-float))
                 (dp (make-vec3)))
             (declare (dynamic-extent c dp))
             (let ((h (interval-hermite! c pred i)))
               (hermite-point! out dp c (/ (- time (node-t nodes i)) h)))
             out)))))

;;; Events: kept in time order, dropped once the flown t passes them.

(defun push-event! (pred kind time &optional object)
  "KIND at TIME (a :contact with its OBJECT's index), unless the flown t is
there already."
  (let ((n (prediction-event-count pred)))
    (when (and (< n *prediction-event-capacity*) (> time (prediction-now pred)))
      (setf (aref (prediction-event-times pred) n) time
            (svref (prediction-event-kinds pred) n) kind
            (svref (prediction-event-objects pred) n) object
            (prediction-event-count pred) (1+ n)))))

(defun drop-past-events! (pred)
  (let* ((times (prediction-event-times pred))
         (kinds (prediction-event-kinds pred))
         (objects (prediction-event-objects pred))
         (n (prediction-event-count pred))
         (now (prediction-now pred))
         (k (loop for i of-type fixnum below n
                  while (<= (aref times i) now)
                  finally (return i))))
    (declare (type (simple-array double-float (*)) times) (type fixnum n k)
             (type double-float now))
    (when (> k 0)
      (replace times times :start2 k :end2 n)
      (replace kinds kinds :start2 k :end2 n)
      (replace objects objects :start2 k :end2 n)
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

;;; Contact (decision 43: Warp drop-outs for contact and life support,
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
    (replace buffer (ship-state-r ship) :start1 1)
    (replace buffer (ship-state-v ship) :start1 4)
    (loop for object across objects
          for i of-type fixnum from 0
          for k of-type fixnum from 7 by 7
          do (cond ((contact-candidate-p sim object i)
                    (let ((state (object-state object)))
                      (setf (aref buffer k) 1d0)
                      (replace buffer (ship-state-r state) :start1 (+ k 1))
                      (replace buffer (ship-state-v state) :start1 (+ k 4))))
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

;;; Restart and extension

(defun signature-matches-p (pred sim)
  "SIM's controls and cut-off are those PRED was made with (throttle, mode,
hold, roll offset, inertial target, keys, fine, cut-off, model)."
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
         (eq (prediction-signature-model pred) (sim-model sim)))))

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
        (prediction-source-loss pred) (sim-loss sim)
        (prediction-head pred) 0
        (prediction-count pred) 0
        (prediction-event-count pred) 0
        (prediction-impact pred) nil
        (prediction-state pred) :running)
  (incf (prediction-restarts pred))
  (push-node! pred (prediction-sim pred))
  pred)

(defun prediction-invalidate! (pred)
  "The next update restarts (after a restore)."
  (setf (prediction-state pred) :empty)
  pred)

(defun prediction-stale-p (pred sim)
  "True when PRED must restart from SIM (decision 75's rules); else moves
its head to SIM's t."
  (declare (type prediction pred) (type sim sim))
  (when (or (eq (prediction-state pred) :empty)
            (> (controls-throttle (sim-controls sim)) 0d0)
            (not (eq (sim-loss sim) (prediction-source-loss pred)))
            (not (signature-matches-p pred sim)))
    (return-from prediction-stale-p t))
  (let ((i (node-interval pred (sim-t sim))))
    (when (null i)
      (return-from prediction-stale-p t))
    (let ((p (make-vec3)))
      (declare (dynamic-extent p))
      (prediction-position! p pred (sim-t sim))
      (vec3-sub! p p (ship-state-r (sim-ship sim)))
      (when (> (vec3-norm p) *prediction-deviation*)
        (return-from prediction-stale-p t)))
    (setf (prediction-head pred) i)
    nil))

(defun prediction-extend! (pred budget)
  "Up to BUDGET sim-step!s of the copy toward the horizon, each stored with
its events; PRED."
  (declare (optimize speed) (type prediction pred) (type fixnum budget)
           (inline sim-step!))
  (let ((sim (prediction-sim pred))
        (steps 0))
    (declare (type fixnum steps))
    (loop
      (when (or (sim-loss sim)
                (>= (sim-t sim) (prediction-end pred))
                (and (>= (prediction-count pred) *prediction-capacity*)
                     (= (prediction-head pred) 0)))
        (setf (prediction-state pred) :complete)
        (return))
      (when (>= steps budget)
        (return))
      (contact-starts! pred sim)
      (let* ((cutoff (sim-cutoff sim))
             (zero (eql (sim-step-ticks! sim *prediction-level* nil) 0)))
        ;; The copy's notices and requests are nobody's (decision 112).
        (setf (sim-notices sim) nil
              (sim-checkpoint-request sim) nil)
        (incf steps)
        ;; A contact answered at the step's start: its node replaced.
        (when (and zero (> (prediction-count pred) (prediction-head pred)))
          (decf (prediction-count pred)))
        (unless (push-node! pred sim)
          (setf (prediction-state pred) :complete)
          (return))
        (unless zero
          (apsis-search! pred (- (prediction-count pred) 2))
          (contact-crossing! pred sim))
        ;; A contact death is no :impact: its spheres' meeting came first.
        (cond ((and (sim-loss sim) (not (eq (loss-cause (sim-loss sim)) :contact)))
               (setf (prediction-impact pred) (sim-loss sim))
               (push-event! pred :impact (loss-time (sim-loss sim))))
              ((and cutoff (null (sim-cutoff sim)))
               (push-event! pred :cutoff (sim-t sim))))))
    (setf (prediction-last-steps pred) steps)
    pred))

(defun prediction-refresh! (pred sim)
  "Restart PRED from SIM if stale; the horizon from SIM's t."
  (declare (type prediction pred) (type sim sim))
  (when (prediction-stale-p pred sim)
    (prediction-restart! pred sim))
  (let ((period (osculating-period sim)))
    (setf (prediction-now pred) (sim-t sim)
          (prediction-end pred) (if period
                                    (+ (sim-t sim) (* (prediction-periods pred) period))
                                    most-positive-double-float))
    (when (and (eq (prediction-state pred) :complete)
               (null (sim-loss (prediction-sim pred)))
               (< (sim-t (prediction-sim pred)) (prediction-end pred)))
      (setf (prediction-state pred) :running)))
  (drop-past-events! pred)
  pred)

(defun prediction-update! (pred sim)
  "Once per frame: restart from the flown SIM when stale (decision 75),
else extend the tail; at most *prediction-steps-per-frame* steps. PRED."
  (prediction-refresh! pred sim)
  (prediction-extend! pred *prediction-steps-per-frame*))

(defun prediction-run! (pred sim)
  "PRED updated from SIM and run to its horizon (tests, scripts); PRED."
  (prediction-refresh! pred sim)
  (prediction-extend! pred most-positive-fixnum))

(defun prediction-complete-p (pred)
  (eq (prediction-state pred) :complete))

;;; Queries

(defun prediction-event (pred kind)
  "The first event of KIND (:apoapsis, :periapsis, :impact, :cutoff,
:contact) after the flown t: its time (for :impact also the copy's loss
record, for :contact the object's index in sim-objects), or nil."
  (let ((times (prediction-event-times pred))
        (kinds (prediction-event-kinds pred)))
    (dotimes (i (prediction-event-count pred) nil)
      (when (and (eq (svref kinds i) kind) (> (aref times i) (prediction-now pred)))
        (return (case kind
                  (:impact (values (aref times i) (prediction-impact pred)))
                  (:contact (values (aref times i) (svref (prediction-event-objects pred) i)))
                  (t (aref times i))))))))

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

(defparameter *dropout-events* '(:impact :contact)
  "The events that drop warp: decision 75's, decision 43's contact.")

(defun prediction-dropout-event (pred)
  "(values time kind) of the earliest drop-out event, or nil."
  (let ((best nil) (best-kind nil))
    (dolist (kind *dropout-events*)
      (let ((time (prediction-event pred kind)))
        (when (and time (or (null best) (< time best)))
          (setf best time best-kind kind))))
    (and best (values best best-kind))))

(defun prediction-dropout-p (sim &optional pred)
  "True when PRED, updated from SIM and run to its horizon, meets a crash or
a contact; without PRED a fresh one."
  (let ((pred (or pred (new-prediction))))
    (prediction-run! pred sim)
    (and (prediction-dropout-event pred) t)))
