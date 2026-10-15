(in-package #:luv-in-space)

;;; The swept crash test and death (docs/architecture.md "Collisions and the
;;; Sun"; decision 74: Crash test on the step's Hermite and the death state,
;;; decision 34: How death works). Milestone 2: Earth (R + 100 km, the
;;; central body) and the Moon (R, a third body) from Earth's frame.

(defstruct (loss (:constructor make-loss (cause body time speed latitude longitude
                                          &optional object)))
  (cause :crash :type keyword :read-only t)         ; :crash or :contact
  (body :earth :type keyword :read-only t)
  (time 0d0 :type double-float :read-only t)        ; s, the impact, off the grid
  (speed 0d0 :type double-float :read-only t)       ; m/s, relative to the body; :contact the normal speed
  (latitude 0d0 :type double-float :read-only t)    ; rad, body-fixed
  (longitude 0d0 :type double-float :read-only t)   ; rad, east, (−π, π]
  (object nil :type (or null keyword) :read-only t)) ; :contact: the object's key

(defun sim-lost-p (sim) (and (sim-loss sim) t))

;;; The step's path relative to a body: p(s) = p0 + m0 s + c2 s² + c3 s³,
;;; s in [0, 1], through the two relative states (m = h v).

(deftype hermite () '(simple-array double-float (12)))   ; p0, m0, c2, c3

(declaim (inline hermite-set! hermite-point! hermite-sag hermite-crossing))

(defun hermite-set! (c p0 v0 p1 v1 h)
  (declare (type hermite c) (type vec3 p0 v0 p1 v1) (type double-float h))
  (dotimes (i 3 c)
    (let* ((a (aref p0 i)) (b (aref p1 i))
           (m0 (* h (aref v0 i))) (m1 (* h (aref v1 i))))
      (setf (aref c i) a
            (aref c (+ 3 i)) m0
            (aref c (+ 6 i)) (- (* 3 (- b a)) (* 2 m0) m1)
            (aref c (+ 9 i)) (+ (* 2 (- a b)) m0 m1)))))

(defun hermite-point! (p dp c s)
  "p(s) into P, dp/ds into DP."
  (declare (type vec3 p dp) (type hermite c) (type double-float s))
  (dotimes (i 3)
    (let ((m0 (aref c (+ 3 i))) (c2 (aref c (+ 6 i))) (c3 (aref c (+ 9 i))))
      (setf (aref p i) (+ (aref c i) (* s (+ m0 (* s (+ c2 (* s c3))))))
            (aref dp i) (+ m0 (* s (+ (* 2 c2) (* 3 s c3))))))))

(defun hermite-sag (c)
  "max(|p''(0)|, |p''(1)|) / 8: p'' is linear in s, so it bounds the
cubic's distance from its chord exactly."
  (declare (type hermite c))
  (let ((a 0d0) (b 0d0))
    (declare (type (double-float 0d0) a b))
    (dotimes (i 3)
      (let ((c2 (aref c (+ 6 i))) (c3 (aref c (+ 9 i))))
        (incf a (* c2 c2))
        (incf b (expt (+ c2 (* 3 c3)) 2))))
    (* 0.25d0 (sqrt (max a b)))))

(defparameter *crash-s-tolerance* 1d-12
  "Newton's stop in s, below decision 74's 1e-9.")
(declaim (type double-float *crash-s-tolerance*))

(defun hermite-crossing (c radius sag)
  "The first s in [0, 1] with |p(s)| = RADIUS on C: 0 if it starts inside,
−1 if it stays outside. The chord's closest approach less SAG first; only
below RADIUS is |p|² minimised by Newton on p·p' from the chord's
parameter (endpoints included), then the crossing by bracketed Newton."
  (declare (type hermite c) (type double-float radius sag))
  (let* ((p0x (aref c 0)) (p0y (aref c 1)) (p0z (aref c 2))
         (dx (+ (aref c 3) (aref c 6) (aref c 9)))
         (dy (+ (aref c 4) (aref c 7) (aref c 10)))
         (dz (+ (aref c 5) (aref c 8) (aref c 11)))
         (dd (+ (* dx dx) (* dy dy) (* dz dz)))
         (s-chord (if (> dd 0d0)
                      (max 0d0 (min 1d0 (/ (- (+ (* p0x dx) (* p0y dy) (* p0z dz))) dd)))
                      0d0))
         (qx (+ p0x (* s-chord dx))) (qy (+ p0y (* s-chord dy))) (qz (+ p0z (* s-chord dz)))
         (reach (+ radius sag))
         (r2 (* radius radius)))
    (when (> (+ (* qx qx) (* qy qy) (* qz qz)) (* reach reach))
      (return-from hermite-crossing -1d0))
    (let ((p (make-vec3)) (dp (make-vec3)) (tolerance *crash-s-tolerance*))
      (declare (dynamic-extent p dp))
      (flet ((at (s)
               ;; (values |p|² p·p' d(p·p')/ds)
               (hermite-point! p dp c s)
               (let ((pdd 0d0))
                 (declare (type double-float pdd))
                 (dotimes (i 3)
                   (incf pdd (* (aref p i) (+ (* 2 (aref c (+ 6 i)))
                                              (* 6 s (aref c (+ 9 i)))))))
                 (values (vec3-dot p p) (vec3-dot p dp) (+ (vec3-dot dp dp) pdd)))))
        (declare (inline at))
        (let ((f0 (+ (* p0x p0x) (* p0y p0y) (* p0z p0z)) ))
          (when (<= f0 r2)
            (return-from hermite-crossing 0d0))
          (let ((s-min
                  (cond ((>= (nth-value 1 (at 0d0)) 0d0) 0d0)
                        ((<= (nth-value 1 (at 1d0)) 0d0) 1d0)
                        (t
                         ;; p·p' from − to +: safeguarded Newton.
                         (let ((lo 0d0) (hi 1d0)
                               (s (if (< 0d0 s-chord 1d0) s-chord 0.5d0)))
                           (declare (type double-float lo hi s))
                           (loop repeat 100
                                 do (multiple-value-bind (f g dg) (at s)
                                      (declare (ignore f))
                                      (if (< g 0d0) (setf lo s) (setf hi s))
                                      (let ((next (if (> dg 0d0) (- s (/ g dg)) -1d0)))
                                        (unless (< lo next hi)
                                          (setf next (* 0.5d0 (+ lo hi))))
                                        (when (or (< (abs (- next s)) tolerance)
                                                  (< (- hi lo) tolerance))
                                          (return))
                                        (setf s next))))
                           s)))))
            (declare (type double-float s-min))
            (let ((f-min (- (the double-float (at s-min)) r2)))
              (when (> f-min 0d0)
                (return-from hermite-crossing -1d0))
              ;; |p|² − R² from + at 0 to ≤ 0 at s-min.
              (let* ((f0 (- f0 r2))
                     (lo 0d0) (hi s-min)
                     (s (* s-min (/ f0 (- f0 f-min)))))
                (declare (type double-float lo hi s))
                (loop repeat 100
                      do (multiple-value-bind (f g) (at s)
                           (let ((f (- f r2)))
                             (if (> f 0d0) (setf lo s) (setf hi s))
                             (let ((next (if (< g 0d0) (- s (/ f (* 2 g))) -1d0)))
                               (unless (< lo next hi)
                                 (setf next (* 0.5d0 (+ lo hi))))
                               (when (or (< (abs (- next s)) tolerance)
                                         (< (- hi lo) tolerance))
                                 (return))
                               (setf s next)))))
                s))))))))

;;; The test after each step, and death

(declaim (inline swept-crash))
(defun swept-crash (earth moon r0 v0 r1 v1 h t1 central)
  "The swept test of one step of H from R0, V0 to R1, V1 at T1 (CENTRAL's
frame) against CENTRAL's crash sphere and, from Earth, the Moon's (decision
74): (values s moon-p), s in [0, 1] the earliest crossing (−1 for none),
MOON-P true when it is the Moon's. EARTH gets the path, MOON the path
relative to the Moon when it was tested. Shared by the ship and the
promoted objects; conses nothing."
  (declare (type hermite earth moon) (type vec3 r0 v0 r1 v1) (type body central)
           (type double-float h t1))
  (let ((earth-sag 0d0) (s-earth -1d0) (s-moon -1d0))
    (declare (type double-float earth-sag s-earth s-moon))
    (hermite-set! earth r0 v0 r1 v1 h)
    (setf earth-sag (hermite-sag earth)
          s-earth (hermite-crossing earth (body-crash-radius central) earth-sag))
    ;; The Moon, only where the path's reach could meet its least distance.
    (when (eq (body-key central) :earth)
      (let ((radius (load-time-value (body-crash-radius (body :moon)) t))
            (reach (+ (sqrt (the (double-float 0d0) (max (vec3-dot r0 r0) (vec3-dot r1 r1))))
                      earth-sag)))
        (declare (type double-float radius))
        (when (> (+ reach radius) +moon-distance-min+)
          (let ((m0 (make-vec3)) (mv0 (make-vec3)) (m1 (make-vec3)) (mv1 (make-vec3)))
            (declare (dynamic-extent m0 mv0 m1 mv1))
            (body-state-into m0 mv0 :moon (- t1 h) :earth)
            (body-state-into m1 mv1 :moon t1 :earth)
            (vec3-sub! m0 r0 m0) (vec3-sub! mv0 v0 mv0)
            (vec3-sub! m1 r1 m1) (vec3-sub! mv1 v1 mv1)
            (hermite-set! moon m0 mv0 m1 mv1 h)
            (setf s-moon (hermite-crossing moon radius (hermite-sag moon)))))))
    (if (and (>= s-moon 0d0) (or (< s-earth 0d0) (< s-moon s-earth)))
        (values s-moon t)
        (values s-earth nil))))

(declaim (ftype (function (sim vec3 vec3 quat fixnum) (values boolean &optional))
                crash-test!))

(defun crash-test! (sim r0 v0 q0 ticks)
  "Test SIM's last step, of TICKS 1/64 s from R0, V0, Q0 at (sim-t sim) − h
to its state now, against the central body's crash sphere and the Moon's
(swept-crash); the earliest crossing kills: the ship at the crossing's
Hermite state, t the impact time, the loss record. True if it did. Conses
nothing when nothing is hit."
  (declare (optimize speed) (type sim sim) (type vec3 r0 v0) (type quat q0)
           (type fixnum ticks))
  (let* ((ship (sim-ship sim))
         (h (* ticks +fine-step+))
         (central (body (sim-body sim)))
         (earth (make-array 12 :element-type 'double-float))
         (moon (make-array 12 :element-type 'double-float)))
    (declare (dynamic-extent earth moon) (type body central) (type double-float h))
    (multiple-value-bind (s moon-p)
        (swept-crash earth moon r0 v0 (ship-state-r ship) (ship-state-v ship) h
                     (sim-t sim) central)
      (declare (type double-float s))
      (when (< s 0d0)
        (return-from crash-test! nil))
      ;; The crossing goes in a cell: a double argument would be boxed on
      ;; every step.
      (let ((s-cell (make-array 1 :element-type 'double-float)))
        (declare (dynamic-extent s-cell))
        (setf (aref s-cell 0) s)
        (die! sim (if moon-p (body :moon) central) (if moon-p moon earth) earth
              q0 ticks s-cell)))
    t))

(defun die! (sim body relative path q0 ticks s-cell)
  "SIM's ship at the crossing s of its last step of TICKS (PATH,
Earth-relative; RELATIVE, to BODY): position and velocity from the
Hermite, attitude slerped from Q0, ω and m_p of the step's end, a latched
load with it; t the impact time; the loss record. SIM."
  (declare (type sim sim) (type body body) (type hermite relative path)
           (type fixnum ticks) (type (simple-array double-float (1)) s-cell))
  (let* ((ship (sim-ship sim))
         (s (aref s-cell 0))
         (h (* ticks +fine-step+))
         (time (+ (- (sim-t sim) h) (* s h)))
         (p (make-vec3)) (dp (make-vec3)))
    (hermite-point! p dp relative s)
    (multiple-value-bind (latitude longitude) (latitude-longitude body p time)
      (let ((loss (make-loss :crash (body-key body) time (/ (vec3-norm dp) h)
                             latitude longitude)))
        (hermite-point! (ship-state-r ship) dp path s)
        (vec3-scale! (ship-state-v ship) dp (/ h))
        (replace (ship-state-q ship) (quat-slerp q0 (ship-state-q ship) s))
        (setf (sim-t sim) time
              (sim-loss sim) loss)))
    (latched-load-update! sim)))

;;; Removal of objects (docs/architecture.md "Removal of objects"; decision
;;; 46: Rails across spheres, hyperbolas and re-entry).

(defun object-remove! (sim object time cause &optional point (body (sim-body sim)))
  "OBJECT removed from SIM at TIME: status :removed and its record (TIME,
BODY, CAUSE :burned-up, :delivered or :impact, POINT: (latitude longitude)
of a crash in radians, a delivery's receiving object and fixture). From
then it is never stepped, promoted, crash-tested or given a state; a
checkpoint from before brings it back. :burned-up writes the notice
\"<NAME> BURNED UP\" (decision 112). OBJECT."
  (setf (object-status object) :removed
        (object-removal object) (make-removal (float time 1d0) body cause point))
  (when (eq cause :burned-up)
    (sim-notice! sim :removal
                 (format nil "~A BURNED UP" (object-def-name (object-def-of object)))))
  object)

(defun crash-point (body p time)
  "(latitude longitude) of BODY-centred P at TIME."
  (multiple-value-list (latitude-longitude body p time)))

(defun object-crash-tests! (sim before ticks)
  "After SIM's step of TICKS 1/64 s: each promoted object's swept crash test
(BEFORE: the step's end t, then each object's r and v at the step's start
by its index, 6 doubles), a crossing removing it at that time; then each
rails object whose segment ends :crash at or before the step's end removed
at that end. Conses nothing unless an object is removed."
  (declare (optimize speed) (type sim sim) (type (simple-array double-float (*)) before)
           (type fixnum ticks))
  (let* ((t1 (aref before 0))
         (h (* ticks +fine-step+))
         (central (body (sim-body sim)))
         (earth (make-array 12 :element-type 'double-float))
         (moon (make-array 12 :element-type 'double-float))
         (r0 (make-vec3)) (v0 (make-vec3)) (p (make-vec3)) (dp (make-vec3)))
    (declare (dynamic-extent earth moon r0 v0 p dp) (type body central) (type double-float h t1))
    (loop for object across (sim-objects sim)
          for i of-type fixnum from 1 by 6
          do (case (object-status object)
               (:promoted
                (let ((state (object-state object)))
                  (dotimes (k 3)
                    (setf (aref r0 k) (aref before (+ i k))
                          (aref v0 k) (aref before (+ i 3 k))))
                  (multiple-value-bind (s moon-p)
                      (swept-crash earth moon r0 v0 (ship-state-r state) (ship-state-v state)
                                   h t1 central)
                    (declare (type double-float s))
                    (when (>= s 0d0)
                      (let ((body (if moon-p (body :moon) central))
                            (time (+ (- t1 h) (* s h))))
                        (hermite-point! p dp (if moon-p moon earth) s)
                        (object-remove! sim object time (if moon-p :impact :burned-up)
                                        (crash-point body p time) (body-key body)))))))
               (:rails
                (let ((segment (rails-segment-at (object-rails object) t1)))
                  (when (and (eq (rails-segment-end-kind segment) :crash)
                             (<= (rails-segment-end segment) t1))
                    (let ((body (body (rails-segment-body segment)))
                          (end (rails-segment-end segment)))
                      (rails-state-into! p dp segment end)
                      (object-remove! sim object end :burned-up (crash-point body p end)
                                      (body-key body))))))))
    sim))

;;; Contact with objects (docs/architecture.md "Collisions and the Sun";
;;; decision 44: Contact, capture and berthing in the simulation, decision
;;; 112, decision 113): the stack against each promoted object after the
;;; crash tests; objects never against each other. The stack is the ship and
;;; its latched load; while armed the grapple's tip is tested first.

(defparameter *contact-substep* 0.25d0
  "m of relative motion between narrow-phase samples at most; decision 44.")
(defparameter *contact-time-tolerance* 1d-4 "s: the first overlap's bisection; decision 44.")
(defparameter *contact-restitution* 0.3d0 "decision 44.")
(defparameter *contact-death-speed* 1d0 "m/s: a faster normal speed kills; decision 33.")
(defparameter *grapple-reach* 0.8d0
  "m: a fixture's plate crossed by the tip this near its pin is a grapple contact; decision 113.")
(defparameter *grapple-rating* 100d3 "kg: a towable fixture's rating; decision 33.")
(defparameter *claws-closing-time* 2d0 "s; decision 33.")
(declaim (type double-float *contact-substep* *contact-time-tolerance* *contact-restitution*
               *contact-death-speed* *grapple-reach* *grapple-rating* *claws-closing-time*))

(defstruct (contact (:constructor make-contact ()) (:copier nil))
  "Outside the sim (decision 112): the scratch a step's start is saved
into, and the last contact responded to."
  (scratch nil :type (or null sim))
  (saved nil :type boolean)                    ; the scratch holds this step's start
  (count 0 :type fixnum)                       ; contacts responded to
  (object 0 :type fixnum)                      ; the last one's object, by index
  (time 0d0 :type double-float)                ; s, its t_c
  (speed 0d0 :type double-float)               ; m/s, the points' relative velocity along n, < 0
  (normal (make-vec3) :type vec3)              ; unit, inertial, from the stack to the object
  (r-s (make-vec3) :type vec3)                 ; m, the point from the stack's CoM, inertial
  (r-o (make-vec3) :type vec3))                ; m, the point from the object's CoM

(defvar *contact* (make-contact))
(declaim (type contact *contact*))

(defmacro with-stack-properties ((properties sim &optional (load (gensym "LOAD"))) &body body)
  "BODY with PROPERTIES SIM's stack's mass properties and LOAD its latched
load (with-stack-load), stack-allocated."
  (let ((com (gensym)) (inertia (gensym)) (inverse (gensym)))
    `(with-stack-load (,load ,sim)
       (let* ((,com (make-vec3)) (,inertia (make-mat3)) (,inverse (make-mat3))
              (,properties (make-mass-properties :com ,com :inertia ,inertia
                                                 :inertia-inverse ,inverse)))
         (declare (dynamic-extent ,com ,inertia ,inverse ,properties))
         (locally (declare (inline ship-mass-properties))
           (ship-mass-properties (ship-state-m-p (sim-ship ,sim)) ,load ,properties))
         ,@body))))

(declaim (inline contact-candidate-p))
(defun contact-candidate-p (sim object index)
  "OBJECT, SIM's INDEXth, can touch the stack: promoted, not excluded, not
the latched load or the berth station."
  (declare (type sim sim) (type object object) (type fixnum index))
  (and (eq (object-status object) :promoted)
       (not (eql index (sim-excluded sim)))
       (not (eql index (grapple-object (sim-grapple sim))))))

(defun contact-within-reach-p (sim level until)
  "True when a candidate's bounding sphere may meet the stack's in SIM's
next step at LEVEL with UNTIL: the distance against both bounds, 1 m and
twice the relative motion's reach (speed, full engine and RCS, the tidal
term 4 μ d / r³). Conses nothing."
  (declare (optimize speed) (type sim sim) (type fixnum level) (type (or null double-float) until)
           (inline step-h stack-bound))
  (let ((objects (sim-objects sim)))
    (unless (loop for object across objects
                  for i of-type fixnum from 0
                  thereis (contact-candidate-p sim object i))
      (return-from contact-within-reach-p nil))
    (with-stack-properties (properties sim load)
      (let* ((ship (sim-ship sim))
             (h (step-h sim level until))
             (bound (stack-bound (mass-properties-com properties) load))
             (r (vec3-norm (ship-state-r ship)))
             (mu (body-mu (body (sim-body sim))))
             (thrust (/ (+ *engine-thrust* (* +rcs-count+ *rcs-thrust*)) *ship-dry-mass*)))
        (declare (type double-float h r))
        (flet ((distance (a b)
                 (declare (type vec3 a b))
                 (let ((dx (- (aref a 0) (aref b 0))) (dy (- (aref a 1) (aref b 1)))
                       (dz (- (aref a 2) (aref b 2))))
                   (sqrt (the (double-float 0d0) (+ (* dx dx) (* dy dy) (* dz dz)))))))
          (loop for object across objects
                for i of-type fixnum from 0
                thereis (and (contact-candidate-p sim object i)
                             (let* ((state (object-state object))
                                    (d (distance (ship-state-r state) (ship-state-r ship)))
                                    (w (distance (ship-state-v state) (ship-state-v ship)))
                                    (a (+ thrust (/ (* 4 mu (+ d (* w h))) (* r r r)))))
                               (<= d (+ bound (object-def-bound (object-def-of object)) 1d0
                                        (* 2 (+ (* w h) (* 0.5d0 a h h)))))))))))))

(defun same-objects-p (a b)
  "Sims A and B hold the same objects' definitions."
  (let ((x (sim-objects a)) (y (sim-objects b)))
    (and (= (length x) (length y))
         (every (lambda (o p) (eq (object-def-of o) (object-def-of p))) x y))))

(defun contact-save! (sim level until)
  "Before SIM's step (decision 112): its state saved into the scratch when a
candidate is within reach (contact-within-reach-p); the scratch made on the
first save. Conses nothing after that."
  (declare (type sim sim) (type fixnum level) (type (or null double-float) until))
  (let ((contact *contact*))
    (setf (contact-saved contact) nil)
    (when (contact-within-reach-p sim level until)
      (let ((scratch (contact-scratch contact)))
        (unless (and scratch (same-objects-p scratch sim))
          (setf scratch (copy-sim sim)
                (contact-scratch contact) scratch))
        (sim-assign! scratch sim)
        (setf (contact-saved contact) t)))
    nil))

;;; Shapes: a box is its centre, its rotation (row-major, column i its axis
;;; i) and its half-extents; a sphere its centre and radius. OUT: depth,
;;; normal from the stack's shape to the object's, contact point.

(deftype contact-out () '(simple-array double-float (7)))

(declaim (inline quat-matrix! column-dot box-support! box-closest!))

(defun quat-matrix! (m q)
  "Q's rotation matrix into M, row-major."
  (declare (type mat3 m) (type quat q))
  (with-quat (((w x y z) q))
    (setf (aref m 0) (- 1 (* 2 (+ (* y y) (* z z)))) (aref m 1) (* 2 (- (* x y) (* w z)))
          (aref m 2) (* 2 (+ (* x z) (* w y))) (aref m 3) (* 2 (+ (* x y) (* w z)))
          (aref m 4) (- 1 (* 2 (+ (* x x) (* z z)))) (aref m 5) (* 2 (- (* y z) (* w x)))
          (aref m 6) (* 2 (- (* x z) (* w y))) (aref m 7) (* 2 (+ (* y z) (* w x)))
          (aref m 8) (- 1 (* 2 (+ (* x x) (* y y))))))
  m)

(defun column-dot (m i v)
  "Column I of M dotted with V."
  (declare (type mat3 m) (type (integer 0 2) i) (type vec3 v))
  (+ (* (aref m i) (aref v 0)) (* (aref m (+ 3 i)) (aref v 1)) (* (aref m (+ 6 i)) (aref v 2))))

(defun box-support! (out c m e direction toward)
  "The box's farthest point along DIRECTION into OUT; an axis nearly
perpendicular to it takes TOWARD's coordinate, clamped (a face's or edge's
point nearest TOWARD)."
  (declare (type vec3 out c e direction toward) (type mat3 m))
  (let ((d (make-vec3)) (x 0d0) (y 0d0) (z 0d0))
    (declare (dynamic-extent d) (type double-float x y z))
    (vec3-sub! d toward c)
    (flet ((coordinate (i)
             (let ((s (column-dot m i direction)) (half (aref e i)))
               (if (< (abs s) 1d-6)
                   (max (- half) (min half (column-dot m i d)))
                   (if (plusp s) half (- half))))))
      (setf x (coordinate 0) y (coordinate 1) z (coordinate 2)))
    (dotimes (k 3 out)
      (setf (aref out k) (+ (aref c k) (* x (aref m (* 3 k))) (* y (aref m (+ 1 (* 3 k))))
                            (* z (aref m (+ 2 (* 3 k)))))))))

(defun box-closest! (out p c m e)
  "The box's point nearest P into OUT (P itself inside)."
  (declare (type vec3 out p c e) (type mat3 m))
  (let ((d (make-vec3)) (x 0d0) (y 0d0) (z 0d0))
    (declare (dynamic-extent d) (type double-float x y z))
    (vec3-sub! d p c)
    (flet ((coordinate (i)
             (let ((half (aref e i))) (max (- half) (min half (column-dot m i d))))))
      (setf x (coordinate 0) y (coordinate 1) z (coordinate 2)))
    (dotimes (k 3 out)
      (setf (aref out k) (+ (aref c k) (* x (aref m (* 3 k))) (* y (aref m (+ 1 (* 3 k))))
                            (* z (aref m (+ 2 (* 3 k)))))))))

(defun box-box-contact! (out ca ma ea cb mb eb)
  "Boxes A and B by their 15 separating axes: nil when one separates them;
else true, OUT the least penetration's depth and axis (A to B; an edge
pair's axis must be 5 % shallower to win over a face's) and the midpoint of
each box's deepest point brought into the other."
  (declare (optimize speed) (type contact-out out) (type vec3 ca ea cb eb) (type mat3 ma mb))
  (let ((d (make-vec3)) (axis (make-vec3)) (best (make-vec3)) (pa (make-vec3)) (pb (make-vec3))
        (minus (make-vec3))
        (best-depth most-positive-double-float))
    (declare (dynamic-extent d axis best pa pb minus) (type double-float best-depth))
    (vec3-sub! d cb ca)
    (flet ((separates-p (bias)
             ;; AXIS, unit: true when it separates; else its depth kept if least.
             (let* ((dist (vec3-dot d axis))
                    (depth (- (+ (* (aref ea 0) (abs (column-dot ma 0 axis)))
                                 (* (aref ea 1) (abs (column-dot ma 1 axis)))
                                 (* (aref ea 2) (abs (column-dot ma 2 axis)))
                                 (* (aref eb 0) (abs (column-dot mb 0 axis)))
                                 (* (aref eb 1) (abs (column-dot mb 1 axis)))
                                 (* (aref eb 2) (abs (column-dot mb 2 axis))))
                              (abs dist))))
               (declare (type double-float bias dist depth))
               (cond ((< depth 0d0) t)
                     (t (when (< (* bias depth) best-depth)
                          (setf best-depth depth)
                          (vec3-scale! best axis (if (minusp dist) -1d0 1d0)))
                        nil)))))
      (flet ((faces (m)
               (declare (type mat3 m))
               (dotimes (i 3)
                 (vec3-set! axis (aref m i) (aref m (+ 3 i)) (aref m (+ 6 i)))
                 (when (separates-p 1d0)
                   (return-from box-box-contact! nil)))))
        (faces ma)
        (faces mb))
      (dotimes (i 3)
        (dotimes (j 3)
          (let ((ax (aref ma i)) (ay (aref ma (+ 3 i))) (az (aref ma (+ 6 i)))
                (bx (aref mb j)) (by (aref mb (+ 3 j))) (bz (aref mb (+ 6 j))))
            (vec3-set! axis (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))
            (let ((length (vec3-norm axis)))
              (when (> length 1d-9)
                (vec3-scale! axis axis (/ length))
                (when (separates-p 1.05d0)
                  (return-from box-box-contact! nil))))))))
    (box-support! pa ca ma ea best cb)
    (vec3-scale! minus best -1d0)
    (box-support! pb cb mb eb minus ca)
    (box-closest! pa pa cb mb eb)
    (box-closest! pb pb ca ma ea)
    (setf (aref out 0) best-depth)
    (dotimes (k 3 t)
      (setf (aref out (+ 1 k)) (aref best k)
            (aref out (+ 4 k)) (* 0.5d0 (+ (aref pa k) (aref pb k)))))))

(declaim (inline box-sphere-contact!))
(defun box-sphere-contact! (out ca ma ea cb radius)
  "Box A and the sphere B by the box's point nearest B's centre: nil when
apart; else true, OUT the depth, the normal (A to B) and the midpoint of
the two deepest points."
  (declare (optimize speed) (type contact-out out) (type vec3 ca ea cb) (type mat3 ma)
           (type double-float radius))
  (let ((q (make-vec3)) (d (make-vec3)) (n (make-vec3)) (local (make-vec3)))
    (declare (dynamic-extent q d n local))
    (box-closest! q cb ca ma ea)
    (vec3-sub! d cb q)
    (let ((dist (vec3-norm d)))
      (when (>= dist radius)
        (return-from box-sphere-contact! nil))
      (cond ((> dist 1d-9)
             (vec3-scale! n d (/ dist))
             (setf (aref out 0) (- radius dist)))
            (t
             ;; The centre inside: the face it is nearest.
             (vec3-sub! d cb ca)
             (dotimes (i 3) (setf (aref local i) (column-dot ma i d)))
             (let ((k 0) (least most-positive-double-float))
               (declare (type (integer 0 2) k) (type double-float least))
               (dotimes (i 3)
                 (let ((gap (- (aref ea i) (abs (aref local i)))))
                   (when (< gap least) (setf least gap k i))))
               (let ((sign (if (minusp (aref local k)) -1d0 1d0)))
                 (vec3-set! n (* sign (aref ma k)) (* sign (aref ma (+ 3 k))) (* sign (aref ma (+ 6 k))))
                 (setf (aref out 0) (+ radius least))
                 (vec3-add-scaled! q cb n least)))))
      (dotimes (k 3 t)
        (setf (aref out (+ 1 k)) (aref n k)
              (aref out (+ 4 k)) (* 0.5d0 (+ (aref q k) (- (aref cb k) (* radius (aref n k))))))))))

(declaim (inline sphere-sphere-contact! flip-normal!))

(defun sphere-sphere-contact! (out ca ra cb rb)
  "Spheres A and B: nil when apart; else true, OUT the depth, the normal (A
to B) and the midpoint of the two deepest points."
  (declare (optimize speed) (type contact-out out) (type vec3 ca cb) (type double-float ra rb))
  (let ((n (make-vec3)))
    (declare (dynamic-extent n))
    (vec3-sub! n cb ca)
    (let ((dist (vec3-norm n)))
      (when (>= dist (+ ra rb))
        (return-from sphere-sphere-contact! nil))
      (if (> dist 1d-9)
          (vec3-scale! n n (/ dist))
          (vec3-set! n 1d0 0d0 0d0))
      (setf (aref out 0) (- (+ ra rb) dist))
      (dotimes (k 3 t)
        (setf (aref out (+ 1 k)) (aref n k)
              (aref out (+ 4 k)) (* 0.5d0 (+ (aref ca k) (* ra (aref n k))
                                             (aref cb k) (- (* rb (aref n k))))))))))

(defun flip-normal! (out)
  "OUT's normal reversed; true."
  (declare (type contact-out out))
  (dotimes (k 3 t)
    (setf (aref out (+ 1 k)) (- (aref out (+ 1 k))))))

(defun stack-object-contact! (out p q-s q-o com def load)
  "The stack's shapes (its CoM at the origin, attitude Q-S, COM its CoM in
body axes from R: the ship's boxes and LOAD's shapes, a stack-load or nil)
against DEF's (its CoM at P, attitude Q-O), inertial axes: nil when none
overlap; else true, OUT the deepest pair's depth, normal and point. Conses
nothing."
  (declare (optimize speed) (type contact-out out) (type vec3 p com) (type quat q-s q-o)
           (type object-def def) (type (or null stack-load) load))
  (let ((ms (make-mat3)) (mo (make-mat3)) (ml (make-mat3)) (q-l (make-quat))
        (ca (make-vec3)) (ea (make-vec3)) (cb (make-vec3)) (eb (make-vec3)) (local (make-vec3))
        (pair (make-array 7 :element-type 'double-float))
        (hit nil))
    (declare (dynamic-extent ms mo ml q-l ca ea cb eb local pair))
    (quat-matrix! ms q-s)
    (quat-matrix! mo q-o)
    (flet ((against-object (box-p ma radius)
             ;; The stack's shape at CA (a box: MA, EA; else a sphere of
             ;; RADIUS) against each of DEF's.
             (declare (type mat3 ma) (type double-float radius))
             (dolist (shape (object-def-shapes def))
               (mat3*vec3! cb mo (shape-centre shape))
               (vec3-add! cb cb p)
               (when (if (eq (shape-kind shape) :box)
                         (progn (vec3-scale! eb (shape-size shape) 0.5d0)
                                (if box-p
                                    (box-box-contact! pair ca ma ea cb mo eb)
                                    (and (box-sphere-contact! pair cb mo eb ca radius)
                                         (flip-normal! pair))))
                         (if box-p
                             (box-sphere-contact! pair ca ma ea cb (shape-radius shape))
                             (sphere-sphere-contact! pair ca radius cb (shape-radius shape))))
                 (when (or (not hit) (> (aref pair 0) (aref out 0)))
                   (replace out pair)
                   (setf hit t))))))
      (loop for box across *ship-collision-boxes*
            do (vec3-sub! local (shape-centre box) com)
               (mat3*vec3! ca ms local)
               (vec3-scale! ea (shape-size box) 0.5d0)
               (against-object t ms 0d0))
      (when load
        (quat-mul! q-l q-s (stack-load-q load))
        (quat-matrix! ml q-l)
        (dolist (shape (object-def-shapes (stack-load-def load)))
          ;; Its centre: q-s (load CoM − com) + q-l centre.
          (vec3-sub! local (stack-load-com load) com)
          (mat3*vec3! ca ms local)
          (mat3*vec3! local ml (shape-centre shape))
          (vec3-add! ca ca local)
          (if (eq (shape-kind shape) :box)
              (progn (vec3-scale! ea (shape-size shape) 0.5d0)
                     (against-object t ml 0d0))
              (against-object nil ml (shape-radius shape))))))
    hit))

(declaim (inline slerp-angle))
(defun slerp-angle (a b)
  "rad: the angle quat-slerp! turns through from A to B."
  (declare (type quat a b))
  (let ((d (+ (* (aref a 0) (aref b 0)) (* (aref a 1) (aref b 1))
              (* (aref a 2) (aref b 2)) (* (aref a 3) (aref b 3)))))
    (* 2 (acos (min 1d0 (abs d))))))

;;; s, speed, n, r-s, r-o and the grapple contact's fixture index (−1: shapes)
(deftype contact-result () '(simple-array double-float (12)))

(declaim (inline contact-approach! contact-find!))

(defun contact-approach! (result s h out p dp q-s q-o s0 s1 o0 o1)
  "At S of the step (OUT the overlap there, P and DP the relative Hermite,
Q-S and Q-O the attitudes, ω linear from the states S0 to S1 and O0 to
O1): true when the contact points approach, RESULT then filled (a shapes'
contact)."
  (declare (optimize speed) (type contact-result result) (type double-float s h)
           (type contact-out out) (type vec3 p dp) (type quat q-s q-o)
           (type ship-state s0 s1 o0 o1))
  (let ((n (make-vec3)) (r-s (make-vec3)) (r-o (make-vec3)) (w (make-vec3)) (wi (make-vec3))
        (u (make-vec3)) (x (make-vec3)))
    (declare (dynamic-extent n r-s r-o w wi u x))
    (vec3-set! n (aref out 1) (aref out 2) (aref out 3))
    (vec3-set! r-s (aref out 4) (aref out 5) (aref out 6))
    (vec3-sub! r-o r-s p)
    (vec3-scale! u dp (/ h))
    (vec3-lerp! w (ship-state-w o0) (ship-state-w o1) s)
    (quat-rotate! wi q-o w)
    (vec3-add! u u (vec3-cross! x wi r-o))
    (vec3-lerp! w (ship-state-w s0) (ship-state-w s1) s)
    (quat-rotate! wi q-s w)
    (vec3-sub! u u (vec3-cross! x wi r-s))
    (let ((speed (vec3-dot u n)))
      (when (< speed 0d0)
        (setf (aref result 0) s
              (aref result 1) speed
              (aref result 11) -1d0)
        (replace result n :start1 2)
        (replace result r-s :start1 5)
        (replace result r-o :start1 8)
        t))))

(defun contact-find! (result sim scratch index com bound h load armed)
  "SIM's step of H from SCRATCH against its INDEXth object: the broad phase
on the relative Hermite against BOUND and the object's, then samples from
the spheres' meeting at most *contact-substep* of relative motion apart (or
as far as the spheres' gap allows). At each, while ARMED, the grapple's tip
first: a free fixture's plate crossed since the last sample within
*grapple-reach* of its pin, the crossing bisected to
*contact-time-tolerance*, is a grapple contact unless the shapes (the ship's
boxes and LOAD's) overlap there; then the shapes, the first overlap bisected
likewise. True when it is approaching, RESULT then s, the normal speed, n,
r-s, r-o and the fixture's index (−1 for shapes) at it."
  (declare (optimize speed) (type contact-result result) (type sim sim scratch)
           (type fixnum index) (type vec3 com) (type double-float bound h)
           (type (or null stack-load) load))
  (let* ((object (svref (sim-objects sim) index))
         (def (object-def-of object))
         (o0 (object-state (svref (sim-objects scratch) index)))
         (o1 (object-state object))
         (s0 (sim-ship scratch))
         (s1 (sim-ship sim))
         (c (make-array 12 :element-type 'double-float))
         (p0 (make-vec3)) (u0 (make-vec3)) (p1 (make-vec3)) (u1 (make-vec3))
         (reach (+ bound (object-def-bound def))))
    (declare (dynamic-extent c p0 u0 p1 u1) (type double-float reach))
    (vec3-sub! p0 (ship-state-r o0) (ship-state-r s0))
    (vec3-sub! u0 (ship-state-v o0) (ship-state-v s0))
    (vec3-sub! p1 (ship-state-r o1) (ship-state-r s1))
    (vec3-sub! u1 (ship-state-v o1) (ship-state-v s1))
    (hermite-set! c p0 u0 p1 u1 h)
    (let ((s-enter (hermite-crossing c reach (hermite-sag c))))
      (declare (type double-float s-enter))
      (when (< s-enter 0d0)
        (return-from contact-find! nil))
      (let ((rate (max 1d-12
                       (+ (sqrt (+ (expt (aref c 3) 2) (expt (aref c 4) 2) (expt (aref c 5) 2)))
                          (* 2 (sqrt (+ (expt (aref c 6) 2) (expt (aref c 7) 2) (expt (aref c 8) 2))))
                          (* 3 (sqrt (+ (expt (aref c 9) 2) (expt (aref c 10) 2) (expt (aref c 11) 2))))
                          (* (slerp-angle (ship-state-q s0) (ship-state-q s1)) bound)
                          (* (slerp-angle (ship-state-q o0) (ship-state-q o1))
                             (object-def-bound def)))))
            (fixtures (and armed (object-def-fixtures def)))
            (p (make-vec3)) (dp (make-vec3)) (q-s (make-quat)) (q-o (make-quat))
            (q-o* (make-quat)) (tip-b (make-vec3)) (tip (make-vec3))
            (plates (make-array 4 :element-type 'double-float :initial-element 0d0))
            (out (make-array 7 :element-type 'double-float)))
        (declare (dynamic-extent p dp q-s q-o q-o* tip-b tip plates out) (type double-float rate)
                 (type list fixtures))
        (assert (<= (length fixtures) 4))
        (vec3-sub! tip-b *grapple-tip* com)
        (labels ((pose! (s)
                   (declare (type double-float s))
                   (hermite-point! p dp c s)
                   (quat-slerp! q-s (ship-state-q s0) (ship-state-q s1) s)
                   (quat-slerp! q-o (ship-state-q o0) (ship-state-q o1) s))
                 (overlap-p (s)
                   (declare (type double-float s))
                   (pose! s)
                   (stack-object-contact! out p q-s q-o com def load))
                 (plate (fixture)
                   ;; After pose!: the tip's height outside FIXTURE's plate;
                   ;; TIP its point from the pin, the object's axes.
                   (quat-rotate! tip q-s tip-b)
                   (vec3-sub! tip tip p)
                   (quat-rotate! tip (quat-conjugate! q-o* q-o) tip)
                   (vec3-sub! tip tip (fixture-pin fixture))
                   (vec3-dot tip (fixture-axis fixture)))
                 (tip-crossing (lo s)
                   ;; The first fixture's plate crossed in (LO, S] within
                   ;; reach of its pin, bisected: (values s-hit j) or −1.
                   (declare (type double-float lo s))
                   (let ((hit -1d0) (hit-j -1))
                     (declare (type double-float hit) (type fixnum hit-j))
                     (pose! s)
                     (loop for fixture in fixtures
                           for j of-type fixnum from 0
                           do (let ((height (plate fixture)))
                                (when (and (fixture-free fixture) (> (aref plates j) 0d0)
                                           (<= height 0d0))
                                  (let ((a lo) (b s))
                                    (declare (type double-float a b))
                                    (loop while (> (* (- b a) h) *contact-time-tolerance*)
                                          do (let ((mid (* 0.5d0 (+ a b))))
                                               (pose! mid)
                                               (if (> (plate fixture) 0d0) (setf a mid) (setf b mid))))
                                    (pose! b)
                                    (let ((height (plate fixture)))
                                      (vec3-add-scaled! tip tip (fixture-axis fixture) (- height)))
                                    (when (and (<= (vec3-norm tip) *grapple-reach*)
                                               (or (< hit 0d0) (< b hit)))
                                      (setf hit b hit-j j))
                                    (pose! s)))
                                (setf (aref plates j) height)))
                     (values hit hit-j))))
          (when fixtures
            (pose! 0d0)
            (loop for fixture in fixtures
                  for j of-type fixnum from 0
                  do (setf (aref plates j) (plate fixture))))
          (let ((s s-enter) (previous -1d0))
            (declare (type double-float s previous))
            (loop
              (multiple-value-bind (tip-hit tip-j)
                  (if fixtures (tip-crossing (max previous 0d0) s) (values -1d0 -1))
                (declare (type double-float tip-hit) (type fixnum tip-j))
                (let ((end (cond ((< tip-hit 0d0) (and (overlap-p s) s))
                                 ((overlap-p tip-hit) tip-hit)
                                 (t
                                  ;; The tip: n along the fixture's inward
                                  ;; axis, the point the tip.
                                  (let ((fixture (nth tip-j fixtures)))
                                    (pose! tip-hit)
                                    (quat-rotate! tip q-o (fixture-axis fixture))
                                    (setf (aref out 0) 0d0
                                          (aref out 1) (- (aref tip 0))
                                          (aref out 2) (- (aref tip 1))
                                          (aref out 3) (- (aref tip 2)))
                                    (quat-rotate! tip q-s tip-b)
                                    (replace out tip :start1 4)
                                    (return-from contact-find!
                                      (when (contact-approach! result tip-hit h out p dp q-s q-o
                                                               s0 s1 o0 o1)
                                        (setf (aref result 11) (float tip-j 1d0))
                                        t)))))))
                  (when end
                    (let ((hit end))
                      (declare (type double-float hit))
                      (when (>= previous 0d0)
                        (let ((lo previous) (hi end))
                          (declare (type double-float lo hi))
                          (loop while (> (* (- hi lo) h) *contact-time-tolerance*)
                                do (let ((mid (* 0.5d0 (+ lo hi))))
                                     (if (overlap-p mid) (setf hi mid) (setf lo mid))))
                          (setf hit hi)))
                      (overlap-p hit)
                      (return-from contact-find!
                        (contact-approach! result hit h out p dp q-s q-o s0 s1 o0 o1))))))
              (when (>= s 1d0)
                (return-from contact-find! nil))
              (pose! s)
              (setf previous s
                    s (min 1d0 (+ s (/ (max *contact-substep* (- (vec3-norm p) reach)) rate)))))))))))

(defun contact-normal-speed (sim index n r-s r-o)
  "m/s: the contact points' relative velocity (the INDEXth object's less the
stack's) along N in SIM now; R-S and R-O the point from each CoM, inertial."
  (declare (type sim sim) (type fixnum index) (type vec3 n r-s r-o))
  (let ((a (make-vec3)) (b (make-vec3)) (w (make-vec3)))
    (declare (dynamic-extent a b w))
    (flet ((point-velocity! (out state r)
             (quat-rotate! w (ship-state-q state) (ship-state-w state))
             (vec3-cross! out w r)
             (vec3-add! out out (ship-state-v state))))
      (point-velocity! a (object-state (svref (sim-objects sim) index)) r-o)
      (point-velocity! b (sim-ship sim) r-s)
      (vec3-sub! a a b)
      (vec3-dot a n))))

(defun contact-impulse! (sim index n r-s r-o &optional (restitution *contact-restitution*))
  "The frictionless impulse j along N at the contact point (R-S from the
stack's CoM, R-O from the INDEXth object's; inertial) with RESTITUTION, to
both bodies' v and ω (decision 44): j = −(1 + e) v_n / (1/m_s + 1/m_o +
n·((I_s⁻¹ (r_s × n)) × r_s) + n·((I_o⁻¹ (r_o × n)) × r_o)), the stack's
mass and tensor with its latched load; a berth or fixed object's terms
zero, and the ship's while berthed. Nil when the points don't approach or
neither body moves; else j."
  (declare (type sim sim) (type fixnum index) (type vec3 n r-s r-o)
           (type double-float restitution))
  (let ((v-n (contact-normal-speed sim index n r-s r-o)))
    (when (>= v-n 0d0)
      (return-from contact-impulse! nil))
    (with-stack-properties (properties sim)
      (let* ((ship (sim-ship sim))
             (object (svref (sim-objects sim) index))
             (def (object-def-of object))
             (state (object-state object))
             (ship-free (not (sim-berthed sim)))
             (object-free (eq (object-def-kind def) :towable))
             (x-s (make-vec3)) (x-o (make-vec3)) (rb (make-vec3)) (nb (make-vec3))
             (y (make-vec3)) (conjugate (make-quat))
             (k 0d0))
        (declare (dynamic-extent x-s x-o rb nb y conjugate) (type double-float k))
        (flet ((arm! (x q inverse r)
                 ;; X = I⁻¹ (r × n), body axes; n·(X × r).
                 (quat-conjugate! conjugate q)
                 (quat-rotate! rb conjugate r)
                 (quat-rotate! nb conjugate n)
                 (vec3-cross! y rb nb)
                 (mat3*vec3! x inverse y)
                 (vec3-dot (vec3-cross! y x rb) nb)))
          (when ship-free
            (incf k (+ (/ (mass-properties-mass properties))
                       (arm! x-s (ship-state-q ship) (mass-properties-inertia-inverse properties)
                             r-s))))
          (when object-free
            (incf k (+ (/ (object-def-mass def))
                       (arm! x-o (ship-state-q state) (object-def-tensor-inverse def) r-o))))
          (when (<= k 0d0)
            (return-from contact-impulse! nil))
          (let ((j (/ (* (- (+ 1 restitution)) v-n) k)))
            (when ship-free
              (vec3-add-scaled! (ship-state-v ship) (ship-state-v ship) n
                                (- (/ j (mass-properties-mass properties))))
              (vec3-add-scaled! (ship-state-w ship) (ship-state-w ship) x-s (- j)))
            (when object-free
              (setf (object-disturbed object) t)
              (vec3-add-scaled! (ship-state-v state) (ship-state-v state) n (/ j (object-def-mass def)))
              (vec3-add-scaled! (ship-state-w state) (ship-state-w state) x-o j))
            j))))))

(defun contact-death! (sim scratch index s ticks speed)
  "Death by contact at S of SIM's step of TICKS from SCRATCH: the ship and
the promoted objects at their Hermite states there (attitudes slerped, ω
and m_p of the step's end; a latched load with the stack), t the contact
time, the loss :contact with the INDEXth object's key and the normal SPEED."
  (declare (type sim sim scratch) (type fixnum index ticks) (type double-float s speed))
  (let* ((h (* ticks +fine-step+))
         (time (+ (sim-t scratch) (* s h)))
         (c (make-array 12 :element-type 'double-float))
         (dp (make-vec3)) (q (make-quat)))
    (flet ((at! (old new)
             (hermite-set! c (ship-state-r old) (ship-state-v old) (ship-state-r new)
                           (ship-state-v new) h)
             (hermite-point! (ship-state-r new) dp c s)
             (vec3-scale! (ship-state-v new) dp (/ h))
             (replace (ship-state-q new) (quat-slerp! q (ship-state-q old) (ship-state-q new) s))))
      (at! (sim-ship scratch) (sim-ship sim))
      (loop for object across (sim-objects sim)
            for old across (sim-objects scratch)
            when (and (object-promoted-p object) (object-promoted-p old))
              do (at! (object-state old) (object-state object))))
    (latched-load-update! sim)
    (multiple-value-bind (latitude longitude)
        (latitude-longitude (body (sim-body sim)) (ship-state-r (sim-ship sim)) time)
      (setf (sim-t sim) time
            (sim-loss sim) (make-loss :contact (sim-body sim) time speed latitude longitude
                                      (object-key (svref (sim-objects sim) index)))))
    sim))

;;; Capture, the stack and berthing (decision 44, decision 112, decision 113)

(defun latched-load-update! (sim)
  "A latched load's stored state from the stack (decision 112): its CoM at
r + q (its CoM − the stack's), q ⊗ its relative quaternion, the stack's
rigid motion. SIM; conses nothing."
  (declare (optimize speed) (type sim sim) (inline ship-mass-properties))
  (unless (and (grapple-object (sim-grapple sim)) (not (sim-berthed sim)))
    (return-from latched-load-update! sim))
  (with-stack-properties (properties sim load)
    (when load
      (let* ((ship (sim-ship sim))
             (state (object-state (svref (sim-objects sim) (grapple-object (sim-grapple sim)))))
             (x (make-vec3)) (w (make-vec3)) (y (make-vec3)) (conjugate (make-quat)))
        (declare (dynamic-extent x w y conjugate))
        (vec3-sub! x (stack-load-com load) (mass-properties-com properties))
        (quat-rotate! x (ship-state-q ship) x)
        (vec3-add! (ship-state-r state) (ship-state-r ship) x)
        (quat-mul! (ship-state-q state) (ship-state-q ship) (stack-load-q load))
        (quat-rotate! w (ship-state-q ship) (ship-state-w ship))
        (vec3-add! (ship-state-v state) (ship-state-v ship) (vec3-cross! y w x))
        (quat-rotate! (ship-state-w state) (quat-conjugate! conjugate (stack-load-q load))
                      (ship-state-w ship)))))
  sim)

(defun berth-hold! (sim)
  "Berthed (decision 112): the ship set from the station's state and the
grapple's pose: R at r_s + q_s p, q = q_s ⊗ q_rel, r its CoM from R, v =
v_s + ω_s × (r − r_s), ω the station's in ship axes. SIM; conses nothing."
  (declare (optimize speed) (type sim sim))
  (with-stack-properties (properties sim)
    (let* ((ship (sim-ship sim))
           (grapple (sim-grapple sim))
           (station (object-state (svref (sim-objects sim) (grapple-object grapple))))
           (r (ship-state-r ship))
           (x (make-vec3)) (w (make-vec3)) (conjugate (make-quat)))
      (declare (dynamic-extent x w conjugate))
      (quat-mul! (ship-state-q ship) (ship-state-q station) (grapple-q grapple))
      (quat-rotate! x (ship-state-q station) (grapple-com grapple))
      (vec3-add! r (ship-state-r station) x)
      (quat-rotate! x (ship-state-q ship) (mass-properties-com properties))
      (vec3-add! r r x)
      (quat-rotate! w (ship-state-q station) (ship-state-w station))
      (vec3-sub! x r (ship-state-r station))
      (vec3-add! (ship-state-v ship) (ship-state-v station) (vec3-cross! x w x))
      (quat-rotate! (ship-state-w ship) (quat-conjugate! conjugate (ship-state-q ship)) w)))
  sim)

(defun grapple-attach! (sim index fixture)
  "The grapple on SIM's INDEXth object's FIXTURE, closing for
*claws-closing-time*."
  (let ((grapple (sim-grapple sim)))
    (setf (grapple-state grapple) :closing
          (grapple-object grapple) index
          (grapple-fixture grapple) fixture
          (grapple-closing-end grapple) (+ (sim-t sim) *claws-closing-time*))
    grapple))

(defun grapple-capture! (sim index fixture)
  "A towable capture (decision 44): SIM's INDEXth object latched at its
pose of the moment and coupled perfectly inelastically: the stack's CoM and
v by mass, its angular momentum about the new CoM kept, ω = I_stack⁻¹ L in
ship axes; the grapple closing; the notice \"LATCHED <NAME>\"; a pending
delivery of it cleared. SIM."
  (let* ((ship (sim-ship sim))
         (object (svref (sim-objects sim) index))
         (def (object-def-of object))
         (state (object-state object))
         (q-s (ship-state-q ship))
         (q-s* (quat-conjugate q-s))
         (before (ship-mass-properties (ship-state-m-p ship) nil))
         (m-s (mass-properties-mass before))
         (m-o (object-def-mass def))
         (grapple (grapple-attach! sim index fixture)))
    (when (eql index (first (sim-delivery sim)))
      (setf (sim-delivery sim) nil))
    ;; The pose: its CoM in ship axes from R, its axes to the ship's.
    (replace (grapple-com grapple)
             (vec3-add (mass-properties-com before)
                       (quat-rotate q-s* (vec3-sub (ship-state-r state) (ship-state-r ship)))))
    (quat-normalize! (grapple-q grapple) (quat-mul q-s* (ship-state-q state)))
    (with-stack-properties (after sim)
      (let* ((m (mass-properties-mass after))
             (r (vec3-add (ship-state-r ship)
                          (quat-rotate q-s (vec3-sub (mass-properties-com after)
                                                     (mass-properties-com before)))))
             (v (vec3-scale (vec3-add (vec3-scale (ship-state-v ship) m-s)
                                      (vec3-scale (ship-state-v state) m-o))
                            (/ m)))
             (l (make-vec3)))
        (loop for (mass body tensor) in (list (list m-s ship (mass-properties-inertia before))
                                              (list m-o state (object-def-tensor def)))
              do (setf l (vec3-add l (vec3-add
                                      (quat-rotate (ship-state-q body)
                                                   (mat3*vec3 tensor (ship-state-w body)))
                                      (vec3-scale (vec3-cross (vec3-sub (ship-state-r body) r)
                                                              (vec3-sub (ship-state-v body) v))
                                                  mass)))))
        (replace (ship-state-r ship) r)
        (replace (ship-state-v ship) v)
        (mat3*vec3! (ship-state-w ship) (mass-properties-inertia-inverse after)
                    (quat-rotate q-s* l))))
    (setf (object-status object) :latched
          (object-disturbed object) t)
    (latched-load-update! sim)
    (sim-notice! sim :grapple (format nil "LATCHED ~A" (object-def-name def)))
    sim))

(defun berth-capture! (sim index fixture)
  "A berth capture (decision 44, decision 113): the ship held at SIM's
INDEXth object (berth-hold!) at its pose of the moment, so it takes the
fixture point's velocity and the station's rotation; throttle cut, rate
damping, transfer off; the grapple closing; the notice \"BERTHED <NAME>\"
and a checkpoint request. SIM."
  (let* ((ship (sim-ship sim))
         (controls (sim-controls sim))
         (station (object-state (svref (sim-objects sim) index)))
         (q-st* (quat-conjugate (ship-state-q station)))
         (grapple (grapple-attach! sim index fixture))
         (com (mass-properties-com (ship-mass-properties (ship-state-m-p ship) nil))))
    (replace (grapple-com grapple)
             (quat-rotate q-st* (vec3-sub (vec3-sub (ship-state-r ship)
                                                    (quat-rotate (ship-state-q ship) com))
                                          (ship-state-r station))))
    (quat-normalize! (grapple-q grapple) (quat-mul q-st* (ship-state-q ship)))
    (controls-throttle-cut controls)
    (fill (controls-duties controls) 0d0)
    (setf (controls-attitude-mode controls) :rate-damping
          (controls-settled controls) nil
          (sim-berthed sim) t
          (sim-transfer sim) nil
          (sim-cutoff sim) nil)
    (berth-hold! sim)
    (sim-notice! sim :grapple (format nil "BERTHED ~A"
                                      (object-def-name (object-def-of (svref (sim-objects sim) index)))))
    (setf (sim-checkpoint-request sim) t)
    sim))

(defun grapple-refusal (sim index fixture)
  "Why the grapple contact with SIM's INDEXth object's FIXTURE now does not
capture, in decision 113's order, or nil."
  (let* ((reading (make-grapple-reading))
         (def (object-def-of (svref (sim-objects sim) index)))
         (towable (eq (fixture-kind (nth fixture (object-def-fixtures def))) :towable)))
    (with-stack-properties (properties sim)
      (fixture-reading-into! reading sim index fixture (mass-properties-com properties)))
    (let ((closing (grapple-reading-closing-speed reading)))
      (cond ((and towable (> (object-def-mass def) *grapple-rating*))
             (format nil "LOAD ~D T OVER ~D T RATING" (round (object-def-mass def) 1000)
                     (round *grapple-rating* 1000)))
            ((> closing *capture-closing-max*) "NO CAPTURE: TOO FAST")
            ((< closing *capture-closing-min*) "NO CAPTURE: TOO SLOW")
            ((not (grapple-reading-offset-ok reading)) "NO CAPTURE: OFF AXIS")
            ((not (grapple-reading-angle-ok reading)) "NO CAPTURE: MISALIGNED")
            ((not (grapple-reading-rotation-ok reading)) "NO CAPTURE: ROTATING")))))

(defun grapple-contact! (sim index fixture)
  "The grapple contact with SIM's INDEXth object's FIXTURE now: capture, or
the refusal's notice and a bounce along the fixture's axis at the tip (its
normal and points into *contact*)."
  (let ((refusal (grapple-refusal sim index fixture))
        (kind (fixture-kind (nth fixture (object-def-fixtures
                                          (object-def-of (svref (sim-objects sim) index)))))))
    (cond ((null refusal)
           (if (eq kind :berth)
               (berth-capture! sim index fixture)
               (grapple-capture! sim index fixture)))
          (t
           (sim-notice! sim :grapple refusal)
           (let* ((contact *contact*)
                  (ship (sim-ship sim))
                  (state (object-state (svref (sim-objects sim) index)))
                  (def (object-def-of (svref (sim-objects sim) index)))
                  (n (contact-normal contact)) (r-s (contact-r-s contact))
                  (r-o (contact-r-o contact)))
             (with-stack-properties (properties sim)
               (replace r-s (quat-rotate (ship-state-q ship)
                                         (vec3-sub *grapple-tip* (mass-properties-com properties)))))
             (vec3-sub! r-o (vec3-add (ship-state-r ship) r-s) (ship-state-r state))
             (replace n (quat-rotate (ship-state-q state)
                                     (fixture-axis (nth fixture (object-def-fixtures def)))))
             (vec3-scale! n n -1d0)
             (contact-impulse! sim index n r-s r-o))))
    sim))

(defun contact-respond! (sim scratch level ticks index result)
  "The response to the contact in RESULT with SIM's INDEXth object, found
in its step of TICKS at LEVEL from SCRATCH: above *contact-death-speed*
death at t_c; else the step redone from SCRATCH to the grid point g at or
before t_c (none when g is the start; the end state when it is the end)
and there a grapple contact (grapple-contact!) or the impulse. The ticks the
step took."
  (declare (type sim sim scratch) (type fixnum level ticks index) (type contact-result result))
  (let* ((contact *contact*)
         (s (aref result 0))
         (speed (aref result 1))
         (fixture (round (aref result 11)))
         (t0 (sim-t scratch)))
    (setf (contact-object contact) index
          (contact-time contact) (+ t0 (* s ticks +fine-step+))
          (contact-speed contact) speed)
    (replace (contact-normal contact) result :start2 2)
    (replace (contact-r-s contact) result :start2 5)
    (replace (contact-r-o contact) result :start2 8)
    (incf (contact-count contact))
    (when (> (- speed) *contact-death-speed*)
      (contact-death! sim scratch index s ticks (- speed))
      (return-from contact-respond! ticks))
    (let ((g (min ticks (floor (* s ticks)))))
      (declare (type fixnum g))
      (unless (= g ticks)
        (sim-assign! sim scratch)
        (when (> g 0)
          (let ((*contact-redo* t))
            (sim-step-ticks! sim level (+ t0 (* g +fine-step+))))))
      (unless (sim-loss sim)
        (cond ((>= fixture 0)
               (grapple-contact! sim index fixture))
              (t
               ;; The point kept from the stack's CoM, so both bodies take
               ;; the impulse at one point.
               (let ((r-o (contact-r-o contact)))
                 (vec3-sub! r-o (contact-r-s contact)
                            (vec3-sub! r-o (ship-state-r (object-state (svref (sim-objects sim) index)))
                                       (ship-state-r (sim-ship sim)))))
               (contact-impulse! sim index (contact-normal contact) (contact-r-s contact)
                                 (contact-r-o contact))))
        (latched-load-update! sim)
        (when (= g ticks)
          (when (sim-delivery sim)
            (delivery-update! sim))
          (sim-update-objects! sim)))
      g)))

(defun contact-test! (sim level ticks)
  "After SIM's step of TICKS at LEVEL and its crash tests, when contact-save!
saved its start: each candidate's contact-find! (the grapple's tip while
armed), the earliest answered by contact-respond!. Nil without a contact
(promotion and demotion are the caller's), else the ticks the step took.
Conses nothing without a contact."
  (declare (type sim sim) (type fixnum level ticks))
  (let ((contact *contact*))
    (unless (contact-saved contact)
      (return-from contact-test! nil))
    (setf (contact-saved contact) nil)
    (let ((scratch (contact-scratch contact))
          (h (* ticks +fine-step+))
          (armed (eq (grapple-state (sim-grapple sim)) :armed))
          (result (make-array 12 :element-type 'double-float))
          (best (make-array 12 :element-type 'double-float))
          (found -1))
      (declare (dynamic-extent result best) (type fixnum found) (type double-float h))
      (with-stack-properties (properties sim load)
        (let* ((com (mass-properties-com properties))
               (bound (locally (declare (inline stack-bound)) (stack-bound com load))))
          (loop for object across (sim-objects sim)
                for i of-type fixnum from 0
                when (and (contact-candidate-p sim object i)
                          (object-promoted-p (svref (sim-objects scratch) i))
                          (contact-find! result sim scratch i com bound h load armed)
                          (or (< found 0) (< (aref result 0) (aref best 0))))
                  do (replace best result)
                     (setf found i))))
      (and (>= found 0)
           (contact-respond! sim scratch level ticks found best)))))

;;; Release, the excluded object and propellant transfer (decision 112,
;;; decision 113, decision 26: Refuelling and economy): G and B, each nil or
;;; a refusal's text.

(defparameter *release-rotation-max* (deg->rad 0.5d0) "rad/s; decision 113.")
(defparameter *transfer-rate* 10d0 "kg/s; decision 26.")
(defparameter *engine-locked-message* "ENGINE LOCKED: BERTHED"
  "An engine or RCS key while berthed; decision 113.")
(declaim (type double-float *release-rotation-max* *transfer-rate*))

;;; Deliveries (docs/architecture.md "Removal of objects"; decision 53:
;;; Deliveries, the stranded offer, checkpoint floors and Skua Yard's
;;; layout): a towable load released beside a receiving fixture, removed
;;; *delivery-wait* later if it stayed near.

(defparameter *delivery-reach* 100d0 "m from the pin at the release; decision 53.")
(defparameter *delivery-speed* 0.05d0 "m/s relative to the pin's motion at the release; decision 53.")
(defparameter *delivery-range* 150d0 "m from the pin the load stays within; decision 53.")
(declaim (type double-float *delivery-reach* *delivery-speed* *delivery-range*))

(defun fixture-pin-into! (p v object fixture time)
  "P, V: OBJECT's FIXTURE's pin at TIME and its velocity; true, or nil
without a state (object-state-into!). Conses nothing."
  (declare (type vec3 p v) (type object object) (type fixture fixture) (type double-float time))
  (let ((q (make-quat)) (w (make-vec3)) (x (make-vec3)))
    (declare (dynamic-extent q w x))
    (when (object-state-into! p v q w object time)
      (quat-rotate! x q (fixture-pin fixture))
      (quat-rotate! w q w)
      (vec3-add! p p x)
      (vec3-add! v v (vec3-cross! w w x))
      t)))

(defun delivery-start! (sim index)
  "A towable release of SIM's INDEXth object: a pending delivery (load,
receiver, fixture, release t) when its CoM lies within *delivery-reach* of a
receiving fixture's pin and moves at most *delivery-speed* relative to it;
else none. SIM."
  (let* ((time (sim-t sim))
         (state (object-state (svref (sim-objects sim) index)))
         (p (make-vec3)) (v (make-vec3)))
    (setf (sim-delivery sim) nil)
    (loop for receiver across (sim-objects sim)
          for i from 0
          do (loop for fixture in (object-def-fixtures (object-def-of receiver))
                   for k from 0
                   when (and (fixture-receiving fixture)
                             (eq (object-status receiver) :promoted)
                             (fixture-pin-into! p v receiver fixture time)
                             (<= (vec3-norm (vec3-sub! p (ship-state-r state) p)) *delivery-reach*)
                             (<= (vec3-norm (vec3-sub! v (ship-state-v state) v)) *delivery-speed*))
                     do (setf (sim-delivery sim) (list index i k time))
                        (return-from delivery-start! sim)))
    sim))

(defun delivery-update! (sim)
  "After a step with a delivery pending: cleared when the load is not free
(grappled again, removed), the receiver removed or the load beyond
*delivery-range* of the pin; at its due time (sim-delivery-due) the load
removed :delivered with the notice \"<NAME> DELIVERED\". SIM; conses
nothing until the removal."
  (declare (optimize speed) (type sim sim))
  (destructuring-bind (index receiver fixture release) (sim-delivery sim)
    (declare (type fixnum index receiver fixture) (ignore release))
    (let* ((time (sim-t sim))
           (objects (sim-objects sim))
           (load (svref objects index))
           (station (svref objects receiver))
           (r (make-vec3)) (v (make-vec3)) (q (make-quat)) (w (make-vec3))
           (p (make-vec3)) (dp (make-vec3)))
      (declare (dynamic-extent r v q w p dp) (type object load station))
      (if (and (member (object-status load) '(:promoted :rails))
               (object-state-into! r v q w load time)
               (fixture-pin-into! p dp station
                                  (nth fixture (object-def-fixtures (object-def-of station))) time)
               (<= (vec3-norm (vec3-sub! p r p)) *delivery-range*))
          (when (>= time (the double-float (sim-delivery-due sim)))
            (setf (sim-delivery sim) nil)
            (let ((def (object-def-of station)))
              (object-remove! sim load time :delivered
                              (list (object-def-key def)
                                    (fixture-name (nth fixture (object-def-fixtures def))))))
            (sim-notice! sim :delivery
                         (format nil "~A DELIVERED" (object-def-name (object-def-of load)))))
          (setf (sim-delivery sim) nil))))
  sim)

(defun grapple-release! (sim)
  "The release (decision 112): a latched load takes the stack's motion at
its CoM (latched-load-update!) and is :promoted again, the ship's CoM
alone the stack's motion at it, v + ω × (r_ship − r_stack), ω kept; a
berthed ship keeps the fixture point's motion berth-hold! gave it and is
integrated again, the transfer off. Rate damping kept, the grapple armed,
the object excluded until the shapes part. SIM."
  (let* ((grapple (sim-grapple sim))
         (index (grapple-object grapple))
         (object (svref (sim-objects sim) index)))
    (cond ((sim-berthed sim)
           (setf (sim-berthed sim) nil
                 (sim-transfer sim) nil))
          (t
           (latched-load-update! sim)
           (let* ((ship (sim-ship sim))
                  (alone (mass-properties-com (ship-mass-properties (ship-state-m-p ship) nil)))
                  (x (with-stack-properties (properties sim)
                       (quat-rotate (ship-state-q ship)
                                    (vec3-sub alone (mass-properties-com properties)))))
                  (w (quat-rotate (ship-state-q ship) (ship-state-w ship))))
             (vec3-add! (ship-state-r ship) (ship-state-r ship) x)
             (vec3-add! (ship-state-v ship) (ship-state-v ship) (vec3-cross w x)))
           (setf (object-status object) :promoted)))
    (setf (grapple-state grapple) :armed
          (grapple-object grapple) nil
          (grapple-fixture grapple) nil
          (sim-excluded sim) index)
    (when (eq (object-def-kind (object-def-of object)) :towable)
      (delivery-start! sim index))
    sim))

(defun grapple-key! (sim)
  "G: stowed ↔ armed; refused while the claws close; latched or berthed the
release, refused above *release-rotation-max* or with the engine on."
  (let ((grapple (sim-grapple sim))
        (ship (sim-ship sim)))
    (ecase (grapple-state grapple)
      (:stowed (setf (grapple-state grapple) :armed) nil)
      (:armed (setf (grapple-state grapple) :stowed) nil)
      (:closing "RELEASE LOCKED: CLAWS CLOSING")
      (:latched
       (cond ((> (vec3-norm (ship-state-w ship)) *release-rotation-max*)
              "RELEASE REFUSED: ROTATING")
             ((and (> (ship-engine-force (controls-throttle (sim-controls sim))) 0d0)
                   (> (ship-state-m-p ship) 0d0))
              "RELEASE REFUSED: ENGINE ON")
             (t (grapple-release! sim) nil))))))

(defun exclusion-update! (sim)
  "After a step: the excluded object no longer excluded once it is not
promoted or its shapes and the stack's no longer overlap. SIM; conses
nothing."
  (declare (optimize speed) (type sim sim))
  (let ((index (sim-excluded sim)))
    (when index
      (let ((object (svref (sim-objects sim) index)))
        (if (not (eq (object-status object) :promoted))
            (setf (sim-excluded sim) nil)
            (with-stack-properties (properties sim load)
              (let ((state (object-state object))
                    (ship (sim-ship sim))
                    (p (make-vec3))
                    (out (make-array 7 :element-type 'double-float)))
                (declare (dynamic-extent p out))
                (vec3-sub! p (ship-state-r state) (ship-state-r ship))
                (unless (and (<= (vec3-norm p)
                                 (+ (locally (declare (inline stack-bound))
                                      (stack-bound (mass-properties-com properties) load))
                                    (object-def-bound (object-def-of object))))
                             (stack-object-contact! out p (ship-state-q ship) (ship-state-q state)
                                                    (mass-properties-com properties)
                                                    (object-def-of object) load))
                  (setf (sim-excluded sim) nil))))))))
  sim)

(defun berth-object (sim)
  "The object SIM is berthed at, or nil."
  (and (sim-berthed sim)
       (svref (sim-objects sim) (grapple-object (sim-grapple sim)))))

(defun transfer-key! (sim)
  "B: the transfer on or off, berthed at a depot only."
  (let ((station (berth-object sim)))
    (cond ((null station) "NOT BERTHED")
          ((not (object-def-depot (object-def-of station))) "NO PROPELLANT HERE")
          (t (setf (sim-transfer sim) (not (sim-transfer sim)))
             nil))))

(defun transfer-step! (sim h)
  "A berthed step of H with the transfer on: m_p += min(*transfer-rate* H,
full − m_p), exact on the grid; at full the transfer off and the notice
\"TANKS FULL\". SIM."
  (declare (type sim sim) (type double-float h))
  (let ((ship (sim-ship sim)))
    (incf (ship-state-m-p ship) (min (* *transfer-rate* h)
                                     (- *propellant-full* (ship-state-m-p ship))))
    (when (>= (ship-state-m-p ship) *propellant-full*)
      (setf (sim-transfer sim) nil)
      (sim-notice! sim :transfer "TANKS FULL")))
  sim)
