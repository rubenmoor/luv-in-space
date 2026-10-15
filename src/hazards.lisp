(in-package #:luv-in-space)

;;; The swept crash test and death (docs/architecture.md "Collisions and the
;;; Sun"; decision 74: Crash test on the step's Hermite and the death state,
;;; decision 34: How death works). Milestone 2: Earth (R + 100 km, the
;;; central body) and the Moon (R, a third body) from Earth's frame.

(defstruct (loss (:constructor make-loss (cause body time speed latitude longitude)))
  (cause :crash :type keyword :read-only t)
  (body :earth :type keyword :read-only t)
  (time 0d0 :type double-float :read-only t)        ; s, the impact, off the grid
  (speed 0d0 :type double-float :read-only t)       ; m/s, relative to the body
  (latitude 0d0 :type double-float :read-only t)    ; rad, body-fixed
  (longitude 0d0 :type double-float :read-only t))  ; rad, east, (−π, π]

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
Hermite, attitude slerped from Q0, ω and m_p of the step's end; t the
impact time; the loss record."
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
    sim))

;;; Removal of objects (docs/architecture.md "Removal of objects"; decision
;;; 46: Rails across spheres, hyperbolas and re-entry). No SHIP message here
;;; (task 158: Grapple, contact, berthing, towing and deliveries' queue).

(defun object-remove! (sim object time cause &optional point (body (sim-body sim)))
  "OBJECT removed from SIM at TIME: status :removed and its record (TIME,
BODY, CAUSE :burned-up, :delivered or :impact, POINT: (latitude longitude)
of a crash in radians, a delivery's receiving object and fixture). From
then it is never stepped, promoted, crash-tested or given a state; a
checkpoint from before brings it back. OBJECT."
  (declare (ignorable sim))
  (setf (object-status object) :removed
        (object-removal object) (make-removal (float time 1d0) body cause point))
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
