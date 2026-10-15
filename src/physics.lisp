(in-package #:luv-in-space)

;;; The ship's state, the gravity models and the coarse step rule
;;; (docs/architecture.md "Integrator", "Gravity model"; decision 69: Ship
;;; state, step rule and kinematic attitude in physics.lisp, decision 17:
;;; Gravity model). Earth's frame only in milestone 2. From milestone 3 the
;;; objects, their rails and attitude modes ("Rails").

(defstruct (ship-state (:constructor %make-ship-state (r v q w m-p dv))
                       (:copier nil))
  (r (make-vec3) :type vec3)          ; m, Earth-centred ICRF
  (v (make-vec3) :type vec3)          ; m/s
  (q (quat 1 0 0 0) :type quat)       ; body -> inertial
  (w (make-vec3) :type vec3)          ; rad/s, body axes
  (m-p 0d0 :type double-float)        ; propellant, kg
  (dv 0d0 :type double-float))        ; odometer, m/s

(defun make-ship-state (r v q w m-p)
  "A ship state from copies of r, v, q, w and m-p; the odometer at 0."
  (%make-ship-state (vec3-copy r) (vec3-copy v) (quat-copy q) (vec3-copy w)
                    (float m-p 1d0) 0d0))

(defun copy-ship-state (state)
  "A deep copy: equalp to STATE, sharing no array."
  (%make-ship-state (vec3-copy (ship-state-r state)) (vec3-copy (ship-state-v state))
                    (quat-copy (ship-state-q state)) (vec3-copy (ship-state-w state))
                    (ship-state-m-p state) (ship-state-dv state)))

(defun ship-state-assign! (out state)
  "Copy STATE into OUT, consing nothing; OUT."
  (declare (type ship-state out state))
  (replace (ship-state-r out) (ship-state-r state))
  (replace (ship-state-v out) (ship-state-v state))
  (replace (ship-state-q out) (ship-state-q state))
  (replace (ship-state-w out) (ship-state-w state))
  (setf (ship-state-m-p out) (ship-state-m-p state)
        (ship-state-dv out) (ship-state-dv state))
  out)

;;; Gravity

(declaim (inline third-body-add! earth-gravity-into! gravity-into!))

(defun third-body-add! (a r d mu)
  "a += μ ((d − r)/|d − r|³ − d/|d|³): a third body at d, direct minus
indirect term."
  (declare (type vec3 a r d) (type double-float mu))
  (with-vec3 (((rx ry rz) r) ((dx dy dz) d))
    (let* ((sx (- dx rx)) (sy (- dy ry)) (sz (- dz rz))
           (s2 (+ (* sx sx) (* sy sy) (* sz sz)))
           (d2 (+ (* dx dx) (* dy dy) (* dz dz)))
           (ks (/ mu (* s2 (sqrt (the (double-float 0d0) s2)))))
           (kd (/ mu (* d2 (sqrt (the (double-float 0d0) d2))))))
      (vec3-set! a
                 (+ (aref a 0) (- (* ks sx) (* kd dx)))
                 (+ (aref a 1) (- (* ks sy) (* kd dy)))
                 (+ (aref a 2) (- (* ks sz) (* kd dz)))))))

(defun earth-gravity-into! (a r model moon sun)
  "gravity-into! in Earth's frame with the Moon and the Sun already at
MOON and SUN (m, Earth-centred; read only for :full), so a caller with
several points at one time reads the ephemeris once."
  (declare (type vec3 a r moon sun) (type keyword model))
  (if (eq model :none)
      (vec3-set! a 0d0 0d0 0d0)
      (with-vec3 (((x y z) r))
        (let* ((r2 (+ (* x x) (* y y) (* z z)))
               (k (/ +mu-earth+ (* r2 (sqrt (the (double-float 0d0) r2))))))
          (if (eq model :point)
              (vec3-set! a (- (* k x)) (- (* k y)) (- (* k z)))
              ;; a = −μ r/r³ − 1.5 J2 μ R²/r⁵ ((1 − 5 z²/r²) x, … y, (3 − 5 z²/r²) z)
              (let* ((c (/ (* 1.5d0 +j2-earth+ +radius-earth+ +radius-earth+) r2))
                     (z2 (* 5 (/ (* z z) r2)))
                     (kxy (* k (+ 1 (* c (- 1 z2)))))
                     (kz (* k (+ 1 (* c (- 3 z2))))))
                (vec3-set! a (- (* kxy x)) (- (* kxy y)) (- (* kz z)))
                (ecase model
                  (:j2)
                  (:full
                   (third-body-add! a r moon +mu-moon+)
                   (third-body-add! a r sun +mu-sun+)))
                a))))))

(declaim (inline third-bodies-into!))

(defun third-bodies-into! (moon sun time)
  "The Moon's and the Sun's positions at TIME (m, Earth-centred) into MOON
and SUN."
  (declare (type vec3 moon sun) (type double-float time))
  (let ((dv (make-vec3)))
    (declare (dynamic-extent dv))
    (body-state-into moon dv :moon time :earth)
    (body-state-into sun dv :sun time :earth)))

(defun gravity-into! (a r time model &optional (central :earth))
  "Write the gravitational acceleration at r (m, CENTRAL's frame) at TIME
into a. MODEL: :none, :point (μ⊕), :j2 (with J2, pole along ICRF z),
:full (:j2 plus the Sun and Moon as third bodies)."
  (declare (type vec3 a r) (type double-float time) (type keyword model central))
  (ecase central
    (:earth
     (let ((moon (make-vec3)) (sun (make-vec3)))
       (declare (dynamic-extent moon sun))
       (when (eq model :full)
         (third-bodies-into! moon sun time))
       (earth-gravity-into! a r model moon sun)))))

;;; One step's third bodies, read once and shared by the ship's and the
;;; promoted objects' rk4-step!s (decision 110): the Moon and the Sun at t
;;; (0–5), t + h/2 (6–11) and t + h (12–17).

(deftype third-bodies () '(simple-array double-float (18)))

(defun make-third-bodies () (make-array 18 :element-type 'double-float :initial-element 0d0))

(declaim (inline third-bodies-step-into!))

(defun third-bodies-step-into! (buffer time h)
  "The Moon and the Sun at TIME, TIME + h/2 and TIME + h into BUFFER; BUFFER."
  (declare (type third-bodies buffer) (type double-float time h))
  (let ((moon (make-vec3)) (sun (make-vec3)) (h/2 (* 0.5d0 h)))
    (declare (dynamic-extent moon sun))
    (flet ((at! (start time)
             (third-bodies-into! moon sun time)
             (replace buffer moon :start1 start)
             (replace buffer sun :start1 (+ start 3))))
      (declare (inline at!))
      (at! 0 time)
      (at! 6 (+ time h/2))
      (at! 12 (+ time h)))
    buffer))

;;; The coarse step rule (docs/architecture.md "Integrator"); the fine step
;;; is +fine-step+.

(defparameter *coarse-eta* (/ 1d0 256) "η, docs/architecture.md \"Integrator\".")
(defparameter *coarse-step-min* (/ 1d0 64) "s, docs/architecture.md \"Integrator\".")
(defparameter *coarse-step-max* 16384d0 "s, docs/architecture.md \"Integrator\".")
(defparameter *thrust-step-max* 1d0 "s, under thrust; docs/architecture.md \"Integrator\".")
(declaim (type double-float *coarse-eta* *coarse-step-min* *coarse-step-max*
               *thrust-step-max*))

(defconstant +moon-distance-min+ 3.5d8
  "m, below the Moon's least distance from Earth (perigee ≥ 356,000 km).")
(defconstant +sun-distance-min+ 1.4d11
  "m, below the Sun's least distance from Earth (perihelion 1.471×10¹¹ m).")

(declaim (inline coarse-step))

(defun coarse-step (r time thrusting &optional (central :earth))
  "h = η min over the frame's bodies of √(d³/μ), down to a power of two,
clamped to [*coarse-step-min*, *coarse-step-max*], ≤ *thrust-step-max*
when THRUSTING."
  (declare (optimize speed) (type vec3 r) (type double-float time))
  (let ((tau 0d0))
    (declare (type double-float tau))
    (flet ((dynamical-time (d2 mu)
             (declare (type (double-float 0d0) d2) (type double-float mu))
             (sqrt (the (double-float 0d0) (/ (* d2 (sqrt d2)) mu)))))
      (ecase central
        (:earth
         (let* ((d (make-vec3)) (dv (make-vec3))
                (r2 (vec3-dot r r))
                (rn (sqrt (the (double-float 0d0) r2))))
           (declare (dynamic-extent d dv))
           (setf tau (dynamical-time r2 +mu-earth+))
           ;; A body is read only if its least possible distance could
           ;; lower tau; skipping it leaves the result unchanged.
           (flet ((third-body (key distance-min mu)
                    (let ((low (- distance-min rn)))
                      (unless (and (> low 0d0) (>= (dynamical-time (* low low) mu) tau))
                        (body-state-into d dv key time :earth)
                        (vec3-sub! d d r)
                        (setf tau (min tau (dynamical-time (vec3-dot d d) mu)))))))
             (declare (inline third-body))
             (third-body :moon +moon-distance-min+ +mu-moon+)
             (third-body :sun +sun-distance-min+ +mu-sun+))))))
    ;; Halving from the upper clamp, a power of two, rounds down exactly.
    (let ((h (* *coarse-eta* tau))
          (p *coarse-step-max*))
      (declare (type double-float h p))
      (loop while (and (> p h) (> p *coarse-step-min*))
            do (setf p (* 0.5d0 p)))
      (if thrusting (min p *thrust-step-max*) p))))

;;; The RK4 step (docs/architecture.md "Integrator"; decision 69). The state
;;; vector y: r 0–2, v 3–5, q 6–9, ω 10–12, m_p 13.

(deftype state-vector () '(simple-array double-float (14)))

(declaim (inline state-vector-axpy!))

(defun state-vector-axpy! (out y k s)
  "out = y + s k."
  (declare (type state-vector out y k) (type double-float s))
  (dotimes (i 14 out)
    (setf (aref out i) (+ (aref y i) (* s (aref k i))))))

;; Hot callers declare it inline: a full call boxes the double-float time.
(declaim (sb-ext:maybe-inline rk4-step!))

(defun rk4-step! (state h time model force-body torque-body mdot m-dry
                  inertia inertia-inverse &optional attitude-fn third-bodies)
  "Advance STATE by h from TIME: classical RK4 on r, v, q, ω and m_p, the
body force (N), torque (N m) and mass flow (kg/s) held over the step, the
mass m-dry + m_p, the inertia tensor about the CoM and its inverse (body
axes) held over the step; conses nothing. When ṁ h > m_p the inputs are
scaled so the tanks empty exactly at the step's end. With ATTITUDE-FN,
called as (attitude-fn q-out w-out time-cell r v) to write the attitude and
the frame's rate (inertial axes) at the time in time-cell (a one-element
double array, so nothing is boxed), attitude is kinematic: q comes from it
at each stage, torque is ignored, q and ω are set from it at t + h. With
THIRD-BODIES (third-bodies-step-into! of this TIME and h) :full reads the
Moon and the Sun from it, bit for bit the same as without."
  (declare (optimize speed) (type ship-state state) (type double-float h time mdot m-dry)
           (type vec3 force-body torque-body) (type mat3 inertia inertia-inverse)
           (type keyword model) (type (or null function) attitude-fn)
           (type (or null third-bodies) third-bodies))
  (let* ((m-p0 (ship-state-m-p state))
         (scale (cond ((<= m-p0 0d0) 0d0)
                      ((> (* mdot h) m-p0) (/ m-p0 (* mdot h)))
                      (t 1d0)))
         (exhausted (< scale 1d0))
         (mdot (* scale mdot))
         (y (make-array 14 :element-type 'double-float))
         (ys (make-array 14 :element-type 'double-float))
         (k1 (make-array 14 :element-type 'double-float))
         (k2 (make-array 14 :element-type 'double-float))
         (k3 (make-array 14 :element-type 'double-float))
         (k4 (make-array 14 :element-type 'double-float))
         (f (make-vec3)) (tau (make-vec3))
         (r (make-vec3)) (v (make-vec3)) (w (make-vec3)) (a (make-vec3))
         (iw (make-vec3)) (tmp (make-vec3)) (q (make-quat)) (dq (make-quat))
         (time-cell (make-array 1 :element-type 'double-float))
         ;; The Moon and the Sun at t, t + h/2 (stages 2 and 3) and t + h
         (moon0 (make-vec3)) (sun0 (make-vec3))
         (moon1 (make-vec3)) (sun1 (make-vec3))
         (moon2 (make-vec3)) (sun2 (make-vec3)))
    (declare (dynamic-extent y ys k1 k2 k3 k4 f tau r v w a iw tmp q dq time-cell
                             moon0 sun0 moon1 sun1 moon2 sun2)
             (type double-float scale mdot))
    (vec3-scale! f force-body scale)
    (vec3-scale! tau torque-body scale)
    (replace y (ship-state-r state))
    (replace y (ship-state-v state) :start1 3)
    (replace y (ship-state-q state) :start1 6)
    (replace y (ship-state-w state) :start1 10)
    (setf (aref y 13) m-p0)
    (when (eq model :full)
      (if third-bodies
          (flet ((from! (moon sun start)
                   (replace moon third-bodies :start2 start)
                   (replace sun third-bodies :start2 (+ start 3))))
            (declare (inline from!))
            (from! moon0 sun0 0)
            (from! moon1 sun1 6)
            (from! moon2 sun2 12))
          (let ((h/2 (* 0.5d0 h)))
            (third-bodies-into! moon0 sun0 time)
            (third-bodies-into! moon1 sun1 (+ time h/2))
            (third-bodies-into! moon2 sun2 (+ time h)))))
    (flet ((derivative! (k y time moon sun)
             (declare (type state-vector k y) (type double-float time) (type vec3 moon sun))
             (vec3-set! r (aref y 0) (aref y 1) (aref y 2))
             (vec3-set! v (aref y 3) (aref y 4) (aref y 5))
             (earth-gravity-into! a r model moon sun)
             (if attitude-fn
                 (progn (setf (aref time-cell 0) time)
                        (funcall attitude-fn q tmp time-cell r v))
                 (quat-set! q (aref y 6) (aref y 7) (aref y 8) (aref y 9)))
             ;; v̇ = gravity + q (F / m) q*
             (quat-rotate! tmp q f)
             (vec3-add-scaled! a a tmp (/ (+ m-dry (aref y 13))))
             (setf (aref k 0) (aref y 3) (aref k 1) (aref y 4) (aref k 2) (aref y 5)
                   (aref k 3) (aref a 0) (aref k 4) (aref a 1) (aref k 5) (aref a 2)
                   (aref k 13) (- mdot))
             (if attitude-fn
                 (fill k 0d0 :start 6 :end 13)
                 (progn
                   (vec3-set! w (aref y 10) (aref y 11) (aref y 12))
                   (quat-derivative! dq q w)
                   (replace k dq :start1 6)
                   ;; I ω̇ = τ − ω × I ω
                   (mat3*vec3! iw inertia w)
                   (vec3-cross! tmp w iw)
                   (vec3-sub! tmp tau tmp)
                   (mat3*vec3! tmp inertia-inverse tmp)
                   (replace k tmp :start1 10)))
             k))
      (declare (inline derivative!))
      (let ((h/2 (* 0.5d0 h)))
        (derivative! k1 y time moon0 sun0)
        (derivative! k2 (state-vector-axpy! ys y k1 h/2) (+ time h/2) moon1 sun1)
        (derivative! k3 (state-vector-axpy! ys y k2 h/2) (+ time h/2) moon1 sun1)
        (derivative! k4 (state-vector-axpy! ys y k3 h) (+ time h) moon2 sun2)
        (dotimes (i 14)
          (setf (aref y i)
                (+ (aref y i)
                   (* (/ h 6) (+ (aref k1 i) (* 2 (aref k2 i)) (* 2 (aref k3 i))
                                 (aref k4 i))))))))
    (let* ((m-p1 (if exhausted 0d0 (max 0d0 (aref y 13))))
           (m0 (+ m-dry m-p0))
           (m1 (+ m-dry m-p1))
           (fn (vec3-norm f)))
      (vec3-set! (ship-state-r state) (aref y 0) (aref y 1) (aref y 2))
      (vec3-set! (ship-state-v state) (aref y 3) (aref y 4) (aref y 5))
      (if attitude-fn
          (let ((q (ship-state-q state)) (w (ship-state-w state)))
            ;; ω in body axes: q* ω_t q
            (setf (aref time-cell 0) (+ time h))
            (funcall attitude-fn q tmp time-cell (ship-state-r state) (ship-state-v state))
            (quat-conjugate! dq q)
            (quat-rotate! w dq tmp))
          (progn
            (quat-set! (ship-state-q state) (aref y 6) (aref y 7) (aref y 8) (aref y 9))
            (quat-normalize! (ship-state-q state) (ship-state-q state))
            (vec3-set! (ship-state-w state) (aref y 10) (aref y 11) (aref y 12))))
      (setf (ship-state-m-p state) m-p1)
      (incf (ship-state-dv state)
            (if (> mdot 0d0)
                (* (/ fn mdot) (log (the (double-float (0d0)) (/ m0 m1))))
                (/ (* fn h) m0)))
      state)))

;;; Objects (docs/architecture.md "Rails", "Removal of objects"; decision 110:
;;; Objects in the simulation: places.lisp's load position, the object struct
;;; and promotion checks). A definition is immutable and shared by every copy
;;; of an object; the object holds what the simulation changes.

(defstruct (shape (:constructor make-shape (kind centre &key size (radius 0d0))))
  (kind :box :type (member :box :sphere) :read-only t)
  (centre (make-vec3) :type vec3 :read-only t)      ; m, body axes about the CoM
  (size nil :type (or null vec3) :read-only t)      ; :box, edge lengths
  (radius 0d0 :type double-float :read-only t))    ; :sphere

(defstruct (fixture (:constructor make-fixture (name pin axis kind &key receiving (free t))))
  (name "" :type string :read-only t)
  (pin (make-vec3) :type vec3 :read-only t)         ; m, body axes about the CoM
  (axis (make-vec3) :type vec3 :read-only t)        ; outward, unit
  (kind :towable :type (member :towable :berth) :read-only t)
  (receiving nil :type boolean :read-only t)        ; takes deliveries (decision 53)
  (free t :type boolean :read-only t))              ; nil: a scenery ship holds it

(defstruct (attitude (:constructor make-attitude
                         (kind &key (q (quat 1 0 0 0)) (axis (vec3 1 0 0)) (rate 0d0)
                                    (epoch 0d0))))
  "An attitude mode: :lvlh, :sun-pointing, :inertial (q) or :spin (q at
EPOCH, turning about the body AXIS at RATE rad/s)."
  (kind :inertial :type (member :lvlh :sun-pointing :inertial :spin) :read-only t)
  (q (quat 1 0 0 0) :type quat :read-only t)
  (axis (vec3 1 0 0) :type vec3 :read-only t)       ; unit, body axes
  (rate 0d0 :type double-float :read-only t)
  (epoch 0d0 :type double-float :read-only t))

(defstruct (rails-segment (:constructor %make-rails-segment) (:copier nil))
  "A conic from START to END around BODY: ORBIT's elements at START, Ω and ω
advanced by their rates, M from MEAN-ANOMALY at START by MEAN-RATE (n + Ṁ)."
  (start 0d0 :type double-float :read-only t)
  (end 0d0 :type double-float :read-only t)
  (body :earth :type keyword :read-only t)
  (orbit nil :type orbit :read-only t)
  (mean-anomaly 0d0 :type double-float :read-only t)
  (raan-rate 0d0 :type double-float :read-only t)
  (argp-rate 0d0 :type double-float :read-only t)
  (mean-rate 0d0 :type double-float :read-only t)
  (end-kind :horizon :type (member :crash :horizon) :read-only t))

(defstruct (object-def (:constructor make-object-def
                           (key name kind mass tensor shapes fixtures attitude bound rails
                            &key depot &aux (tensor-inverse (mat3-inverse-symmetric tensor)))))
  (key nil :type keyword :read-only t)
  (name "" :type string :read-only t)               ; display name
  (kind :towable :type (member :towable :berth :fixed) :read-only t)
  (mass 0d0 :type double-float :read-only t)        ; kg
  (tensor (make-mat3) :type mat3 :read-only t)      ; kg m², body axes (FRD) about the CoM
  (tensor-inverse (make-mat3) :type mat3 :read-only t)
  (shapes nil :type list :read-only t)
  (fixtures nil :type list :read-only t)
  (attitude nil :type attitude :read-only t)
  (bound 0d0 :type double-float :read-only t)       ; m
  (rails nil :type list :read-only t)               ; the definition's segments
  (depot nil :type boolean :read-only t))           ; berthed here, B refuels (decision 113)

(defstruct (removal (:constructor make-removal (time body cause point)))
  (time 0d0 :type double-float :read-only t)
  (body :earth :type keyword :read-only t)
  (cause :burned-up :type (member :burned-up :delivered :impact) :read-only t)
  (point nil :read-only t))     ; (latitude longitude) of a crash; a delivery's place and fixture

(defstruct (object (:constructor %make-object
                       (def-of status rails attitude state next-check removal))
                   (:copier nil))
  (def-of nil :type object-def :read-only t)
  (status :rails :type (member :rails :promoted :latched :removed)) ; :latched: in the stack
  (rails nil :type list)                            ; segments, replaced whole
  (attitude nil :type attitude)                     ; replaced whole
  (state nil :type ship-state :read-only t)         ; promoted: r, v, q, ω (body); m_p 0
  (next-check 0d0 :type double-float)               ; the next promotion check's t
  (removal nil :type (or null removal)))

(defun new-object (def)
  "An object on its definition's rails, unpromoted."
  (%make-object def :rails (object-def-rails def) (object-def-attitude def)
                (make-ship-state (make-vec3) (make-vec3) (quat 1 0 0 0) (make-vec3) 0d0)
                0d0 nil))

(defun copy-object (object)
  "A copy, equalp to OBJECT, sharing the definition and the immutable parts."
  (%make-object (object-def-of object) (object-status object) (object-rails object)
                (object-attitude object) (copy-ship-state (object-state object))
                (object-next-check object) (object-removal object)))

(defun object-assign! (out object)
  "Copy OBJECT into OUT, an object of the same definition, consing nothing;
OUT."
  (declare (type object out object))
  (assert (eq (object-def-of out) (object-def-of object)))
  (setf (object-status out) (object-status object)
        (object-rails out) (object-rails object)
        (object-attitude out) (object-attitude object)
        (object-next-check out) (object-next-check object)
        (object-removal out) (object-removal object))
  (ship-state-assign! (object-state out) (object-state object))
  out)

(defun object-key (object) (object-def-key (object-def-of object)))
(defun object-removed-p (object) (eq (object-status object) :removed))
(defun object-promoted-p (object) (eq (object-status object) :promoted))

;;; Rails (docs/architecture.md "Rails", "Removal of objects"; decision 46:
;;; Rails across spheres, hyperbolas and re-entry). Milestone 3 builds one
;;; segment around Earth; milestone 4 adds segments across spheres.

(defparameter *rails-horizon* (utc->grid-t 2050 1 1 0 0 0)
  "The ephemerides' end, decision 18: Ephemerides.")
(defparameter *polar-cos* 1d-15
  "|cos i| below this gives Ω̇ = 0: the double nearest 90° keeps Ω exactly.")

(defun rails-crash-time (orbit start m0 mean-rate radius)
  "The first inbound crossing of RADIUS at or after START, M = M0 at START
advancing at MEAN-RATE; START if it is below RADIUS there; nil if never."
  (when (< (orbit-q orbit) radius)
    (let* ((e (orbit-e orbit))
           (c (max -1d0 (min 1d0 (/ (- (/ (orbit-semi-latus orbit) radius) 1) e))))
           (theta (acos c)))
      (if (elliptic-p orbit)
          (let* ((ea (- (true->eccentric theta e)))
                 (mc (- ea (* e (sin ea)))))
            (if (and (<= mc m0) (< m0 (- mc)))
                start
                (+ start (/ (mod (- mc m0) +two-pi+) mean-rate))))
          (let* ((h (- (true->hyperbolic theta e)))
                 (mc (- (* e (sinh h)) h)))
            (cond ((< m0 mc) (+ start (/ (- mc m0) mean-rate)))
                  ((< m0 (- mc)) start)))))))

(defun make-rails (orbit start &key (body :earth) (j2 t))
  "The segment list of an object on ORBIT (around BODY) from START: one
segment to the first inbound crossing of the crash radius, or to
*rails-horizon*; an Earth-centred ellipse with J2's secular rates unless J2
is nil."
  (let* ((body (body body))
         (orbit (copy-orbit orbit))
         (start (float start 1d0))
         (m0 (orbit-mean-anomaly orbit start)))
    (multiple-value-bind (raan-rate argp-rate mean-delta)
        (if (and j2 (elliptic-p orbit) (eq (body-key body) :earth))
            (orbit-j2-rates orbit body)
            (values 0d0 0d0 0d0))
      (when (< (abs (cos (orbit-i orbit))) *polar-cos*)
        (setf raan-rate 0d0))
      (let* ((mean-rate (+ (orbit-n orbit) mean-delta))
             (crash (rails-crash-time orbit start m0 mean-rate (body-crash-radius body)))
             (crash (and crash (< crash *rails-horizon*) crash)))
        (list (%make-rails-segment :start start
                                   :end (or crash (max start *rails-horizon*))
                                   :body (body-key body) :orbit orbit :mean-anomaly m0
                                   :raan-rate raan-rate :argp-rate argp-rate
                                   :mean-rate mean-rate
                                   :end-kind (if crash :crash :horizon)))))))

(defun rails-raan (segment time)
  "Ω of SEGMENT at TIME."
  (+ (orbit-raan (rails-segment-orbit segment))
     (* (rails-segment-raan-rate segment) (- time (rails-segment-start segment)))))

(declaim (sb-ext:maybe-inline rails-state-into!))

(defun rails-state-into! (r v segment time)
  "Write the state on SEGMENT at TIME (m, m/s, around its body) into R and V;
R. Conses nothing."
  (declare (optimize speed) (type vec3 r v) (type rails-segment segment)
           (type double-float time)
           (inline solve-kepler-elliptic solve-kepler-hyperbolic))
  (let* ((orbit (rails-segment-orbit segment))
         (dt (- time (rails-segment-start segment)))
         (e (orbit-e orbit)) (q (orbit-q orbit)) (mu (orbit-mu orbit))
         (m (+ (rails-segment-mean-anomaly segment) (* (rails-segment-mean-rate segment) dt)))
         (pv (make-vec3)) (qv (make-vec3)))
    (declare (dynamic-extent pv qv) (type double-float e q mu m))
    (perifocal-axes! pv qv (orbit-i orbit)
                     (+ (orbit-raan orbit) (* (rails-segment-raan-rate segment) dt))
                     (+ (orbit-argp orbit) (* (rails-segment-argp-rate segment) dt)))
    (multiple-value-bind (x y vx vy)
        (if (< e 1d0)
            (let* ((a (/ q (- 1 e)))
                   (ea (solve-kepler-elliptic (wrap-signed m) e))
                   (ce (cos ea)) (se (sin ea))
                   (b (sqrt (the (double-float 0d0) (- 1 (* e e)))))
                   (k (/ (sqrt (the (double-float 0d0) (* mu a))) (* a (- 1 (* e ce))))))
              (values (* a (- ce e)) (* a b se) (* k (- se)) (* k b ce)))
            (let* ((a (/ q (- e 1)))
                   (h (solve-kepler-hyperbolic m e))
                   (ch (cosh h)) (sh (sinh h))
                   (b (sqrt (the (double-float 0d0) (- (* e e) 1))))
                   (k (/ (sqrt (the (double-float 0d0) (* mu a))) (* a (- (* e ch) 1)))))
              (values (* q (- 1 (/ (- ch 1) (- e 1))))
                      (* q (sqrt (the (double-float 0d0) (/ (+ e 1) (- e 1)))) sh)
                      (* k (- sh)) (* k b ch))))
      (declare (type double-float x y vx vy))
      (dotimes (i 3 r)
        (setf (aref r i) (+ (* x (aref pv i)) (* y (aref qv i)))
              (aref v i) (+ (* vx (aref pv i)) (* vy (aref qv i))))))))

(declaim (inline rails-segment-at))

(defun rails-segment-at (rails time)
  "The segment of RAILS containing TIME: the first ending at or after it,
else the last."
  (loop for (segment . more) on rails
        when (or (null more) (<= time (rails-segment-end segment)))
          return segment))

;;; Attitude modes (docs/design.md "Rigid bodies: attitude, mass, shapes and
;;; fixtures"), a pure function of the state and t, on rails and promoted.

(declaim (sb-ext:maybe-inline object-attitude-into!))

(defun object-attitude-into! (q w mode r v time)
  "Write the attitude of MODE at the state (R, V) and TIME into Q (body ->
inertial) and its angular velocity, inertial axes, into W; Q. :lvlh: x the
horizontal along the motion, z to the centre, ω = h / r². :sun-pointing: x to
the Sun, z the ecliptic south pole made perpendicular to x. :inertial: q.
:spin: q₀ turned about the axis by rate (t − epoch). Conses nothing."
  (declare (optimize speed) (type quat q) (type vec3 w r v) (type attitude mode)
           (type double-float time))
  (let ((x (make-vec3)) (y (make-vec3)) (z (make-vec3)))
    (declare (dynamic-extent x y z))
    (ecase (attitude-kind mode)
      (:lvlh
       ;; y = −ĥ, z = −r̂, x = y × z
       (vec3-normalize! y (vec3-cross! y v r))
       (vec3-normalize! z (vec3-scale! z r -1d0))
       (vec3-cross! x y z)
       (quat-from-basis! q x y z)
       (vec3-scale! w (vec3-cross! w r v) (/ (vec3-dot r r))))
      (:sun-pointing
       (let ((s (make-vec3)) (ds (make-vec3)))
         (declare (dynamic-extent s ds))
         (body-state-into s ds :sun time :earth)
         (vec3-sub! s s r)
         (vec3-sub! ds ds v)
         (let ((sn (vec3-norm s)))
           (vec3-scale! x s (/ sn))
           ;; ẋ = (ṡ − (ṡ·x) x) / |s|, into ds
           (vec3-add-scaled! ds ds x (- (vec3-dot ds x)))
           (vec3-scale! ds ds (/ sn)))
         (vec3-set! z 0d0 (sin +obliquity+) (- (cos +obliquity+)))
         (let ((nx (vec3-dot z x)))
           (vec3-add-scaled! z z x (- nx))
           (let ((un (vec3-norm z)))
             (vec3-scale! z z (/ un))
             (vec3-cross! y z x)
             (quat-from-basis! q x y z)
             ;; ω = x × ẋ + ((n·x)(ẋ·y) / |n − (n·x) x|) x
             (vec3-cross! w x ds)
             (vec3-add-scaled! w w x (/ (* nx (vec3-dot ds y)) un))))))
      (:inertial
       (replace q (attitude-q mode))
       (vec3-set! w 0d0 0d0 0d0))
      (:spin
       (let* ((axis (attitude-axis mode))
              (rate (attitude-rate mode))
              (half (* 0.5d0 rate (- time (attitude-epoch mode))))
              (s (sin half))
              (turn (make-quat)))
         (declare (dynamic-extent turn))
         (quat-set! turn (cos half) (* s (aref axis 0)) (* s (aref axis 1)) (* s (aref axis 2)))
         (quat-mul! q (attitude-q mode) turn)
         (vec3-scale! w (quat-rotate! w q axis) rate))))
    q))

(defun object-state-into! (r v q w object time)
  "Write OBJECT's state at TIME into R, V (m, m/s, Earth-centred), Q (body ->
inertial) and W (rad/s, body axes); true, or nil for a removed object or a
time past its rails' crash end. A promoted object's state is the integrated
one, a latched load's the stack's (decision 112), valid only at the sim's t."
  (declare (type vec3 r v w) (type quat q) (type object object) (type double-float time))
  (ecase (object-status object)
    (:removed nil)
    ((:promoted :latched)
     (let ((state (object-state object)))
       (replace r (ship-state-r state))
       (replace v (ship-state-v state))
       (replace q (ship-state-q state))
       (replace w (ship-state-w state))
       t))
    (:rails
     (let ((segment (rails-segment-at (object-rails object) time)))
       (unless (and (eq (rails-segment-end-kind segment) :crash)
                    (> time (rails-segment-end segment)))
         (rails-state-into! r v segment time)
         (object-attitude-into! q w (object-attitude object) r v time)
         (let ((conjugate (make-quat)))
           (declare (dynamic-extent conjugate))
           (quat-rotate! w (quat-conjugate! conjugate q) w))
         t)))))

;;; Promotion, demotion and the promoted step (docs/architecture.md "Rails";
;;; decision 19: Time-warp rules, decision 110, decision 134: Objects in the
;;; sim: the shared third-body reads, the principal-axis test and rechecks).
;;; clock.lisp decides when.

(defun object-promote! (object time)
  "OBJECT, on rails, integrated from TIME on: its rails state and attitude
at TIME, exact at that instant. True, or nil past its rails' crash end."
  (declare (type object object) (type double-float time))
  (let ((state (object-state object)))
    (when (object-state-into! (ship-state-r state) (ship-state-v state)
                              (ship-state-q state) (ship-state-w state) object time)
      (setf (ship-state-m-p state) 0d0
            (ship-state-dv state) 0d0
            (object-status object) :promoted)
      t)))

(defparameter *spin-axis-tolerance* (deg->rad 1d0)
  "rad: a demoted towable object's ω this close to I ω spins on rails.")

(defun demotion-attitude (object time)
  "The attitude mode OBJECT keeps on rails from TIME: a berth or fixed
object's own; a towable object's :spin about ω (body axes) from its q at
TIME when ω is a principal axis within *spin-axis-tolerance* (ω ∥ I ω, so
a degenerate tensor's whole eigenspace counts), else :inertial at q."
  (let* ((def (object-def-of object))
         (state (object-state object))
         (q (quat-copy (ship-state-q state)))
         (w (ship-state-w state))
         (rate (vec3-norm w)))
    (if (not (eq (object-def-kind def) :towable))
        (object-attitude object)
        (let ((iw (mat3*vec3 (object-def-tensor def) w)))
          (if (and (> rate 0d0)
                   (<= (vec3-norm (vec3-cross w iw))
                       (* (sin *spin-axis-tolerance*) rate (vec3-norm iw))))
              (make-attitude :spin :q q :axis (vec3-scale w (/ rate)) :rate rate :epoch time)
              (make-attitude :inertial :q q))))))

(defun object-demote! (object time &optional (body :earth))
  "OBJECT, promoted, back on rails from TIME: make-rails of its osculating
elements around BODY (J2's rates for an Earth ellipse), its attitude mode
by demotion-attitude."
  (declare (type object object) (type double-float time))
  (let ((state (object-state object)))
    (setf (object-attitude object) (demotion-attitude object time)
          (object-rails object) (make-rails (state->orbit (ship-state-r state) (ship-state-v state)
                                                          (body-mu (body body)) time)
                                            time :body body)
          (object-status object) :rails)
    object))

(defvar *object-attitude*)

(defun object-kinematic-attitude (q w time-cell r v)
  "rk4-step!'s attitude function for a berth or fixed object: the mode
*object-attitude*, without a closure per step."
  (declare (optimize speed) (type quat q) (type vec3 w r v)
           (type (simple-array double-float (1)) time-cell) (inline object-attitude-into!))
  (object-attitude-into! q w *object-attitude* r v (aref time-cell 0)))

;; Hot callers declare it inline: a full call boxes h and t.
(declaim (sb-ext:maybe-inline object-step!))

(defun object-step! (object h time model third-bodies)
  "Advance promoted OBJECT by h from TIME under MODEL's gravity, with
THIRD-BODIES as rk4-step! takes them: no force, torque or ṁ, its mass and
tensor; a towable object's attitude integrated torque-free, a berth or
fixed object's its mode's (kinematic). Conses nothing."
  (declare (optimize speed) (type object object) (type double-float h time)
           (type keyword model) (type (or null third-bodies) third-bodies)
           (inline rk4-step!))
  (let ((def (object-def-of object))
        (zero (load-time-value (make-vec3) t))
        (*object-attitude* (object-attitude object)))
    (rk4-step! (object-state object) h time model zero zero 0d0 (object-def-mass def)
               (object-def-tensor def) (object-def-tensor-inverse def)
               (if (eq (object-def-kind def) :towable) nil #'object-kinematic-attitude)
               third-bodies)))
