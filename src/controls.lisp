(in-package #:luv-in-space)

;;; The control state that input, flight scripts and the autopilot write,
;;; the flight computer, the RCS mixer and flight-step!, one step of the ship
;;; (docs/design.md "Controls", "Control modes"; decision 70: Flight
;;; computer: per-step control laws, holds and the RCS mixer, decision 29:
;;; Control scheme: key map, control modes, gamepad).

;;; Control state

(deftype key-axes () '(simple-array fixnum (3)))
(deftype duty-cycles () '(simple-array double-float (24)))
(deftype hold-kind ()
  '(member :prograde :retrograde :normal :anti-normal :radial-out :radial-in :inertial))

(declaim (inline make-key-axes make-duty-cycles))
(defun make-key-axes () (make-array 3 :element-type 'fixnum :initial-element 0))
(defun make-duty-cycles () (make-array 24 :element-type 'double-float :initial-element 0d0))

(defstruct (controls (:constructor %make-controls) (:copier nil))
  (throttle 0d0 :type double-float)                     ; lever: 0 or 0.1–1
  (rotation-keys (make-key-axes) :type key-axes)        ; −1, 0, +1 about body x, y, z (roll, pitch, yaw)
  (translation-keys (make-key-axes) :type key-axes)     ; −1, 0, +1 along body x, y, z
  (fine nil :type boolean)
  (attitude-mode :hold :type (member :direct :rate-damping :hold))
  (hold :prograde :type hold-kind)
  (inertial-target (quat 1 0 0 0) :type quat)           ; body -> inertial, for :inertial
  (roll-offset 0d0 :type double-float)                  ; rad about the hold axis
  (settled nil :type boolean)
  (duties (make-duty-cycles) :type duty-cycles))        ; the last step's, *rcs-positions* order

(defun new-controls ()
  "The start's controls: a prograde hold over rate damping, throttle off
(decision 38: Start orbit: polar)."
  (%make-controls))

(defun copy-controls (controls)
  "A deep copy: equalp to CONTROLS, sharing no array."
  (let ((copy (copy-structure controls)))
    (setf (controls-rotation-keys copy) (copy-seq (controls-rotation-keys controls))
          (controls-translation-keys copy) (copy-seq (controls-translation-keys controls))
          (controls-inertial-target copy) (quat-copy (controls-inertial-target controls))
          (controls-duties copy) (copy-seq (controls-duties controls)))
    copy))

(defun controls-assign! (out controls)
  "Copy CONTROLS into OUT, consing nothing; OUT."
  (declare (type controls out controls))
  (setf (controls-throttle out) (controls-throttle controls)
        (controls-fine out) (controls-fine controls)
        (controls-attitude-mode out) (controls-attitude-mode controls)
        (controls-hold out) (controls-hold controls)
        (controls-roll-offset out) (controls-roll-offset controls)
        (controls-settled out) (controls-settled controls))
  (replace (controls-rotation-keys out) (controls-rotation-keys controls))
  (replace (controls-translation-keys out) (controls-translation-keys controls))
  (replace (controls-inertial-target out) (controls-inertial-target controls))
  (replace (controls-duties out) (controls-duties controls))
  out)

;;; Commands (docs/design.md "Controls"); each returns CONTROLS.

(defun controls-set-throttle (controls throttle)
  "Set the lever; below *throttle-min* is off, above 1 is 1."
  (setf (controls-throttle controls)
        (let ((throttle (float throttle 1d0)))
          (if (< throttle *throttle-min*) 0d0 (min throttle 1d0))))
  controls)

(defun controls-throttle-full (controls) (controls-set-throttle controls 1d0))
(defun controls-throttle-cut (controls) (controls-set-throttle controls 0d0))

(defun controls-set-rotation-key (controls axis sign)
  "AXIS :roll, :pitch or :yaw; SIGN −1, 0 or +1 (held keys). A pitch or yaw
key releases a hold to rate damping."
  (declare (type (integer -1 1) sign))
  (setf (aref (controls-rotation-keys controls) (ecase axis (:roll 0) (:pitch 1) (:yaw 2)))
        sign)
  (when (and (/= sign 0) (not (eq axis :roll)) (eq (controls-attitude-mode controls) :hold))
    (setf (controls-attitude-mode controls) :rate-damping
          (controls-settled controls) nil))
  controls)

(defun controls-set-translation-key (controls axis sign)
  "AXIS :x, :y or :z (body); SIGN −1, 0 or +1 (held keys)."
  (declare (type (integer -1 1) sign))
  (setf (aref (controls-translation-keys controls) (ecase axis (:x 0) (:y 1) (:z 2)))
        sign)
  controls)

;;; The held-key map (decision 81: Input in milestone 2: held keys per
;;; frame, taps, gating and the shared key map), shared by input.lisp and
;;; the flight scripts' (:keys …).

(defparameter *flight-keys*
  '((:w :rotation :pitch -1) (:s :rotation :pitch 1)
    (:a :rotation :yaw -1) (:d :rotation :yaw 1)
    (:q :rotation :roll -1) (:e :rotation :roll 1)
    (:h :translation :x 1) (:n :translation :x -1)
    (:i :translation :z -1) (:k :translation :z 1)
    (:j :translation :y -1) (:l :translation :y 1))
  "(key :rotation|:translation axis sign): luv's physical key names.")

(defun held-keys-sign (keys kind axis)
  (signum (loop for (key k a sign) in *flight-keys*
                when (and (eq k kind) (eq a axis) (member key keys))
                  sum sign)))

(defun controls-apply-held-keys! (controls keys)
  "Set each rotation and translation key state to the sum of the signs of
its held KEYS (opposite keys cancel), calling the setters only on a change
so their hold rules apply then; CONTROLS."
  (loop for axis in '(:roll :pitch :yaw)
        for i from 0
        for sign = (held-keys-sign keys :rotation axis)
        unless (= sign (aref (controls-rotation-keys controls) i))
          do (controls-set-rotation-key controls axis sign))
  (loop for axis in '(:x :y :z)
        for i from 0
        for sign = (held-keys-sign keys :translation axis)
        unless (= sign (aref (controls-translation-keys controls) i))
          do (controls-set-translation-key controls axis sign))
  controls)

(defun controls-toggle-fine (controls)
  (setf (controls-fine controls) (not (controls-fine controls)))
  controls)

(defun controls-set-hold (controls kind)
  "Hold KIND (:prograde … :radial-in) with the default roll; the active
hold's key again releases it to rate damping."
  (check-type kind (member :prograde :retrograde :normal :anti-normal :radial-out :radial-in))
  (if (and (eq (controls-attitude-mode controls) :hold) (eq (controls-hold controls) kind))
      (setf (controls-attitude-mode controls) :rate-damping)
      (setf (controls-attitude-mode controls) :hold
            (controls-hold controls) kind
            (controls-roll-offset controls) 0d0))
  (setf (controls-settled controls) nil)
  controls)

(defun controls-inertial-hold (controls q)
  "R: hold the attitude Q (the ship's, body -> inertial); again releases it."
  (declare (type quat q))
  (if (and (eq (controls-attitude-mode controls) :hold) (eq (controls-hold controls) :inertial))
      (setf (controls-attitude-mode controls) :rate-damping)
      (setf (controls-attitude-mode controls) :hold
            (controls-hold controls) :inertial
            (controls-roll-offset controls) 0d0
            (controls-inertial-target controls) (replace (controls-inertial-target controls) q)))
  (setf (controls-settled controls) nil)
  controls)

(defun controls-toggle-rate-damping (controls)
  "T: a hold or direct goes to rate damping, rate damping to direct."
  (setf (controls-attitude-mode controls)
        (if (eq (controls-attitude-mode controls) :rate-damping) :direct :rate-damping)
        (controls-settled controls) nil)
  controls)

;;; The RCS mixer (decision 70): duty cycles u in [0, 1] minimising
;;; |w (B_τ u − τ_c)|² + |B_F u − F_c|² + e² |u|² by Stark–Parker BVLS,
;;; Householder QR on the free columns in their fixed order.

(defparameter *mixer-torque-weight* 1d5
  "w per metre, decision 70; at 1e6 rounding loses the force fit beside a torque.")
(defparameter *mixer-duty-weight* 1d-4 "e in N, decision 70.")
(defparameter *mixer-iteration-limit* 96 "Least-squares solves per mix, decision 70.")
(declaim (type double-float *mixer-torque-weight* *mixer-duty-weight*)
         (type fixnum *mixer-iteration-limit*))

(defun rcs-mix! (u b tau-c f-c)
  "Write into U the duty cycles of the 24 thrusters of B (rcs-matrix!) that
give the body torque TAU-C exactly when it is reachable and as much of the
force F-C as fits; zero command gives zeros. Conses nothing."
  (declare (optimize speed) (type duty-cycles u) (type rcs-matrix b) (type vec3 tau-c f-c))
  (let* ((w *mixer-torque-weight*)
         (e *mixer-duty-weight*)
         (e2 (* e e))
         ;; A: rows 0–2 w B_τ, rows 3–5 B_F; y the weighted command.
         (a (make-array '(6 24) :element-type 'double-float))
         (y (make-array 6 :element-type 'double-float))
         (r (make-array 6 :element-type 'double-float))
         (noise (make-array 6 :element-type 'double-float))
         (g (make-array 24 :element-type 'double-float))
         (tol (make-array 24 :element-type 'double-float))
         (z (make-array 24 :element-type 'double-float))
         (m (make-array '(30 24) :element-type 'double-float))
         (rhs (make-array 30 :element-type 'double-float))
         (rdiag (make-array 24 :element-type 'double-float))
         (free (make-array 24 :element-type 'fixnum))
         ;; 0 at 0, 1 free, 2 at 1, 3 and 4: at 0 or 1, refused this round
         (state (make-array 24 :element-type '(integer 0 4) :initial-element 0))
         (solves 0))
    (declare (dynamic-extent a y r noise g tol z m rhs rdiag free state) (type fixnum solves))
    (fill u 0d0)
    (dotimes (j 24)
      (dotimes (i 3)
        (setf (aref a i j) (* w (aref b (+ i 3) j))
              (aref a (+ i 3) j) (aref b i j))))
    (dotimes (i 3)
      (setf (aref y i) (* w (aref tau-c i))
            (aref y (+ i 3)) (aref f-c i)))
    (labels ((solve! ()
               ;; z_c for the free columns free[c]: least squares on
               ;; [A_free; e 𝟙] z = [y − Σ_{u=1} A_j; 0].
               (let* ((k (let ((k 0))
                           (declare (type fixnum k))
                           (dotimes (j 24 k)
                             (when (= (aref state j) 1)
                               (setf (aref free k) j)
                               (incf k)))))
                      (nr (+ 6 k)))
                 (declare (type fixnum k nr))
                 (incf solves)
                 (dotimes (c k)
                   (let ((j (aref free c)))
                     (dotimes (i 6) (setf (aref m i c) (aref a i j)))
                     (loop for i of-type fixnum from 6 below nr
                           do (setf (aref m i c) (if (= i (+ 6 c)) e 0d0)))))
                 (dotimes (i 6) (setf (aref rhs i) (aref y i)))
                 (dotimes (j 24)
                   (when (member (aref state j) '(2 4))
                     (dotimes (i 6) (decf (aref rhs i) (aref a i j)))))
                 (loop for i of-type fixnum from 6 below nr do (setf (aref rhs i) 0d0))
                 (dotimes (c k)
                   (let ((s 0d0))
                     (declare (type double-float s))
                     (loop for i of-type fixnum from c below nr
                           do (incf s (expt (aref m i c) 2)))
                     (let* ((old (aref m c c))
                            (norm (sqrt (the (double-float 0d0) s)))
                            (alpha (if (> old 0d0) (- norm) norm))
                            (vtv (* 2 (- s (* old alpha)))))
                       (setf (aref m c c) (- old alpha)
                             (aref rdiag c) alpha)
                       (flet ((reflect (column)
                                (declare (type fixnum column))
                                (let ((d 0d0))
                                  (declare (type double-float d))
                                  (loop for i of-type fixnum from c below nr
                                        do (incf d (* (aref m i c) (aref m i column))))
                                  (let ((f (/ (* 2 d) vtv)))
                                    (loop for i of-type fixnum from c below nr
                                          do (decf (aref m i column) (* f (aref m i c))))))))
                         (loop for column of-type fixnum from (1+ c) below k do (reflect column))
                         (let ((d 0d0))
                           (declare (type double-float d))
                           (loop for i of-type fixnum from c below nr
                                 do (incf d (* (aref m i c) (aref rhs i))))
                           (let ((f (/ (* 2 d) vtv)))
                             (loop for i of-type fixnum from c below nr
                                   do (decf (aref rhs i) (* f (aref m i c))))))))))
                 (loop for c of-type fixnum from (1- k) downto 0
                       do (let ((s (aref rhs c)))
                            (declare (type double-float s))
                            (loop for cc of-type fixnum from (1+ c) below k
                                  do (decf s (* (aref m c cc) (aref z (aref free cc)))))
                            (setf (aref z (aref free c)) (/ s (aref rdiag c)))))
                 k))
             (gradient! ()
               ;; g = Aᵀ (y − A u) − e² u, and tol its rounding error: a
               ;; gradient below it frees no column.
               (dotimes (i 6)
                 (let ((s (aref y i)) (n (abs (aref y i))))
                   (declare (type double-float s n))
                   (dotimes (j 24)
                     (let ((x (* (aref a i j) (aref u j))))
                       (decf s x)
                       (incf n (abs x))))
                   (setf (aref r i) s
                         (aref noise i) (* 16 double-float-epsilon n))))
               (dotimes (j 24)
                 (let ((s (- (* e2 (aref u j)))) (n 0d0))
                   (declare (type double-float s n))
                   (dotimes (i 6)
                     (incf s (* (aref a i j) (aref r i)))
                     (incf n (abs (* (aref a i j) (aref noise i)))))
                   (setf (aref g j) s
                         (aref tol j) n))))
             (pick ()
               ;; The bound column whose gradient points inside furthest,
               ;; or nil: the KKT conditions hold.
               (let ((best -1) (best-g 0d0))
                 (declare (type fixnum best) (type double-float best-g))
                 (dotimes (j 24)
                   (let ((gj (case (aref state j)
                               (0 (aref g j))
                               (2 (- (aref g j)))
                               (t 0d0))))
                     (when (and (> gj (aref tol j)) (> gj best-g))
                       (setf best j best-g gj))))
                 (when (>= best 0) best))))
      (gradient!)
      (loop
        (let ((t-col (pick)))
          (when (or (null t-col) (>= solves *mixer-iteration-limit*))
            (return))
          (let ((from (aref state t-col)))
            (setf (aref state t-col) 1)
            (solve!)
            (if (if (= from 0) (<= (aref z t-col) 0d0) (>= (aref z t-col) 1d0))
                ;; Moving t would go the wrong way (rounding): refuse it this round.
                (setf (aref state t-col) (if (= from 0) 3 4))
                (loop
                  (let ((alpha 1d0) (leaving -1))
                    (declare (type double-float alpha) (type fixnum leaving))
                    (dotimes (j 24)
                      (when (= (aref state j) 1)
                        (let ((zj (aref z j)) (uj (aref u j)))
                          (cond ((< zj 0d0)
                                 (let ((s (/ uj (- uj zj))))
                                   (when (< s alpha) (setf alpha s leaving j))))
                                ((> zj 1d0)
                                 (let ((s (/ (- 1d0 uj) (- zj uj))))
                                   (when (< s alpha) (setf alpha s leaving j))))))))
                    (dotimes (j 24)
                      (when (= (aref state j) 1)
                        (setf (aref u j) (+ (aref u j) (* alpha (- (aref z j) (aref u j)))))))
                    (when (< leaving 0)
                      (return))
                    ;; Columns reaching a bound leave the free set.
                    (dotimes (j 24)
                      (when (= (aref state j) 1)
                        (cond ((or (= j leaving) (<= (aref u j) 0d0) (>= (aref u j) 1d0))
                               (let ((upper (if (= j leaving)
                                                (> (aref z j) 1d0)
                                                (>= (aref u j) 1d0))))
                                 (setf (aref u j) (if upper 1d0 0d0)
                                       (aref state j) (if upper 2 0)))))))
                    (when (>= solves *mixer-iteration-limit*)
                      (return))
                    (solve!)))))
          ;; A step taken: refused columns may be tried again.
          (unless (> (aref state t-col) 2)
            (dotimes (j 24)
              (case (aref state j)
                (3 (setf (aref state j) 0))
                (4 (setf (aref state j) 2)))))
          (gradient!))))
    u))

;;; Holds (docs/design.md "Control modes"; docs/architecture.md "Frames"):
;;; the target attitude and its frame's rate.

(declaim (inline hold-attitude-into!))

(defun hold-attitude-into! (q w-t kind r v roll-offset inertial-target)
  "Write the hold's target attitude (body -> inertial) into Q and its frame's
rate (rad/s, inertial axes) into W-T. Orbital holds: the nose along the
hold direction of the orbital frame (prograde v, normal r × v, radial out
completing the triple), the cab (−z) radial out, towards normal for the
radial holds; :inertial holds INERTIAL-TARGET. Then ROLL-OFFSET (rad) about
the nose."
  (declare (type quat q inertial-target) (type vec3 w-t r v) (type hold-kind kind)
           (type double-float roll-offset))
  (if (eq kind :inertial)
      (progn (replace q inertial-target)
             (vec3-set! w-t 0d0 0d0 0d0))
      (let ((p (make-vec3)) (n (make-vec3)) (o (make-vec3))
            (x (make-vec3)) (y (make-vec3)) (z (make-vec3)))
        (declare (dynamic-extent p n o x y z))
        (vec3-normalize! p v)
        (vec3-normalize! n (vec3-cross! n r v))
        (vec3-cross! o p n)
        (flet ((axes (nose nose-sign cab cab-sign)
                 (declare (type vec3 nose cab) (type double-float nose-sign cab-sign))
                 (vec3-scale! x nose nose-sign)
                 (vec3-scale! z cab (- cab-sign))))
          (declare (inline axes))
          (ecase kind
            (:prograde (axes p 1d0 o 1d0))
            (:retrograde (axes p -1d0 o 1d0))
            (:normal (axes n 1d0 o 1d0))
            (:anti-normal (axes n -1d0 o 1d0))
            (:radial-out (axes o 1d0 n 1d0))
            (:radial-in (axes o -1d0 n 1d0))))
        (vec3-cross! y z x)
        (quat-from-basis! q x y z)
        ;; The frame turns as point-mass gravity bends v: ω_t = v̂ × a / |v|.
        (let* ((rn (vec3-norm r))
               (k (/ (- +mu-earth+) (* rn rn rn))))
          (vec3-scale! w-t (vec3-cross! w-t p r) (/ k (vec3-norm v))))))
  (unless (= roll-offset 0d0)
    (let ((roll (make-quat)))
      (declare (dynamic-extent roll))
      (quat-set! roll (cos (* 0.5d0 roll-offset)) (sin (* 0.5d0 roll-offset)) 0d0 0d0)
      (quat-mul! q q roll)))
  q)

(defun hold-attitude (kind r v roll-offset &optional (inertial-target (quat 1 0 0 0)))
  "The hold's target attitude and its frame's rate (inertial axes), fresh:
(values q w-t); see hold-attitude-into!."
  (let ((q (make-quat)) (w-t (make-vec3)))
    (hold-attitude-into! q w-t kind r v (float roll-offset 1d0) inertial-target)
    (values q w-t)))

(defun controls-attitude-fn (controls)
  "The active hold's attitude function for rk4-step!'s kinematic mode (above
×4), or nil without a hold. It keeps the hold, roll offset and inertial
target of this moment."
  (when (eq (controls-attitude-mode controls) :hold)
    (let ((kind (controls-hold controls))
          (roll-offset (controls-roll-offset controls))
          (target (quat-copy (controls-inertial-target controls))))
      (declare (type hold-kind kind) (type double-float roll-offset) (type quat target))
      (lambda (q w-t time-cell r v)
        (declare (optimize speed) (ignore time-cell) (type quat q) (type vec3 w-t r v))
        (hold-attitude-into! q w-t kind r v roll-offset target)))))

;;; The flight computer (decision 70): commanded body torque and force, once
;;; per step from the state at its start.

(defparameter *fine-control* 0.1d0
  "Torque, force and rate factor in fine control, docs/design.md \"Controls\".")
(defparameter *damping-rate* (* 5 (/ pi 180))
  "rad/s, a held key's rate and the slew cap, docs/design.md \"Control modes\".")
(defparameter *damping-time* 0.5d0 "s, the rate loop's, docs/design.md \"Control modes\".")
(defparameter *hold-torque-fraction* 0.8d0
  "Of the couple, for slews and holds, docs/design.md \"Control modes\".")
(defparameter *hold-tail-gain* 0.5d0 "1/s, the slew's linear tail, decision 70.")
(defparameter *settle-angle-in* (* 0.1d0 (/ pi 180))
  "rad, settled below it, docs/design.md \"Control modes\".")
(defparameter *settle-rate-in* (* 0.02d0 (/ pi 180)) "rad/s, as *settle-angle-in*.")
(defparameter *settle-angle-out* (* 0.2d0 (/ pi 180)) "rad, unsettled above it, decision 70.")
(defparameter *settle-rate-out* (* 0.05d0 (/ pi 180)) "rad/s, as *settle-angle-out*.")
(declaim (type double-float *fine-control* *damping-rate* *damping-time*
               *hold-torque-fraction* *hold-tail-gain* *settle-angle-in* *settle-rate-in*
               *settle-angle-out* *settle-rate-out*))

(declaim (inline rate-law! hold-rate-command! flight-commands!))

(defun rate-law! (tau-c w-c w inertia limit)
  "τ = I (ω_c − ω) / *damping-time* + ω × I ω, each body axis clamped to
LIMIT × its couple."
  (declare (type vec3 tau-c w-c w) (type mat3 inertia) (type double-float limit))
  (let ((e (make-vec3)) (iw (make-vec3)))
    (declare (dynamic-extent e iw))
    (vec3-scale! e (vec3-sub! e w-c w) (/ *damping-time*))
    (mat3*vec3! tau-c inertia e)
    (vec3-cross! e w (mat3*vec3! iw inertia w))
    (vec3-add! tau-c tau-c e)
    (dotimes (axis 3 tau-c)
      (let ((c (* limit (rcs-couple axis))))
        (setf (aref tau-c axis) (max (- c) (min c (aref tau-c axis))))))))

(defun hold-rate-command! (w-c controls state inertia fine roll-rate)
  "The hold's commanded rate (body axes) into W-C: the frame's rate, the
roll keys' and a slew towards the target, √(2 × 0.8 α |φ|) braking,
*hold-tail-gain* |φ| near it, at most *damping-rate*; updates settled."
  (declare (type vec3 w-c) (type controls controls) (type ship-state state)
           (type mat3 inertia) (type double-float fine roll-rate))
  (let ((q (ship-state-q state)) (w (ship-state-w state))
        (q-t (make-quat)) (q-e (make-quat)) (w-t (make-vec3)) (axis (make-vec3))
        (i-axis (make-vec3)))
    (declare (dynamic-extent q-t q-e w-t axis i-axis))
    (hold-attitude-into! q-t w-t (controls-hold controls)
                         (ship-state-r state) (ship-state-v state)
                         (controls-roll-offset controls) (controls-inertial-target controls))
    ;; The frame's rate in body axes: q* ω_t q.
    (quat-conjugate! q-e q)
    (quat-rotate! w-c q-e w-t)
    ;; The error rotation q* q_t, shortest arc; φ in body axes.
    (quat-mul! q-e q-e q-t)
    (when (minusp (aref q-e 0))
      (dotimes (i 4) (setf (aref q-e i) (- (aref q-e i)))))
    (vec3-set! axis (aref q-e 1) (aref q-e 2) (aref q-e 3))
    (let* ((s (vec3-norm axis))
           (angle (* 2 (atan s (aref q-e 0))))
           (rate-error (let ((d (make-vec3)))
                         (declare (dynamic-extent d))
                         (vec3-norm (vec3-sub! d w w-c)))))
      (setf (controls-settled controls)
            (if (controls-settled controls)
                (not (or (> angle *settle-angle-out*) (> rate-error *settle-rate-out*)))
                (and (< angle *settle-angle-in*) (< rate-error *settle-rate-in*))))
      (incf (aref w-c 0) roll-rate)
      (when (> s 0d0)
        (vec3-scale! axis axis (/ s))
        (mat3*vec3! i-axis inertia axis)
        (let ((alpha most-positive-double-float))
          (declare (type double-float alpha))
          (dotimes (i 3)
            (let ((need (abs (aref i-axis i))))
              (when (> need 0d0)
                (setf alpha (min alpha (/ (* *hold-torque-fraction* fine (rcs-couple i))
                                          need))))))
          (vec3-add-scaled! w-c w-c axis
                            (min (* fine *damping-rate*)
                                 (sqrt (the (double-float 0d0)
                                            (* 2 *hold-torque-fraction* alpha angle)))
                                 (* *hold-tail-gain* angle)))))
      w-c)))

(defun flight-commands! (tau-c f-c controls state com inertia h)
  "The commanded torque (N m) and force (N), body axes, about the CoM COM,
for the step of h from STATE. Direct: a held rotation key commands its
axis's couple. Rate damping: held keys command *damping-rate*, none zero.
Hold: the slew and hold of hold-rate-command!, roll keys turning the roll
offset. Translation is always direct, the axis's torque-free force.
×*fine-control* in fine."
  (declare (type vec3 tau-c f-c com) (type controls controls) (type ship-state state)
           (type mat3 inertia) (type double-float h))
  (let ((fine (if (controls-fine controls) *fine-control* 1d0))
        (rotation (controls-rotation-keys controls))
        (translation (controls-translation-keys controls))
        (w-c (make-vec3)))
    (declare (dynamic-extent w-c))
    (ecase (controls-attitude-mode controls)
      (:direct
       (setf (controls-settled controls) nil)
       (dotimes (axis 3)
         (setf (aref tau-c axis) (* fine (float (aref rotation axis) 1d0) (rcs-couple axis)))))
      (:rate-damping
       (setf (controls-settled controls) nil)
       (dotimes (axis 3)
         (setf (aref w-c axis) (* fine *damping-rate* (float (aref rotation axis) 1d0))))
       (rate-law! tau-c w-c (ship-state-w state) inertia fine))
      (:hold
       (let ((roll-rate (* fine *damping-rate* (float (aref rotation 0) 1d0))))
         (incf (controls-roll-offset controls) (* roll-rate h))
         (hold-rate-command! w-c controls state inertia fine roll-rate)
         (rate-law! tau-c w-c (ship-state-w state) inertia (* *hold-torque-fraction* fine)))))
    (dotimes (axis 3)
      (setf (aref f-c axis)
            (* fine (float (aref translation axis) 1d0)
               (rcs-translation-capability axis com))))
    tau-c))

;;; One step of the ship

;; Hot callers declare it inline: a full call boxes h and t.
(declaim (sb-ext:maybe-inline flight-step!))

(defun flight-step! (state controls h time model &optional attitude-fn)
  "Advance STATE by h from TIME under CONTROLS: mass properties from m_p,
flight computer (its state into CONTROLS), mixer (duties into CONTROLS),
engine plus RCS force,
torque and ṁ, then rk4-step!. With ATTITUDE-FN (kinematic, above ×4) the
engine only. Conses nothing."
  (declare (optimize speed) (type ship-state state) (type controls controls)
           (type double-float h time) (type keyword model)
           (type (or null function) attitude-fn)
           (inline ship-mass-properties rk4-step!))
  (let* ((com (make-vec3)) (inertia (make-mat3)) (inverse (make-mat3))
         (properties (make-mass-properties :com com :inertia inertia
                                           :inertia-inverse inverse))
         (b (make-array '(6 24) :element-type 'double-float))
         (tau-c (make-vec3)) (f-c (make-vec3))
         (force (make-vec3)) (torque (make-vec3))
         (u (controls-duties controls))
         (m-p (ship-state-m-p state))
         (throttle (controls-throttle controls))
         (mdot (ship-engine-mdot throttle)))
    (declare (dynamic-extent com inertia inverse properties b tau-c f-c force torque)
             (type double-float mdot))
    (ship-mass-properties m-p nil properties)
    (cond (attitude-fn
           ;; Kinematic: the attitude is the hold's.
           (fill u 0d0)
           (setf (controls-settled controls) (eq (controls-attitude-mode controls) :hold)))
          ((<= m-p 0d0) (fill u 0d0))
          (t
           (rcs-matrix! b com)
           (flight-commands! tau-c f-c controls state com inertia h)
           (rcs-mix! u b tau-c f-c)))
    (vec3-set! force (ship-engine-force throttle) 0d0 0d0)
    (let ((rcs-mdot (/ *rcs-thrust* (* *rcs-isp* +g0+))))
      (dotimes (i +rcs-count+)
        (let ((ui (aref u i)))
          (when (> ui 0d0)
            (incf (aref force 0) (* ui (aref b 0 i)))
            (incf (aref force 1) (* ui (aref b 1 i)))
            (incf (aref force 2) (* ui (aref b 2 i)))
            (incf (aref torque 0) (* ui (aref b 3 i)))
            (incf (aref torque 1) (* ui (aref b 4 i)))
            (incf (aref torque 2) (* ui (aref b 5 i)))
            (incf mdot (* ui rcs-mdot))))))
    (rk4-step! state h time model force torque mdot *ship-dry-mass* inertia inverse
               attitude-fn)))
