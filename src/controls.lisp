(in-package #:luv-in-space)

;;; The control state that input, flight scripts and the autopilot write,
;;; the flight computer, the RCS mixer and flight-step!, one step of the ship
;;; (docs/design.md "Controls", "Control modes"; decision 70: Flight
;;; computer: per-step control laws, holds and the RCS mixer, decision 29:
;;; Control scheme: key map, control modes, gamepad).

;;; Control state

(deftype key-axes () '(simple-array fixnum (3)))
(deftype duty-cycles () '(simple-array double-float (24)))

(declaim (inline make-key-axes make-duty-cycles))
(defun make-key-axes () (make-array 3 :element-type 'fixnum :initial-element 0))
(defun make-duty-cycles () (make-array 24 :element-type 'double-float :initial-element 0d0))

(defstruct (controls (:constructor %make-controls) (:copier nil))
  (throttle 0d0 :type double-float)                     ; lever: 0 or 0.1–1
  (rotation-keys (make-key-axes) :type key-axes)        ; −1, 0, +1 about body x, y, z (roll, pitch, yaw)
  (translation-keys (make-key-axes) :type key-axes)     ; −1, 0, +1 along body x, y, z
  (fine nil :type boolean)
  (attitude-mode :hold :type (member :direct :rate-damping :hold))
  (hold :prograde :type (member :prograde :retrograde :normal :anti-normal
                                :radial-out :radial-in :inertial))
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
  "AXIS :roll, :pitch or :yaw; SIGN −1, 0 or +1 (held keys)."
  (declare (type (integer -1 1) sign))
  (setf (aref (controls-rotation-keys controls) (ecase axis (:roll 0) (:pitch 1) (:yaw 2)))
        sign)
  controls)

(defun controls-set-translation-key (controls axis sign)
  "AXIS :x, :y or :z (body); SIGN −1, 0 or +1 (held keys)."
  (declare (type (integer -1 1) sign))
  (setf (aref (controls-translation-keys controls) (ecase axis (:x 0) (:y 1) (:z 2)))
        sign)
  controls)

(defun controls-toggle-fine (controls)
  (setf (controls-fine controls) (not (controls-fine controls)))
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

;;; The flight computer (decision 70): commanded body torque and force.

(defparameter *fine-control* 0.1d0
  "Torque, force and rate factor in fine control, docs/design.md \"Controls\".")
(declaim (type double-float *fine-control*))

(declaim (inline flight-commands!))

(defun flight-commands! (tau-c f-c controls com)
  "The commanded torque (N m) and force (N), body axes, about the CoM COM.
Direct: a held rotation key commands its axis's couple; translation is
always direct, the axis's torque-free force; ×*fine-control* in fine."
  (declare (type vec3 tau-c f-c com) (type controls controls))
  (let ((fine (if (controls-fine controls) *fine-control* 1d0))
        (rotation (controls-rotation-keys controls))
        (translation (controls-translation-keys controls)))
    (dotimes (axis 3)
      (setf (aref tau-c axis)
            (ecase (controls-attitude-mode controls)
              (:direct (* fine (float (aref rotation axis) 1d0) (rcs-couple axis)))
              ;; PLACEHOLDER(102): rate damping and holds command no torque yet.
              ((:rate-damping :hold) 0d0))
            (aref f-c axis)
            (* fine (float (aref translation axis) 1d0)
               (rcs-translation-capability axis com))))
    tau-c))

;;; One step of the ship

;; Hot callers declare it inline: a full call boxes h and t.
(declaim (sb-ext:maybe-inline flight-step!))

(defun flight-step! (state controls h time model &optional attitude-fn)
  "Advance STATE by h from TIME under CONTROLS: mass properties from m_p,
flight computer, mixer (duties into CONTROLS), engine plus RCS force,
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
    (if (or attitude-fn (<= m-p 0d0))
        (fill u 0d0)
        (progn
          (rcs-matrix! b com)
          (flight-commands! tau-c f-c controls com)
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
