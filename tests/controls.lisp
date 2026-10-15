(in-package #:luv-in-space.tests)

;;; controls.lisp: the control state, the RCS mixer, direct mode,
;;; translation, flight-step!, rate damping and the holds (docs/design.md
;;; "Control modes"; decision 70: Flight computer: per-step control laws,
;;; holds and the RCS mixer).

(define-test controls-state-and-commands
  (let ((c (lis::new-controls)))
    (is eq :hold (lis::controls-attitude-mode c))
    (is eq :prograde (lis::controls-hold c))
    (is = 0d0 (lis::controls-throttle c))
    (is = 0d0 (lis::controls-throttle (lis::controls-set-throttle c 0.09d0)))
    (is = 0.1d0 (lis::controls-throttle (lis::controls-set-throttle c 0.1d0)))
    (is = 1d0 (lis::controls-throttle (lis::controls-set-throttle c 1.5d0)))
    (is = 0d0 (lis::controls-throttle (lis::controls-throttle-cut c)))
    (is = 1d0 (lis::controls-throttle (lis::controls-throttle-full c)))
    (lis::controls-set-rotation-key c :pitch -1)
    (lis::controls-set-rotation-key c :roll 1)
    (lis::controls-set-translation-key c :z -1)
    (is equalp #(1 -1 0) (lis::controls-rotation-keys c))
    (is equalp #(0 0 -1) (lis::controls-translation-keys c))
    (true (lis::controls-fine (lis::controls-toggle-fine c)))
    (false (lis::controls-fine (lis::controls-toggle-fine c)))
    (fail (lis::controls-set-rotation-key c :sideways 1))
    (let ((copy (lis::copy-controls c)))
      (true (equalp c copy))
      (false (eq (lis::controls-duties c) (lis::controls-duties copy)))
      (lis::controls-set-rotation-key copy :yaw 1)
      (setf (aref (lis::controls-inertial-target copy) 0) 0.5d0)
      (false (equalp c copy))
      (is = 0 (aref (lis::controls-rotation-keys c) 2))
      (is = 1d0 (aref (lis::controls-inertial-target c) 0)))))

;;; The mixer

(defun mix (m-p tau force &optional com)
  "Duties, force and torque of the mixer for TAU and FORCE (lists) at the
CoM of M-P kg of propellant, or COM."
  (let* ((com (or com (lis::mass-properties-com (mass-properties m-p))))
         (b (lis::rcs-matrix! (lis::make-rcs-matrix) com))
         (u (lis::rcs-mix! (lis::make-duty-cycles) b (apply #'lis::vec3 tau)
                           (apply #'lis::vec3 force)))
         (f (lis::make-vec3))
         (torque (lis::make-vec3)))
    (dotimes (i 24)
      (dotimes (k 3)
        (incf (aref f k) (* (aref u i) (aref b k i)))
        (incf (aref torque k) (* (aref u i) (aref b (+ k 3) i)))))
    (values u f torque)))

(defun test-torques ()
  "50 torques from a fixed list, Σ |τ_i| / couple_i ≤ 0.9: inside the
reachable set, which is convex and holds ± each couple. (A box of 90 % of
every couple at once is not: its corners need more nozzles than there are.)"
  (loop for k below 50
        for s = (loop for i below 3
                      collect (- (/ (mod (+ (* k 7919) (* i 104729) 13) 2001) 1000) 1))
        for l1 = (max 1/1000 (reduce #'+ (mapcar #'abs s)))
        for scale = (* 9/10 (/ (1+ (mod k 5)) 5) (/ l1))
        collect (loop for si in s for axis from 0
                      collect (float (* scale si (lis::rcs-couple axis)) 1d0))))

(define-test mixer-torques
  (dolist (m-p '(28000 14000 0))
    (dolist (tau (test-torques))
      (multiple-value-bind (u f torque) (mix m-p tau '(0 0 0))
        (declare (ignore f))
        (dotimes (axis 3)
          (true (< (abs (- (aref torque axis) (nth axis tau)))
                   (* 1d-6 (lis::rcs-couple axis)))
                "~D kg, ~A: ~A" m-p tau torque))
        (true (every (lambda (x) (<= 0d0 x 1d0)) u)))))
  (is equalp (lis::make-duty-cycles) (mix 28000 '(0 0 0) '(0 0 0))))

(defun axial-aft-p (i) (= 1d0 (aref lis::*rcs-directions* i 0)))

(define-test mixer-pure-x
  ;; On the axis only the four aft axial nozzles fire, at 1,780 N.
  (multiple-value-bind (u f torque) (mix 0 '(0 0 0) '(1780 0 0) (lis::vec3 10.34d0 0 0))
    (dotimes (i 24)
      (if (axial-aft-p i)
          (true (< (abs (- (aref u i) 1)) 1d-12))
          (true (< (aref u i) 1d-12))))
    (true (< (abs (- (aref f 0) 1780)) 1d-9))
    (true (< (lis::vec3-norm torque) 1d-6)))
  ;; At the wet CoM, 0.22 m below the axis, the aft nozzles' thrust pitches
  ;; by 4 × 445 × 0.22 N m; a pitch couple of z nozzles trims it, so a few
  ;; more fire, still 1,780 N without torque (plan 14 said only the four).
  (multiple-value-bind (u f torque) (mix 28000 '(0 0 0) '(1780 0 0))
    (dotimes (i 24)
      (when (axial-aft-p i)
        (true (< (abs (- (aref u i) 1)) 1d-12)))
      (when (/= 0d0 (aref lis::*rcs-directions* i 1))
        (true (< (aref u i) 1d-9))))
    (true (< (abs (- (aref f 0) 1780)) 1d-6))
    (true (< (lis::vec3-norm torque) 1d-6))))

(define-test mixer-lateral
  (let* ((com (lis::mass-properties-com (mass-properties 28000)))
         (a-f (- 14.5d0 (aref com 0)))
         (a-r (- (aref com 0) 0.5d0))
         (nominal (lis::rcs-translation-capability 1 com)))
    ;; The key's command, 1,266 N, is met exactly.
    (multiple-value-bind (u f torque) (mix 28000 '(0 0 0) (list 0 nominal 0))
      (declare (ignore u))
      (true (< (abs (- (aref f 1) nominal)) 1d-6))
      (true (< (lis::vec3-norm torque) 1d-6)))
    ;; 10 kN: as much as fits without torque. Beyond the nominal 1,266 N:
    ;; the two front lateral nozzles at full, the aft ones balancing them
    ;; and the axial nozzles' yaw couples (4 × 445 × 1.8 N m), so
    ;; 890 + (890 a_f + 3,204) / a_r = 1,592 N, not plan 14's 1,266 N.
    (multiple-value-bind (u f torque) (mix 28000 '(0 0 0) '(0 10000 0))
      (true (every (lambda (x) (<= 0d0 x 1d0)) u))
      (true (< (lis::vec3-norm torque) 1d-6) "~A" torque)
      (true (< (relative-error (aref f 1) (+ 890 (/ (+ (* 890 a-f) 3204) a-r))) 1d-9)
            "~A" f)
      (true (< (abs (aref f 0)) 1d-6))
      (true (< (abs (aref f 2)) 1d-6)))))

(define-test mixer-lateral-loaded
  ;; Task 256: with 50 t and 100 t at stack-design-pose the CoM lies ahead of
  ;; the front blocks; the capability is met without torque and is all that
  ;; fits (a 10 kN command gets the same).
  (dolist (key '(:skua-yard-3 :skua-yard-5))
    (let* ((com (lis::mass-properties-com
                 (lis::ship-mass-properties
                  28000d0 (lis::stack-design-pose (lis::object-definition key)))))
           (nominal (lis::rcs-translation-capability 1 com)))
      (dolist (force (list (list 0 nominal 0) (list 0 0 (- nominal))))
        (multiple-value-bind (u f torque) (mix 28000 '(0 0 0) force com)
          (true (every (lambda (x) (<= 0d0 x 1d0)) u))
          (true (< (lis::vec3-norm (lis::vec3-sub f (apply #'lis::vec3 force))) 1d-6)
                "~A ~A" key f)
          (true (< (lis::vec3-norm torque) 1d-6) "~A ~A" key torque)))
      (multiple-value-bind (u f torque) (mix 28000 '(0 0 0) '(0 10000 0) com)
        (declare (ignore u))
        (true (< (relative-error (aref f 1) nominal) 1d-9) "~A ~A" key f)
        (true (< (lis::vec3-norm torque) 1d-6))))))

;;; Direct mode, translation and propellant through flight-step!

(defun rest-state (m-p)
  (lis::make-ship-state (lis::vec3 0 0 0) (lis::vec3 0 0 0) (lis::quat 1 0 0 0)
                        (lis::vec3 0 0 0) m-p))

(defun direct-controls (&key rotation translation fine)
  "Direct mode with keys held: ((axis sign) …)."
  (let ((c (lis::new-controls)))
    (setf (lis::controls-attitude-mode c) :direct
          (lis::controls-fine c) fine)
    (loop for (axis sign) in rotation do (lis::controls-set-rotation-key c axis sign))
    (loop for (axis sign) in translation do (lis::controls-set-translation-key c axis sign))
    c))

(defun applied (b u)
  "Force and torque of duties U on B."
  (let ((f (lis::make-vec3)) (torque (lis::make-vec3)))
    (dotimes (i 24 (values f torque))
      (dotimes (k 3)
        (incf (aref f k) (* (aref u i) (aref b k i)))
        (incf (aref torque k) (* (aref u i) (aref b (+ k 3) i)))))))

(define-test direct-taps
  ;; docs/design.md "Control modes": the shortest direct tap, 0.027°/s wet
  ;; and 0.060°/s dry per 1/60 s frame; "dry" with 1 kg left, as empty
  ;; tanks fire nothing.
  (loop for (m-p per-frame) in '((28000d0 0.027d0) (1d0 0.060d0))
        for state = (rest-state m-p)
        for c = (direct-controls :rotation '((:pitch 1)))
        for p = (mass-properties m-p)
        for b = (rcs-matrix m-p)
        do (lis::flight-step! state c lis::+fine-step+ 0d0 :none)
           (multiple-value-bind (f torque) (applied b (lis::controls-duties c))
             (declare (ignore f))
             (true (< (abs (- (aref torque 1) 12460)) (* 1d-6 12460)))
             (let ((expected (lis::vec3-scale (lis::mat3*vec3 (lis::mass-properties-inertia-inverse p)
                                                              torque)
                                              lis::+fine-step+))
                   (dw (lis::ship-state-w state)))
               (true (< (/ (lis::vec3-norm (lis::vec3-sub dw expected)) (lis::vec3-norm expected))
                        1d-9))
                                ;; 5 %: the design's figures use its I_yy table, which
                 ;; ship.lisp meets within 5 % (tests/ship.lisp).
                 (true (< (relative-error (degrees (* (aref dw 1) 64/60)) per-frame) 0.05d0))))))

(define-test translation-tap
  ;; One fine step of +x wet: 1,780 N / 35,961 kg / 64 = 0.77 mm/s.
  (let ((state (rest-state 28000d0))
        (c (direct-controls :translation '((:x 1)))))
    (lis::flight-step! state c lis::+fine-step+ 0d0 :none)
    (let ((dv (aref (lis::ship-state-v state) 0)))
      (true (< (relative-error dv 0.77d-3) 0.01d0))
      (true (< (relative-error dv (/ 1780 35961d0 64)) 1d-6)))))

(define-test translation-loaded
  ;; Task 256: J held 1 s in direct mode with 100 t latched: the
  ;; capability's acceleration and no rotation beyond the mixer's residual.
  (let* ((state (rest-state 28000d0))
         (c (direct-controls :translation '((:y 1))))
         (load (lis::stack-design-pose (lis::object-definition :skua-yard-5)))
         (p (lis::ship-mass-properties 28000d0 load))
         (expected (/ (lis::rcs-translation-capability 1 (lis::mass-properties-com p))
                      (lis::mass-properties-mass p))))
    (dotimes (i 64)
      (lis::flight-step! state c lis::+fine-step+ (* i lis::+fine-step+) :none nil nil load))
    (let ((v (lis::ship-state-v state)))
      (true (< (relative-error (aref v 1) expected) 1d-4) "~A ~A" v expected)
      (true (< (abs (aref v 0)) (* 1d-9 expected)))
      (true (< (abs (aref v 2)) (* 1d-9 expected))))
    (true (< (lis::vec3-norm (lis::ship-state-w state)) 1d-12) "~A" (lis::ship-state-w state))))

(define-test rcs-propellant
  ;; One second of every key held, with and without the engine: the tanks
  ;; lose Σ u_i 445 / (360 g₀) h per step, plus the engine's ṁ h; one pool.
  ;; Within 1e-9 kg, not plan 14's 1e-12 relative: m_p ≈ 28,000 kg rounds
  ;; to 3.6e-12 kg a step, about 1e-12 of the 1.46 kg burned over 64.
  (dolist (throttle '(0d0 1d0))
    (let ((state (rest-state 28000d0))
          (c (direct-controls :rotation '((:roll 1) (:pitch -1) (:yaw 1))
                              :translation '((:x 1) (:y -1) (:z 1))))
          (expected 0d0))
      (lis::controls-set-throttle c throttle)
      (dotimes (i 64)
        (lis::flight-step! state c lis::+fine-step+ (* i lis::+fine-step+) :none)
        (incf expected (* lis::+fine-step+
                          (+ (lis::ship-engine-mdot throttle)
                             (/ (* 445 (reduce #'+ (lis::controls-duties c)))
                                (* 360 lis::+g0+))))))
      (true (> (reduce #'+ (lis::controls-duties c)) 4))
      (true (< (abs (- (- 28000 (lis::ship-state-m-p state)) expected)) 1d-9)))))

(define-test kinematic-flight-step
  ;; With an attitude function: engine only, no duties, ω from the function.
  (let ((state (rest-state 28000d0))
        (c (direct-controls :rotation '((:pitch 1)) :translation '((:y 1)))))
    (lis::controls-throttle-full c)
    (lis::flight-step! state c 1d0 0d0 :none
                       (lambda (q w time-cell r v)
                         (declare (ignore time-cell r v))
                         (lis::quat-set! q 1d0 0d0 0d0 0d0)
                         (lis::vec3-set! w 0d0 0d0 0d0)))
    (is equalp (lis::make-duty-cycles) (lis::controls-duties c))
    (is equalp (lis::vec3 0 0 0) (lis::ship-state-w state))
    (let ((v (lis::ship-state-v state)))
      (is = 0d0 (aref v 1))
      (true (< (relative-error (aref v 0)
                               (* lis::*engine-isp* lis::+g0+
                                  (log (/ 35961 (- 35961 (lis::ship-engine-mdot 1d0))))))
               1d-9)))))

(defun start-state ()
  "The start of decision 38: Start orbit: polar: 400 km, i 90°, RAAN 270°,
at the ascending node; the prograde hold's attitude (−z radial out) and
frame rate, full tanks."
  (let* ((a (+ lis::+radius-earth+ 400d3))
         (n (sqrt (/ lis::+mu-earth+ (expt a 3))))
         (r (lis::vec3 0 (- a) 0))
         (v (lis::vec3 0 0 (* n a))))
    (lis::make-ship-state r v
                          (lis::quat-from-basis (lis::vec3 0 0 1) (lis::vec3 1 0 0)
                                                (lis::vec3 0 1 0))
                          (lis::vec3 0 (- n) 0) 28000d0)))

(defun flight-steps (state controls n &optional (time (lis::start-t)))
  "N fine steps from TIME under :full gravity; STATE."
  (declare (type fixnum n) (type double-float time) (inline lis::flight-step!))
  (dotimes (i n state)
    (lis::flight-step! state controls lis::+fine-step+ time :full)
    (setf time (+ time lis::+fine-step+))))

(define-test direct-pitch-second
  ;; Task 101's Done when: 64 fine steps of held pitch from the start
  ;; change ω_y by 12,460 N m / I_yy × 1 s (0.0276 rad/s wet), I_yy of each
  ;; step's propellant; with the start's I_yy throughout the 0.5 kg burned
  ;; makes it 5e-6.
  (let* ((state (start-state))
         (w0 (aref (lis::ship-state-w state) 1))
         (controls (direct-controls :rotation '((:pitch 1))))
         (expected 0d0))
    (dotimes (i 64)
      (incf expected (/ (* 12460 lis::+fine-step+)
                        (tensor (mass-properties (lis::ship-state-m-p state)) 1 1)))
      (flight-steps state controls 1 (+ (lis::start-t) (* i lis::+fine-step+))))
    (true (< (relative-error (- (aref (lis::ship-state-w state) 1) w0) expected) 1d-6))
    (true (< (relative-error expected (/ 12460 (tensor (mass-properties 28000) 1 1))) 1d-5))))

(define-test flight-step-cons-nothing-and-deterministic
  (flet ((controls ()
           (lis::controls-set-throttle
            (direct-controls :rotation '((:pitch 1) (:roll -1)) :translation '((:y 1)))
            0.5d0)))
    (let ((a (start-state)) (ca (controls))
          (b (start-state)) (cb (controls)))
      (dotimes (i 3) (flight-steps a ca 3333))
      (let ((consed (least-bytes-consed (lambda () (flight-steps b cb 3333)))))
        (true (< consed 1024) "~D bytes" consed))
      (is equalp a b)
      (is equalp ca cb))))

;;; Rate damping, holds and the key rules (docs/design.md "Control modes";
;;; decision 70). "Dry" is 5 kg left: a 90° slew dry burns ~1.9 kg.

(defun rate-damping-run (m-p fine)
  "Pitch held from rest for 10 s, then released for 10 s, in field-free
space: (values time to 95 % of the rate, peak ω_y in °/s, time to |ω| <
0.02°/s after release)."
  (let ((state (rest-state m-p))
        (c (lis::controls-toggle-rate-damping (lis::new-controls)))
        (rate (if fine 0.5d0 5d0))
        t95 (peak 0d0) t-stop)
    (when fine (lis::controls-toggle-fine c))
    (lis::controls-set-rotation-key c :pitch 1)
    (dotimes (i 640)
      (lis::flight-step! state c lis::+fine-step+ 0d0 :none)
      (let ((w (degrees (aref (lis::ship-state-w state) 1))))
        (setf peak (max peak w))
        (when (and (null t95) (>= w (* 0.95d0 rate))) (setf t95 (/ (1+ i) 64d0)))))
    (lis::controls-set-rotation-key c :pitch 0)
    (dotimes (i 640)
      (lis::flight-step! state c lis::+fine-step+ 0d0 :none)
      (when (and (null t-stop) (< (degrees (lis::vec3-norm (lis::ship-state-w state))) 0.02d0))
        (setf t-stop (/ (1+ i) 64d0))))
    (values t95 peak t-stop)))

(define-test rate-damping
  ;; Measured: 95 % at 3.23 s wet, 1.89 s dry; stopped 4.48 s after release.
  (multiple-value-bind (t95 peak t-stop) (rate-damping-run 28000d0 nil)
    (true (and t95 (<= t95 3.5d0)) "~A" t95)
    (true (<= peak (* 1.01d0 5)))
    (true (and t-stop (<= t-stop 6d0)) "~A" t-stop))
  (multiple-value-bind (t95 peak) (rate-damping-run 5d0 nil)
    (true (and t95 (<= t95 2d0)) "~A" t95)
    (true (<= peak (* 1.01d0 5))))
  (multiple-value-bind (t95 peak) (rate-damping-run 28000d0 t)
    (declare (ignore t95))
    (true (<= (* 0.99d0 0.5d0) peak (* 1.01d0 0.5d0)) "~A" peak)))

(defun orbital-frame (r v)
  "Prograde, normal and radial out (docs/architecture.md \"Frames\")."
  (let* ((p (lis::vec3-normalize v))
         (n (lis::vec3-normalize (lis::vec3-cross r v))))
    (values p n (lis::vec3-cross p n))))

(defun hold-axes (kind r v)
  "The nose's and the cab's (−z) directions of KIND's default attitude."
  (multiple-value-bind (p n o) (orbital-frame r v)
    (ecase kind
      (:prograde (values p o))
      (:retrograde (values (lis::vec3-scale p -1d0) o))
      (:normal (values n o))
      (:anti-normal (values (lis::vec3-scale n -1d0) o))
      (:radial-out (values o n))
      (:radial-in (values (lis::vec3-scale o -1d0) n)))))

(defparameter *orbital-holds* '(:prograde :retrograde :normal :anti-normal :radial-out :radial-in))

(defun angle-between-deg (a b)
  (degrees (atan (lis::vec3-norm (lis::vec3-cross a b)) (lis::vec3-dot a b))))

(define-test hold-attitude
  ;; The default rolls, and the frame's rate n along r × v in the circular
  ;; start orbit; the roll offset turns about the nose.
  (let* ((s (start-state)) (r (lis::ship-state-r s)) (v (lis::ship-state-v s))
         (n (/ (lis::vec3-norm v) (lis::vec3-norm r))))
    (dolist (kind *orbital-holds*)
      (multiple-value-bind (q w-t) (lis::hold-attitude kind r v 0d0)
        (multiple-value-bind (nose cab) (hold-axes kind r v)
          (true (< (angle-between-deg (lis::quat-rotate q (lis::vec3 1 0 0)) nose) 1d-12))
          (true (< (angle-between-deg (lis::quat-rotate q (lis::vec3 0 0 -1)) cab) 1d-12) "~A" kind))
        (multiple-value-bind (p normal) (orbital-frame r v)
          (declare (ignore p))
          (true (< (lis::vec3-norm (lis::vec3-sub w-t (lis::vec3-scale normal n))) (* 1d-12 n))))))
    (is equalp (lis::ship-state-q s) (lis::hold-attitude :prograde r v 0d0))
    (let ((q (lis::hold-attitude :prograde r v (lis::deg->rad 30d0)))
          (q0 (lis::hold-attitude :prograde r v 0d0)))
      (true (< (angle-between-deg (lis::quat-rotate q (lis::vec3 1 0 0))
                                  (lis::quat-rotate q0 (lis::vec3 1 0 0)))
               1d-12))
      (true (< (abs (- (angle-between-deg (lis::quat-rotate q (lis::vec3 0 1 0))
                                          (lis::quat-rotate q0 (lis::vec3 0 1 0)))
                       30))
               1d-9)))
    (let ((target (lis::quat-from-axis-angle (lis::vec3 1 2 3) 1d0)))
      (multiple-value-bind (q w-t) (lis::hold-attitude :inertial r v 0d0 target)
        (is equalp target q)
        (is equalp (lis::vec3 0 0 0) w-t)))))

(defun off-state (kind m-p)
  "The start orbit, at rest, the attitude KIND's target yawed 90°."
  (let* ((s (start-state))
         (q (lis::hold-attitude kind (lis::ship-state-r s) (lis::ship-state-v s) 0d0)))
    (replace (lis::ship-state-q s)
             (lis::quat-mul q (lis::quat-from-axis-angle (lis::vec3 0 0 1) (/ pi 2))))
    (lis::vec3-set! (lis::ship-state-w s) 0d0 0d0 0d0)
    (setf (lis::ship-state-m-p s) m-p)
    s))

(defun hold-error (state controls)
  "The angle (°) and the rate error (°/s) from the hold's target."
  (multiple-value-bind (q-t w-t)
      (lis::hold-attitude (lis::controls-hold controls) (lis::ship-state-r state)
                          (lis::ship-state-v state) (lis::controls-roll-offset controls)
                          (lis::controls-inertial-target controls))
    (let* ((q* (lis::quat-conjugate (lis::ship-state-q state)))
           (e (lis::quat-mul q* q-t)))
      (values (degrees (* 2 (atan (lis::vec3-norm (lis::vec3 (aref e 1) (aref e 2) (aref e 3)))
                                  (abs (aref e 0)))))
              (degrees (lis::vec3-norm (lis::vec3-sub (lis::ship-state-w state)
                                                        (lis::quat-rotate q* w-t))))))))

(defun hold-run (state controls seconds)
  "SECONDS of fine steps from the start date under :full gravity: (values
the time settled is first reported, the largest angle and rate error from
then on, the final time)."
  (let ((time (lis::start-t)) settled (angle 0d0) (rate 0d0))
    (dotimes (i (* 64 seconds))
      (lis::flight-step! state controls lis::+fine-step+ time :full)
      (setf time (+ time lis::+fine-step+))
      (when (and (null settled) (lis::controls-settled controls))
        (setf settled (/ (1+ i) 64d0)))
      (when settled
        (multiple-value-bind (a w) (hold-error state controls)
          (setf angle (max angle a) rate (max rate w)))))
    (values settled angle rate time)))

(defun hold-controls (kind)
  (let ((c (lis::new-controls)))
    (unless (eq kind (lis::controls-hold c))
      (lis::controls-set-hold c kind))
    c))

(define-test prograde-hold-settles
  ;; Measured: settled 24.9 s wet, 24.6 s dry (5 kg).
  (let ((state (off-state :prograde 28000d0))
        (c (hold-controls :prograde)))
    (multiple-value-bind (settled angle rate) (hold-run state c 160)
      (true (and settled (<= settled 40)) "~A" settled)
      (true (< angle 0.1d0) "~A" angle)
      (true (< rate 0.02d0) "~A" rate)
      (true (lis::controls-settled c))))
  (let ((settled (hold-run (off-state :prograde 5d0) (hold-controls :prograde) 40)))
    (true (and settled (<= settled 35)) "~A" settled)))

(define-test holds-settle
  ;; Measured: 25.0 s for each; R after the tumble 6.6 s.
  (dolist (kind (rest *orbital-holds*))
    (let ((state (off-state kind 28000d0))
          (c (hold-controls kind)))
      (multiple-value-bind (settled angle) (hold-run state c 40)
        (true (and settled (<= settled 40)) "~A ~A" kind settled)
        (true (< angle 0.1d0))
        ;; The default roll on the ship itself.
        (multiple-value-bind (nose cab) (hold-axes kind (lis::ship-state-r state)
                                                   (lis::ship-state-v state))
          (let ((q (lis::ship-state-q state)))
            (true (< (angle-between-deg (lis::quat-rotate q (lis::vec3 1 0 0)) nose) 0.1d0))
            (true (< (angle-between-deg (lis::quat-rotate q (lis::vec3 0 0 -1)) cab) 0.1d0)
                  "~A" kind))))))
  ;; R after a tumble at 2°/s about an off-principal axis.
  (let ((state (start-state))
        (c (lis::new-controls)))
    (replace (lis::ship-state-w state)
             (lis::vec3-scale (lis::vec3-normalize (lis::vec3 1.2d0 -1.2d0 1d0))
                              (lis::deg->rad 2d0)))
    (let ((q0 (lis::quat-copy (lis::ship-state-q state))))
      (lis::controls-inertial-hold c q0)
      (multiple-value-bind (settled angle) (hold-run state c 40)
        (true (and settled (<= settled 40)) "R ~A" settled)
        (true (< angle 0.1d0))
        (is equalp q0 (lis::controls-inertial-target c))))))

(define-test roll-key-during-hold
  ;; Roll held 2 s turns the roll offset 10°; the hold follows and keeps it.
  (let ((state (start-state)) (c (lis::new-controls)) (time (lis::start-t)))
    (flet ((run (seconds)
             (dotimes (i (* 64 seconds))
               (lis::flight-step! state c lis::+fine-step+ time :full)
               (setf time (+ time lis::+fine-step+)))))
      (run 1)
      (lis::controls-set-rotation-key c :roll 1)
      (run 2)
      (lis::controls-set-rotation-key c :roll 0)
      (true (< (abs (- (degrees (lis::controls-roll-offset c)) 10)) 1d-9))
      (run 20)
      (is eq :hold (lis::controls-attitude-mode c))
      (true (lis::controls-settled c))
      (let* ((q0 (lis::hold-attitude :prograde (lis::ship-state-r state) (lis::ship-state-v state) 0d0))
             (y0 (lis::quat-rotate q0 (lis::vec3 0 1 0)))
             (y (lis::quat-rotate (lis::ship-state-q state) (lis::vec3 0 1 0)))
             (roll (degrees (atan (lis::vec3-dot (lis::vec3-cross y0 y)
                                                 (lis::quat-rotate q0 (lis::vec3 1 0 0)))
                                  (lis::vec3-dot y0 y)))))
        (true (< (abs (- roll 10)) 0.5d0) "~A" roll)))))

(define-test key-rules
  (let ((c (lis::new-controls)) (q (lis::quat-from-axis-angle (lis::vec3 0 1 0) 0.3d0)))
    (flet ((mode () (lis::controls-attitude-mode c)))
      ;; 1 twice: rate damping.
      (lis::controls-set-hold c :prograde)
      (is eq :rate-damping (mode))
      (false (lis::controls-attitude-fn c))
      (lis::controls-set-hold c :prograde)
      (is eq :hold (mode))
      (true (functionp (lis::controls-attitude-fn c)))
      ;; Another hold switches; the roll offset returns to the default.
      (setf (lis::controls-roll-offset c) 0.2d0)
      (lis::controls-set-hold c :normal)
      (is eq :normal (lis::controls-hold c))
      (is = 0d0 (lis::controls-roll-offset c))
      ;; A roll key keeps the hold; W (pitch) releases it to rate damping.
      (lis::controls-set-rotation-key c :roll 1)
      (is eq :hold (mode))
      (lis::controls-set-rotation-key c :roll 0)
      (lis::controls-set-rotation-key c :pitch -1)
      (is eq :rate-damping (mode))
      (lis::controls-set-rotation-key c :pitch 0)
      ;; A yaw key too.
      (lis::controls-set-hold c :radial-in)
      (lis::controls-set-rotation-key c :yaw 1)
      (is eq :rate-damping (mode))
      (lis::controls-set-rotation-key c :yaw 0)
      ;; T: rate damping ↔ direct; a hold to rate damping.
      (lis::controls-toggle-rate-damping c)
      (is eq :direct (mode))
      (lis::controls-toggle-rate-damping c)
      (is eq :rate-damping (mode))
      (lis::controls-set-hold c :retrograde)
      (lis::controls-toggle-rate-damping c)
      (is eq :rate-damping (mode))
      ;; R: the current attitude; R again releases.
      (lis::controls-inertial-hold c q)
      (is eq :hold (mode))
      (is eq :inertial (lis::controls-hold c))
      (is equalp q (lis::controls-inertial-target c))
      (false (eq q (lis::controls-inertial-target c)))
      (lis::controls-inertial-hold c q)
      (is eq :rate-damping (mode))
      ;; From direct, a hold engages.
      (lis::controls-toggle-rate-damping c)
      (lis::controls-set-hold c :anti-normal)
      (is eq :hold (mode)))))

(define-test kinematic-burn
  ;; 60 s full throttle prograde: the coarse step (1 s under thrust) with
  ;; the hold's kinematic attitude against the fine step flown by the hold.
  ;; Measured: Δv 6e-9 relative, apoapsis 3 mm.
  (flet ((apoapsis (s)
           (lis::orbit-apoapsis-radius
            (lis::state->orbit (lis::ship-state-r s) (lis::ship-state-v s) lis::+mu-earth+ 0d0))))
    (let* ((fine (start-state)) (fine-c (lis::controls-throttle-full (lis::new-controls)))
           (coarse (start-state)) (coarse-c (lis::controls-throttle-full (lis::new-controls)))
           (attitude-fn (lis::controls-attitude-fn coarse-c))
           (end (+ (lis::start-t) 60)))
      (flight-steps fine fine-c (* 64 60))
      (loop with time = (lis::start-t)
            while (< time end)
            do (let ((h (lis::coarse-step (lis::ship-state-r coarse) time t)))
                 (is = 1d0 h)
                 (lis::flight-step! coarse coarse-c h time :full attitude-fn)
                 (incf time h)))
      (true (lis::controls-settled coarse-c))
      (true (< (relative-error (lis::ship-state-dv coarse) (lis::ship-state-dv fine)) 1d-6))
      (true (< (abs (- (apoapsis coarse) (apoapsis fine))) 1d0))
      (true (> (- (apoapsis fine) (lis::vec3-norm (lis::ship-state-r fine))) 1d5)))))

(defparameter *key-script*
  ;; (step command args…), steps of 1/64 s over 120 s.
  '((0 :key :pitch 1) (200 :key :pitch 0) (300 :hold :normal) (1500 :key :roll -1)
    (1600 :key :roll 0) (1700 :translate :y 1) (1800 :translate :y 0) (2000 :fine)
    (2100 :key :yaw 1) (2300 :key :yaw 0) (2400 :fine) (2500 :inertial)
    (3500 :toggle) (3600 :key :roll 1) (3700 :key :roll 0) (3800 :toggle)
    (4000 :hold :radial-out) (5000 :throttle 0.3d0) (5600 :throttle 0d0)
    (6000 :hold :radial-out)))

(defun scripted-run ()
  (let ((state (start-state)) (c (lis::new-controls)) (time (lis::start-t))
        (script *key-script*))
    (dotimes (i (* 64 120))
      (loop while (and script (= i (first (first script))))
            do (destructuring-bind (command &rest args) (rest (pop script))
                 (ecase command
                   (:key (apply #'lis::controls-set-rotation-key c args))
                   (:translate (apply #'lis::controls-set-translation-key c args))
                   (:hold (lis::controls-set-hold c (first args)))
                   (:inertial (lis::controls-inertial-hold c (lis::ship-state-q state)))
                   (:toggle (lis::controls-toggle-rate-damping c))
                   (:fine (lis::controls-toggle-fine c))
                   (:throttle (lis::controls-set-throttle c (first args))))))
      (lis::flight-step! state c lis::+fine-step+ time :full)
      (setf time (+ time lis::+fine-step+)))
    (values state c)))

(define-test scripted-keys-deterministic
  (multiple-value-bind (a ca) (scripted-run)
    (multiple-value-bind (b cb) (scripted-run)
      (is equalp a b)
      (is equalp ca cb)
      (false (eq (lis::controls-attitude-mode ca) :hold)))))

(define-test hold-step-cons-nothing
  (let ((state (off-state :prograde 28000d0)) (c (hold-controls :prograde)))
    (flight-steps state c 640)
    (let ((consed (least-bytes-consed (lambda () (flight-steps state c 2134)))))
      (true (< consed 1024) "~D bytes" consed))))

(define-test target-relative-values
  ;; decision 114: 100 m ahead along v, closing 0.5 m/s; the nose yawed 90°
  ;; (body x → inertial y), the cab (−z) along inertial −z.
  (let* ((r (lis::vec3 7000d3 0 0))
         (v (lis::vec3 0 7500d0 0))
         (q (lis::quat (sqrt 0.5d0) 0 0 (sqrt 0.5d0)))
         (r-t (lis::vec3 7000d3 100d0 0))
         (v-t (lis::vec3 0 7499.5d0 0))
         (out (lis::make-target-relative)))
    (lis::target-relative-into! out r v q r-t v-t)
    (loop for expected in '(100d0 0.5d0  0.5d0 0d0 0d0  1d0 0d0 0d0)
          for i from 0
          do (true (< (abs (- (aref out i) expected)) 1d-12) "~D: ~A" i (aref out i)))
    ;; Opening, the target behind and below: the signs.
    (lis::target-relative-into! out r v q (lis::vec3 7000d3 -100d0 100d0) v-t)
    (true (< (aref out 1) 0d0))
    (true (< (abs (- (aref out 5) (- (sqrt 0.5d0)))) 1d-12))
    (true (< (abs (- (aref out 7) (sqrt 0.5d0))) 1d-12))
    (is = 0 (least-bytes-consed (lambda () (lis::target-relative-into! out r v q r-t v-t))))))
