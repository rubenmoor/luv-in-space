(in-package #:luv-in-space.tests)

;;; controls.lisp: the control state, the RCS mixer, direct mode,
;;; translation and flight-step! (docs/design.md "Control modes"; decision
;;; 70: Flight computer: per-step control laws, holds and the RCS mixer).

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
      (flight-steps a ca 10000)
      (let ((consed (let ((before (sb-ext:get-bytes-consed)))
                      (flight-steps b cb 10000)
                      (- (sb-ext:get-bytes-consed) before))))
        (true (< consed 1024) "~D bytes" consed))
      (is equalp a b)
      (is equalp ca cb))))
