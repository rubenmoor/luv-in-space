(in-package #:luv-in-space.tests)

;;; ship.lisp: the Mule's parts, mass properties, engine and RCS layout
;;; (docs/design.md "The ship(s)"; decision 71: The Mule's mass model in
;;; ship.lisp).

(defun mass-properties (m-p)
  (lis::ship-mass-properties (float m-p 1d0)))

(defun tensor (properties i j)
  (aref (lis::mass-properties-inertia properties) (+ (* 3 i) j)))

(defun degrees (radians) (* radians (/ 180 pi)))

(define-test ship-masses
  (is = 7961d0 lis::*ship-dry-mass*)
  (is = 7961d0 (lis::mass-properties-mass (mass-properties 0)))
  (is = 35961d0 (lis::mass-properties-mass (mass-properties 28000)))
  (is = 7961 (reduce #'+ lis::*ship-parts* :key #'second)))

(define-test ship-inertia-table
  ;; docs/design.md "Inertia": (m-p com I_xx I_yy I_zz I_xz tilt°), an
  ;; independent script's figures (decision 71).
  (loop for (m-p com ixx iyy izz ixz tilt)
          in '((28000 (10.34d0 0 -0.22d0) 8.79d4 4.48d5 4.21d5 2.0d4 3.4d0)
               (14000 (9.46d0 0 -0.36d0) 7.06d4 3.35d5 3.09d5 2.7d4 6.3d0)
               (0 (9.46d0 0 -0.99d0) 4.95d4 1.99d5 1.78d5 2.7d4 11.3d0))
        for p = (mass-properties m-p)
        do (dotimes (i 3)
             (true (< (abs (- (aref (lis::mass-properties-com p) i) (nth i com))) 0.01d0)))
           (true (< (relative-error (tensor p 0 0) ixx) 0.05d0))
           (true (< (relative-error (tensor p 1 1) iyy) 0.05d0))
           (true (< (relative-error (tensor p 2 2) izz) 0.05d0))
           (true (< (relative-error (tensor p 0 2) ixz) 0.10d0))
           (true (< (abs (- (degrees (* 0.5d0 (atan (* 2 (tensor p 0 2))
                                                    (- (tensor p 2 2) (tensor p 0 0)))))
                            tilt))
                    1d0))))

(define-test ship-tensor-well-formed
  (dolist (m-p '(28000 14000 5000 0))
    (let* ((p (mass-properties m-p))
           (i (lis::mass-properties-inertia p))
           (product (lis::mat3* i (lis::mass-properties-inertia-inverse p))))
      (dotimes (a 3)
        (dotimes (b 3)
          (is = (tensor p a b) (tensor p b a))
          (true (< (abs (- (aref product (+ (* 3 a) b)) (if (= a b) 1 0))) 1d-12))))
      ;; Positive definite: Sylvester's leading minors, the last det I.
      (true (> (aref i 0) 0))
      (true (> (- (* (aref i 0) (aref i 4)) (* (aref i 1) (aref i 3))) 0))
      (true (> (+ (* (aref i 0) (- (* (aref i 4) (aref i 8)) (* (aref i 5) (aref i 7))))
                  (* (aref i 1) (- (* (aref i 5) (aref i 6)) (* (aref i 3) (aref i 8))))
                  (* (aref i 2) (- (* (aref i 3) (aref i 7)) (* (aref i 4) (aref i 6)))))
               0)))))

(defun parts-properties (parts)
  (multiple-value-bind (m first second) (lis::parts-mass-sums parts)
    (lis::mass-properties-from-sums! (lis::make-mass-properties) m first second)))

(define-test ship-tensor-closed-forms
  ;; One box: m (b² + c²)/12 and its turns, about its own centre.
  (let ((p (parts-properties '(("box" 12 ((3d0 -1d0 2d0)) (1d0 2d0 3d0) :box)))))
    (is equalp (lis::vec3 3 -1 2) (lis::mass-properties-com p))
    (is equalp (lis::mat3 13 0 0 0 10 0 0 0 5) (lis::mass-properties-inertia p)))
  ;; Two 1 m cubes (point masses with their own m/6 𝟙): the parallel-axis
  ;; tensor about the CoM (2, 0, 2) exactly.
  (let* ((p (parts-properties '(("a" 6 ((4d0 2d0 0d0)) (1d0 1d0 1d0) :box)
                                ("b" 12 ((1d0 -1d0 3d0)) (1d0 1d0 1d0) :box))))
         (expected (lis::make-mat3)))
    (loop for (m d) in '((6 (2 2 -2)) (12 (-1 -1 1)))
          for d2 = (reduce #'+ (mapcar #'* d d))
          do (dotimes (i 3)
               (dotimes (j 3)
                 (incf (aref expected (+ (* 3 i) j))
                       (+ (if (= i j) (/ m 6) 0)
                          (* m (- (if (= i j) d2 0) (* (nth i d) (nth j d)))))))))
    (is equalp (lis::vec3 2 0 2) (lis::mass-properties-com p))
    (is equalp expected (lis::mass-properties-inertia p))))

(define-test ship-trim-angle
  ;; docs/design.md "Propulsion": wet 1.6°, half 0.9°, dry −3.0°.
  (loop for (m-p angle) in '((28000 1.6d0) (14000 0.9d0) (0 -3.0d0))
        for trim = (degrees (lis::ship-trim-angle m-p))
        do (true (< (abs (- trim angle)) 0.1d0))
           (true (< (abs trim) 4))))

(define-test ship-engine
  (is = 0d0 (lis::ship-engine-force 0.05d0))
  (is = 10180d0 (lis::ship-engine-force 0.1d0))
  (is = 101800d0 (lis::ship-engine-force 1d0))
  (is = 101800d0 (lis::ship-engine-force 1.5d0))
  ;; ṁ = F / (Isp g₀) = 23.08 kg/s; 28 t burn in 1,213 s.
  (true (< (abs (- (lis::ship-engine-mdot 1d0) 23.08d0)) 0.01d0))
  (is = 0d0 (lis::ship-engine-mdot 0d0))
  (true (< (abs (- (lis::ship-dv-remaining 28000d0) 6650)) 1)))

(defun rcs-matrix (m-p)
  (lis::rcs-matrix! (lis::make-rcs-matrix)
                    (lis::mass-properties-com (mass-properties m-p))))

(define-test ship-rcs-couples
  (is = 3204d0 (lis::rcs-couple 0))
  (is = 12460d0 (lis::rcs-couple 1))
  (is = 12460d0 (lis::rcs-couple 2))
  ;; The pitch couple from the layout: the z nozzles of the front blocks at
  ;; z = +1.8 (pushing −z) and of the aft blocks at z = −1.8 (pushing +z).
  (let ((b (rcs-matrix 28000))
        (force (lis::make-vec3)) (torque (lis::make-vec3)))
    (dotimes (i 24)
      (when (and (/= 0 (aref lis::*rcs-directions* i 2))
                 (= (aref lis::*rcs-positions* i 2)
                    (if (> (aref lis::*rcs-positions* i 0) 7) 1.8d0 -1.8d0)))
        (dotimes (k 3)
          (incf (aref force k) (aref b k i))
          (incf (aref torque k) (aref b (+ k 3) i)))))
    (true (< (lis::vec3-norm force) 1d-9))
    (true (< (abs (- (aref torque 1) 12460)) 1d-9))
    (true (< (abs (aref torque 0)) 1d-9))
    (true (< (abs (aref torque 2)) 1d-9))))

(define-test ship-rcs-opposed-pairs
  (let ((b (rcs-matrix 14000))
        (pairs 0))
    (dotimes (i 24)
      (dotimes (j 24)
        ;; j pushes against i along the same line of action.
        (when (and (< i j)
                   (loop for k below 3
                         always (= (aref lis::*rcs-directions* j k)
                                   (- (aref lis::*rcs-directions* i k))))
                   (loop for k below 3
                         always (or (/= 0 (aref lis::*rcs-directions* i k))
                                    (= (aref lis::*rcs-positions* i k)
                                       (aref lis::*rcs-positions* j k)))))
          (incf pairs)
          (dotimes (row 6)
            (true (< (abs (+ (aref b row i) (aref b row j))) 1d-9))))))
    (is = 12 pairs)))

(define-test ship-rcs-translation
  (dolist (m-p '(28000 0))
    (is = 1780d0 (lis::rcs-translation-capability
                  0 (lis::mass-properties-com (mass-properties m-p)))))
  (loop for (m-p force) in '((28000 1266) (0 1391))
        for com = (lis::mass-properties-com (mass-properties m-p))
        do (dolist (axis '(1 2))
             (true (< (abs (- (lis::rcs-translation-capability axis com) force)) 1)))))

(defun burn (seconds m-p)
  "A full-throttle burn in field-free space (:none) from rest, 1 s steps
(the thrust cap); returns the state."
  (let ((state (lis::make-ship-state (lis::vec3 0 0 0) (lis::vec3 0 0 0)
                                     (lis::quat 1 0 0 0) (lis::vec3 0 0 0) m-p))
        (force (lis::vec3 (lis::ship-engine-force 1d0) 0 0))
        (zero (lis::make-vec3))
        (mdot (lis::ship-engine-mdot 1d0))
        (p (lis::make-mass-properties)))
    (dotimes (i seconds state)
      (lis::ship-mass-properties (lis::ship-state-m-p state) nil p)
      (lis::rk4-step! state 1d0 (float i 1d0) :none force zero mdot lis::*ship-dry-mass*
                      (lis::mass-properties-inertia p) (lis::mass-properties-inertia-inverse p)))))

(define-test ship-thrust
  ;; docs/architecture.md "Drift bounds", Thrust.
  (let* ((state (burn 100 28000d0))
         (m1 (+ lis::*ship-dry-mass* (lis::ship-state-m-p state)))
         (dv (* lis::*engine-isp* lis::+g0+ (log (/ 35961d0 m1))))
         (speed (lis::vec3-norm (lis::ship-state-v state))))
    (true (< (relative-error speed dv) 1d-9))
    (true (< (relative-error (lis::ship-state-dv state) dv) 1d-12)))
  ;; The tanks empty in the step containing 28,000 / ṁ = 1,212.98 s (the
  ;; design's 1,213 s; plan 14's "1,213.2" was a rounding slip).
  (true (< (abs (- (/ 28000 (lis::ship-engine-mdot 1d0)) 1213)) 0.05d0))
  (true (> (lis::ship-state-m-p (burn 1212 28000d0)) 0))
  (let ((state (burn 1213 28000d0)))
    (is = 0d0 (lis::ship-state-m-p state))
    (let ((dv (lis::ship-state-dv state))
          (state (burn 1300 28000d0)))
      (is = 0d0 (lis::ship-state-m-p state))
      (is = dv (lis::ship-state-dv state))
      (is = 7961d0 (lis::mass-properties-mass (mass-properties (lis::ship-state-m-p state))))
      (true (< (relative-error dv (lis::ship-dv-remaining 28000d0)) 1d-12)))))

(defun mass-properties-loop (n p b)
  (declare (type fixnum n) (inline lis::ship-mass-properties))
  (dotimes (i n)
    (lis::ship-mass-properties (* 2.8d0 i) nil p)
    (lis::rcs-matrix! b (lis::mass-properties-com p))))

(define-test ship-mass-properties-conses-nothing
  (let ((p (lis::make-mass-properties))
        (b (lis::make-rcs-matrix)))
    (mass-properties-loop 1 p b)
    (let ((consed (least-bytes-consed (lambda () (mass-properties-loop 3334 p b)))))
      (true (< consed 1024) "~D bytes" consed))))

;;; The stack and the ship's collision shapes (plan 24, task 187; decision
;;; 112: Grapple, stack and berth in the sim: state, notices to the clock,
;;; the latest checkpoint).

(define-test ship-mass-properties-unloaded
  ;; The values before the load existed, printed readably: bit for bit.
  (loop for (m-p com inertia)
          in '((28000d0 (10.340613955716982d0 0d0 -0.21826979227496454d0)
                (89975.71194156798d0 0d0 19915.702938786257d0 0d0 452196.27640097216d0 0d0
                 19915.702938786257d0 0d0 424934.85548783d0))
               (14000d0 (9.462244748557758d0 0d0 -0.357415418241428d0)
                (72710.7300942318d0 0d0 26810.198519620448d0 0d0 339315.7821470713d0 0d0
                 26810.198519620448d0 0d0 313146.5430812654d0))
               (0d0 (9.46078130888079d0 0d0 -0.9859565381233514d0)
                (51604.38513605464d0 0d0 26821.685350332904d0 0d0 203446.94811655593d0 0d0
                 26821.685350332904d0 0d0 182211.2540089268d0)))
        for p = (lis::ship-mass-properties m-p nil)
        do (is equalp (coerce com 'lis::vec3) (lis::mass-properties-com p))
           (is equalp (coerce inertia 'lis::mat3) (lis::mass-properties-inertia p))))

(define-test stack-inertia-table
  ;; docs/design.md "Inertia", the loaded rows: (m-p rock mass com I_xx I_yy
  ;; I_zz I_xz) at stack-design-pose, against the unloaded row of the same
  ;; propellant. The tensor within 0.5 % plus the unloaded ship's own
  ;; deviation from its row (decision 71's model; decision 135: The stack
  ;; in ship.lisp: the load's form, loaded inertia bounds, the range
  ;; finder's details).
  (loop for (m-p key mass com ixx iyy izz ixz base)
          in '((28000d0 :skua-yard-3 85961d0 (15.21d0 0 -0.09d0) 1.55d5 1.98d6 1.95d6 -1.9d4
                (8.79d4 4.48d5 4.21d5))
               (0d0 :skua-yard-3 57961d0 (17.44d0 0 -0.14d0) 1.22d5 8.60d5 8.32d5 -3.6d4
                (4.95d4 1.99d5 1.78d5))
               (28000d0 :skua-yard-5 135961d0 (16.85d0 0 -0.06d0) 2.98d5 2.73d6 2.70d6 -3.1d4
                (8.79d4 4.48d5 4.21d5)))
        for unloaded = (lis::ship-mass-properties m-p nil)
        for p = (lis::ship-mass-properties
                 m-p (lis::stack-design-pose (lis::object-definition key)))
        do (is = mass (lis::mass-properties-mass p))
           (dotimes (i 3)
             (true (<= (abs (- (aref (lis::mass-properties-com p) i) (nth i com))) 0.005d0)))
           (loop for axis from 0
                 for row in (list ixx iyy izz)
                 for own in base
                 do (true (<= (abs (- (tensor p axis axis) row))
                              (+ (* 0.005d0 row) (abs (- (tensor unloaded axis axis) own))))
                          "~D ~A axis ~D: ~F" m-p key axis (tensor p axis axis)))
           (true (<= (abs (- (tensor p 0 2) ixz)) 1d3))))

(define-test stack-design-pose
  (let ((load (lis::stack-design-pose (lis::object-definition :skua-yard-3))))
    ;; The rock's centre at 16.9 + r, its +x along the ship's −x.
    (is equalp (lis::vec3 (+ 16.6d0 2.114d0) 0 0) (lis::stack-load-com load))
    (is equalp (lis::vec3 -1 0 0)
            (lis::quat-rotate (lis::stack-load-q load) (lis::vec3 1 0 0)))))

(define-test stack-turned-load
  ;; K-17 (not isotropic) turned 90° about x at p: the parallel-axis sum
  ;; done by hand about the stack's CoM.
  (let* ((def (lis::object-definition :k-17))
         (q (lis::quat-from-axis-angle (lis::vec3 1 0 0) (/ pi 2)))
         (pos (lis::vec3 22.0d0 0.4d0 -0.3d0))
         (ship (lis::ship-mass-properties 14000d0 nil))
         (p (lis::ship-mass-properties 14000d0 (lis::make-stack-load def pos q)))
         (m0 (lis::mass-properties-mass ship))
         (m1 (lis::object-def-mass def))
         (c (lis::vec3-scale (lis::vec3-add (lis::vec3-scale (lis::mass-properties-com ship) m0)
                                            (lis::vec3-scale pos m1))
                             (/ (+ m0 m1))))
         (r (lis::mat3-from-quat q))
         (turned (lis::mat3* (lis::mat3* r (lis::object-def-tensor def)) (lis::mat3-transpose r)))
         (expected (lis::make-mat3)))
    (loop for (m centre own) in (list (list m0 (lis::mass-properties-com ship)
                                            (lis::mass-properties-inertia ship))
                                      (list m1 pos turned))
          for d = (lis::vec3-sub centre c)
          do (dotimes (i 3)
               (dotimes (j 3)
                 (incf (aref expected (+ (* 3 i) j))
                       (+ (aref own (+ (* 3 i) j))
                          (* m (- (if (= i j) (lis::vec3-dot d d) 0)
                                  (* (aref d i) (aref d j)))))))))
    ;; The turn swaps I_yy and I_zz of K-17's own tensor.
    (true (< (relative-error (aref turned 4) 5.65d4) 1d-12))
    (is = (+ m0 m1) (lis::mass-properties-mass p))
    (dotimes (i 3)
      (true (< (relative-error (aref (lis::mass-properties-com p) i)
                               (if (zerop (aref c i)) 1d0 (aref c i)))
               1d-12)))
    (dotimes (k 9)
      (true (< (/ (abs (- (aref (lis::mass-properties-inertia p) k) (aref expected k)))
                  (aref expected 0))
               1d-12)))))

(defun loaded-mass-properties-loop (n p load)
  (declare (type fixnum n) (inline lis::ship-mass-properties lis::stack-bound))
  (let ((sum 0d0))
    (declare (type double-float sum))
    (dotimes (i n sum)
      (lis::ship-mass-properties (* 2.8d0 i) load p)
      (incf sum (lis::stack-bound (lis::mass-properties-com p) load)))))

(define-test stack-conses-nothing
  (let ((p (lis::make-mass-properties))
        (load (lis::stack-design-pose (lis::object-definition :skua-yard-3))))
    (loaded-mass-properties-loop 1 p load)
    (let ((consed (least-bytes-consed (lambda () (loaded-mass-properties-loop 3334 p load)))))
      (true (< consed 1024) "~D bytes" consed))))

(define-test stack-bound
  ;; Unloaded wet: the farthest corner, the truss's aft corners from the CoM
  ;; (THRIFT's bound 12.6 m, docs/design.md "Rigid bodies").
  (let* ((com (lis::mass-properties-com (lis::ship-mass-properties 28000d0 nil)))
         (expected (loop for shape across lis::*ship-collision-boxes*
                         maximize (loop for k below 8
                                        maximize (lis::vec3-norm
                                                  (lis::vec3-sub
                                                   (lis::vec3-add
                                                    (lis::shape-centre shape)
                                                    (let ((s (lis::shape-size shape)))
                                                      (lis::vec3 (* (if (logbitp 0 k) 0.5 -0.5) (aref s 0))
                                                                 (* (if (logbitp 1 k) 0.5 -0.5) (aref s 1))
                                                                 (* (if (logbitp 2 k) 0.5 -0.5) (aref s 2)))))
                                                   com))))))
    (is = expected (lis::stack-bound com))
    (true (< (abs (- (lis::stack-bound com) 12.6d0)) 0.05d0)))
  ;; Loaded: the farther of the ship's corners and the load's bound sphere
  ;; from the stack's CoM; K-17's reaches past the aft corners.
  (dolist (key '(:skua-yard-3 :k-17))
    (let* ((load (lis::stack-design-pose (lis::object-definition key)))
           (com (lis::mass-properties-com (lis::ship-mass-properties 28000d0 load)))
           (sphere (+ (lis::vec3-norm (lis::vec3-sub (lis::stack-load-com load) com))
                      (lis::object-def-bound (lis::stack-load-def load)))))
      (is = (max (lis::stack-bound com) sphere) (lis::stack-bound com load))
      (if (eq key :k-17)
          (true (> sphere (lis::stack-bound com)))
          (true (< sphere (lis::stack-bound com)))))))

;;; The range finder (decision 113: Grapple and contact details the design
;;; leaves open).

(defun range-scene (sim &key (range 50d0) (offset-y 0.2d0) (offset-z 0d0) (angle 5d0)
                             (closing 0.1d0) (rotation 0.3d0))
  "SIM with Skua Yard 3 promoted by hand in a 500 km orbit and a wet ship
before its fixture: RANGE m, the pin at OFFSET-Y, OFFSET-Z in ship axes,
yawed ANGLE degrees off the fixture axis, closing at CLOSING m/s and
rotating ROTATION °/s relative to it (about the ship's z); SIM."
  (let* ((rock (lis::sim-object sim :skua-yard-3))
         (state (lis::object-state rock))
         (radius (+ lis::+radius-earth+ 500d3))
         (q-o (lis::quat-from-axis-angle (lis::vec3 1 2 3) 0.7d0))
         (theta (lis::deg->rad angle))
         (q-s (lis::quat-mul (lis::quat-mul q-o (lis::quat 0 0 0 1))
                             (lis::quat-from-axis-angle (lis::vec3 0 0 1) theta)))
         (axis (lis::quat-rotate q-o (lis::vec3 1 0 0)))
         (pin (lis::vec3-add (lis::vec3 radius 0 0)
                             (lis::quat-rotate q-o (lis::fixture-pin
                                                    (first (lis::object-def-fixtures
                                                            (lis::object-def-of rock)))))))
         (d-b (lis::vec3 (/ (+ range (* offset-y (sin theta))) (cos theta)) offset-y offset-z))
         (com (lis::mass-properties-com (lis::ship-mass-properties 28000d0 nil)))
         (tip-b (lis::quat-rotate q-s (lis::vec3-sub lis::*grapple-tip* com)))
         (tip (lis::vec3-sub pin (lis::quat-rotate q-s d-b)))
         (v-o (lis::vec3 0 (sqrt (/ lis::+mu-earth+ radius)) 0))
         (w-o (lis::vec3 (lis::deg->rad 0.15d0) 0 0))
         (w-s-i (lis::vec3-add (lis::quat-rotate q-o w-o)
                               (lis::quat-rotate q-s (lis::vec3 0 0 (lis::deg->rad rotation)))))
         (v-s (lis::vec3-sub (lis::vec3-sub v-o (lis::vec3-scale axis closing))
                             (lis::vec3-cross w-s-i tip-b))))
    (setf (lis::object-status rock) :promoted)
    (lis::ship-state-assign! state (lis::make-ship-state (lis::vec3 radius 0 0) v-o q-o w-o 0d0))
    (lis::ship-state-assign! (lis::sim-ship sim)
                             (lis::make-ship-state (lis::vec3-sub tip tip-b) v-s q-s
                                                   (lis::quat-rotate (lis::quat-conjugate q-s) w-s-i)
                                                   28000d0))
    sim))

(defun range-reading (sim &rest scene)
  (lis::grapple-range-into! (lis::make-grapple-reading) (apply #'range-scene sim scene)))

(define-test grapple-range-finder
  (let* ((sim (lis::new-game-sim))
         (reading (range-reading sim)))
    (true reading)
    (is = (position :skua-yard-3 (lis::sim-objects sim) :key #'lis::object-key)
        (lis::grapple-reading-object reading))
    (is = 0 (lis::grapple-reading-fixture reading))
    (loop for (value expected) in `((,(lis::grapple-reading-range reading) 50d0)
                                    (,(lis::grapple-reading-offset-y reading) 0.2d0)
                                    (,(lis::grapple-reading-offset-z reading) 0d0)
                                    (,(lis::grapple-reading-closing-speed reading) 0.1d0)
                                    (,(lis::grapple-reading-angle reading) ,(lis::deg->rad 5d0))
                                    (,(lis::grapple-reading-rotation reading)
                                     ,(lis::deg->rad 0.3d0)))
          do (true (< (abs (- value expected)) 1d-9) "~F, not ~F" value expected))
    (true (and (lis::grapple-reading-closing-ok reading) (lis::grapple-reading-offset-ok reading)
               (lis::grapple-reading-angle-ok reading) (lis::grapple-reading-rotation-ok reading)))
    ;; Each tolerance's edges: (scene inside outside reader).
    (loop for (key inside outside reader)
            in '((:closing 0.0101d0 0.0099d0 lis::grapple-reading-closing-ok)
                 (:closing 0.2999d0 0.3001d0 lis::grapple-reading-closing-ok)
                 (:offset-y 0.2999d0 0.3001d0 lis::grapple-reading-offset-ok)
                 (:offset-z -0.2999d0 -0.3001d0 lis::grapple-reading-offset-ok)
                 (:angle 9.99d0 10.01d0 lis::grapple-reading-angle-ok)
                 (:rotation 0.499d0 0.501d0 lis::grapple-reading-rotation-ok))
          do (flet ((flag (value)
                      (funcall reader (range-reading sim key value
                                                     :offset-y (if (eq key :offset-y) value 0d0)))))
               (true (flag inside) "~A ~F inside" key inside)
               (false (flag outside) "~A ~F outside" key outside)))
    ;; The radial offset: 0.2 and 0.2 inside, 0.25 and 0.2 outside.
    (true (lis::grapple-reading-offset-ok (range-reading sim :offset-y 0.2d0 :offset-z 0.2d0)))
    (false (lis::grapple-reading-offset-ok (range-reading sim :offset-y 0.25d0 :offset-z 0.2d0)))
    ;; Behind the tip, beyond 500 m from the tip, or on rails: nil.
    (false (range-reading sim :range -1d0))
    (false (range-reading sim :range 501d0))
    (true (range-reading sim :range 495d0))
    (range-scene sim)
    (setf (lis::object-status (lis::sim-object sim :skua-yard-3)) :rails)
    (false (lis::grapple-range-into! (lis::make-grapple-reading) sim))))

(define-test grapple-range-conses-nothing
  (let ((sim (range-scene (lis::new-game-sim)))
        (reading (lis::make-grapple-reading)))
    (lis::grapple-range-into! reading sim)
    (let ((consed (least-bytes-consed
                   (lambda () (dotimes (i 1000) (lis::grapple-range-into! reading sim))))))
      (true (< consed 1024) "~D bytes" consed))))
