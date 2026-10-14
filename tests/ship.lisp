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
    (let ((consed (let ((before (sb-ext:get-bytes-consed)))
                    (mass-properties-loop 10000 p b)
                    (- (sb-ext:get-bytes-consed) before))))
      (true (< consed 1024) "~D bytes" consed))))
