(in-package #:luv-in-space.tests)

;;; physics.lisp: ship state, gravity models, the coarse step rule
;;; (docs/architecture.md "Gravity model", "Integrator").

(defun relative-error (x reference)
  (/ (abs (- x reference)) (abs reference)))

(defun gravity (r model &optional (time (lis::start-t)))
  (lis::gravity-into! (lis::make-vec3) r time model))

(define-test ship-state-copy
  (let* ((state (lis::make-ship-state (lis::vec3 7d6 0 0) (lis::vec3 0 7.5d3 0)
                                      (lis::quat 1 0 0 0) (lis::vec3 0 0.1d0 0) 28000))
         (copy (lis::copy-ship-state state)))
    (true (equalp state copy))
    (false (eq (lis::ship-state-r state) (lis::ship-state-r copy)))
    (false (eq (lis::ship-state-q state) (lis::ship-state-q copy)))
    (is = 0d0 (lis::ship-state-dv copy))
    (setf (aref (lis::ship-state-w copy) 1) 0.2d0)
    (false (equalp state copy))
    (is = 0.1d0 (aref (lis::ship-state-w state) 1))))

(define-test gravity-none-and-point
  (let ((r (lis::vec3 7d6 -2d6 3d6)))
    (is equalp (lis::vec3 0 0 0) (gravity r :none))
    (let ((a (gravity r :point))
          (rn (lis::vec3-norm r)))
      (true (< (relative-error (lis::vec3-norm a) (/ lis::+mu-earth+ (* rn rn))) 1d-15))
      (true (< (lis::vec3-norm (lis::vec3-cross a r)) (* 1d-15 (lis::vec3-norm a) rn))))
    (fail (gravity r :nonsense))))

(define-test gravity-j2-closed-forms
  (let* ((rr (+ lis::+radius-earth+ 400d3))
         (k (/ lis::+mu-earth+ (* rr rr)))
         (c (* lis::+j2-earth+ (expt (/ lis::+radius-earth+ rr) 2))))
    ;; Equator, at 30° longitude so x and y both carry the term.
    (let* ((u (lis::vec3 (cos (/ pi 6)) (sin (/ pi 6)) 0))
           (a (gravity (lis::vec3-scale u rr) :j2))
           (radial (lis::vec3-dot a u)))
      (true (< (relative-error radial (- (* k (+ 1 (* 1.5d0 c))))) 1d-14))
      (true (< (lis::vec3-norm (lis::vec3-add-scaled a u (- radial)))
               (* 1d-14 (abs radial)))))
    ;; Over the pole.
    (let ((a (gravity (lis::vec3 0 0 rr) :j2)))
      (true (< (relative-error (aref a 2) (- (* k (- 1 (* 3 c))))) 1d-14))
      (is = 0d0 (aref a 0))
      (is = 0d0 (aref a 1)))
    ;; z → −z mirrors the acceleration exactly.
    (let ((a (gravity (lis::vec3 4d6 -3d6 5d6) :j2))
          (b (gravity (lis::vec3 4d6 -3d6 -5d6) :j2)))
      (is = (aref a 0) (aref b 0))
      (is = (aref a 1) (aref b 1))
      (is = (aref a 2) (- (aref b 2))))))

(defun rational-sqrt (x)
  "√x of a positive rational, to far below double precision."
  (let ((y (rational (sqrt (float x 1d0)))))
    (dotimes (i 4 y)
      (setf y (/ (+ y (/ x y)) 2)))))

(defun exact-third-body (r d mu)
  "μ ((d − r)/|d − r|³ − d/|d|³) in rationals, as a list of three."
  (let* ((r (map 'list #'rational r))
         (d (map 'list #'rational d))
         (mu (rational mu))
         (s (mapcar #'- d r))
         (s2 (reduce #'+ (mapcar #'* s s)))
         (d2 (reduce #'+ (mapcar #'* d d)))
         (ks (/ mu (* s2 (rational-sqrt s2))))
         (kd (/ mu (* d2 (rational-sqrt d2)))))
    (mapcar (lambda (si di) (- (* ks si) (* kd di))) s d)))

(defun third-body-term (r d mu)
  (lis::third-body-add! (lis::make-vec3) r d mu))

(defun three-points (d distance)
  "Points at DISTANCE towards, away from and across the direction of d."
  (let ((u (lis::vec3-normalize d))
        (across (lis::vec3-normalize (lis::vec3-cross d (lis::vec3 0 0 1)))))
    (list (lis::vec3-scale u distance)
          (lis::vec3-scale u (- distance))
          (lis::vec3-scale across distance))))

(define-test gravity-third-bodies
  (let ((time (lis::start-t)))
    (dolist (body (list (cons :moon lis::+mu-moon+) (cons :sun lis::+mu-sun+)))
      (let ((d (lis::body-state (car body) time :earth)))
        (dolist (r (three-points d 7d6))
          (let* ((term (third-body-term r d (cdr body)))
                 (exact (exact-third-body r d (cdr body)))
                 (exact-norm (rational-sqrt (reduce #'+ (mapcar #'* exact exact))))
                 (err (rational-sqrt (reduce #'+ (mapcar (lambda (x e) (expt (- (rational x) e) 2))
                                                           (coerce term 'list) exact)))))
            (true (< (/ err exact-norm) 1d-9) "~A at ~A" (car body) r)))))
    ;; :full is :j2 plus both terms.
    (let* ((r (lis::vec3 5d6 -4d6 2d6))
           (expected (gravity r :j2 time)))
      (lis::third-body-add! expected r (lis::body-state :moon time) lis::+mu-moon+)
      (lis::third-body-add! expected r (lis::body-state :sun time) lis::+mu-sun+)
      (is equalp expected (gravity r :full time)))))

;; The tidal term's next order is ±1.5 r/d, ~2.7 % at 7,000 km from Earth,
;; so the plan's 2 % is loosened to 3 r/d (~5.5 %).
(define-test gravity-moon-tidal
  (let* ((d (lis::body-state :moon (lis::start-t)))
         (dn (lis::vec3-norm d))
         (rn 7d6)
         (tidal (/ (* 2 lis::+mu-moon+ rn) (expt dn 3))))
    (dolist (r (subseq (three-points d rn) 0 2))
      (let ((term (third-body-term r d lis::+mu-moon+)))
        (true (< (relative-error (lis::vec3-norm term) tidal) (* 3 (/ rn dn))))
        ;; Away from the Earth along the line, both ways.
        (true (plusp (lis::vec3-dot term r)))))))

(defun power-of-two-p (x)
  (= 0.5d0 (decode-float x)))

(define-test coarse-step-rule
  (let* ((time (lis::start-t))
         (away-from-moon (lis::vec3-normalize (lis::vec3-scale (lis::body-state :moon time) -1d0)))
         (away-from-sun (lis::vec3-normalize (lis::vec3-scale (lis::body-state :sun time) -1d0)))
         (leo (lis::vec3 (+ lis::+radius-earth+ 400d3) 0 0)))
    (flet ((check (r &optional thrusting)
             (let ((h (lis::coarse-step r time thrusting)))
               (true (power-of-two-p h) "~A not a power of two" h)
               h)))
      (is = 2d0 (check leo))
      (is = 2d0 (lis::coarse-step leo 0d0 nil))
      (is = 512d0 (check (lis::vec3-scale away-from-moon 2d8)))
      (is = 1d0 (check leo t))
      (is = 1d0 (check (lis::vec3-scale away-from-moon 2d8) t))
      ;; At the surface the rule gives 2 s (√(R³/μ)/256 ≈ 3.2 s); the lower
      ;; clamp needs d < ~185 km from a centre.
      (is = 2d0 (check (lis::vec3 0 0 lis::+radius-earth+)))
      (is = (/ 1d0 64) (check (lis::vec3 1d5 0 0)))
      (is = (/ 1d0 64) (check (lis::vec3 0 0 0)))
      (is = 16384d0 (check (lis::vec3-scale away-from-sun 1d10)))
      (is = 1d0 (check (lis::vec3-scale away-from-sun 1d10) t)))))

(defun gravity-and-step-loop (a r time n)
  "N evaluations of :full gravity and the step rule; the sum of the steps."
  (declare (type lis::vec3 a r) (type double-float time) (type fixnum n))
  (let ((sum 0d0))
    (declare (type double-float sum))
    (dotimes (i n sum)
      (let ((time (+ time i)))
        (lis::gravity-into! a r time :full)
        (incf sum (lis::coarse-step r time nil))))))

(define-test gravity-and-step-cons-nothing
  (let ((a (lis::make-vec3))
        (r (lis::vec3 6.7d6 1d5 2d5))
        (time (lis::start-t)))
    ;; The first run fills the ephemeris cache.
    (is = 2000d0 (gravity-and-step-loop a r time 1000))
    ;; Measured before the check: Parachute conses its result first.
    (let ((consed (least-bytes-consed
                   (lambda () (gravity-and-step-loop a r time 1000)))))
      ;; The returned sum is one boxed double.
      (true (<= consed 16) "~D bytes" consed))))

;;; The RK4 step (docs/architecture.md "Integrator", "Drift bounds").

(defparameter *test-inertia* (lis::mat3 1d4 0 0  0 2d4 0  0 0 3d4))

(defun step! (state h time &key (model :point) (force (lis::make-vec3))
                                (torque (lis::make-vec3)) (mdot 0d0) (m-dry 8000d0)
                                (inertia *test-inertia*) attitude-fn)
  (lis::rk4-step! state h time model force torque mdot m-dry inertia
                  (lis::mat3-inverse-symmetric inertia) attitude-fn))

(defun drift-orbit (e)
  "The test orbit of \"Drift bounds\": periapsis 6,778 km, i 51.6°, Ω 30°,
ω 40°, at periapsis at the start date."
  (lis::make-elliptic-orbit lis::+mu-earth+ (/ 6778d3 (- 1 e)) e (deg 51.6) (deg 30) (deg 40)
                            0d0 (lis::start-t)))

(defun specific-energy (r v)
  (- (/ (lis::vec3-dot v v) 2) (/ lis::+mu-earth+ (lis::vec3-norm r))))

(defun drift-run (e orbits coarse)
  "Integrate the drift orbit for ORBITS periods at the fine step or the
coarse step rule; (values ΔE/E ΔL/L position-error) as maxima at whole
periods."
  (declare (type double-float e) (type fixnum orbits) (inline lis::rk4-step!))
  (let* ((orbit (drift-orbit e))
         (period (lis::orbit-period orbit))
         (t0 (lis::start-t))
         (inertia *test-inertia*)
         (inverse (lis::mat3-inverse-symmetric inertia))
         (zero (lis::make-vec3))
         (state (multiple-value-bind (r v) (lis::orbit-state orbit t0)
                  (lis::make-ship-state r v (lis::quat-from-axis-angle (lis::vec3 1 2 3) 0.7d0)
                                        (lis::make-vec3) 0d0)))
         (r (lis::ship-state-r state))
         (v (lis::ship-state-v state))
         (e0 (specific-energy r v))
         (l0 (lis::vec3-cross r v))
         (time t0) (next 1) (de 0d0) (dl 0d0) (dr 0d0))
    (declare (type double-float time de dl dr) (type fixnum next))
    (loop while (<= next orbits)
          do (let ((h (if coarse (lis::coarse-step r time nil) lis::+fine-step+)))
               (declare (type double-float h))
               (lis::rk4-step! state h time :point zero zero 0d0 1000d0 inertia inverse)
               (setf time (+ time h)))
             (when (>= time (+ t0 (* next period)))
               (setf de (max de (relative-error (specific-energy r v) e0))
                     dl (max dl (/ (lis::vec3-norm (lis::vec3-sub (lis::vec3-cross r v) l0))
                                   (lis::vec3-norm l0)))
                     dr (max dr (lis::vec3-norm (lis::vec3-sub r (lis::orbit-state orbit time)))))
               (incf next)))
    (values de dl dr)))

(defmacro define-drift-test (name e orbits coarse de dl dr)
  `(define-test ,name
     (multiple-value-bind (de dl dr) (drift-run ,e ,orbits ,coarse)
       (true (< de ,de) "ΔE/E ~,2E" de)
       (true (< dl ,dl) "ΔL/L ~,2E" dl)
       (true (< dr ,dr) "position ~,3F m" dr))))

;; The table of docs/architecture.md "Drift bounds". The fine rows run 5
;; circular orbits and 1 at e = 0.5 instead of 10 and 2, bounds unchanged:
;; the whole table took 2.3 s, over the ~2 s of plan 14 (measured 2026-10-14:
;; ΔE/E 1.5e-13, ΔL/L 1.1e-13 and 3e-5 m over 10 circular orbits).
(define-drift-test drift-fine-circular 0d0 5 nil 1d-11 1d-11 0.05d0)
(define-drift-test drift-fine-e0.5 0.5d0 1 nil 1d-11 1d-11 0.05d0)
(define-drift-test drift-coarse-circular 0d0 1000 t 1d-10 1d-10 5d0)
(define-drift-test drift-coarse-e0.5 0.5d0 1000 t 1d-9 1d-9 100d0)
(define-drift-test drift-coarse-e0.9 0.9d0 200 t 1d-8 1d-9 5000d0)

(defun rotation-run (inertia)
  "1,000 s of torque-free rotation at the fine step from |ω| = 0.1 rad/s
off the principal axes; (values ΔT/T Δ|Iω|/|Iω| max ||q| − 1|), the
angular momentum compared in inertial axes."
  (let* ((inverse (lis::mat3-inverse-symmetric inertia))
         (zero (lis::make-vec3))
         (state (lis::make-ship-state (lis::vec3 1d8 0 0) (lis::make-vec3) (lis::quat 1 0 0 0)
                                      (lis::vec3-scale (lis::vec3-normalize (lis::vec3 1 0.5d0 -0.7d0))
                                                       0.1d0)
                                      0d0))
         (time 0d0) (dt 0d0) (dl 0d0) (dq 0d0))
    (flet ((energy ()
             (let ((w (lis::ship-state-w state)))
               (/ (lis::vec3-dot w (lis::mat3*vec3 inertia w)) 2)))
           (momentum ()
             (lis::quat-rotate (lis::ship-state-q state)
                               (lis::mat3*vec3 inertia (lis::ship-state-w state)))))
      (let ((t0 (energy)) (l0 (momentum)))
        (dotimes (i (* 1000 64))
          (lis::rk4-step! state lis::+fine-step+ time :none zero zero 0d0 1000d0 inertia inverse)
          (incf time lis::+fine-step+)
          (when (zerop (mod (1+ i) 640))
            (let ((q (lis::ship-state-q state)))
              (setf dt (max dt (relative-error (energy) t0))
                    dl (max dl (rel-diff (momentum) l0))
                    dq (max dq (abs (- 1 (sqrt (loop for x across q sum (* x x))))))))))
        (values dt dl dq)))))

(define-test rotation-torque-free
  (let ((tilted (let ((rot (lis::mat3-from-quat (lis::quat-from-axis-angle (lis::vec3 0 1 0)
                                                                           (deg 10)))))
                  (lis::mat3* rot (lis::mat3* *test-inertia* (lis::mat3-transpose rot))))))
    (true (/= 0d0 (aref tilted 2)))
    (dolist (inertia (list *test-inertia* tilted))
      (multiple-value-bind (dt dl dq) (rotation-run inertia)
        (true (< dt 1d-9) "ΔT/T ~,2E" dt)
        (true (< dl 1d-9) "Δ|Iω| ~,2E" dl)
        (true (< dq 1d-12) "|q| − 1 ~,2E" dq)))))

(define-test rk4-exhaustion
  ;; ṁ h = 2 m_p: the step is scaled by ½ and empties the tanks exactly.
  (let* ((force 101800d0) (mdot 23.08d0) (m-p (* 0.5d0 mdot)) (m-dry 7961d0)
         (state (lis::make-ship-state (lis::make-vec3) (lis::make-vec3) (lis::quat 1 0 0 0)
                                      (lis::make-vec3) m-p)))
    (step! state 1d0 0d0 :model :none :force (lis::vec3 force 0 0) :mdot mdot :m-dry m-dry
                         :torque (lis::vec3 0 100 0))
    (is = 0d0 (lis::ship-state-m-p state))
    (let ((rocket (* (/ force mdot) (log (/ (+ m-dry m-p) m-dry)))))
      (true (< (relative-error (lis::ship-state-dv state) rocket) 1d-12))
      (true (< (relative-error (aref (lis::ship-state-v state) 0) rocket) 1d-6)))
    ;; Dry: force, torque and ṁ are zero.
    (let ((before (lis::copy-ship-state state)))
      (step! state 1d0 1d0 :model :none :force (lis::vec3 force 0 0) :mdot mdot :m-dry m-dry
                           :torque (lis::vec3 0 100 0))
      (is equalp (lis::ship-state-v before) (lis::ship-state-v state))
      (is equalp (lis::ship-state-w before) (lis::ship-state-w state))
      (is = 0d0 (lis::ship-state-m-p state))
      (is = (lis::ship-state-dv before) (lis::ship-state-dv state)))))

(defun prograde-attitude (q-out w-out time-cell r v)
  "Body x along v, z towards −r (orthogonalised), the frame's rate r × v / r²."
  (declare (ignore time-cell))
  (let* ((x (lis::vec3-normalize v))
         (z (lis::vec3-normalize (lis::vec3-add-scaled (lis::vec3-scale r -1d0) x
                                                       (lis::vec3-dot r x))))
         (y (lis::vec3-cross z x)))
    (replace q-out (lis::quat-from-basis x y z))
    (lis::vec3-scale! w-out (lis::vec3-cross r v) (/ (lis::vec3-dot r r)))))

(define-test rk4-kinematic-attitude
  (let* ((orbit (drift-orbit 0d0))
         (time (lis::start-t))
         (state (multiple-value-bind (r v) (lis::orbit-state orbit time)
                  (lis::make-ship-state r v (lis::quat 1 0 0 0) (lis::make-vec3) 28000d0))))
    (dotimes (i 100)
      (let ((h (lis::coarse-step (lis::ship-state-r state) time t)))
        (step! state h time :force (lis::vec3 101800d0 0 0) :mdot 23.08d0 :m-dry 7961d0
                            :attitude-fn #'prograde-attitude)
        (incf time h)))
    (let* ((q (lis::ship-state-q state))
           (x (lis::quat-rotate q (lis::vec3 1 0 0)))
           (vn (lis::vec3-normalize (lis::ship-state-v state))))
      (true (< (lis::vec3-norm (lis::vec3-cross x vn)) 1d-12))
      (true (plusp (lis::vec3-dot x vn)))
      ;; ω in body axes is the orbit rate about body −y.
      (let ((w (lis::ship-state-w state))
            (rate (/ (lis::vec3-norm (lis::vec3-cross (lis::ship-state-r state)
                                                      (lis::ship-state-v state)))
                     (lis::vec3-dot (lis::ship-state-r state) (lis::ship-state-r state)))))
        (true (< (relative-error (- (aref w 1)) rate) 1d-9)))
      ;; The burn followed v: 100 s of thrust, all along track.
      (true (< (relative-error (lis::ship-state-dv state)
                               (* (/ 101800d0 23.08d0) (log (/ 35961d0 (- 35961d0 2308d0)))))
               1d-12)))))

(defun full-steps (state n)
  "N fine steps from the start date with :full gravity, thrust and torque."
  (declare (type fixnum n) (inline lis::rk4-step!))
  (let ((force (lis::vec3 1000d0 0 0))
        (torque (lis::vec3 10d0 -20d0 5d0))
        (inertia *test-inertia*)
        (inverse (lis::mat3-inverse-symmetric *test-inertia*))
        (time (lis::start-t)))
    (declare (type double-float time))
    (dotimes (i n state)
      (lis::rk4-step! state lis::+fine-step+ time :full force torque 0.3d0 8000d0 inertia inverse)
      (setf time (+ time lis::+fine-step+)))))

(defun leo-state ()
  (multiple-value-bind (r v) (lis::orbit-state (drift-orbit 0d0) (lis::start-t))
    (lis::make-ship-state r v (lis::quat 1 0 0 0) (lis::vec3 0.01d0 0 0) 28000d0)))

(define-test rk4-cons-nothing-and-deterministic
  (let ((a (leo-state))
        (b (leo-state)))
    (dotimes (i 3) (full-steps a 3333))
    (let ((consed (least-bytes-consed (lambda () (full-steps b 3333)))))
      (true (< consed 1024) "~D bytes" consed))
    (is equalp a b)))

;;; Objects and rails (docs/architecture.md "Rails", "Rails, orbits and
;;; frames", "Removal of objects").

(defun test-object-def (attitude rails)
  (lis::make-object-def :test "TEST" :towable 2500d0 (lis::mat3 1d3 0 0  0 2d3 0  0 0 3d3)
                        nil nil attitude 5d0 rails))

(define-test object-copy-and-assign
  (let* ((orbit (drift-orbit 0d0))
         (def (test-object-def (lis::make-attitude :lvlh)
                               (lis::make-rails orbit (lis::start-t))))
         (a (lis::new-object def))
         (b (lis::copy-object a)))
    (true (equalp a b))
    (is eq :rails (lis::object-status a))
    (is eq :test (lis::object-key a))
    (false (lis::object-promoted-p a))
    (false (lis::object-removed-p a))
    (setf (lis::object-status b) :promoted
          (lis::object-next-check b) 12.5d0
          (lis::object-removal b) (lis::make-removal 1d0 :earth :burned-up '(0d0 0d0)))
    (setf (aref (lis::ship-state-r (lis::object-state b)) 0) 7d6)
    (false (equalp a b))
    (is = 0d0 (aref (lis::ship-state-r (lis::object-state a)) 0))
    (true (lis::object-removed-p (progn (setf (lis::object-status b) :removed) b)))
    (let ((consed (least-bytes-consed (lambda () (dotimes (i 1000) (lis::object-assign! a b))))))
      (true (<= consed 0) "~D bytes" consed))
    (true (equalp a b))
    (is eq (lis::object-def-of a) (lis::object-def-of b))))

(defun rails-point (rails time)
  (let ((r (lis::make-vec3)) (v (lis::make-vec3)))
    (lis::rails-state-into! r v (lis::rails-segment-at rails time) time)
    (values r v)))

(define-test rails-against-integrator
  ;; Two-body rails and the coarse step over 10 orbits of the drift orbit:
  ;; within the coarse position bound (5 m, "Drift bounds").
  (let* ((orbit (drift-orbit 0d0))
         (t0 (lis::start-t))
         (rails (lis::make-rails orbit t0 :j2 nil))
         (period (lis::orbit-period orbit))
         (zero (lis::make-vec3))
         (inverse (lis::mat3-inverse-symmetric *test-inertia*))
         (state (multiple-value-bind (r v) (rails-point rails t0)
                  (lis::make-ship-state r v (lis::quat 1 0 0 0) (lis::make-vec3) 0d0)))
         (time t0) (next 1) (dr 0d0))
    (multiple-value-bind (r v) (lis::orbit-state orbit t0)
      (true (< (rel-diff r (rails-point rails t0)) 1d-15))
      (true (< (rel-diff v (nth-value 1 (rails-point rails t0))) 1d-15)))
    (is eq :horizon (lis::rails-segment-end-kind (first rails)))
    (is = lis::*rails-horizon* (lis::rails-segment-end (first rails)))
    (loop while (<= next 10)
          do (let ((h (lis::coarse-step (lis::ship-state-r state) time nil)))
               (lis::rk4-step! state h time :point zero zero 0d0 1000d0 *test-inertia* inverse)
               (incf time h))
             (when (>= time (+ t0 (* next period)))
               (setf dr (max dr (lis::vec3-norm (lis::vec3-sub (lis::ship-state-r state)
                                                               (rails-point rails time)))))
               (incf next)))
    (true (< dr 5d0) "~,3F m" dr)))

(defun leo-51.6 ()
  "A 400 km circular orbit at 51.6° from the start date."
  (lis::make-elliptic-orbit lis::+mu-earth+ 6778d3 0d0 (deg 51.6) (deg 30) 0d0 0d0 (lis::start-t)))

(define-test rails-j2-node
  (let* ((orbit (leo-51.6))
         (t0 (lis::start-t))
         (rails (lis::make-rails orbit t0))
         (segment (first rails))
         (period (lis::orbit-period orbit))
         (p (lis::orbit-semi-latus orbit))
         (closed (* -3/2 (lis::orbit-n orbit) lis::+j2-earth+
                    (expt (/ lis::+radius-earth+ p) 2) (cos (deg 51.6)))))
    (true (< (relative-error (lis::rails-segment-raan-rate segment) closed) 1d-9))
    ;; The ship integrated with J2 for a day: its osculating node averaged
    ;; over the first and the last orbit moves as the rails' node does; the
    ;; first orbit's average removes the start's short-period offset
    ;; (measured: ship −4.7014°, rails −4.6811°, 0.020° apart).
    (let* ((zero (lis::make-vec3))
           (inverse (lis::mat3-inverse-symmetric *test-inertia*))
           (state (multiple-value-bind (r v) (rails-point rails t0)
                    (lis::make-ship-state r v (lis::quat 1 0 0 0) (lis::make-vec3) 0d0)))
           (elements (lis::state->orbit (lis::ship-state-r state) (lis::ship-state-v state)
                                        lis::+mu-earth+ t0))
           (end (+ t0 lis::+day+))
           (first-sum 0d0) (first-n 0) (last-sum 0d0) (last-n 0)
           (time t0))
      (loop while (< time end)
            do (lis::rk4-step! state 2d0 time :j2 zero zero 0d0 1000d0 *test-inertia* inverse)
               (incf time 2d0)
               (flet ((node ()
                        (lis::state->orbit-into! elements (lis::ship-state-r state)
                                                 (lis::ship-state-v state) lis::+mu-earth+ time)
                        (lis::orbit-raan elements)))
                 (cond ((<= time (+ t0 period)) (incf first-sum (node)) (incf first-n))
                       ((> time (- end period)) (incf last-sum (node)) (incf last-n)))))
      (let* ((ship-drift (- (/ last-sum last-n) (/ first-sum first-n)))
             (rails-drift (* (lis::rails-segment-raan-rate segment) (- lis::+day+ period)))
             (miss (abs (- ship-drift rails-drift))))
        (true (< rails-drift (deg -4)))
        (true (< miss (deg 0.05)) "~,4F°" (/ (* miss 180) pi)))))
  ;; A polar orbit's node stays at 270° exactly.
  (let* ((orbit (lis::make-elliptic-orbit lis::+mu-earth+ 6778d3 0d0 (deg 90) (deg 270) 0d0 0d0
                                          (lis::start-t)))
         (segment (first (lis::make-rails orbit (lis::start-t)))))
    (is = 0d0 (lis::rails-segment-raan-rate segment))
    (is = (deg 270) (lis::rails-raan segment (+ (lis::start-t) (* 3650 lis::+day+))))))

(defun k-17-orbit (perigee-altitude)
  "800 km × PERIGEE-ALTITUDE, polar, at apogee at the start date."
  (let* ((ra (+ lis::+radius-earth+ 800d3))
         (rp (+ lis::+radius-earth+ perigee-altitude))
         (a (/ (+ ra rp) 2)))
    (lis::make-elliptic-orbit lis::+mu-earth+ a (/ (- ra rp) (+ ra rp)) (deg 90) (deg 270) 0d0
                              pi (lis::start-t))))

(defun rails-crossing (rails from to radius)
  "The time in [FROM, TO] at which the rails' radius falls through RADIUS,
by bisection down to adjacent doubles (~2.4e-7 s in 2040)."
  (loop for mid = (* 0.5d0 (+ from to))
        until (or (= mid from) (= mid to))
        do (if (> (lis::vec3-norm (rails-point rails mid)) radius)
               (setf from mid)
               (setf to mid)))
  (* 0.5d0 (+ from to)))

(define-test rails-crash-end
  (let ((t0 (lis::start-t)))
    (dolist (j2 '(nil t))
      (let* ((rails (lis::make-rails (k-17-orbit 90d3) t0 :j2 j2))
             (segment (first rails))
             (end (lis::rails-segment-end segment))
             (reference (rails-crossing rails (+ t0 2500) (+ t0 2700) *earth-crash-radius*)))
        (is eq :crash (lis::rails-segment-end-kind segment))
        (true (< (abs (- end reference)) 1d-6) "J2 ~A: ~,9F s off" j2 (- end reference))
        (if j2
            (false (< (abs (- end t0 2603.07d0)) 0.005d0))
            (true (< (abs (- end t0 2603.07d0)) 0.005d0) "~,3F s" (- end t0)))))
    (let ((segment (first (lis::make-rails (k-17-orbit (+ 100d3 1)) t0))))
      (is eq :horizon (lis::rails-segment-end-kind segment))
      (is = lis::*rails-horizon* (lis::rails-segment-end segment)))
    ;; A hyperbola: one two-body segment to the horizon.
    (let ((segment (first (lis::make-rails (lis::make-hyperbolic-orbit
                                            lis::+mu-earth+ 7378d3 1.5d0 (deg 30) 0d0 0d0
                                            -1d0 t0)
                                           t0))))
      (is eq :horizon (lis::rails-segment-end-kind segment))
      (is = 0d0 (lis::rails-segment-raan-rate segment))
      (is = (lis::orbit-n (lis::rails-segment-orbit segment))
          (lis::rails-segment-mean-rate segment)))))

(define-test object-state-past-crash
  (let* ((t0 (lis::start-t))
         (object (lis::new-object (test-object-def (lis::make-attitude :lvlh)
                                                   (lis::make-rails (k-17-orbit 90d3) t0))))
         (end (lis::rails-segment-end (first (lis::object-rails object))))
         (r (lis::make-vec3)) (v (lis::make-vec3)) (q (lis::make-quat)) (w (lis::make-vec3)))
    (true (lis::object-state-into! r v q w object end))
    (false (lis::object-state-into! r v q w object (+ end lis::+fine-step+)))
    (setf (lis::object-status object) :removed)
    (false (lis::object-state-into! r v q w object t0))))

(defun attitude-at (mode orbit time)
  "(values q ω-inertial ω-body) of MODE on ORBIT at TIME."
  (multiple-value-bind (r v) (lis::orbit-state orbit time)
    (let ((q (lis::make-quat)) (w (lis::make-vec3)))
      (lis::object-attitude-into! q w mode r v time)
      (values q w))))

(defun finite-difference-rate (mode orbit time dt)
  "ω (inertial) from q at TIME ± DT: 2 q̇ q*."
  (let* ((a (attitude-at mode orbit (- time dt)))
         (b (attitude-at mode orbit (+ time dt)))
         (b (if (minusp (loop for i below 4 sum (* (aref a i) (aref b i))))
                (map 'lis::quat #'- b)
                b))
         (q (attitude-at mode orbit time))
         (q (if (minusp (loop for i below 4 sum (* (aref a i) (aref q i))))
                (map 'lis::quat #'- q)
                q))
         (dq (map 'lis::quat (lambda (x y) (/ (- y x) (* 2 dt))) a b))
         (product (lis::quat-mul dq (lis::quat-conjugate q))))
    (lis::vec3 (* 2 (aref product 1)) (* 2 (aref product 2)) (* 2 (aref product 3)))))

(define-test attitude-modes-rates
  (let* ((time (+ (lis::start-t) 1234d0))
         (orbit (drift-orbit 0.3d0))
         (spin-q (lis::quat-normalize (lis::quat 0.3d0 -0.5d0 0.7d0 0.2d0)))
         (modes (list (lis::make-attitude :lvlh)
                      (lis::make-attitude :sun-pointing)
                      (lis::make-attitude :inertial :q spin-q)
                      (lis::make-attitude :spin :q spin-q
                                                :axis (lis::vec3-normalize (lis::vec3 1 2 -2))
                                                :rate (deg 0.3) :epoch (lis::start-t)))))
    (dolist (mode modes)
      (multiple-value-bind (q w) (attitude-at mode orbit time)
        (let ((miss (lis::vec3-norm (lis::vec3-sub w (finite-difference-rate mode orbit time (/ 1d0 64))))))
          (true (< miss 1d-9) "~A: ~,2E rad/s" (lis::attitude-kind mode) miss))
        (true (< (abs (- 1 (sqrt (loop for x across q sum (* x x))))) 1d-14))))
    ;; The frames' axes.
    (multiple-value-bind (r v) (lis::orbit-state orbit time)
      (let ((q (attitude-at (first modes) orbit time)))
        (true (< (rel-diff (lis::quat-rotate q (lis::vec3 0 0 1))
                           (lis::vec3-normalize (lis::vec3-scale r -1d0)))
                 1d-14))
        (true (plusp (lis::vec3-dot (lis::quat-rotate q (lis::vec3 1 0 0)) v))))
      (let* ((q (attitude-at (second modes) orbit time))
             (sun (lis::vec3-sub (lis::body-state :sun time) r))
             (south (lis::vec3 0 (sin lis::+obliquity+) (- (cos lis::+obliquity+)))))
        (true (< (rel-diff (lis::quat-rotate q (lis::vec3 1 0 0)) (lis::vec3-normalize sun))
                 1d-14))
        (true (< (abs (lis::vec3-dot (lis::quat-rotate q (lis::vec3 0 1 0)) south)) 1d-14))
        (true (plusp (lis::vec3-dot (lis::quat-rotate q (lis::vec3 0 0 1)) south)))))))

(defun rails-attitude-loop (object r v q w time n)
  (declare (type double-float time) (type fixnum n))
  (dotimes (i n)
    (lis::object-state-into! r v q w object (+ time (* 7d0 i)))))

(define-test rails-cons-nothing
  (let* ((t0 (lis::start-t))
         (r (lis::make-vec3)) (v (lis::make-vec3)) (q (lis::make-quat)) (w (lis::make-vec3))
         (objects (mapcar (lambda (mode)
                            (lis::new-object (test-object-def
                                              mode (lis::make-rails (k-17-orbit 300d3) t0))))
                          (list (lis::make-attitude :lvlh) (lis::make-attitude :sun-pointing)
                                (lis::make-attitude :inertial)
                                (lis::make-attitude :spin :rate 0.01d0))))
         (hyperbola (lis::new-object
                     (test-object-def (lis::make-attitude :lvlh)
                                      (lis::make-rails (lis::make-hyperbolic-orbit
                                                        lis::+mu-earth+ 7378d3 1.5d0 (deg 30)
                                                        0d0 0d0 -1d0 t0)
                                                       t0)))))
    (dolist (object (cons hyperbola objects))
      (rails-attitude-loop object r v q w t0 1000)
      (let ((consed (least-bytes-consed (lambda () (rails-attitude-loop object r v q w t0 1000)))))
        (true (<= consed 0) "~A: ~D bytes" (lis::attitude-kind (lis::object-attitude object))
              consed)))))
