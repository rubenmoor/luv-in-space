(in-package #:luv-in-space.tests)

;;; hazards.lisp: the swept crash test and death (docs/architecture.md
;;; "Testing" → "Crash detection"; decision 74: Crash test on the step's
;;; Hermite and the death state).

(defun crash-sim (r v &key (model :point) (time (lis::start-t)))
  "The start's sim moved to R, V at TIME: direct mode, no rotation."
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim)))
    (replace (lis::ship-state-r ship) r)
    (replace (lis::ship-state-v ship) v)
    (fill (lis::ship-state-w ship) 0d0)
    (setf (lis::sim-model sim) model
          (lis::sim-t sim) time
          (lis::controls-attitude-mode (lis::sim-controls sim)) :direct)
    sim))

(defun fly-until-lost (sim level end)
  "Steps of SIM at LEVEL until it is lost or reaches END; SIM."
  (loop until (or (lis::sim-lost-p sim) (>= (lis::sim-t sim) end))
        do (step-sim sim 1000 level end))
  sim)

(defun deg-diff (a b)
  "|a − b| in degrees of two angles in radians."
  (lis::rad->deg (abs (lis::wrap-signed (- a b)))))

(define-test crash-radial-free-fall
  ;; t = √(r₀³/(2μ)) (√(x(1 − x)) + arccos √x), x = r_c/r₀
  (let* ((r0 (+ lis::+radius-earth+ 1000d3))
         (x (/ *earth-crash-radius* r0))
         (expected (* (sqrt (/ (expt r0 3) (* 2 lis::+mu-earth+)))
                      (+ (sqrt (* x (- 1 x))) (acos (sqrt x))))))
    (dolist (level '(1 1000))
      (let* ((sim (fly-until-lost (crash-sim (lis::vec3 0 (- r0) 0) (lis::vec3 0 0 0))
                                  level (+ (lis::start-t) 1000)))
             (loss (lis::sim-loss sim)))
        (true loss)
        (when loss
          (is eq :crash (lis::loss-cause loss))
          (is eq :earth (lis::loss-body loss))
          (is = (lis::loss-time loss) (lis::sim-t sim))
          (true (< (abs (- (lis::loss-time loss) (lis::start-t) expected)) 1d-3)
                "×~D: ~,6F s, closed form ~,6F s" level
                (- (lis::loss-time loss) (lis::start-t)) expected)
          (true (< (abs (- (lis::vec3-norm (lis::ship-state-r (lis::sim-ship sim)))
                           *earth-crash-radius*))
                   1d-3)))))))

(defun dive-orbit (periapsis-radius &optional (time (lis::start-t)))
  "A polar orbit from 400 km apoapsis down to PERIAPSIS-RADIUS, at its
apoapsis at TIME."
  (let* ((ra (+ lis::+radius-earth+ 400d3))
         (a (* 0.5d0 (+ ra periapsis-radius)))
         (e (/ (- ra periapsis-radius) (+ ra periapsis-radius))))
    (lis::make-elliptic-orbit lis::+mu-earth+ a e (/ pi 2) (* 3/2 pi) (lis::deg->rad 30) pi
                              time)))

(defun orbit-sim (orbit &optional (time (lis::start-t)))
  (multiple-value-bind (r v) (lis::orbit-state orbit time)
    (crash-sim r v :time time)))

(define-test crash-deorbit
  ;; Periapsis 50 km below the crash radius, at the coarse step.
  (let* ((orbit (dive-orbit (- *earth-crash-radius* 50d3)))
         (expected (lis::orbit-radius-crossings orbit *earth-crash-radius* (lis::start-t)))
         (sim (fly-until-lost (orbit-sim orbit) 1000 (+ (lis::start-t) 20000)))
         (loss (lis::sim-loss sim)))
    (true loss)
    (when loss
      (true (< (lis::loss-time loss)
               (lis::orbit-next-apsis orbit (lis::start-t) :periapsis))
            "the first pass")
      (true (< (abs (- (lis::loss-time loss) expected)) 1d-2)
            "~,6F s from the Kepler crossing" (- (lis::loss-time loss) expected))
      (multiple-value-bind (latitude longitude)
          (lis::latitude-longitude :earth (lis::orbit-state orbit expected) expected)
        (true (< (deg-diff (lis::loss-latitude loss) latitude) 0.01d0))
        (true (< (deg-diff (lis::loss-longitude loss) longitude) 0.01d0)))
      (let ((vis-viva (sqrt (* lis::+mu-earth+
                               (- (/ 2 *earth-crash-radius*) (/ (lis::orbit-a orbit)))))))
        (true (< (abs (- (lis::loss-speed loss) vis-viva)) 1d-3)
              "~,6F m/s, vis-viva ~,6F m/s" (lis::loss-speed loss) vis-viva)))))

(define-test crash-grazing
  ;; Periapsis 1 m above the crash radius, two orbits. The fine run takes
  ;; the fine step within 300 s of each periapsis and the coarse step
  ;; between (the whole orbit at 1/64 s costs ~1 s); out there the crash
  ;; test only meets the chord bound.
  (let* ((orbit (dive-orbit (+ *earth-crash-radius* 1d0)))
         (end (+ (lis::start-t) (* 2 (lis::orbit-period orbit)))))
    (false (lis::sim-loss (fly-until-lost (orbit-sim orbit) 1000 end)) "coarse")
    (let ((sim (orbit-sim orbit)))
      (loop for periapsis = (lis::orbit-next-apsis orbit (lis::sim-t sim) :periapsis)
            for window = (+ (lis::start-t)
                            (lis::grid-ceiling-step (- periapsis (lis::start-t) 300)))
            while (< periapsis end)
            do (fly-until-lost sim 1000 window)
               (setf (lis::controls-attitude-mode (lis::sim-controls sim)) :direct)
               (fill (lis::ship-state-w (lis::sim-ship sim)) 0d0)
               (fly-until-lost sim 1 (+ window 600)))
      (fly-until-lost sim 1000 end)
      (false (lis::sim-loss sim) "fine")
      (is = end (lis::sim-t sim)))))

;;; The Moon: a ship 50,000 km out at 3 km/s relative, offset by B across
;;; the Moon's path, :full, from 2040-01-01.

(defparameter *moon-crash-radius* (lis::body-crash-radius (lis::body :moon)))

(defun moon-approach (b)
  (let ((time (lis::start-t)))
    (multiple-value-bind (mr mv) (lis::body-state :moon time)
      (let* ((u (lis::vec3-normalize mv))
             (n (lis::vec3-normalize (lis::vec3-cross mr mv)))
             (r (lis::vec3-add mr (lis::vec3-add (lis::vec3-scale u -5d7) (lis::vec3-scale n b))))
             (v (lis::vec3-add mv (lis::vec3-scale u 3000d0))))
        (crash-sim r v :model :full :time time)))))

(defun selenocentric (sim)
  "(values r v) of SIM's ship relative to the Moon."
  (let ((ship (lis::sim-ship sim)))
    (multiple-value-bind (mr mv) (lis::body-state :moon (lis::sim-t sim))
      (values (lis::vec3-sub (lis::ship-state-r ship) mr)
              (lis::vec3-sub (lis::ship-state-v ship) mv)))))

(defun moon-pass (b)
  "Fly the approach with offset B for 30,000 s or until lost: (values sim
q), q the osculating selenocentric periapsis radius at the step closest to
the Moon."
  (let ((sim (moon-approach b))
        (end (+ (lis::start-t) 30000))
        (closest nil) (q nil))
    (loop until (or (lis::sim-lost-p sim) (>= (lis::sim-t sim) end))
          do (step-sim sim 1 1000 end)
             (multiple-value-bind (r v) (selenocentric sim)
               (let ((d (lis::vec3-norm r)))
                 (when (or (null closest) (< d closest))
                   (setf closest d
                         q (lis::orbit-q (lis::state->orbit r v lis::+mu-moon+
                                                            (lis::sim-t sim))))))))
    (values sim q)))

(defun two-body-offset (q)
  "The offset b at 50,000 km and 3 km/s whose selenocentric hyperbola has
periapsis radius Q: b v = q √(2ε + 2μ/q)."
  (let* ((mu lis::+mu-moon+)
         (energy (- (/ (expt 3000d0 2) 2) (/ mu 5d7))))
    (/ (* q (sqrt (+ (* 2 energy) (/ (* 2 mu) q)))) 3000d0)))

(define-test crash-moon
  (let* ((sim (moon-approach (* 0.9d0 *moon-crash-radius*)))
         (loss (lis::sim-loss (fly-until-lost sim 1000 (+ (lis::start-t) 30000)))))
    (true loss)
    (when loss
      (is eq :moon (lis::loss-body loss))
      (multiple-value-bind (r v) (selenocentric sim)
        (true (< (abs (- (lis::vec3-norm r) *moon-crash-radius*)) 1d-3)
              "|p| − R ~,6F m" (- (lis::vec3-norm r) *moon-crash-radius*))
        (true (< (abs (- (lis::vec3-norm v) (lis::loss-speed loss))) 1d-3)))))
  ;; Aimed 1 km above: b shot so the flown pass's osculating periapsis is
  ;; R + 1 km within 1 m (Earth's tide puts the start's two-body aim 1.1 km
  ;; low, through the crash sphere: a crossing, caught).
  (let* ((target (+ *moon-crash-radius* 1000d0))
         (slope (/ (- (two-body-offset (+ target 1)) (two-body-offset (- target 1))) 2))
         (b (two-body-offset target))
         (sim nil) (q nil))
    (loop repeat 8
          do (multiple-value-setq (sim q) (moon-pass b))
          until (< (abs (- q target)) 1d0)
          do (incf b (* slope (- target q))))
    (true (< (abs (- q target)) 1d0) "periapsis ~,3F m above R" (- q *moon-crash-radius*))
    (false (lis::sim-loss sim))))

(define-test crash-death-stops
  (let* ((start (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3))))
         (checkpoint (lis::take-checkpoint start))
         (sim (fly-until-lost start 1000 (+ (lis::start-t) 20000)))
         (lost (lis::copy-sim sim)))
    (true (lis::sim-lost-p sim))
    (false (lis::checkpoint-safe-p sim))
    (false (lis::sim-step! sim 1))
    (false (lis::sim-step! sim 1000))
    (true (equalp lost sim))
    (lis::restore-checkpoint! sim checkpoint)
    (false (lis::sim-lost-p sim))
    (fly-until-lost sim 1000 (+ (lis::start-t) 20000))
    (true (equalp lost sim))))

(define-test crash-test-cons-nothing-near-the-moon
  ;; Falling from 5,000 km above the Moon, Earth-ward of it (so within the
  ;; Moon's least distance): its branch runs every step. 2,048 fine steps, so
  ;; 16 B a step fills a 32 KB region (`least-bytes-consed`).
  (flet ((run ()
           (multiple-value-bind (mr mv) (lis::body-state :moon (lis::start-t))
             (let ((sim (crash-sim (lis::vec3-add mr (lis::vec3-scale (lis::vec3-normalize mr) -5d6))
                                   mv :model :full)))
               (let ((consed (least-bytes-consed (lambda () (step-sim sim 2048 1)))))
                 (values consed sim))))))
    (run)
    (multiple-value-bind (consed sim) (run)
      (false (lis::sim-lost-p sim))
      (true (< consed 1024) "~D bytes" consed))))

;;; Re-entry and removal of objects (docs/architecture.md "Testing" →
;;; "Re-entry", "Removal of objects"; decision 46: Rails across spheres,
;;; hyperbolas and re-entry).

(defun k-17-sim (perigee-altitude)
  "The start's sim, two-body, Derelict K-17 on two-body rails of 800 ×
PERIGEE-ALTITUDE from apogee at the start; the ship far away."
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-model sim) :point
          (lis::object-rails (lis::sim-object sim :k-17))
          (lis::make-rails (k-17-orbit perigee-altitude) (lis::start-t) :j2 nil))
    (lis::sim-recheck-objects! sim)
    sim))

(defun k-17-crash-end (sim)
  (lis::rails-segment-end (first (lis::object-rails (lis::sim-object sim :k-17)))))

(defun k-17-promoted (perigee-altitude start)
  "K-17-SIM at START (a grid time), the ship on two-body 800 × 130 km from
the same apogee, 500 m off its plane (clear of K-17's shapes), K-17
promoted."
  (let ((sim (k-17-sim perigee-altitude)))
    (setf (lis::sim-t sim) start)
    (multiple-value-bind (r v)
        (rails-point (lis::make-rails (k-17-orbit 130d3) (lis::start-t) :j2 nil) start)
      (place-ship! sim (lis::vec3-add-scaled r (lis::vec3-normalize (lis::vec3-cross r v)) 500d0)
                   v))))

(defun removal-point (sim)
  "The closed form's crash point of K-17 on its rails: (latitude longitude)."
  (let ((end (k-17-crash-end sim)))
    (multiple-value-list
     (lis::latitude-longitude (lis::body :earth) (rails-point (lis::object-rails (lis::sim-object sim :k-17)) end)
                              end))))

(defun k-17-removal (sim) (lis::object-removal (lis::sim-object sim :k-17)))

(define-test reentry-on-rails
  (let* ((t0 (lis::start-t))
         (sim (k-17-sim 90d3))
         (end (k-17-crash-end sim))
         (point (removal-point sim))
         (reference (rails-crossing (lis::object-rails (lis::sim-object sim :k-17))
                                    (+ t0 2500) (+ t0 2700) *earth-crash-radius*)))
    (step-sim sim most-positive-fixnum 1000 (+ t0 2700))
    (let ((removal (k-17-removal sim)))
      (true (lis::object-removed-p (lis::sim-object sim :k-17)))
      (is eq :burned-up (lis::removal-cause removal))
      (is eq :earth (lis::removal-body removal))
      (is = end (lis::removal-time removal))
      (true (< (abs (- end reference)) 1d-6) "~,9F s off" (- end reference))
      (true (< (abs (- end t0 2603.07d0)) 0.005d0) "~,3F s" (- end t0))
      (is equal point (lis::removal-point removal)))
    ;; Its notice, once (decision 112).
    (is equal '((:removal "K-17 BURNED UP")) (lis::sim-notices sim))))

(define-test reentry-promoted
  ;; Two-body from apogee at the coarse step (×1,000) and the fine step.
  (let* ((probe (k-17-sim 90d3))
         (end (k-17-crash-end probe))
         (point (removal-point probe)))
    (dolist (level '(1000 1))
      (let ((sim (k-17-promoted 90d3 (lis::start-t))))
        (is equal '(:k-17) (promoted-keys sim))
        (step-sim sim most-positive-fixnum level (+ end 60))
        (false (lis::sim-lost-p sim))
        (let ((removal (k-17-removal sim)))
          (true removal "×~D" level)
          (is equal '((:removal "K-17 BURNED UP")) (lis::sim-notices sim) "×~D" level)
          (when removal
            (is eq :burned-up (lis::removal-cause removal))
            (true (< (abs (- (lis::removal-time removal) end)) 1d-2)
                  "×~D: ~,6F s off" level (- (lis::removal-time removal) end))
            (destructuring-bind (lat lon) (lis::removal-point removal)
              (true (< (deg-diff lat (first point)) 0.01d0) "×~D" level)
              (true (< (deg-diff lon (second point)) 0.01d0) "×~D" level))))))))

(define-test reentry-after-removal
  ;; No state, no promotion with the ship at the crash point, no new test.
  (let* ((sim (k-17-sim 90d3))
         (k-17 (lis::sim-object sim :k-17))
         (end (k-17-crash-end sim)))
    (step-sim sim most-positive-fixnum 1000 (+ (lis::start-t) 2700))
    (let ((removal (k-17-removal sim)))
      (true removal)
      (dolist (time (list end (lis::sim-t sim) lis::*rails-horizon*))
        (false (lis::object-state-into! (lis::make-vec3) (lis::make-vec3) (lis::make-quat)
                                        (lis::make-vec3) k-17 time)))
      (multiple-value-bind (r v) (rails-point (lis::object-rails k-17) end)
        ;; 50 km above the crash point.
        (place-ship! sim (lis::vec3-scale r (+ 1 (/ 50d3 *earth-crash-radius*))) v))
      (is eq :removed (lis::object-status k-17))
      (false (member :k-17 (promoted-keys sim)))
      (step-sim sim 100 1)
      (is eq :removed (lis::object-status k-17))
      (is eq removal (lis::object-removal k-17)))))

(define-test reentry-checkpoint
  ;; Restored before t_c, K-17 is back and burns up at the same t_c, on
  ;; rails and promoted.
  (dolist (make (list (lambda () (k-17-sim 90d3))
                      (lambda () (k-17-promoted 90d3 (lis::start-t)))))
    (let* ((sim (funcall make))
           (checkpoint (lis::take-checkpoint sim))
           (end (+ (lis::start-t) 2700)))
      (step-sim sim most-positive-fixnum 1000 end)
      (let ((flown (lis::copy-sim sim)))
        (true (k-17-removal flown))
        (lis::restore-checkpoint! sim checkpoint)
        (false (k-17-removal sim))
        (false (lis::object-removed-p (lis::sim-object sim :k-17)))
        (step-sim sim most-positive-fixnum 1000 end)
        (true (equalp sim flown))
        (is = (lis::removal-time (k-17-removal flown))
            (lis::removal-time (k-17-removal sim)))))))

(define-test reentry-perigee-above
  ;; Perigee 1 m above the crash radius: on rails past perigee, and
  ;; promoted at the fine step through it.
  (let* ((t0 (lis::start-t))
         (perigee (+ t0 (* 0.5d0 (lis::orbit-period (k-17-orbit (+ 100d3 1)))))))
    (let ((sim (k-17-sim (+ 100d3 1))))
      (step-sim sim most-positive-fixnum 1000 (+ perigee 100))
      (is eq :rails (lis::object-status (lis::sim-object sim :k-17))))
    (let ((sim (k-17-promoted (+ 100d3 1) (/ (floor (* 64 (- perigee 60))) 64d0))))
      (is equal '(:k-17) (promoted-keys sim))
      (step-sim sim most-positive-fixnum 1 (+ perigee 60))
      (is eq :promoted (lis::object-status (lis::sim-object sim :k-17)))
      (false (k-17-removal sim)))))

(define-test object-remove-delivered
  ;; A delivery's record has the crash's shape: time, body, cause, point.
  (let* ((sim (lis::new-game-sim))
         (pod (lis::sim-object sim :pod-14))
         (time (+ (lis::sim-t sim) 600)))
    (is eq pod (lis::object-remove! sim pod time :delivered '(:highport "PLANT")))
    (let ((removal (lis::object-removal pod)))
      (is eq :removed (lis::object-status pod))
      (true (typep removal 'lis::removal))
      (is = time (lis::removal-time removal))
      (is eq :earth (lis::removal-body removal))
      (is eq :delivered (lis::removal-cause removal))
      (is equal '(:highport "PLANT") (lis::removal-point removal))
      (false (lis::sim-notices sim)))
    (step-sim sim 10 1)
    (is eq :removed (lis::object-status pod))))

;;; Contact (docs/architecture.md "Testing" → "Contact"; decision 44:
;;; Contact, capture and berthing in the simulation, decision 113). A wet
;;; Mule, two-body, beside an object promoted by hand on a 500 km circular
;;; orbit at (r, 0, 0) moving along +y.

(defun contact-scene (key centre &key (v-rel (lis::vec3 0 0 0)) (q-rel (lis::quat 0 0 0 1))
                                      (q-o (lis::quat 1 0 0 0)) (w-o (lis::make-vec3))
                                      (m-p 28000d0))
  "The start's sim, :point, object KEY promoted by hand on the orbit (a
towable at Q-O and W-O, body axes; a berth object in its mode's attitude),
the ship with M-P kg of propellant turned Q-REL from the object's axes,
direct mode, at rest
relative to it but for V-REL: the object's CoM at CENTRE in ship axes from
R, moving at V-REL (ship axes) relative to the ship's CoM. (values sim
index)."
  (let* ((sim (lis::new-game-sim))
         (index (position key (lis::sim-objects sim) :key #'lis::object-key))
         (object (lis::sim-object sim key))
         (radius (+ lis::+radius-earth+ 500d3))
         (r-o (lis::vec3 radius 0 0))
         (v-o (lis::vec3 0 (sqrt (/ lis::+mu-earth+ radius)) 0))
         (q-o (lis::quat-copy q-o))
         (w-o (lis::vec3-copy w-o)))
    (unless (eq (lis::object-def-kind (lis::object-def-of object)) :towable)
      (let ((w (lis::make-vec3)))
        (lis::object-attitude-into! q-o w (lis::object-attitude object) r-o v-o (lis::sim-t sim))
        (setf w-o (lis::quat-rotate (lis::quat-conjugate q-o) w))))
    (let* ((q-s (lis::quat-mul q-o q-rel))
           (com (lis::mass-properties-com (lis::ship-mass-properties m-p nil)))
           (r-s (lis::vec3-sub r-o (lis::quat-rotate q-s (lis::vec3-sub centre com))))
           (v-s (lis::vec3-sub v-o (lis::quat-rotate q-s v-rel))))
      (setf (lis::sim-model sim) :point
            (lis::object-status object) :promoted)
      (lis::ship-state-assign! (lis::object-state object)
                               (lis::make-ship-state r-o v-o q-o w-o 0d0))
      (lis::ship-state-assign! (lis::sim-ship sim)
                               (lis::make-ship-state r-s v-s q-s (lis::make-vec3) m-p))
      (setf (lis::controls-attitude-mode (lis::sim-controls sim)) :direct)
      (lis::sim-recheck-objects! sim)
      (values sim index))))

(defparameter *yard-3-radius* 1.814d0)

(defun aft-scene (speed &key (gap 0.2d0) (key :skua-yard-3) (radius *yard-3-radius*))
  "A sphere GAP m behind the truss box's aft face (x −2.0 from R), 0.5 m
off its axis, closing at SPEED."
  (contact-scene key (lis::vec3 (- -2d0 gap radius) 0.5d0 0) :v-rel (lis::vec3 speed 0 0)))

(defun contact-count () (lis::contact-count lis::*contact*))

(defun step-to-contact (sim level end)
  "Steps of SIM at LEVEL until a contact is answered, it is lost or reaches
END: (values sim the state before the contact's step, or nil)."
  (loop until (or (lis::sim-lost-p sim) (>= (lis::sim-t sim) end))
        do (let ((before (lis::copy-sim sim))
                 (count (contact-count)))
             (lis::sim-step! sim level :until end)
             (unless (= count (contact-count))
               (return-from step-to-contact (values sim before)))))
  (values sim nil))

(defun record-normal-speed (sim)
  "The last contact's normal speed of its points in SIM now."
  (let ((contact lis::*contact*))
    (lis::contact-normal-speed sim (lis::contact-object contact) (lis::contact-normal contact)
                               (lis::contact-r-s contact) (lis::contact-r-o contact))))

(defun stack-momenta (sim index)
  "(values p L) of SIM's ship and INDEXth object: linear momentum, and
angular momentum about their common CoM, inertial."
  (let* ((ship (lis::sim-ship sim))
         (properties (lis::ship-mass-properties (lis::ship-state-m-p ship) nil))
         (state (lis::object-state (svref (lis::sim-objects sim) index)))
         (def (lis::object-def-of (svref (lis::sim-objects sim) index)))
         (bodies (list (list (lis::mass-properties-mass properties) ship
                             (lis::mass-properties-inertia properties))
                       (list (lis::object-def-mass def) state (lis::object-def-tensor def))))
         (mass (+ (first (first bodies)) (first (second bodies))))
         (cm (lis::vec3-scale (lis::vec3-add (lis::vec3-scale (lis::ship-state-r ship)
                                                              (first (first bodies)))
                                             (lis::vec3-scale (lis::ship-state-r state)
                                                              (first (second bodies))))
                              (/ mass)))
         (p (lis::make-vec3)) (l (lis::make-vec3)))
    (loop for (m s tensor) in bodies
          do (setf p (lis::vec3-add p (lis::vec3-scale (lis::ship-state-v s) m))
                   l (lis::vec3-add l (lis::vec3-add
                                       (lis::quat-rotate (lis::ship-state-q s)
                                                         (lis::mat3*vec3 tensor (lis::ship-state-w s)))
                                       (lis::vec3-scale (lis::vec3-cross
                                                         (lis::vec3-sub (lis::ship-state-r s) cm)
                                                         (lis::ship-state-v s))
                                                        m)))))
    (values p l)))

(defun vector-error (a b &optional (scale (lis::vec3-norm b)))
  (/ (lis::vec3-norm (lis::vec3-sub a b)) scale))

(define-test contact-bounce
  ;; The truss box's aft face into Skua Yard 3 (50 t) at 0.50 m/s, 0.5 m
  ;; off axis: answered on the step's start at the fine step.
  (multiple-value-bind (sim index) (aft-scene 0.5d0)
    (multiple-value-bind (sim before) (step-to-contact sim 1 (+ (lis::start-t) 2))
      (true before)
      (false (lis::sim-lost-p sim))
      (is = index (lis::contact-object lis::*contact*))
      ;; Off its rails: demoted on its own state's (task 267).
      (false (lis::object-disturbed (svref (lis::sim-objects before) index)))
      (true (lis::object-disturbed (svref (lis::sim-objects sim) index)))
      (is = (lis::sim-t before) (lis::sim-t sim))
      (true (on-grid-p (lis::sim-t sim)))
      (true (< (vector-error (lis::contact-normal lis::*contact*)
                               (lis::quat-rotate (lis::ship-state-q (lis::sim-ship sim))
                                                 (lis::vec3 -1 0 0)))
               1d-9))
      (let ((v0 (record-normal-speed before))
            (v1 (record-normal-speed sim)))
        (true (< (abs (+ v0 0.5d0)) 1d-3) "~F m/s before" v0)
        (true (< (abs (/ (+ v1 (* 0.3d0 v0)) v0)) 1d-9) "~F after ~F" v1 v0))
      ;; Momentum, the impulse alone in the object's frame (velocities of
      ;; 0.5 m/s, not 7.6 km/s, so their rounding stays below 1e-12).
      (let ((shifted (lis::copy-sim before))
            (v-o (lis::vec3-copy (lis::ship-state-v (lis::object-state (svref (lis::sim-objects before)
                                                                                index))))))
        (dolist (state (list (lis::sim-ship shifted)
                             (lis::object-state (svref (lis::sim-objects shifted) index))))
          (lis::vec3-sub! (lis::ship-state-v state) (lis::ship-state-v state) v-o))
        (multiple-value-bind (p0 l0) (stack-momenta shifted index)
          (let ((contact lis::*contact*))
            (true (lis::contact-impulse! shifted index (lis::contact-normal contact)
                                         (lis::contact-r-s contact) (lis::contact-r-o contact))))
          (multiple-value-bind (p1 l1) (stack-momenta shifted index)
            (true (< (vector-error p1 p0) 1d-12) "p ~,3E" (vector-error p1 p0))
            (true (< (vector-error l1 l0) 1d-12) "L ~,3E" (vector-error l1 l0)))
          (true (< (abs (/ (+ (record-normal-speed shifted) (* 0.3d0 (record-normal-speed before)))
                           (record-normal-speed before)))
                   1d-9))))
      ;; Apart after it: no second contact.
      (let ((count (contact-count)))
        (step-sim sim 128 1)
        (is = count (contact-count))
        (true (> (record-normal-speed sim) 0d0))))))

(define-test contact-bounce-coarse
  ;; At ×1,000 the step is redone to the grid point before t_c.
  (let ((sim (aft-scene 0.5d0 :gap 0.6d0)))
    (multiple-value-bind (sim before) (step-to-contact sim 1000 (+ (lis::start-t) 10))
      (true before)
      (false (lis::sim-lost-p sim))
      (true (on-grid-p (lis::sim-t sim)))
      (true (< (lis::sim-t before) (lis::sim-t sim) (lis::contact-time lis::*contact*)))
      (true (< (- (lis::contact-time lis::*contact*) (lis::sim-t sim)) (/ 1d0 64)))
      (true (< (abs (- (lis::contact-time lis::*contact*) (lis::sim-t before) 1.2d0)) 1d-3))
      (true (< 0.1d0 (record-normal-speed sim) 0.2d0) "~F m/s" (record-normal-speed sim)))))

(define-test contact-berth-immovable
  ;; The ship's aft face (x −2.0 from R) into Tern Station's +x end (17.58 m
  ;; from its CoM) at 0.5 m/s; the station's state is its own run's.
  (multiple-value-bind (sim index)
      (contact-scene :tern-station (lis::vec3 (- -2d0 0.2d0 17.58d0) 0 0)
                     :v-rel (lis::vec3 0.5d0 0 0) :q-rel (lis::quat 1 0 0 0))
    (let ((alone (lis::copy-sim sim))
          (end (+ (lis::start-t) 3)))
      (setf (lis::sim-excluded alone) index)
      (multiple-value-bind (sim before) (step-to-contact sim 1 end)
        (true before)
        (let ((v0 (record-normal-speed before))
              (v1 (record-normal-speed sim)))
          ;; The station's LVLH turn moves its face's point by ~1 mm/s.
          (true (< (abs (+ v0 0.5d0)) 3d-3) "~F m/s before" v0)
          (true (< (abs (- v1 0.15d0)) 1d-3) "~F m/s after" v1)
          (true (< (abs (/ (+ v1 (* 0.3d0 v0)) v0)) 1d-9)))
        (step-sim sim most-positive-fixnum 1 end)
        (step-sim alone most-positive-fixnum 1 end)
        (is = end (lis::sim-t sim))
        (true (equalp (lis::object-state (svref (lis::sim-objects alone) index))
                      (lis::object-state (svref (lis::sim-objects sim) index))))
        (false (lis::object-disturbed (svref (lis::sim-objects sim) index)))))))

(define-test contact-death-speed
  ;; 0.99 m/s bounces, 1.01 m/s kills at t_c, 0.2 m / 1.01 m/s after the start.
  (let ((sim (step-to-contact (aft-scene 0.99d0) 1 (+ (lis::start-t) 2))))
    (false (lis::sim-lost-p sim))
    (true (on-grid-p (lis::sim-t sim))))
  (multiple-value-bind (sim index) (aft-scene 1.01d0)
    (declare (ignore index))
    (step-to-contact sim 1 (+ (lis::start-t) 2))
    (let ((loss (lis::sim-loss sim)))
      (true loss)
      (when loss
        (is eq :contact (lis::loss-cause loss))
        (is eq :skua-yard-3 (lis::loss-object loss))
        (is = (lis::loss-time loss) (lis::sim-t sim))
        (true (< (abs (- (lis::loss-speed loss) 1.01d0)) 1d-4) "~F m/s" (lis::loss-speed loss))
        (true (< (abs (- (lis::loss-time loss) (lis::start-t) (/ 0.2d0 1.01d0))) 1d-4)
              "~,6F s" (- (lis::loss-time loss) (lis::start-t)))
        (false (lis::sim-step! sim 1))))))

(define-test contact-no-tunnelling
  ;; The cone's box (x 15.0–16.6 from R, ±0.8) 1.0 m into the path of Skua
  ;; Yard 6 (r 2.616 m) passing along +y at 100 m/s: caught at the fine
  ;; step, and in one coarse step of 2 s from 100 m away. First touch at
  ;; the box's edge, y = −0.8 − √(r² − (r − 1)²).
  (let* ((r 2.616d0)
         (y-touch (- -0.8d0 (sqrt (- (* r r) (expt (- r 1) 2))))))
    (loop for (level y0 bound) in '((1 -10d0 1d-4) (1000 -100d0 1d-3))
          do (let ((sim (contact-scene :skua-yard-6 (lis::vec3 (+ 16.6d0 r -1d0) y0 0)
                                       :v-rel (lis::vec3 0 100 0))))
               (when (= level 1000)
                 (is = 2d0 (lis::step-h sim level nil)))
               (step-to-contact sim level (+ (lis::start-t) 5))
               (let ((loss (lis::sim-loss sim)))
                 (true loss "×~D" level)
                 (when loss
                   (is eq :skua-yard-6 (lis::loss-object loss))
                   (true (< (abs (- (lis::loss-time loss) (lis::start-t) (/ (- y-touch y0) 100)))
                            bound)
                         "×~D: ~,6F s" level (- (lis::loss-time loss) (lis::start-t)))))))))

(define-test contact-touching-or-receding
  ;; Overlapping 1 mm at rest, and 1 cm receding at 0.05 m/s, radially
  ;; outward (the tide parts them): no impulse, the run its own without the
  ;; object.
  (loop for (gap speed) in '((-0.001d0 0d0) (-0.01d0 -0.05d0))
        do (multiple-value-bind (sim index) (aft-scene speed :gap gap)
             (let ((alone (lis::copy-sim sim))
                   (count (contact-count)))
               (setf (lis::sim-excluded alone) index)
               (step-sim sim 256 1)
               (step-sim alone 256 1)
               (is = count (contact-count))
               (setf (lis::sim-excluded alone) nil)
               (true (equalp alone sim) "~F m, ~F m/s" gap speed)))))

(define-test contact-cons-nothing
  ;; 10,000 fine steps beside a promoted object out of reach (1 km), and
  ;; 2,048 within reach, saved every step (30 m, at rest).
  (loop for (distance steps) in '((1000d0 10000) (30d0 2048))
        do (let ((sim (contact-scene :skua-yard-3 (lis::vec3 (- distance) 0 0))))
             (step-sim sim 10 1)
             (let ((consed (least-bytes-consed (lambda () (step-sim sim steps 1)))))
               (true (< consed 1024) "~F m: ~D bytes" distance consed)
               (false (lis::sim-lost-p sim))
               (is equal '(:skua-yard-3) (promoted-keys sim))))))

;;; Capture, the stack and berthing (docs/architecture.md "Testing" →
;;; "Contact", "Berthing"; decision 44, decision 112, decision 113). The
;;; grapple armed, a fixture straight ahead of the tip.

(defun fixture-scene (key &key (fixture 0) (gap 0.05d0) (speed 0.1d0) (offset 0d0) (angle 0d0)
                               (w-o (lis::make-vec3)) (m-p 28000d0) (armed t))
  "KEY's FIXTURE ahead of the tip: its plate GAP m beyond it along the
fixture's axis, the pin OFFSET m along ship y, its inward axis turned ANGLE
rad about ship z from +x, closing at SPEED along ship x; M-P kg of
propellant; the grapple ARMED (else stowed). (values sim index)."
  (let* ((def (lis::object-definition key))
         (f (nth fixture (lis::object-def-fixtures def)))
         (q-l (lis::quat-mul (lis::quat-from-axis-angle (lis::vec3 0 0 1) angle)
                             (lis::stack-load-q (lis::stack-design-pose def f))))
         (axis (lis::quat-rotate q-l (lis::fixture-axis f)))
         (pin (lis::vec3-add (lis::vec3-add-scaled lis::*grapple-tip* axis (- gap))
                             (lis::vec3 0 offset 0)))
         (centre (lis::vec3-sub pin (lis::quat-rotate q-l (lis::fixture-pin f)))))
    (multiple-value-bind (sim index)
        (contact-scene key centre :v-rel (lis::vec3 (- speed) 0 0)
                                  :q-rel (lis::quat-conjugate q-l) :w-o w-o :m-p m-p)
      (when armed
        (setf (lis::grapple-state (lis::sim-grapple sim)) :armed))
      (values sim index))))

(defun shifted-sim (sim index)
  "A copy of SIM with the ship's and the INDEXth object's r and v less the
object's: momenta's rounding at metres and cm/s, not 7,000 km and 7.6 km/s."
  (let* ((copy (lis::copy-sim sim))
         (object (lis::object-state (svref (lis::sim-objects copy) index)))
         (r (lis::vec3-copy (lis::ship-state-r object)))
         (v (lis::vec3-copy (lis::ship-state-v object))))
    (dolist (state (list (lis::sim-ship copy) object) copy)
      (lis::vec3-sub! (lis::ship-state-r state) (lis::ship-state-r state) r)
      (lis::vec3-sub! (lis::ship-state-v state) (lis::ship-state-v state) v))))

(defun stack-load (sim)
  (let ((grapple (lis::sim-grapple sim)))
    (lis::make-stack-load (lis::object-def-of (svref (lis::sim-objects sim)
                                                     (lis::grapple-object grapple)))
                          (lis::grapple-com grapple) (lis::grapple-q grapple))))

(defun stack-energy-momenta (sim)
  "(values p L E) of SIM's latched stack, L about its CoM."
  (let* ((ship (lis::sim-ship sim))
         (properties (lis::ship-mass-properties (lis::ship-state-m-p ship) (stack-load sim)))
         (m (lis::mass-properties-mass properties))
         (w (lis::ship-state-w ship))
         (iw (lis::mat3*vec3 (lis::mass-properties-inertia properties) w)))
    (values (lis::vec3-scale (lis::ship-state-v ship) m)
            (lis::quat-rotate (lis::ship-state-q ship) iw)
            (* 0.5d0 (+ (* m (lis::vec3-dot (lis::ship-state-v ship) (lis::ship-state-v ship)))
                        (lis::vec3-dot w iw))))))

(defun two-body-energy (sim index)
  (let* ((ship (lis::sim-ship sim))
         (properties (lis::ship-mass-properties (lis::ship-state-m-p ship) nil))
         (object (svref (lis::sim-objects sim) index))
         (def (lis::object-def-of object))
         (state (lis::object-state object)))
    (flet ((energy (m v tensor w)
             (* 0.5d0 (+ (* m (lis::vec3-dot v v)) (lis::vec3-dot w (lis::mat3*vec3 tensor w))))))
      (+ (energy (lis::mass-properties-mass properties) (lis::ship-state-v ship)
                 (lis::mass-properties-inertia properties) (lis::ship-state-w ship))
         (energy (lis::object-def-mass def) (lis::ship-state-v state) (lis::object-def-tensor def)
                 (lis::ship-state-w state))))))

(defun load-follows-error (sim)
  "m: the latched load's object-state-into! from where the stack puts it."
  (let* ((ship (lis::sim-ship sim))
         (load (stack-load sim))
         (com (lis::mass-properties-com (lis::ship-mass-properties (lis::ship-state-m-p ship) load)))
         (expected (lis::vec3-add (lis::ship-state-r ship)
                                  (lis::quat-rotate (lis::ship-state-q ship)
                                                    (lis::vec3-sub (lis::stack-load-com load) com))))
         (r (lis::make-vec3)))
    (true (lis::object-state-into! r (lis::make-vec3) (lis::make-quat) (lis::make-vec3)
                                   (svref (lis::sim-objects sim)
                                          (lis::grapple-object (lis::sim-grapple sim)))
                                   (lis::sim-t sim)))
    (lis::vec3-norm (lis::vec3-sub r expected))))

(define-test capture-inside-tolerance
  ;; Skua Yard 3 (50 t) at 0.10 m/s, 0.1 m off, 5°: latched at the touching
  ;; step's start; the stack's properties are those of its pose then.
  (multiple-value-bind (sim index) (fixture-scene :skua-yard-3 :offset 0.1d0
                                                              :angle (lis::deg->rad 5d0))
    (let ((reading (lis::make-grapple-reading)))
      (true (lis::grapple-range-into! reading sim))
      (true (and (lis::grapple-reading-closing-ok reading) (lis::grapple-reading-offset-ok reading)
                 (lis::grapple-reading-angle-ok reading) (lis::grapple-reading-rotation-ok reading))))
    (multiple-value-bind (sim before) (step-to-contact sim 1 (+ (lis::start-t) 2))
      (true before)
      (false (lis::sim-lost-p sim))
      (is = (lis::sim-t before) (lis::sim-t sim))
      (let* ((grapple (lis::sim-grapple sim))
             (ship (lis::sim-ship before))
             (state (lis::object-state (svref (lis::sim-objects before) index)))
             (q-s* (lis::quat-conjugate (lis::ship-state-q ship)))
             (expected (lis::make-stack-load
                        (lis::object-definition :skua-yard-3)
                        (lis::vec3-add (lis::mass-properties-com
                                        (lis::ship-mass-properties (lis::ship-state-m-p ship) nil))
                                       (lis::quat-rotate q-s* (lis::vec3-sub (lis::ship-state-r state)
                                                                             (lis::ship-state-r ship))))
                        (lis::quat-mul q-s* (lis::ship-state-q state))))
             (a (lis::ship-mass-properties 28000d0 expected))
             (b (lis::ship-mass-properties 28000d0 (stack-load sim))))
        (is eq :closing (lis::grapple-state grapple))
        (is = index (lis::grapple-object grapple))
        (is = 0 (lis::grapple-fixture grapple))
        (is = (+ (lis::sim-t sim) 2) (lis::grapple-closing-end grapple))
        (is eq :latched (lis::object-status (svref (lis::sim-objects sim) index)))
        (is equal '((:grapple "LATCHED SKUA YARD 3")) (lis::sim-notices sim))
        (is = 85961d0 (lis::mass-properties-mass b))
        (true (< (vector-error (lis::mass-properties-com b) (lis::mass-properties-com a) 1d0) 1d-9))
        (true (< (/ (reduce #'max (map 'vector (lambda (x y) (abs (- x y)))
                                       (lis::mass-properties-inertia b) (lis::mass-properties-inertia a)))
                    (reduce #'max (lis::mass-properties-inertia a)))
                 1d-9))
        (is equal "RELEASE LOCKED: CLAWS CLOSING" (lis::grapple-key! sim))
        (is eq :closing (lis::grapple-state grapple))
        ;; The load follows the stack, latched after 2 s, not demoted.
        (true (< (load-follows-error sim) 1d-9))
        (step-sim sim 127 1)
        (is eq :closing (lis::grapple-state grapple))
        (step-sim sim 1 1)
        (is eq :latched (lis::grapple-state grapple))
        (step-sim sim 1000 1)
        (true (< (load-follows-error sim) 1d-9) "~,3E m" (load-follows-error sim))
        (step-sim sim 100 1000)
        (true (< (load-follows-error sim) 1d-9) "~,3E m" (load-follows-error sim))
        (is eq :latched (lis::object-status (svref (lis::sim-objects sim) index)))
        (false (lis::sim-lost-p sim))))))

(define-test capture-momentum
  ;; 0.2 m off at 0.25 m/s, both turning at 0.3°/s about the fixture's axis:
  ;; the coupling keeps p and L (about the system's CoM), loses energy.
  (let ((w (lis::vec3 (lis::deg->rad 0.3d0) 0 0)))
    (multiple-value-bind (sim index) (fixture-scene :skua-yard-3 :speed 0.25d0 :offset 0.2d0
                                                                 :w-o w)
      (let* ((ship (lis::sim-ship sim))
             (state (lis::object-state (svref (lis::sim-objects sim) index))))
        (replace (lis::ship-state-w ship)
                 (lis::quat-rotate (lis::quat-conjugate (lis::ship-state-q ship))
                                   (lis::quat-rotate (lis::ship-state-q state) w))))
      (multiple-value-bind (sim before) (step-to-contact sim 1 (+ (lis::start-t) 2))
        (true before)
        (is eq :closing (lis::grapple-state (lis::sim-grapple sim)))
        (let ((shifted (shifted-sim before index)))
          (multiple-value-bind (p0 l0) (stack-momenta shifted index)
            (let ((e0 (two-body-energy shifted index)))
              (lis::grapple-capture! shifted index 0)
              (true (lis::object-disturbed (svref (lis::sim-objects shifted) index)))
              (multiple-value-bind (p1 l1 e1) (stack-energy-momenta shifted)
                (true (< (vector-error p1 p0) 1d-12) "p ~,3E" (vector-error p1 p0))
                (true (< (vector-error l1 l0) 1d-12) "L ~,3E" (vector-error l1 l0))
                (true (< e1 e0) "~F J, ~F J before" e1 e0)))))))))

(define-test capture-missed
  ;; One tolerance just outside each, too slow, and 150 t: a bounce along
  ;; the fixture's axis with its message, no capture.
  (loop for (key args message) in
        `((:skua-yard-3 (:speed 0.31d0) "NO CAPTURE: TOO FAST")
          (:skua-yard-3 (:speed 0.005d0 :gap 0.005d0) "NO CAPTURE: TOO SLOW")
          (:skua-yard-3 (:offset 0.31d0) "NO CAPTURE: OFF AXIS")
          (:skua-yard-3 (:angle ,(lis::deg->rad 10.5d0)) "NO CAPTURE: MISALIGNED")
          (:skua-yard-3 (:w-o ,(lis::vec3 (lis::deg->rad 0.6d0) 0 0)) "NO CAPTURE: ROTATING")
          (:skua-yard-6 () "LOAD 150 T OVER 100 T RATING"))
        do (multiple-value-bind (sim index) (apply #'fixture-scene key args)
             (multiple-value-bind (sim before) (step-to-contact sim 1 (+ (lis::start-t) 3))
               (true before message)
               (false (lis::sim-lost-p sim) message)
               (is equal `((:grapple ,message)) (lis::sim-notices sim))
               (is eq :armed (lis::grapple-state (lis::sim-grapple sim)) message)
               (is eq :promoted (lis::object-status (svref (lis::sim-objects sim) index)) message)
               (true (on-grid-p (lis::sim-t sim)))
               (let* ((state (lis::object-state (svref (lis::sim-objects sim) index)))
                      (fixture (first (lis::object-def-fixtures (lis::object-definition key))))
                      (axis (lis::quat-rotate (lis::ship-state-q state) (lis::fixture-axis fixture))))
                 (true (< (vector-error (lis::contact-normal lis::*contact*) (lis::vec3-scale axis -1d0))
                          1d-12)
                       message))
               (let ((v0 (record-normal-speed before))
                     (v1 (record-normal-speed sim)))
                 (true (< (abs (/ (+ v1 (* 0.3d0 v0)) v0)) 1d-9) "~A: ~F after ~F" message v1 v0))
               (let ((count (contact-count)))
                 (step-sim sim 64 1)
                 (is = count (contact-count) message))))))

(defun berth-pose-error (sim p q-rel)
  "(values m rad): SIM's ship's R and axes from the station-relative pose P,
Q-REL."
  (let* ((ship (lis::sim-ship sim))
         (station (lis::object-state (svref (lis::sim-objects sim)
                                            (lis::grapple-object (lis::sim-grapple sim)))))
         (com (lis::mass-properties-com (lis::ship-mass-properties (lis::ship-state-m-p ship) nil)))
         (q-st* (lis::quat-conjugate (lis::ship-state-q station)))
         (here (lis::quat-rotate q-st* (lis::vec3-sub (lis::vec3-sub (lis::ship-state-r ship)
                                                                     (lis::quat-rotate (lis::ship-state-q ship) com))
                                                      (lis::ship-state-r station))))
         (e (lis::quat-mul (lis::quat-conjugate q-rel) (lis::quat-mul q-st* (lis::ship-state-q ship)))))
    (values (lis::vec3-norm (lis::vec3-sub here p))
            (* 2 (asin (min 1d0 (lis::vec3-norm (lis::vec3 (aref e 1) (aref e 2) (aref e 3)))))))))

(define-test berth-tern-station
  ;; Berthed at Tern Station's fixture, then one orbit at ×1,000: the pose
  ;; held, the throttle and the keys changing nothing.
  (multiple-value-bind (sim index) (fixture-scene :tern-station)
    (step-to-contact sim 1 (+ (lis::start-t) 2))
    (let ((grapple (lis::sim-grapple sim))
          (controls (lis::sim-controls sim)))
      (true (lis::sim-berthed sim))
      (is = index (lis::grapple-object grapple))
      (is eq :closing (lis::grapple-state grapple))
      (is equal '((:grapple "BERTHED TERN STATION")) (lis::sim-notices sim))
      (true (lis::sim-checkpoint-request sim))
      (is = 0d0 (lis::controls-throttle controls))
      (is eq :rate-damping (lis::controls-attitude-mode controls))
      (false (lis::sim-transfer sim))
      (is eq :promoted (lis::object-status (svref (lis::sim-objects sim) index)))
      ;; The ship moves with the fixture's point: no relative motion there.
      (let ((reading (lis::make-grapple-reading)))
        (lis::fixture-reading-into! reading sim index 0
                                    (lis::mass-properties-com (lis::ship-mass-properties 28000d0 nil)))
        (true (< (abs (lis::grapple-reading-closing-speed reading)) 1d-9))
        (true (< (lis::grapple-reading-rotation reading) 1d-12)))
      (let* ((p (lis::vec3-copy (lis::grapple-com grapple)))
             (q-rel (lis::quat-copy (lis::grapple-q grapple)))
             (keyed (lis::copy-sim sim))
             (end (+ (lis::sim-t sim) (lis::orbit-period
                                       (lis::state->orbit (lis::ship-state-r (lis::sim-ship sim))
                                                          (lis::ship-state-v (lis::sim-ship sim))
                                                          lis::+mu-earth+ (lis::sim-t sim)))))
             (worst-m 0d0) (worst-rad 0d0))
        (multiple-value-bind (m rad) (berth-pose-error sim p q-rel)
          (true (< m 1d-6))
          (true (< rad 1d-9)))
        (let ((c (lis::sim-controls keyed)))
          (lis::controls-throttle-full c)
          (lis::controls-set-rotation-key c :pitch 1)
          (lis::controls-set-translation-key c :x 1))
        (loop until (>= (lis::sim-t sim) end)
              do (step-sim sim 10 1000 end)
                 (multiple-value-bind (m rad) (berth-pose-error sim p q-rel)
                   (setf worst-m (max worst-m m) worst-rad (max worst-rad rad))))
        (step-sim keyed most-positive-fixnum 1000 end)
        (true (< worst-m 1d-6) "~,3E m" worst-m)
        (true (< worst-rad 1d-9) "~,3E rad" worst-rad)
        (is eq :latched (lis::grapple-state grapple))
        (false (lis::sim-lost-p sim))
        (is = 28000d0 (lis::ship-state-m-p (lis::sim-ship sim)))
        (true (equalp (lis::sim-ship sim) (lis::sim-ship keyed)))
        (true (equalp (lis::sim-objects sim) (lis::sim-objects keyed)))))))

(define-test stack-cons-nothing
  ;; 2,048 fine steps latched to Skua Yard 3, and berthed at Tern Station.
  (dolist (key '(:skua-yard-3 :tern-station))
    (let ((sim (fixture-scene key)))
      (step-to-contact sim 1 (+ (lis::start-t) 2))
      (is eq :closing (lis::grapple-state (lis::sim-grapple sim)) "~A" key)
      (step-sim sim 200 1)
      (let ((consed (least-bytes-consed (lambda () (step-sim sim 2048 1)))))
        (true (< consed 1024) "~A: ~D bytes" key consed))
      (false (lis::sim-lost-p sim)))))

;;; Release, the excluded object, propellant transfer and the berth's
;;; checkpoint (docs/architecture.md "Testing" → "Contact", "Berthing";
;;; decision 112, decision 113, decision 26: Refuelling and economy).

(defun attached-scene (key)
  "KEY's fixture grappled and the claws latched (berthed at a berth
object), the notices cleared: (values sim index)."
  (multiple-value-bind (sim index) (fixture-scene key)
    (step-to-contact sim 1 (+ (lis::start-t) 2))
    (step-sim sim 200 1)
    (assert (eq :latched (lis::grapple-state (lis::sim-grapple sim))))
    (setf (lis::sim-notices sim) nil)
    (values sim index)))

(defun inertial-w (state)
  (lis::quat-rotate (lis::ship-state-q state) (lis::ship-state-w state)))

(define-test release-towable
  ;; The stack turning at 0.31°/s off its principal axes: both bodies take
  ;; its motion at their CoMs and share ω; p and L kept.
  (multiple-value-bind (sim index) (attached-scene :skua-yard-3)
    (replace (lis::ship-state-w (lis::sim-ship sim)) (lis::vec3 0.004d0 0.003d0 -0.002d0))
    (lis::latched-load-update! sim)
    (let* ((sim (shifted-sim sim index))
           (ship (lis::sim-ship sim))
           (object (svref (lis::sim-objects sim) index))
           (state (lis::object-state object))
           (r (lis::vec3-copy (lis::ship-state-r ship)))
           (v (lis::vec3-copy (lis::ship-state-v ship)))
           (w (inertial-w ship)))
      (multiple-value-bind (p0 l0) (stack-energy-momenta sim)
        (false (lis::grapple-key! sim))
        (is eq :armed (lis::grapple-state (lis::sim-grapple sim)))
        (false (lis::grapple-object (lis::sim-grapple sim)))
        (is eq :promoted (lis::object-status object))
        (is = index (lis::sim-excluded sim))
        (dolist (body (list ship state))
          (let ((expected (lis::vec3-add v (lis::vec3-cross w (lis::vec3-sub (lis::ship-state-r body) r)))))
            (true (< (vector-error (lis::ship-state-v body) expected) 1d-12)
                  "v ~,3E" (vector-error (lis::ship-state-v body) expected))
            (true (< (vector-error (inertial-w body) w) 1d-12))))
        (multiple-value-bind (p1 l1) (stack-momenta sim index)
          (true (< (vector-error p1 p0) 1d-12) "p ~,3E" (vector-error p1 p0))
          (true (< (vector-error l1 l0) 1d-12) "L ~,3E" (vector-error l1 l0)))))))

(define-test release-refused
  (let* ((sim (attached-scene :skua-yard-3))
         (w (lis::ship-state-w (lis::sim-ship sim))))
    (replace w (lis::vec3 0 0 (lis::deg->rad 0.6d0)))
    (is equal "RELEASE REFUSED: ROTATING" (lis::grapple-key! sim))
    (replace w (lis::vec3 0 0 (lis::deg->rad 0.4d0)))
    (lis::controls-throttle-full (lis::sim-controls sim))
    (is equal "RELEASE REFUSED: ENGINE ON" (lis::grapple-key! sim))
    (is eq :latched (lis::grapple-state (lis::sim-grapple sim)))
    (lis::controls-throttle-cut (lis::sim-controls sim))
    (false (lis::grapple-key! sim))
    (is eq :armed (lis::grapple-state (lis::sim-grapple sim)))))

(define-test release-exclusion
  ;; Released, stowed, pushed 0.3 m into the cone and closing: no contact
  ;; while the shapes overlap; receding, excluded until they part; closing
  ;; again, contact.
  (multiple-value-bind (sim index) (attached-scene :skua-yard-3)
    (false (lis::grapple-key! sim))
    (false (lis::grapple-key! sim))
    (is eq :stowed (lis::grapple-state (lis::sim-grapple sim)))
    (let* ((ship (lis::sim-ship sim))
           (state (lis::object-state (svref (lis::sim-objects sim) index)))
           (count (contact-count)))
      (flet ((along-x (speed)
               (lis::quat-rotate (lis::ship-state-q ship) (lis::vec3 speed 0 0)))
             (relative-v! (speed)
               (replace (lis::ship-state-v state)
                        (lis::vec3-add (lis::ship-state-v ship)
                                       (lis::quat-rotate (lis::ship-state-q ship)
                                                         (lis::vec3 speed 0 0))))))
        (lis::vec3-sub! (lis::ship-state-r state) (lis::ship-state-r state) (along-x 0.3d0))
        (relative-v! -0.02d0)
        (step-sim sim 128 1)
        (is = index (lis::sim-excluded sim))
        (relative-v! 0.2d0)
        (loop repeat 640 while (lis::sim-excluded sim) do (step-sim sim 1 1))
        (false (lis::sim-excluded sim))
        (is = count (contact-count))
        (step-sim sim 16 1)
        (relative-v! -0.1d0)
        (multiple-value-bind (sim before) (step-to-contact sim 1 (+ (lis::sim-t sim) 5))
          (true before)
          (is = index (lis::contact-object lis::*contact*))
          (false (lis::sim-lost-p sim)))))))

(define-test release-berth
  ;; Released from Tern Station: the station's motion at the ship's CoM;
  ;; coasting 100 s the ship leaves the held pose's path only by the tide,
  ;; at most 3 n² d t² / 2 (Hill: the held point is not a free path), under
  ;; 1 cm for 10 s.
  (multiple-value-bind (sim index) (attached-scene :tern-station)
    (let ((held (lis::copy-sim sim))
          (station (lis::object-state (svref (lis::sim-objects sim) index)))
          (ship (lis::sim-ship sim))
          (t0 (lis::sim-t sim)))
      (false (lis::grapple-key! sim))
      (false (lis::sim-berthed sim))
      (is eq :armed (lis::grapple-state (lis::sim-grapple sim)))
      (is = index (lis::sim-excluded sim))
      (is eq :rate-damping (lis::controls-attitude-mode (lis::sim-controls sim)))
      (let* ((x (lis::vec3-sub (lis::ship-state-r ship) (lis::ship-state-r station)))
             (expected (lis::vec3-add (lis::ship-state-v station)
                                      (lis::vec3-cross (inertial-w station) x)))
             (d (lis::vec3-norm x))
             (n2 (/ lis::+mu-earth+ (expt (lis::vec3-norm (lis::ship-state-r ship)) 3)))
             (worst 0d0))
        (true (< (vector-error (lis::ship-state-v ship) expected) 1d-12))
        (loop with end = (+ t0 100)
              until (>= (lis::sim-t sim) end)
              do (lis::sim-step! sim 10 :until end)
                 (step-sim held 100 10 (lis::sim-t sim))
                 (let ((dt (- (lis::sim-t sim) t0))
                       (e (lis::vec3-norm (lis::vec3-sub (lis::ship-state-r ship)
                                                         (lis::ship-state-r (lis::sim-ship held))))))
                   (setf worst (max worst (/ e (* 1.5d0 n2 d dt dt))))
                   (when (<= dt 10)
                     (true (< e 0.01d0) "~,3E m at ~F s" e dt))))
        (true (< worst 1d0) "~,3F of the tide's bound" worst)
        (false (lis::sim-lost-p sim))))))

(define-test transfer-fulmar
  ;; Berthed at Fulmar Depot: 10 kg/s exactly to 28,000 kg, then off with
  ;; TANKS FULL once; the same at ×1,000; off at the release.
  (let ((sim (attached-scene :fulmar-depot))
        (ship nil))
    (setf ship (lis::sim-ship sim))
    (loop for (level m0 end) in '((1 27990d0 1) (1000 20000d0 800))
          do (setf (lis::ship-state-m-p ship) m0
                   (lis::sim-notices sim) nil)
             (false (lis::transfer-key! sim))
             (true (lis::sim-transfer sim))
             (let ((t0 (lis::sim-t sim)))
               (loop repeat 100000 while (lis::sim-transfer sim)
                     do (lis::sim-step! sim level)
                        (true (on-grid-p (lis::sim-t sim)))
                        (is = (min 28000d0 (+ m0 (* 10 (- (lis::sim-t sim) t0))))
                            (lis::ship-state-m-p ship)))
               (is = 28000d0 (lis::ship-state-m-p ship))
               (true (>= (- (lis::sim-t sim) t0) end))
               (step-sim sim 4 level)
               (is = 28000d0 (lis::ship-state-m-p ship))
               (is equal '((:transfer "TANKS FULL")) (lis::sim-notices sim) "×~D" level)))
    (true (lis::sim-berthed sim))
    (setf (lis::ship-state-m-p ship) 27000d0)
    (false (lis::transfer-key! sim))
    (false (lis::grapple-key! sim))
    (false (lis::sim-transfer sim))
    (step-sim sim 64 1)
    (true (<= (lis::ship-state-m-p ship) 27000d0) "rate damping's RCS only")
    (is equal "NOT BERTHED" (lis::transfer-key! sim)))
  (is equal "NO PROPELLANT HERE" (lis::transfer-key! (attached-scene :tern-station)))
  (is equal "NOT BERTHED" (lis::transfer-key! (lis::new-game-sim))))

(define-test berth-checkpoint
  ;; Berthing at Fulmar Depot under a clock takes the latest checkpoint; a
  ;; contact death after the release, then Return's restart, restores it.
  (multiple-value-bind (sim index) (fixture-scene :fulmar-depot)
    (let ((clock (clock-at sim 1)))
      (loop repeat 256 until (lis::sim-berthed sim) do (lis::clock-step! clock sim))
      (true (lis::sim-berthed sim))
      (true (lis::clock-checkpoint clock))
      (true (equalp sim (lis::clock-checkpoint clock)))
      (true (member "BERTHED FULMAR DEPOT" (lis::clock-messages clock) :test #'equal))
      (let ((checkpoint (lis::copy-sim (lis::clock-checkpoint clock))))
        (loop repeat 200 do (lis::clock-step! clock sim))
        (false (lis::grapple-key! sim))
        (let* ((ship (lis::sim-ship sim))
               (to (lis::vec3-sub (lis::ship-state-r (lis::object-state (svref (lis::sim-objects sim)
                                                                               index)))
                                  (lis::ship-state-r ship))))
          (lis::vec3-add-scaled! (lis::ship-state-v ship) (lis::ship-state-v ship) to
                                 (/ 5d0 (lis::vec3-norm to))))
        (loop repeat 2000 until (lis::sim-lost-p sim) do (lis::clock-step! clock sim))
        (true (lis::sim-lost-p sim))
        (is eq :contact (lis::loss-cause (lis::sim-loss sim)))
        (lis::clock-restart! clock sim (lis::clock-checkpoint clock))
        (true (equalp sim checkpoint))
        (true (lis::sim-berthed sim))))))

;;; Deliveries (docs/architecture.md "Testing" → "Delivery", "Removal of
;;; objects"; decision 53: Deliveries, the stranded offer, checkpoint floors
;;; and Skua Yard's layout). Skua Yard 3 latched to a wet Mule (radially
;;; above it), Highport promoted by hand ahead of it along the track, its
;;; plant's pin DISTANCE m from the load, the load falling back at SPEED.

(defun along-track (state)
  (let* ((r (lis::ship-state-r state))
         (n (lis::vec3-normalize (lis::vec3-cross r (lis::ship-state-v state)))))
    (lis::vec3-normalize (lis::vec3-cross n r))))

(defun delivery-scene (distance speed)
  "(values sim load-index highport-index), released, the notices cleared."
  (multiple-value-bind (sim index) (attached-scene :skua-yard-3)
    (let* ((highport (lis::sim-object sim :highport))
           (h-index (position highport (lis::sim-objects sim)))
           (load (lis::object-state (svref (lis::sim-objects sim) index)))
           (u (along-track load))
           (up (lis::vec3-normalize (lis::ship-state-r load)))
           (q (lis::quat-from-basis u up (lis::vec3-cross u up)))
           (pin (lis::fixture-pin (first (lis::object-def-fixtures (lis::object-def-of highport)))))
           (r (lis::vec3-sub (lis::vec3-add-scaled (lis::ship-state-r load) u distance)
                             (lis::quat-rotate q pin)))
           (v (lis::vec3-add-scaled (lis::ship-state-v load) u speed)))
      (setf (lis::object-status highport) :promoted)
      (lis::ship-state-assign! (lis::object-state highport)
                               (lis::make-ship-state r v q (lis::make-vec3) 0d0))
      (false (lis::grapple-key! sim))
      (setf (lis::sim-notices sim) nil)
      (values sim index h-index))))

(defun delivered-notices (sim)
  (count :delivery (lis::sim-notices sim) :key #'first))

(define-test delivery-removed
  ;; 80 m out at 0.04 m/s: removed at release + 600 s exactly, at the fine
  ;; step and at ×1,000, with the notice once.
  (let ((times '()))
    (dolist (level '(1 1000))
      (multiple-value-bind (sim index h-index) (delivery-scene 80d0 0.04d0)
        (let ((t0 (lis::sim-t sim))
              (load (svref (lis::sim-objects sim) index)))
          (is equal (list index h-index 0 t0) (lis::sim-delivery sim))
          (loop until (or (eq (lis::object-status load) :removed) (> (lis::sim-t sim) (+ t0 700)))
                do (lis::sim-step! sim level)
                   (true (on-grid-p (lis::sim-t sim))))
          (let ((removal (lis::object-removal load)))
            (true removal "×~D" level)
            (is eq :delivered (lis::removal-cause removal))
            (is = (+ t0 600) (lis::removal-time removal))
            (is equal '(:highport "plant") (lis::removal-point removal))
            (push (lis::removal-time removal) times))
          (false (lis::sim-delivery sim))
          (step-sim sim 64 level)
          (is equal '((:delivery "SKUA YARD 3 DELIVERED")) (lis::sim-notices sim) "×~D" level)
          (false (lis::sim-lost-p sim)))))
    (is = (first times) (second times))))

(define-test delivery-not-removed
  ;; Too fast, too far, drifting past 150 m, grappled again at 300 s.
  (flet ((never-removed (sim index label)
           (let ((load (svref (lis::sim-objects sim) index)))
             (step-sim sim most-positive-fixnum 1000 (+ (lis::sim-t sim) 700))
             (false (lis::sim-delivery sim) label)
             (false (eq (lis::object-status load) :removed) label)
             (is = 0 (delivered-notices sim) label))))
    (multiple-value-bind (sim index) (delivery-scene 80d0 0.06d0)
      (false (lis::sim-delivery sim))
      (never-removed sim index "0.06 m/s"))
    (multiple-value-bind (sim index) (delivery-scene 110d0 0.04d0)
      (false (lis::sim-delivery sim))
      (never-removed sim index "110 m"))
    (multiple-value-bind (sim index) (delivery-scene 80d0 0.04d0)
      (true (lis::sim-delivery sim))
      (step-sim sim most-positive-fixnum 1000 (+ (lis::sim-t sim) 60))
      ;; Pushed 0.15 m/s more away from the pin: past 150 m before 600 s.
      (let ((load (lis::object-state (svref (lis::sim-objects sim) index))))
        (lis::vec3-add-scaled! (lis::ship-state-v load) (lis::ship-state-v load)
                               (along-track load) -0.15d0))
      (step-sim sim most-positive-fixnum 1000 (+ (lis::sim-t sim) 250))
      (true (lis::sim-delivery sim))
      (step-sim sim most-positive-fixnum 1000 (+ (lis::sim-t sim) 250))
      (false (lis::sim-delivery sim))
      (never-removed sim index "drifted"))
    (multiple-value-bind (sim index) (delivery-scene 80d0 0.04d0)
      (step-sim sim most-positive-fixnum 1000 (+ (lis::sim-t sim) 300))
      (true (lis::sim-delivery sim))
      (lis::grapple-capture! sim index 0)
      (false (lis::sim-delivery sim))
      (setf (lis::sim-notices sim) nil)
      (never-removed sim index "grappled again")
      (is eq :latched (lis::object-status (svref (lis::sim-objects sim) index))))))

(define-test delivery-checkpoint
  ;; A checkpoint at release + 300 s restored: the same removal bit for bit.
  (multiple-value-bind (sim index) (delivery-scene 80d0 0.04d0)
    (let ((t0 (lis::sim-t sim)))
      (step-sim sim most-positive-fixnum 1000 (+ t0 300))
      (let ((checkpoint (lis::take-checkpoint sim)))
        (step-sim sim most-positive-fixnum 1000 (+ t0 700))
        (let ((first-run (lis::copy-sim sim)))
          (true (lis::object-removal (svref (lis::sim-objects sim) index)))
          (lis::restore-checkpoint! sim checkpoint)
          (is eq :promoted (lis::object-status (svref (lis::sim-objects sim) index)))
          (true (lis::sim-delivery sim))
          (step-sim sim most-positive-fixnum 1000 (+ t0 700))
          (is = (lis::removal-time (lis::object-removal (svref (lis::sim-objects first-run) index)))
              (lis::removal-time (lis::object-removal (svref (lis::sim-objects sim) index))))
          (true (equalp sim first-run)))))))

;;; Plan 24's round (task 194): from hand-placed starts beside each place,
;;; the sim driven only by its controls, grapple-key! and transfer-key!.

(defun notice-texts (sim) (mapcar #'second (lis::sim-notices sim)))

(defun arm-grapple (sim)
  (false (lis::grapple-key! sim))
  (is eq :armed (lis::grapple-state (lis::sim-grapple sim))))

(defun hold-translation (sim sign seconds)
  "The ±x translation key held SECONDS at ×1, then released; SIM."
  (let ((controls (lis::sim-controls sim)))
    (lis::controls-set-translation-key controls :x sign)
    (step-sim sim (* 64 seconds) 1)
    (lis::controls-set-translation-key controls :x 0)
    sim))

(defun place-highport-ahead! (sim index distance)
  "Highport promoted by hand, its body axes the ship's, its plant's pin
DISTANCE m ahead of the INDEXth object's CoM along ship +x (the fixture
facing it), at that object's velocity; Highport's index."
  (let* ((highport (lis::sim-object sim :highport))
         (load (lis::object-state (svref (lis::sim-objects sim) index)))
         (q (lis::quat-copy (lis::ship-state-q (lis::sim-ship sim))))
         (pin (lis::fixture-pin (first (lis::object-def-fixtures (lis::object-def-of highport)))))
         (r (lis::vec3-sub (lis::vec3-add-scaled (lis::ship-state-r load)
                                                 (lis::quat-rotate q (lis::vec3 1 0 0)) distance)
                           (lis::quat-rotate q pin))))
    (setf (lis::object-status highport) :promoted)
    (lis::ship-state-assign! (lis::object-state highport)
                             (lis::make-ship-state r (lis::vec3-copy (lis::ship-state-v load)) q
                                                   (lis::make-vec3) 0d0))
    (position highport (lis::sim-objects sim))))

(defun tow-errors (sim index h-index range)
  "(values p u), ship axes: the INDEXth object's CoM from the point RANGE m
out along Highport's plant axis, and its velocity relative to Highport."
  (let* ((q* (lis::quat-conjugate (lis::ship-state-q (lis::sim-ship sim))))
         (station (lis::object-state (svref (lis::sim-objects sim) h-index)))
         (fixture (first (lis::object-def-fixtures (lis::object-definition :highport))))
         (stop (lis::vec3-add (lis::ship-state-r station)
                              (lis::quat-rotate (lis::ship-state-q station)
                                                (lis::vec3-add-scaled (lis::fixture-pin fixture)
                                                                      (lis::fixture-axis fixture)
                                                                      range))))
         (load (lis::object-state (svref (lis::sim-objects sim) index))))
    (values (lis::quat-rotate q* (lis::vec3-sub (lis::ship-state-r load) stop))
            (lis::quat-rotate q* (lis::vec3-sub (lis::ship-state-v load)
                                                (lis::ship-state-v station))))))

(defun tow! (sim index h-index range)
  "Translation keys at ×1 until the INDEXth object's CoM rests within 1 m and
1 cm/s of RANGE m out on Highport's plant axis: towards it at 0.5 m/s at
most, braking over 40 s. The steps taken, or nil."
  (let ((controls (lis::sim-controls sim)))
    (loop for steps from 0 below 40000
          do (multiple-value-bind (p u) (tow-errors sim index h-index range)
               (when (and (< (lis::vec3-norm p) 1d0) (< (lis::vec3-norm u) 0.01d0))
                 (loop for axis in '(:x :y :z)
                       do (lis::controls-set-translation-key controls axis 0))
                 (return steps))
               (loop for axis in '(:x :y :z)
                     for i from 0
                     for e = (- (aref u i) (max -0.5d0 (min 0.5d0 (/ (aref p i) -40d0))))
                     do (lis::controls-set-translation-key
                         controls axis (cond ((> e 0.005d0) -1) ((< e -0.005d0) 1) (t 0)))))
             (lis::sim-step! sim 1)
             (when (lis::sim-lost-p sim)
               (return nil)))))

(defun round-tow-and-deliver ()
  ;; Skua Yard 3 grappled, towed 80 m to 70 m off Highport's plant, released
  ;; there and taken in at release + 600 s.
  (multiple-value-bind (sim index) (fixture-scene :skua-yard-3 :armed nil)
    (arm-grapple sim)
    (step-to-contact sim 1 (+ (lis::start-t) 2))
    (is equal '("LATCHED SKUA YARD 3") (notice-texts sim))
    (setf (lis::sim-notices sim) nil)
    (step-sim sim 200 1)
    (is eq :latched (lis::grapple-state (lis::sim-grapple sim)))
    (lis::controls-toggle-rate-damping (lis::sim-controls sim))
    (let ((h-index (place-highport-ahead! sim index 150d0)))
      (true (tow! sim index h-index 70d0) "towed")
      (step-sim sim 64 1)
      (multiple-value-bind (p u) (tow-errors sim index h-index 70d0)
        (true (< (lis::vec3-norm p) 2d0) "~,3F m off the stop" (lis::vec3-norm p))
        (true (< (lis::vec3-norm u) 0.02d0) "~,4F m/s" (lis::vec3-norm u))))
    (false (lis::grapple-key! sim))
    (let ((t0 (lis::sim-t sim))
          (load (svref (lis::sim-objects sim) index)))
      (true (lis::sim-delivery sim) "pending")
      (hold-translation sim -1 2)
      (step-sim sim most-positive-fixnum 1000 (+ t0 700))
      (false (lis::sim-lost-p sim))
      (let ((removal (lis::object-removal load)))
        (true removal)
        (when removal
          (is eq :delivered (lis::removal-cause removal))
          (is = (+ t0 600) (lis::removal-time removal))
          (is equal '(:highport "plant") (lis::removal-point removal))))
      (is equal '("SKUA YARD 3 DELIVERED") (notice-texts sim)))))

(defun round-berth (key &key (m-p 28000d0))
  "Berthed at KEY's first fixture after G armed the grapple, the claws
latched: SIM."
  (let ((sim (fixture-scene key :armed nil :m-p m-p))
        (name (lis::object-def-name (lis::object-definition key))))
    (arm-grapple sim)
    (step-to-contact sim 1 (+ (lis::start-t) 2))
    (true (lis::sim-berthed sim) "~A" name)
    (is equal (list (format nil "BERTHED ~A" name)) (notice-texts sim))
    (setf (lis::sim-notices sim) nil)
    (step-sim sim 200 1)
    (is eq :latched (lis::grapple-state (lis::sim-grapple sim)))
    sim))

(defun round-refuel (sim seconds)
  "B, then SECONDS at ×1,000: m_p rises by 10 kg/s exactly."
  (let ((m0 (lis::ship-state-m-p (lis::sim-ship sim)))
        (t0 (lis::sim-t sim)))
    (false (lis::transfer-key! sim))
    (true (lis::sim-transfer sim))
    (step-sim sim most-positive-fixnum 1000 (+ t0 seconds))
    (is = (min 28000d0 (+ m0 (* 10 seconds))) (lis::ship-state-m-p (lis::sim-ship sim)))))

(defun round-fulmar (sim)
  "From the berth at Fulmar Depot with 27,000 kg: refuelled to full, released,
backed off, a bounce into the depot at ~0.2 m/s, then the engine into it:
(values sim the bounce's normal speeds before and after)."
  (round-refuel sim 150)
  (false (lis::sim-transfer sim))
  (is equal '("TANKS FULL") (notice-texts sim))
  (false (lis::grapple-key! sim))
  (false (lis::grapple-key! sim))
  (is eq :stowed (lis::grapple-state (lis::sim-grapple sim)))
  (hold-translation sim -1 4)
  (step-sim sim (* 64 20) 1)
  (hold-translation sim 1 8)
  (multiple-value-bind (sim before) (step-to-contact sim 1 (+ (lis::sim-t sim) 120))
    (let ((speeds (and before (list (record-normal-speed before) (record-normal-speed sim)))))
      (step-sim sim (* 64 30) 1)
      (lis::controls-throttle-full (lis::sim-controls sim))
      (step-sim sim (* 64 30) 1)
      (values sim speeds))))

(define-test round-at-every-place
  (round-tow-and-deliver)
  ;; Tern Station: no propellant there.
  (let ((sim (round-berth :tern-station)))
    (is equal "NO PROPELLANT HERE" (lis::transfer-key! sim))
    (false (lis::grapple-key! sim))
    (false (lis::sim-berthed sim)))
  ;; Highport: refuelled.
  (round-refuel (round-berth :highport :m-p 20000d0) 100)
  ;; Fulmar Depot under a clock: the berth's checkpoint, a bounce, a death,
  ;; Return, and the same flight again bit for bit.
  (let* ((sim (fixture-scene :fulmar-depot :armed nil :m-p 27000d0))
         (clock (clock-at sim 1)))
    (arm-grapple sim)
    (loop repeat 256 until (lis::sim-berthed sim) do (lis::clock-step! clock sim))
    (true (lis::sim-berthed sim))
    (true (equalp sim (lis::clock-checkpoint clock)))
    (let ((checkpoint (lis::copy-sim sim)))
      (multiple-value-bind (sim speeds) (round-fulmar sim)
        (true speeds "bounced")
        (when speeds
          (destructuring-bind (v0 v1) speeds
            (true (< 0.1d0 (- v0) 0.3d0) "~F m/s" v0)
            (true (< (abs (/ (+ v1 (* 0.3d0 v0)) v0)) 1d-9) "~F after ~F" v1 v0)))
        (let ((loss (lis::sim-loss sim)))
          (true loss)
          (when loss
            (is eq :contact (lis::loss-cause loss))
            (is eq :fulmar-depot (lis::loss-object loss))
            (true (> (lis::loss-speed loss) 1d0) "~F m/s" (lis::loss-speed loss))))
        (let ((lost (lis::copy-sim sim)))
          (lis::clock-restart! clock sim (lis::clock-checkpoint clock))
          (true (equalp sim checkpoint))
          (round-fulmar sim)
          (true (equalp sim lost)))))))
