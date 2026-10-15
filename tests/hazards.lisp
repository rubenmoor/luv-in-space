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
                                      (q-o (lis::quat 1 0 0 0)) (w-o (lis::make-vec3)))
  "The start's sim, :point, object KEY promoted by hand on the orbit (a
towable at Q-O and W-O, body axes; a berth object in its mode's attitude),
the ship turned Q-REL from the object's axes, direct mode, at rest
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
           (com (lis::mass-properties-com (lis::ship-mass-properties 28000d0 nil)))
           (r-s (lis::vec3-sub r-o (lis::quat-rotate q-s (lis::vec3-sub centre com))))
           (v-s (lis::vec3-sub v-o (lis::quat-rotate q-s v-rel))))
      (setf (lis::sim-model sim) :point
            (lis::object-status object) :promoted)
      (lis::ship-state-assign! (lis::object-state object)
                               (lis::make-ship-state r-o v-o q-o w-o 0d0))
      (lis::ship-state-assign! (lis::sim-ship sim)
                               (lis::make-ship-state r-s v-s q-s (lis::make-vec3) 28000d0))
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
                      (lis::object-state (svref (lis::sim-objects sim) index))))))))

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
