(in-package #:luv-in-space.tests)

;;; hazards.lisp: the swept crash test and death (docs/architecture.md
;;; "Testing" → "Crash detection"; decision 74: Crash test on the step's
;;; Hermite and the death state).

(defparameter *earth-crash-radius* (lis::body-crash-radius (lis::body :earth)))

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
  ;; Moon's least distance): its branch runs every step.
  (flet ((run ()
           (multiple-value-bind (mr mv) (lis::body-state :moon (lis::start-t))
             (let ((sim (crash-sim (lis::vec3-add mr (lis::vec3-scale (lis::vec3-normalize mr) -5d6))
                                   mv :model :full)))
               (let ((before (sb-ext:get-bytes-consed)))
                 (step-sim sim 100 1000)
                 (values (- (sb-ext:get-bytes-consed) before) sim))))))
    (run)
    (multiple-value-bind (consed sim) (run)
      (false (lis::sim-lost-p sim))
      (true (< consed 1024) "~D bytes" consed))))
