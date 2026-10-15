(in-package #:luv-in-space.tests)

;;; prediction.lisp: the milestone-2 events by sim-step! on a copy
;;; (docs/architecture.md "Testing" → "Warp, events and collisions",
;;; Prediction; decision 75: Prediction in milestone 2: the step function on
;;; a copy).

(defun run-prediction (sim &rest args)
  (lis::prediction-run! (apply #'lis::new-prediction args) sim))

(defun prediction-nodes (pred)
  "The stored nodes from the head, as a fresh array."
  (subseq (lis::prediction-nodes pred)
          (* 7 (lis::prediction-head pred)) (* 7 (lis::prediction-count pred))))

(defun last-node (pred)
  "(values t r v) of PRED's last node."
  (let ((nodes (lis::prediction-nodes pred))
        (k (* 7 (1- (lis::prediction-count pred)))))
    (values (aref nodes k) (subseq nodes (+ k 1) (+ k 4)) (subseq nodes (+ k 4) (+ k 7)))))

(defun prediction-events (pred)
  (loop for i below (lis::prediction-event-count pred)
        collect (list (svref (lis::prediction-event-kinds pred) i)
                      (aref (lis::prediction-event-times pred) i))))

(defun r-dot-v (sim)
  (let ((ship (lis::sim-ship sim)))
    (lis::vec3-dot (lis::ship-state-r ship) (lis::ship-state-v ship))))

(define-test prediction-grid-ceiling
  (let ((t0 (lis::start-t)))
    (is = t0 (lis::grid-ceiling t0))
    (is = (+ t0 1/64) (lis::grid-ceiling (+ t0 1d-6)))
    (is = (+ t0 1/64) (lis::grid-ceiling (+ t0 1/64)))))

(define-test prediction-coasting-bit-for-bit
  ;; :full at ×1,000: one orbit ahead the prediction is the flight.
  (let* ((sim (lis::new-game-sim))
         (pred (run-prediction sim))
         (period (lis::osculating-period sim)))
    (true (lis::prediction-complete-p pred))
    (false (lis::prediction-event pred :impact))
    (multiple-value-bind (end r v) (last-node pred)
      (true (>= (- end (lis::sim-t sim)) period))
      (let ((flown (step-sim (lis::copy-sim sim) 100000 1000 end)))
        (is = end (lis::sim-t flown))
        (true (equalp r (lis::ship-state-r (lis::sim-ship flown))))
        (true (equalp v (lis::ship-state-v (lis::sim-ship flown))))))
    ;; Frames of 500 steps.
    (let ((framed (lis::new-prediction))
          (frames 0))
      (let ((lis::*prediction-steps-per-frame* 500))
        (loop do (lis::prediction-update! framed sim)
                 (incf frames)
                 (true (<= (lis::prediction-last-steps framed) 500))
              until (lis::prediction-complete-p framed)))
      (true (> frames 5))
      (is = 1 (lis::prediction-restarts framed))
      (true (equalp (prediction-nodes pred) (prediction-nodes framed)))
      (is equal (prediction-events pred) (prediction-events framed)))))

(defun apsis-sim ()
  "A :point orbit with e 0.1, periapsis 400 km, a quarter past it."
  (let* ((q (+ lis::+radius-earth+ 400d3))
         (orbit (lis::make-elliptic-orbit lis::+mu-earth+ (/ q 0.9d0) 0.1d0 (lis::deg->rad 60)
                                          1d0 2d0 (/ pi 2) (lis::start-t))))
    (values (orbit-sim orbit) orbit)))

(define-test prediction-apsides
  (multiple-value-bind (sim orbit) (apsis-sim)
    ;; The apoapsis comes first.
    (let ((pred (run-prediction sim))
          (start (lis::sim-t sim)))
      (dolist (kind '(:apoapsis :periapsis))
        (let ((predicted (lis::prediction-event pred kind))
              (kepler (lis::orbit-next-apsis orbit start kind)))
          (true predicted)
          (when predicted
            (true (< (abs (- predicted kepler)) 1d-3)
                  "~A ~,6F s from Kepler" kind (- predicted kepler))
            ;; The flight: coarse to 20 s before (the inertial hold it engages
            ;; makes ×1 steps cost the mixer), then ×1 to the grid point after;
            ;; r·v changes sign over the last fine step.
            (let ((after (lis::grid-ceiling predicted))
                  (before-sign nil))
              (step-sim sim 100000 1000 (- after 20))
              (step-sim sim 100000 1 (- after 1/64))
              (setf before-sign (plusp (r-dot-v sim)))
              (step-sim sim 1 1 after)
              (is = after (lis::sim-t sim))
              (true (> (- after predicted) -1d-9))
              (is eq (eq kind :apoapsis) before-sign)
              (is eq (eq kind :periapsis) (plusp (r-dot-v sim))))))))))

(define-test prediction-impact
  (let* ((start (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3))))
         (pred (run-prediction start)))
    (multiple-value-bind (time loss) (lis::prediction-event pred :impact)
      (true time)
      (when time
        (is eq :earth (lis::loss-body loss))
        (is = time (lis::loss-time loss))
        (is = time (lis::prediction-dropout-event pred))
        (is eq :impact (nth-value 1 (lis::prediction-dropout-event pred)))
        (let ((coarse (fly-until-lost (lis::copy-sim start) 1000 (+ time 10))))
          (true (< (abs (- (lis::loss-time (lis::sim-loss coarse)) time)) 1d-2)))
        (let ((fine (fly-until-lost (lis::copy-sim start) 1 (+ time 10))))
          (true (lis::sim-lost-p fine))
          (true (< (abs (- (lis::loss-time (lis::sim-loss fine)) time)) 0.05d0)
                "×1: ~,6F s from the prediction" (- (lis::loss-time (lis::sim-loss fine)) time)))))
    (false (lis::checkpoint-safe-p start))
    (true (lis::checkpoint-safe-p (lis::new-game-sim)))))

(define-test prediction-cutoff
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-model sim) :point
          (lis::sim-cutoff sim) 25d0)
    (lis::controls-throttle-full (lis::sim-controls sim))
    (let* ((pred (run-prediction sim))
           (cut (lis::prediction-event pred :cutoff))
           (apoapsis (lis::prediction-event pred :apoapsis)))
      (true cut)
      (true apoapsis)
      (when (and cut apoapsis)
        (loop while (lis::sim-cutoff sim) do (step-sim sim 1 100))
        (is = cut (lis::sim-t sim))
        (step-sim sim 100000 100 (lis::grid-ceiling apoapsis))
        (let ((p (lis::prediction-position! (lis::make-vec3) pred apoapsis)))
          (true (< (abs (- (lis::vec3-norm p)
                           (lis::vec3-norm (lis::ship-state-r (lis::sim-ship sim)))))
                   1d0)))))))

(defun frames (pred sim n)
  "N frames of two fine steps of SIM, each followed by prediction-update!."
  (dotimes (i n pred)
    (step-sim sim 2 1)
    (lis::prediction-update! pred sim)))

(define-test prediction-restart-rules
  (flet ((restarts-after (change)
           (let ((sim (lis::new-game-sim))
                 (pred (lis::new-prediction)))
             (setf (lis::sim-model sim) :point)
             (lis::prediction-run! pred sim)
             (frames pred sim 2)
             (let ((before (lis::prediction-restarts pred)))
               (funcall change sim)
               (lis::prediction-update! pred sim)
               (- (lis::prediction-restarts pred) before)))))
    (is = 1 (restarts-after (lambda (sim) (lis::controls-set-throttle (lis::sim-controls sim) 0.5))))
    (is = 1 (restarts-after (lambda (sim) (lis::controls-set-hold (lis::sim-controls sim) :retrograde))))
    (is = 1 (restarts-after (lambda (sim) (incf (aref (lis::ship-state-r (lis::sim-ship sim)) 0) 2d0))))
    (is = 0 (restarts-after (lambda (sim) (incf (aref (lis::ship-state-r (lis::sim-ship sim)) 0) 0.1d0)))))
  ;; Coasting 100 frames: no restart, the tail grows.
  (let* ((sim (lis::new-game-sim))
         (pred (lis::new-prediction)))
    (setf (lis::sim-model sim) :point)
    (lis::prediction-run! pred sim)
    (multiple-value-bind (end r) (last-node pred)
      (frames pred sim 100)
      (is = 1 (lis::prediction-restarts pred))
      (true (> (last-node pred) end))
      (let ((p (lis::prediction-position! (lis::make-vec3) pred end)))
        (true (equalp r p)))))
  ;; Throttle on: a restart each call.
  (let* ((sim (lis::new-game-sim))
         (pred (run-prediction sim)))
    (lis::controls-set-throttle (lis::sim-controls sim) 0.5)
    (frames pred sim 3)
    (is = 4 (lis::prediction-restarts pred))))

(define-test prediction-death-and-restore
  (let* ((start (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3))))
         (checkpoint (lis::take-checkpoint start))
         (pred (run-prediction start)))
    (fly-until-lost start 1000 (+ (lis::start-t) 20000))
    (lis::prediction-update! pred start)
    (is = 2 (lis::prediction-restarts pred))
    (true (lis::prediction-complete-p pred))
    (false (lis::prediction-event pred :impact))
    (lis::prediction-update! pred start)
    (is = 2 (lis::prediction-restarts pred))
    (lis::restore-checkpoint! start checkpoint)
    (lis::prediction-update! pred start)
    (is = 3 (lis::prediction-restarts pred))))

(define-test prediction-budget-and-consing
  (let* ((sim (lis::new-game-sim))
         (pred (lis::new-prediction :periods 2)))
    (lis::prediction-update! pred sim)
    (is = lis::*prediction-steps-per-frame* (lis::prediction-last-steps pred))
    (false (lis::prediction-complete-p pred))
    (lis::prediction-update! pred sim)
    (true (lis::prediction-complete-p pred))
    (let ((consed (least-bytes-consed
                   (lambda ()
                     (lis::prediction-invalidate! pred)
                     (lis::prediction-run! pred sim)))))
      (true (< consed 1024) "~D bytes" consed))
    (is = 4 (lis::prediction-restarts pred))))
