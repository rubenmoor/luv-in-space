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

;;; The contact drop-out (docs/architecture.md "Time warp", "Trajectory
;;; prediction"; decision 43: Warp drop-outs for contact and life support,
;;; decision 113: Grapple and contact details the design leaves open).

(defun intercept! (sim key v-rel duration)
  "SIM's ship (:point) placed to coast into object KEY's CoM DURATION s
from now at V-REL (inertial) relative to it, both on Kepler conics; KEY
put on J2-free rails when on rails. (values sim index)."
  (let* ((object (lis::sim-object sim key))
         (index (position object (lis::sim-objects sim)))
         (t0 (lis::sim-t sim))
         (t1 (+ t0 duration))
         (mu lis::+mu-earth+))
    (multiple-value-bind (r v) (object-rv object t0)
      (let ((orbit (lis::state->orbit r v mu t0)))
        (when (eq (lis::object-status object) :rails)
          (setf (lis::object-rails object) (lis::make-rails orbit t0 :j2 nil)))
        (multiple-value-bind (r1 v1) (lis::orbit-state orbit t1)
          (multiple-value-bind (r0 v0)
              (lis::orbit-state (lis::state->orbit r1 (lis::vec3-add v1 v-rel) mu t1) t0)
            (setf (lis::sim-model sim) :point)
            (lis::ship-state-assign! (lis::sim-ship sim)
                                     (lis::make-ship-state r0 v0 (lis::quat 1 0 0 0)
                                                           (lis::make-vec3) 28000d0))
            (lis::sim-recheck-objects! sim)
            (values sim index)))))))

(defun approach-sim ()
  "The ship 1,800 s from Skua Yard 3 (50 t, promoted by hand), closing at
2 m/s radially at the end: ~5 km away now. (values sim index)."
  (let ((sim (contact-scene :skua-yard-3 (lis::vec3 -100 0 0))))
    (intercept! sim :skua-yard-3 (lis::vec3 -2 0 0) 1800d0)))

(define-test prediction-contact-event
  (multiple-value-bind (sim index) (approach-sim)
    (let* ((ship (lis::sim-ship sim))
           (object (svref (lis::sim-objects sim) index))
           (d (lis::vec3-norm (lis::vec3-sub (lis::ship-state-r (lis::object-state object))
                                             (lis::ship-state-r ship))))
           (pred (run-prediction sim)))
      (true (< 4000 d 7000) "~,1F m" d)
      (multiple-value-bind (time object-index) (lis::prediction-event pred :contact)
        (true time)
        (is eql index object-index)
        ;; The spheres meet a little before the CoMs: ~(bounds) / 2 m/s.
        (when time
          (true (< 1780 (- time (lis::sim-t sim)) 1800) "~,3F s" (- time (lis::sim-t sim)))
          (is = time (lis::prediction-dropout-event pred))
          (is eq :contact (nth-value 1 (lis::prediction-dropout-event pred)))))
      ;; The copy's contact death is no :impact.
      (true (lis::sim-lost-p (lis::prediction-sim pred)))
      (false (lis::prediction-event pred :impact))
      (false (lis::checkpoint-safe-p sim))
      (let ((consed (least-bytes-consed (lambda ()
                                          (lis::prediction-invalidate! pred)
                                          (lis::prediction-run! pred sim)))))
        (true (< consed 1024) "~D bytes" consed)))))

(define-test prediction-contact-none
  ;; Berthed, latched, just released, overlapping at the start (bouncing,
  ;; or dying): no :contact event.
  (flet ((events (sim)
           (let ((pred (run-prediction sim)))
             (list (lis::prediction-event pred :contact) (lis::prediction-event pred :impact)))))
    (dolist (key '(:tern-station :skua-yard-3))
      (let ((sim (attached-scene key)))
        (is equal '(nil nil) (events sim) "~A attached" key)
        (false (lis::grapple-key! sim))
        (is equal '(nil nil) (events sim) "~A released" key)))
    (dolist (speed '(0.5d0 1.01d0))
      (is equal '(nil nil) (events (aft-scene speed)) "~F m/s" speed))))

(define-test prediction-contact-rails
  ;; Tern Station on rails ~180 km away, promoted on the copy: seen.
  (multiple-value-bind (sim index) (intercept! (lis::new-game-sim) :tern-station
                                               (lis::vec3 0 0 50) 3000d0)
    (is eq :rails (lis::object-status (svref (lis::sim-objects sim) index)))
    (let ((pred (run-prediction sim)))
      (multiple-value-bind (time object-index) (lis::prediction-event pred :contact)
        (true time)
        (is eql index object-index)
        (when time
          (true (< 2998 (- time (lis::sim-t sim)) 3000) "~,3F s" (- time (lis::sim-t sim))))))))

(define-test clock-dropout-to-contact
  ;; At ×1,000 through clock-frame!: every step above ×1 at least 3 s ×
  ;; level before the event (less 0.05 s), ×1 at it, a message at each
  ;; lowering; warp up refused within 3 s of it.
  (let* ((sim (approach-sim))
         (event (lis::prediction-event (run-prediction (lis::copy-sim sim)) :contact))
         (clock (clock-at sim 1000))
         (record '())
         (messages '())
         (refused nil)
         (lis::*frame-step-time-limit* nil))
    (setf (lis::clock-hook clock)
          (lambda (clock s)
            (push (cons (lis::sim-t s) (lis::clock-level clock)) record)
            (when (and (not refused) (< 0 (- event (lis::sim-t s)) 3))
              (setf refused t)
              (false (lis::clock-warp-up clock s))
              (true (find-if (lambda (m) (search "WARP REFUSED: CONTACT IN " m))
                             (lis::clock-messages clock))))
            (setf messages (union messages (lis::clock-messages clock) :test #'string=))
            nil))
    (loop repeat 100000
          until (or (lis::sim-lost-p sim) (> (lis::sim-t sim) (+ event 5)))
          do (lis::clock-frame! clock sim 1/60))
    (true refused)
    (is = 0 (count-if (lambda (r)
                        (and (> (cdr r) 1) (< (- event (car r)) (- (* 3 (cdr r)) 0.05d0))))
                      record))
    (is = 1 (cdr (find-if (lambda (r) (<= (car r) event)) record)))
    (dolist (level '(100 10 4 1))
      (true (find-if (lambda (m) (search (format nil "WARP ×~:D: CONTACT IN " level) m)) messages)
            "×~D in ~S" level messages))))

(define-test clock-checkpoint-contact
  ;; A checkpoint request with a contact predicted takes none.
  (let* ((sim (approach-sim))
         (clock (lis::new-clock sim)))
    (setf (lis::sim-checkpoint-request sim) t)
    (lis::clock-step! clock sim)
    (false (lis::sim-checkpoint-request sim))
    (false (lis::clock-checkpoint clock))))
