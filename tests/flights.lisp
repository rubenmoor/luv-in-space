(in-package #:luv-in-space.tests)

;;; flights.lisp: the safe reader and the step parser; prediction-apsis
;;; (docs/architecture.md "Scripted runs"; decision 87: Flight scripts in
;;; milestone 2: additions, apsis conditions, live fly and scripts/fly).

(defun script-error-of (source)
  "The flight-script-error reading SOURCE signals, or nil."
  (handler-case (progn (lis::read-flight source) nil)
    (lis::flight-script-error (e) e)))

(defun script-error-text (source)
  (let ((e (script-error-of source)))
    (and e (princ-to-string e))))

(define-test flights-reader-rejects
  (let ((packages (length (list-all-packages))))
    (true (script-error-of "((:throttle #.(+ 1 0)))"))
    (true (script-error-of "(#S(lis::flight-step))"))
    (true (search "FOO IS NOT A KEYWORD" (script-error-text "((:throttle 1.0)
 (:hold foo))")))
    (true (search "SECOND FORM" (script-error-text "((:throttle 1.0)) ((:throttle 0))")))
    (true (search "NOT BEFORE MILESTONE 3" (script-error-text "((:autopilot :hohmann 800e3))")))
    (true (search "NOT BEFORE MILESTONE 3" (script-error-text "((:start :state (:date \"2040-01-01\")))")))
    (true (search "NOT BEFORE MILESTONE 5" (script-error-text "((:wait :quest-step \"2.3\"))")))
    (true (search "NOT BEFORE MILESTONE 4" (script-error-text "((:expect :soi :moon))")))
    (true (search ":BOGUS" (script-error-text "((:bogus 1))")))
    (true (search "THROTTLE" (script-error-text "((:throttle 0.05))")))
    (true (script-error-of "((:expect :apoapsis))"))
    (true (script-error-of "((:wait :altitude-below 120e3 :before 10))"))
    (true (script-error-of "((:throttle 1.0) (:start :new-game))"))
    (true (search "NO SCRIPT" (script-error-text "no-such-script")))
    ;; The line of the offending step.
    (let ((e (script-error-of "(; a comment
  (:throttle 1.0)
  (:after 10 :throttle 0)
  (:grapple :arm))")))
      (true e)
      (when e
        (is = 4 (lis::flight-script-error-line e))
        (true (search "LINE 4" (princ-to-string e)))))
    (is = packages (length (list-all-packages)))))

(define-test flights-reader-parses
  (let ((steps (lis::read-flight "((:start :new-game)
 (:at 0.01 :warp 1)
 (:after-dv 9.10 :throttle 0)
 (:after 10 :throttle 0)
 (:after 300)
 (:after-dv 25.0 :throttle 0)
 (:keys (:w :h) :for 2)
 (:fine t)
 (:hold nil)
 (:wait :apoapsis :before 18.5)
 (:wait :date \"2040-01-01T01:00:00\" :within 7200)
 (:expect :lost :crash)
 (:screenshot \"falling\" :view :map))")))
    (is = 13 (length steps))
    (destructuring-bind (start at bare after pass dv keys fine hold apsis date lost shot) steps
      (is eq :start (lis::flight-step-name start))
      (is = 1/64 (lis::flight-step-at at))
      (is equal '(1) (lis::flight-step-args at))
      (is eq :action (lis::flight-step-kind bare))
      (is = 0 (lis::flight-step-after bare))
      (false (lis::flight-step-at bare))
      (is eql 9.1d0 (first (lis::flight-step-args bare)))
      (is = 10 (lis::flight-step-after after))
      (is eq :pass (lis::flight-step-kind pass))
      (is = 300 (lis::flight-step-after pass))
      (is equal '(25d0) (lis::flight-step-args dv))
      (is equal '((:w :h)) (lis::flight-step-args keys))
      (is = 2 (lis::flight-step-for keys))
      (is equal '(t) (lis::flight-step-args fine))
      (is equal '(nil) (lis::flight-step-args hold))
      (is eq :wait (lis::flight-step-kind apsis))
      (is = 18.5 (lis::flight-step-before apsis))
      (is = (* 30 86400) (lis::flight-step-within apsis))
      (is = (lis::utc->grid-t 2040 1 1 1 0 0) (first (lis::flight-step-args date)))
      (is = 7200 (lis::flight-step-within date))
      (is eq :expect (lis::flight-step-kind lost))
      (is equal '(:crash) (lis::flight-step-args lost))
      (is eq :map (lis::flight-step-view shot))
      (is equal '("falling") (lis::flight-step-args shot))))
  ;; grid-ceiling: 0 stays, anything above rounds up.
  (is = 0 (lis::flight-step-at (first (lis::read-flight "((:at 0 :throttle 1.0))"))))
  (is eq :outside (lis::flight-step-view (first (lis::read-flight "((:screenshot \"x\"))")))))

(define-test flights-prediction-apsis
  ;; Coasting after a small prograde burn: the Hermite's apoapsis.
  (let* ((sim (lis::new-game-sim))
         (v (lis::ship-state-v (lis::sim-ship sim))))
    (lis::vec3-scale! v v 1.003d0)
    (let ((pred (run-prediction sim)))
      (multiple-value-bind (time radius) (lis::prediction-apsis pred :apoapsis)
        (true time)
        (when time
          (is = (lis::prediction-event pred :apoapsis) time)
          (let ((flown (step-sim (lis::copy-sim sim) 100000 1000 time)))
            (is = time (lis::sim-t flown))
            (true (< (abs (- radius (lis::vec3-norm (lis::ship-state-r (lis::sim-ship flown)))))
                     1d-3)))
          (true (< 400d3 (- radius lis::+radius-earth+) 500d3))))))
  ;; An impact first: the osculating periapsis of the impact state.
  (let* ((pred (run-prediction (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3)))))
         (impact (lis::prediction-event pred :impact)))
    (true impact)
    (when impact
      (multiple-value-bind (time radius) (lis::prediction-apsis pred :periapsis)
        (true (> time impact))
        (true (< (abs (- radius (- *earth-crash-radius* 50d3))) 1d0)
              "periapsis ~,3F m off" (- radius (- *earth-crash-radius* 50d3)))))))

;;; run-flight (headless): actions, waits, expectations, failures, the record

(defun shot (record label)
  "The :screenshot entry LABEL of RECORD."
  (find-if (lambda (e) (and (eq (lis::flight-entry-kind e) :screenshot)
                            (equal (lis::flight-entry-label e) label)))
           record))

(defun shot-t (record label) (lis::flight-entry-t (shot record label)))

(defun failure-text (record)
  (let ((e (lis::flight-record-failure record)))
    (and e (lis::flight-entry-label e))))

(define-test flights-run-timing
  (let ((record (lis::run-flight "((:at 0.01 :screenshot \"a\") (:after 300) (:screenshot \"b\"))")))
    (true (lis::flight-record-passed-p record))
    (is = (+ (lis::start-t) 1/64) (shot-t record "a"))
    (is = 300 (- (shot-t record "b") (shot-t record "a")))
    (is equal '(:screenshot :screenshot :end) (mapcar #'lis::flight-entry-kind record))
    (is = 3 (length (lis::flight-record-lines record)))))

(define-test flights-run-cutoff
  (let ((burn-dv (/ (* lis::*engine-thrust* lis::+fine-step+)
                    (+ lis::*ship-dry-mass* lis::*propellant-full*)))
        (apoapsides '()))
    (dolist (script '("((:throttle 1.0) (:after-dv 25.0 :throttle 0) (:screenshot \"cut\"))"
                      "((:warp :coast) (:throttle 1.0) (:after-dv 25.0 :throttle 0) (:screenshot \"cut\"))"))
      (multiple-value-bind (record sim) (lis::run-flight script)
        (true (lis::flight-record-passed-p record) "~{~A~%~}" (lis::flight-record-lines record))
        (let* ((ship (lis::sim-ship sim))
               (burn (find :burn record :key #'lis::flight-entry-kind)))
          (is = 0 (lis::controls-throttle (lis::sim-controls sim)))
          (true (<= 25d0 (lis::ship-state-dv ship) (+ 25d0 burn-dv))
                "odometer ~,6F" (lis::ship-state-dv ship))
          (true (on-grid-p (lis::sim-t sim)))
          (true burn)
          (when burn
            (true (<= 25d0 (lis::flight-entry-dv burn) (+ 25d0 burn-dv))))
          (push (nth-value 1 (lis::prediction-apsis (run-prediction sim) :apoapsis))
                apoapsides))))
    (true (< (abs (- (first apoapsides) (second apoapsides))) 1d0)
          "apoapsides ~,3F m apart" (- (first apoapsides) (second apoapsides)))))

(define-test flights-run-waits
  ;; An event wait ends at the grid point at or after the predicted time, less :before.
  (let* ((burn "(:warp :coast) (:throttle 1.0) (:after 2 :throttle 0)")
         (cut (nth-value 1 (lis::run-flight (format nil "(~A)" burn))))
         (apoapsis (lis::prediction-event (run-prediction cut) :apoapsis)))
    (true apoapsis)
    (when apoapsis
      (is = (lis::grid-ceiling apoapsis)
          (shot-t (lis::run-flight (format nil "(~A (:wait :apoapsis) (:screenshot \"ap\"))" burn))
                  "ap"))
      (is = (lis::grid-ceiling (- apoapsis 10))
          (shot-t (lis::run-flight
                   (format nil "(~A (:wait :apoapsis :before 10) (:screenshot \"ap\"))" burn))
                  "ap"))))
  (multiple-value-bind (record sim)
      (lis::run-flight "((:warp :coast) (:wait :date \"2040-01-01T01:00:00\"))")
    (true (lis::flight-record-passed-p record))
    (is = (lis::utc->grid-t 2040 1 1 1 0 0) (lis::sim-t sim)))
  (multiple-value-bind (record sim)
      (lis::run-flight "((:wait :altitude-below 100e3 :within 10) (:screenshot \"never\"))")
    (false (lis::flight-record-passed-p record))
    (true (search "ALTITUDE-BELOW 100.000 KM" (failure-text record)) "~A" (failure-text record))
    (false (shot record "never"))
    (is = (+ (lis::start-t) 10) (lis::sim-t sim))))

(define-test flights-run-refusal
  (let ((record (lis::run-flight "((:throttle 1.0) (:warp 1000) (:screenshot \"never\"))")))
    (is equal "WARP REFUSED: ENGINE ON, LIMIT ×10" (failure-text record))
    (false (shot record "never"))
    (is eq :end (lis::flight-entry-kind (first (last record))))))

(define-test flights-run-keys
  (let* ((record (lis::run-flight "((:keys (:w) :for 2) (:screenshot \"turned\")
 (:after 30) (:expect :rate-below 0.01))"))
         (turned (lis::flight-entry-sim (shot record "turned"))))
    (true (lis::flight-record-passed-p record) "~{~A~%~}" (lis::flight-record-lines record))
    (is eq :rate-damping (lis::controls-attitude-mode (lis::sim-controls turned)))
    (true (> (abs (aref (lis::ship-state-w (lis::sim-ship turned)) 1)) 1d-3))
    (is equalp #(0 0 0) (lis::controls-rotation-keys (lis::sim-controls turned)))))

(define-test flights-run-death
  ;; Radial but for 1 m/s across: the osculating elements stay defined.
  (let ((fall (crash-sim (lis::vec3 0 (- (+ lis::+radius-earth+ 1000d3)) 0) (lis::vec3 1 0 0))))
    (let ((record (lis::run-flight "((:warp :coast) (:wait :impact))" :sim fall)))
      (false (lis::flight-record-passed-p record))
      (let ((failure (lis::flight-record-failure record)))
        (true failure)
        (when failure
          (is eq :crash (lis::loss-cause (lis::flight-entry-loss failure))))))
    (multiple-value-bind (record sim)
        (lis::run-flight "((:warp :coast) (:wait :impact) (:expect :lost :crash)
 (:expect :impact-as-predicted 0.1) (:screenshot \"lost\"))" :sim fall)
      (true (lis::flight-record-passed-p record) "~{~A~%~}" (lis::flight-record-lines record))
      (true (lis::sim-lost-p sim))
      (true (lis::sim-lost-p (lis::flight-entry-sim (shot record "lost")))))
    (multiple-value-bind (record sim)
        (lis::run-flight "((:warp :coast) (:wait :impact) (:expect :lost :crash)
 (:restart) (:expect :start-state))" :sim fall)
      (true (lis::flight-record-passed-p record) "~{~A~%~}" (lis::flight-record-lines record))
      (true (equalp sim fall)))
    ;; Not lost: the restart is refused.
    (true (search "NOT LOST" (failure-text (lis::run-flight "((:restart))"))))))

;;; The milestone-2 scripts (flights/*.sexp)

(defun script-record-lines (record)
  (format nil "~{~A~%~}" (lis::flight-record-lines record)))

(defun record-equal (a b)
  "Records equal entry by entry, every slot (doubles bit for bit)."
  (and (= (length a) (length b))
       (every #'equalp a b)))

(define-test flights-scripts
  (dolist (name '("burn" "raise-orbit" "deorbit"))
    (multiple-value-bind (record sim) (lis::run-flight name)
      (true (lis::flight-record-passed-p record) "~A:~%~A" name (script-record-lines record))
      (true (find :expect record :key #'lis::flight-entry-kind))
      ;; Determinism: a second run gives the same end sim and record.
      (multiple-value-bind (again again-sim) (lis::run-flight name)
        (true (equalp sim again-sim) "~A: end sims differ" name)
        (true (record-equal record again) "~A: records differ" name))
      (cond ((equal name "burn")
             (let ((m-p (lis::ship-state-m-p (lis::sim-ship sim))))
               (true (< (abs (- m-p 27769d0)) 5d0) "propellant ~,1F kg" m-p))
             (is = 1 (count :burn record :key #'lis::flight-entry-kind)))
            ((equal name "raise-orbit")
             (is = 2 (count :burn record :key #'lis::flight-entry-kind)))
            (t
             ;; Crashed at the predicted time, restarted at the start state.
             (is = 1 (count :burn record :key #'lis::flight-entry-kind))
             (let ((lost (shot record "lost")))
               (true lost)
               (when lost
                 (let ((loss (lis::flight-entry-loss lost)))
                   (is eq :crash (lis::loss-cause loss))
                   (true (realp (lis::loss-latitude loss)))
                   (true (realp (lis::loss-longitude loss)))
                   (true (search "LAT " (script-record-lines record))))))
             (true (equalp sim (lis::new-game-sim))))))))

(define-test flights-raise-orbit-two-body
  ;; decision 87: the 400 km raise within 1 % two-body; the script's bands
  ;; (for the full model) left out.
  (let* ((steps (remove-if (lambda (s)
                             (and (eq (lis::flight-step-kind s) :expect)
                                  (member (lis::flight-step-name s)
                                          '(:apoapsis-above :apoapsis-below
                                            :periapsis-above :periapsis-below))))
                           (lis::read-flight "raise-orbit")))
         (record (lis::run-flight steps :model :point)))
    (true (lis::flight-record-passed-p record) "~A" (script-record-lines record))
    (flet ((within (m) (< (abs (- m 800d3)) 4d3)))
      (let ((transfer (shot record "transfer"))
            (raised (shot record "raised")))
        (true (within (lis::flight-entry-apoapsis transfer))
              "transfer apoapsis ~,3F km" (/ (lis::flight-entry-apoapsis transfer) 1d3))
        (true (within (lis::flight-entry-apoapsis raised))
              "raised apoapsis ~,3F km" (/ (lis::flight-entry-apoapsis raised) 1d3))
        (true (within (lis::flight-entry-periapsis raised))
              "raised periapsis ~,3F km" (/ (lis::flight-entry-periapsis raised) 1d3))))))

(defun flown-by-frames (name elapsed-list)
  "NAME flown through clock-frame! with a game-less clock, the frames'
elapsed times cycling through ELAPSED-LIST: (values record sim-at-the-end)."
  (let* ((sim (lis::new-game-sim))
         (end nil)
         (flight (lis::new-flight (lis::read-flight name) sim))
         (clock (lis::new-clock sim))
         (lis::*frame-step-time-limit* nil)
         ;; At x1 the prediction steers no step; the waits run it to the end.
         (lis::*prediction-steps-per-frame* 64))
    (lis::flight-install! clock flight (lambda (flight sim)
                                         (declare (ignore flight))
                                         (setf end (lis::copy-sim sim))))
    (loop for frames from 0
          for elapsed = (nth (mod frames (length elapsed-list)) elapsed-list)
          until (or (lis::flight-done flight) (> frames 100000))
          do (lis::clock-frame! clock sim elapsed)
             (lis::flight-frame! clock sim))
    (false (lis::clock-hook clock))
    (values (lis::flight-record flight) end)))

(define-test flights-live-frames
  ;; docs/architecture.md "Running scripts": live and headless take the same
  ;; steps, bit for bit, at any frame rate.
  (multiple-value-bind (record sim) (lis::run-flight "burn")
    (dolist (elapsed-list '((1/60) (0.013d0 0.021d0 0.0007d0 0.05d0 0.2d0 0.016d0 0d0)))
      (multiple-value-bind (live live-sim) (flown-by-frames "burn" elapsed-list)
        (true (equalp sim live-sim) "~A: end sims differ" elapsed-list)
        (true (record-equal record live) "~A: records differ~%~A~%~A" elapsed-list
              (script-record-lines record) (script-record-lines live))))))
