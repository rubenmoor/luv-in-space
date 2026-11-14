(defpackage #:luv-in-space.tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:lis #:luv-in-space)))

(in-package #:luv-in-space.tests)

;;; Every Parachute check is a MAKE-INSTANCE with its own quoted form, so
;;; SBCL compiles a constructor for each call site on its first run: ~2.5 s
;;; of scripts/test (task 262). Notinline takes the generic path instead.
(declaim (notinline make-instance))

(defparameter *earth-crash-radius* (lis::body-crash-radius (lis::body :earth)))

(defun least-bytes-consed (thunk &optional (runs 3))
  "The least of RUNS measurements of the bytes THUNK conses. SBCL counts
allocation per 32 KB region, so a run that conses a few bytes now and then
reads 32,768; a run that conses in every step reads a region or more each time."
  (declare (type function thunk) (type fixnum runs))
  (let ((least most-positive-fixnum))
    (dotimes (i runs least)
      (let ((before (sb-ext:get-bytes-consed)))
        (funcall thunk)
        (setf least (min least (- (sb-ext:get-bytes-consed) before)))))))

(define-test system-loads
  (true (find-package '#:luv-in-space))
  (true (fboundp 'lis:main))
  (true (fboundp 'lis:screenshot-main)))

(defclass stub-artifact ()
  ((label :initarg :label) (status :initarg :status) (diagnostic :initarg :diagnostic)))
(defmethod luv:live-artifact-label ((a stub-artifact)) (slot-value a 'label))
(defmethod luv:live-artifact-status ((a stub-artifact)) (slot-value a 'status))
(defmethod luv:live-artifact-diagnostic ((a stub-artifact)) (slot-value a 'diagnostic))

(defclass stub-game () ((artifacts :initarg :artifacts)))
(defmethod luv:application-live-artifacts ((game stub-game)) (slot-value game 'artifacts))

(define-test failed-live-artifacts-reported
  (let* ((game (make-instance
                'stub-game
                :artifacts (list (make-instance 'stub-artifact :label "earth" :status :failed
                                                               :diagnostic "no such shader")
                                 (make-instance 'stub-artifact :label "overlay"
                                                               :status :installed
                                                               :diagnostic nil))))
         (report (with-output-to-string (out)
                   (is = 1 (lis:report-failed-live-artifacts game out)))))
    (true (search "earth FAILED" report))
    (true (search "no such shader" report))
    (false (search "overlay" report))
    (is = 0 (lis:report-failed-live-artifacts (make-instance 'stub-game :artifacts nil)
                                              (make-broadcast-stream)))))

;;; Task 289: Profile scripts/test and bring it back under its budget. The
;;; slowest tests run in scripts/test-slow instead (the slow system's test-op
;;; runs them with its own package): none is dropped.

(defparameter *slow-test-names*
  '(rendezvous-end-to-end-tern rendezvous-plan-highport rendezvous-plan-blocked
    rendezvous-search-highport flights-approach-refused flights-approach-tern
    flights-approach-k-17 flights-approach-screenshots flights-scripts
    terminal-approach-tern round-at-every-place map-rendezvous-aim-until-the-arrival
    clock-levels-equal-direct-steps autopilot-flown-through-clock autopilot-replan
    drift-coarse-circular drift-coarse-e0.5 drift-fine-circular drift-fine-e0.5
    reentry-promoted clock-tern-beside-parked-ship
    sun-patch-twin render-cockpit-limb capture-cockpit-poses-home-head
    capture-message-age)
  "Tests of this package that scripts/test leaves to scripts/test-slow.")

(defun run-tests (&key slow)
  "Run this package's tests less *slow-test-names* (or only those, with SLOW);
signals an error when any fails."
  (let* ((names (mapcar #'symbol-name *slow-test-names*))
         (tests (remove-if-not (lambda (test)
                                 (eq (and slow t)
                                     (and (member (symbol-name (parachute:name test)) names
                                                  :test #'string=)
                                          t)))
                               (parachute:package-tests (find-package '#:luv-in-space.tests))))
         (report (parachute:test tests :report 'parachute:summary)))
    (unless (eq :passed (parachute:status report))
      (error "Tests of ~A failed" (package-name *package*)))
    report))
