(defpackage #:luv-in-space.tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:lis #:luv-in-space)))

(in-package #:luv-in-space.tests)

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
