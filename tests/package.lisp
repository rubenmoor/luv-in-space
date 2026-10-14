(defpackage #:luv-in-space.tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:lis #:luv-in-space)))

(in-package #:luv-in-space.tests)

(define-test system-loads
  (true (find-package '#:luv-in-space))
  (true (fboundp 'lis:main))
  (true (fboundp 'lis:screenshot-main)))
