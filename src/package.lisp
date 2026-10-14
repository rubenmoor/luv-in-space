(defpackage #:luv-in-space
  (:use #:cl)
  (:export #:main #:start #:stop #:running-p #:*game*
           #:screenshot #:screenshot-main #:report-failed-live-artifacts))

(defpackage #:luv-in-space.shaders
  (:use #:cl #:luv.shader)
  (:shadowing-import-from #:luv.shader #:step))
