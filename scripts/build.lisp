;;;; Dump the luv-in-space executable to build/luv-in-space.
;;;; sbcl --script scripts/build.lisp, from the repo root in the dev shell.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (require :sb-concurrency)
  (require :sb-posix))

(handler-case (asdf:make "luv-in-space/program")
  (error (condition)
    (format *error-output* "~&build failed: ~A~%" condition)
    (sb-ext:exit :code 1 :abort t)))
