;;;; Slynk server bootstrap for ./sly: luv's sly-server.lisp, adapted to this
;;;; checkout. luv's helper files come from luv's source in the flake input.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (require :sb-concurrency)
  (require :sb-posix))

(defparameter cl-user::*luv-project-root* nil)

(defun getenv-or (name default)
  (or (uiop:getenv name) default))

(defun required-directory (name)
  (let ((value (uiop:getenv name)))
    (unless value
      (error "~A is not set. Enter the dev shell (`nix develop`) or use ./sly." name))
    (uiop:ensure-directory-pathname value)))

(defun emit-swash-event (session event message &rest fields)
  (let ((process
          (uiop:launch-program
           (append (list (or (uiop:getenv "LUV_SWASH") (error "LUV_SWASH is not set"))
                         "emit" session "--event" event "--message" message)
                   (loop for field in fields append (list "--field" field)))
           :input nil :output nil :error-output *error-output*)))
    (unless (zerop (uiop:wait-process process))
      (error "Could not publish Swash event ~A for ~A" event session))))

(let* ((project-root (uiop:pathname-directory-pathname *load-truename*))
       (slynk-root (required-directory "LUV_SLYNK_DIR"))
       (requested-port (parse-integer (getenv-or "LUV_SLYNK_PORT" "0")))
       (session (or (uiop:getenv "SWASH_SESSION")
                    (error "sly-server.lisp must run inside a Swash session")))
       (name (getenv-or "LUV_NAME" session))
       (system (getenv-or "LUV_SLY_SYSTEM" "luv-in-space")))
  (setf cl-user::*luv-project-root* project-root)
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(namestring slynk-root))
     (:directory ,(namestring project-root))
     :inherit-configuration))
  (unless (asdf:find-system :slynk nil)
    (asdf:load-asd (merge-pathnames #P"slynk.asd" slynk-root)))
  (asdf:load-system :slynk)
  (let ((luv-root (asdf:system-source-directory "luv")))
    (load (merge-pathnames #P"scripts/sly-asdf-status.lisp" luv-root))
    (handler-bind ((style-warning #'muffle-warning))
      (asdf:load-system "luv/build/cli")))
  (labels ((build-call (name &rest arguments)
             (apply (symbol-function (find-symbol name "LUV-BUILD")) arguments))
           (deadline-exceeded-p (condition)
             (typep condition (find-symbol "DEADLINE-EXCEEDED" "LUV-BUILD"))))
    (build-call "START" project-root :system (list system) :invocation "sly boot"
                :redirect-output-p nil :report-plan-p nil)
    (handler-case
        (progn
          (asdf:load-system system)
          (when (build-call "FINISH" :done)
            (sb-ext:exit :code 1 :abort t)))
      (error (condition)
        (if (deadline-exceeded-p condition)
            (progn
              (build-call "FINISH" :deadline)
              (sb-ext:exit :code 1 :abort t))
            (progn
              (build-call "FAILED" (princ-to-string condition))
              (build-call "FINISH" :error)
              (error condition))))))
  (format t "~&Starting luv-in-space Slynk on a kernel-assigned loopback port.~%")
  (force-output)
  (let ((port (funcall (find-symbol "CREATE-SERVER" "SLYNK")
                       :interface "127.0.0.1" :port requested-port :dont-close t)))
    (emit-swash-event
     session "slynk-ready" (format nil "Lisp ~A is ready" name)
     "LUV_KIND=LISP"
     (format nil "LUV_ROOT=~A" (namestring project-root))
     (format nil "LUV_NAME=~A" name)
     (format nil "LUV_SLYNK_PORT=~D" port)
     (format nil "LUV_SLYNK_PID=~D" (sb-posix:getpid)))
    (format t "~&Slynk is ready on 127.0.0.1:~D (Swash ~A).~%" port session)
    (force-output)
    (loop (sleep 3600))))
