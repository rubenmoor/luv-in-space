;;;; The ./sly command client: luv's sly-client from the flake input, loaded
;;;; as is, with the parts tied to luv's checkout and games redefined for this
;;;; one. scripts/build-sly-client dumps it to build/sly-client.

(require :asdf)
(asdf:load-system "sly-client")

(in-package #:sly-client)

(defparameter *luv-root* (asdf:system-source-directory "luv")
  "luv's source in the store, fixed when the client is built.")

(defun ensure-sly-dependency-core ()
  "luv's dependency-core builder run on this checkout: Slynk and everything
outside it (luv and its dependencies) in build/sly-dependencies.core."
  (let* ((core (merge-pathnames #P"build/sly-dependencies.core" *project-root*))
         (stamp (merge-pathnames #P"build/sly-dependencies.environment" *project-root*))
         (environment
           (format nil "~{~A~%~}"
                   (append (list (namestring *luv-root*))
                           (mapcar #'uiop:getenv
                                   '("LUV_SLYNK_DIR" "LUV_SLY_SYSTEM" "CL_SOURCE_REGISTRY"
                                     "ASDF_OUTPUT_TRANSLATIONS" "LUV_NATIVE_LIBRARY_PATH")))))
         (asd (merge-pathnames #P"luv-in-space.asd" *project-root*)))
    (unless (and (probe-file core)
                 (probe-file stamp)
                 (string= environment (uiop:read-file-string stamp))
                 (> (file-write-date core) (file-write-date asd)))
      (let ((temporary (format nil "~A.tmp" (namestring core)))
            (log (merge-pathnames #P"build/sly-dependency-core-build.log" *project-root*)))
        (format t "Building cached Sly dependency core...~%")
        (force-output)
        (sb-posix:setenv "LUV_SLY_DEPENDENCY_CORE_OUTPUT" temporary 1)
        (unless (zerop (sb-ext:process-exit-code
                        (sb-ext:run-program
                         "sbcl"
                         (list "--noinform" "--non-interactive" "--disable-debugger"
                               "--load" (namestring (merge-pathnames
                                                     #P"scripts/sly-dependency-core.lisp"
                                                     *luv-root*)))
                         :search t :directory (namestring *project-root*)
                         :input nil :output log :if-output-exists :supersede
                         :error :output)))
          (error "Could not build the Sly dependency core; see ~A" log))
        (rename-file temporary core)
        (with-open-file (out stamp :direction :output :if-exists :supersede)
          (write-string environment out))))
    core))

(defparameter *game-status-form*
  "(let* ((game (and (find-package :luv-in-space)
                     (symbol-value (find-symbol \"*GAME*\" :luv-in-space))))
          (health (and game
                       (ignore-errors
                        (luv:canvas-health
                         (funcall (find-symbol \"GAME-CANVAS\" :luv-in-space) game))))))
     (princ (if game :luv-in-space :idle))
     (when health (format t \" ~S\" health)))")

(defun print-game-status ()
  (let ((answer (ignore-errors
                 (with-slynk-connection (stream)
                   (authenticate stream)
                   (evaluate-captured-output-on stream *game-status-form* "CL-USER")))))
    (format t "~A~%"
            (cond ((null answer) "Game state unknown (image busy or not loaded).")
                  ((search "LUV-IN-SPACE" (string-upcase (princ-to-string answer)))
                   "luv-in-space is playing: ./sly screenshot PNG; ./sly stop-playing closes it.")
                  (t "Nothing is playing: ./sly play starts it.")))
    (when answer
      (print-canvas-health (princ-to-string answer)))
    (when (and answer (search "LUV-IN-SPACE" (string-upcase (princ-to-string answer))))
      (print-failed-live-artifacts))))

(defun print-failed-live-artifacts ()
  "Shaders that failed to rebuild: luv keeps the last good pipeline silently."
  (let ((report (ignore-errors
                 (with-slynk-connection (stream)
                   (authenticate stream)
                   (evaluate-captured-output-on
                    stream "(luv-in-space:report-failed-live-artifacts)" "CL-USER")))))
    (when (plusp (length report))
      (write-result-string report))))

(defun run-play (arguments)
  (when arguments
    (error "play does not accept arguments"))
  (ensure-server)
  (evaluate "(progn
               (when luv-in-space:*game*
                 (error \"luv-in-space is already playing; ./sly stop-playing first.\"))
               (luv-in-space:start)
               :playing)"
            "CL-USER"))

(defun run-stop-playing (arguments)
  (when arguments
    (error "stop-playing does not accept arguments"))
  (ensure-server)
  (evaluate "(luv-in-space:stop)" "CL-USER"))

(defun run-screenshot (arguments)
  (unless (= (length arguments) 1)
    (error "screenshot requires exactly one PNG pathname"))
  (let ((pathname (merge-pathnames (pathname (first arguments)) (truename "."))))
    (evaluate (format nil "(progn
                             (unless luv-in-space:*game*
                               (error \"Nothing is playing: ./sly play first.\"))
                             (multiple-value-bind (path width height)
                                 (luv-in-space:screenshot ~S)
                               (list (namestring (truename path)) width height)))"
                      pathname)
              "CL-USER")))

(defvar *luv-usage* (fdefinition 'usage))

(defun usage (&optional (stream *standard-output*))
  (let ((text (with-output-to-string (out) (funcall *luv-usage* out))))
    (dolist (edit '(("play [luvcraft|luft] [--fullscreen]" . "play")
                    ("Prefix a client command with --luvcraft only to attach to a separate
standalone build/luvcraft process.
" . "")))
      (let ((start (search (car edit) text)))
        (when start
          (setf text (concatenate 'string (subseq text 0 start) (cdr edit)
                                  (subseq text (+ start (length (car edit)))))))))
    (write-string text stream)))

(defun luv-in-space-entry-point ()
  "The checkout is the working directory ./sly changes into."
  (setf *project-root* (uiop:ensure-directory-pathname (uiop:getcwd)))
  (entry-point))
