(in-package #:luv-in-space)

;;; Flight scripts: the safe reader and the step parser (docs/architecture.md
;;; "Scripted runs"; decision 56: Flight-script actions, start states and the
;;; quest play-through, decision 87: Flight scripts in milestone 2: additions,
;;; apsis conditions, live fly and scripts/fly). Simulation code: no HAL, no
;;; SDL.

(defparameter *flight-default-within* (* 30 +day+)
  "s of game time a wait may take without :within; docs/architecture.md \"Steps\".")

(define-condition flight-script-error (error)
  ((message :initarg :message :reader flight-script-error-message)
   (line :initarg :line :initform nil :reader flight-script-error-line))
  (:report (lambda (c stream)
             (format stream "~@[LINE ~D: ~]~A"
                     (flight-script-error-line c) (flight-script-error-message c)))))

(defun script-error (line control &rest args)
  (error 'flight-script-error :line line :message (apply #'format nil control args)))

;;; A step

(defstruct (flight-step (:copier nil))
  (line 1 :type fixnum)                         ; where the step starts in the script
  (at nil :type (or null double-float))         ; s from the script's start, on the grid
  (after 0d0 :type double-float)                ; s from the previous step's end, on the grid
  (kind :pass :type (member :pass :action :wait :expect))
  (name nil :type (or null keyword))            ; the action or the condition
  (args nil :type list)                         ; its arguments, parsed
  (within nil :type (or null double-float))     ; waits: s, on the grid
  (before nil :type (or null double-float))     ; event waits: s
  (view nil :type (member nil :outside :map))   ; :screenshot
  (for nil :type (or null double-float)))       ; :keys: s, on the grid

;;; The vocabulary (docs/architecture.md "Actions", "Waits and expectations")

(defparameter *flight-actions*
  '(:start :warp :hold :throttle :after-dv :keys :fine :screenshot :restart))

(defparameter *flight-conditions*
  '((:apoapsis :event) (:periapsis :event) (:impact :event)
    (:altitude-below :any :real) (:periapsis-below :any :real) (:periapsis-above :any :real)
    (:apoapsis-below :any :real) (:apoapsis-above :any :real)
    (:crashed :any) (:settled :any) (:rate-below :any :real) (:date :any :date)
    (:propellant-above :any :real) (:propellant-full :any)
    (:drop-out :wait :drop-out)
    (:lost :expect :cause) (:impact-as-predicted :expect :real) (:start-state :expect))
  "(condition where argument-type …); where is :event (a wait on the
prediction), :wait, :expect or :any.")

(defparameter *later-vocabulary*
  '((:action (:target . 3) (:autopilot . 3) (:grapple . 3) (:release . 3) (:transfer . 3)
     (:translate . 3) (:approach . 3) (:ack . 5))
    (:condition (:soi . 4) (:dominant . 4) (:sun-distance-below . 4) (:sun-distance-above . 4)
     (:latched . 3) (:berthed . 3) (:on-station . 3) (:removed . 3) (:hull-above . 4)
     (:hull-below . 4) (:autopilot-done . 3) (:quest-step . 5) (:quest-skipped . 5)
     (:quest-line-done . 5) (:dv-below . 3) (:date-between . 3) (:elapsed-below . 3)
     (:burn . 3) (:since-burn . 3) (:clearance-above . 3) (:aim-point . 3) (:offer . 5))
    (:start (:state . 3) (:after . 5) (:file . 5))
    (:hold (:target . 3) (:anti-target . 3) (:rel-prograde . 3) (:rel-retrograde . 3))
    (:drop-out (:burn . 3) (:contact . 3) (:soi . 4) (:sun-zone . 4) (:life-support . 4))
    (:cause (:contact . 3) (:heat . 4) (:life-support . 4)))
  "Per kind, (keyword . milestone) of what later milestones bring.")

(defparameter *flight-holds*
  '(:prograde :retrograde :normal :anti-normal :radial-out :radial-in :inertial))

(defun check-not-later (kind key line)
  "Reject KEY with \"NOT BEFORE MILESTONE <n>\" if a later milestone brings it."
  (let ((later (assoc key (rest (assoc kind *later-vocabulary*)))))
    (when later
      (script-error line "NOT BEFORE MILESTONE ~D: ~S" (cdr later) key))))

;;; Arguments

(defun script-real (x line what)
  (unless (realp x)
    (script-error line "~A WANTS A NUMBER, NOT ~S" what x))
  (float x 1d0))

(defun script-duration (x line what)
  "X s, not negative, rounded up to the 1/64 s grid."
  (let ((s (script-real x line what)))
    (when (minusp s)
      (script-error line "~A WANTS A TIME OF 0 OR MORE, NOT ~S" what x))
    (grid-ceiling s)))

(defun parse-utc (string line)
  "\"YYYY-MM-DD\" or \"YYYY-MM-DDThh:mm:ss\" as a grid t (decision 66)."
  (flet ((bad ()
           (script-error line "BAD DATE ~S: WRITE \"YYYY-MM-DD\" OR \"YYYY-MM-DDThh:mm:ss\""
                         string)))
    (unless (and (stringp string)
                 (or (= (length string) 10)
                     (and (= (length string) 19) (char= (char string 10) #\T)
                          (char= (char string 13) #\:) (char= (char string 16) #\:)))
                 (char= (char string 4) #\-) (char= (char string 7) #\-))
      (bad))
    (flet ((field (start end)
             (if (every #'digit-char-p (subseq string start end))
                 (parse-integer string :start start :end end)
                 (bad))))
      (let* ((long (= (length string) 19))
             (year (field 0 4)) (month (field 5 7)) (day (field 8 10))
             (hour (if long (field 11 13) 0))
             (minute (if long (field 14 16) 0))
             (second (if long (field 17 19) 0)))
        (unless (and (<= 1 month 12) (<= 1 day 31) (< hour 24) (< minute 60) (< second 60))
          (bad))
        (utc->grid-t year month day hour minute second)))))

(defun parse-options (list allowed line what)
  "LIST as a plist of ALLOWED keys, each at most once."
  (unless (evenp (length list))
    (script-error line "~S: OPTIONS COME IN PAIRS: ~S" what list))
  (loop for (key nil) on list by #'cddr
        for seen from 0
        do (unless (member key allowed)
             (script-error line "~S TAKES ~:[NO OPTIONS~;~:*~{~S~^, ~}~], NOT ~S"
                           what allowed key))
           (when (member key (subseq list 0 (* 2 seen)))
             (script-error line "~S: ~S GIVEN TWICE" what key)))
  list)

;;; Actions

(defun parse-action (step name args line first)
  (setf (flight-step-kind step) :action
        (flight-step-name step) name)
  (flet ((arity (n)
           (unless (= (length args) n)
             (script-error line "~S WANTS ~D ARGUMENT~:P: ~S" name n (cons name args)))))
    (setf (flight-step-args step)
          (ecase name
            (:start
             (unless first
               (script-error line ":START ONLY AS THE FIRST STEP"))
             (when (or (flight-step-at step) (plusp (flight-step-after step)))
               (script-error line ":START TAKES NO TIME"))
             (check-not-later :start (first args) line)
             (unless (equal args '(:new-game))
               (script-error line "UNKNOWN START ~S: (:START :NEW-GAME)" args))
             args)
            (:warp
             (arity 1)
             (let ((level (first args)))
               (unless (or (eq level :coast) (member level *warp-levels*))
                 (script-error line "WARP ~S: ONE OF ~{~D~^, ~} OR :COAST" level *warp-levels*))
               args))
            (:hold
             (arity 1)
             (let ((mode (first args)))
               (check-not-later :hold mode line)
               (unless (or (null mode) (member mode *flight-holds*))
                 (script-error line "UNKNOWN HOLD ~S" mode))
               args))
            (:throttle
             (arity 1)
             (let ((f (script-real (first args) line :throttle)))
               (unless (or (= f 0d0) (<= 0.1d0 f 1d0))
                 (script-error line "THROTTLE ~S: 0 OR 0.1 TO 1.0" (first args)))
               (list f)))
            (:after-dv
             (unless (and (= (length args) 3) (equal (rest args) '(:throttle 0)))
               (script-error line "WRITE (:AFTER-DV <M/S> :THROTTLE 0)"))
             (let ((dv (script-real (first args) line :after-dv)))
               (unless (plusp dv)
                 (script-error line ":AFTER-DV WANTS A Δv ABOVE 0, NOT ~S" (first args)))
               (list dv)))
            (:keys
             (let ((keys (first args)))
               (unless (and (listp keys) keys)
                 (script-error line "WRITE (:KEYS (<KEY> …) :FOR <S>)"))
               (dolist (key keys)
                 (unless (assoc key *flight-keys*)
                   (script-error line "UNKNOWN KEY ~S: ONE OF ~{~S~^ ~}" key
                                 (mapcar #'first *flight-keys*))))
               (let ((options (parse-options (rest args) '(:for) line :keys)))
                 (unless (getf options :for)
                   (script-error line ":KEYS WANTS :FOR <S>"))
                 (setf (flight-step-for step)
                       (script-duration (getf options :for) line :for)))
               (list keys)))
            (:fine
             (arity 1)
             (unless (member (first args) '(t nil))
               (script-error line ":FINE WANTS T OR NIL, NOT ~S" (first args)))
             args)
            (:screenshot
             (let ((label (first args)))
               (unless (and (stringp label) (plusp (length label)))
                 (script-error line ":SCREENSHOT WANTS A LABEL, A STRING"))
               (let* ((options (parse-options (rest args) '(:view) line :screenshot))
                      (view (getf options :view :outside)))
                 (unless (member view '(:outside :map))
                   (script-error line ":VIEW ~S: :OUTSIDE OR :MAP" view))
                 (setf (flight-step-view step) view))
               (list label)))
            (:restart
             (arity 0)
             nil)))))

;;; Waits and expectations

(defun parse-condition (step kind args line)
  (let* ((name (first args))
         (spec (assoc name *flight-conditions*)))
    (unless (keywordp name)
      (script-error line "~S WANTS A CONDITION, NOT ~S" kind name))
    (check-not-later :condition name line)
    (unless spec
      (script-error line "UNKNOWN CONDITION ~S" name))
    (destructuring-bind (where &rest types) (rest spec)
      (when (or (and (eq kind :wait) (eq where :expect))
                (and (eq kind :expect) (member where '(:wait :event))))
        (script-error line "~S IS NOT ~:[AN EXPECTATION~;A WAIT~]" name (eq kind :wait)))
      (let ((given (rest args)))
        (when (< (length given) (length types))
          (script-error line "~S WANTS ~D ARGUMENT~:P" name (length types)))
        (setf (flight-step-kind step) kind
              (flight-step-name step) name
              (flight-step-args step)
              (loop for type in types
                    for x in given
                    collect (ecase type
                              (:real (script-real x line name))
                              (:date (parse-utc x line))
                              (:drop-out
                               (check-not-later :drop-out x line)
                               (unless (eq x :impact)
                                 (script-error line "UNKNOWN DROP-OUT ~S" x))
                               x)
                              (:cause
                               (check-not-later :cause x line)
                               (unless (eq x :crash)
                                 (script-error line "UNKNOWN CAUSE ~S" x))
                               x))))
        (let ((options (parse-options (nthcdr (length types) given)
                                      (cond ((eq kind :expect) nil)
                                            ((eq where :event) '(:within :before))
                                            (t '(:within)))
                                      line kind)))
          (when (eq kind :wait)
            (setf (flight-step-within step)
                  (script-duration (getf options :within *flight-default-within*) line :within))
            (when (getf options :before)
              (setf (flight-step-before step)
                    (let ((s (script-real (getf options :before) line :before)))
                      (when (minusp s)
                        (script-error line ":BEFORE WANTS 0 OR MORE, NOT ~S" s))
                      s)))))))
    step))

;;; A step

(defun parse-flight-step (form line first)
  "The flight-step of FORM, the script's step at LINE (FIRST: the first)."
  (unless (and (consp form) (null (cdr (last form))))
    (script-error line "A STEP IS A NON-EMPTY LIST, NOT ~S" form))
  (let ((step (make-flight-step :line line))
        (rest form))
    (when (member (first rest) '(:at :after))
      (let ((key (pop rest)))
        (unless rest
          (script-error line "~S WANTS A TIME" key))
        (let ((s (script-duration (pop rest) line key)))
          (if (eq key :at)
              (setf (flight-step-at step) s)
              (setf (flight-step-after step) s)))))
    (when (null rest)
      (return-from parse-flight-step step))
    (let ((head (first rest)))
      (cond ((member head '(:at :after))
             (script-error line "ONE :AT OR :AFTER PER STEP"))
            ((member head '(:wait :expect))
             (parse-condition step head (rest rest) line))
            ((member head *flight-actions*)
             (parse-action step head (rest rest) line first))
            (t
             (check-not-later :action head line)
             (check-not-later :condition head line)
             (script-error line "UNKNOWN STEP ~S" head))))
    step))

;;; The safe reader (decision 87)

(defun safe-readtable ()
  "The standard syntax with # rejected: no #., #S, #P or #| |#."
  (let ((readtable (copy-readtable nil)))
    (set-macro-character #\# (lambda (stream char)
                               (declare (ignore stream char))
                               (error 'flight-script-error
                                      :message "# IS NOT ALLOWED IN A SCRIPT"))
                         t readtable)
    readtable))

(defun check-symbols (form line)
  "Reject any symbol in FORM but keywords, T and NIL."
  (cond ((consp form)
         (check-symbols (car form) line)
         (check-symbols (cdr form) line))
        ((and (symbolp form) (not (keywordp form)) (not (member form '(t nil))))
         (script-error line "~A IS NOT A KEYWORD: WRITE :~:*~A" (symbol-name form)))
        ((not (or (symbolp form) (realp form) (stringp form)))
         (script-error line "~S IS NOT ALLOWED IN A SCRIPT" form))))

(defun skip-blank (stream)
  "Skip whitespace and ; comments; the next character, nil at the end."
  (loop for c = (peek-char t stream nil nil)
        do (if (eql c #\;)
               (read-line stream nil)
               (return c))))

(defun read-script-forms (text)
  "The steps of TEXT, one list of steps, as ((line . form) …)."
  (let ((package (make-package (string (gensym "FLIGHT-SCRIPT-")) :use nil)))
    (unwind-protect
         (with-standard-io-syntax
           (let ((*read-eval* nil)
                 (*read-default-float-format* 'double-float)
                 (*readtable* (safe-readtable))
                 (*package* package))
             (import (list t nil) package)
             (with-input-from-string (in text)
               (flet ((line () (1+ (count #\Newline text :end (file-position in)))))
                 (unless (eql (skip-blank in) #\()
                   (script-error (line) "A SCRIPT IS ONE LIST OF STEPS"))
                 (read-char in)
                 (let ((forms '()))
                   (loop
                     (let ((c (skip-blank in))
                           (line (line)))
                       (cond ((null c)
                              (script-error line "THE LIST OF STEPS IS NOT CLOSED"))
                             ((char= c #\))
                              (read-char in)
                              (return))
                             (t
                              (let ((form (handler-case (read in)
                                            (flight-script-error (e)
                                              (script-error line "~A"
                                                            (flight-script-error-message e)))
                                            (end-of-file ()
                                              (script-error line "A STEP IS NOT CLOSED"))
                                            (error (e)
                                              (script-error line "READ ERROR: ~A" e)))))
                                (check-symbols form line)
                                (push (cons line form) forms))))))
                   (when (skip-blank in)
                     (script-error (line) "A SECOND FORM AFTER THE LIST OF STEPS"))
                   (nreverse forms))))))
      (delete-package package))))

(defun flight-script-pathname (name)
  "flights/<NAME>.sexp in the system's source directory."
  (unless (and (plusp (length name))
               (every (lambda (c) (or (char<= #\a c #\z) (digit-char-p c) (char= c #\-))) name))
    (script-error nil "BAD SCRIPT NAME ~S: a-z, 0-9 AND -" name))
  (asdf:system-relative-pathname "luv-in-space" (format nil "flights/~A.sexp" name)))

(defun flight-script-text (source)
  "The text of SOURCE: a pathname, a string starting with ( or ; (the
script itself) or a name (flights/<name>.sexp)."
  (let ((pathname (cond ((pathnamep source) source)
                        ((member (find-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline)))
                                              source)
                                 '(#\( #\;))
                         nil)
                        (t (flight-script-pathname source)))))
    (cond ((null pathname) source)
          ((probe-file pathname) (uiop:read-file-string pathname :external-format :utf-8))
          (t (script-error nil "NO SCRIPT ~A" (namestring pathname))))))

(defun read-flight (source)
  "The flight-steps of SOURCE (flight-script-text's), read safely and
parsed; a flight-script-error naming the line for anything else."
  (loop for (line . form) in (read-script-forms (flight-script-text source))
        for first = t then nil
        collect (parse-flight-step form line first)))
