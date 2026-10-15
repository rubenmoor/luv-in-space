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

;;; Running a script (docs/architecture.md "Running scripts"; decision 87):
;;; flight-step-hook in the clock's hook before each step, headless
;;; (run-flight) and live alike.

(defparameter *flight-coast-limit* 1000000
  "(:warp :coast)'s highest level; milestone 3 adds its close-work caps.")
(defparameter *flight-burn-prediction-interval* 1d0
  "s of game time between run-flight's prediction-update!s while the
throttle is on: each restarts the prediction, ~1 ms (decision 108:
run-flight's prediction during burns and the hook's order in clock-step!).")

(defstruct (flight-entry (:copier nil))
  (kind :end :type (member :screenshot :expect :burn :failure :end))
  (label nil)                                   ; screenshot label, condition or reason
  (pass nil)                                    ; :expect
  (value nil)                                   ; :expect: what it measured, as text
  (view nil)                                    ; :screenshot
  (sim nil)                                     ; :screenshot: a copy
  (start nil)                                   ; :burn: t of throttle on
  (dv nil)                                      ; :burn: m/s by the odometer
  (t 0d0 :type double-float)
  (elapsed 0d0 :type double-float)              ; s since the script's start
  (altitude 0d0 :type double-float)
  (apoapsis nil)                                ; m above R⊕, osculating; nil: escape
  (periapsis 0d0 :type double-float)
  (speed 0d0 :type double-float)
  (propellant 0d0 :type double-float)
  (loss nil))

(defstruct (flight (:constructor %make-flight (script start origin)) (:copier nil))
  (script #() :type simple-vector)              ; flight-steps
  (index 0 :type fixnum)                        ; the current step
  (phase :due :type (member :due :running))     ; waiting for its time, or begun
  (start nil :type sim)                         ; the run's start state
  (origin 0d0 :type double-float)               ; the script's start t
  (previous-end 0d0 :type double-float)         ; the previous step's end
  (begun 0d0 :type double-float)                ; the current step's start
  (deadline nil)                                ; a running wait's t limit
  (until nil)                                   ; a running step's grid t
  (entries nil)                                 ; the record, newest first
  (coast nil)
  (impact-time nil)                             ; at the last (:wait :impact)'s start
  (burn nil)                                    ; an open burn: (t . odometer)
  (loss-seen nil)
  (level 1 :type fixnum)                        ; the clock's at the last hook
  (dropout nil)                                 ; t of the last drop-out to :impact
  (failure nil)                                 ; the failure entry
  (done nil)
  (restart nil)                                 ; live: (:restart)'s function, else clock-restart!
  (cache nil))                                  ; (t steps . until) of the last hook

(defun new-flight (steps sim)
  "A flight of STEPS (flight-steps) from SIM, its start state."
  (let ((flight (%make-flight (coerce steps 'simple-vector) (copy-sim sim) (sim-t sim))))
    (setf (flight-previous-end flight) (sim-t sim))
    flight))

(defun flight-current (flight)
  (let ((i (flight-index flight)) (script (flight-script flight)))
    (and (< i (length script)) (svref script i))))

;;; The record

(defun flight-entry-at (flight sim kind &rest args)
  "An entry of KIND with SIM's state, pushed on the record; the entry."
  (let* ((ship (sim-ship sim))
         (body (body (sim-body sim)))
         (r (ship-state-r ship)) (v (ship-state-v ship))
         (orbit (state->orbit r v (body-mu body) (sim-t sim)))
         (radius (body-radius body))
         (entry (apply #'make-flight-entry
                       :kind kind :t (sim-t sim)
                       :elapsed (- (sim-t sim) (flight-origin flight))
                       :altitude (- (vec3-norm r) radius)
                       :apoapsis (let ((a (orbit-apoapsis-radius orbit))) (and a (- a radius)))
                       :periapsis (- (orbit-periapsis-radius orbit) radius)
                       :speed (vec3-norm v)
                       :propellant (ship-state-m-p ship)
                       :loss (sim-loss sim)
                       args)))
    (push entry (flight-entries flight))
    entry))

(defun flight-burn-check! (flight sim &optional end)
  "Open a burn at the throttle going on, close it at its going off, a loss
or the flight's END."
  (let ((on (and (not end) (> (controls-throttle (sim-controls sim)) 0d0)))
        (burn (flight-burn flight))
        (dv (ship-state-dv (sim-ship sim))))
    (cond ((and on (not burn) (not (sim-loss sim)))
           (setf (flight-burn flight) (cons (sim-t sim) dv)))
          ((and burn (or (not on) (sim-loss sim)))
           (flight-entry-at flight sim :burn :start (car burn) :dv (- dv (cdr burn)))
           (setf (flight-burn flight) nil)))))

(defun flight-finish! (flight sim &optional reason)
  "End the flight: the open burn closed, REASON's failure, the end entry."
  (unless (flight-done flight)
    (flight-burn-check! flight sim t)
    (when reason
      (setf (flight-failure flight) (flight-entry-at flight sim :failure :label reason)))
    (flight-entry-at flight sim :end)
    (setf (flight-done flight) t)))

(defun flight-record (flight)
  "The entries in order."
  (reverse (flight-entries flight)))

(defun flight-record-failure (record)
  "The failure entry of RECORD, or nil."
  (find :failure record :key #'flight-entry-kind))

(defun flight-record-passed-p (record)
  "No failure, every expectation passed."
  (notany (lambda (e) (or (eq (flight-entry-kind e) :failure)
                          (and (eq (flight-entry-kind e) :expect) (not (flight-entry-pass e)))))
          record))

;;; Conditions

(defun km-text (m) (format nil "~,3F KM" (/ m 1000d0)))

(defun condition-value-text (name x)
  "X, an argument or a measure of condition NAME, in display units."
  (cond ((not (realp x)) (format nil "~A" x))
        ((eq name :date) (format-utc x))
        ((member name '(:altitude-below :periapsis-below :periapsis-above
                        :apoapsis-below :apoapsis-above))
         (km-text x))
        ((eq name :rate-below) (format nil "~,3F°/S" x))
        ((member name '(:propellant-above :propellant-full)) (format nil "~,1F KG" x))
        (t (format nil "~,3F S" x))))

(defun condition-text (step)
  "The condition of STEP as written, its numbers in display units."
  (let ((name (flight-step-name step)))
    (format nil "~A~{ ~A~}" name
            (mapcar (lambda (x) (condition-value-text name x)) (flight-step-args step)))))

(defun flight-apsis-altitude (clock sim kind)
  "The next :apoapsis or :periapsis above the body from CLOCK's prediction,
run to its horizon (decision 87); the osculating one when the prediction
has none (lost, or the apsis beyond its horizon); nil for an escape's
apoapsis."
  (let* ((pred (clock-prediction clock))
         (body (body (sim-body sim)))
         (radius (or (and (not (sim-loss sim))
                          (progn (prediction-run! pred sim)
                                 (nth-value 1 (prediction-apsis pred kind))))
                     (let* ((ship (sim-ship sim))
                            (orbit (state->orbit (ship-state-r ship) (ship-state-v ship)
                                                 (body-mu body) (sim-t sim))))
                       (if (eq kind :apoapsis)
                           (orbit-apoapsis-radius orbit)
                           (orbit-periapsis-radius orbit))))))
    (and radius (- radius (body-radius body)))))

(defun condition-holds (flight clock sim step)
  "(values holds measured) of STEP's condition now."
  (let* ((name (flight-step-name step))
         (x (first (flight-step-args step)))
         (ship (sim-ship sim))
         (loss (sim-loss sim)))
    (flet ((apsis (kind above)
             ;; no apoapsis: an escape, above any altitude
             (let ((h (flight-apsis-altitude clock sim kind)))
               (values (if h (if above (> h x) (< h x)) above) h))))
      (ecase name
        (:altitude-below
         (let ((h (- (vec3-norm (ship-state-r ship)) (body-radius (body (sim-body sim))))))
           (values (< h x) h)))
        (:periapsis-below (apsis :periapsis nil))
        (:periapsis-above (apsis :periapsis t))
        (:apoapsis-below (apsis :apoapsis nil))
        (:apoapsis-above (apsis :apoapsis t))
        (:crashed (values (and loss (eq (loss-cause loss) :crash) t) nil))
        (:settled (values (controls-settled (sim-controls sim)) nil))
        (:rate-below
         (let ((rate (rad->deg (vec3-norm (ship-state-w ship)))))
           (values (< rate x) rate)))
        (:date (values (>= (sim-t sim) x) (sim-t sim)))
        (:propellant-above (values (> (ship-state-m-p ship) x) (ship-state-m-p ship)))
        (:propellant-full (values (>= (ship-state-m-p ship) *propellant-full*)
                                  (ship-state-m-p ship)))
        (:drop-out (values (and (flight-dropout flight)
                                (>= (flight-dropout flight) (flight-begun flight)))
                           nil))
        (:lost (values (and loss (eq (loss-cause loss) x) t) (and loss (loss-cause loss))))
        (:impact-as-predicted
         (let ((late (and loss (flight-impact-time flight)
                          (- (loss-time loss) (flight-impact-time flight)))))
           (values (and late (<= (abs late) x)) late)))
        (:start-state (values (equalp sim (flight-start flight)) nil))))))

;;; Steps

(defun flight-step-due (flight step)
  "The t at which STEP begins."
  (if (flight-step-at step)
      (+ (flight-origin flight) (flight-step-at step))
      (+ (flight-previous-end flight) (flight-step-after step))))

(defun flight-step-end! (flight sim)
  (setf (flight-previous-end flight) (sim-t sim)
        (flight-phase flight) :due
        (flight-deadline flight) nil
        (flight-until flight) nil)
  (incf (flight-index flight)))

(defun flight-set-hold! (controls mode ship)
  "MODE held (nil: rate damping); a hold already held stays."
  (cond ((null mode)
         (unless (eq (controls-attitude-mode controls) :rate-damping)
           (setf (controls-attitude-mode controls) :rate-damping
                 (controls-settled controls) nil)))
        ((and (eq (controls-attitude-mode controls) :hold) (eq (controls-hold controls) mode)))
        ((eq mode :inertial) (controls-inertial-hold controls (ship-state-q ship)))
        (t (controls-set-hold controls mode))))

(defun flight-warp! (clock sim level)
  "Warp up or down to LEVEL; the clock's refusal text, or nil."
  (loop while (> (clock-level clock) level) do (clock-warp-down clock))
  (loop while (< (clock-level clock) level)
        do (unless (clock-warp-up clock sim)
             (return (second (assoc :refused (clock-message-list clock)))))))

(defun flight-begin! (flight clock sim step)
  "Begin STEP at its due time: an action applied (a refusal's text as the
second value), a wait set up. True when STEP is over at once."
  (let ((controls (sim-controls sim))
        (args (flight-step-args step))
        (now (sim-t sim)))
    (setf (flight-begun flight) now)
    (ecase (flight-step-kind step)
      (:pass t)
      (:expect
       (multiple-value-bind (holds value) (condition-holds flight clock sim step)
         (flight-entry-at flight sim :expect :label (condition-text step)
                                             :pass (and holds t)
                                             :value (and value (condition-value-text
                                                                (flight-step-name step) value))))
       t)
      (:wait
       (setf (flight-deadline flight) (+ now (flight-step-within step)))
       (case (flight-step-name step)
         ((:apoapsis :periapsis :impact)
          (let ((pred (clock-prediction clock))
                (kind (flight-step-name step)))
            (prediction-run! pred sim)
            (let ((time (prediction-event pred kind)))
              (unless time
                (return-from flight-begin!
                  (values nil (format nil "(:WAIT ~S): NONE PREDICTED" kind))))
              (when (eq kind :impact)
                (setf (flight-impact-time flight) time))
              (setf (flight-until flight)
                    (grid-ceiling (- time (or (flight-step-before step) 0d0)))))))
         (:date (setf (flight-until flight) (first args))))
       nil)
      (:action
       (ecase (flight-step-name step)
         (:start t)
         (:warp
          (let ((level (first args)))
            (cond ((eq level :coast) (setf (flight-coast flight) t) t)
                  (t (setf (flight-coast flight) nil)
                     (let ((refusal (flight-warp! clock sim level)))
                       (if refusal (values nil refusal) t))))))
         (:hold (flight-set-hold! controls (first args) (sim-ship sim)) t)
         (:throttle
          (controls-set-throttle controls (first args))
          (when (= (controls-throttle controls) 0d0)
            (setf (sim-cutoff sim) nil))
          t)
         (:after-dv
          (if (= (controls-throttle controls) 0d0)
              (values nil "(:AFTER-DV): THROTTLE OFF")
              (progn (setf (sim-cutoff sim) (+ (ship-state-dv (sim-ship sim)) (first args))
                           (flight-deadline flight) (+ now *flight-default-within*))
                     nil)))
         (:keys
          (controls-apply-held-keys! controls (first args))
          (setf (flight-until flight) (+ now (flight-step-for step)))
          nil)
         (:fine (setf (controls-fine controls) (first args)) t)
         (:screenshot
          (flight-entry-at flight sim :screenshot :label (first args)
                                                  :view (flight-step-view step)
                                                  :sim (copy-sim sim))
          t)
         (:restart
          (if (sim-loss sim)
              (progn (if (flight-restart flight)
                         (funcall (flight-restart flight))
                         (clock-restart! clock sim (flight-start flight)))
                     (setf (flight-loss-seen flight) nil
                           (flight-coast flight) nil
                           (flight-burn flight) nil)
                     t)
              (values nil "(:RESTART): NOT LOST"))))))))

(defun flight-running! (flight clock sim step)
  "A begun STEP: (values over until reason), REASON a failure."
  (let ((now (sim-t sim))
        (until (flight-until flight))
        (deadline (flight-deadline flight)))
    (ecase (flight-step-kind step)
      (:action
       (ecase (flight-step-name step)
         (:keys
          (cond ((>= now until)
                 (controls-apply-held-keys! (sim-controls sim) nil)
                 t)
                (t (values nil until))))
         (:after-dv
          (cond ((= (controls-throttle (sim-controls sim)) 0d0) t)
                ((>= now deadline) (values nil nil "(:AFTER-DV): NO CUT-OFF"))
                (t (values nil deadline))))))
      (:wait
       (let ((over (case (flight-step-name step)
                     ((:apoapsis :periapsis) (>= now until))
                     (:impact (or (sim-loss sim) (>= now until)))
                     (t (condition-holds flight clock sim step)))))
         (cond (over t)
               ((or (>= now deadline) (sim-loss sim))
                (values nil nil
                        (format nil "(:WAIT ~A): ~:[NOT WITHIN ~A~;LOST~*~]"
                                (condition-text step) (sim-loss sim)
                                (format-duration (flight-step-within step)))))
               (t (values nil (if until (min until deadline) deadline)))))))))

(defun loss-reason (loss)
  (format nil "LOST: ~A" (loss-cause loss)))

(defun flight-advance! (flight clock sim)
  "Every step due at SIM's t run; the until of the next step, or nil."
  (loop
    (when (flight-done flight)
      (return nil))
    (flight-burn-check! flight sim)
    (when (and (sim-loss sim) (not (flight-loss-seen flight)))
      (setf (flight-loss-seen flight) t)
      (flight-burn-check! flight sim)
      (let ((step (flight-current flight)))
        (when (and step (eq (flight-phase flight) :running) (eq (flight-step-kind step) :wait)
                   (member (flight-step-name step) '(:impact :crashed)))
          (flight-step-end! flight sim)))
      (let ((step (flight-current flight)))
        (unless (and step (eq (flight-step-kind step) :expect) (eq (flight-step-name step) :lost))
          (flight-finish! flight sim (loss-reason (sim-loss sim)))
          (return nil))))
    (let ((step (flight-current flight)))
      (unless step
        (flight-finish! flight sim)
        (return nil))
      (ecase (flight-phase flight)
        (:due
         (let ((due (flight-step-due flight step)))
           (when (< (sim-t sim) due)
             (when (sim-loss sim)
               (flight-finish! flight sim (format nil "~A: TIME CANNOT PASS"
                                                  (loss-reason (sim-loss sim))))
               (return nil))
             (return due))
           (multiple-value-bind (over refusal) (flight-begin! flight clock sim step)
             (cond (refusal (flight-finish! flight sim refusal) (return nil))
                   (over (flight-step-end! flight sim))
                   (t (setf (flight-phase flight) :running))))))
        (:running
         (multiple-value-bind (over until reason) (flight-running! flight clock sim step)
           (cond (reason (flight-finish! flight sim reason) (return nil))
                 (over (flight-step-end! flight sim))
                 (t (return until)))))))))

(defun flight-coast! (clock sim)
  "Up to the highest level the clock accepts, refusing nothing aloud."
  (loop for index = (1+ (clock-level-index clock))
        while (< index (length *warp-levels*))
        do (let ((level (nth index *warp-levels*)))
             (when (or (> level *flight-coast-limit*) (warp-refusal clock sim level))
               (return))
             (clock-warp-up clock sim))))

(defun flight-dropout-check! (flight clock sim)
  "Note a lowering by the drop-out rule since the last look."
  (let ((before (flight-level flight)))
    (when (< (clock-level clock) before)
      (multiple-value-bind (delta kind) (clock-dropout clock sim)
        (when (and delta (eq kind :impact) (< delta (* *dropout-real-time* before)))
          (setf (flight-dropout flight) (sim-t sim)))))
    (setf (flight-level flight) (clock-level clock))))

(defun flight-step-hook (flight clock sim)
  "The per-step function (decision 87): before each step of SIM under
CLOCK, the due steps of FLIGHT run, (:warp :coast) raises, the clock's
lowering rules hold again; the step's until. Called twice at one state it
runs once."
  (let ((cache (flight-cache flight)))
    (when (and cache (= (first cache) (sim-t sim)) (= (second cache) (sim-steps sim)))
      (return-from flight-step-hook (cddr cache))))
  (flight-dropout-check! flight clock sim)
  (let ((until (flight-advance! flight clock sim)))
    (unless (flight-done flight)
      (flight-burn-check! flight sim)
      (when (flight-coast flight)
        (flight-coast! clock sim))
      (clock-check-level! clock sim)
      (flight-dropout-check! flight clock sim))
    (setf (flight-cache flight) (list* (sim-t sim) (sim-steps sim) until))
    until))

(defun run-flight (script &key model sim)
  "Fly SCRIPT (read-flight's source, or a list of flight-steps) headless
from the new-game state, or from SIM (a copy; the script's :start then
ignored), with the gravity MODEL if given: per step prediction-update!,
the hook, clock-step! with no accumulator; while the throttle is on the
prediction is updated once per *flight-burn-prediction-interval*. (values
record end-sim flight)."
  (let* ((steps (if (and (listp script) (every #'flight-step-p script))
                    script
                    (read-flight script)))
         (sim (if sim (copy-sim sim) (new-game-sim))))
    (when model
      (setf (sim-model sim) model))
    (let* ((flight (new-flight steps sim))
           (clock (new-clock sim))
           (predicted (sim-t sim)))
      (setf (clock-hook clock) (lambda (clock sim) (flight-step-hook flight clock sim)))
      (loop
        (when (or (= (controls-throttle (sim-controls sim)) 0d0)
                  (>= (sim-t sim) predicted))
          (prediction-update! (clock-prediction clock) sim)
          (setf predicted (+ (sim-t sim) *flight-burn-prediction-interval*)))
        (flight-step-hook flight clock sim)
        (when (flight-done flight)
          (return))
        (unless (clock-step! clock sim)
          (when (sim-loss sim)
            (flight-step-hook flight clock sim))
          (unless (flight-done flight)
            (flight-finish! flight sim "STUCK"))))
      (values (flight-record flight) sim flight))))

;;; Lines

(defun flight-entry-state-text (entry)
  (format nil "T+~,3F S ~A ALT ~A AP ~:[ESCAPE~;~:*~A~] PE ~A SPD ~,1F M/S PROP ~,1F KG~@[ ~A~]"
          (flight-entry-elapsed entry) (format-utc (flight-entry-t entry))
          (km-text (flight-entry-altitude entry))
          (and (flight-entry-apoapsis entry) (km-text (flight-entry-apoapsis entry)))
          (km-text (flight-entry-periapsis entry))
          (flight-entry-speed entry) (flight-entry-propellant entry)
          (let ((loss (flight-entry-loss entry)))
            (and loss (format nil "~A ~A AT ~,3F S ~,3F M/S LAT ~,3F° LON ~,3F°"
                              (loss-reason loss) (loss-body loss)
                              (- (loss-time loss) (flight-entry-t entry))
                              (loss-speed loss)
                              (rad->deg (loss-latitude loss)) (rad->deg (loss-longitude loss)))))))

(defun flight-record-lines (record)
  "One line per entry of RECORD."
  (mapcar (lambda (e)
            (let ((state (flight-entry-state-text e)))
              (ecase (flight-entry-kind e)
                (:screenshot (format nil "SCREENSHOT ~A (~A): ~A"
                                     (flight-entry-label e) (flight-entry-view e) state))
                (:expect (format nil "EXPECT ~A: ~:[FAIL~;PASS~]~@[ (~A)~]: ~A"
                                 (flight-entry-label e) (flight-entry-pass e)
                                 (flight-entry-value e) state))
                (:burn (format nil "BURN FROM T+~,3F S: ~,3F M/S IN ~,3F S: ~A"
                               (- (flight-entry-start e) (- (flight-entry-t e)
                                                             (flight-entry-elapsed e)))
                               (flight-entry-dv e)
                               (- (flight-entry-t e) (flight-entry-start e))
                               state))
                (:failure (format nil "FAILED: ~A: ~A" (flight-entry-label e) state))
                (:end (format nil "END ~:[FAILED~;PASSED~]: ~A"
                              (flight-record-passed-p record) state)))))
          record))

;;; Live (decision 87): fly runs a script in the game, frame by frame through
;;; clock-frame! and clock-step!, the same steps as run-flight.

(defun flight-install! (clock flight &optional on-end)
  "CLOCK's hook runs FLIGHT; at its end the hook is removed and ON-END
called with FLIGHT and the sim (before the frame's further steps)."
  (setf (clock-hook clock)
        (lambda (clock sim) (flight-hook! flight clock sim on-end))))

(defun flight-hook! (flight clock sim on-end)
  (prog1 (flight-step-hook flight clock sim)
    (when (flight-done flight)
      (setf (clock-hook clock) nil)
      (when on-end
        (funcall on-end flight sim)))))

(defun flight-frame! (clock sim)
  "After a frame's steps: a lost SIM takes none, so the hook runs once here,
as run-flight calls it after a loss."
  (when (and (sim-loss sim) (clock-hook clock))
    (funcall (clock-hook clock) clock sim)))

(defun flight-running-p (&optional (game *game*))
  "GAME runs a flight: input-frame! leaves the controls and warp alone."
  (let ((flight (and game (game-flight game))))
    (and flight (not (flight-done flight)))))

(defun fly (name &key (game *game*))
  "Fly the script NAME (read-flight's source) in GAME from its start state,
the view outside; returns at once. At the end the record lines are printed
to the image's output; flight-status reads them."
  (let ((steps (read-flight name)))
    (call-in-game game
                  (lambda ()
                    (let ((flight (new-flight steps (new-game-sim))))
                      (setf (flight-restart flight)
                            (lambda () (game-restart! game (flight-start flight))))
                      (game-restart! game (flight-start flight))
                      (setf (game-flight game) flight)
                      (flight-install! (game-clock game) flight
                                       (lambda (flight sim)
                                         (declare (ignore sim))
                                         (format t "~&~{~A~%~}" (flight-record-lines
                                                                 (flight-record flight)))
                                         (finish-output))))))
    name))

(defun flight-status (&key (game *game*))
  "The record lines of GAME's flight so far, or nil."
  (call-in-game game
                (lambda ()
                  (let ((flight (game-flight game)))
                    (and flight (flight-record-lines (flight-record flight)))))))

(defun flight-stop (&key (game *game*))
  "Stop GAME's flight: the hook removed, the slot cleared."
  (call-in-game game
                (lambda ()
                  (when (game-flight game)
                    (setf (clock-hook (game-clock game)) nil
                          (game-flight game) nil))))
  (values))
