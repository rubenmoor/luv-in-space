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
  (view nil :type (member nil :outside :map :cockpit)) ; :screenshot
  (for nil :type (or null double-float)))       ; :keys: s, on the grid

;;; The vocabulary (docs/architecture.md "Actions", "Waits and expectations")

(defparameter *flight-actions*
  '(:start :warp :hold :throttle :after-dv :keys :fine :screenshot :restart
    :target :grapple :release :transfer :translate :autopilot))

(defparameter *flight-conditions*
  '((:apoapsis :event) (:periapsis :event) (:impact :event)
    (:altitude-below :any :real) (:periapsis-below :any :real) (:periapsis-above :any :real)
    (:apoapsis-below :any :real) (:apoapsis-above :any :real)
    (:crashed :any) (:settled :any) (:rate-below :any :real) (:date :any :date)
    (:propellant-above :any :real) (:propellant-full :any)
    (:drop-out :wait :drop-out)
    (:lost :any :cause) (:impact-as-predicted :expect :real) (:start-state :expect)
    (:latched :any &optional :object) (:berthed :any :fixture) (:on-station :any :station)
    (:removed :any :object &optional :removal) (:date-between :expect :date :date)
    (:autopilot-done :any) (:dv-below :expect :real) (:elapsed-below :expect :real)
    (:burn :expect :label :between :date :date) (:since-burn :expect :label :below :real)
    (:clearance-above :expect :real) (:aim-point :expect :aim))
  "(condition where argument-type … [&optional type …]); where is :event (a
wait on the prediction), :wait, :expect or :any.")

(defparameter *flight-condition-options*
  '((:clearance-above :except :only :until))
  "Options an expectation takes beyond its arguments, kept as a plist after them.")

(defparameter *burn-labels*
  '(:departure :circularise :apsis :plane :mcc :capture :phasing :arrival :terminal :trim)
  "The autopilot's burn labels (docs/architecture.md \"Waits and expectations\").")

(defparameter *later-vocabulary*
  '((:action (:approach . 3) (:ack . 5))
    (:condition (:soi . 4) (:dominant . 4) (:sun-distance-below . 4) (:sun-distance-above . 4)
     (:hull-above . 4) (:hull-below . 4) (:quest-step . 5)
     (:quest-skipped . 5) (:quest-line-done . 5) (:offer . 5))
    (:start (:after . 5) (:file . 5))
    (:state-key (:consumables . 4) (:quest . 5))
    (:body (:moon . 4) (:sun . 4))
    (:preset (:tli . 4) (:capture . 4) (:return . 4))
    (:drop-out (:soi . 4) (:sun-zone . 4) (:life-support . 4))
    (:cause (:heat . 4) (:life-support . 4)))
  "Per kind, (keyword . milestone) of what later milestones bring.")

(defparameter *flight-holds*
  '(:prograde :retrograde :normal :anti-normal :radial-out :radial-in :inertial
    :target :anti-target :rel-prograde :rel-retrograde))

(defparameter *flight-target-holds*
  '((:target . :target) (:anti-target . :anti-target)
    (:rel-prograde . :relative-prograde) (:rel-retrograde . :relative-retrograde))
  "(script hold . controls-set-target-hold's kind): keys 7–0.")

(defparameter *flight-translations*
  '((:fwd . :h) (:aft . :n) (:up . :i) (:down . :k) (:left . :j) (:right . :l))
  "(:translate axis . its key in *flight-keys*).")

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

;;; Start states (docs/architecture.md "Start states"; decision 122: Flight
;;; scripts in milestone 3: autopilot steps, the record, close work, start
;;; states). check-start-state reads (:start :state PLIST) into a normal
;;; form, stated-sim builds the sim from it.

(defun point-key-p (key)
  "KEY names a place or an object."
  (or (find key *places* :key #'place-key) (find key *objects* :key #'object-def-key)))

(defun point-rails (key)
  "The rails of place or object KEY: its object's definition's, or the
place's own (Skua Yard's drop point)."
  (let ((place (find key *places* :key #'place-key)))
    (if (and place (null (place-object place)))
        (place-rails place)
        (object-def-rails (object-definition (if place (place-object place) key))))))

(defun state-point (x line)
  (unless (and (keywordp x) (point-key-p x))
    (script-error line "UNKNOWN PLACE ~S" x))
  x)

(defun state-fixture (x line)
  "(object-key fixture-index) of fixture X: (<place> <fixture>) or a load's
id for its one fixture."
  (flet ((object-of (key)
           (let ((place (find key *places* :key #'place-key)))
             (cond ((and place (place-object place)))
                   (place (script-error line "~S HAS NO FIXTURE" key))
                   ((find key *objects* :key #'object-def-key) key)
                   (t (script-error line "UNKNOWN PLACE ~S" key))))))
    (cond ((keywordp x)
           (let* ((key (object-of x))
                  (fixtures (object-def-fixtures (object-definition key))))
             (unless (= (length fixtures) 1)
               (script-error line "~S HAS ~D FIXTURES: WRITE (~S <FIXTURE>)" x (length fixtures) x))
             (list key 0)))
          ((and (consp x) (= (length x) 2) (keywordp (first x)) (keywordp (second x)))
           (let* ((key (object-of (first x)))
                  (index (position (second x) (object-def-fixtures (object-definition key))
                                   :key #'fixture-name :test #'string-equal)))
             (unless index
               (script-error line "UNKNOWN FIXTURE ~S" x))
             (list key index)))
          (t (script-error line "A FIXTURE IS (<PLACE> <FIXTURE>) OR A LOAD, NOT ~S" x)))))

(defun rails-apsis-after (rails kind time)
  "The first :periapsis or :apoapsis passage on RAILS at or after TIME (M 0
or π at the segment's mean rate)."
  (let* ((segment (rails-segment-at rails time))
         (rate (rails-segment-mean-rate segment))
         (m (+ (rails-segment-mean-anomaly segment)
               (* rate (- time (rails-segment-start segment)))))
         (goal (if (eq kind :periapsis) 0d0 pi)))
    (+ time (/ (mod (- goal m) +two-pi+) rate))))

(defun state-date (x line)
  "Date X as a grid t: a UTC string or (:periapsis|:apoapsis <place>
[:before s] [:after UTC])."
  (let ((time (cond ((stringp x) (parse-utc x line))
                    ((and (consp x) (member (first x) '(:periapsis :apoapsis)) (rest x))
                     (let* ((key (state-point (second x) line))
                            (options (parse-options (cddr x) '(:before :after) line (first x)))
                            (after (if (getf options :after)
                                       (parse-utc (getf options :after) line)
                                       (start-t)))
                            (before (script-duration (getf options :before 0) line :before)))
                       (grid-ceiling (- (rails-apsis-after (point-rails key) (first x) after)
                                        before))))
                    (t (script-error line "BAD :DATE ~S" x)))))
    (when (< time (start-t))
      (script-error line ":DATE ~A BEFORE THE START" (format-utc time)))
    time))

(defun state-orbit (x line)
  "Orbit X in normal form: (:circular a u lead) with u in rad or nil and
lead nil or (place rad); (:near place how m); (:on-axis object fixture m);
(:elements a e i Ω ω M), metres and radians."
  (unless (and (consp x) (keywordp (first x)) (null (cdr (last x))))
    (script-error line "BAD :ORBIT ~S" x))
  (destructuring-bind (kind &rest args) x
    (case kind
      (:circular
       (unless args
         (script-error line "WRITE (:CIRCULAR <ALT> [:U <°>] [:LEAD (<PLACE> <°>)])"))
       (let ((altitude (script-real (first args) line :circular))
             (options (parse-options (rest args) '(:u :lead) line :circular)))
         (unless (> altitude 0d0)
           (script-error line ":CIRCULAR WANTS AN ALTITUDE ABOVE 0, NOT ~S" (first args)))
         (when (and (getf options :u) (getf options :lead))
           (script-error line ":CIRCULAR TAKES :U OR :LEAD, NOT BOTH"))
         (let ((lead (getf options :lead)))
           (when lead
             (unless (and (consp lead) (= (length lead) 2))
               (script-error line "WRITE :LEAD (<PLACE> <°>)"))
             (setf lead (list (state-point (first lead) line)
                              (deg->rad (script-real (second lead) line :lead)))))
           (list :circular (+ +radius-earth+ altitude)
                 (and (getf options :u) (deg->rad (script-real (getf options :u) line :u)))
                 lead))))
      (:near
       (let ((key (state-point (first args) line)))
         (cond ((equal (rest args) '(:at-slot)) (list :near key :at-slot 0d0))
               ((and (= (length args) 3) (member (second args) '(:behind :above)))
                (let ((m (script-real (third args) line (second args))))
                  (when (minusp m)
                    (script-error line "~S WANTS 0 M OR MORE, NOT ~S" (second args) (third args)))
                  (list :near key (second args) m)))
               (t (script-error line "WRITE (:NEAR <PLACE> :BEHIND <M>), :ABOVE <M> OR :AT-SLOT")))))
      (:on-axis
       (unless (= (length args) 2)
         (script-error line "WRITE (:ON-AXIS <FIXTURE> <M>)"))
       (let ((m (script-real (second args) line :on-axis)))
         (when (minusp m)
           (script-error line ":ON-AXIS WANTS 0 M OR MORE, NOT ~S" (second args)))
         (list* :on-axis (append (state-fixture (first args) line) (list m)))))
      (:elements
       (check-not-later :body (first args) line)
       (unless (eq (first args) :earth)
         (script-error line ":ELEMENTS AROUND :EARTH, NOT ~S" (first args)))
       (unless (= (length args) 7)
         (script-error line "WRITE (:ELEMENTS :EARTH A E I Ω ω M)"))
       (destructuring-bind (a e &rest angles)
           (mapcar (lambda (y) (script-real y line :elements)) (rest args))
         (unless (and (> a 0d0) (<= 0d0 e) (< e 1d0))
           (script-error line ":ELEMENTS WANTS A > 0 AND 0 ≤ E < 1"))
         (list* :elements a e (mapcar #'deg->rad angles))))
      (t (script-error line "UNKNOWN ORBIT ~S" kind)))))

(defun station-point-p (x)
  "X is a place without a fixture (Skua Yard): on station by its stop point."
  (let ((place (and (keywordp x) (find x *places* :key #'place-key))))
    (and place (null (place-object place)))))

(defun target-key-p (key)
  "KEY is a place or a towable object: what ] and [ can target."
  (or (find key *places* :key #'place-key)
      (let ((def (find key *objects* :key #'object-def-key)))
        (and def (eq (object-def-kind def) :towable)))))

(defun check-start-state (plist line)
  "PLIST of (:start :state PLIST) checked and in normal form: (:date t
:orbit … :berthed (object index) :grappled key :propellant kg :removed
((key cause) …) :target key), each nil when not given."
  (unless (and (listp plist) (null (cdr (last plist))))
    (script-error line ":STATE WANTS A LIST OF KEYS AND VALUES, NOT ~S" plist))
  (loop for (key nil) on plist by #'cddr
        do (check-not-later :state-key key line))
  (let* ((options (parse-options plist '(:date :orbit :berthed :grappled :propellant :removed :target)
                                 line :state))
         (date (let ((x (getf options :date))) (and x (state-date x line))))
         (orbit (let ((x (getf options :orbit))) (and x (state-orbit x line))))
         (berthed (let ((x (getf options :berthed))) (and x (state-fixture x line))))
         (grappled (let ((x (getf options :grappled)))
                     (and x (first (state-fixture x line)))))
         (propellant (let ((x (getf options :propellant)))
                       (and x (script-real x line :propellant))))
         (removed (getf options :removed))
         (target (getf options :target)))
    (when (and berthed orbit)
      (script-error line ":BERTHED REPLACES :ORBIT: NOT BOTH"))
    (when (and berthed grappled)
      (script-error line ":BERTHED OR :GRAPPLED, NOT BOTH"))
    (when berthed
      (unless (eq (fixture-kind (nth (second berthed) (object-def-fixtures
                                                        (object-definition (first berthed)))))
                  :berth)
        (script-error line ":BERTHED WANTS A BERTH FIXTURE, NOT ~S" (getf options :berthed))))
    (when grappled
      (unless (eq (object-def-kind (object-definition grappled)) :towable)
        (script-error line ":GRAPPLED WANTS A LOAD, NOT ~S" grappled))
      (when (eq (first orbit) :on-axis)
        (script-error line ":GRAPPLED TAKES NO :ON-AXIS"))
      (when (and (eq (first orbit) :near) (eq (third orbit) :at-slot)
                 (not (eq (second orbit) grappled)))
        (script-error line ":GRAPPLED ~S WANTS (:NEAR ~S :AT-SLOT)" grappled grappled)))
    (when (and propellant (not (<= 0d0 propellant *propellant-full*)))
      (script-error line ":PROPELLANT 0 TO ~,0F KG, NOT ~S" *propellant-full* (getf options :propellant)))
    (unless (listp removed)
      (script-error line "WRITE :REMOVED ((<ID> <CAUSE>) …)"))
    (dolist (pair removed)
      (unless (and (consp pair) (= (length pair) 2))
        (script-error line "WRITE :REMOVED ((<ID> <CAUSE>) …), NOT ~S" pair))
      (unless (and (keywordp (first pair)) (find (first pair) *objects* :key #'object-def-key))
        (script-error line "UNKNOWN OBJECT ~S" (first pair)))
      (unless (member (second pair) '(:burned-up :delivered))
        (script-error line "UNKNOWN CAUSE ~S: :BURNED-UP OR :DELIVERED" (second pair)))
      (when (> (count (first pair) removed :key #'first) 1)
        (script-error line "~S REMOVED TWICE" (first pair))))
    (when (and target (not (and (keywordp target) (target-key-p target))))
      (script-error line "UNKNOWN TARGET ~S" target))
    ;; What the state uses must be there.
    (flet ((object-of (key)
             (let ((place (find key *places* :key #'place-key)))
               (if place (place-object place) key))))
      (dolist (key (list (and (member (first orbit) '(:near :on-axis)) (second orbit))
                         (and (eq (first orbit) :circular) (first (fourth orbit)))
                         (first berthed) grappled target))
        (when (and key (assoc (object-of key) removed))
          (script-error line "~S IS REMOVED" key)))
      (when (and target grappled (eq (object-of target) grappled))
        (script-error line ":TARGET ~S IS LATCHED" target)))
    (list :date date :orbit orbit :berthed berthed :grappled grappled :propellant propellant
          :removed removed :target target)))

(defun point-state (sim key)
  "(values r v q w) of place or object KEY at SIM's t: its object's state
(w inertial), or the drop point's rails (q, w nil)."
  (let ((place (find key *places* :key #'place-key)))
    (if (and place (null (place-object place)))
        (let ((r (make-vec3)) (v (make-vec3)))
          (rails-state-into! r v (rails-segment-at (place-rails place) (sim-t sim)) (sim-t sim))
          (values r v nil nil))
        (let ((r (make-vec3)) (v (make-vec3)) (q (make-quat)) (w (make-vec3)))
          (unless (object-state-into! r v q w (sim-object sim (if place (place-object place) key))
                                      (sim-t sim))
            (script-error nil "~S HAS NO STATE AT ~A" key (format-utc (sim-t sim))))
          (values r v q (quat-rotate q w))))))

(defun polar-u (r)
  "The argument of latitude of R in the common polar plane (i 90°, Ω 270°):
the ascending node along −y, the plane's normal along −x."
  (atan (aref r 2) (- (aref r 1))))

(defun stated-orbit-state (sim orbit)
  "(values r v q) of the ship's CoM for ORBIT (normal form) at SIM's t; Q
nil for the prograde hold's attitude."
  (let ((time (sim-t sim)))
    (ecase (first orbit)
      (:circular
       (destructuring-bind (a u lead) (rest orbit)
         (let ((u (cond (lead (- (polar-u (point-state sim (first lead))) (second lead)))
                        (u)
                        (t 0d0))))
           ;; Directly in the plane: an epoch's mean anomaly at t ~1e9 s
           ;; would round u by ~1e-10 rad.
           (let ((s (sqrt (/ +mu-earth+ a))))
             (values (vec3 0 (* a (- (cos u))) (* a (sin u)))
                     (vec3 0 (* s (sin u)) (* s (cos u)))
                     nil)))))
      (:near
       (destructuring-bind (key how m) (rest orbit)
         (multiple-value-bind (r v) (point-state sim key)
           (values (ecase how
                     (:at-slot r)
                     (:behind (vec3-add-scaled r (vec3-normalize v) (- m)))
                     (:above (vec3-add-scaled r (vec3-normalize r) m)))
                   v nil))))
      (:on-axis
       (destructuring-bind (key index m) (rest orbit)
         (on-axis-state sim key index m)))
      (:elements
       (destructuring-bind (a e i raan argp mean-anomaly) (rest orbit)
         (multiple-value-bind (r v)
             (orbit-state (make-elliptic-orbit +mu-earth+ a e i raan argp mean-anomaly time) time)
           (values r v nil)))))))

(defun on-axis-state (sim key index m)
  "(values r v q) of the ship's CoM with the grapple's tip M m out along
object KEY's INDEXth fixture's axis: +x along the inward axis, +z the
object's +z made perpendicular to it (its +y when under 1e-6), at the
object's velocity there."
  (multiple-value-bind (r-o v-o q-o w-o) (point-state sim key)
    (let* ((fixture (nth index (object-def-fixtures (object-definition key))))
           (axis (quat-rotate q-o (fixture-axis fixture)))
           (tip (vec3-add-scaled (vec3-add r-o (quat-rotate q-o (fixture-pin fixture))) axis m))
           (x (vec3-scale axis -1d0))
           (z (flet ((across (b)
                       (let ((b (quat-rotate q-o b)))
                         (vec3-add-scaled b x (- (vec3-dot b x))))))
                (let ((z (across (vec3 0 0 1))))
                  (vec3-normalize (if (< (vec3-norm z) 1d-6) (across (vec3 0 1 0)) z)))))
           (q (quat-from-basis x (vec3-cross z x) z))
           (com (mass-properties-com (ship-mass-properties (ship-state-m-p (sim-ship sim)) nil)))
           (r (vec3-sub tip (quat-rotate q (vec3-sub *grapple-tip* com)))))
      (values r (vec3-add v-o (vec3-cross w-o (vec3-sub r r-o))) q))))

(defun stated-ship! (sim r v q)
  "The ship's CoM at R, V; attitude Q at rest and rate damping, or for nil
the prograde hold's attitude and rate, held and settled."
  (let ((ship (sim-ship sim))
        (controls (sim-controls sim)))
    (replace (ship-state-r ship) r)
    (replace (ship-state-v ship) v)
    (if q
        (progn (replace (ship-state-q ship) q)
               (vec3-set! (ship-state-w ship) 0d0 0d0 0d0)
               (setf (controls-attitude-mode controls) :rate-damping
                     (controls-settled controls) nil))
        (multiple-value-bind (q w-t) (hold-attitude :prograde r v 0d0)
          (replace (ship-state-q ship) q)
          (replace (ship-state-w ship) (quat-rotate (quat-conjugate q) w-t))))
    sim))

(defun stated-berth! (sim key index)
  "Berthed at object KEY's INDEXth fixture (plan 24's berth-capture! at the
pose: the tip on the pin, the claws closed)."
  (let ((object (sim-object sim key)))
    (object-promote! object (sim-t sim))
    (multiple-value-bind (r v q) (on-axis-state sim key index 0d0)
      (stated-ship! sim r v q))
    (berth-capture! sim (position object (sim-objects sim)) index)
    (setf (grapple-state (sim-grapple sim)) :latched)
    sim))

(defun stated-grapple! (sim key orbit)
  "Load KEY latched at the capture pose (stack-design-pose): with (:near KEY
:at-slot) the load at its rails state and attitude, else the stack's CoM on
ORBIT (the ship's own without) in the prograde hold's attitude."
  (let* ((object (sim-object sim key))
         (index (position object (sim-objects sim)))
         (pose (stack-design-pose (object-def-of object)))
         (grapple (sim-grapple sim))
         (at-slot (and (eq (first orbit) :near) (eq (third orbit) :at-slot))))
    (multiple-value-bind (r-l v-l q-l w-l) (and at-slot (point-state sim key))
      (setf (grapple-state grapple) :latched
            (grapple-object grapple) index
            (grapple-fixture grapple) 0
            (object-status object) :latched
            (object-disturbed object) t)
      (replace (grapple-com grapple) (stack-load-com pose))
      (replace (grapple-q grapple) (stack-load-q pose))
      (let ((com (with-stack-properties (properties sim)
                   (vec3-copy (mass-properties-com properties)))))
        (cond (at-slot
               (let* ((q (quat-mul q-l (quat-conjugate (stack-load-q pose))))
                      (r (vec3-add r-l (quat-rotate q (vec3-sub com (stack-load-com pose)))))
                      (ship (sim-ship sim)))
                 (stated-ship! sim r (vec3-add v-l (vec3-cross w-l (vec3-sub r r-l))) q)
                 (replace (ship-state-w ship) (quat-rotate (quat-conjugate q) w-l))))
              (orbit
               (multiple-value-bind (r v) (stated-orbit-state sim orbit)
                 (stated-ship! sim r v nil)))
              (t (let ((ship (sim-ship sim)))
                   (stated-ship! sim (vec3-copy (ship-state-r ship)) (vec3-copy (ship-state-v ship))
                                 nil))))))
    (latched-load-update! sim)
    sim))

(defun stated-sim (plist)
  "The sim of (:start :state PLIST) (docs/architecture.md \"Start states\"):
new-game-sim at the date, the objects fresh on their definition rails, the
removals recorded at the date, the ship placed, berthed or grappled, the
target set, the objects' first checks due at once; a flight-script-error for
a bad PLIST. The same PLIST gives equalp sims."
  (destructuring-bind (&key date orbit berthed grappled propellant removed target)
      (check-start-state plist nil)
    (let ((sim (new-game-sim)))
      (when date
        (setf (sim-t sim) date))
      (setf (sim-objects sim) (map 'simple-vector #'new-object *objects*))
      (when propellant
        (setf (ship-state-m-p (sim-ship sim)) propellant))
      (loop for (key cause) in removed
            do (object-remove! sim (sim-object sim key) (sim-t sim) cause))
      (cond (berthed (apply #'stated-berth! sim berthed))
            (grappled (stated-grapple! sim grappled orbit))
            (orbit (multiple-value-call #'stated-ship! sim (stated-orbit-state sim orbit))))
      (when target
        (sim-set-target! sim target))
      (setf (sim-notices sim) nil
            (sim-checkpoint-request sim) nil)
      (sim-recheck-objects! sim))))

(defun flight-start-sim (steps)
  "The start state of STEPS: (:start :state …)'s stated-sim, else
new-game-sim."
  (let ((first (first steps)))
    (if (and first (eq (flight-step-name first) :start) (eq (first (flight-step-args first)) :state))
        (stated-sim (second (flight-step-args first)))
        (new-game-sim))))

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
             (cond ((equal args '(:new-game)) args)
                   ((and (eq (first args) :state) (= (length args) 2))
                    (check-start-state (second args) line)
                    args)
                   (t (script-error line "UNKNOWN START ~S: (:START :NEW-GAME) OR (:START :STATE (…))"
                                    args))))
            (:warp
             (arity 1)
             (let ((level (first args)))
               (unless (or (eq level :coast) (member level *warp-levels*))
                 (script-error line "WARP ~S: ONE OF ~{~D~^, ~} OR :COAST" level *warp-levels*))
               args))
            (:hold
             (arity 1)
             (let ((mode (first args)))
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
                 (unless (member view '(:outside :map :cockpit))
                   (script-error line ":VIEW ~S: :OUTSIDE, :MAP OR :COCKPIT" view))
                 (setf (flight-step-view step) view))
               (list label)))
            (:restart
             (arity 0)
             nil)
            (:target
             (arity 1)
             (unless (and (keywordp (first args)) (target-key-p (first args)))
               (script-error line "UNKNOWN TARGET ~S" (first args)))
             args)
            (:grapple
             (arity 1)
             (unless (member (first args) '(:arm :stow))
               (script-error line ":GRAPPLE :ARM OR :STOW, NOT ~S" (first args)))
             args)
            (:release
             (arity 0)
             nil)
            (:transfer
             (arity 1)
             (unless (member (first args) '(:on :off))
               (script-error line ":TRANSFER :ON OR :OFF, NOT ~S" (first args)))
             args)
            (:translate
             (unless (and (= (length args) 3) (eq (second args) :dv))
               (script-error line "WRITE (:TRANSLATE <AXIS> :DV <M/S>)"))
             (unless (assoc (first args) *flight-translations*)
               (script-error line "UNKNOWN AXIS ~S: ONE OF ~{~S~^ ~}" (first args)
                             (mapcar #'first *flight-translations*)))
             (let ((dv (script-real (third args) line :translate)))
               (unless (plusp dv)
                 (script-error line ":TRANSLATE WANTS A Δv ABOVE 0, NOT ~S" (third args)))
               (list (first args) dv)))
            (:autopilot (parse-autopilot args line))))))

(defun parse-autopilot (args line)
  "(:autopilot …)'s arguments in normal form: (:off), or (preset goal range
limit) as autopilot-request! takes them, metres, radians and seconds."
  (when (equal args '(:off))
    (return-from parse-autopilot args))
  (let ((preset (first args)))
    (check-not-later :preset preset line)
    (flet ((altitude (x)
             (let ((m (script-real x line preset)))
               (unless (plusp m)
                 (script-error line "~S WANTS AN ALTITUDE ABOVE 0, NOT ~S" preset x))
               m))
           (no-options (rest)
             (when rest
               (script-error line "~S TAKES NO OPTIONS, NOT ~S" preset rest))))
      (case preset
        (:circularise
         (no-options (cddr args))
         (unless (member (second args) '(:ap :pe))
           (script-error line ":CIRCULARISE :AP OR :PE, NOT ~S" (second args)))
         (list :circularise (if (eq (second args) :ap) :apoapsis :periapsis) nil nil))
        ((:set-ap :set-pe)
         (unless (rest args)
           (script-error line "WRITE (:AUTOPILOT ~S <ALT>)" preset))
         (no-options (cddr args))
         (list (if (eq preset :set-ap) :set-apoapsis :set-periapsis) (altitude (second args)) nil nil))
        (:hohmann
         (unless (rest args)
           (script-error line "WRITE (:AUTOPILOT :HOHMANN <ALT> [:INC <°>])"))
         (let ((inc (getf (parse-options (cddr args) '(:inc) line :hohmann) :inc)))
           (list :change-orbit (list (altitude (second args))
                                     (and inc (deg->rad (script-real inc line :inc))))
                 nil nil)))
        (:plane
         (no-options (cddr args))
         (let ((x (second args)))
           (list :change-plane
                 (if (eq x :target) :target (deg->rad (script-real x line :plane)))
                 nil nil)))
        (:rendezvous
         (let ((spec (second args))
               (options (parse-options (cddr args) '(:range :limit) line :rendezvous)))
           (unless (and (keywordp spec) (target-key-p spec))
             (state-fixture spec line))
           (let ((range (getf options :range))
                 (limit (getf options :limit)))
             (when range
               (setf range (script-real range line :range))
               (unless (<= 100d0 range 1000d0)
                 (script-error line ":RANGE 100 TO 1,000 M, NOT ~S" (getf options :range))))
             (when limit
               (setf limit (script-real limit line :limit))
               (unless (and (<= *rendezvous-limit-step* limit *rendezvous-limit-max*)
                            (zerop (mod limit *rendezvous-limit-step*)))
                 (script-error line ":LIMIT IN STEPS OF ~D S FROM ~D TO ~D, NOT ~S"
                               (round *rendezvous-limit-step*) (round *rendezvous-limit-step*)
                               (round *rendezvous-limit-max*) (getf options :limit))))
             (list :rendezvous spec range limit))))
        (t (script-error line "UNKNOWN PRESET ~S" preset))))))

;;; Waits and expectations

(defun clearance-options (options line)
  "(:clearance-above …)'s OPTIONS checked: :except and :only lists of
objects (a place for its object), not both; :until :aim-point."
  (when (and (getf options :except) (getf options :only))
    (script-error line ":CLEARANCE-ABOVE TAKES :EXCEPT OR :ONLY, NOT BOTH"))
  (let ((until (getf options :until)))
    (when (and until (not (eq until :aim-point)))
      (script-error line ":UNTIL :AIM-POINT, NOT ~S" until))
    (flet ((objects (x)
             (unless (and (listp x) x (null (cdr (last x))))
               (script-error line "WRITE (<ID> …), NOT ~S" x))
             (mapcar (lambda (key)
                       (let ((place (and (keywordp key) (find key *places* :key #'place-key))))
                         (cond ((and place (place-object place)))
                               (place (script-error line "~S IS NO OBJECT" key))
                               ((and (keywordp key) (find key *objects* :key #'object-def-key)) key)
                               (t (script-error line "UNKNOWN OBJECT ~S" key)))))
                     x)))
      (list :except (and (getf options :except) (objects (getf options :except)))
            :only (and (getf options :only) (objects (getf options :only)))
            :until until))))

(defun parse-condition (step kind args line)
  (let* ((name (first args))
         (spec (assoc name *flight-conditions*)))
    (unless (keywordp name)
      (script-error line "~S WANTS A CONDITION, NOT ~S" kind name))
    (check-not-later :condition name line)
    (unless spec
      (script-error line "UNKNOWN CONDITION ~S" name))
    (destructuring-bind (where &rest spec-types) (rest spec)
      (when (or (and (eq kind :wait) (eq where :expect))
                (and (eq kind :expect) (member where '(:wait :event))))
        (script-error line "~S IS NOT ~:[AN EXPECTATION~;A WAIT~]" name (eq kind :wait)))
      (let* ((given (rest args))
             (extra (rest (assoc name *flight-condition-options*)))
             (allowed (append extra
                              (cond ((eq kind :expect) nil)
                                    ((eq where :event) '(:within :before))
                                    (t '(:within)))))
             (required (ldiff spec-types (member '&optional spec-types)))
             ;; An optional argument is there unless an option's key stands in its place.
             (types (append required
                            (loop for type in (rest (member '&optional spec-types))
                                  for x in (nthcdr (length required) given)
                                  until (member x allowed)
                                  collect type))))
        (when (< (length given) (length required))
          (script-error line "~S WANTS ~D ARGUMENT~:P" name (length required)))
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
                               (unless (member x '(:impact :burn :contact))
                                 (script-error line "UNKNOWN DROP-OUT ~S" x))
                               x)
                              (:cause
                               (check-not-later :cause x line)
                               (unless (member x '(:crash :contact))
                                 (script-error line "UNKNOWN CAUSE ~S" x))
                               x)
                              (:object
                               (unless (and (keywordp x) (find x *objects* :key #'object-def-key))
                                 (script-error line "UNKNOWN OBJECT ~S" x))
                               x)
                              (:removal
                               (unless (member x '(:burned-up :delivered :impact))
                                 (script-error line "UNKNOWN CAUSE ~S: :BURNED-UP, :DELIVERED OR :IMPACT" x))
                               x)
                              (:fixture (state-fixture x line) x)
                              (:station
                               (unless (station-point-p x)
                                 (state-fixture x line))
                               x)
                              (:label
                               (unless (member x *burn-labels*)
                                 (script-error line "UNKNOWN BURN ~S: ONE OF ~{~S~^ ~}" x *burn-labels*))
                               x)
                              ((:between :below)
                               (unless (eq x type)
                                 (script-error line "~S WANTS ~S, NOT ~S" name type x))
                               x)
                              (:aim
                               (unless (member x '(1 2 3))
                                 (script-error line ":AIM-POINT 1, 2 OR 3, NOT ~S" x))
                               x))))
        (let ((options (parse-options (nthcdr (length types) given) allowed line kind)))
          (when extra
            (setf (flight-step-args step)
                  (append (flight-step-args step) (list (clearance-options options line)))))
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
  "(:warp :coast)'s highest level away from close work (decision 122).")
(defparameter *flight-close-distance* 1000d0
  "m beyond a promoted object's bound within which (:warp :coast) stops at ×4; decision 122.")
(defparameter *flight-burn-prediction-interval* 1d0
  "s of game time between run-flight's prediction-update!s above ×4 while
the prediction would restart (the throttle on): each restart costs ~1 ms
(decision 108: run-flight's prediction during burns and the hook's order in
clock-step!).")
(defparameter *flight-close-prediction-interval* 10d0
  "s of game time between run-flight's prediction-update!s at ×1 and ×4
while the prediction would restart (decision 122): every step is fine there
whatever the prediction says.")
(defparameter *flight-translate-limit* 600d0
  "s a (:translate …) may take; decision 122.")
(defparameter *on-station-distance* 150d0 "m: docs/story.md \"On station at F\".")
(defparameter *on-station-speed* 0.10d0 "m/s: docs/story.md \"On station at F\".")

(defstruct (flight-entry (:copier nil))
  (kind :end :type (member :screenshot :expect :burn :failure :end))
  (label nil)                                   ; screenshot label, condition, reason or burn label
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
  (loss nil)
  (target nil)                                  ; the target's name, or nil
  (range nil))                                  ; m to the target's CoM, or nil

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
  (dropout nil)                                 ; (t . kind) of the last drop-out
  (translate nil)                               ; a running (:translate …): the odometer to reach
  (failure nil)                                 ; the failure entry
  (done nil)
  (restart nil)                                 ; live: (:restart)'s function, else clock-restart!
  (cache nil)                                   ; (t steps . until) of the last hook
  (headless nil)                                ; run-flight: planning jobs run at once
  (request nil)                                 ; a running (:autopilot …): its snapshot's t
  (engage nil)                                  ; the last engage's flight-engage
  (plan-burn nil))                              ; an open plan burn: (what label start . odometer)

;;; The last engage (decision 122): the reference of the record's
;;; expectations, and the clearance kept since.

(defstruct (flight-engage (:constructor %make-flight-engage (time dv plan mark least frozen previous flags))
                          (:copier nil))
  (time 0d0 :type double-float)
  (dv 0d0 :type double-float)                   ; the odometer
  (plan nil)                                    ; the plan engaged, then its replans
  (mark nil)                                    ; the SHIP log's newest entry before it
  (least nil :type (simple-array double-float (*))) ; per object: the least CoM distance
  (frozen nil)                                  ; least at the :arrival burn's start, or nil
  (previous nil :type (simple-array double-float (*))) ; the last hook's ship, then objects: r v
  (flags nil :type simple-vector)               ; per object: promoted at the last hook
  (previous-t nil))                             ; that hook's t, or nil

(defun make-flight-engage (clock sim)
  "SIM's plan just engaged under CLOCK: the reference t, odometer, plan and
SHIP-log mark, no clearance yet."
  (let ((n (length (sim-objects sim))))
    (%make-flight-engage (sim-t sim) (ship-state-dv (sim-ship sim)) (sim-plan sim)
                         (first (clock-log clock))
                         (make-array n :element-type 'double-float
                                       :initial-element most-positive-double-float)
                         nil
                         (make-array (* 6 (1+ n)) :element-type 'double-float :initial-element 0d0)
                         (make-array n :initial-element nil))))

(defun clearance-skipped-p (sim object)
  "OBJECT counts for no clearance: removed or the latched load."
  (or (object-removed-p object) (eq object (sim-held-object sim))))

(defun flight-clearance-update! (engage sim)
  "At a hook: ENGAGE's least distances lowered over the step since the last
hook, per object promoted at both ends (not removed nor the load) on the
relative Hermite of the two states (hermite-least); the states kept for the
next. Objects on rails stay ≥ 100 km away (promotion's invariant): not
measured. Conses nothing."
  (let* ((objects (sim-objects sim))
         (n (length objects))
         (previous (flight-engage-previous engage))
         (flags (flight-engage-flags engage))
         (least (flight-engage-least engage))
         (t0 (flight-engage-previous-t engage))
         (ship (sim-ship sim)))
    (when (and t0 (> (sim-t sim) t0))
      (let ((h (- (sim-t sim) t0))
            (c (make-array 12 :element-type 'double-float))
            (p0 (make-vec3)) (u0 (make-vec3)) (p1 (make-vec3)) (u1 (make-vec3)))
        (declare (dynamic-extent c p0 u0 p1 u1))
        (dotimes (i n)
          (let ((object (svref objects i)))
            (when (and (svref flags i) (object-promoted-p object)
                       (not (clearance-skipped-p sim object)))
              (let ((k (* 6 (1+ i)))
                    (state (object-state object)))
                (dotimes (j 3)
                  (setf (aref p0 j) (- (aref previous (+ k j)) (aref previous j))
                        (aref u0 j) (- (aref previous (+ k 3 j)) (aref previous (+ 3 j)))
                        (aref p1 j) (- (aref (ship-state-r state) j) (aref (ship-state-r ship) j))
                        (aref u1 j) (- (aref (ship-state-v state) j) (aref (ship-state-v ship) j))))
                (hermite-set! c p0 u0 p1 u1 h)
                (setf (aref least i) (min (aref least i) (hermite-least c 0d0 1d0)))))))))
    (flet ((keep! (k state)
             (replace previous (ship-state-r state) :start1 k)
             (replace previous (ship-state-v state) :start1 (+ k 3))))
      (keep! 0 ship)
      (dotimes (i n)
        (let* ((object (svref objects i))
               (promoted (and (object-promoted-p object) t)))
          (setf (svref flags i) promoted)
          (when promoted
            (keep! (* 6 (1+ i)) (object-state object))))))
    (setf (flight-engage-previous-t engage) (sim-t sim))
    engage))

(defun flight-engage-update! (flight sim)
  "At a hook before any step begins: the clearance since the last hook; the
:arrival burn's start freezes a copy for :until :aim-point; a replan's plan
kept."
  (let ((engage (flight-engage flight)))
    (when engage
      (flight-clearance-update! engage sim)
      (let ((plan (sim-plan sim)))
        (when plan
          (setf (flight-engage-plan engage) plan))
        (when (and plan (eq plan (flight-engage-plan engage)) (null (flight-engage-frozen engage))
                   (or (plan-terminal-p sim)
                       (let ((burn (plan-burn-due sim)))
                         (and burn (eq (planned-burn-label burn) :arrival)))))
          (setf (flight-engage-frozen engage) (copy-seq (flight-engage-least engage))))))))

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
         (target (sim-target sim))
         (target-r (make-vec3)) (target-v (make-vec3))
         (entry (apply #'make-flight-entry
                       :kind kind :t (sim-t sim)
                       :elapsed (- (sim-t sim) (flight-origin flight))
                       :altitude (- (vec3-norm r) radius)
                       :apoapsis (let ((a (orbit-apoapsis-radius orbit))) (and a (- a radius)))
                       :periapsis (- (orbit-periapsis-radius orbit) radius)
                       :speed (vec3-norm v)
                       :propellant (ship-state-m-p ship)
                       :loss (sim-loss sim)
                       :target (and target (target-name target))
                       :range (and target (target-state-into! target-r target-v sim)
                                   (vec3-norm (vec3-sub target-r r)))
                       args)))
    (push entry (flight-entries flight))
    entry))

(defun plan-running-part (sim)
  "What SIM's plan flies now for the record (decision 122): its current
planned-burn from its start (plan-burn-due) until the cursor leaves it, its
rendezvous-part while :terminal; else nil."
  (let ((plan (sim-plan sim)))
    (when plan
      (case (sim-plan-phase sim)
        (:terminal (burn-plan-rendezvous plan))
        ((:holding :burning)
         (let ((burn (svref (burn-plan-burns plan) (sim-plan-index sim))))
           (and (or (eq (sim-plan-phase sim) :burning)
                    (> (sim-plan-resume sim) 0d0)
                    (>= (sim-t sim) (planned-burn-start burn)))
                burn)))))))

(defun flight-plan-burn-check! (flight sim end)
  "While a plan is engaged, one :burn entry per planned burn with its
label (its start, the cursor leaving it, Δv by the odometer) and one
:terminal from the :arrival cut to on station; closed at a loss or END."
  (let ((open (flight-plan-burn flight))
        (part (and (not end) (not (sim-loss sim)) (plan-running-part sim)))
        (dv (ship-state-dv (sim-ship sim))))
    (when (and open (not (eq part (first open))))
      (destructuring-bind (label start . odometer) (rest open)
        (flight-entry-at flight sim :burn :label label :start start :dv (- dv odometer)))
      (setf (flight-plan-burn flight) nil))
    (when (and part (not (flight-plan-burn flight)))
      (setf (flight-plan-burn flight)
            (if (typep part 'planned-burn)
                (list* part (planned-burn-label part) (sim-t sim) dv)
                (list* part :terminal (sim-plan-terminal-t sim) dv))))))

(defun flight-burn-check! (flight sim &optional end)
  "Open a burn at the throttle going on, close it at its going off, a loss
or the flight's END; while a plan is engaged its burns instead
(flight-plan-burn-check!)."
  (flight-plan-burn-check! flight sim end)
  (let ((on (and (not end) (null (sim-plan sim)) (> (controls-throttle (sim-controls sim)) 0d0)))
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
  (cond ((and (consp x) (eq name :clearance-above))
         (format nil "~{~S ~S~^ ~}" (loop for (key value) on x by #'cddr
                                           when value collect key and collect value)))
        ((not (realp x)) (format nil "~A" x))
        ((member name '(:date :date-between :burn)) (format-utc x))
        ((eq name :aim-point) (format nil "~D" x))
        ((eq name :dv-below) (format nil "~,3F M/S" x))
        ((eq name :clearance-above) (format nil "~,1F M" x))
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
            (remove "" (mapcar (lambda (x) (condition-value-text name x)) (flight-step-args step))
                    :test #'string=))))

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

(defun sim-held-object (sim)
  "The object the grapple holds, latched or berthed, or nil."
  (let ((grapple (sim-grapple sim)))
    (and (eq (grapple-state grapple) :latched) (grapple-object grapple)
         (svref (sim-objects sim) (grapple-object grapple)))))

(defun on-station-measure (sim station)
  "(values distance speed) of docs/story.md's \"On station at F\" for
STATION (a fixture as state-fixture reads it): the grapple's tip from the
pin, the tip's velocity relative to the object's rigid motion there (its
frame's, so a stop point turning with the object keeps still); for a place
without a fixture (Skua Yard) the stack's CoM against its stop point
(rendezvous-fixture, decision 121). Nil when the object has no state."
  (let* ((ship (sim-ship sim))
         (r (ship-state-r ship)) (v (ship-state-v ship))
         (time (sim-t sim)))
    (if (station-point-p station)
        (let ((p (make-vec3)) (u (make-vec3)))
          (when (stop-point-into! p u sim station (rendezvous-fixture sim station) 0d0 time)
            (values (vec3-norm (vec3-sub p r)) (vec3-norm (vec3-sub u v)))))
        (destructuring-bind (key index) (state-fixture station nil)
          (let ((fixture (nth index (object-def-fixtures (object-definition key))))
                (r-o (make-vec3)) (v-o (make-vec3)) (q-o (make-quat)) (w-o (make-vec3)))
            (when (object-state-into! r-o v-o q-o w-o (sim-object sim key) time)
              (let* ((pin (vec3-add r-o (quat-rotate q-o (fixture-pin fixture))))
                     (com (with-stack-properties (properties sim)
                            (vec3-copy (mass-properties-com properties))))
                     (arm (quat-rotate (ship-state-q ship) (vec3-sub *grapple-tip* com)))
                     (tip (vec3-add r arm))
                     (tip-v (vec3-add v (vec3-cross (quat-rotate (ship-state-q ship) (ship-state-w ship))
                                                    arm)))
                     (frame-v (vec3-add v-o (vec3-cross (quat-rotate q-o w-o) (vec3-sub tip r-o)))))
                (values (vec3-norm (vec3-sub tip pin)) (vec3-norm (vec3-sub tip-v frame-v))))))))))

(defun engage-log (flight clock)
  "CLOCK's SHIP log entries since the last engage's mark, oldest first."
  (let* ((log (clock-log clock))
         (mark (flight-engage-mark (flight-engage flight)))
         (end (and mark (position mark log :test #'eq))))
    (reverse (if end (subseq log 0 end) log))))

(defun autopilot-outcome (flight clock sim)
  "(values outcome text) of the plan engaged last, from the SHIP log since
the engage (decision 122): :done at \"AUTOPILOT: PLAN COMPLETE\" or, after
\"ON STATION: …\", while it keeps station; :failed at its abort, \"AUTOPILOT:
GOAL MISSED\" or a disengage, or with nothing engaged; else nil."
  (unless (flight-engage flight)
    (return-from autopilot-outcome (values :failed "NOTHING ENGAGED")))
  (let ((station nil))
    (loop for (nil key text) in (engage-log flight clock)
          do (cond ((and (eq key :autopilot) (string= text "AUTOPILOT: PLAN COMPLETE"))
                    (return-from autopilot-outcome (values :done text)))
                   ((and (eq key :autopilot)
                         (or (string= text "AUTOPILOT: GOAL MISSED")
                             (eql 0 (search "AUTOPILOT ABORT" text))
                             (eql 0 (search "AUTOPILOT OFF" text))))
                    (return-from autopilot-outcome (values :failed text)))
                   ((eq key :station) (setf station text))))
    (if (and station (sim-plan sim) (eq (sim-plan-phase sim) :station))
        (values :done station)
        (values nil station))))

(defun record-plan-burn (flight label)
  "(values start entry) of the first burn of LABEL since the last engage:
in the record, else the open one."
  (let* ((since (flight-engage-time (flight-engage flight)))
         (entry (find-if (lambda (e) (and (eq (flight-entry-kind e) :burn)
                                          (eq (flight-entry-label e) label)
                                          (>= (flight-entry-start e) since)))
                         (flight-entries flight) :from-end t))
         (open (flight-plan-burn flight)))
    (cond (entry (values (flight-entry-start entry) entry))
          ((and open (eq (second open) label)) (values (third open) nil)))))

(defun clearance-least (flight sim options)
  "(values distance key) of the least clearance since the last engage over
the objects OPTIONS (:except, :only, :until) admit; nil distance when none
was measured (every object stayed on rails, ≥ 100 km)."
  (destructuring-bind (&key except only until) options
    (let* ((engage (flight-engage flight))
           (least (if (eq until :aim-point) (flight-engage-frozen engage) (flight-engage-least engage)))
           (best nil) (key nil))
      (when least
        (loop for object across (sim-objects sim)
              for d across least
              for id = (object-key object)
              do (when (and (< d most-positive-double-float)
                            (not (member id except))
                            (or (null only) (member id only))
                            (not (clearance-skipped-p sim object))
                            (or (null best) (< d best)))
                   (setf best d key id))))
      (values best key))))

(defun record-condition (flight sim step)
  "(values holds measured) of an expectation on the record since the last
engage (decision 122); failed with \"NOTHING ENGAGED\" before any."
  (let ((engage (flight-engage flight))
        (args (flight-step-args step)))
    (unless engage
      (return-from record-condition (values nil "NOTHING ENGAGED")))
    (ecase (flight-step-name step)
      (:dv-below
       (let ((dv (- (ship-state-dv (sim-ship sim)) (flight-engage-dv engage))))
         (values (< dv (first args)) (format nil "~,3F M/S" dv))))
      (:elapsed-below
       (let ((elapsed (- (sim-t sim) (flight-engage-time engage))))
         (values (< elapsed (first args)) (format-duration elapsed))))
      (:burn
       (destructuring-bind (label between from to) args
         (declare (ignore between))
         (let ((start (record-plan-burn flight label)))
           (if start
               (values (<= from start to) (format-utc start))
               (values nil (format nil "NO ~A BURN" label))))))
      (:since-burn
       (destructuring-bind (label below s) args
         (declare (ignore below))
         (let ((start (record-plan-burn flight label)))
           (if start
               (values (< (- (sim-t sim) start) s) (format-duration (- (sim-t sim) start)))
               (values nil (format nil "NO ~A BURN" label))))))
      (:clearance-above
       (destructuring-bind (m options) args
         (if (and (eq (getf options :until) :aim-point) (null (flight-engage-frozen engage)))
             (values nil "NO ARRIVAL BURN")
             (multiple-value-bind (d key) (clearance-least flight sim options)
               (values (or (null d) (> d m))
                       (if d (format nil "~A ~,3F M" key d) "NONE PROMOTED"))))))
      (:aim-point
       (let ((aim (rendezvous-plan-aim-point (flight-engage-plan engage))))
         (values (eql aim (first args)) (if aim (format nil "~D" aim) "NO RENDEZVOUS")))))))

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
        (:drop-out (let ((dropout (flight-dropout flight)))
                     (values (and dropout (>= (car dropout) (flight-begun flight))
                                  (eq (cdr dropout) x))
                             nil)))
        (:latched
         (let ((object (sim-held-object sim)))
           (values (and object (not (sim-berthed sim)) (or (null x) (eq (object-key object) x)))
                   (and object (object-key object)))))
        (:berthed
         (let ((object (sim-held-object sim)))
           (values (and object (sim-berthed sim)
                        (equal (list (object-key object) (grapple-fixture (sim-grapple sim)))
                               (state-fixture x nil)))
                   nil)))
        (:on-station
         (multiple-value-bind (distance speed) (on-station-measure sim x)
           (values (and distance (<= distance *on-station-distance*) (<= speed *on-station-speed*))
                   (and distance (format nil "~,1F M ~,3F M/S" distance speed)))))
        (:removed
         (let* ((object (sim-object sim x))
                (cause (and (object-removed-p object) (removal-cause (object-removal object)))))
           (values (and cause (or (null (second (flight-step-args step)))
                                  (eq cause (second (flight-step-args step)))))
                   cause)))
        (:date-between
         (values (<= x (sim-t sim) (second (flight-step-args step))) (sim-t sim)))
        (:lost (values (and loss (eq (loss-cause loss) x) t) (and loss (loss-cause loss))))
        (:impact-as-predicted
         (let ((late (and loss (flight-impact-time flight)
                          (- (loss-time loss) (flight-impact-time flight)))))
           (values (and late (<= (abs late) x)) late)))
        (:start-state (values (equalp sim (flight-start flight)) nil))
        (:autopilot-done
         (multiple-value-bind (outcome text) (autopilot-outcome flight clock sim)
           (values (eq outcome :done) text)))
        ((:dv-below :elapsed-below :burn :since-burn :clearance-above :aim-point)
         (record-condition flight sim step))))))

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

(defun flight-key-result (refusal)
  "A key's REFUSAL text as flight-begin!'s values: over, or failed with it."
  (if refusal (values nil refusal) t))

(defun flight-pilot-input! (clock sim)
  "A pilot's key disengages an engaged autopilot first (decision 118)."
  (when (sim-plan sim)
    (autopilot-disengage! clock sim t)))

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
         (:hold
          (flight-pilot-input! clock sim)
          (let ((kind (cdr (assoc (first args) *flight-target-holds*))))
            (cond ((null kind) (flight-set-hold! controls (first args) (sim-ship sim)) t)
                  ((and (controls-target-hold-p controls) (eq (controls-hold controls) kind)) t)
                  (t (flight-key-result (sim-target-hold! sim kind))))))
         (:target
          (let ((key (first args)))
            (if (target-available-p sim key)
                (progn (sim-set-target! sim key) t)
                (values nil (format nil "NO TARGET ~A" key)))))
         (:grapple
          (let ((state (grapple-state (sim-grapple sim))))
            (cond ((eq state (if (eq (first args) :arm) :armed :stowed)) t)
                  ((member state '(:stowed :armed)) (flight-key-result (grapple-key! sim)))
                  (t (values nil (format nil "(:GRAPPLE ~S): ~:[~A~;BERTHED~*~], (:RELEASE) FIRST"
                                         (first args) (sim-berthed sim) state))))))
         (:release
          (if (member (grapple-state (sim-grapple sim)) '(:closing :latched))
              (flight-key-result (grapple-key! sim))
              (values nil "(:RELEASE): NOTHING HELD")))
         (:transfer
          (if (eq (sim-transfer sim) (eq (first args) :on))
              t
              (flight-key-result (transfer-key! sim))))
         (:translate
          (if (sim-berthed sim)
              (values nil *engine-locked-message*)
              (progn
                (flight-pilot-input! clock sim)
                (flight-warp! clock sim 1)
                (controls-apply-held-keys! controls
                                           (list (cdr (assoc (first args) *flight-translations*))))
                (setf (flight-translate flight) (+ (ship-state-dv (sim-ship sim)) (second args))
                      (flight-deadline flight) (+ now *flight-translate-limit*))
                nil)))
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
         (:autopilot
          (let ((model (clock-autopilot clock)))
            (if (equal args '(:off))
                (progn (autopilot-key! model clock sim :backspace) t)
                (destructuring-bind (preset goal range limit) args
                  (autopilot-request! model clock sim preset goal :range range :limit limit)
                  (setf (flight-request flight) now)
                  (multiple-value-bind (over until reason) (flight-autopilot-step flight clock sim)
                    (declare (ignore until))
                    (if reason (values nil reason) over))))))
         (:restart
          (if (sim-loss sim)
              (progn (if (flight-restart flight)
                         (funcall (flight-restart flight))
                         (clock-restart! clock sim (flight-start flight)))
                     (setf (flight-loss-seen flight) nil
                           (flight-coast flight) nil
                           (flight-burn flight) nil
                           (flight-translate flight) nil
                           (flight-request flight) nil
                           (flight-engage flight) nil
                           (flight-plan-burn flight) nil)
                     t)
              (values nil "(:RESTART): NOT LOST"))))))))

(defun flight-autopilot-step (flight clock sim)
  "A requested (:autopilot …) (decision 122): (values over until reason).
Headless its job runs to completion at once, live over frames; once the plan
is shown and t ≥ the snapshot's + *plan-barrier* the second Return engages
it, and the flight keeps the engage (flight-engage). A refusal fails with
the game's text."
  (let* ((model (clock-autopilot clock))
         (barrier (+ (flight-request flight) *plan-barrier*)))
    (when (and (flight-headless flight) (autopilot-planning-p model))
      (autopilot-job-work! (autopilot-model-job model) most-positive-fixnum)
      (autopilot-job-poll! model clock sim))
    (when (and (eq (autopilot-model-state model) :shown) (>= (sim-t sim) barrier))
      (autopilot-confirm! model clock sim)
      (when (and (flight-headless flight) (autopilot-planning-p model))
        ;; stale: re-planned from now, engaged once shown
        (autopilot-job-work! (autopilot-model-job model) most-positive-fixnum)
        (autopilot-job-poll! model clock sim)))
    (case (autopilot-model-state model)
      (:refused
       (setf (flight-request flight) nil)
       (values nil nil (format nil "PLAN REFUSED: ~A" (autopilot-model-text model))))
      ((:planning :shown) (values nil (and (< (sim-t sim) barrier) barrier)))
      (:engaged
       (setf (flight-request flight) nil
             (flight-engage flight) (make-flight-engage clock sim))
       t)
      (t
       (setf (flight-request flight) nil)
       (values nil nil "(:AUTOPILOT): NOTHING PLANNED")))))

(defun flight-running! (flight clock sim step)
  "A begun STEP: (values over until reason), REASON a failure."
  (let ((now (sim-t sim))
        (until (flight-until flight))
        (deadline (flight-deadline flight)))
    (ecase (flight-step-kind step)
      (:action
       (ecase (flight-step-name step)
         (:autopilot (flight-autopilot-step flight clock sim))
         (:keys
          (cond ((>= now until)
                 (controls-apply-held-keys! (sim-controls sim) nil)
                 t)
                (t (values nil until))))
         (:translate
          (flet ((release! ()
                   (controls-apply-held-keys! (sim-controls sim) nil)
                   (setf (flight-translate flight) nil)))
            (cond ((>= (ship-state-dv (sim-ship sim)) (flight-translate flight)) (release!) t)
                  ((>= now deadline) (release!) (values nil nil "(:TRANSLATE): NO ΔV"))
                  (t (values nil deadline)))))
         (:after-dv
          (cond ((= (controls-throttle (sim-controls sim)) 0d0) t)
                ((>= now deadline) (values nil nil "(:AFTER-DV): NO CUT-OFF"))
                (t (values nil deadline))))))
      (:wait
       (when (eq (flight-step-name step) :autopilot-done)
         (multiple-value-bind (outcome text) (autopilot-outcome flight clock sim)
           (when (eq outcome :failed)
             (return-from flight-running!
               (values nil nil (format nil "(:WAIT ~A): ~A" (condition-text step) text))))))
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
                   (member (flight-step-name step) '(:impact :crashed :lost)))
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

(defun flight-close-object-p (sim)
  "A promoted object (not the latched load) within *flight-close-distance*
plus its bound of the stack's CoM."
  (let ((r (ship-state-r (sim-ship sim))))
    (loop for object across (sim-objects sim)
          thereis (and (object-promoted-p object)
                       (<= (vec3-norm (vec3-sub (ship-state-r (object-state object)) r))
                           (+ *flight-close-distance* (object-def-bound (object-def-of object))))))))

(defun flight-coast-cap (flight sim)
  "(:warp :coast)'s highest level now (decision 122): ×1 while :approach or
:translate runs; ×4 in a plan's :terminal or :station, or near a promoted
object unless berthed; else *flight-coast-limit*."
  (let ((step (flight-current flight)))
    (cond ((and step (eq (flight-phase flight) :running)
                (member (flight-step-name step) '(:approach :translate)))
           1)
          ((or (plan-terminal-p sim) (and (not (sim-berthed sim)) (flight-close-object-p sim)))
           *fine-levels-max*)
          (t *flight-coast-limit*))))

(defun flight-coast! (flight clock sim)
  "Down to the cap, else up to the highest level the clock accepts under
it, refusing nothing aloud."
  (let ((cap (flight-coast-cap flight sim)))
    (loop while (> (clock-level clock) cap) do (clock-warp-down clock))
    (loop for index = (1+ (clock-level-index clock))
          while (< index (length *warp-levels*))
          do (let ((level (nth index *warp-levels*)))
               (when (or (> level cap) (warp-refusal clock sim level))
                 (return))
               (clock-warp-up clock sim)))))

(defun flight-dropout-check! (flight clock sim)
  "Note a lowering by the drop-out rule since the last look, with its kind
(:impact, :contact or :burn)."
  (let ((before (flight-level flight)))
    (when (< (clock-level clock) before)
      (multiple-value-bind (delta kind) (clock-dropout clock sim)
        (when (and delta (< delta (* *dropout-real-time* before)))
          (setf (flight-dropout flight) (cons (sim-t sim) kind)))))
    (setf (flight-level flight) (clock-level clock))))

(defun flight-step-hook (flight clock sim)
  "The per-step function (decision 87): before each step of SIM under
CLOCK, the due steps of FLIGHT run, (:warp :coast) raises, the clock's
lowering rules hold again; the step's until. Called twice at one state it
runs once."
  (let ((cache (flight-cache flight)))
    (when (and cache (= (first cache) (sim-t sim)) (= (second cache) (sim-steps sim)))
      (return-from flight-step-hook (cddr cache))))
  (flight-engage-update! flight sim)
  (flight-dropout-check! flight clock sim)
  (let ((until (flight-advance! flight clock sim)))
    (unless (flight-done flight)
      (flight-burn-check! flight sim)
      (when (flight-coast flight)
        (flight-coast! flight clock sim))
      (clock-check-level! clock sim)
      (flight-dropout-check! flight clock sim))
    (let ((engage (flight-engage flight)))
      (when (and engage (null (flight-engage-previous-t engage)))
        (flight-clearance-update! engage sim)))
    (setf (flight-cache flight) (list* (sim-t sim) (sim-steps sim) until))
    until))

(defun flight-prediction-restarts-p (sim)
  "SIM's prediction would restart at every update: the throttle on, a
translation key held, a plan's :terminal or :station (decision 122)."
  (or (thrust-like-p sim)
      (notevery #'zerop (controls-translation-keys (sim-controls sim)))))

(defun run-flight (script &key model sim)
  "Fly SCRIPT (read-flight's source, or a list of flight-steps) headless
from its start state (flight-start-sim), or from SIM (a copy; the script's
:start then ignored), with the gravity MODEL if given: per step prediction-update!,
the hook, clock-step! with no accumulator; while the prediction would
restart (flight-prediction-restarts-p) it is updated once per
*flight-burn-prediction-interval* above ×4, once per
*flight-close-prediction-interval* at ×1 and ×4 (decision 122). (values
record end-sim flight clock)."
  (let* ((steps (if (and (listp script) (every #'flight-step-p script))
                    script
                    (read-flight script)))
         (sim (if sim (copy-sim sim) (flight-start-sim steps))))
    (when model
      (setf (sim-model sim) model))
    (let* ((flight (new-flight steps sim))
           (clock (new-clock sim))
           (predicted nil))
      (setf (flight-headless flight) t)
      (setf (clock-hook clock) (lambda (clock sim) (flight-step-hook flight clock sim)))
      (loop
        (when (or (null predicted)
                  (not (flight-prediction-restarts-p sim))
                  (>= (sim-t sim) (+ predicted (if (clock-kinematic-p clock)
                                                   *flight-burn-prediction-interval*
                                                   *flight-close-prediction-interval*))))
          (prediction-update! (clock-prediction clock) sim)
          (setf predicted (sim-t sim)))
        (flight-step-hook flight clock sim)
        (when (flight-done flight)
          (return))
        (unless (clock-step! clock sim)
          (when (sim-loss sim)
            (flight-step-hook flight clock sim))
          (unless (flight-done flight)
            (flight-finish! flight sim "STUCK"))))
      (values (flight-record flight) sim flight clock))))

;;; Lines

(defun flight-entry-state-text (entry)
  (format nil "T+~,3F S ~A ALT ~A AP ~:[ESCAPE~;~:*~A~] PE ~A SPD ~,1F M/S PROP ~,1F KG~@[ ~A~]~@[ ~A~]"
          (flight-entry-elapsed entry) (format-utc (flight-entry-t entry))
          (km-text (flight-entry-altitude entry))
          (and (flight-entry-apoapsis entry) (km-text (flight-entry-apoapsis entry)))
          (km-text (flight-entry-periapsis entry))
          (flight-entry-speed entry) (flight-entry-propellant entry)
          (and (flight-entry-target entry)
               (format nil "TGT ~A~@[ RNG ~A~]" (flight-entry-target entry)
                       (and (flight-entry-range entry) (km-text (flight-entry-range entry)))))
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
                (:burn (format nil "BURN ~@[~A ~]FROM T+~,3F S: ~,3F M/S IN ~,3F S: ~A"
                               (flight-entry-label e)
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
  "Fly the script NAME (read-flight's source) in GAME from its start state
(flight-start-sim), the view outside; returns at once. At the end the
record lines are printed to the image's output; flight-status reads them."
  (let* ((steps (read-flight name))
         (start (flight-start-sim steps)))
    (call-in-game game
                  (lambda ()
                    (let ((flight (new-flight steps start)))
                      (setf (flight-restart flight)
                            (lambda () (game-restart! game (flight-start flight))))
                      (setf (clock-checkpoint (game-clock game)) nil)
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
