(in-package #:luv-in-space)

;;; The cockpit's screens: their lines as pure functions, the line batch of
;;; the :cab-lines pipeline, the 10 Hz rebuild of each screen's text region,
;;; and the table of instruments (plan 32: Cockpit instruments, buttons, the
;;; grapple camera and messages; decision 127: Cockpit screens' contents in
;;; milestone 3: lines, modes, plots, colours and updates): ORBIT, ATTITUDE,
;;; TARGET (stowed), SHIP, AUTOPILOT and COMMS, with the dot-matrix strip
;;; (decision 128: Cockpit devices, buttons, picking, the grapple camera and
;;; messages in milestone 3).

;;; Colours (sRGB hex, decision 127; the readout's constants of overlay.lisp
;;; where they exist) and their inks: the radiance the tone map shows as that
;;; colour, as the text shader multiplies it (exposure 1; the shader divides
;;; by the frame's exposure).

(defparameter *screen-white* #xE8E4D8 "Values.")
(defparameter *screen-grey* #x8A8A80 "Labels and units.")
(defparameter *screen-green* #x40D040 "In tolerance, engaged, latched.")
(defparameter *screen-amber* #xFFB000 "Caution, with \"!\".")
(defparameter *screen-red* #xFF3030 "Danger, inverted and flashing.")
(defparameter *screen-cyan* #x40D0FF "The own ship.")
(defparameter *screen-magenta* #xE040E0 "The target.")
(defparameter *screen-black* #x0A0C0C "The faces' backlight, the inverted text.")

(defun srgb-hex-bytes (hex)
  (values (ldb (byte 8 16) hex) (ldb (byte 8 8) hex) (ldb (byte 8 0) hex)))

(let ((inks (make-hash-table)))
  (defun screen-ink (hex)
    "The text ink of the sRGB colour HEX: a linear (single-float (3)) whose
emissive draw, divided by the exposure, tone-maps to HEX (emissive-for-srgb)."
    (or (gethash hex inks)
        (setf (gethash hex inks)
              (multiple-value-bind (r g b) (multiple-value-call #'emissive-for-srgb
                                             (srgb-hex-bytes hex) 1d0)
                (make-array 3 :element-type 'single-float
                              :initial-contents (list (float r 1f0) (float g 1f0) (float b 1f0))))))))

(let ((colours (make-hash-table)))
  (defun screen-line-colour (hex)
    "HEX as the :cab-lines tint's linear colour (before the tone map)."
    (or (gethash hex colours)
        (setf (gethash hex colours)
              (multiple-value-call #'text-colour (srgb-hex-bytes hex))))))

;;; Runs and lines: a run is (text colour style flash) as overlay.lisp's
;;; readout-run, its text as given (not upcased); a line a list of runs.

(defun screen-run (text colour &optional (style :plain) flash)
  (list text colour style flash))

(defparameter *screen-flash-period* 0.5d0 "s of real time: the 2 Hz of a danger.")

(defun screen-flash (real-time)
  "The phase of REAL-TIME, :on in the first half of every *screen-flash-period*."
  (if (< (mod real-time *screen-flash-period*) (* 0.5d0 *screen-flash-period*)) :on :off))

(defun screen-line (&rest parts)
  "A line of runs from PARTS: a colour name (:white :grey :green :amber :red :cyan :magenta)
followed by the text of its run."
  (loop for (colour text) on parts by #'cddr
        collect (screen-run text (ecase colour
                                   (:white *screen-white*) (:grey *screen-grey*)
                                   (:green *screen-green*) (:amber *screen-amber*)
                                   (:red *screen-red*) (:cyan *screen-cyan*)
                                   (:magenta *screen-magenta*)))))

(defun screen-line-length (line)
  (loop for run in line sum (length (run-text run))))

(defun screen-fit-line (line columns)
  "LINE cut to COLUMNS characters, \"…\" last when anything went."
  (if (<= (screen-line-length line) columns)
      line
      (let ((left columns) (out '()))
        (dolist (run line)
          (let ((text (run-text run)))
            (cond ((<= (length text) left)
                   (push run out) (decf left (length text)))
                  ((zerop left)
                   (let ((last (first out)))
                     (when last
                       (setf (first out)
                             (list (concatenate 'string (subseq (run-text last) 0 (1- (length (run-text last)))) "…")
                                   (run-colour last) (run-style last) (run-flash last)))))
                   (return))
                  (t (push (list (concatenate 'string (subseq text 0 (max 0 (1- left))) "…")
                                 (run-colour run) (run-style run) (run-flash run))
                           out)
                     (setf left 0)
                     (return)))))
        (nreverse out))))

;;; Formats shared with the readout (overlay.lisp, units.lisp).

(defun format-thousands (n) (format nil "~:D" (round n)))

(defun format-clock-time (seconds)
  "SECONDS (to a future event) as T-mm:ss, T-h:mm:ss above an hour."
  (let ((total (max 0 (round seconds))))
    (multiple-value-bind (minutes second) (floor total 60)
      (multiple-value-bind (hours minute) (floor minutes 60)
        (if (plusp hours)
            (format nil "T-~D:~2,'0D:~2,'0D" hours minute second)
            (format nil "T-~2,'0D:~2,'0D" minute second))))))

(defun signed-fixed (x decimals)
  "X with its sign always: +0.0, -1.5."
  (let ((text (format-fixed x decimals)))
    (if (char= (char text 0) #\-) text (concatenate 'string "+" text))))

;;; ORBIT (24 × 8)

(defun orbit-time-text (state kind)
  "T-mm:ss to the next apsis of KIND, T--:-- without an event."
  (let ((time (and (render-state-prediction state)
                   (prediction-apsis (render-state-prediction state) kind))))
    (if time (format-clock-time (- time (render-state-t state))) "T--:--")))

(defun orbit-lines (state real-time)
  (let* ((r (render-state-r state)) (v (render-state-v state))
         (distance (vec3-norm r)) (speed (vec3-norm v))
         (orbit (state->orbit r v +mu-earth+ (render-state-t state)))
         (apoapsis (orbit-apoapsis-radius orbit))
         (periapsis (orbit-periapsis-radius orbit))
         (earth (body :earth))
         (period (orbit-period orbit))
         (crash (< periapsis (body-crash-radius earth)))
         (vs (/ (vec3-dot r v) distance)))
    (list
     (screen-line :white "EARTH" :grey "  SOI --:--")   ; PLACEHOLDER(4): the SOI time
     (screen-line :grey "ALT  " :white (format-fixed (m->km (- distance +radius-earth+)) 2) :grey " km")
     (if (< speed 1000d0)
         (screen-line :grey "SPD " :white (format-fixed speed 1) :grey " m/s")
         (screen-line :grey "SPD " :white (format-fixed (m/s->km/s speed) 3) :grey " km/s"))
     (screen-line :grey "VS  " :white (signed-fixed vs 1) :grey " m/s")
     (if apoapsis
         (screen-line :grey "AP " :white (format-fixed (m->km (- apoapsis +radius-earth+)) 1)
                      :grey " km " :white (orbit-time-text state :apoapsis))
         (screen-line :grey "AP " :white "ESCAPE"))
     (let ((text (format nil "PE ~A km ~A" (format-fixed (m->km (- periapsis +radius-earth+)) 1)
                         (orbit-time-text state :periapsis))))
       (if crash
           (list (screen-run text *screen-red* :inverted (screen-flash real-time)))
           (screen-line :grey "PE " :white (format-fixed (m->km (- periapsis +radius-earth+)) 1)
                        :grey " km " :white (orbit-time-text state :periapsis))))
     (screen-line :grey "INC " :white (format nil "~A°" (format-fixed (rad->deg (orbit-i orbit)) 2))
                  :grey " ECC " :white (format-fixed (orbit-e orbit) 4))
     (cond ((null period) (screen-line :grey "PER " :white "--"))
           ((> period 36000d0) (screen-line :grey "PER " :white (format-fixed (/ period 3600d0) 1)
                                            :grey " h"))
           (t (screen-line :grey "PER " :white (format-fixed (/ period 60d0) 1) :grey " min"))))))

;;; SHIP (22 × 8)

(defun ship-acceleration (state)
  "m/s²: the engine's full thrust over the stack's mass."
  (/ *engine-thrust* (render-state-stack-mass state)))

(defun ship-dv (state)
  "m/s: the rocket equation at the stack's mass with its load."
  (let* ((m (render-state-stack-mass state)) (dry (- m (render-state-m-p state))))
    (* (engine-exhaust-velocity) (log (/ m dry)))))

(defun plan-propellant-need (plan state)
  "kg: what PLAN's burns not yet cut at STATE's t take from the stack's mass,
the rocket equation per burn. Conses nothing but its result."
  (let* ((mass (render-state-stack-mass state)) (end mass) (now (render-state-t state)))
    (loop for burn across (burn-plan-burns plan)
          when (> (planned-burn-cut-t burn) now)
            do (setf end (* end (exp (- (/ (planned-burn-dv burn)
                                          (* (planned-burn-isp burn) +g0+)))))))
    (- mass end)))

(defun propellant-low-p (state)
  "Whether PROP is lit (decision 128): below 5 % of the load, or below the need
of the plan shown or engaged."
  (let* ((model (render-state-autopilot-model state))
         (plan (and model (case (autopilot-model-state model)
                            (:shown (autopilot-model-plan model))
                            (:engaged (autopilot-model-flown model))))))
    (or (< (render-state-m-p state) (* 0.05d0 *propellant-full*))
        (and plan (< (render-state-m-p state) (plan-propellant-need plan state))))))

(defun ship-lines (state)
  (let* ((controls (render-state-controls state))
         (throttle (controls-throttle controls))
         (m-p (render-state-m-p state))
         (load (render-state-load-def state))
         (berth (render-state-berth-def state))
         (dv (ship-dv state)))
    (list
     (let ((line (screen-line :grey "PROP " :white (format-thousands m-p) :grey " kg "
                              :white (format nil "~D" (round (* 100 m-p) *propellant-full*))
                              :grey " %")))
       (if (propellant-low-p state)
           (cons (screen-run "! " *screen-amber*) (mapcar (lambda (run) (list (run-text run) *screen-amber*
                                                                              :plain nil))
                                                          line))
           line))
     (if (< dv 1000d0)
         (screen-line :grey "DV " :white (format-fixed dv 1) :grey " m/s")
         (screen-line :grey "DV " :white (format-fixed (m/s->km/s dv) 2) :grey " km/s"))
     (screen-line :grey "MASS " :white (format-fixed (/ (render-state-stack-mass state) 1000d0) 1)
                  :grey " t " :white (format-fixed (ship-acceleration state) 2) :grey " m/s²")
     (cond (berth
            (cond ((render-state-transfer state)
                   (screen-line :grey "XFER " :green (format nil "~D kg/s" (round *transfer-rate*))))
                  ((object-def-depot berth) (screen-line :grey "XFER OFF [B]"))
                  (t (screen-line :grey "XFER --"))))
           (t (screen-line :grey "THR " :white (format nil "~D" (round (* 100 throttle))) :grey " %  ENG "
                           :white (if (and (> (engine-throttle throttle) 0d0) (> m-p 0d0)) "ON" "OFF"))))
     (cond (berth (screen-line :grey "BERTHED " :white (object-def-name berth)))
           (load (screen-line :grey "LOAD " :white (format-fixed (/ (object-def-mass load) 1000d0) 1)
                              :grey " t " :white (object-def-name load)))
           (t '()))
     ;; PLACEHOLDER(4): life support, the Sun and the hull's temperature.
     (screen-line :grey "LIFE --- d")
     (screen-line :grey "SUN --- AU")
     (screen-line :grey "HULL --- K"))))

;;; ATTITUDE (19 × 8): the plot and the bars are cockpit-attitude.lisp's; here
;;; the bars' letters and the mode line.

(defparameter *hold-abbreviations*
  '((:prograde . "PRO") (:retrograde . "RET") (:normal . "NRM") (:anti-normal . "ANRM")
    (:radial-out . "ROUT") (:radial-in . "RIN") (:inertial . "INRT") (:target . "TGT")
    (:anti-target . "ATGT") (:relative-prograde . "RPRO") (:relative-retrograde . "RRET"))
  "A hold's name in at most four characters, for ATTITUDE's mode line.")

(defun attitude-mode-line (state)
  "The mode line (decision 127): \"HOLD PRO  SETTLED\" or \"HOLD PRO  SLEWING\", \"DAMP\",
\"DIRECT\", \"+FINE\" after any, led by \"AUTOPILOT\" while a plan is engaged."
  (let* ((controls (render-state-controls state))
         (mode (controls-attitude-mode controls))
         (fine (controls-fine controls))
         (autopilot (render-state-autopilot state))
         (hold (cdr (assoc (controls-hold controls) *hold-abbreviations*)))
         (settled (controls-settled controls)))
    (append
     (when autopilot (screen-line :green "AUTOPILOT "))
     (ecase mode
       (:direct (screen-line :white "DIRECT"))
       (:rate-damping (screen-line :white "DAMP"))
       (:hold (cond (autopilot (screen-line :white (format nil "HOLD ~A" hold)))
                    (fine (screen-line :white (format nil "~A " hold)
                                       (if settled :green :amber) (if settled "SETTLED" "SLEWING")))
                    (t (screen-line :white (format nil "HOLD ~A  " hold)
                                    (if settled :green :amber) (if settled "SETTLED" "SLEWING"))))))
     (when fine (screen-line :amber " +FINE")))))

(defun attitude-lines (state)
  "Rows 1 to 7 hold the plot; the bars' letters P Y R under the bars on row 7,
the mode on row 8."
  (list '() '() '() '() '() '()
        (screen-line :grey "             P Y R")
        (attitude-mode-line state)))

;;; TARGET (24 × 8), the grapple stowed (the armed camera is plan 32 piece 7's)

(defun format-range (metres)
  "METRES as 12.345 km from 1 km, else 340.2 m."
  (if (>= metres 1000d0)
      (concatenate 'string (format-fixed (m->km metres) 3) " km")
      (concatenate 'string (format-fixed metres 1) " m")))

(defun rendezvous-phase-line (state)
  "The engaged rendezvous plan's terminal phase on the shown target: a line
\"TERMINAL\" or \"ON STATION 100 m\", else nil."
  (let ((sim (render-state-sim state)))
    (when (and sim (render-state-target state) (sim-plan sim))
      (let ((part (burn-plan-rendezvous (sim-plan sim))))
        (case (sim-plan-phase sim)
          (:terminal (screen-line :green "TERMINAL"))
          (:station (screen-line :green (format nil "ON STATION ~D m"
                                                (round (if part (rendezvous-part-range part) 0d0))))))))))

(defun target-lines (state)
  (let ((target (render-state-target state)))
    (if (null target)
        (list (screen-line :white "NO TARGET") '() (screen-line :grey "PICK ] [  OR MAP CLICK"))
        (let* ((rel (render-state-target-relative state))
               (closing (aref rel 1))
               (small (< (max (abs (aref rel 2)) (abs (aref rel 3)) (abs (aref rel 4))) 1d0))
               (decimals (if small 2 1))
               (pred (render-state-prediction state)))
          (list
           (screen-line :magenta (target-name target))
           (screen-line :grey "RNG " :white (format-range (aref rel 0)))
           (screen-line :grey "CLS " :white (format nil "~A m/s" (signed-fixed closing (if (< (abs closing) 1d0) 2 1))))
           (screen-line :grey "F " :white (signed-fixed (aref rel 2) decimals)
                        :grey " R " :white (signed-fixed (aref rel 3) decimals)
                        :grey " D " :white (signed-fixed (aref rel 4) decimals))
           (multiple-value-bind (time distance) (if pred (prediction-closest-approach pred) nil)
             (if time
                 (screen-line :grey "CA " :white (format-clock-time (- time (render-state-t state)))
                              :white " " :white (format-range distance))
                 (screen-line :grey "CA --")))
           (or (rendezvous-phase-line state) '())
           '()
           (screen-line :grey "TGT ] NEXT [ PREV"))))))

;;; AUTOPILOT (22 × 8): by the model's state (decision 127, decision 118).

(defparameter *autopilot-screen-rows* 7 "Rows over the plot or the menu's window; row 8 is the hint.")

(defun format-arrival (seconds)
  "SECONDS ahead as mm:ss, h:mm:ss above an hour."
  (subseq (format-clock-time seconds) 2))

(defun autopilot-plan-screen-lines (plan sim)
  "The plan's four lines (decision 127) from autopilot-plan-summary: burn n/m
and when, its Δv and duration, the total Δv and propellant, what is left and
the arrival; nil when the plan's cursor has no burn left."
  (let ((burns (burn-plan-burns plan)))
    (unless (and (eq (sim-plan sim) plan) (>= (sim-plan-index sim) (length burns)))
      (let ((summary (autopilot-plan-summary plan sim)))
        (list
         (screen-line :grey "BURN " :white (format nil "~D/~D" (1+ (getf summary :index)) (getf summary :burns))
                      :white " " :white (format-clock-time (getf summary :next-in)))
         (screen-line :grey "DV " :white (format-fixed (getf summary :next-dv) 1)
                      :grey " m/s  " :white (format nil "~D" (round (getf summary :next-duration))) :grey " s")
         (screen-line :grey "TOTAL " :white (format nil "~D" (round (getf summary :total-dv)))
                      :grey " m/s " :white (format nil "~D" (round (getf summary :propellant))) :grey " kg")
         (screen-line :grey "LEFT " :white (format-fixed (/ (getf summary :left) 1000d0) 1)
                      :grey " t  ARR " :white (format-arrival (- (getf summary :arrival) (sim-t sim)))))))))

(defun autopilot-menu-window (model sim)
  "The menu's rows, seven of them keeping the selection in sight, the
selected one inverted over the width of the grid."
  (multiple-value-bind (texts selected) (autopilot-menu-lines model sim)
    (let* ((count (length texts))
           (rows *autopilot-screen-rows*)
           (start (max 0 (min (- count rows) (- (or selected 0) (floor rows 2))))))
      ;; the marker column goes: the inverted row says which is selected
      (loop for text in (subseq texts start (min count (+ start rows)))
            for i from start
            collect (let ((text (subseq text 2)))
                      (if (eql i selected)
                          (list (screen-run (format nil "~22A" text) *screen-white* :inverted))
                          (screen-line :white text)))))))

(defun autopilot-row-8 (clock model real-time)
  (case (autopilot-model-state model)
    (:closed (screen-line :grey "PRESETS [O]"))
    (:menu (screen-line :grey "PLAN [RET]  CLOSE [O]"))
    (:refused (screen-line :amber (format nil "! ~A" (or (autopilot-model-text model) "REFUSED"))))
    (:planning (screen-line :grey "DROP [BKSP]"))
    (:shown (screen-line :grey "RET ENGAGE  BKSP DROP"))
    (:engaged (screen-line :green "ENGAGED  OFF [BKSP]"))
    (:off (if (autopilot-abort-unread-p clock model)
              (let ((text (or (autopilot-model-text model) "ABORT")))
                (list (screen-run (if (and (> (length text) 11) (string= "AUTOPILOT: " text :end2 11))
                                      (subseq text 11) text)
                                  *screen-red* :inverted (screen-flash real-time))))
              (screen-line :grey "PRESETS [O]")))))

(defun autopilot-lines (state clock real-time)
  (let* ((model (render-state-autopilot-model state))
         (sim (render-state-sim state))
         (lines
           (when (and model sim)
             (ecase (autopilot-model-state model)
               ((:menu :refused) (autopilot-menu-window model sim))
               (:planning (list (screen-line :white "PLANNING…")))
               ((:shown :engaged)
                (let ((plan (autopilot-map-plan model)))
                  (or (and plan (autopilot-plan-screen-lines plan sim))
                      (list (or (rendezvous-phase-line state) '())))))
               ((:closed :off) '())))))
    (when model
      (append lines (make-list (max 0 (- (1- 8) (length lines))))
              (list (autopilot-row-8 clock model real-time))))))

;;; The screens, by name: their grids come from *cockpit-panels*.

(defun screen-lines (name state clock real-time &optional cockpit)
  "NAME's lines (lists of runs), cut to the grid, as a function of the
render STATE, the CLOCK and REAL-TIME (pure but for COMMS, which reads and
advances the log's scroll in COCKPIT, the game's cockpit-state)."
  (let ((columns (first (getf (cockpit-panel name) :grid))))
    (mapcar (lambda (line) (screen-fit-line line columns))
            (ecase name
              (:orbit (orbit-lines state real-time))
              (:attitude (attitude-lines state))
              (:target (target-lines state))
              (:autopilot (autopilot-lines state clock real-time))
              (:ship (ship-lines state))
              (:comms (comms-lines clock (or cockpit (make-cockpit-state))))))))

;;; A screen's text and line regions and what the rebuild needs.

(defstruct (screen-view (:copier nil) (:constructor %make-screen-view))
  name panel columns rows cap
  text lines                       ; the text-region and the line-region
  plane scratch line-plane         ; text planes: at the grid, a shifted copy, the lines'
  plot)                            ; AUTOPILOT: its orbit plot (cockpit-autopilot.lisp)

(defun make-screen-view (name text-batch line-batch)
  (let* ((panel (cockpit-panel name))
         (cap (panel-grid-cap panel))
         (origin (panel-grid-origin panel cap)))
    (destructuring-bind (columns rows) (getf panel :grid)
      (multiple-value-bind (c n r u) (panel-frame panel)
        (declare (ignore c))
        (flet ((plane (origin)
                 (make-text-plane :origin (coerce origin 'list) :right (coerce r 'list)
                                  :up (coerce u 'list))))
          (%make-screen-view :name name :panel panel :columns columns :rows rows
                            :cap (float cap 1f0)
                            :text (text-batch-region text-batch (* columns rows))
                            :lines (line-batch-region line-batch rows)
                            :plane (plane origin) :scratch (plane origin)
                            :line-plane (plane (vlin origin -0.001d0 n))
                            :plot (when (eq name :autopilot) (make-autopilot-plot line-batch))))))))

(defun cockpit-screens-init! (text-batch line-batch)
  "A screen-view for each of *cockpit-text-screens*, its regions made in the batches."
  (mapcar (lambda (name) (make-screen-view name text-batch line-batch)) *cockpit-text-screens*))

(defun screen-view-draw! (view atlas lines)
  "LINES (lists of runs) written to VIEW's regions: each run's cells of its
row from its column on; an inverted run a block of its colour in the line
region under dark text; the rest zeroed."
  (let* ((cap (screen-view-cap view))
         (cell (* +text-cell-width+ cap))
         (plane (screen-view-plane view)) (scratch (screen-view-scratch view))
         (line-plane (screen-view-line-plane view))
         (text (screen-view-text view)) (blocks (screen-view-lines view))
         (index 0) (block-index 0))
    (declare (type single-float cap cell) (type fixnum index block-index))
    (loop for line in lines
          for row from 0 below (screen-view-rows view)
          do (let ((column 0))
               (declare (type fixnum column))
               (dolist (run line)
                 (let* ((string (run-text run)) (length (length string))
                        (inverted (and (eq (run-style run) :inverted) (not (eq (run-flash run) :off)))))
                   (when (and (plusp length) (<= (+ column length) (screen-view-columns view)))
                     (dotimes (k 3)
                       (setf (aref scratch k) (+ (aref plane k) (* column cell (aref plane (+ 3 k))))))
                     (when inverted
                       (let* ((y (- (* 0.5f0 cap) (* row +text-cell-height+ cap)))
                              (x0 (* column cell)) (x1 (* (+ column length) cell)))
                         (flet ((at (x k) (+ (aref line-plane k) (* x (aref line-plane (+ 3 k)))
                                             (* y (aref line-plane (+ 6 k))))))
                           (line-put! blocks block-index (at x0 0) (at x0 1) (at x0 2)
                                      (at x1 0) (at x1 1) (at x1 2)
                                      (screen-line-colour (run-colour run)) (* 1.5f0 cap))
                           (incf block-index))))
                     (setf index (grid-run! atlas text index scratch string :row row
                                            :cap cap :colour (screen-ink (if inverted *screen-black* (run-colour run)))
                                            :kind :emissive))
                     (incf column length))))))
    (text-region-clear! text index)
    (line-region-clear! blocks block-index)
    view))

;;; The cockpit's per-frame state in the game (not the sim): when each screen
;;; was rebuilt and under which signature.

(defparameter *screen-rebuild-period* 0.1d0 "s of real time between a screen's rebuilds.")

(defstruct (cockpit-state (:copier nil))
  (rebuilt (make-array 8 :element-type 'double-float :initial-element -1d9)
   :type (simple-array double-float (8)))
  (signatures (make-array 8 :element-type 'fixnum :initial-element -1)
   :type (simple-array fixnum (8)))
  (rebuilds 0 :type fixnum)
  ;; The strip's marquee: the queued messages, the one shown and since when.
  (strip-queue nil :type list)
  (strip-seen nil)                    ; the newest log entry queued
  (strip-current nil)                 ; the text shown on line 3, or nil
  (strip-start 0d0 :type double-float)
  (strip-built -1d9 :type double-float)
  (strip-line-1 "" :type string)
  (strip-line-2 "" :type string)
  (strip-hover nil)                   ; the hovered button's "<NAME> [<KEY>]" (plan 32 piece 6)
  ;; COMMS: the log's typing, its scroll (lines up from the bottom).
  (typed nil)                         ; the newest log entry typed out
  (typing 0d0 :type double-float)     ; characters typed of the next entry
  (typing-time nil)                   ; the real time of the last advance
  (typing-serial 0 :type fixnum)      ; changes with every character shown
  (scroll 0 :type fixnum)
  (log-lines 0 :type fixnum)
  ;; The lamps and the gauges (cockpit-devices.lisp; decision 128).
  (mstr nil :type (member nil :red :amber)) ; the master alarm until acknowledged
  (peri-lit nil) (prop-lit nil) (abort-lit nil) ; the alarms' last states: MSTR's onsets
  (lamp-colours (make-array 12 :initial-element nil) :type simple-vector)
  (lamp-flashes (make-array 12 :initial-element nil) :type simple-vector)
  (lamp-built -1d9 :type double-float)
  (lamp-signature -1 :type fixnum)
  (needles (make-array 2 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (2))) ; the lagged values, 0 to 1: PROP, hull
  (needle-time nil))                  ; the real time of the last lag step

(defun cockpit-state-acknowledge! (cockpit)
  "Y or a click: the master alarm acknowledged; a later onset lights it again."
  (setf (cockpit-state-mstr cockpit) nil)
  cockpit)

(defun cockpit-state-invalidate! (cockpit)
  "The next frame rebuilds every screen (a restart, a loaded state)."
  (fill (cockpit-state-rebuilt cockpit) -1d9)
  (setf (cockpit-state-strip-built cockpit) -1d9
        (cockpit-state-lamp-built cockpit) -1d9)
  cockpit)

(defun cockpit-state-restart! (cockpit clock)
  "A restart: every screen rebuilds; the marquee and the log's typing start
from the log as it is (nothing replays), the scroll at the bottom."
  (cockpit-state-invalidate! cockpit)
  (setf (cockpit-state-mstr cockpit) nil
        (cockpit-state-peri-lit cockpit) nil (cockpit-state-prop-lit cockpit) nil
        (cockpit-state-abort-lit cockpit) nil
        (cockpit-state-lamp-built cockpit) -1d9)
  (setf (cockpit-state-strip-queue cockpit) nil
        (cockpit-state-strip-current cockpit) nil
        (cockpit-state-strip-seen cockpit) (first (clock-log clock))
        (cockpit-state-typed cockpit) (first (clock-log clock))
        (cockpit-state-typing cockpit) 0d0
        (cockpit-state-typing-time cockpit) nil
        (cockpit-state-scroll cockpit) 0)
  cockpit)

(declaim (inline signature-mix))
(defun signature-mix (hash x)
  (declare (type fixnum hash x))
  (logand (+ (* hash 31) x) most-positive-fixnum))

(defun cockpit-signature (state clock view)
  "A fixnum of what changes a screen's mode at once (decision 127): the view,
the hold kind and whether settled, fine, the attitude mode, the grapple's
state, the target, the autopilot model's state and selection, warp, pause,
the berth, the transfer, the log's length. Conses nothing."
  (declare (type render-state state) (type clock clock))
  (let* ((controls (render-state-controls state))
         (model (render-state-autopilot-model state))
         (h 7))
    (declare (type fixnum h))
    (macrolet ((mix (form) `(setf h (signature-mix h (the fixnum ,form)))))
      (mix (sxhash view))
      (mix (sxhash (controls-hold controls)))
      (mix (sxhash (controls-attitude-mode controls)))
      (mix (if (controls-settled controls) 1 0))
      (mix (if (controls-fine controls) 1 0))
      (mix (sxhash (render-grapple-state (render-state-grapple state))))
      (mix (sxhash (render-state-target state)))
      (mix (if (render-state-autopilot state) 1 0))
      (when model
        (mix (sxhash (autopilot-model-state model)))
        (mix (sxhash (autopilot-model-selection model)))
        (when (eq (autopilot-model-state model) :off)
          (mix (if (autopilot-abort-unread-p clock model) 1 0))))
      (mix (clock-level clock))
      (mix (if (clock-paused clock) 1 0))
      (mix (if (render-state-berth-def state) 1 0))
      (mix (if (render-state-transfer state) 1 0))
      (mix (if (render-state-load-def state) 1 0))
      (mix (length (clock-log clock))))
    h))

(defun comms-signature (cockpit)
  "What of COMMS changes between two 10 Hz rebuilds: the typing and the scroll."
  (declare (type cockpit-state cockpit))
  (logand (+ (* 7 (cockpit-state-typing-serial cockpit)) (cockpit-state-scroll cockpit))
          most-positive-fixnum))

(defun cockpit-update! (cockpit views atlas state clock real-time view &key strip devices attitude snap)
  "Rebuild the text of each of VIEWS (screen-views) whose last rebuild is 0.1 s
of REAL-TIME old, or was made under another cockpit-signature (COMMS also
while it types); COCKPIT the game's cockpit-state. The log's typing and the
strip (a strip-view) advance every frame, as ATTITUDE's plot (an attitude-view); SNAP shows both finished (captures:
the log fully typed, the strip's latest message static). Returns the number
of screens rebuilt."
  (declare (type cockpit-state cockpit) (type double-float real-time))
  (comms-type! cockpit clock real-time snap)
  (when strip (strip-update! cockpit strip state clock real-time snap))
  (when attitude (attitude-view-update! attitude atlas state))
  (let ((signature (cockpit-signature state clock view)) (built 0))
    (declare (type fixnum signature built))
    (when devices
      (cockpit-devices-update! devices cockpit state clock real-time signature snap))
    (loop for screen in views
          for i of-type fixnum from 0
          do (let ((signature (if (eq (screen-view-name screen) :comms)
                                  (signature-mix signature (comms-signature cockpit))
                                  signature)))
               (declare (type fixnum signature))
               (when (or (/= signature (aref (cockpit-state-signatures cockpit) i))
                         (>= (- real-time (aref (cockpit-state-rebuilt cockpit) i))
                             *screen-rebuild-period*)
                         (< real-time (aref (cockpit-state-rebuilt cockpit) i)))
                 (screen-view-draw! screen atlas
                                    (screen-lines (screen-view-name screen) state clock real-time cockpit))
                 (when (screen-view-plot screen)
                   (autopilot-plot-update! (screen-view-plot screen) state))
                 (setf (aref (cockpit-state-rebuilt cockpit) i) real-time
                       (aref (cockpit-state-signatures cockpit) i) signature)
                 (incf built)
                 (incf (cockpit-state-rebuilds cockpit)))))
    built))

;;; The dot-matrix strip (docs/design.md "Instruments and how each is drawn";
;;; decision 128): 19 × 3 cells of 6 × 9 dots (a glyph 5 × 7 in a cell's
;;; corner), 2.45 mm apart, round LEDs of Ø 1.9 mm, amber lit and 6 % of that
;;; unlit. Line 1 the date, line 2 the warp, line 3 the messages.

(defconstant +strip-columns+ 19)
(defconstant +strip-dot-columns+ 114 "19 cells of 6 dots.")
(defconstant +strip-dot-rows+ 27 "Three lines of 9 dots.")
(defconstant +strip-led-rows+ 21 "Of those, 7 a line carry LEDs: the gap rows are bare.")
(defparameter *strip-pitch* 0.00245d0 "m between dots.")
(defparameter *strip-dot-radius* 0.00095d0)
(defparameter *strip-unlit* 0.06 "The share of the lit emissive an unlit dot keeps.")
(defparameter *strip-message-hold* 3d0 "s of real time a short message stays.")
(defparameter *strip-scroll-speed* 12d0 "characters a second.")
(defparameter *strip-queue-max* 8)

(defstruct (strip-view (:copier nil) (:constructor %make-strip-view))
  (data nil :type (simple-array single-float (*)))
  (start 0 :type fixnum)             ; the first dot's record in the batch
  (line-1 "" :type string) (line-2 "" :type string) (line-3 "" :type string)
  (shift 0 :type fixnum)             ; line 3's dots to the right
  (valid nil))                       ; whether the lines above were written

(let ((table (make-hash-table)))
  (defun strip-glyph-bits (char)
    "CHAR's 5 × 7 dots as a simple-bit-vector of 35 (row-major), blank without a glyph."
    (let ((char (char-upcase char)))
      (or (gethash char table)
          (setf (gethash char table)
                (let ((bits (make-array 35 :element-type 'bit :initial-element 0)))
                  (loop for row in (glyph char) for y from 0
                        do (loop for c across row for x from 0
                                 when (char= c #\#) do (setf (sbit bits (+ (* y 5) x)) 1)))
                  bits))))))

(defun strip-dot-point (gx gy)
  "The body point (from the eye) of the dot at column GX of 114 and row GY of 27
on the strip's face, 2 mm in front of it."
  (multiple-value-bind (c n r u) (panel-frame (cockpit-panel :strip))
    (vlin c (* (- (+ gx 0.5d0) (* 0.5d0 +strip-dot-columns+)) *strip-pitch*) r
          (* (- (* 0.5d0 +strip-dot-rows+) (+ gy 0.5d0)) *strip-pitch*) u
          0.002d0 n)))

(defun make-strip-view (batch)
  "A strip-view of 114 × 21 LEDs (7 rows of each line) taken from the dot BATCH,
laid out on the strip panel's face, all unlit."
  (let ((start (dot-batch-take! batch (* +strip-dot-columns+ +strip-led-rows+)))
        (data (dot-batch-data batch))
        (tint (screen-line-colour *screen-amber*)))
    (dotimes (led +strip-led-rows+)
      (dotimes (gx +strip-dot-columns+)
        (let* ((gy (+ (* 9 (floor led 7)) (mod led 7)))
               (base (* +dot-record-floats+ (+ start (* led +strip-dot-columns+) gx)))
               (p (strip-dot-point gx gy)))
          (setf (aref data base) (float (aref p 0) 1f0)
                (aref data (+ base 1)) (float (aref p 1) 1f0)
                (aref data (+ base 2)) (float (aref p 2) 1f0)
                (aref data (+ base 3)) (float *strip-dot-radius* 1f0)
                (aref data (+ base 4)) (aref tint 0)
                (aref data (+ base 5)) (aref tint 1)
                (aref data (+ base 6)) (aref tint 2)
                (aref data (+ base 7)) *strip-unlit*))))
    (%make-strip-view :data data :start start)))

(defun strip-dot-lit (text shift gx row)
  "Whether the dot at column GX of a line's dots, ROW 0 to 8, is lit by TEXT
placed SHIFT dots to the right. Conses nothing."
  (declare (type string text) (type fixnum shift gx row))
  (let ((tx (- gx shift)))
    (and (>= tx 0) (< row 7)
         (multiple-value-bind (index column) (floor tx 6)
           (and (< index (length text)) (< column 5)
                (= 1 (sbit (the simple-bit-vector (strip-glyph-bits (char text index)))
                           (+ (* row 5) column))))))))

(defun strip-dots! (view line-1 line-2 line-3 &optional (shift 0))
  "Write VIEW's dots for the three lines of text, LINE-3 SHIFT dots to the right
(negative to the left); nothing when they equal the last ones'. Conses nothing."
  (declare (type strip-view view) (type string line-1 line-2 line-3) (type fixnum shift))
  (unless (and (strip-view-valid view) (= shift (strip-view-shift view))
               (string= line-1 (strip-view-line-1 view)) (string= line-2 (strip-view-line-2 view))
               (string= line-3 (strip-view-line-3 view)))
    (let ((data (strip-view-data view)) (start (strip-view-start view)))
      (flet ((paint (line text shift)
               (declare (type fixnum line shift) (type string text))
               (dotimes (row 7)
                 (let ((led (+ (* line 7) row)))
                   (dotimes (gx +strip-dot-columns+)
                     (setf (aref data (+ (* +dot-record-floats+ (+ start (* led +strip-dot-columns+) gx)) 7))
                           (if (strip-dot-lit text shift gx row) 1f0 *strip-unlit*)))))))
        (paint 0 line-1 0) (paint 1 line-2 0) (paint 2 line-3 shift)))
    (setf (strip-view-line-1 view) line-1 (strip-view-line-2 view) line-2
          (strip-view-line-3 view) line-3 (strip-view-shift view) shift
          (strip-view-valid view) t))
  view)

(defun strip-line-1 (time)
  "The game's UTC date and time, 19 characters."
  (format-utc time))

(defun format-rate (rate)
  "A rate to three significant digits, K or M above 999."
  (cond ((>= rate 999.5d6) (format nil "~AG" (format-significant (/ rate 1d9) 3)))
        ((>= rate 999.5d3) (format nil "~AM" (format-significant (/ rate 1d6) 3)))
        ((>= rate 999.5d0) (format nil "~AK" (format-significant (/ rate 1d3) 3)))
        (t (format-significant rate 3))))

(defun strip-line-2 (level rate paused &optional (act t))
  "PAUSED, or WARP ×<level> with ACT ×<rate> when RATE (actual) is under 90 %
of LEVEL and ACT is true; "ACT " goes when the line would pass 19 characters."
  (if paused
      "PAUSED"
      (let ((base (format nil "WARP ×~D" level)))
        (if (and act rate (< rate (* *readout-act-fraction* level)))
            (let ((full (format nil "~A ACT ×~A" base (format-rate rate))))
              (if (<= (length full) +strip-columns+)
                  full
                  (format nil "~A ×~A" base (format-rate rate))))
            base))))

(defun strip-message-duration (text)
  "s of real time TEXT stays on line 3: 3 s up to 19 characters, else until its
end has scrolled off the left at 12 characters a second."
  (if (<= (length text) +strip-columns+)
      *strip-message-hold*
      (/ (+ +strip-columns+ (length text)) *strip-scroll-speed*)))

(defun strip-message-shift (text elapsed)
  "Line 3's dots to the right ELAPSED s after TEXT began: 0 for a short one,
else from the right edge towards the left."
  (declare (type string text) (type double-float elapsed))
  (if (<= (length text) +strip-columns+)
      0
      (values (floor (- +strip-dot-columns+ (* 6d0 *strip-scroll-speed* elapsed))))))

(defun strip-enqueue! (cockpit clock)
  "The log's entries newer than the last seen, oldest first, on the queue
(at most *strip-queue-max*, the oldest dropped)."
  (let* ((log (clock-log clock))
         (seen (cockpit-state-strip-seen cockpit)))
    (unless (eq (first log) seen)
      (let* ((tail (and seen (member seen log :test #'eq)))
             (new (reverse (if tail (ldiff log tail) log))))
        (setf (cockpit-state-strip-queue cockpit)
              (let ((queue (append (cockpit-state-strip-queue cockpit) (mapcar #'third new))))
                (last queue *strip-queue-max*))
              (cockpit-state-strip-seen cockpit) (first log))))))

(defun strip-advance! (cockpit real-time)
  "The message on line 3: the one shown leaves when its time is up, the next
of the queue takes its place."
  (let ((current (cockpit-state-strip-current cockpit)))
    (when (and current (>= (- real-time (cockpit-state-strip-start cockpit))
                           (strip-message-duration current)))
      (setf current nil (cockpit-state-strip-current cockpit) nil))
    (when (and (null current) (cockpit-state-strip-queue cockpit))
      (setf (cockpit-state-strip-current cockpit) (pop (cockpit-state-strip-queue cockpit))
            (cockpit-state-strip-start cockpit) real-time))))

(defun strip-update! (cockpit view state clock real-time snap)
  "The strip's three lines and its dots: lines 1 and 2 every 0.1 s of REAL-TIME,
line 3 every frame. SNAP shows the log's latest message static."
  (declare (type cockpit-state cockpit) (type strip-view view) (type double-float real-time))
  (when (or (>= (- real-time (cockpit-state-strip-built cockpit)) *screen-rebuild-period*)
            (< real-time (cockpit-state-strip-built cockpit)))
    (setf (cockpit-state-strip-line-1 cockpit) (strip-line-1 (render-state-t state))
          (cockpit-state-strip-line-2 cockpit)
          (strip-line-2 (clock-level clock) (clock-actual-rate clock) (clock-paused clock)
                        (null (render-state-loss state)))
          (cockpit-state-strip-built cockpit) real-time))
  (cond (snap
         (let ((entry (first (clock-log clock))))
           (setf (cockpit-state-strip-queue cockpit) nil
                 (cockpit-state-strip-seen cockpit) entry
                 (cockpit-state-strip-current cockpit)
                 (and entry (subseq (third entry) 0 (min +strip-columns+ (length (third entry)))))
                 (cockpit-state-strip-start cockpit) real-time)))
        (t (strip-enqueue! cockpit clock)
           (strip-advance! cockpit real-time)))
  (let ((current (cockpit-state-strip-current cockpit)))
    (if current
        (strip-dots! view (cockpit-state-strip-line-1 cockpit) (cockpit-state-strip-line-2 cockpit)
                     current (strip-message-shift current (- real-time (cockpit-state-strip-start cockpit))))
        (strip-dots! view (cockpit-state-strip-line-1 cockpit) (cockpit-state-strip-line-2 cockpit)
                     (or (cockpit-state-strip-hover cockpit) "")))))

;;; COMMS (26 × 11): the job card, a rule, the SHIP log typed out.

(defparameter *comms-typing-rate* 60d0 "characters a second of real time.")
(defparameter *comms-columns* 26)
(defparameter *comms-indent* 6 "The continuation lines' spaces.")
(defparameter *comms-cursor* #\█)
(defparameter *comms-log-rows* 7 "Rows 5 to 11.")

(defun log-entry-text (entry)
  "ENTRY (t key text) as the log shows it: HH:MM and the text, game UTC."
  (multiple-value-bind (year month day hour minute) (t->utc (first entry))
    (declare (ignore year month day))
    (format nil "~2,'0D:~2,'0D ~A" hour minute (third entry))))

(defun wrap-log-text (text)
  "TEXT as lines of at most 26 characters, the continuation lines indented 6,
broken at spaces where it can."
  (let ((lines '()) (start 0) (n (length text)) (first t))
    (loop while (< start n)
          do (let* ((width (if first *comms-columns* (- *comms-columns* *comms-indent*)))
                    (limit (+ start width))
                    (end (cond ((>= limit n) n)
                               ((char= (char text limit) #\Space) limit)
                               (t (let ((space (position #\Space text :start start :end limit :from-end t)))
                                    (if (and space (> space start)) space limit)))))
                    (line (string-right-trim " " (subseq text start end))))
               (push (if first line (concatenate 'string (make-string *comms-indent* :initial-element #\Space) line))
                     lines)
               (setf start (if (and (< end n) (char= (char text end) #\Space)) (1+ end) end)
                     first nil)))
    (or (nreverse lines) (list ""))))

(defun comms-type! (cockpit clock real-time snap)
  "Advance the log's typing to REAL-TIME: 60 characters a second over the
entries not yet typed, in arrival order; SNAP types them all."
  (declare (type cockpit-state cockpit) (type double-float real-time))
  (let* ((log (clock-log clock))
         (typed (cockpit-state-typed cockpit)))
    (cond ((eq (first log) typed)
           (setf (cockpit-state-typing cockpit) 0d0))
          (snap
           (setf (cockpit-state-typed cockpit) (first log) (cockpit-state-typing cockpit) 0d0)
           (incf (cockpit-state-typing-serial cockpit)))
          (t
           (let* ((tail (and typed (member typed log :test #'eq)))
                  (pending (reverse (if tail (ldiff log tail) log)))
                  (last-time (cockpit-state-typing-time cockpit))
                  (dt (if (and last-time (>= real-time last-time)) (- real-time last-time) 0d0))
                  (before (floor (cockpit-state-typing cockpit)))
                  (progress (+ (cockpit-state-typing cockpit) (* *comms-typing-rate* dt))))
             (loop while pending
                   do (let ((n (length (log-entry-text (first pending)))))
                        (if (>= progress n)
                            (setf progress (- progress n)
                                  (cockpit-state-typed cockpit) (pop pending))
                            (return))))
             (setf (cockpit-state-typing cockpit) (if pending progress 0d0))
             (when (or (/= before (floor (cockpit-state-typing cockpit)))
                       (not (eq typed (cockpit-state-typed cockpit))))
               (incf (cockpit-state-typing-serial cockpit))))))
    (setf (cockpit-state-typing-time cockpit) real-time)))

(defun comms-log-text-lines (clock cockpit)
  "The log's lines oldest first as far as it has been typed, the cursor after
the last character."
  (let* ((entries (reverse (clock-log clock)))
         (typed (cockpit-state-typed cockpit))
         (position (and typed (position typed entries :test #'eq)))
         (done (if position (subseq entries 0 (1+ position)) '()))
         (next (and (< (length done) (length entries)) (nth (length done) entries)))
         (lines '()))
    (dolist (entry done)
      (setf lines (append lines (wrap-log-text (log-entry-text entry)))))
    (let ((partial (if next
                       (let ((text (log-entry-text next)))
                         (subseq text 0 (min (length text) (floor (cockpit-state-typing cockpit)))))
                       "")))
      (cond ((string= partial "")
             (if lines
                 (let ((last (car (last lines))))
                   (setf lines (append (butlast lines)
                                       (wrap-log-text (concatenate 'string last (string *comms-cursor*))))))
                 (setf lines (list (string *comms-cursor*)))))
            (t (setf lines (append lines (wrap-log-text (concatenate 'string partial (string *comms-cursor*))))))))
    lines))

(defun comms-lines (clock cockpit)
  "COMMS's lines: the job card, the rule and the log's window, scrolled by
COCKPIT's scroll (which new lines hold in place when it is not at the bottom)."
  (let* ((all (comms-log-text-lines clock cockpit))
         (total (length all))
         (maximum (max 0 (- total *comms-log-rows*)))
         (scroll (cockpit-state-scroll cockpit)))
    (when (plusp scroll)
      (incf scroll (- total (cockpit-state-log-lines cockpit))))
    (setf scroll (max 0 (min maximum scroll))
          (cockpit-state-scroll cockpit) scroll
          (cockpit-state-log-lines cockpit) total)
    (let* ((end (- total scroll)) (begin (max 0 (- end *comms-log-rows*))))
      (append (list (screen-line :grey "JOB")
                    (screen-line :white "NONE")     ; PLACEHOLDER(5): the job card
                    nil
                    (screen-line :grey (make-string *comms-columns* :initial-element #\-)))
              (mapcar (lambda (text) (screen-line :amber text)) (subseq all begin end))))))

(defun comms-scroll! (cockpit lines)
  "Scroll COMMS's log LINES up (down when negative); the next rebuild clamps it."
  (setf (cockpit-state-scroll cockpit) (max 0 (+ (cockpit-state-scroll cockpit) lines)))
  cockpit)

;;; The instruments of docs/design.md "Flight" → "Instruments", each at its
;;; place in the cockpit: (name panel lines-or-device); LINES a list of rows of
;;; the panel's grid, DEVICE a keyword the later pieces of plan 32 draw.

(defparameter *cockpit-instruments*
  '(("Altitude" :orbit (2))
    ("Speed" :orbit (3))
    ("Apoapsis and periapsis" :orbit (5 6))
    ("Throttle" :throttle :lever)
    ("Propellant" :ship (1))
    ("Attitude mode" :attitude (8))
    ("Warp" :strip (2))
    ("Clock and date" :strip (1))
    ("Alarms and messages" :caution-lamps :lamps)
    ("Inclination, eccentricity, period" :orbit (7 8))
    ("Vertical speed" :orbit (4))
    ("Attitude indicator" :attitude (1 2 3 4 5 6 7))
    ("Δv remaining" :ship (2))
    ("Mass and thrust-to-weight" :ship (3))
    ("Burn" :autopilot (1 2 3 4))
    ("Target" :target (1 2 3 4 5))
    ("Grapple range and alignment" :target (1 2 3))
    ("More alarms" :caution-lamps :lamps)
    ("Body and SOI" :orbit (1))
    ("Sun" :ship (7 8))
    ("Life support" :ship (6))
    ("Radio and messages" :comms (1 2 3)))
  "Every instrument of \"Flight\", \"Instruments\" and its place: the panel
and the grid rows or the device (decision 127).")
