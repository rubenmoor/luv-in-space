(in-package #:luv-in-space)

;;; The cockpit's screens: their lines as pure functions, the line batch of
;;; the :cab-lines pipeline, the 10 Hz rebuild of each screen's text region,
;;; and the table of instruments (plan 32: Cockpit instruments, buttons, the
;;; grapple camera and messages; decision 127: Cockpit screens' contents in
;;; milestone 3: lines, modes, plots, colours and updates). So far ORBIT and
;;; SHIP; the other faces keep PLACEHOLDER(166) until their pieces.

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

(defun propellant-low-p (state)
  "Whether PROP is lit: below 5 % of the load (plan 32 piece 3 adds the shown plan's need)."
  (< (render-state-m-p state) (* 0.05d0 *propellant-full*)))

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

;;; The screens, by name: their grids come from *cockpit-panels*.

(defun screen-lines (name state clock real-time)
  "NAME's lines (lists of runs), cut to the grid, as a pure function of the
render STATE, the CLOCK and REAL-TIME."
  (declare (ignorable clock))
  (let ((columns (first (getf (cockpit-panel name) :grid))))
    (mapcar (lambda (line) (screen-fit-line line columns))
            (ecase name
              (:orbit (orbit-lines state real-time))
              (:ship (ship-lines state))))))

;;; A screen's text and line regions and what the rebuild needs.

(defstruct (screen-view (:copier nil) (:constructor %make-screen-view))
  name panel columns rows cap
  text lines                       ; the text-region and the line-region
  plane scratch line-plane)        ; text planes: at the grid, a shifted copy, the lines'

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
                            :line-plane (plane (vlin origin -0.001d0 n))))))))

(defun cockpit-screens-init! (text-batch line-batch)
  "A screen-view for each of *cockpit-live-screens*, its regions made in the batches."
  (mapcar (lambda (name) (make-screen-view name text-batch line-batch)) *cockpit-live-screens*))

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
  (rebuilds 0 :type fixnum))

(defun cockpit-state-invalidate! (cockpit)
  "The next frame rebuilds every screen (a restart, a loaded state)."
  (fill (cockpit-state-rebuilt cockpit) -1d9)
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
        (mix (sxhash (autopilot-model-selection model))))
      (mix (clock-level clock))
      (mix (if (clock-paused clock) 1 0))
      (mix (if (render-state-berth-def state) 1 0))
      (mix (if (render-state-transfer state) 1 0))
      (mix (if (render-state-load-def state) 1 0))
      (mix (length (clock-log clock))))
    h))

(defun cockpit-update! (cockpit views atlas state clock real-time view)
  "Rebuild the text of each of VIEWS (screen-views) whose last rebuild is 0.1 s
of REAL-TIME old, or was made under another cockpit-signature. COCKPIT the
game's cockpit-state; returns the number rebuilt."
  (declare (type cockpit-state cockpit) (type double-float real-time))
  (let ((signature (cockpit-signature state clock view)) (built 0))
    (declare (type fixnum signature built))
    (loop for screen in views
          for i of-type fixnum from 0
          do (when (or (/= signature (aref (cockpit-state-signatures cockpit) i))
                       (>= (- real-time (aref (cockpit-state-rebuilt cockpit) i))
                           *screen-rebuild-period*)
                       (< real-time (aref (cockpit-state-rebuilt cockpit) i)))
               (screen-view-draw! screen atlas
                                  (screen-lines (screen-view-name screen) state clock real-time))
               (setf (aref (cockpit-state-rebuilt cockpit) i) real-time
                     (aref (cockpit-state-signatures cockpit) i) signature)
               (incf built)
               (incf (cockpit-state-rebuilds cockpit))))
    built))

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
