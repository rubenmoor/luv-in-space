(in-package #:luv-in-space)

;;; The cockpit's buttons and the pointer (plan 32: Cockpit instruments,
;;; buttons, the grapple camera and messages, piece 6; decision 128: Cockpit
;;; devices, buttons, picking, the grapple camera and messages in milestone
;;; 3): the 18 lenses of the buttons panel with their engravings, lighting and
;;; press flashes; the clickable rectangles (buttons, MSTR, the AUTOPILOT
;;; menu's rows, the COMMS face) in eye coordinates; the pointer's ray through
;;; the cockpit camera and the nearest hit; the hover outline and the key
;;; shown on the strip.

;;; The buttons

(defparameter *buttons*
  '((:pro "PRO" "PRO" :|1| "1" 0 0 :prograde)
    (:retr "RETR" "RET" :|2| "2" 0 1 :retrograde)
    (:nml "NML" "NOR" :|3| "3" 0 2 :normal)
    (:anml "ANML" "ANO" :|4| "4" 0 3 :anti-normal)
    (:rad+ "RAD+" "R+" :|5| "5" 0 4 :radial-out)
    (:rad- "RAD-" "R-" :|6| "6" 0 5 :radial-in)
    (:tgt "TGT" "TGT" :|7| "7" 1 0 :target)
    (:atgt "ATGT" "ATG" :|8| "8" 1 1 :anti-target)
    (:rpro "RPRO" "RPR" :|9| "9" 1 2 :relative-prograde)
    (:rret "RRET" "RRT" :|0| "0" 1 3 :relative-retrograde)
    (:inrt "INRT" "INR" :r "R" 1 4 :inertial)
    (:damp "DAMP" "RAT" :t "T" 1 5 nil)
    (:fine "FINE" "FIN" :f "F" 2 0 nil)
    (:grpl "GRPL" "GRP" :g "G" 2 1 nil)
    (:xfer "XFER" "XFR" :b "B" 2 2 nil)
    (:ap "AP" "AP" :o "O RET BKSP" 2 3 nil)
    (:wrp- "WRP-" "W-" :|,| "," 2 4 nil)
    (:wrp+ "WRP+" "W+" :|.| "." 2 5 nil))
  "(id name engraving key key-label row column hold): the 18 buttons, three rows
of six. NAME is the strip's (decision 128); the ENGRAVING is at most three
characters, what fits a 36 mm lens at the readability rule's cap. The key is the
one a click runs; AP's three are Return with a plan shown, Backspace engaged,
else O.")

(defparameter *button-lens* 0.036d0 "m: a lens's side.")
(defparameter *button-pitch* 0.042d0 "m between two lenses' centres.")
(defparameter *button-flash* 0.15d0 "s of real time a press lights its lens white.")
(defparameter *button-cap-per-distance* 0.0176d0)
(defparameter *outline-gap* 0.002d0 "m: the hover outline's distance from a lens.")
(defparameter *outline-pixels* 2d0)
(defparameter *comms-wheel-lines* 3 "Lines of COMMS's log a wheel notch scrolls.")

(defun button-index-for-key (key)
  "The position in *buttons* of the button whose key is KEY (AP's three), or nil."
  (position-if (lambda (b)
                 (or (eq key (fourth b))
                     (and (eq (first b) :ap) (member key '(:o :return :backspace)))))
               *buttons*))

(defun button-hover-text (id)
  "\"<NAME> [<KEY>]\" for the strip's line 3; nil when ID is no button."
  (let ((b (find id *buttons* :key #'first)))
    (when b (format nil "~A [~A]" (second b) (fifth b)))))

;;; Lighting

(defun button-colour (b state clock)
  "The colour key (:amber, :green, :white) the button entry B is lit in now, or
nil (decision 128). Conses nothing."
  (declare (type render-state state))
  (let* ((id (first b))
         (hold (eighth b))
         (controls (render-state-controls state))
         (model (render-state-autopilot-model state)))
    (cond (hold
           (and (eq (controls-attitude-mode controls) :hold)
                (eq (controls-hold controls) hold)
                (if (controls-settled controls) :green :amber)))
          (t (case id
               (:damp (and (eq (controls-attitude-mode controls) :rate-damping) :green))
               (:fine (and (controls-fine controls) :green))
               (:grpl (let ((grapple (render-grapple-state (render-state-grapple state))))
                        (cond ((or (member grapple '(:closing :latched)) (render-state-berth-def state)) :green)
                              ((eq grapple :armed) :white))))
               (:xfer (and (render-state-transfer state) :green))
               (:ap (cond ((or (render-state-autopilot state)
                               (and model (eq (autopilot-model-state model) :engaged)))
                           :green)
                          ((and model (eq (autopilot-model-state model) :shown)) :white)))
               (:wrp+ (and (> (clock-level clock) 1) :white))
               (t nil))))))

;;; The clickables

(defstruct (clickable (:copier nil))
  "A rectangle the pointer can hit: its ID (a button's keyword, :mstr, :comms,
:menu-row), INDEX (a menu row's), CENTRE, unit NORMAL (to the eye), RIGHT and UP
vec3 in eye coordinates, half sizes in metres, and ENABLED."
  id (index 0 :type fixnum)
  (centre (make-vec3) :type vec3) (normal (make-vec3) :type vec3)
  (right (make-vec3) :type vec3) (up (make-vec3) :type vec3)
  (half-width 0d0 :type double-float) (half-height 0d0 :type double-float)
  (enabled t))

(defun panel-clickable (id panel-name a b depth half-width half-height &optional (index 0))
  "The rectangle at A right, B up and DEPTH along the normal of PANEL-NAME's face."
  (multiple-value-bind (c n r u) (panel-frame (cockpit-panel panel-name))
    (make-clickable :id id :index index :centre (vlin c a r b u depth n)
                    :normal n :right r :up u
                    :half-width (float half-width 1d0) :half-height (float half-height 1d0))))

(defun button-centre (b)
  (values (* (- (seventh b) 2.5d0) *button-pitch*)
          (* (- 1 (sixth b)) *button-pitch*)))

(defun build-clickables ()
  "The clickables, a vector: the 18 buttons, MSTR, the COMMS face and the seven
menu rows (disabled until the menu shows)."
  (let ((list '()))
    (dolist (b *buttons*)
      (multiple-value-bind (a v) (button-centre b)
        (push (panel-clickable (first b) :buttons a v *lens-depth* (* 0.5d0 *button-lens*) (* 0.5d0 *button-lens*))
              list)))
    (let ((mstr (find :mstr *lamps* :key #'first)))
      (push (panel-clickable :mstr (third mstr) 0d0 (- (fourth mstr)) *lens-depth*
                             (getf *lamp-lens* :half-width) (getf *lamp-lens* :half-height))
            list))
    (destructuring-bind (w h) (getf (cockpit-panel :comms) :size)
      (push (panel-clickable :comms :comms 0d0 0d0 0d0 (* 0.5d0 w) (* 0.5d0 h)) list))
    (let* ((panel (cockpit-panel :autopilot)) (cap (panel-grid-cap panel)))
      (destructuring-bind (w h) (getf panel :size)
        (let* ((columns (first (getf panel :grid))) (width (* columns +text-cell-width+ cap))
               (top (- (* 0.5d0 h) 0.01d0)))
          (dotimes (k *autopilot-screen-rows*)
            (let ((row (panel-clickable :menu-row :autopilot
                                        (+ (- (* 0.5d0 w)) 0.01d0 (* 0.5d0 width))
                                        (- top (* 0.5d0 cap) (* k +text-cell-height+ cap))
                                        0.003d0 (* 0.5d0 width) (* 0.5d0 +text-cell-height+ cap) k)))
              (setf (clickable-enabled row) nil)
              (push row list))))))
    (coerce (nreverse list) 'simple-vector)))

(defvar *clickables* nil "The clickables, made once (clickables).")

(defun clickables ()
  (or *clickables* (setf *clickables* (build-clickables))))

(defun clickable-for (id &optional (index 0))
  (find-if (lambda (c) (and (eq (clickable-id c) id) (= (clickable-index c) index))) (clickables)))

;;; The pointer's ray and the pick

(defun cockpit-ray! (out view width height x y)
  "OUT, the unit direction in eye (body) axes of the pixel X, Y of a WIDTH x HEIGHT
canvas seen by the cockpit camera of VIEW."
  (declare (type vec3 out) (type cockpit-view view) (type real width height x y))
  (let* ((tan-half (tan (* 0.5d0 (cockpit-view-fov view))))
         (aspect (/ (float width 1d0) (float height 1d0)))
         (cx (* (- (/ (* 2d0 x) width) 1d0) tan-half aspect))
         (cy (* (- (/ (* 2d0 y) height) 1d0) tan-half))
         (head (make-quat)) (v (vec3 1d0 cx cy)))
    (cockpit-head-quat! head view)
    (quat-rotate! out head v)
    (vec3-normalize out)))

(defun clickable-distance (c ray)
  "The distance along RAY (from the eye) to rectangle C, or nil when it misses."
  (declare (type clickable c) (type vec3 ray))
  (let* ((n (clickable-normal c))
         (dn (vec3-dot ray n))
         (centre (clickable-centre c)))
    (when (< dn -1d-9)
      (let ((distance (/ (vec3-dot centre n) dn)))
        (when (> distance 0d0)
          (let* ((dx (- (* distance (aref ray 0)) (aref centre 0)))
                 (dy (- (* distance (aref ray 1)) (aref centre 1)))
                 (dz (- (* distance (aref ray 2)) (aref centre 2)))
                 (right (clickable-right c)) (up (clickable-up c)))
            (when (and (<= (abs (+ (* dx (aref right 0)) (* dy (aref right 1)) (* dz (aref right 2))))
                           (clickable-half-width c))
                       (<= (abs (+ (* dx (aref up 0)) (* dy (aref up 1)) (* dz (aref up 2))))
                           (clickable-half-height c)))
              distance)))))))

(defun cockpit-pick (view width height x y &key (menu-rows 0) (clickables (clickables)))
  "(values id index clickable) of the nearest enabled clickable under the pixel X, Y
(a WIDTH x HEIGHT canvas, the cockpit camera of VIEW), or nil. The first MENU-ROWS
menu rows count."
  (let ((ray (make-vec3)) (best nil) (best-distance 0d0))
    (cockpit-ray! ray view width height x y)
    (loop for c across clickables
          do (when (and (clickable-enabled-p c menu-rows))
               (let ((d (clickable-distance c ray)))
                 (when (and d (or (null best) (< d best-distance)))
                   (setf best c best-distance d)))))
    (when best
      (values (clickable-id best) (clickable-index best) best))))

(defun clickable-enabled-p (c menu-rows)
  (if (eq (clickable-id c) :menu-row)
      (< (clickable-index c) menu-rows)
      (clickable-enabled c)))

(defun game-menu-rows (game)
  "How many of the AUTOPILOT menu's rows show now: 0 unless the menu or a refusal is up."
  (let* ((clock (game-clock game)) (model (clock-autopilot clock)) (sim (game-sim game)))
    (if (and model (member (autopilot-model-state model) '(:menu :refused)))
        (min *autopilot-screen-rows* (length (autopilot-offered-rows model sim)))
        0)))

(defun game-pick (game x y)
  "cockpit-pick for GAME's pointer at X, Y in the cockpit view."
  (destructuring-bind (width height) (game-extent game)
    (cockpit-pick (game-cockpit-view game) width height x y :menu-rows (game-menu-rows game))))

(defun cockpit-pointer-active-p (game)
  "The pointer acts on the cockpit: the cockpit view, the right button up, not lost, no flight."
  (and (eq (game-view game) :cockpit)
       (not (input-drag (game-input game)))
       (not (sim-lost-p (game-sim game)))
       (not (flight-running-p game))))

(defun menu-select-row! (game index)
  "Select the INDEXth row of the menu's window (as Up and Down would)."
  (let* ((clock (game-clock game)) (model (clock-autopilot clock)) (sim (game-sim game)))
    (autopilot-sync! model sim)
    (let* ((rows (autopilot-offered-rows model sim))
           (selected (position (autopilot-row-key (autopilot-selected-row model sim)) rows
                               :key #'autopilot-row-key))
           (row (nth (+ (menu-window-start (length rows) selected *autopilot-screen-rows*) index) rows)))
      (when (and row (member (autopilot-model-state model) '(:menu :refused)))
        (autopilot-drop! model sim)
        (setf (autopilot-model-selection model) (autopilot-row-key row))))))

(defun ap-click-key (game)
  "AP's key: Backspace engaged, Return with a plan shown, else O."
  (let* ((sim (game-sim game)) (model (clock-autopilot (game-clock game))))
    (cond ((sim-plan sim) :backspace)
          ((and model (eq (autopilot-model-state model) :shown)) :return)
          (t :o))))

(defun cockpit-click! (game x y)
  "A left press at pixel X, Y in the cockpit: the key of the button hit through the
key path's gating, MSTR as Y, a menu row selected, the COMMS face marking the log read."
  (when (cockpit-pointer-active-p game)
    (multiple-value-bind (id index) (game-pick game x y)
      (case id
        ((nil))
        (:mstr (key-pressed! game :y))
        (:comms (clock-acknowledge! (game-clock game)))
        (:menu-row (menu-select-row! game index))
        (t (let ((b (find id *buttons* :key #'first)))
             (key-pressed! game (if (eq id :ap) (ap-click-key game) (fourth b)))))))))

(defun cockpit-comms-under-pointer-p (game)
  "The pointer rests on the COMMS face (the wheel scrolls its log)."
  (let* ((input (game-input game)) (x (input-pointer-x input)) (y (input-pointer-y input)))
    (and x y (eq (cockpit-pick-id game x y) :comms))))

(defun cockpit-pick-id (game x y)
  (values (game-pick game x y)))

;;; Press flashes, hover

(defun button-press! (cockpit key real-time)
  "A press of KEY lights its button's lens for *button-flash*."
  (let ((i (button-index-for-key key)))
    (when i
      (setf (aref (cockpit-state-press-times cockpit) i) real-time))))

(defun cockpit-hover-update! (game)
  "The cockpit state's hover from the game's pointer: the id and index under it (nil
when inactive), the strip's key text and the metres a pixel spans at 1 m."
  (let* ((cockpit (game-cockpit-state game)) (input (game-input game))
         (x (input-pointer-x input)) (y (input-pointer-y input)))
    (multiple-value-bind (id index)
        (if (and x y (cockpit-pointer-active-p game)) (game-pick game x y) nil)
      (setf (cockpit-state-hover-id cockpit) id
            (cockpit-state-hover-index cockpit) (or index 0)
            (cockpit-state-strip-hover cockpit) (and id (button-hover-text id))
            (cockpit-state-pixel-scale cockpit)
            (/ (* 2d0 (tan (* 0.5d0 (cockpit-view-fov (game-cockpit-view game)))))
               (float (second (game-extent game)) 1d0))))
    cockpit))

;;; Drawing

(defstruct (button-lens (:copier nil))
  entry frame (region nil) (cap 0f0 :type single-float) (advance 0f0 :type single-float) (ink nil))

(defstruct (cockpit-buttons (:copier nil) (:constructor %make-cockpit-buttons ()))
  (lenses '()) (lens-region nil) (outline-region nil))

(defun new-button-lens (entry atlas text-batch)
  (multiple-value-bind (a v) (button-centre entry)
    (let* ((frame (panel-plane-frame :buttons a v *lens-depth*))
           (c (plane-frame-centre frame))
           (distance (sqrt (+ (expt (aref c 0) 2) (expt (aref c 1) 2) (expt (aref c 2) 2))))
           (cap (* *button-cap-per-distance* distance))
           (name (third entry))
           (region (text-batch-region text-batch (length name)))
           (lens (make-button-lens :entry entry :frame frame :region region :cap (float cap 1f0))))
      (let ((origin (loop for k below 3
                          collect (+ (aref c k) (* -0.5d0 cap (aref (plane-frame-u frame) k))
                                     (* 0.002d0 (aref (plane-frame-n frame) k))))))
        (multiple-value-bind (index advance)
            (shaped-run! atlas region 0
                         (make-text-plane :origin origin :right (coerce (plane-frame-r frame) 'list)
                                          :up (coerce (plane-frame-u frame) 'list))
                         name :font :label :cap (float cap 1f0) :kind :emissive :align :centre
                              :colour (screen-ink *screen-grey*))
          (declare (ignore index))
          (setf (button-lens-advance lens) (float advance 1f0))))
      lens)))

(defun make-cockpit-buttons (atlas text-batch line-batch)
  (let ((buttons (%make-cockpit-buttons)))
    (setf (cockpit-buttons-lenses buttons)
          (loop for entry in *buttons* collect (new-button-lens entry atlas text-batch))
          (cockpit-buttons-lens-region buttons) (line-batch-region line-batch (length *buttons*))
          (cockpit-buttons-outline-region buttons) (line-batch-region line-batch 4))
    (line-region-clear! (cockpit-buttons-outline-region buttons))
    buttons))

(defun button-engraving-ink! (lens lit)
  (let ((ink (if lit :dark :grey)))
    (unless (eq ink (button-lens-ink lens))
      (text-region-recolour! (button-lens-region lens)
                             (screen-ink (if lit *screen-black* *screen-grey*)))
      (setf (button-lens-ink lens) ink))))

(defun cockpit-outline-draw! (region cockpit)
  "The hover outline: 4 segments 2 mm outside the hovered lens, 2 pixels wide, or none."
  (let* ((id (cockpit-state-hover-id cockpit))
         (c (and id (not (member id '(:comms :menu-row))) (clickable-for id))))
    (if (null c)
        (when (cockpit-state-outline-lit cockpit)
          (line-region-clear! region)
          (setf (cockpit-state-outline-lit cockpit) nil))
        (let* ((centre (clickable-centre c)) (n (clickable-normal c))
               (r (clickable-right c)) (u (clickable-up c))
               (distance (vec3-norm centre))
               (width (float (* *outline-pixels* (cockpit-state-pixel-scale cockpit) distance) 1f0))
               (hw (+ (clickable-half-width c) *outline-gap* (* 0.5d0 width)))
               (hh (+ (clickable-half-height c) *outline-gap* (* 0.5d0 width)))
               (white (screen-line-colour *screen-white*)))
          (flet ((corner (sr su k)
                   (+ (aref centre k) (* sr hw (aref r k)) (* su hh (aref u k)) (* 0.0005d0 (aref n k)))))
            (loop for (sr0 su0 sr1 su1) in '((-1 -1 1 -1) (1 -1 1 1) (1 1 -1 1) (-1 1 -1 -1))
                  for i from 0
                  do (line-put! region i
                                (float (corner sr0 su0 0) 1f0) (float (corner sr0 su0 1) 1f0) (float (corner sr0 su0 2) 1f0)
                                (float (corner sr1 su1 0) 1f0) (float (corner sr1 su1 1) 1f0) (float (corner sr1 su1 2) 1f0)
                                white width)))
          (setf (cockpit-state-outline-lit cockpit) t)))))

(defun cockpit-buttons-draw! (buttons cockpit state clock real-time)
  "Every lens lit for its function or a press flash, its engraving inked, and the
hover outline, for REAL-TIME."
  (declare (type double-float real-time))
  (let ((region (cockpit-buttons-lens-region buttons)) (index 0))
    (declare (type fixnum index))
    (dolist (lens (cockpit-buttons-lenses buttons))
      (let* ((colour (if (< (- real-time (aref (cockpit-state-press-times cockpit) index)) *button-flash*)
                         :white
                         (button-colour (button-lens-entry lens) state clock)))
             (frame (button-lens-frame lens)) (hw (float (* 0.5d0 *button-lens*) 1f0)))
        (plane-put! region index frame (- hw) 0f0 0f0 hw 0f0 0f0
                    (lamp-tint (or colour :white) (and colour t)) (* 2f0 hw))
        (button-engraving-ink! lens (and colour t))
        (incf index))))
  (cockpit-outline-draw! (cockpit-buttons-outline-region buttons) cockpit)
  buttons)
