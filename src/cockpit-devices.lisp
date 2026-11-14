(in-package #:luv-in-space)

;;; The cockpit's devices (plan 32: Cockpit instruments, buttons, the grapple
;;; camera and messages, piece 3; decision 128: Cockpit devices, buttons,
;;; picking, the grapple camera and messages in milestone 3): the ten lamps
;;; of the two columns and ENG on the throttle quadrant, the master alarm,
;;; the PROP and hull gauges with their lagged needles and the throttle
;;; lever. Lamp conditions are pure functions of the render state, the clock
;;; and the cockpit's state, rebuilt at 10 Hz; flashing, the needles and the
;;; lever are written every frame into :cab-lines segments and recoloured
;;; text records, consing nothing.

;;; Lamp conditions

(defparameter *lamps*
  '((:mstr "MSTR" :caution-lamps -0.22d0 :red)
    (:peri "PERI" :caution-lamps -0.11d0 :red)
    (:prop "PROP" :caution-lamps 0d0 :amber)
    (:temp "TEMP" :caution-lamps 0.11d0 :amber)        ; PLACEHOLDER(4): the hull's temperature
    (:life "LIFE" :caution-lamps 0.22d0 :amber)        ; PLACEHOLDER(4): life support
    (:warp "WARP" :status-lamps -0.22d0 :white)
    (:rcs "RCS" :status-lamps -0.11d0 :amber)
    (:auto "AUTO" :status-lamps 0d0 :white)
    (:grpl "GRPL" :status-lamps 0.11d0 :white)
    (:msg "MSG" :status-lamps 0.22d0 :white)            ; PLACEHOLDER(5): radio messages
    (:eng "ENG" :throttle nil :white))
  "(key engraving panel dz nominal-colour): the lamps in the order of the
cockpit-state's lamp vectors; DZ the lens's body z in its column (the lamp
columns' five rows, docs/design.md \"Layout\"), nil for ENG, which has its place
in the quadrant (*eng-lens*). The nominal colour tints the unlit lens.")

(defparameter *lamp-hold* 3d0 "s of real time WARP and GRPL flash after their message.")

(declaim (inline lamp-on-p))
(defun lamp-on-p (flash real-time)
  "Whether a lamp flashing as FLASH (nil, :fast or :slow) is on at REAL-TIME
(0 or more). Conses nothing."
  (declare (type (double-float 0d0 1d9) real-time))
  (flet ((first-half (period)
           (declare (type double-float period))
           (let ((phase (/ real-time period)))
             (< (- phase (the fixnum (truncate phase))) 0.5d0))))
    (case flash
      (:fast (first-half 0.5d0))
      (:slow (first-half 1d0))
      (t t))))

(defun message-recent-p (clock key)
  "Whether the last message of KEY came within *lamp-hold* s of the clock's real time."
  (let ((stamp (clock-message-stamp clock key)))
    (and stamp (< (- (clock-real-time clock) stamp) *lamp-hold*))))

(defun peri-alarm-p (state)
  "A periapsis below the crash radius, or an impact in the prediction."
  (let* ((orbit (state->orbit (render-state-r state) (render-state-v state) +mu-earth+
                              (render-state-t state)))
         (prediction (render-state-prediction state)))
    (or (< (orbit-periapsis-radius orbit) (body-crash-radius (body :earth)))
        (and prediction (prediction-event prediction :impact) t))))

(defun grapple-refused-recently-p (clock)
  "Whether the newest :grapple message, within 3 s, is a missed capture or an over-rating."
  (and (message-recent-p clock :grapple)
       (let ((entry (find :grapple (clock-log clock) :key #'second)))
         (and entry (or (search "NO CAPTURE" (third entry)) (search "RATING" (third entry))) t))))

(defun lamp-condition (key state clock cockpit)
  "(values colour flash) of the lamp KEY: COLOUR :red, :amber, :green, :white or
nil (unlit), FLASH nil, :fast (2 Hz) or :slow (1 Hz) (decision 128)."
  (let ((model (render-state-autopilot-model state))
        (controls (render-state-controls state)))
    (ecase key
      (:mstr (case (cockpit-state-mstr cockpit)
               (:red (values :red :fast))
               (:amber (values :amber nil))
               (t (values nil nil))))
      (:peri (if (peri-alarm-p state) (values :red nil) (values nil nil)))
      (:prop (if (propellant-low-p state) (values :amber nil) (values nil nil)))
      ((:temp :life :msg) (values nil nil))
      (:warp (cond ((or (message-recent-p clock :refused) (message-recent-p clock :lowered))
                    (values :amber :slow))
                   ((> (clock-level clock) 1) (values :white nil))
                   (t (values nil nil))))
      (:rcs (if (or (clock-kinematic-p clock)
                    (and (not (eq (controls-attitude-mode controls) :hold))
                         (> (vec3-norm (render-state-w state)) *warp-rotation-limit*)))
                (values :amber nil)
                (values nil nil)))
      (:auto (cond ((autopilot-abort-unread-p clock model) (values :red :fast))
                   ((or (render-state-autopilot state)
                        (and model (eq (autopilot-model-state model) :engaged)))
                    (values :green nil))
                   ((and model (member (autopilot-model-state model) '(:planning :shown)))
                    (values :white nil))
                   (t (values nil nil))))
      (:grpl (let ((grapple (render-grapple-state (render-state-grapple state))))
               (cond ((grapple-refused-recently-p clock) (values :amber :slow))
                     ((or (member grapple '(:closing :latched)) (render-state-berth-def state))
                      (values :green nil))
                     ((eq grapple :armed) (values :white nil))
                     (t (values nil nil)))))
      (:eng (if (and (> (engine-throttle (controls-throttle controls)) 0d0)
                     (> (render-state-m-p state) 0d0)
                     (not (render-state-berth-def state)))
                (values :white nil)
                (values nil nil))))))

(defun lamp-states (state clock cockpit)
  "((key colour flash) …) for every lamp, fresh: the pure form of the lamps'
conditions the tests read."
  (loop for (key) in *lamps*
        collect (multiple-value-bind (colour flash) (lamp-condition key state clock cockpit)
                  (list key colour flash))))

(defun mstr-update! (cockpit peri prop abort)
  "The master alarm's onsets (decision 128): a PERI or an autopilot abort that
has just come on flashes it red, a PROP one lights it amber, unless it is
flashing already; the alarms' states kept for the next call."
  (let ((red (or (and peri (not (cockpit-state-peri-lit cockpit)))
                 (and abort (not (cockpit-state-abort-lit cockpit)))))
        (amber (and prop (not (cockpit-state-prop-lit cockpit)))))
    (cond (red (setf (cockpit-state-mstr cockpit) :red))
          ((and amber (not (eq (cockpit-state-mstr cockpit) :red)))
           (setf (cockpit-state-mstr cockpit) :amber)))
    (setf (cockpit-state-peri-lit cockpit) peri
          (cockpit-state-prop-lit cockpit) prop
          (cockpit-state-abort-lit cockpit) abort)
    cockpit))

(defun cockpit-lamps-rebuild! (cockpit state clock)
  "Every lamp's colour and flash into COCKPIT's vectors, MSTR's onsets first."
  (mstr-update! cockpit
                (peri-alarm-p state)
                (propellant-low-p state)
                (autopilot-abort-unread-p clock (render-state-autopilot-model state)))
  (loop for (key) in *lamps*
        for i from 0
        do (multiple-value-bind (colour flash) (lamp-condition key state clock cockpit)
             (setf (svref (cockpit-state-lamp-colours cockpit) i) colour
                   (svref (cockpit-state-lamp-flashes cockpit) i) flash)))
  cockpit)

;;; The gauges' needles and the lever

(defparameter *needle-lag* 0.3d0 "s of real time: a needle's first-order lag.")
(defparameter *dial-sweep* 270d0 "degrees from the lower left to the lower right.")
(defparameter *hull-gauge-range* '(150d0 500d0) "K at the two stops.")

(declaim (inline lever-angle))
(defun lever-angle (throttle)
  "Degrees: 0 to 60 for the lever's 0 to 100 %, -10 at off."
  (declare (type double-float throttle))
  (if (<= throttle 0d0) -10d0 (* 60d0 (min 1d0 throttle))))

(defun prop-gauge-value (state)
  "The PROP gauge's fraction, 0 to 1."
  (max 0d0 (min 1d0 (/ (render-state-m-p state) *propellant-full*))))

(defun hull-gauge-value (state)
  "The hull gauge's fraction: at its 150 K stop until milestone 4 (PLACEHOLDER(4))."
  (declare (ignore state))
  0d0)

(defun cockpit-needles-update! (cockpit state real-time snap)
  "The needles' values one lag step towards the gauges' values at REAL-TIME;
SNAP sets them (captures, the first call)."
  (declare (type cockpit-state cockpit) (type double-float real-time))
  (let* ((needles (cockpit-state-needles cockpit))
         (last (cockpit-state-needle-time cockpit))
         (dt (if last (max 0d0 (min 1d0 (- real-time (the double-float last)))) 0d0))
         (k (- 1d0 (exp (/ (- dt) *needle-lag*)))))
    (declare (type double-float dt k))
    (flet ((advance (i target)
             (declare (type double-float target))
             (setf (aref needles i)
                   (if (or snap (null last))
                       target
                       (+ (aref needles i) (* k (- target (aref needles i))))))))
      (advance 0 (prop-gauge-value state))
      (advance 1 (hull-gauge-value state)))
    (setf (cockpit-state-needle-time cockpit) real-time)
    cockpit))

;;; Plane frames in single floats

(defstruct (plane-frame (:copier nil))
  "A point and three unit vectors, single-float triples: right, up, normal."
  (centre (make-array 3 :element-type 'single-float) :type (simple-array single-float (3)))
  (r (make-array 3 :element-type 'single-float) :type (simple-array single-float (3)))
  (u (make-array 3 :element-type 'single-float) :type (simple-array single-float (3)))
  (n (make-array 3 :element-type 'single-float) :type (simple-array single-float (3))))

(defun single-triple (v)
  (make-array 3 :element-type 'single-float
                :initial-contents (list (float (aref v 0) 1f0) (float (aref v 1) 1f0) (float (aref v 2) 1f0))))

(defun panel-plane-frame (panel-name &optional (a 0d0) (b 0d0) (c 0d0))
  "PANEL-NAME's frame with its origin moved A along right, B along up and C
along the normal."
  (multiple-value-bind (centre n r u) (panel-frame (cockpit-panel panel-name))
    (make-plane-frame :centre (single-triple (vlin centre a r b u c n))
                      :r (single-triple r) :u (single-triple u) :n (single-triple n))))

(declaim (inline plane-put!))
(defun plane-put! (region index frame a0 b0 c0 a1 b1 c1 colour width)
  "Segment INDEX of REGION from (A0 B0 C0) to (A1 B1 C1) in FRAME's right, up and
normal axes (metres), COLOUR a linear triple, WIDTH metres across."
  (declare (type line-region region) (type fixnum index) (type plane-frame frame)
           (type single-float a0 b0 c0 a1 b1 c1 width)
           (type (simple-array single-float (3)) colour)
           (optimize speed (safety 0)))
  (let ((o (plane-frame-centre frame)) (r (plane-frame-r frame))
        (u (plane-frame-u frame)) (n (plane-frame-n frame)))
    (flet ((x (k a b c) (+ (aref o k) (* a (aref r k)) (* b (aref u k)) (* c (aref n k)))))
      (declare (inline x))
      (line-put! region index (x 0 a0 b0 c0) (x 1 a0 b0 c0) (x 2 a0 b0 c0)
                 (x 0 a1 b1 c1) (x 1 a1 b1 c1) (x 2 a1 b1 c1) colour width))))

;;; The devices

(defparameter *lens-depth* 0.0015d0 "m: a lens, a dial's lines in front of the face.")
(defparameter *lamp-lens* '(:half-width 0.039d0 :half-height 0.021d0)
  "The lamp columns' lens: 0.078 × 0.042 m on a 0.08 m face.")
(defparameter *eng-lens* '(:panel :throttle :a 0.040d0 :b -0.060d0 :half-width 0.030d0 :half-height 0.0135d0)
  "ENG's lens on the throttle quadrant, lower right.")
(defparameter *unlit-share* 0.07 "The share of a lens's colour an unlit lens keeps.")
(defparameter *prop-dial* '(:panel :throttle :a 0d0 :b 0.030d0 :radius 0.050d0)
  "The PROP gauge: Ø 0.10 m on the quadrant's upper part.")
(defparameter *hull-dial* '(:panel :hull-gauge :a 0d0 :b 0d0 :radius 0.060d0 :depth 0.0075d0)
  "The hull gauge: Ø 0.12 m, its needle at the 150 K stop until milestone 4.")
(defparameter *lever* '(:panel :throttle :a 0d0 :b -0.068d0 :length 0.10d0 :width 0.012d0)
  "The throttle lever: pivot low on the quadrant, standing in the dial's gap.")
(defparameter *needle* '(:width 0.002d0 :length-per-diameter 0.42d0)
  "A needle: 2 mm wide, 0.42 of the dial's diameter long (decision 128's 0.42 of the radius would not reach the scale).")

(defstruct (lamp (:copier nil) (:constructor %make-lamp (key name colour frame half-width half-height region cap)))
  key name colour frame (half-width 0f0 :type single-float) (half-height 0f0 :type single-float)
  region (cap 0f0 :type single-float) (ink nil) (advance 0f0 :type single-float))

(defstruct (cockpit-devices (:copier nil) (:constructor %make-cockpit-devices ()))
  lamps                                  ; a list of lamp, *lamps*' order
  (dynamic nil)                          ; the line-region of lenses, needles and the lever
  (prop-frame nil) (hull-frame nil) (lever-frame nil)
  (prop-length 0f0 :type single-float) (hull-length 0f0 :type single-float)
  (lever-length 0f0 :type single-float) (lever-width 0f0 :type single-float)
  (static nil))

(defun lamp-ink-tints ()
  "A vector of (hex lit-tint unlit-tint) per colour key, made once."
  (loop for (key hex) in `((:red ,*screen-red*) (:amber ,*screen-amber*) (:green ,*screen-green*)
                           (:white ,*screen-white*))
        collect (let ((lit (screen-line-colour hex)))
                  (list key hex lit
                        (make-array 3 :element-type 'single-float
                                      :initial-contents (list (* *unlit-share* (aref lit 0))
                                                              (* *unlit-share* (aref lit 1))
                                                              (* *unlit-share* (aref lit 2))))))))

(defvar *lamp-tints* nil)

(defun lamp-tint (colour lit)
  (let ((entry (assoc colour (or *lamp-tints* (setf *lamp-tints* (lamp-ink-tints))))))
    (if lit (third entry) (fourth entry))))

(defun make-lamp (index spec atlas text-batch)
  (destructuring-bind (key name panel dz colour) spec
    (declare (ignore index))
    (let* ((eng (null dz))
           (frame (if eng
                      (panel-plane-frame (getf *eng-lens* :panel) (getf *eng-lens* :a) (getf *eng-lens* :b)
                                         *lens-depth*)
                      (panel-plane-frame panel 0d0 (- dz) *lens-depth*)))
           (half-width (if eng (getf *eng-lens* :half-width) (getf *lamp-lens* :half-width)))
           (half-height (if eng (getf *eng-lens* :half-height) (getf *lamp-lens* :half-height)))
           (cap (* 0.0176d0 (let ((c (plane-frame-centre frame)))
                             (sqrt (+ (expt (aref c 0) 2) (expt (aref c 1) 2) (expt (aref c 2) 2))))))
           (region (text-batch-region text-batch (length name)))
           (lamp (%make-lamp key name colour frame (float half-width 1f0) (float half-height 1f0) region (float cap 1f0))))
      (let ((origin (let ((c (plane-frame-centre frame)) (u (plane-frame-u frame)) (n (plane-frame-n frame)))
                      (loop for k below 3 collect (+ (aref c k) (* -0.5d0 cap (aref u k)) (* 0.002d0 (aref n k)))))))
        (multiple-value-bind (index advance)
            (shaped-run! atlas region 0
                         (make-text-plane :origin origin :right (coerce (plane-frame-r frame) 'list)
                                          :up (coerce (plane-frame-u frame) 'list))
                         name :font :label :cap (float cap 1f0) :kind :emissive :align :centre
                              :colour (screen-ink *screen-grey*))
          (declare (ignore index))
          (setf (lamp-advance lamp) (float advance 1f0))))
      lamp)))

(declaim (inline dial-direction))
(defun dial-direction (fraction)
  "(values right up) singles of the unit vector of a dial's FRACTION, 0 at the
lower left, 1 at the lower right."
  (declare (type real fraction))
  (let ((angle (* (- (* 270f0 (float fraction 1f0)) 135f0) 0.017453292f0)))
    (values (sin angle) (cos angle))))

(defun dial-static! (devices atlas text-batch line-batch)
  "The dials' scales: the rings, ticks, coloured arcs and numbers, made once."
  (let* ((region (line-batch-region line-batch 160))
         (index 0)
         (grey (screen-line-colour *screen-grey*)) (white (screen-line-colour *screen-white*))
         (red (screen-line-colour *screen-red*)) (amber (screen-line-colour *screen-amber*))
         (dark (screen-line-colour *screen-black*)))
    (flet ((segment (frame a0 b0 a1 b1 colour width)
             (plane-put! region index frame a0 b0 0f0 a1 b1 0f0 colour width)
             (incf index))
           (point (cx cy radius fraction)
             (multiple-value-bind (s c) (dial-direction fraction)
               (values (+ cx (* radius s)) (+ cy (* radius c))))))
      (flet ((arc (frame cx cy radius from to colour width)
               (let ((steps (max 1 (ceiling (* (- to from) *dial-sweep*) 10d0))))
                 (dotimes (k steps)
                   (multiple-value-bind (a0 b0) (point cx cy radius (+ from (* (- to from) (/ k steps))))
                     (multiple-value-bind (a1 b1) (point cx cy radius (+ from (* (- to from) (/ (1+ k) steps))))
                       (segment frame a0 b0 a1 b1 colour width))))))
             (tick (frame cx cy inner outer fraction colour width)
               (multiple-value-bind (a0 b0) (point cx cy inner fraction)
                 (multiple-value-bind (a1 b1) (point cx cy outer fraction)
                   (segment frame a0 b0 a1 b1 colour width))))
             (numeral (frame cx cy radius fraction string &optional (ink *screen-grey*))
               (let* ((cap 0.0135d0) (width (* (length string) +text-cell-width+ cap))
                      (region (text-batch-region text-batch (length string))))
                 (multiple-value-bind (a b) (point cx cy radius fraction)
                   (let ((origin (loop for k below 3
                                       collect (+ (aref (plane-frame-centre frame) k)
                                                  (* (- a (* 0.5 width)) (aref (plane-frame-r frame) k))
                                                  (* (- b (* 0.5 cap)) (aref (plane-frame-u frame) k))
                                                  (* 0.0015 (aref (plane-frame-n frame) k))))))
                     (grid-run! atlas region 0
                                (make-text-plane :origin origin :right (coerce (plane-frame-r frame) 'list)
                                                 :up (coerce (plane-frame-u frame) 'list))
                                string :font :mono :cap (float cap 1f0) :colour (screen-ink ink)
                                       :kind :emissive))))))
        ;; PROP: a ring, ticks every 10 %, a red arc below 5 %, 0 50 100.
        (let* ((frame (cockpit-devices-prop-frame devices)) (radius 0.043) (cy 0f0))
          (arc frame 0f0 cy radius 0d0 1d0 grey 0.0015)
          (arc frame 0f0 cy (+ radius 0.004) 0d0 0.05d0 red 0.004)
          (dotimes (k 11)
            (tick frame 0f0 cy (if (evenp k) 0.034 0.037) radius (/ k 10d0) white 0.0015))
          (numeral frame 0f0 cy 0.027 0d0 "0")
          (numeral frame 0f0 cy 0.022 0.5d0 "50")
          (numeral frame 0f0 cy 0.027 1d0 "100"))
        ;; The hull gauge: 150 to 500 K, amber 350-400, red 400-450, hatched beyond.
        (let* ((frame (cockpit-devices-hull-frame devices)) (radius 0.052)
               (fraction (lambda (kelvin) (/ (- kelvin 150d0) 350d0))))
          (arc frame 0f0 0f0 radius 0d0 (funcall fraction 350d0) dark 0.002)
          (arc frame 0f0 0f0 radius (funcall fraction 350d0) (funcall fraction 400d0) amber 0.005)
          (arc frame 0f0 0f0 radius (funcall fraction 400d0) (funcall fraction 450d0) red 0.005)
          (loop for k from 0 to 7
                do (tick frame 0f0 0f0 0.043 radius (funcall fraction (+ 150d0 (* 50d0 k))) dark 0.002))
          (loop for k from 0 below 7
                do (tick frame 0f0 0f0 0.044 0.056 (+ (funcall fraction 450d0) (* k (/ (- 1d0 (funcall fraction 450d0)) 6))) red 0.0015))
          (loop for kelvin in '(150d0 250d0 350d0 450d0)
                do (numeral frame 0f0 0f0 0.031 (funcall fraction kelvin) (format nil "~D" (round kelvin)) *screen-black*)))))
    (line-region-clear! region index)
    (setf (cockpit-devices-static devices) region)
    devices))

(defun make-cockpit-devices (atlas text-batch line-batch)
  "The devices' regions and static scales in the batches."
  (let* ((devices (%make-cockpit-devices)))
    (setf (cockpit-devices-prop-frame devices)
          (panel-plane-frame (getf *prop-dial* :panel) (getf *prop-dial* :a) (getf *prop-dial* :b) *lens-depth*)
          (cockpit-devices-hull-frame devices)
          (panel-plane-frame (getf *hull-dial* :panel) (getf *hull-dial* :a) (getf *hull-dial* :b) (getf *hull-dial* :depth))
          (cockpit-devices-lever-frame devices)
          (panel-plane-frame (getf *lever* :panel) (getf *lever* :a) (getf *lever* :b) *lens-depth*)
          (cockpit-devices-prop-length devices)
          (float (* (getf *needle* :length-per-diameter) 2 (getf *prop-dial* :radius)) 1f0)
          (cockpit-devices-hull-length devices)
          (float (* (getf *needle* :length-per-diameter) 2 (getf *hull-dial* :radius)) 1f0)
          (cockpit-devices-lever-length devices) (float (getf *lever* :length) 1f0)
          (cockpit-devices-lever-width devices) (float (getf *lever* :width) 1f0))
    (setf (cockpit-devices-lamps devices)
          (loop for spec in *lamps* for i from 0 collect (make-lamp i spec atlas text-batch)))
    (dial-static! devices atlas text-batch line-batch)
    (setf (cockpit-devices-dynamic devices) (line-batch-region line-batch 14))
    devices))


(defun lamp-engraving-ink! (lamp lit)
  "LAMP's engraving dark on a lit lens, grey on an unlit one; the records rewritten on a change."
  (let ((ink (if lit :dark :grey)))
    (unless (eq ink (lamp-ink lamp))
      (text-region-recolour! (lamp-region lamp)
                             (screen-ink (if lit *screen-black* *screen-grey*)))
      (setf (lamp-ink lamp) ink))))

(defun cockpit-devices-draw! (devices cockpit state real-time)
  "Every lens, the needles and the lever written for REAL-TIME. Conses nothing."
  (declare (type cockpit-devices devices) (type cockpit-state cockpit) (type double-float real-time))
  (let ((region (cockpit-devices-dynamic devices)) (index 0))
    (declare (type fixnum index))
    (dolist (lamp (cockpit-devices-lamps devices))
      (let* ((colour (svref (cockpit-state-lamp-colours cockpit) index))
             (lit (and colour (lamp-on-p (svref (cockpit-state-lamp-flashes cockpit) index) real-time)))
             (frame (lamp-frame lamp)) (hw (lamp-half-width lamp)))
        (plane-put! region index frame (- hw) 0f0 0f0 hw 0f0 0f0
                    (lamp-tint (if lit colour (lamp-colour lamp)) lit)
                    (* 2f0 (lamp-half-height lamp)))
        (lamp-engraving-ink! lamp lit)
        (incf index)))
    (let ((needles (cockpit-state-needles cockpit))
          (white (screen-line-colour *screen-white*)))
      (flet ((needle (frame length value &optional (ink white))
               (declare (type single-float length))
               (multiple-value-bind (s c) (dial-direction (float value 1f0))
                 (plane-put! region index frame 0f0 0f0 0f0 (* length s) (* length c) 0f0 ink
                             (float (getf *needle* :width) 1f0))
                 (incf index))))
        (needle (cockpit-devices-prop-frame devices) (cockpit-devices-prop-length devices) (aref needles 0))
        (needle (cockpit-devices-hull-frame devices) (cockpit-devices-hull-length devices) (aref needles 1)
                (screen-line-colour *screen-black*))))
    (let* ((angle (* (float (lever-angle (controls-throttle (render-state-controls state))) 1f0) 0.017453292f0))
           (s (sin angle)) (c (cos angle))
           (length (cockpit-devices-lever-length devices)))
      (plane-put! region index (cockpit-devices-lever-frame devices) 0f0 0f0 0f0
                  0f0 (* length s) (* length c) (screen-line-colour *screen-white*)
                  (cockpit-devices-lever-width devices)))
    devices))

(defun cockpit-devices-update! (devices cockpit state clock real-time signature snap)
  "Rebuild the lamps' conditions at 10 Hz of REAL-TIME or on a new SIGNATURE,
step the needles, draw the devices; SNAP (captures) snaps the needles."
  (declare (type double-float real-time) (type fixnum signature))
  (when (or (/= signature (cockpit-state-lamp-signature cockpit))
            (>= (- real-time (cockpit-state-lamp-built cockpit)) *screen-rebuild-period*)
            (< real-time (cockpit-state-lamp-built cockpit)))
    (cockpit-lamps-rebuild! cockpit state clock)
    (setf (cockpit-state-lamp-built cockpit) real-time
          (cockpit-state-lamp-signature cockpit) signature))
  (cockpit-needles-update! cockpit state real-time snap)
  (cockpit-devices-draw! devices cockpit state real-time))
