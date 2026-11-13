(in-package #:luv-in-space)

;;; The screen-space overlay: dot-matrix labels (decision 7: Placeholder
;;; labels: dot-matrix overlay quads) and their pipeline.

(defparameter *glyphs*
  '((#\P "####." "#...#" "#...#" "####." "#...." "#...." "#....")
    (#\L "#...." "#...." "#...." "#...." "#...." "#...." "#####")
    (#\A ".###." "#...#" "#...#" "#####" "#...#" "#...#" "#...#")
    (#\C ".###." "#...#" "#...." "#...." "#...." "#...#" ".###.")
    (#\E "#####" "#...." "#...." "####." "#...." "#...." "#####")
    (#\H "#...#" "#...#" "#...#" "#####" "#...#" "#...#" "#...#")
    (#\O ".###." "#...#" "#...#" "#...#" "#...#" "#...#" ".###.")
    (#\D "####." "#...#" "#...#" "#...#" "#...#" "#...#" "####.")
    (#\R "####." "#...#" "#...#" "####." "#.#.." "#..#." "#...#")
    (#\B "####." "#...#" "#...#" "####." "#...#" "#...#" "####.")
    (#\F "#####" "#...." "#...." "####." "#...." "#...." "#....")
    (#\G ".###." "#...#" "#...." "#.###" "#...#" "#...#" ".####")
    (#\I ".###." "..#.." "..#.." "..#.." "..#.." "..#.." ".###.")
    (#\J "..###" "...#." "...#." "...#." "...#." "#..#." ".##..")
    (#\K "#...#" "#..#." "#.#.." "##..." "#.#.." "#..#." "#...#")
    (#\M "#...#" "##.##" "#.#.#" "#.#.#" "#...#" "#...#" "#...#")
    (#\N "#...#" "#...#" "##..#" "#.#.#" "#..##" "#...#" "#...#")
    (#\Q ".###." "#...#" "#...#" "#...#" "#.#.#" "#..#." ".##.#")
    (#\S ".####" "#...." "#...." ".###." "....#" "....#" "####.")
    (#\T "#####" "..#.." "..#.." "..#.." "..#.." "..#.." "..#..")
    (#\U "#...#" "#...#" "#...#" "#...#" "#...#" "#...#" ".###.")
    (#\V "#...#" "#...#" "#...#" "#...#" "#...#" ".#.#." "..#..")
    (#\W "#...#" "#...#" "#...#" "#.#.#" "#.#.#" "#.#.#" ".#.#.")
    (#\X "#...#" "#...#" ".#.#." "..#.." ".#.#." "#...#" "#...#")
    (#\Y "#...#" "#...#" ".#.#." "..#.." "..#.." "..#.." "..#..")
    (#\Z "#####" "....#" "...#." "..#.." ".#..." "#...." "#####")
    (#\Space "....." "....." "....." "....." "....." "....." ".....")
    (#\- "....." "....." "....." "#####" "....." "....." ".....")
    (#\. "....." "....." "....." "....." "....." ".##.." ".##..")
    (#\: "....." ".##.." ".##.." "....." ".##.." ".##.." ".....")
    (#\( "..#.." ".#..." "#...." "#...." "#...." ".#..." "..#..")
    (#\) "..#.." "...#." "....#" "....#" "....#" "...#." "..#..")
    (#\0 ".###." "#...#" "#..##" "#.#.#" "##..#" "#...#" ".###.")
    (#\1 "..#.." ".##.." "..#.." "..#.." "..#.." "..#.." ".###.")
    (#\2 ".###." "#...#" "....#" "...#." "..#.." ".#..." "#####")
    (#\3 "####." "....#" "....#" ".###." "....#" "....#" "####.")
    (#\4 "...#." "..##." ".#.#." "#..#." "#####" "...#." "...#.")
    (#\5 "#####" "#...." "####." "....#" "....#" "#...#" ".###.")
    (#\6 ".###." "#...." "#...." "####." "#...#" "#...#" ".###.")
    (#\7 "#####" "....#" "...#." "..#.." ".#..." ".#..." ".#...")
    (#\8 ".###." "#...#" "#...#" ".###." "#...#" "#...#" ".###.")
    (#\9 ".###." "#...#" "#...#" ".####" "....#" "....#" ".###.")
    (#\% "##..." "##..#" "...#." "..#.." ".#..." "#..##" "...##")
    (#\/ "....." "....#" "...#." "..#.." ".#..." "#...." ".....")
    (#\! "..#.." "..#.." "..#.." "..#.." "..#.." "....." "..#..")
    (#\+ "....." "..#.." "..#.." "#####" "..#.." "..#.." ".....")
    (#\, "....." "....." "....." "....." ".##.." "..#.." ".#...")
    (#\° ".##.." "#..#." "#..#." ".##.." "....." "....." ".....")
    (#\× "....." "#...#" ".#.#." "..#.." ".#.#." "#...#" ".....")
    (#\… "....." "....." "....." "....." "....." "....." "#.#.#")
    (#\> ".#..." "..#.." "...#." "....#" "...#." "..#.." ".#...")
    (#\< "...#." "..#.." ".#..." "#...." ".#..." "..#.." "...#.")
    (#\? ".###." "#...#" "....#" "...#." "..#.." "....." "..#..")
    (#\' "..#.." "..#.." ".#..." "....." "....." "....." ".....")
    (#\[ ".###." ".#..." ".#..." ".#..." ".#..." ".#..." ".###.")
    (#\] ".###." "...#." "...#." "...#." "...#." "...#." ".###.")
    (#\= "....." "....." "#####" "....." "#####" "....." ".....")
    (#\; "....." ".##.." ".##.." "....." ".##.." "..#.." ".#...")
    (#\_ "....." "....." "....." "....." "....." "....." "#####")
    (#\· "....." "....." "..#.." ".###." "..#.." "....." ".....")
    (#\Δ "..#.." "..#.." ".#.#." ".#.#." "#...#" "#...#" "#####")
    (#\− "....." "....." "....." "#####" "....." "....." ".....")
    (#\* "....." "#.#.#" ".###." "#####" ".###." "#.#.#" ".....")
    (#\# ".#.#." ".#.#." "#####" ".#.#." "#####" ".#.#." ".#.#."))
  "5x7 dot-matrix glyphs: A-Z, 0-9, space, - . : ( ) and decision 85's
% / ! + , ° × …, > (the autopilot menu's selection, decision 118) and
< ? ' [ ] = ; _ · Δ − * # (the messages of plan 24 to plan 27 and the cockpit's strip,
decision 128); a character missing here draws blank (decision 85: the
tests require every one used, glyph-missing).")

(defparameter *label-dot-px* 2 "Dot pitch of labels, cap 14 px; decision 85.")
(defparameter *marker-line-px* 2 "Ring and cross line width; decision 84.")
(defparameter *marker-ring-px* 12 "The ship's ring; decision 84.")
(defparameter *marker-diamond-px* 9 "Apsis diamonds; decision 84.")
(defparameter *marker-cross-px* 12 "The impact X; decision 84.")
(defparameter *marker-dot-px* 7 "Bodies below 6 px; decision 84.")
(defparameter *marker-box-px* 12 "The target's box; decision 125: Map in milestone 3.")
(defparameter *marker-triangle-px* 10 "Closest approach; decision 125.")
(defparameter *marker-plus-px* 12 "Burns; decision 125.")
(defparameter *overlay-max-vertices* 165312
  "Capacity of the frame's overlay buffer (a multiple of 6, so whole quads
are kept), by decision 129: twice the fullest readout (29,478 vertices),
autopilot panel (13 lines of 30 characters, 39,786) and message line (71
characters, 7,248) plus 12,288 for the map's markers and labels.")

(defun glyph (char)
  "CHAR's seven rows, upcased, or nil."
  (rest (assoc (char-upcase char) *glyphs*)))

(defun glyph-missing (string)
  "The first character of STRING without a glyph, as written, or nil."
  (find-if-not (lambda (char) (assoc char *glyphs*)) string))

(defconstant +overlay-vertex-floats+ 7 "Anchor xy, pixel offset xy, colour rgb.")

(defstruct (overlay-buffer (:constructor %make-overlay-buffer))
  "Preallocated overlay vertices: COUNT of them in FLOATS. Vertices past
the capacity are dropped."
  (floats nil :type (simple-array single-float (*)))
  (count 0 :type fixnum))

(defun make-overlay-buffer (&optional (vertices *overlay-max-vertices*))
  (%make-overlay-buffer :floats (make-array (* +overlay-vertex-floats+ vertices)
                                            :element-type 'single-float
                                            :initial-element 0f0)))

(defun overlay-clear! (out)
  (setf (overlay-buffer-count out) 0)
  out)

(declaim (inline overlay-vertex! overlay-triangle! overlay-quad!))
(defun overlay-vertex! (out x y r g b)
  "One vertex at pixel X, Y from the top left, linear colour R G B."
  (declare (type overlay-buffer out) (type single-float x y r g b))
  (let* ((floats (overlay-buffer-floats out))
         (o (* +overlay-vertex-floats+ (overlay-buffer-count out))))
    (when (<= (+ o +overlay-vertex-floats+) (length floats))
      (setf (aref floats o) -1f0 (aref floats (+ o 1)) -1f0
            (aref floats (+ o 2)) x (aref floats (+ o 3)) y
            (aref floats (+ o 4)) r (aref floats (+ o 5)) g (aref floats (+ o 6)) b)
      (incf (overlay-buffer-count out)))))

(defun overlay-triangle! (out x1 y1 x2 y2 x3 y3 r g b)
  (overlay-vertex! out x1 y1 r g b)
  (overlay-vertex! out x2 y2 r g b)
  (overlay-vertex! out x3 y3 r g b))

(defun overlay-quad! (out x1 y1 x2 y2 x3 y3 x4 y4 r g b)
  "The quad 1 2 3 4, corners in order, as two triangles."
  (overlay-triangle! out x1 y1 x2 y2 x3 y3 r g b)
  (overlay-triangle! out x1 y1 x3 y3 x4 y4 r g b))

(defmacro with-linear-colour ((r g b) rgb &body body)
  "R G B: the single-float linear channels of the sRGB integer RGB."
  (let ((c (gensym)))
    `(let* ((,c ,rgb)
            (,r (coerce (srgb-channel-linear ,c 16) 'single-float))
            (,g (coerce (srgb-channel-linear ,c 8) 'single-float))
            (,b (coerce (srgb-channel-linear ,c 0) 'single-float)))
       ,@body)))

(defun marker-size (kind)
  (ecase kind
    (:ring *marker-ring-px*) (:diamond *marker-diamond-px*)
    (:cross *marker-cross-px*) (:dot *marker-dot-px*)
    (:box *marker-box-px*) (:triangle *marker-triangle-px*) (:plus *marker-plus-px*)))

(defun marker-vertices! (out kind x y colour &optional size)
  "Triangles of marker KIND centred on pixel X, Y within a SIZE box (default
the kind's, decision 84: Map ribbons and markers: osculating orbit, adaptive
segments, overlay markers and visibility) in the sRGB integer COLOUR:
:ring a 16-segment outline, :diamond, :cross two bars, :dot a 12-gon,
:box a square outline, :triangle a filled one pointing up, :plus two bars. Conses
nothing for real X, Y that are not boxed doubles."
  (declare (optimize speed) (type overlay-buffer out) (type real x y)
           (type (unsigned-byte 24) colour))
  (let* ((x (coerce x 'single-float)) (y (coerce y 'single-float))
         (h (* 0.5f0 (coerce (the real (or size (marker-size kind))) 'single-float)))
         (line (coerce (the real *marker-line-px*) 'single-float)))
    (declare (type single-float x y h line))
    (with-linear-colour (r g b) colour
      (ecase kind
        (:ring
         (let ((inner (max 0f0 (- h line))) (step (/ (* 2 (coerce pi 'single-float)) 16)))
           (dotimes (k 16)
             (let ((c0 (cos (* k step))) (s0 (sin (* k step)))
                   (c1 (cos (* (1+ k) step))) (s1 (sin (* (1+ k) step))))
               (overlay-quad! out (+ x (* h c0)) (+ y (* h s0)) (+ x (* h c1)) (+ y (* h s1))
                              (+ x (* inner c1)) (+ y (* inner s1))
                              (+ x (* inner c0)) (+ y (* inner s0)) r g b)))))
        (:diamond
         (overlay-quad! out (- x h) y x (- y h) (+ x h) y x (+ y h) r g b))
        (:cross
         ;; Corners at ±(a ± w) with w = line / (2 √2) keep the bars in the box.
         (let* ((w (/ line (* 2 (sqrt 2f0)))) (a (- h w)))
           (overlay-quad! out (- x a w) (- y a (- w)) (- x a (- w)) (- y a w)
                          (+ x a w) (+ y a (- w)) (+ x a (- w)) (+ y a w) r g b)
           (overlay-quad! out (- x a w) (+ y a (- w)) (- x a (- w)) (+ y a w)
                          (+ x a w) (- y a (- w)) (+ x a (- w)) (- y a w) r g b)))
        (:box
         (let ((i (max 0f0 (- h line))))
           (overlay-quad! out (- x h) (- y h) (+ x h) (- y h) (+ x h) (- y i) (- x h) (- y i) r g b)
           (overlay-quad! out (- x h) (+ y i) (+ x h) (+ y i) (+ x h) (+ y h) (- x h) (+ y h) r g b)
           (overlay-quad! out (- x h) (- y i) (- x i) (- y i) (- x i) (+ y i) (- x h) (+ y i) r g b)
           (overlay-quad! out (+ x i) (- y i) (+ x h) (- y i) (+ x h) (+ y i) (+ x i) (+ y i) r g b)))
        (:triangle
         (overlay-triangle! out x (- y h) (+ x h) (+ y h) (- x h) (+ y h) r g b))
        (:plus
         (let ((w (* 0.5f0 line)))
           (overlay-quad! out (- x h) (- y w) (+ x h) (- y w) (+ x h) (+ y w) (- x h) (+ y w) r g b)
           (overlay-quad! out (- x w) (- y h) (+ x w) (- y h) (+ x w) (- y w) (- x w) (- y w) r g b)
           (overlay-quad! out (- x w) (+ y w) (+ x w) (+ y w) (+ x w) (+ y h) (- x w) (+ y h) r g b)))
        (:dot
         (let ((step (/ (* 2 (coerce pi 'single-float)) 12)))
           (dotimes (k 12)
             (overlay-triangle! out x y
                                (+ x (* h (cos (* k step)))) (+ y (* h (sin (* k step))))
                                (+ x (* h (cos (* (1+ k) step))))
                                (+ y (* h (sin (* (1+ k) step))))
                                r g b))))))
    out))

(defun label-at! (out string x y colour &optional (dot *label-dot-px*))
  "STRING, upcased, in dot-matrix glyphs of DOT px dots, 6 dots a cell: the
first glyph's left edge at pixel X, the rows centred on Y, in the sRGB
integer COLOUR. Conses nothing for real X, Y that are not boxed doubles."
  (declare (optimize speed) (type overlay-buffer out) (type string string)
           (type real x y dot) (type (unsigned-byte 24) colour))
  (let* ((dot (coerce dot 'single-float))
         (left (coerce x 'single-float))
         (top (- (coerce y 'single-float) (* 3.5f0 dot))))
    (declare (type single-float dot left top))
    (with-linear-colour (r g b) colour
      (dotimes (column (length string))
        (let ((x0 (+ left (* dot 6 column))))
          (loop for row in (glyph (char string column))
                for j of-type fixnum from 0
                do (dotimes (i 5)
                     (when (char= (char (the simple-string row) i) #\#)
                       (let ((px (+ x0 (* dot i))) (py (+ top (* dot j))))
                         (overlay-quad! out px py (+ px dot) py (+ px dot) (+ py dot)
                                        px (+ py dot) r g b))))))))
    out))

(defparameter *marker-label-order* '(:earth :sun :moon :ship :target :impact :burn :closest-approach :aim :periapsis
    :apoapsis :ascending :descending :candidate)
  "Labels are placed in this order, each right of its marker, else left of
it, else dropped where it would overlap one placed before; decision 103.")

(defun label-width (string &optional (dot *label-dot-px*))
  "STRING's width in px: 6 dots a cell, the last cell's gap not counted."
  (if (zerop (length string)) 0 (- (* 6 dot (length string)) dot)))

(defun place-map-labels! (markers)
  "Set each marker's LABEL-SIDE and LABEL-X: in *marker-label-order*, right
of the marker (*marker-label-gap-px* from its centre), else as far left,
else nil when both overlap a label placed before. Conses nothing."
  (declare (optimize speed) (type map-markers markers))
  (let ((items (map-markers-items markers))
        (count (map-markers-count markers))
        (height (float (* 7 (the fixnum *label-dot-px*)) 1d0))
        (gap (float *marker-label-gap-px* 1d0)))
    (declare (type double-float height gap))
    (dotimes (i count)
      (setf (map-marker-label-side (svref items i)) nil))
    (flet ((free-p (x0 x1 y)
             ;; no placed label's box meets [x0, x1] × [y − h/2, y + h/2]
             (declare (type double-float x0 x1 y))
             (dotimes (j count t)
               (let ((other (svref items j)))
                 (when (and (map-marker-label-side other)
                            (< x0 (+ (map-marker-label-x other)
                                     (label-width (map-marker-label other))))
                            (< (map-marker-label-x other) x1)
                            (< (abs (- y (map-marker-y other))) height))
                   (return nil))))))
      (declare (inline free-p))
      (dolist (key *marker-label-order*)
        (dotimes (i count)
          (let ((marker (svref items i)))
            (when (and (eq (map-marker-key marker) key) (map-marker-label marker))
              (let* ((x (map-marker-x marker)) (y (map-marker-y marker))
                     (width (float (label-width (map-marker-label marker)) 1d0))
                     (right (+ x gap)) (left (- x gap width)))
                (cond ((free-p right (+ right width) y)
                       (setf (map-marker-label-side marker) :right (map-marker-label-x marker) right))
                      ((free-p left (+ left width) y)
                       (setf (map-marker-label-side marker) :left (map-marker-label-x marker) left)))))))))
    markers))

(defparameter *label-backing-colour* #x0A0C0C "Behind map labels, near-black, sRGB; decision 104.")
(defparameter *label-backing-px* 2 "The backing's margin around a label's cells, px; decision 104.")
(declaim (type (unsigned-byte 24) *label-backing-colour*) (type fixnum *label-backing-px*))

(defun label-backing! (out string x y &optional (dot *label-dot-px*))
  "An opaque *label-backing-colour* box behind label-at!'s STRING at X, Y:
its cells grown by *label-backing-px* on every side. Conses nothing."
  (declare (optimize speed) (type overlay-buffer out) (type string string)
           (type single-float x y) (type fixnum dot))
  (let* ((pad (coerce (the fixnum *label-backing-px*) 'single-float))
         (x0 (- x pad)) (x1 (+ x (coerce (the fixnum (label-width string dot)) 'single-float) pad))
         (h (+ (* 3.5f0 dot) pad)) (y0 (- y h)) (y1 (+ y h)))
    (with-linear-colour (r g b) *label-backing-colour*
      (overlay-quad! out x0 y0 x1 y0 x1 y1 x0 y1 r g b))
    out))

(defun map-marker-vertices! (out markers)
  "MARKERS (ribbons.lisp's map-markers) into OUT: the backings of the
labels place-map-labels! keeps, then the shapes at their pixels, the ship's
last, then the labels in *marker-label-colour*, rows centred on the marker
(decision 104: Map labels on a near-black backing). Conses nothing; OUT."
  (declare (type overlay-buffer out) (type map-markers markers))
  (place-map-labels! markers)
  (dotimes (i (map-markers-count markers))
    (let ((marker (svref (map-markers-items markers) i)))
      (when (map-marker-label-side marker)
        (label-backing! out (map-marker-label marker)
                        (coerce (map-marker-label-x marker) 'single-float)
                        (coerce (map-marker-y marker) 'single-float)))))
  ;; the ship's ring, the first, on top
  (loop for i from (1- (map-markers-count markers)) downto 0
        for marker = (svref (map-markers-items markers) i)
        do (marker-vertices! out (map-marker-kind marker)
                             (coerce (map-marker-x marker) 'single-float)
                             (coerce (map-marker-y marker) 'single-float)
                             (map-marker-colour marker)
                             (map-marker-size marker)))
  (dotimes (i (map-markers-count markers) out)
    (let ((marker (svref (map-markers-items markers) i)))
      (when (map-marker-label-side marker)
        (label-at! out (map-marker-label marker)
                   (coerce (map-marker-label-x marker) 'single-float)
                   (coerce (map-marker-y marker) 'single-float)
                   *marker-label-colour*)))))

;;; The minimal readout (decision 85: Minimal readout: layout, formats,
;;; colours and alarms): lines of runs, a pure function of the render
;;; state, the clock and the real time.

(defparameter *readout-label-colour* #x8A8A80 "Row labels, grey, sRGB; decision 85.")
(defparameter *readout-value-colour* #xE8E4D8 "Values, paper white, sRGB; decision 85.")
(defparameter *readout-caution-colour* #xFFB000 "Cautions, amber, with \"!\"; decision 85.")
(defparameter *readout-danger-colour* #xFF3030 "Danger, red, inverted, flashing; decision 85.")
(defparameter *readout-flash-period* 0.5d0 "s of real time, on in the first half; decision 85.")
(defparameter *readout-messages* 3 "Clock messages shown at most, newest last; decision 85.")
(defparameter *readout-message-chars* 40 "A message longer than this is cut, \"…\" last; decision 85.")
(defparameter *readout-act-fraction* 0.9d0
  "The actual rate shows below this fraction of the nominal level; decision 85.")

(defun readout-run (text colour &optional (style :plain) flash)
  "A run of the readout: TEXT upcased, sRGB COLOUR, STYLE :plain or
:inverted (a COLOUR block, the text in *label-backing-colour*), FLASH nil
(steady) or the phase now, :on or :off."
  (list (string-upcase text) colour style flash))

(defun run-text (run) (first run))
(defun run-colour (run) (second run))
(defun run-style (run) (third run))
(defun run-flash (run) (fourth run))

(defun line-text (line)
  "The runs' texts of LINE, joined."
  (apply #'concatenate 'string (mapcar #'run-text line)))

(defun format-fixed (x decimals)
  "X with DECIMALS decimals, rounded half away from zero."
  (let* ((scale (expt 10 decimals))
         (n (round (* (abs x) scale)))
         (sign (if (and (minusp x) (plusp n)) "-" "")))
    (if (zerop decimals)
        (format nil "~A~D" sign n)
        (multiple-value-bind (whole fraction) (floor n scale)
          (format nil "~A~D.~v,'0D" sign whole decimals fraction)))))

(defun format-significant (x digits)
  "X to DIGITS significant digits, no exponent; zero or less shows as zero."
  (if (<= x 0)
      (format-fixed 0d0 (1- digits))
      (let ((decimals (- digits 1 (floor (log x 10)))))
        (if (<= decimals 0)
            (let ((scale (expt 10 (- decimals))))
              (format nil "~D" (* scale (round x scale))))
            (format-fixed x decimals)))))

(defun format-speed (speed)
  "m/s with one decimal below 1,000 m/s, else km/s with three; decision 85."
  (if (< speed 1000d0)
      (format nil "~A M/S" (format-fixed speed 1))
      (format nil "~A KM/S" (format-fixed (m/s->km/s speed) 3))))

(defun format-km (metres) (format nil "~A KM" (format-fixed (m->km metres) 1)))

(defparameter *hold-names*
  '((:prograde . "PROGRADE") (:retrograde . "RETROGRADE") (:normal . "NORMAL")
    (:anti-normal . "ANTI-NORMAL") (:radial-out . "RADIAL OUT") (:radial-in . "RADIAL IN")
    (:inertial . "INERTIAL") (:target . "TARGET") (:anti-target . "ANTI-TARGET")
    (:relative-prograde . "REL PROGRADE") (:relative-retrograde . "REL RETROGRADE")))

(defun mode-text (controls &optional autopilot)
  "The MODE row's value; AUTOPILOT (a plan engaged) leads with \"AUTOPILOT\"."
  (format nil "~:[~;AUTOPILOT ~]~A~:[~; FINE~]"
          autopilot
          (ecase (controls-attitude-mode controls)
            (:direct "DIRECT")
            (:rate-damping "RATE DAMP")
            (:hold (format nil "HOLD ~A ~:[SLEWING~;SETTLED~]"
                           (cdr (assoc (controls-hold controls) *hold-names*))
                           (controls-settled controls))))
          (controls-fine controls)))

(defun cut-text (text &optional (chars *readout-message-chars*))
  "TEXT, or its first CHARS − 1 characters and \"…\"."
  (if (> (length text) chars)
      (concatenate 'string (subseq text 0 (1- chars)) "…")
      text))

(defun readout-lines (render-state clock real-time)
  "The milestone-2 readout (decision 85): a list of lines, each a list of
runs (see readout-run). Rows ALT, SPD, AP, PE, THR, PROP, MODE, WARP, UTC
from RENDER-STATE at its t_r and CLOCK; the alarms, the impact's from CLOCK's
prediction (decision 84, decision 75), flashing by REAL-TIME, the game's
summed seconds; then CLOCK's live messages."
  (let* ((r (render-state-r render-state)) (v (render-state-v render-state))
         (time (render-state-t render-state))
         (controls (render-state-controls render-state))
         (m-p (render-state-m-p render-state))
         (earth (body :earth))
         (orbit (state->orbit r v +mu-earth+ time))
         (apoapsis (orbit-apoapsis-radius orbit))
         (periapsis (orbit-periapsis-radius orbit))
         (throttle (controls-throttle controls))
         (level (clock-level clock))
         (rate (clock-actual-rate clock))
         (paused (clock-paused clock))
         (flash (if (< (mod real-time *readout-flash-period*) (* 0.5d0 *readout-flash-period*))
                    :on :off)))
    (flet ((row (label &rest values)
             ;; values after the first lead with a space
             (cons (readout-run (format nil "~A " label) *readout-label-colour*)
                   (loop for value in (remove nil values)
                         for space = "" then " "
                         collect (if (stringp value)
                                     (readout-run (concatenate 'string space value)
                                                  *readout-value-colour*)
                                     (funcall value space)))))
           (caution (text &optional (space ""))
             (readout-run (format nil "~A! ~A" space text) *readout-caution-colour*)))
      (append
       (list (row "ALT" (format-km (- (vec3-norm r) +radius-earth+)))
             (row "SPD" (format-speed (vec3-norm v)))
             (row "AP" (if apoapsis (format-km (- apoapsis +radius-earth+)) "ESCAPE"))
             (row "PE" (format-km (- periapsis +radius-earth+)))
             (row "THR" (format nil "~D% ~:[OFF~;ON~]" (round (* 100 throttle))
                                (and (> throttle 0d0) (> m-p 0d0))))
             (row "PROP" (format nil "~D KG ~D%" (round m-p) (round (* 100 m-p) *propellant-full*)))
             (row "MODE" (mode-text controls (render-state-autopilot render-state)))
             (row "WARP" (format nil "×~D" level)
                  (and (not paused) (null (render-state-loss render-state))
                       rate (< rate (* *readout-act-fraction* level))
                       (lambda (space)
                         (caution (format nil "ACT ×~A" (format-significant rate 3)) space)))
                  (and paused "PAUSED"))
             (row "UTC" (format-utc time)))
       (when (< periapsis (body-crash-radius earth))
         (list (list (caution (format nil "PE BELOW ~D KM"
                                      (round (- (body-crash-radius earth) (body-radius earth))
                                             1000))))))
       (multiple-value-bind (impact loss) (prediction-event (clock-prediction clock) :impact)
         (when impact
           (list (list (readout-run (format nil "~A IMPACT IN ~A"
                                            (if (eq (loss-body loss) :moon) "MOON" "EARTH")
                                            (format-duration (- impact time)))
                                    *readout-danger-colour* :inverted flash)))))
       (mapcar (lambda (text) (list (caution (cut-text text))))
               (let ((texts (clock-messages clock))
                     (newest (first (clock-log clock))))
                 ;; the message line shows the newest entry (decision 129)
                 (when (and newest texts (string= (third newest) (car (last texts))))
                   (setf texts (butlast texts)))
                 (last texts *readout-messages*)))))))

(defparameter *readout-cell-width-px* 12 "A character's cell, 6 dots of 2 px; decision 85.")
(defparameter *readout-cell-height-px* 22 "A line's height; decision 85.")
(defparameter *readout-margin-px* 16 "The box from the frame's top left edges; decision 85.")
(defparameter *readout-padding-px* 8 "The text inside the box's edges.")
(defparameter *readout-box-colour* #x0A0C0C "The opaque box, near-black, sRGB; decision 85.")
(defparameter *readout-invert-px* 2 "An inverted run's block around its cells.")

(defun run-chars (line)
  (loop for run in line sum (length (run-text run))))

(defun readout-box-size (lines &optional min-chars max-chars)
  "The width and height in px of the box READOUT-VERTICES! draws for LINES,
the widest line counted as at least MIN-CHARS and at most MAX-CHARS cells."
  (let ((chars (loop for line in lines maximize (run-chars line))))
    (when min-chars (setf chars (max chars min-chars)))
    (when max-chars (setf chars (min chars max-chars)))
    (values (+ (* 2 *readout-padding-px*)
               (max 0 (- (* chars *readout-cell-width-px*) *label-dot-px*)))
            (+ (* 2 *readout-padding-px*) (* (length lines) *readout-cell-height-px*)))))

(defun readout-vertices! (out lines x y &optional min-chars max-chars)
  "LINES (readout-lines' runs) into OUT: the opaque *readout-box-colour* box
with its top left at pixel X, Y, sized to the lines, then each run's text
(decision 85); an :inverted run as its colour's block with the text in the
box's colour; a flashing run in its :off phase not at all. MIN-CHARS and
MAX-CHARS bound the box's width in cells, as readout-box-size's. OUT."
  (multiple-value-bind (width height) (readout-box-size lines min-chars max-chars)
    (let ((x0 (float x 1f0)) (y0 (float y 1f0))
          (x1 (float (+ x width) 1f0)) (y1 (float (+ y height) 1f0)))
      (with-linear-colour (r g b) *readout-box-colour*
        (overlay-quad! out x0 y0 x1 y0 x1 y1 x0 y1 r g b))))
  (loop for line in lines
        for centre from (+ y *readout-padding-px* (/ *readout-cell-height-px* 2))
          by *readout-cell-height-px*
        do (let ((left (+ x *readout-padding-px*)))
             (dolist (run line)
               (let ((text (run-text run)))
                 (unless (eq (run-flash run) :off)
                   (if (eq (run-style run) :inverted)
                       (let* ((pad *readout-invert-px*)
                              (x0 (float (- left pad) 1f0))
                              (x1 (float (+ left (label-width text) pad) 1f0))
                              (h (+ (* 3.5f0 *label-dot-px*) pad))
                              (y0 (float (- centre h) 1f0)) (y1 (float (+ centre h) 1f0)))
                         (with-linear-colour (r g b) (run-colour run)
                           (overlay-quad! out x0 y0 x1 y0 x1 y1 x0 y1 r g b))
                         (label-at! out text left centre *readout-box-colour*))
                       (label-at! out text left centre (run-colour run))))
                 (incf left (* (length text) *readout-cell-width-px*))))))
  out)

;;; The message line (decision 129: Overlay of the outside and map views in
;;; milestone 3): the newest SHIP log entry as one line in the readout's format.

(defparameter *message-line-chars* 60 "A text longer than this is cut, \"…\" last; decision 129.")
(defparameter *message-line-fresh* 30d0
  "s of real time after its arrival an :info or :caution entry turns grey; decision 129.")

(defun autopilot-message-severity (text)
  (cond ((or (eql 0 (search "AUTOPILOT ABORT" text)) (string= text "AUTOPILOT: GOAL MISSED"))
         :danger)
        ((eql 0 (search "PLAN STALE" text)) :caution)
        (t :info)))

(defun grapple-message-severity (text)
  "LATCHED and BERTHED are news, every other grapple notice a refusal."
  (if (or (eql 0 (search "LATCHED " text)) (eql 0 (search "BERTHED " text))) :info :caution))

(defun transfer-message-severity (text)
  (if (string= text "TANKS FULL") :info :caution))

(defparameter *message-severity*
  '((:refused . :caution) (:lowered . :caution) (:engine-locked . :caution) (:rcs-off . :caution)
    (:target . :caution) (:autopilot-refused . :caution)
    (:autopilot . autopilot-message-severity)
    (:grapple . grapple-message-severity)
    (:transfer . transfer-message-severity))
  "Notice key to :info, :caution, :danger, or to a function of the text giving one;
a key not here is :info (:removal, :delivery, :station); decision 129.")

(defun message-severity (key text)
  (let ((value (cdr (assoc key *message-severity*))))
    (cond ((null value) :info)
          ((keywordp value) value)
          (t (funcall value text)))))

(defstruct (message-line (:constructor make-message-line ()))
  "The render side's arrival tracking: the newest log entry seen (eq) and the
real time it was first seen."
  (entry nil)
  (seen 0d0 :type double-float))

(defun message-line-reset! (message-line)
  (setf (message-line-entry message-line) nil
        (message-line-seen message-line) 0d0)
  message-line)

(defun message-line-update! (message-line clock real-time &optional (age 0d0))
  "Once per frame: a newer newest entry of CLOCK's log is seen at REAL-TIME,
AGE seconds ago (captures). Conses nothing."
  (declare (type double-float real-time age))
  (let ((newest (first (clock-log clock))))
    (unless (eq newest (message-line-entry message-line))
      (setf (message-line-entry message-line) newest
            (message-line-seen message-line) (- real-time age))))
  message-line)

(defun message-line-lines (clock message-line real-time)
  "The message line (decision 129): nil without a log entry, else a list of one
line: the entry's game UTC time grey, its text by its severity, cut at
*message-line-chars*; :caution amber with \"! \", :danger red inverted, flashing
at 2 Hz while unread (below clock-log-read), steady after; :info and :caution
grey from *message-line-fresh* s after MESSAGE-LINE saw it."
  (let ((entry (message-line-entry message-line)))
    (when entry
      (destructuring-bind (game-t key text) entry
        (let* ((severity (message-severity key text))
               (text (cut-text text *message-line-chars*))
               (stamp (readout-run (format nil "~A " (subseq (format-utc game-t) 11))
                                   *readout-label-colour*)))
          (list
           (list stamp
                 (ecase severity
                   (:danger
                    (readout-run text *readout-danger-colour* :inverted
                                 (and (member entry (clock-log-unread clock) :test #'eq)
                                      (if (< (mod real-time *readout-flash-period*)
                                             (* 0.5d0 *readout-flash-period*))
                                          :on :off))))
                   ((:info :caution)
                    (let ((prefix (if (eq severity :caution) "! " "")))
                      (readout-run (concatenate 'string prefix text)
                                   (cond ((>= (- real-time (message-line-seen message-line))
                                              *message-line-fresh*)
                                          *readout-label-colour*)
                                         ((eq severity :caution) *readout-caution-colour*)
                                         (t *readout-value-colour*))))))))))))) 

(defun message-line-origin (lines extent)
  "The top left pixel of the box for LINES in a frame of EXTENT (width height):
centred horizontally, its bottom *readout-margin-px* above the frame's bottom."
  (destructuring-bind (width height) extent
    (multiple-value-bind (box-width box-height) (readout-box-size lines)
      (values (max 0 (floor (- width box-width) 2))
              (max 0 (- height *readout-margin-px* box-height))))))

;;; The autopilot panel (decision 129): the preset menu, the plan and its burns,
;;; the engaged state, as lines in the readout's format. The plan's four lines
;;; are shared with the cockpit's AUTOPILOT screen (decision 127).

(defparameter *autopilot-panel-chars* 30 "The panel's width in cells, a longer line is cut; decision 129.")
(defparameter *autopilot-panel-menu-rows* 8 "Menu rows shown at most; decision 129.")
(defparameter *autopilot-panel-burns* 6 "Burn rows shown at most; decision 129.")
(defparameter *readout-ok-colour* #x40D040 "Engaged, in tolerance, green, sRGB; decision 127.")

(defun autopilot-panel-origin (lines extent)
  "The top left pixel of the panel's box for LINES in a frame of EXTENT
(width height): *readout-margin-px* from the top and right edges, the box
*autopilot-panel-chars* cells wide (decision 129)."
  (let ((box-width (readout-box-size lines *autopilot-panel-chars* *autopilot-panel-chars*)))
    (values (max 0 (- (first extent) *readout-margin-px* box-width))
            *readout-margin-px*)))

(defun format-clock-time (seconds)
  "SECONDS (to a future event) as T-mm:ss, T-h:mm:ss above an hour."
  (let ((total (max 0 (round seconds))))
    (multiple-value-bind (minutes second) (floor total 60)
      (multiple-value-bind (hours minute) (floor minutes 60)
        (if (plusp hours)
            (format nil "T-~D:~2,'0D:~2,'0D" hours minute second)
            (format nil "T-~2,'0D:~2,'0D" minute second))))))

(defun format-arrival (seconds)
  "SECONDS ahead as mm:ss, h:mm:ss above an hour."
  (subseq (format-clock-time seconds) 2))

(defun menu-window-start (count selected rows)
  "The first of the ROWS menu rows shown of COUNT, keeping SELECTED in sight."
  (max 0 (min (- count rows) (- (or selected 0) (floor rows 2)))))

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

(defun autopilot-abort-unread-p (clock model)
  "Whether the engaged plan ended in an abort (the model :off with its text)
whose log entry Y has not marked read."
  (and model (eq (autopilot-model-state model) :off)
       (let ((text (autopilot-model-text model)))
         (and text (search "ABORT" text)
              (let ((mark (clock-log-read clock)))
                (loop for entry in (clock-log clock)
                      until (eq entry mark)
                      thereis (and (eq (second entry) :autopilot) (search "ABORT" (third entry)) t)))))
       t))

(defun plain-runs (&rest parts)
  "A line of plain runs from PARTS: a colour (sRGB integer) then its text, as written."
  (loop for (colour text) on parts by #'cddr collect (list text colour :plain nil)))

(defun autopilot-plan-lines (plan sim)
  "The plan's four lines (decision 127) from autopilot-plan-summary: burn n/m
and when, its Δv and duration, the total Δv and propellant, what is left and
the arrival; nil when the plan's cursor has no burn left. Mixed case: the
screen shows it as it is, the panel upcases."
  (let ((burns (burn-plan-burns plan))
        (label *readout-label-colour*) (value *readout-value-colour*))
    (unless (and (eq (sim-plan sim) plan) (>= (sim-plan-index sim) (length burns)))
      (let ((summary (autopilot-plan-summary plan sim)))
        (list
         (plain-runs label "BURN " value (format nil "~D/~D" (1+ (getf summary :index)) (getf summary :burns))
                     value " " value (format-clock-time (getf summary :next-in)))
         (plain-runs label "DV " value (format-fixed (getf summary :next-dv) 1)
                     label " m/s  " value (format nil "~D" (round (getf summary :next-duration))) label " s")
         (plain-runs label "TOTAL " value (format nil "~D" (round (getf summary :total-dv)))
                     label " m/s " value (format nil "~D" (round (getf summary :propellant))) label " kg")
         (plain-runs label "LEFT " value (format-fixed (/ (getf summary :left) 1000d0) 1)
                     label " t  ARR " value (format-arrival (- (getf summary :arrival) (sim-t sim)))))))))

(defun panel-line (&rest parts)
  "A readout line from PARTS, colour then text, the texts upcased."
  (loop for (colour text) on parts by #'cddr collect (readout-run text colour)))

(defun upcase-line (line)
  (mapcar (lambda (run) (list (string-upcase (run-text run)) (run-colour run) (run-style run) (run-flash run)))
          line))

(defun burn-dv-text (dv)
  "A burn's Δv as the map's burn markers show it: whole from 100, one decimal from 1, two below."
  (format-fixed dv (cond ((< dv 0.995d0) 2) ((< dv 99.95d0) 1) (t 0))))

(defun burn-duration-text (seconds)
  (if (< seconds 100d0)
      (format nil "~D S" (round seconds))
      (multiple-value-bind (minutes second) (floor (round seconds) 60)
        (format nil "~D:~2,'0D" minutes second))))

(defun flash-phase (real-time)
  (if (< (mod real-time *readout-flash-period*) (* 0.5d0 *readout-flash-period*)) :on :off))

(defun panel-burn-rows (plan sim mode)
  "Up to *autopilot-panel-burns* rows \"<n> <LABEL> <dv> M/S <duration>\" of PLAN:
done burns grey, the current one \">\"-marked and paper white, later ones white;
MODE :engaged marks the sim's cursor, :kept greys them all, :shown none done."
  (let* ((burns (burn-plan-burns plan))
         (count (length burns))
         (index (case mode (:engaged (sim-plan-index sim)) (:kept count) (t -1)))
         (start (menu-window-start count (max 0 index) *autopilot-panel-burns*)))
    (loop for i from start below (min count (+ start *autopilot-panel-burns*))
          for burn = (svref burns i)
          collect (panel-line (cond ((< i index) *readout-label-colour*) (t *readout-value-colour*))
                              (format nil "~A~D ~A ~A M/S ~A" (if (= i index) "> " "  ") (1+ i)
                                      (symbol-name (planned-burn-label burn))
                                      (burn-dv-text (planned-burn-dv burn))
                                      (burn-duration-text (- (planned-burn-cut-t burn)
                                                             (planned-burn-start burn))))))))

(defun panel-phase-line (sim)
  "A rendezvous plan's :terminal or :station phase in green, else nil."
  (let ((plan (sim-plan sim)))
    (when plan
      (case (sim-plan-phase sim)
        (:terminal (panel-line *readout-ok-colour* "TERMINAL"))
        (:station (let ((part (burn-plan-rendezvous plan)))
                    (panel-line *readout-ok-colour*
                                (format nil "ON STATION ~D M"
                                        (round (if part (rendezvous-part-range part) 0d0))))))))))

(defun panel-menu-rows (model sim)
  "The menu's window of at most *autopilot-panel-menu-rows* rows keeping the
selection in sight, the selection paper white inverted over the box's width."
  (multiple-value-bind (texts selected) (autopilot-menu-lines model sim)
    (let* ((count (length texts))
           (start (menu-window-start count selected *autopilot-panel-menu-rows*)))
      (loop for i from start below (min count (+ start *autopilot-panel-menu-rows*))
            for text = (subseq (nth i texts) 2)
            collect (if (eql i selected)
                        (list (readout-run (format nil "~VA" *autopilot-panel-chars* text)
                                           *readout-value-colour* :inverted))
                        (panel-line *readout-value-colour* text))))))

(defun panel-abort-text (text)
  "The abort's reason as \"ABORT: <REASON>\"."
  (let ((reason (cond ((eql 0 (search "AUTOPILOT ABORT: " text)) (subseq text 17))
                      ((eql 0 (search "AUTOPILOT: " text)) (subseq text 11))
                      (t text))))
    (if (search "ABORT" reason) reason (format nil "ABORT: ~A" reason))))

(defun autopilot-panel-lines (model sim render-state clock real-time)
  "The autopilot panel's lines (decision 129): nil while MODEL is :closed, else
\"AUTOPILOT\" and the state word, then the state's rows and its hint, each cut
at *autopilot-panel-chars*. RENDER-STATE is unused for now."
  (declare (ignore render-state))
  (let ((state (autopilot-model-state model)))
    (unless (eq state :closed)
      (let* ((grey *readout-label-colour*) (white *readout-value-colour*)
             (green *readout-ok-colour*))
        (multiple-value-bind (plan shot mode) (autopilot-map-plan model)
          (declare (ignore shot))
          (mapcar
           (lambda (line) (screen-fit-line line *autopilot-panel-chars*))
           (append
            (list (if (eq state :engaged)
                      (panel-line grey "AUTOPILOT " green "ENGAGED")
                      (panel-line grey "AUTOPILOT "
                                  white (ecase state
                                          (:menu "MENU") (:refused "REFUSED") (:planning "PLANNING…")
                                          (:shown "PLAN") (:off "OFF")))))
            (ecase state
              ((:menu :refused)
               (append (panel-menu-rows model sim)
                       (list (if (eq state :refused)
                                 (panel-line *readout-caution-colour*
                                             (format nil "! ~A" (or (autopilot-model-text model) "REFUSED")))
                                 (panel-line grey "PLAN [RET]  CLOSE [O]")))))
              (:planning '())
              ((:shown :engaged)
               (append (when plan (mapcar #'upcase-line (autopilot-plan-lines plan sim)))
                       (let ((phase (panel-phase-line sim))) (when phase (list phase)))
                       (when plan (panel-burn-rows plan sim mode))
                       (list (if (eq state :engaged)
                                 (panel-line green "ENGAGED  OFF [BKSP]")
                                 (panel-line grey "ENGAGE [RET]  DROP [BKSP]")))))
              (:off
               (append (when plan (panel-burn-rows plan sim :kept))
                       (list (if (autopilot-abort-unread-p clock model)
                                 (list (readout-run (panel-abort-text (autopilot-model-text model))
                                                    *readout-danger-colour* :inverted
                                                    (flash-phase real-time)))
                                 (panel-line grey "OFF  RE-PLAN [RET]  CLOSE [O]"))))))))))))) 

;;; The LOST panel (decision 86: Death view, LOST panel and restart in
;;; milestone 2): lines in the readout's format, a pure function of the loss.

(defparameter *lost-panel-top* 0.6d0 "The panel's top, a fraction of the height; decision 86.")

(defun lost-panel-lines (loss &optional checkpoint)
  "The LOST panel of LOSS (decision 86): the title red inverted and steady,
the body and the impact's UTC, its speed relative to the body as the
readout's SPD, latitude and longitude; for a contact (decision 113) the
object's name and the UTC, the normal speed to 0.01 m/s; a blank line, Return (to the
latest checkpoint when CHECKPOINT, decision 112) and N."
  (flet ((label (text) (readout-run text *readout-label-colour*))
         (value (text) (readout-run text *readout-value-colour*))
         (degrees (angle) (format-fixed (abs (rad->deg angle)) 2)))
    (append
     (list (list (readout-run (format nil "LOST: ~A" (symbol-name (loss-cause loss)))
                              *readout-danger-colour* :inverted)))
     (if (eq (loss-cause loss) :contact)
         (list (list (value (format nil "~A  ~A"
                                    (object-def-name (object-definition (loss-object loss)))
                                    (format-utc (loss-time loss)))))
               ;; Two decimals: a death is just above 1.0 m/s.
               (list (label "SPEED ")
                     (value (if (< (loss-speed loss) 100d0)
                                (format nil "~A M/S" (format-fixed (loss-speed loss) 2))
                                (format-speed (loss-speed loss))))))
         (list (list (value (format nil "~:[EARTH~;MOON~]  ~A" (eq (loss-body loss) :moon)
                                    (format-utc (loss-time loss)))))
               (list (label "SPEED ") (value (format-speed (loss-speed loss))))
               (list (label "LAT ")
                     (value (format nil "~A° ~:[N~;S~]" (degrees (loss-latitude loss))
                                    (minusp (loss-latitude loss))))
                     (label "  LON ")
                     (value (format nil "~A° ~:[E~;W~]" (degrees (loss-longitude loss))
                                    (minusp (loss-longitude loss)))))))
     (list '()
           (list (value "RETURN")
                 (label (if checkpoint "  RESTART FROM CHECKPOINT" "  RESTART FROM START")))
           (list (value "N") (label "       NEW GAME"))))))

(defun lost-panel-origin (lines extent)
  "The top left pixel of the panel's box for LINES in a frame of EXTENT
(width height): centred horizontally, its top at *lost-panel-top*."
  (destructuring-bind (width height) extent
    (values (max 0 (floor (- width (readout-box-size lines)) 2))
            (floor (* *lost-panel-top* height)))))

(defun quad (anchor-x anchor-y left top size color)
  "Two triangles of overlay vertices: a SIZE-pixel square at pixel offset
LEFT, TOP from the clip-space anchor."
  (loop for (dx dy) in '((0 0) (1 0) (1 1) (0 0) (1 1) (0 1))
        append (list* anchor-x anchor-y (+ left (* dx size)) (+ top (* dy size)) color)))

(defun overlay-floats (out)
  "OUT's vertices as a fresh single-float vector."
  (subseq (overlay-buffer-floats out) 0 (* +overlay-vertex-floats+ (overlay-buffer-count out))))

(defun overlay-vertex-buffers ()
  `((:array-stride ,(* 4 +overlay-vertex-floats+)
     :attributes ((:shader-location 0 :offset 0 :format :float32x2)
                  (:shader-location 1 :offset 8 :format :float32x2)
                  (:shader-location 2 :offset 16 :format :float32x3)))))

(defun overlay-pipeline (renderer layout)
  "The :overlay live pipeline: screen-space quads on the target after the
tone map, no depth (decision 76)."
  (live-pipeline renderer :overlay layout (overlay-vertex-buffers)))
