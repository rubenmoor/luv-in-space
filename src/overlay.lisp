(in-package #:luv-in-space)

;;; The screen-space overlay: dot-matrix labels (decision 7: Placeholder
;;; labels: dot-matrix overlay quads) and their pipeline.

(defparameter *label* "PLACEHOLDER(80)"
  "Top left until the parts that replace the placeholders land.")

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
    (#\9 ".###." "#...#" "#...#" ".####" "....#" "....#" ".###."))
  "5x7 dot-matrix glyphs: A-Z, 0-9, space, - . : ( ); a character missing
here draws blank (decision 85: the tests require every one used).")

(defparameter *label-colour* #xFFBF33 "The placeholder label, amber, sRGB.")
(defparameter *label-dot-px* 2 "Dot pitch of labels, cap 14 px; decision 85.")
(defparameter *marker-line-px* 2 "Ring and cross line width; decision 84.")
(defparameter *marker-ring-px* 12 "The ship's ring; decision 84.")
(defparameter *marker-diamond-px* 9 "Apsis diamonds; decision 84.")
(defparameter *marker-cross-px* 12 "The impact X; decision 84.")
(defparameter *marker-dot-px* 7 "Bodies below 6 px; decision 84.")
(defparameter *overlay-max-vertices* 24576 "Capacity of an overlay buffer.")

(defun glyph (char)
  "CHAR's seven rows, upcased, or nil."
  (rest (assoc (char-upcase char) *glyphs*)))

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
    (:cross *marker-cross-px*) (:dot *marker-dot-px*)))

(defun marker-vertices! (out kind x y colour &optional size)
  "Triangles of marker KIND centred on pixel X, Y within a SIZE box (default
the kind's, decision 84: Map ribbons and markers: osculating orbit, adaptive
segments, overlay markers and visibility) in the sRGB integer COLOUR:
:ring a 16-segment outline, :diamond, :cross two bars, :dot a 12-gon. Conses
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

(defparameter *marker-label-order* '(:earth :sun :moon :ship :impact :periapsis :apoapsis)
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

(defun map-marker-vertices! (out markers)
  "MARKERS (ribbons.lisp's map-markers) into OUT: the shapes at their
pixels, the ship's last, then the labels place-map-labels! keeps, in *marker-label-colour*, rows
centred on the marker. Conses nothing; OUT."
  (declare (type overlay-buffer out) (type map-markers markers))
  (place-map-labels! markers)
  ;; the ship's ring, the first, on top
  (loop for i from (1- (map-markers-count markers)) downto 0
        for marker = (svref (map-markers-items markers) i)
        do (marker-vertices! out (map-marker-kind marker)
                             (coerce (map-marker-x marker) 'single-float)
                             (coerce (map-marker-y marker) 'single-float)
                             (map-marker-colour marker)))
  (dotimes (i (map-markers-count markers) out)
    (let ((marker (svref (map-markers-items markers) i)))
      (when (map-marker-label-side marker)
        (label-at! out (map-marker-label marker)
                   (coerce (map-marker-label-x marker) 'single-float)
                   (coerce (map-marker-y marker) 'single-float)
                   *marker-label-colour*)))))

(defun quad (anchor-x anchor-y left top size color)
  "Two triangles of overlay vertices: a SIZE-pixel square at pixel offset
LEFT, TOP from the clip-space anchor."
  (loop for (dx dy) in '((0 0) (1 0) (1 1) (0 0) (1 1) (0 1))
        append (list* anchor-x anchor-y (+ left (* dx size)) (+ top (* dy size)) color)))

(defun overlay-floats (out)
  "OUT's vertices as a fresh single-float vector."
  (subseq (overlay-buffer-floats out) 0 (* +overlay-vertex-floats+ (overlay-buffer-count out))))

(defun label-vertices (string &key (dot 3) (margin 16) (colour *label-colour*))
  "Overlay vertices drawing STRING in dot-matrix glyphs at the top left."
  (let ((out (make-overlay-buffer (* 6 35 (length string)))))
    (label-at! out string margin (+ margin (* 3.5 dot)) colour dot)
    (overlay-floats out)))


(defun overlay-vertex-buffers ()
  `((:array-stride ,(* 4 +overlay-vertex-floats+)
     :attributes ((:shader-location 0 :offset 0 :format :float32x2)
                  (:shader-location 1 :offset 8 :format :float32x2)
                  (:shader-location 2 :offset 16 :format :float32x3)))))

(defun overlay-pipeline (renderer layout)
  "The :overlay live pipeline: screen-space quads on the target after the
tone map, no depth (decision 76)."
  (live-pipeline renderer :overlay layout (overlay-vertex-buffers)))
