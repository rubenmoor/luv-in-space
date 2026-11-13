(in-package #:luv-in-space.tests)

;;; text.lisp: Slug text on planes (decision 126: Cockpit structure and view
;;; in milestone 3: frames, draw order, Slug text, head look, exposure, cab
;;; light, portholes, bezels, tape labels). The GPU check is in render.lisp.

(defvar *text-test-atlas* nil)

(defun text-test-atlas ()
  "The three fonts' glyph records without a device: a cache with no device, and
an atlas whose locations are zeros (only the GPU packs the real ones)."
  (or *text-test-atlas*
      (setf *text-test-atlas*
            (let* ((cache (luv.slug:make-slug-glyph-cache nil))
                   (fonts (lis::load-text-fonts cache))
                   (locations (make-hash-table :test #'eq)))
              (loop for font across fonts
                    do (loop for record across (lis::text-font-glyphs font)
                             when (lis::text-glyph-drawn record)
                               do (setf (gethash (lis::text-glyph-drawn record) locations) (list 0 0))))
              (lis::make-text-atlas :cache cache :fonts fonts
                                    :atlas (luv.slug:make-slug-glyph-atlas :locations locations))))))

(defun record-floats (batch index)
  "Record INDEX of BATCH as a list of its 27 floats."
  (loop for k below lis::+text-record-floats+
        collect (aref (lis::text-batch-data batch) (+ k (* index lis::+text-record-floats+)))))

(defun record-vec (record from)
  (subseq record from (+ from 3)))

(defun vec-length (v)
  (sqrt (reduce #'+ (map 'list (lambda (x) (* (float x 1d0) x)) v))))

(define-test text-repertoire-in-every-font
  (let ((atlas (text-test-atlas)))
    (false (lis::text-missing-characters atlas))
    (dolist (font lis::*text-fonts*)
      (loop for char across lis::*text-repertoire*
            for record = (lis::text-glyph-for atlas font char)
            do (true (plusp (lis::text-glyph-advance record)) "~S in ~S has no advance" char font)
               (unless (char= char #\Space)
                 (true (lis::text-glyph-drawn record) "~S in ~S has no outline" char font)))
      (is eq (lis::text-glyph-for atlas font #\?) (lis::text-glyph-for atlas font (code-char 955))
          "a character outside the repertoire gives ?")
      (isnt eq (lis::text-glyph-for atlas font #\H) (lis::text-glyph-for atlas font #\?)))
    (dolist (char (coerce "°×…±²³µΔ←→↑↓○⊗▲▼■□◆·" 'list))
      (isnt eq (lis::text-glyph-for atlas :mono char) (lis::text-glyph-for atlas :mono #\?)
            "~S is in the repertoire" char)
      (isnt = (lis::text-repertoire-index char) (lis::text-repertoire-index #\?)))))

(define-test grid-run-cells
  ;; Character k of row j at its cell's origin, on an oblique plane.
  (let* ((atlas (text-test-atlas))
         (batch (lis::make-text-batch 200))
         (region (lis::text-batch-region batch 200))
         (cap 0.013d0)
         (right (list 0.6d0 0.0d0 0.8d0))
         (up (list 0.0d0 1.0d0 0.0d0))
         (origin (list 0.5d0 0.3d0 -0.7d0))
         (plane (lis::make-text-plane :origin origin :right right :up up))
         (scale (/ cap (lis::text-font-cap-em (lis::text-font-of atlas :mono))))
         (rows '("HELLO" "WORLD!" "grid ok")))
    (loop for row in rows
          for j from 0
          do (lis::grid-run! atlas region (* j 20) plane row :cap (float cap 1f0) :row j :field 20))
    (loop for row in rows
          for j from 0
          do (loop for k below 20
                   for record = (record-floats batch (+ (* j 20) k))
                   do (if (and (< k (length row)) (char/= (char row k) #\Space))
                          (let* ((glyph (lis::text-glyph-for atlas :mono (char row k)))
                                 (x (+ (* k 0.86d0 cap) (* scale (lis::text-glyph-min-x glyph))))
                                 (y (+ (* j -1.6d0 cap) (* scale (lis::text-glyph-min-y glyph)))))
                            (loop for i below 3
                                  for expected = (+ (nth i origin) (* x (nth i right)) (* y (nth i up)))
                                  do (true (< (abs (- expected (nth i record))) 1d-6)
                                           "~S column ~D row ~D axis ~D: ~F, expected ~F"
                                           (char row k) k j i (nth i record) expected)))
                          (true (every #'zerop record) "blank cell ~D of row ~D is not zero" k j))))))

(define-test grid-run-alignment-and-field
  (let* ((atlas (text-test-atlas))
         (batch (lis::make-text-batch 100))
         (region (lis::text-batch-region batch 100))
         (plane (lis::make-text-plane))
         (cell (* 0.86d0 0.01d0)))
    (flet ((first-x (align)
             (lis::text-region-clear! region)
             (is = 10 (lis::grid-run! atlas region 0 plane "OO" :field 10 :align align :cap 0.01f0))
             (let ((glyph (lis::text-glyph-for atlas :mono #\O))
                   (scale (/ 0.01d0 (lis::text-font-cap-em (lis::text-font-of atlas :mono)))))
               (- (nth 0 (record-floats batch (loop for i below 10
                                                    unless (every #'zerop (record-floats batch i)) return i)))
                  (* scale (lis::text-glyph-min-x glyph))))))
      (true (< (abs (first-x :left)) 1d-6))
      (true (< (abs (- (* 8 cell) (first-x :right))) 1d-6))
      (true (< (abs (- (* 4 cell) (first-x :centre))) 1d-6)))
    (is = 0 (lis::grid-run! atlas region 0 plane "" :field 0))
    (fail (lis::grid-run! atlas region 95 plane "TOO LONG" :field 8) 'error)
    (fail (lis::text-batch-region batch 1) 'error)))

(define-test text-cap-height-and-advance
  (let* ((atlas (text-test-atlas))
         (batch (lis::make-text-batch 4))
         (region (lis::text-batch-region batch 4))
         (cap 0.013f0))
    (dolist (font '(:mono :mono-bold :label))
      (lis::text-region-clear! region)
      (lis::grid-run! atlas region 0 (lis::make-text-plane) "H" :font font :cap cap)
      (let* ((record (record-floats batch 0))
             (stem-height (vec-length (subseq record 6 9)))
             (width (vec-length (subseq record 3 6)))
             (glyph (lis::text-glyph-for atlas font #\H)))
        (true (< (abs (- stem-height cap)) (* 0.01 cap)) "~S H's outline is ~F m high" font stem-height)
        (true (< 0 width (* 1.2d0 0.86d0 cap)))
        (true (< (abs (lis::text-glyph-min-y glyph)) 1f-3))))
    ;; Monaspace advances are the grid's cell width (0.86 cap, within 2%).
    (dolist (font '(:mono :mono-bold))
      (let ((scale (/ cap (lis::text-font-cap-em (lis::text-font-of atlas font)))))
        (loop for char across lis::*text-repertoire*
              for advance = (lis::text-glyph-advance (lis::text-glyph-for atlas font char))
              do (true (< (abs (- (* advance scale) (* 0.86d0 cap))) (* 0.02d0 0.86d0 cap))
                       "~S in ~S advances ~F of ~F" char font (* advance scale) (* 0.86d0 cap)))))))

(defparameter *label-texts*
  '("ORBIT" "ATT [WASD QE]" "TGT NEXT ] PREV [ CLR \\" "AUTOPILOT [O] [RET]" "SHIP  XFER [B]"
    "COMMS  ACK [Y]" "WARP [. , /] PAUSE [P]" "COMPUTER  DAMP [T]" "THR [SHFT/CTRL]"
    "[Z] MAX [X] CUT" "HULL TEMP" "ACK [Y]" "STATUS" "VIEW [V]  MAP [M]  HOME [HOME]")
  "decision 126's tape labels.")

(define-test shaped-run-narrower-than-grid
  (let* ((atlas (text-test-atlas))
         (batch (lis::make-text-batch 100))
         (region (lis::text-batch-region batch 100))
         (cap 0.02f0)
         (text "AUTOPILOT [O] [RET]")
         (plane (lis::make-text-plane)))
    (multiple-value-bind (next advance min-x max-x min-y max-y)
        (lis::shaped-run! atlas region 0 plane text :cap cap)
      (true (plusp next))
      (true (< advance (* (length text) 0.86d0 cap))
            "shaped ~F m, grid ~F m" advance (* (length text) 0.86d0 cap))
      (true (< 0 min-x)) (true (< min-x max-x (* 1.01 advance)))
      (true (< min-y 0 max-y))
      ;; Every outline's quad inside the extents (an unrotated plane in x, y).
      (let ((scale (/ cap (lis::text-font-cap-em (lis::text-font-of atlas :label))))
            (lowest most-positive-double-float))
        (dotimes (i next)
          (let* ((record (record-floats batch i))
                 (low (+ (nth 0 record) (* scale (- (nth 18 record) (nth 9 record)))))
                 (high (+ (nth 0 record) (* scale (- (nth 21 record) (nth 9 record))))))
            (setf lowest (min lowest low))
            (true (>= (+ low 1d-6) min-x) "glyph ~D starts at ~F, extents ~F" i low min-x)
            (true (<= high (+ max-x 1d-6)))))
        (true (< (abs (- lowest min-x)) 1d-6))))
    ;; The label texts all shape into the atlas's repertoire glyphs.
    (dolist (label *label-texts*)
      (lis::text-region-clear! region)
      (finish (lis::shaped-run! atlas region 0 plane label :cap cap)))
    ;; Alignment and rotation.
    (let ((advance (nth-value 1 (lis::shaped-run! atlas region 0 plane "AB" :cap cap))))
      (lis::shaped-run! atlas region 0 plane "AB" :cap cap :align :centre)
      (let ((centred (nth 0 (record-floats batch 0)))
            (left (progn (lis::shaped-run! atlas region 0 plane "AB" :cap cap) (nth 0 (record-floats batch 0)))))
        (true (< (abs (- (- left (* 0.5d0 advance)) centred)) 1d-6)))
      (lis::shaped-run! atlas region 0 plane "AB" :cap cap :rotation (float (/ pi 2) 1f0))
      (let ((record (record-floats batch 0)))
        ;; Turned a quarter: the right edge points along +y.
        (true (< (abs (nth 3 record)) 1f-6))
        (true (> (nth 4 record) 0))))))

(define-test grid-run-conses-nothing
  (let* ((atlas (text-test-atlas))
         (batch (lis::make-text-batch 2000))
         (region (lis::text-batch-region batch 2000))
         (plane (lis::make-text-plane :origin '(0.1 0.2 0.3) :right '(0 1 0) :up '(0 0 1)))
         (line "ALT 400.2 km  PE 398.1 km  AP 401.9 km  ECC 0.0003  INC 90.0")
         (colour (lis::text-colour 120 255 160)))
    (flet ((fill-region ()
             (loop for row below 40
                   do (lis::grid-run! atlas region (* row 50) plane line
                                      :field 50 :row row :cap 0.012f0 :colour colour
                                      :kind :lit :factor 0.5f0 :align :centre))))
      (fill-region)
      (true (< (least-bytes-consed #'fill-region) 1000)))))

(define-test marker-run-centres-a-glyph-on-a-point
  ;; The record's quad is centred on the outline's box, so its centre is the point;
  ;; a space has no outline and zeroes its record.
  (let* ((atlas (text-test-atlas))
         (batch (lis::make-text-batch 4))
         (region (lis::text-batch-region batch 2))
         (plane (lis::make-text-plane :origin '(0.5 0.1 0.2) :right '(0 1 0) :up '(0 0 1)))
         (colour (lis::text-colour 232 228 216)))
    (is = 1 (lis::marker-run! atlas region 0 plane #\■ 0.02f0 -0.01f0 :cap 0.012f0 :colour colour))
    (let* ((record (record-floats batch 0))
           (x (+ (nth 0 record) (* 0.5 (nth 3 record)) (* 0.5 (nth 6 record))))
           (y (+ (nth 1 record) (* 0.5 (nth 4 record)) (* 0.5 (nth 7 record))))
           (z (+ (nth 2 record) (* 0.5 (nth 5 record)) (* 0.5 (nth 8 record)))))
      (true (< (abs (- x 0.5)) 1f-6))
      (true (< (abs (- y (+ 0.1 0.02))) 1f-5) "along right: ~F" y)
      (true (< (abs (- z (+ 0.2 -0.01))) 1f-5) "along up: ~F" z))
    (lis::marker-run! atlas region 1 plane #\Space 0f0 0f0 :cap 0.012f0 :colour colour)
    (true (every #'zerop (record-floats batch 1)))
    (is = 0 (least-bytes-consed
             (lambda () (dotimes (i 1000) (lis::marker-run! atlas region 0 plane #\○ 0.02f0 0f0
                                                            :cap 0.012f0 :colour colour)))))))

(define-test text-repertoire-has-the-middle-dot-and-minus
  (true (find #\· lis::*text-repertoire*))
  (true (find #\− lis::*text-repertoire*)))
