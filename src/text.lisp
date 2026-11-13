(in-package #:luv-in-space)

;;; Slug text on planes in the world (docs/architecture.md "Files in load
;;; order"; decision 126: Cockpit structure and view in milestone 3: frames,
;;; draw order, Slug text, head look, exposure, cab light, portholes, bezels,
;;; tape labels).
;;;
;;; Adapted from Luvcraft's luvcraft/text.lisp (instance records, glyph
;;; placement, the atlas draw) and the :slug-world-text vertex shader of
;;; luvcraft/shaders.lisp (the per-vertex dilation in pixels), by Mikael
;;; Brockman, luv at the revision pinned in flake.lock, as post.lisp credits
;;; Luvcraft's tone curve. What differs: positions are in the caller's frame
;;; and go through a block of clip rows the caller gives (the cockpit's body
;;; axes from the eye, not the world's), the compare is :greater (reversed-Z,
;;; decision 4: Depth strategy), one atlas holds the glyphs of all three
;;; fonts so all text is one draw, and each instance carries a kind and a
;;; factor (emissive or lit).
;;;
;;; The shaders are in shaders.lisp (:cab-text); the fragment stage is luv's
;;; luv.slug:slug-atlas-fragment-specification as it is.

(defparameter *text-fonts* '(:mono :mono-bold :label)
  "The fonts of the atlas, as font-path's keys.")

(defparameter *text-repertoire*
  (concatenate 'string
               (loop for code from 32 to 126 collect (code-char code))
               "°×…±²³µΔ←→↑↓○⊗▲▼■□◆·█−")
  "The characters the atlas holds for each font: printable ASCII and the
extra ones docs/design.md \"Instruments and how each is drawn\" and the
labels use. A character outside it draws as ?.")

(defparameter *text-max-glyphs* 8192 "Records in the text batch.")

(defconstant +text-record-floats+ 27
  "Luvcraft's 24 floats (origin, right edge, up edge, outline low and high
with the band counts, atlas location and ink red, band low with ink green,
band high with ink blue) and kind, factor, spare.")

(defconstant +text-cell-width+ 0.86f0 "Of a grid cell, in cap heights (docs/design.md \"Readability\").")
(defconstant +text-cell-height+ 1.6f0 "Of a grid cell, in cap heights.")

(defparameter *text-index*
  (let ((table (make-array #x2700 :element-type '(unsigned-byte 16) :initial-element 0)))
    (loop for char across *text-repertoire*
          for i from 0
          do (setf (aref table (char-code char)) (1+ i)))
    table)
  "Character code -> 1 + its index in *text-repertoire*, 0 outside it.")

(declaim (type (simple-array (unsigned-byte 16) (*)) *text-index*))

(defun text-repertoire-index (char)
  "CHAR's index in *text-repertoire*; that of ? outside it."
  (declare (type character char))
  (let* ((code (char-code char))
         (entry (if (< code (length *text-index*)) (aref *text-index* code) 0)))
    (declare (type fixnum code entry))
    (if (plusp entry) (1- entry) (- (char-code #\?) 32))))

;;; The atlas

(defstruct (text-glyph (:copier nil))
  "One character of one font in the atlas, in em units of its font."
  (band 0f0 :type single-float)
  (curve 0f0 :type single-float)
  (min-x 0f0 :type single-float)
  (min-y 0f0 :type single-float)
  (max-x 0f0 :type single-float)
  (max-y 0f0 :type single-float)
  (h-bands 0f0 :type single-float)
  (v-bands 0f0 :type single-float)
  (advance 0f0 :type single-float)
  (drawn nil))

(defstruct (text-font (:copier nil))
  ;; MISSING: the repertoire's characters the font has no glyph for.
  key path loader (units-per-em 1000 :type fixnum)
  (cap-em 0.7f0 :type single-float)
  (glyphs #() :type simple-vector)
  (missing '()))

(defstruct (text-atlas (:copier nil))
  cache atlas (fonts #() :type simple-vector))

(defun text-font-of (atlas key)
  (svref (text-atlas-fonts atlas) (position key *text-fonts*)))

(defun text-missing-characters (atlas)
  "The characters of *text-repertoire* some font of ATLAS has no glyph for."
  (remove-duplicates (loop for font across (text-atlas-fonts atlas)
                           append (text-font-missing font))))

(defun text-glyph-for (atlas key char)
  "The record of CHAR in font KEY (a font-path key); ?'s outside the repertoire."
  (svref (text-font-glyphs (text-font-of atlas key)) (text-repertoire-index char)))

(defun load-text-font (cache key)
  "KEY's font: its glyph resources through CACHE and a record per repertoire
character (the atlas locations come after, in make-text-atlas)."
  (let* ((path (font-path key))
         (loader (luv.slug:slug-font-loader path))
         (upm (zpb-ttf:units/em loader))
         (cap (luv.slug:slug-font-cap-height loader))
         (font (make-text-font :key key :path path :loader loader :units-per-em upm
                               :cap-em (float (if cap (/ cap upm) 0.7) 1f0)
                               :glyphs (make-array (length *text-repertoire*)))))
    (loop for char across *text-repertoire*
          for i from 0
          do (let ((record (make-text-glyph)))
               (setf (svref (text-font-glyphs font) i) record)
               (if (zpb-ttf:glyph-exists-p char loader)
                   (let* ((glyph (zpb-ttf:find-glyph char loader))
                          (resource (luv.slug::slug-device-glyph-for
                                     cache path (zpb-ttf:font-index glyph) loader)))
                     (setf (text-glyph-advance record)
                           (float (/ (zpb-ttf:advance-width glyph) upm) 1f0))
                     (when resource
                       (let* ((serialized (luv.slug:slug-device-glyph-serialized resource))
                              (packed (luv.slug:slug-serialized-outline-packed-outline serialized)))
                         (setf (text-glyph-drawn record) resource
                               (text-glyph-min-x record) (float (luv.slug:slug-packed-outline-min-x packed) 1f0)
                               (text-glyph-min-y record) (float (luv.slug:slug-packed-outline-min-y packed) 1f0)
                               (text-glyph-max-x record) (float (luv.slug:slug-packed-outline-max-x packed) 1f0)
                               (text-glyph-max-y record) (float (luv.slug:slug-packed-outline-max-y packed) 1f0)
                               (text-glyph-h-bands record)
                               (float (luv.slug:slug-serialized-outline-horizontal-band-count serialized) 1f0)
                               (text-glyph-v-bands record)
                               (float (luv.slug:slug-serialized-outline-vertical-band-count serialized) 1f0)))))
                   (push char (text-font-missing font)))))
    font))

(defun load-text-fonts (cache)
  "The fonts of *text-fonts*, loaded through CACHE (a device is needed only to
pack the atlas, not here)."
  (map 'simple-vector (lambda (key) (load-text-font cache key)) *text-fonts*))

(defun make-text-atlas-for (device)
  "The atlas of all repertoire glyphs of *text-fonts* on DEVICE, packed by
one slug-glyph-atlas-for call (decision 126)."
  (let* ((cache (luv.slug:make-slug-glyph-cache device))
         (fonts (load-text-fonts cache))
         (placements (loop for font across fonts
                           append (loop for record across (text-font-glyphs font)
                                        when (text-glyph-drawn record)
                                          collect (luv.slug:make-slug-glyph-placement
                                                   :resource (text-glyph-drawn record)))))
         (atlas (luv.slug:slug-glyph-atlas-for cache placements))
         (locations (luv.slug:slug-glyph-atlas-locations atlas)))
    (loop for font across fonts
          do (loop for record across (text-font-glyphs font)
                   when (text-glyph-drawn record)
                     do (destructuring-bind (band curve) (gethash (text-glyph-drawn record) locations)
                          (setf (text-glyph-band record) (float band 1f0)
                                (text-glyph-curve record) (float curve 1f0)))))
    (make-text-atlas :cache cache :atlas atlas :fonts fonts)))

(defun release-text-atlas (atlas)
  (luv.slug:release-slug-glyph-cache (text-atlas-cache atlas)))

(defun text-font-scale (atlas key cap)
  "Metres per em of font KEY at cap height CAP metres."
  (/ cap (text-font-cap-em (text-font-of atlas key))))

;;; Colours, planes, batch and regions

(defun text-colour (red green blue)
  "A colour of linear singles from sRGB integers 0-255."
  (flet ((linear (byte)
           (let ((x (/ byte 255d0)))
             (float (if (<= x 0.04045d0) (/ x 12.92d0) (expt (/ (+ x 0.055d0) 1.055d0) 2.4d0)) 1f0))))
    (make-array 3 :element-type 'single-float
                  :initial-contents (list (linear red) (linear green) (linear blue)))))

(defparameter *text-white* (text-colour 255 255 255))

(defun make-text-plane (&key (origin '(0 0 0)) (right '(1 0 0)) (up '(0 1 0)))
  "A plane of text: its origin and right and up unit vectors, nine singles,
in the frame the draw's clip rows take."
  (make-array 9 :element-type 'single-float
                :initial-contents (mapcar (lambda (x) (float x 1f0)) (append origin right up))))

(deftype text-plane () '(simple-array single-float (9)))

(defstruct (text-batch (:copier nil) (:constructor %make-text-batch))
  (data nil :type (simple-array single-float (*)))
  (count 0 :type fixnum)
  (view nil))

(defun make-text-batch (&optional (capacity *text-max-glyphs*))
  "A batch of CAPACITY records, none reserved."
  (let ((data (make-array (* capacity +text-record-floats+) :element-type 'single-float
                                                            :initial-element 0f0)))
    (%make-text-batch :data data
                      :view (make-array (length data) :element-type 'single-float
                                                      :displaced-to data :fill-pointer 0))))

(defun text-batch-capacity (batch)
  (floor (length (text-batch-data batch)) +text-record-floats+))

(defstruct (text-region (:copier nil))
  "CAPACITY consecutive records of BATCH from START that one caller rewrites."
  batch (start 0 :type fixnum) (capacity 0 :type fixnum))

(defun text-batch-region (batch capacity)
  "A region of CAPACITY records at the end of BATCH's used part."
  (let ((start (text-batch-count batch)))
    (when (> (+ start capacity) (text-batch-capacity batch))
      (error "The text batch holds ~D glyphs; ~D asked for after ~D." (text-batch-capacity batch)
             capacity start))
    (setf (text-batch-count batch) (+ start capacity))
    (make-text-region :batch batch :start start :capacity capacity)))

(defun text-region-clear! (region &optional (from 0))
  "Zero REGION's records from index FROM: zero-area quads draw nothing."
  (let ((data (text-batch-data (text-region-batch region))))
    (fill data 0f0
          :start (* +text-record-floats+ (+ (text-region-start region) from))
          :end (* +text-record-floats+ (+ (text-region-start region) (text-region-capacity region))))
    region))

(defun text-region-recolour! (region colour)
  "REGION's records' ink set to COLOUR, a linear triple: nothing else changes.
Conses nothing."
  (declare (type text-region region) (type (simple-array single-float (3)) colour))
  (let ((data (text-batch-data (text-region-batch region))))
    (dotimes (i (text-region-capacity region))
      (let ((base (* +text-record-floats+ (+ (text-region-start region) i))))
        (setf (aref data (+ base 17)) (aref colour 0)
              (aref data (+ base 20)) (aref colour 1)
              (aref data (+ base 23)) (aref colour 2))))
    region))

(defun text-batch-upload! (batch buffer)
  "Write BATCH's used records to the vertex buffer BUFFER."
  (let ((view (text-batch-view batch)))
    (setf (fill-pointer view) (* +text-record-floats+ (text-batch-count batch)))
    (when (plusp (text-batch-count batch))
      (luv:write-buffer buffer view))))

;;; Runs

(declaim (inline put-text-record!))
(defun put-text-record! (data base glyph pen-x pen-y scale plane rotation-cos rotation-sin
                         colour kind factor)
  "The record of GLYPH at BASE in DATA: its em origin at (PEN-X, PEN-Y) metres
along PLANE's axes turned by the angle whose cosine and sine are given, SCALE
metres per em."
  (declare (optimize speed (safety 0)) (sb-ext:muffle-conditions sb-ext:compiler-note)
           (type (simple-array single-float (*)) data colour) (type fixnum base)
           (type text-glyph glyph) (type single-float pen-x pen-y scale rotation-cos rotation-sin factor)
           (type text-plane plane) (type fixnum kind))
  (let* ((pad (float luv.slug:*slug-static-padding* 1f0))
         (left (- (text-glyph-min-x glyph) pad)) (bottom (- (text-glyph-min-y glyph) pad))
         (right (+ (text-glyph-max-x glyph) pad)) (top (+ (text-glyph-max-y glyph) pad))
         (rx (+ (* rotation-cos (aref plane 3)) (* rotation-sin (aref plane 6))))
         (ry (+ (* rotation-cos (aref plane 4)) (* rotation-sin (aref plane 7))))
         (rz (+ (* rotation-cos (aref plane 5)) (* rotation-sin (aref plane 8))))
         (ux (- (* rotation-cos (aref plane 6)) (* rotation-sin (aref plane 3))))
         (uy (- (* rotation-cos (aref plane 7)) (* rotation-sin (aref plane 4))))
         (uz (- (* rotation-cos (aref plane 8)) (* rotation-sin (aref plane 5))))
         (x (+ pen-x (* left scale))) (y (+ pen-y (* bottom scale)))
         (width (* (- right left) scale)) (height (* (- top bottom) scale)))
    (declare (type single-float pad left bottom right top rx ry rz ux uy uz x y width height))
    (setf (aref data base) (+ (aref plane 0) (* x rx) (* y ux))
          (aref data (+ base 1)) (+ (aref plane 1) (* x ry) (* y uy))
          (aref data (+ base 2)) (+ (aref plane 2) (* x rz) (* y uz))
          (aref data (+ base 3)) (* width rx) (aref data (+ base 4)) (* width ry)
          (aref data (+ base 5)) (* width rz)
          (aref data (+ base 6)) (* height ux) (aref data (+ base 7)) (* height uy)
          (aref data (+ base 8)) (* height uz)
          (aref data (+ base 9)) left (aref data (+ base 10)) bottom
          (aref data (+ base 11)) (text-glyph-h-bands glyph)
          (aref data (+ base 12)) right (aref data (+ base 13)) top
          (aref data (+ base 14)) (text-glyph-v-bands glyph)
          (aref data (+ base 15)) (text-glyph-band glyph)
          (aref data (+ base 16)) (text-glyph-curve glyph)
          (aref data (+ base 17)) (aref colour 0)
          (aref data (+ base 18)) (text-glyph-min-x glyph)
          (aref data (+ base 19)) (text-glyph-min-y glyph)
          (aref data (+ base 20)) (aref colour 1)
          (aref data (+ base 21)) (text-glyph-max-x glyph)
          (aref data (+ base 22)) (text-glyph-max-y glyph)
          (aref data (+ base 23)) (aref colour 2)
          (aref data (+ base 24)) (float kind 1f0)
          (aref data (+ base 25)) factor
          (aref data (+ base 26)) 0f0)
    nil))

(defun text-kind (kind)
  (ecase kind (:emissive 0) (:lit 1)))

(defun text-grid-cell-origin (plane cap column row)
  "The em origin (baseline, left) of the character at COLUMN of ROW of a grid
run on PLANE at cap height CAP, as three doubles (docs/design.md
\"Readability\": cells 0.86 cap wide and 1.6 cap high, rows downward)."
  (let ((x (* column +text-cell-width+ cap)) (y (- (* row +text-cell-height+ cap))))
    (values-list (loop for i below 3
                       collect (+ (aref plane i) (* x (aref plane (+ 3 i))) (* y (aref plane (+ 6 i))))))))

(defun grid-run! (atlas region index plane string
                  &key (font :mono) (cap 0.01f0) (colour *text-white*) (kind :emissive)
                    (factor 1f0) (field (length string)) (align :left) (row 0))
  "STRING as FIELD monospace cells (default its length) of ROW of PLANE at cap
height CAP metres, written to REGION's records from INDEX; the field's other
cells are zeroed. ALIGN :left, :right or :centre places the string in the
field; one record per cell, spaces and characters without an outline
blank. Conses nothing. Returns the index after the field."
  (declare (type text-region region) (type (unsigned-byte 24) index field) (type (signed-byte 24) row)
           (type string string) (type single-float cap factor) (type text-plane plane)
           (type (simple-array single-float (3)) colour))
  (let* ((data (text-batch-data (text-region-batch region)))
         (font-data (text-font-of atlas font))
         (glyphs (text-font-glyphs font-data))
         (scale (/ cap (text-font-cap-em font-data)))
         (length (min (length string) field))
         (shift (ecase align
                  (:left 0) (:right (- field length)) (:centre (floor (- field length) 2))))
         (kind (text-kind kind)))
    (declare (type single-float scale) (type (unsigned-byte 24) length) (type (signed-byte 24) shift)
             (type fixnum kind))
    (when (> (+ index field) (text-region-capacity region))
      (error "The region holds ~D glyphs; ~D asked for from ~D." (text-region-capacity region) field index))
    (dotimes (cell field)
      (let* ((base (* +text-record-floats+ (+ (text-region-start region) index cell)))
             (k (- cell shift))
             (glyph (and (<= 0 k) (< k length)
                         (svref glyphs (text-repertoire-index (char string k))))))
        (declare (type fixnum base k))
        (if (and glyph (text-glyph-drawn glyph))
            (put-text-record! data base glyph
                              (* cell +text-cell-width+ cap)
                              (- (* row +text-cell-height+ cap))
                              scale plane 1f0 0f0 colour kind factor)
            (fill data 0f0 :start base :end (+ base +text-record-floats+)))))
    (+ index field)))

(defun shaped-run! (atlas region index plane string
                    &key (font :label) (cap 0.01f0) (colour *text-white*) (kind :emissive)
                      (factor 1f0) (align :left) (rotation 0f0))
  "STRING shaped by HarfBuzz (kerning, advances) at cap height CAP metres, its
baseline origin at PLANE's origin, written to REGION from INDEX; ALIGN
:left, :right or :centre by its advance width, ROTATION radians turns it counterclockwise
in the plane. Made once, not per frame. Returns the index after the run, the
advance width and the outlines' extents (min-x max-x min-y max-y), in metres
along the plane's axes before the rotation. A glyph outside the atlas (a
substitution the repertoire lacks) is an error."
  (let* ((font-data (text-font-of atlas font))
         (path (text-font-path font-data))
         (shaped (luv.slug:slug-shaped-text-for path string))
         (glyphs (luv.slug:make-slug-glyph-placements shaped (text-font-loader font-data)
                                                      (text-atlas-cache atlas) path))
         (locations (luv.slug:slug-glyph-atlas-locations (text-atlas-atlas atlas)))
         (scale (/ cap (text-font-cap-em font-data)))
         (advance (* scale (/ (luv.slug:slug-shaped-text-x-advance shaped)
                              (luv.slug:slug-shaped-text-units-per-em shaped))))
         (shift (ecase align (:left 0) (:right (- advance)) (:centre (* -0.5 advance))))
         (c (cos rotation)) (s (sin rotation))
         (min-x most-positive-single-float) (max-x most-negative-single-float)
         (min-y most-positive-single-float) (max-y most-negative-single-float)
         (colour (coerce colour '(simple-array single-float (3))))
         (data (text-batch-data (text-region-batch region)))
         (kind (text-kind kind))
         (record (make-text-glyph)))
    (when (> (+ index (length glyphs)) (text-region-capacity region))
      (error "The region holds ~D glyphs; ~D asked for from ~D." (text-region-capacity region)
             (length glyphs) index))
    (loop for placement in glyphs
          for i from index
          do (let ((location (gethash (luv.slug:slug-glyph-placement-resource placement) locations))
                   (serialized (luv.slug:slug-device-glyph-serialized
                                (luv.slug:slug-glyph-placement-resource placement)))
                   (x (+ shift (* scale (luv.slug:slug-glyph-placement-origin-x placement))))
                   (y (* scale (luv.slug:slug-glyph-placement-origin-y placement))))
               (unless location
                 (error "Glyph ~D of ~S is not in the text atlas." (luv.slug:slug-glyph-placement-glyph-id placement)
                        string))
               (setf (text-glyph-band record) (float (first location) 1f0)
                     (text-glyph-curve record) (float (second location) 1f0)
                     (text-glyph-min-x record) (float (luv.slug:slug-glyph-placement-outline-min-x placement) 1f0)
                     (text-glyph-min-y record) (float (luv.slug:slug-glyph-placement-outline-min-y placement) 1f0)
                     (text-glyph-max-x record) (float (luv.slug:slug-glyph-placement-outline-max-x placement) 1f0)
                     (text-glyph-max-y record) (float (luv.slug:slug-glyph-placement-outline-max-y placement) 1f0)
                     (text-glyph-h-bands record)
                     (float (luv.slug:slug-serialized-outline-horizontal-band-count serialized) 1f0)
                     (text-glyph-v-bands record)
                     (float (luv.slug:slug-serialized-outline-vertical-band-count serialized) 1f0))
               (put-text-record! data (* +text-record-floats+ (+ (text-region-start region) i)) record
                                 (float x 1f0) (float y 1f0) (float scale 1f0) plane
                                 (float c 1f0) (float s 1f0) colour kind (float factor 1f0))
               (setf min-x (min min-x (+ x (* scale (text-glyph-min-x record))))
                     max-x (max max-x (+ x (* scale (text-glyph-max-x record))))
                     min-y (min min-y (+ y (* scale (text-glyph-min-y record))))
                     max-y (max max-y (+ y (* scale (text-glyph-max-y record)))))))
    (values (+ index (length glyphs)) advance min-x max-x min-y max-y)))

;;; The GPU side: the pipeline and the draw. The caller gives the clip rows
;;; (sixteen singles: x, y, z, w) of the frame its instances are in; the
;;; frame renderer keeps the atlas, the layout and the quad corners
;;; (render.lisp), each frame-slot a block, a bind group and an instance
;;; buffer.

(defconstant +text-block-floats+ 60
  "The :cab-text shaders' block, the cab block of cockpit.lisp: clip rows
(16), screen (clip units per pixel x, y), params (1 / exposure, the lit
text's constant factor, the cab's ambient), then what only the cab fills (the
Sun, the LED strips, the openings: 36 floats, zero in a text-block!).")

(defparameter *text-lit-factor* 0.04f0
  "The lit text's constant radiance factor (ink x this) outside the cockpit,
where the cab's strips are not in the block; the cab block's is 0 and the
glyph's irradiance from the strips (cab-irradiance, decision 126) lights it.")

(defun text-corners ()
  "The glyph quad's six corners as three singles each."
  (coerce '(0.0 0.0 0.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 0.0)
          '(simple-array single-float (*))))

(defun text-layout (renderer)
  (gpu-create renderer (luv:make-bind-group-layout-descriptor
                        :entries '((:binding 0 :type :texture)
                                   (:binding 1 :type :texture)
                                   (:binding 2 :type :uniform-buffer)))))

(defun text-pipeline (renderer layout format)
  "The :cab-text live pipeline into FORMAT: quads per vertex, glyph records per
instance; premultiplied alpha, depth compared :greater, not written."
  (live-pipeline renderer :cab-text layout
                 `((:array-stride 12
                    :attributes ((:shader-location 0 :offset 0 :format :float32x3)))
                   (:array-stride ,(* 4 +text-record-floats+) :step-mode :instance
                    :attributes ,(loop for k below 9
                                       collect `(:shader-location ,(1+ k) :offset ,(* 12 k)
                                                 :format :float32x3))))
                 :format format :blend :premultiplied-alpha
                 :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                  :depth-compare :greater)))

(defstruct (text-slot (:copier nil) (:constructor %make-text-slot))
  "A frame target's text resources: the block, its bind group, the instances."
  buffer bind-group instances)

(defun make-text-slot (renderer layout atlas capacity)
  (let* ((device (renderer-device renderer))
         (buffer (luv:create device (luv:make-buffer-descriptor
                                     :label "text block" :size (* 4 +text-block-floats+)
                                     :usage '(:uniform :copy-dst))))
         (atlas (text-atlas-atlas atlas)))
    (%make-text-slot
     :buffer buffer
     :instances (luv:create device (luv:make-buffer-descriptor
                                    :label "text instances"
                                    :size (* 4 +text-record-floats+ capacity)
                                    :usage '(:vertex :copy-dst)))
     :bind-group (luv:create device (luv:make-bind-group-descriptor
                                     :label "text"
                                     :layout layout
                                     :entries `((:binding 0 :resource ,(luv.slug:slug-glyph-atlas-band-view atlas))
                                                (:binding 1 :resource ,(luv.slug:slug-glyph-atlas-curve-view atlas))
                                                (:binding 2 :resource ,buffer)))))))

(defun release-text-slot (slot)
  (luv:destroy (text-slot-bind-group slot))
  (luv:destroy (text-slot-instances slot))
  (luv:destroy (text-slot-buffer slot)))

(defun text-block! (data rows extent exposure &optional (irradiance *text-lit-factor*))
  "DATA's text block from the sixteen clip ROWS (singles, from index 0), the
target's EXTENT and EXPOSURE."
  (declare (type (simple-array single-float (*)) data rows))
  (replace data rows :end2 16)
  (fill data 0f0 :start 24)
  (setf (aref data 16) (float (/ 2 (first extent)) 1f0)
        (aref data 17) (float (/ 2 (second extent)) 1f0)
        (aref data 18) 0f0 (aref data 19) 0f0
        (aref data 20) (float (/ 1 exposure) 1f0)
        (aref data 21) (float irradiance 1f0)
        (aref data 22) 0f0 (aref data 23) 0f0)
  data)

(defun encode-text (pass renderer slot batch block-data corners)
  "BATCH's records into the open scene PASS through SLOT, with BLOCK-DATA the
text block already filled; nothing for an empty batch."
  (when (plusp (text-batch-count batch))
    (luv:write-buffer (text-slot-buffer slot) block-data)
    (text-batch-upload! batch (text-slot-instances slot))
    (luv:set-pipeline pass (renderer-pipeline renderer :cab-text))
    (luv:set-bind-group pass 0 (text-slot-bind-group slot))
    (luv:set-vertex-buffer pass 0 corners)
    (luv:set-vertex-buffer pass 1 (text-slot-instances slot))
    (luv:draw pass 6 (text-batch-count batch))))

;;; The line batch: segments of the :cab-lines pipeline (shaders.lisp), in
;;; regions like the text batch's: plot lines, inverted runs' blocks, outlines
;;; (decision 127: Cockpit screens' contents in milestone 3: lines, modes,
;;; plots, colours and updates). Drawn in the same bind group as the text.

(defconstant +line-record-floats+ 10 "From xyz, to xyz, tint rgb and the width in metres.")
(defparameter *line-max-records* 512)

(defstruct (line-batch (:copier nil) (:constructor %make-line-batch))
  (data nil :type (simple-array single-float (*)))
  (count 0 :type fixnum)
  (view nil))

(defun make-line-batch (&optional (capacity *line-max-records*))
  (let ((data (make-array (* capacity +line-record-floats+) :element-type 'single-float
                                                            :initial-element 0f0)))
    (%make-line-batch :data data
                      :view (make-array (length data) :element-type 'single-float
                                                      :displaced-to data :fill-pointer 0))))

(defun line-batch-capacity (batch)
  (floor (length (line-batch-data batch)) +line-record-floats+))

(defstruct (line-region (:copier nil))
  batch (start 0 :type fixnum) (capacity 0 :type fixnum))

(defun line-batch-region (batch capacity)
  (let ((start (line-batch-count batch)))
    (when (> (+ start capacity) (line-batch-capacity batch))
      (error "The line batch holds ~D segments; ~D asked for after ~D."
             (line-batch-capacity batch) capacity start))
    (setf (line-batch-count batch) (+ start capacity))
    (make-line-region :batch batch :start start :capacity capacity)))

(defun line-region-clear! (region &optional (from 0))
  "Zero REGION's segments from index FROM: zero-width segments draw nothing."
  (fill (line-batch-data (line-region-batch region)) 0f0
        :start (* +line-record-floats+ (+ (line-region-start region) from))
        :end (* +line-record-floats+ (+ (line-region-start region) (line-region-capacity region))))
  region)

(defun line-put! (region index ax ay az bx by bz colour width)
  "Segment INDEX of REGION from A to B (body axes from the eye, metres), COLOUR a
linear (single-float (3)) as the screen shows it, WIDTH metres across. Conses nothing."
  (declare (type line-region region) (type fixnum index) (type single-float ax ay az bx by bz width)
           (type (simple-array single-float (3)) colour))
  (when (>= index (line-region-capacity region))
    (error "The line region holds ~D segments." (line-region-capacity region)))
  (let ((data (line-batch-data (line-region-batch region)))
        (base (* +line-record-floats+ (+ (line-region-start region) index))))
    (declare (type fixnum base))
    (setf (aref data base) ax (aref data (+ base 1)) ay (aref data (+ base 2)) az
          (aref data (+ base 3)) bx (aref data (+ base 4)) by (aref data (+ base 5)) bz
          (aref data (+ base 6)) (aref colour 0) (aref data (+ base 7)) (aref colour 1)
          (aref data (+ base 8)) (aref colour 2) (aref data (+ base 9)) width)
    index))

(defun line-batch-upload! (batch buffer)
  (let ((view (line-batch-view batch)))
    (setf (fill-pointer view) (* +line-record-floats+ (line-batch-count batch)))
    (when (plusp (line-batch-count batch))
      (luv:write-buffer buffer view))))

(defun line-corners ()
  "A segment's quad: along 0 or 1, across -1 or 1, six corners of two singles."
  (coerce '(0.0 -1.0 1.0 -1.0 1.0 1.0 0.0 -1.0 1.0 1.0 0.0 1.0) '(simple-array single-float (*))))

(defun lines-pipeline (renderer layout format)
  "The :cab-lines live pipeline into FORMAT: premultiplied alpha, depth compared
:greater, not written."
  (live-pipeline renderer :cab-lines layout
                 `((:array-stride 8
                    :attributes ((:shader-location 0 :offset 0 :format :float32x2)))
                   (:array-stride ,(* 4 +line-record-floats+) :step-mode :instance
                    :attributes ((:shader-location 1 :offset 0 :format :float32x3)
                                 (:shader-location 2 :offset 12 :format :float32x3)
                                 (:shader-location 3 :offset 24 :format :float32x4))))
                 :format format :blend :premultiplied-alpha
                 :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                  :depth-compare :greater)))

(defun make-line-buffer (renderer capacity)
  (luv:create (renderer-device renderer)
              (luv:make-buffer-descriptor :label "line instances"
                                          :size (* 4 +line-record-floats+ capacity)
                                          :usage '(:vertex :copy-dst))))

(defun encode-lines (pass renderer text-slot batch buffer corners)
  "BATCH's segments into the open scene PASS (the block of TEXT-SLOT written
before), through BUFFER; nothing for an empty batch."
  (when (plusp (line-batch-count batch))
    (line-batch-upload! batch buffer)
    (luv:set-pipeline pass (renderer-pipeline renderer :cab-lines))
    (luv:set-bind-group pass 0 (text-slot-bind-group text-slot))
    (luv:set-vertex-buffer pass 0 corners)
    (luv:set-vertex-buffer pass 1 buffer)
    (luv:draw pass 6 (line-batch-count batch))))

;;; The dot batch: the strip's round LEDs of the :cab-dots pipeline
;;; (shaders.lisp), one record each: centre xyz, the radius in metres, tint rgb,
;;; the share of the emissive that burns.

(defconstant +dot-record-floats+ 8)

(defstruct (dot-batch (:copier nil) (:constructor %make-dot-batch))
  (data nil :type (simple-array single-float (*)))
  (count 0 :type fixnum)
  (view nil))

(defun make-dot-batch (&optional (capacity 2560))
  (let ((data (make-array (* capacity +dot-record-floats+) :element-type 'single-float
                                                           :initial-element 0f0)))
    (%make-dot-batch :data data
                     :view (make-array (length data) :element-type 'single-float
                                                     :displaced-to data :fill-pointer 0))))

(defun dot-batch-capacity (batch)
  (floor (length (dot-batch-data batch)) +dot-record-floats+))

(defun dot-batch-take! (batch count)
  "The index of COUNT new records at the batch's end."
  (let ((start (dot-batch-count batch)))
    (when (> (+ start count) (dot-batch-capacity batch))
      (error "The dot batch holds ~D dots; ~D asked for after ~D." (dot-batch-capacity batch) count start))
    (setf (dot-batch-count batch) (+ start count))
    start))

(defun dot-batch-upload! (batch buffer)
  (let ((view (dot-batch-view batch)))
    (setf (fill-pointer view) (* +dot-record-floats+ (dot-batch-count batch)))
    (when (plusp (dot-batch-count batch))
      (luv:write-buffer buffer view))))

(defun dots-pipeline (renderer layout format)
  "The :cab-dots live pipeline into FORMAT: as the lines'."
  (live-pipeline renderer :cab-dots layout
                 `((:array-stride 8
                    :attributes ((:shader-location 0 :offset 0 :format :float32x2)))
                   (:array-stride ,(* 4 +dot-record-floats+) :step-mode :instance
                    :attributes ((:shader-location 1 :offset 0 :format :float32x4)
                                 (:shader-location 2 :offset 16 :format :float32x4))))
                 :format format :blend :premultiplied-alpha
                 :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                  :depth-compare :greater)))

(defun make-dot-buffer (renderer capacity)
  (luv:create (renderer-device renderer)
              (luv:make-buffer-descriptor :label "dot instances"
                                          :size (* 4 +dot-record-floats+ capacity)
                                          :usage '(:vertex :copy-dst))))

(defun encode-dots (pass renderer text-slot batch buffer corners)
  "BATCH's dots into the open scene PASS (the block of TEXT-SLOT written
before), through BUFFER, on the lines' corners; nothing for an empty batch."
  (when (plusp (dot-batch-count batch))
    (dot-batch-upload! batch buffer)
    (luv:set-pipeline pass (renderer-pipeline renderer :cab-dots))
    (luv:set-bind-group pass 0 (text-slot-bind-group text-slot))
    (luv:set-vertex-buffer pass 0 corners)
    (luv:set-vertex-buffer pass 1 buffer)
    (luv:draw pass 6 (dot-batch-count batch))))
