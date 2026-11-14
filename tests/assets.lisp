(in-package #:luv-in-space.tests)

;;; assets.lisp: the assets root, missing assets, the Blue Marble's pixels
;;; (decision 13: Asset storage and attribution; decision 40: Earth's look
;;; and drag).

(define-test assets-root-from-env
  (is equal (uiop:ensure-directory-pathname "/opt/lis/assets")
      (lis::assets-root "/opt/lis/assets"))
  (is equal (uiop:ensure-directory-pathname "/opt/lis/assets")
      (lis::assets-root "/opt/lis/assets/"))
  (let ((default (asdf:system-relative-pathname "luv-in-space" "assets/")))
    (is equal default (lis::assets-root nil))
    (is equal default (lis::assets-root ""))))

(define-test asset-path-missing-signals
  (fail (lis::asset-path "earth/no-such-texture.jpg") 'error)
  (true (probe-file (lis::asset-path "earth/blue-marble-5400.jpg"))))

(defun rgb (pixels row column)
  "Red, green and blue bytes of an ABGR8888 word."
  (let ((word (aref pixels row column)))
    (values (ldb (byte 8 0) word) (ldb (byte 8 8) word) (ldb (byte 8 16) word))))

(defvar *blue-marble* nil
  "The decoded Blue Marble, kept by blue-marble-decodes for the tests after it.")

(defun blue-marble ()
  (or *blue-marble*
      (setf *blue-marble* (lis::load-image-pixels (lis::asset-path "earth/blue-marble-5400.jpg")))))

(defvar *blue-marble-eighth* nil)

(defun blue-marble-eighth ()
  (or *blue-marble-eighth*
      (setf *blue-marble-eighth* (lis::downsample-pixels (blue-marble) lis::*earth-coarse-factor*))))

(defun share-earth-pixels ()
  "The canvases the tests open take the one decode and its copy, ~0.23 s
each (task 258: Bring scripts/test back under its ~15 s budget)."
  (setf lis::*earth-pixels* (cons (blue-marble) (blue-marble-eighth))
        lis::*earth-night-pixels* (cons (black-marble) (black-marble-eighth))))

(define-test blue-marble-decodes
  (let* ((start (get-internal-real-time))
         (pixels (setf *blue-marble*
                       (lis::load-image-pixels (lis::asset-path "earth/blue-marble-5400.jpg"))))
         (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (true (< seconds 1) "decoding took ~,2F s" seconds)
    (is equal '(2700 5400) (array-dimensions pixels))
    (is equal '(unsigned-byte 32) (array-element-type pixels))
    ;; 0° N 0° E, Gulf of Guinea: sea.
    (multiple-value-bind (r g b) (rgb pixels 1350 2700)
      (declare (ignore g))
      (true (> b r) "Gulf of Guinea red ~D blue ~D" r b))
    ;; 23° N 10° E: Sahara.
    (multiple-value-bind (r g b) (rgb pixels 1005 2850)
      (declare (ignore g))
      (true (> r b) "Sahara red ~D blue ~D" r b))
    ;; Five rows above the bottom: Antarctica's summer ice. The Arctic is
    ;; dark ocean in this January map, so the top rows are not white.
    (dotimes (channel 3)
      (let ((mean (/ (loop for column below 5400
                           sum (nth channel (multiple-value-list (rgb pixels 2694 column))))
                     5400.0)))
        (true (> mean 150) "row 2694 channel ~D mean ~,1F" channel mean)))))

(defun channel-means (pixels)
  (declare (type (array (unsigned-byte 32) (* *)) pixels))
  (let ((r 0) (g 0) (b 0) (n (array-total-size pixels)))
    (declare (type fixnum r g b n))
    (dotimes (i n)
      (let ((word (row-major-aref pixels i)))
        (incf r (ldb (byte 8 0) word))
        (incf g (ldb (byte 8 8) word))
        (incf b (ldb (byte 8 16) word))))
    (mapcar (lambda (s) (/ s (float n 1d0))) (list r g b))))

(define-test blue-marble-eighth
  ;; The far Earth's copy (decision 78: Sky in numbers: stars, Sun, Moon
  ;; and Earth's look): 2700 / 8 rounded up, its means the full map's.
  (let* ((pixels (blue-marble))
         (coarse (blue-marble-eighth)))
    (is = 8 lis::*earth-coarse-factor*)
    (is equal '(338 675) (array-dimensions coarse))
    (loop for full in (channel-means pixels)
          for small in (channel-means coarse)
          for channel from 0
          do (true (< (abs (- full small)) 1) "channel ~D: ~,2F, the copy ~,2F" channel full small))
    ;; A block's mean, by hand.
    (let ((sum 0))
      (dotimes (y 8) (dotimes (x 8) (incf sum (ldb (byte 8 16) (aref pixels (+ 800 y) (+ 1600 x))))))
      (is = (round sum 64) (ldb (byte 8 16) (aref coarse 100 200))))))

;;; Earth's night lights and clouds (decision 130: Earth's night lights and
;;; clouds in numbers; scripts/convert-earth).

(defvar *black-marble* nil)
(defvar *black-marble-eighth* nil)
(defvar *clouds* nil)

(defun black-marble ()
  (or *black-marble*
      (setf *black-marble* (lis::load-image-pixels (lis::asset-path "earth/black-marble-5400.jpg")))))

(defun clouds ()
  (or *clouds*
      (setf *clouds* (lis::load-image-pixels (lis::asset-path "earth/clouds-5400.jpg")))))

(defun texel (latitude longitude)
  "The (row column) of the equirectangular 5400 x 2700 map at LATITUDE and LONGITUDE (degrees)."
  (values (min 2699 (floor (* (- 90 latitude) 15)))
          (min 5399 (floor (* (+ 180 longitude) 15)))))

(defun file-bytes (name)
  (with-open-file (s (lis::asset-path name)) (file-length s)))

(define-test earth-layer-files
  (dolist (name '("earth/black-marble-5400.jpg" "earth/clouds-5400.jpg"))
    (true (<= (file-bytes name) 5000000) "~A is ~D bytes" name (file-bytes name)))
  (is equal '(2700 5400) (array-dimensions (black-marble)))
  (is equal '(2700 5400) (array-dimensions (clouds)))
  (let ((total (loop for f in (directory (merge-pathnames "**/*.*" (lis::asset-path "")))
                     sum (with-open-file (s f :element-type '(unsigned-byte 8)) (file-length s)))))
    (true (<= total 25000000) "assets/ is ~D bytes" total)))

(defun block-max-sum (pixels row column)
  "The largest R+G+B in the 3 x 3 block at ROW, COLUMN."
  (loop for dy from -1 to 1
        maximize (loop for dx from -1 to 1
                      maximize (multiple-value-bind (r g b) (rgb pixels (+ row dy) (+ column dx))
                                (+ r g b)))))

(defun black-marble-eighth ()
  (or *black-marble-eighth*
      (setf *black-marble-eighth*
            (lis::downsample-pixels (black-marble) lis::*earth-coarse-factor*))))

(define-test black-marble-night
  (let ((pixels (black-marble)))
    ;; Lit: the Nile delta, the Po valley.
    (loop for (name lat lon) in '(("Nile delta" 30.0 31.2) ("Po valley" 45.2 9.5))
          do (multiple-value-bind (row column) (texel lat lon)
               (true (> (block-max-sum pixels row column) 200) "~A: ~D" name
                     (block-max-sum pixels row column))))
    ;; Dark after the background's removal: the Sahara, the open Pacific.
    (loop for (name lat lon) in '(("Sahara" 23.0 10.0) ("Pacific" 0.0 -150.0))
          do (multiple-value-bind (row column) (texel lat lon)
               (multiple-value-bind (r g b) (rgb pixels row column)
                 (true (and (<= r 2) (<= g 2) (<= b 2)) "~A: ~D ~D ~D" name r g b))))))

(defun coarse-mean (pixels lat0 lat1 lon0 lon1)
  "Mean R byte of the 1/8 copy PIXELS (338 x 675) between the latitudes and longitudes (degrees)."
  (let ((row0 (floor (* (- 90 lat1) 1.875))) (row1 (floor (* (- 90 lat0) 1.875)))
        (col0 (floor (* (+ 180 lon0) 1.875))) (col1 (min 675 (floor (* (+ 180 lon1) 1.875))))
        (sum 0) (n 0))
    (loop for row from row0 below row1
          do (loop for column from col0 below col1
                   do (incf sum (ldb (byte 8 0) (aref pixels row column)))
                      (incf n)))
    (/ sum n 1.0)))

(define-test clouds-layer
  (let* ((eighth (lis::downsample-pixels (clouds) lis::*earth-coarse-factor*))
         (ocean (coarse-mean eighth -60 -50 -180 180))
         (sahara (coarse-mean eighth 15 30 0 30)))
    (true (> (- ocean sahara) 50) "Southern Ocean ~,1F, Sahara ~,1F" ocean sahara)))

(define-test merge-alpha
  (let* ((rgb (make-array '(2 2) :element-type '(unsigned-byte 32)
                                 :initial-contents '((#xff112233 #x00445566) (#x80ffffff #x00000000))))
         (cloud (make-array '(2 2) :element-type '(unsigned-byte 32)
                                   :initial-contents '((#xff0000aa #xff0000bb) (#xff0000cc #xff0000dd))))
         (merged (lis::merge-alpha-pixels rgb cloud)))
    (is = #xaa112233 (aref merged 0 0))
    (is = #xbb445566 (aref merged 0 1))
    (is = #xccffffff (aref merged 1 0))
    (is = #xdd000000 (aref merged 1 1))
    (is = #xff112233 (aref rgb 0 0))))

;;; The star table (decision 13; plan 17 piece 1).

(defun star-index (table hip)
  (position hip (lis::star-table-hip table)))

(defun star-direction (table i)
  (let ((d (lis::star-table-directions table)))
    (list (aref d (* 3 i)) (aref d (+ (* 3 i) 1)) (aref d (+ (* 3 i) 2)))))

(defun degrees-between (u v)
  (let ((dot (reduce #'+ (mapcar (lambda (a b) (* (float a 1d0) b)) u v))))
    (* (/ 180 pi) (acos (max -1d0 (min 1d0 dot))))))

(defun radec-direction (ra dec)
  (let ((a (* ra (/ pi 180))) (d (* dec (/ pi 180))))
    (list (* (cos d) (cos a)) (* (cos d) (sin a)) (sin d))))

(define-test star-table-loads
  (let* ((path (lis::asset-path "stars/hyg-6.5.sexp"))
         (start (get-internal-real-time))
         (table (lis::load-star-table path))
         (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0))
         (n (lis::star-table-count table))
         (magnitudes (lis::star-table-magnitudes table)))
    (true (< seconds 0.5) "loading took ~,3F s" seconds)
    (true (< (with-open-file (s path) (file-length s)) 1000000))
    (true (<= 8000 n 10000) "~D stars" n)
    (is = n (length magnitudes))
    (true (every (lambda (m) (<= m 6.5)) magnitudes))
    ;; Sirius (HIP 32349) the only star brighter than -1.5 + 0.02; no Sun.
    (let ((sirius (star-index table 32349)))
      (true sirius)
      (is = 1 (count-if (lambda (m) (< m -1.0)) magnitudes))
      (true (< (abs (- (aref magnitudes sirius) -1.44)) 0.02))
      (true (< (degrees-between (star-direction table sirius)
                                (radec-direction 101.287d0 -16.716d0))
               0.01)))
    (true (every (lambda (m) (> m -1.5)) magnitudes) "no Sun at -26.7")
    ;; Polaris (HIP 11767) near the ICRF pole.
    (let ((polaris (star-index table 11767)))
      (true (< (degrees-between (star-direction table polaris) '(0d0 0d0 1d0)) 0.75)))
    (let ((bad (loop for i below n
                     for length = (sqrt (reduce #'+ (mapcar (lambda (c) (* (float c 1d0) c))
                                                            (star-direction table i))))
                     count (>= (abs (- length 1)) 1d-6))))
      (is = 0 bad "~D directions not unit" bad))
    ;; Stars without a colour index are flagged, not given a fake one.
    (true (< 0 (count 0 (lis::star-table-coloured table)) 100))))

(define-test star-table-rejects-malformed
  (uiop:with-temporary-file (:pathname path :stream s :direction :output)
    (format s "(1 2.0 3.0 4.0 nil)~%(1 2.0 3.0)~%")
    (finish-output s)
    (fail (lis::load-star-table path) 'error))
  (uiop:with-temporary-file (:pathname path :stream s :direction :output)
    (format s "(1 #.(error \"read-eval\") 3.0 4.0 nil)~%")
    (finish-output s)
    (fail (lis::load-star-table path) 'error)))
