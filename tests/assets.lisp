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

(define-test blue-marble-decodes
  (let* ((start (get-internal-real-time))
         (pixels (lis::load-image-pixels (lis::asset-path "earth/blue-marble-5400.jpg")))
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
  (let ((sums (list 0 0 0)) (n (array-total-size pixels)))
    (dotimes (i n)
      (let ((word (row-major-aref pixels i)))
        (dotimes (k 3) (incf (nth k sums) (ldb (byte 8 (* 8 k)) word)))))
    (mapcar (lambda (s) (/ s (float n 1d0))) sums)))

(define-test blue-marble-eighth
  ;; The far Earth's copy (decision 78: Sky in numbers: stars, Sun, Moon
  ;; and Earth's look): 2700 / 8 rounded up, its means the full map's.
  (let* ((pixels (lis::load-image-pixels (lis::asset-path "earth/blue-marble-5400.jpg")))
         (coarse (lis::downsample-pixels pixels 8)))
    (is equal '(338 675) (array-dimensions coarse))
    (loop for full in (channel-means pixels)
          for small in (channel-means coarse)
          for channel from 0
          do (true (< (abs (- full small)) 1) "channel ~D: ~,2F, the copy ~,2F" channel full small))
    ;; A block's mean, by hand.
    (let ((sum 0))
      (dotimes (y 8) (dotimes (x 8) (incf sum (ldb (byte 8 16) (aref pixels (+ 800 y) (+ 1600 x))))))
      (is = (round sum 64) (ldb (byte 8 16) (aref coarse 100 200))))))

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
