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
