(in-package #:luv-in-space.tests)

;;; stars.lisp: the B-V colours, the brightness slope through the tone map,
;;; the instances (decision 78: Sky in numbers: stars, Sun, Moon and Earth's
;;; look). The GPU checks are in tests/render.lisp.

(defun chromaticity (rgb)
  "CIE xy of linear sRGB (IEC 61966-2-1)."
  (destructuring-bind (r g b) rgb
    (let ((x (+ (* 0.4124 r) (* 0.3576 g) (* 0.1805 b)))
          (y (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))
          (z (+ (* 0.0193 r) (* 0.1192 g) (* 0.9505 b))))
      (values (/ x (+ x y z)) (/ y (+ x y z))))))

(define-test star-colours
  (let ((table (lis::star-colour-table)))
    ;; The Sun's B-V: white within 0.03 in chromaticity (D65 0.3127, 0.3290).
    (multiple-value-bind (x y) (chromaticity (lis::star-colour table 0.65d0))
      (true (< (sqrt (+ (expt (- x 0.3127) 2) (expt (- y 0.3290) 2))) 0.03)
            "B-V 0.65 at x ~,4F y ~,4F" x y))
    (flet ((blue-red (bv)
             (let ((c (lis::star-colour table bv))) (/ (third c) (first c)))))
      (true (> (blue-red -0.3d0) 1 (blue-red 1.5d0))))
    (loop for bv from -0.4d0 to 2.0d0 by 0.05d0
          do (let ((l (apply #'lis::srgb-luminance (lis::star-colour table bv))))
               (true (< (abs (- l 1)) 1d-6) "B-V ~,2F luminance ~F" bv l)))
    (is = 5778 (round (lis::bv-temperature 0.65d0)))))

(defun star-byte (magnitude exposure)
  (lis::tone-map-srgb (* lis::*star-radiance-limit* (expt 10d0 (* 0.2d0 (- 6.5d0 magnitude))))
                      exposure))

(define-test star-brightness
  ;; decision 78's slope: mag 6.5 shows at the deep exposure; at the sunlit
  ;; one the stars of mag 3 and fainter are gone, the brightest dim
  ;; (decision 95: Star fading at the sunlit exposure as built).
  (true (>= (star-byte 6.5d0 lis::*exposure-deep*) 20))
  (true (< (star-byte 3.0d0 lis::*exposure-sunlit*) 8))
  (true (< (star-byte 1.0d0 lis::*exposure-sunlit*) 24))
  (true (< (star-byte -1.44d0 lis::*exposure-sunlit*) 80)))

(define-test star-instances
  (let* ((stars (lis::load-star-table))
         (data (lis::star-instances stars)))
    (is = (* lis::+star-instance-floats+ (lis::star-table-count stars)) (length data))
    ;; The first row is Sirius: brightness 10^(0.2 x 7.94), excess 4.44.
    (let ((light (lis::srgb-luminance (aref data 4) (aref data 5) (aref data 6))))
      (true (< (abs (- light (expt 10 (* 0.2 (- 6.5 (aref (lis::star-table-magnitudes stars) 0))))))
               1d-3)))
    (true (< (abs (- (aref data 3) 4.44)) 0.03))))

(define-test tone-map-srgb
  (is = 0 (lis::tone-map-srgb 0 16d0))
  (is = 255 (lis::tone-map-srgb 100 1d0))
  (is = 0 (lis::tone-map-srgb -1 1d0))
  ;; ACES(1) = 0.8038, sRGB 0.9101.
  (is = 232 (lis::tone-map-srgb 1d0 1d0)))
