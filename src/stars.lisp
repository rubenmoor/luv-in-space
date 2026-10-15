(in-package #:luv-in-space)

;;; The stars at infinity (decision 78: Sky in numbers: stars, Sun, Moon and
;;; Earth's look; docs/luv-capabilities.md "Starfield"): the HYG table as one
;;; static instance buffer, drawn first in the scene pass as additive
;;; Gaussian quads at clip z 0.

(defparameter *star-radiance-limit* 1.25e-3
  "Peak radiance of a mag 6.5 star; live, a uniform.")
(defparameter *star-sigma* 0.7 "Gaussian sigma in pixels of stars of mag 3.0 and fainter.")
(defparameter *star-sigma-per-magnitude* 0.15 "Added per magnitude brighter than 3.0.")
(defparameter *star-sigma-max* 1.6)
(defparameter *star-saturation* 0.6
  "0 white, 1 the blackbody's colour; applied when the renderer builds the buffer.")

(defconstant +star-instance-floats+ 7
  "Direction xyz, magnitudes brighter than 3.0, colour rgb x 10^(0.2 (6.5 - m)).
luv's vertex formats have no scalar, so the magnitudes ride in the direction's w.")

;;; B-V to linear sRGB

(defconstant +colour-table-size+ 64)
(defconstant +colour-bv-min+ -0.4d0)
(defconstant +colour-bv-max+ 2.0d0)

(defun bv-temperature (bv)
  "Kelvin from B-V: Ballesteros (2012), EPL 97, 34008, arXiv:1201.1809."
  (* 4600 (+ (/ (+ (* 0.92d0 bv) 1.7d0)) (/ (+ (* 0.92d0 bv) 0.62d0)))))

(defun cie-1931 (nm)
  "x, y, z of the CIE 1931 2-degree observer at NM: Wyman, Sloan, Shirley
(2013), JCGT 2(2), multi-lobe fit, Listing 1."
  (flet ((lobe (mu below above)
           (let ((u (* (- nm mu) (if (< nm mu) below above))))
             (exp (* -0.5d0 u u)))))
    (values (+ (* 0.362d0 (lobe 442.0d0 0.0624d0 0.0374d0))
               (* 1.056d0 (lobe 599.8d0 0.0264d0 0.0323d0))
               (* -0.065d0 (lobe 501.1d0 0.0490d0 0.0382d0)))
            (+ (* 0.821d0 (lobe 568.8d0 0.0213d0 0.0247d0))
               (* 0.286d0 (lobe 530.9d0 0.0613d0 0.0322d0)))
            (+ (* 1.217d0 (lobe 437.0d0 0.0845d0 0.0278d0))
               (* 0.681d0 (lobe 459.0d0 0.0385d0 0.0725d0))))))

(defun planck (nm kelvin)
  "Spectral radiance of a blackbody, up to a constant factor."
  (let ((m (* nm 1d-9)))
    (/ 1 (* (expt m 5) (- (exp (/ 1.438776877d-2 (* m kelvin))) 1)))))

(defun xyz-linear-srgb (x y z)
  "IEC 61966-2-1:1999/Amd 1:2003, D65."
  (values (+ (* 3.2406255d0 x) (* -1.5372080d0 y) (* -0.4986286d0 z))
          (+ (* -0.9689307d0 x) (* 1.8757561d0 y) (* 0.0415175d0 z))
          (+ (* 0.0557101d0 x) (* -0.2040211d0 y) (* 1.0569959d0 z))))

(defun srgb-luminance (r g b)
  (+ (* 0.2126d0 r) (* 0.7152d0 g) (* 0.0722d0 b)))

(defun blackbody-colour (kelvin)
  "Linear sRGB of a blackbody at KELVIN, clipped at 0, at luminance 1."
  (let ((x 0d0) (y 0d0) (z 0d0))
    (loop for nm from 380d0 to 780d0 by 1d0
          for p = (planck nm kelvin)
          do (multiple-value-bind (a b c) (cie-1931 nm)
               (incf x (* p a)) (incf y (* p b)) (incf z (* p c))))
    (multiple-value-bind (r g b) (xyz-linear-srgb x y z)
      (let* ((r (max r 0d0)) (g (max g 0d0)) (b (max b 0d0))
             (l (srgb-luminance r g b)))
        (list (/ r l) (/ g l) (/ b l))))))

(defun star-colour-table (&optional (saturation *star-saturation*))
  "+colour-table-size+ colours over B-V from +colour-bv-min+ to
+colour-bv-max+, mixed toward white by 1 - SATURATION; luminance 1."
  (let ((table (make-array (* 3 +colour-table-size+) :element-type 'double-float)))
    (dotimes (i +colour-table-size+ table)
      (let ((bv (+ +colour-bv-min+ (* i (/ (- +colour-bv-max+ +colour-bv-min+)
                                           (1- +colour-table-size+))))))
        (loop for c in (blackbody-colour (bv-temperature bv))
              for k from 0
              do (setf (aref table (+ (* 3 i) k))
                       (+ (- 1 saturation) (* saturation c))))))))

(defun star-colour (table bv)
  "The colour of B-V from TABLE, linear between entries, clamped at its ends."
  (let* ((f (* (/ (- (max +colour-bv-min+ (min +colour-bv-max+ bv)) +colour-bv-min+)
                  (- +colour-bv-max+ +colour-bv-min+))
               (1- +colour-table-size+)))
         (i (min (floor f) (- +colour-table-size+ 2)))
         (s (- f i)))
    (loop for k below 3
          collect (+ (* (- 1 s) (aref table (+ (* 3 i) k)))
                     (* s (aref table (+ (* 3 (1+ i)) k)))))))

;;; The instances

(defun star-instances (stars &optional (table (star-colour-table)))
  "Per star of the star-table STARS: direction, max(0, 3 - m), colour at
luminance 1 x 10^(0.2 (6.5 - m)) (half Pogson's slope; the shader
multiplies by *star-radiance-limit*). No colour index: white."
  (let* ((n (star-table-count stars))
         (out (make-array (* +star-instance-floats+ n) :element-type 'single-float)))
    (dotimes (i n out)
      (let* ((m (aref (star-table-magnitudes stars) i))
             (brightness (expt 10d0 (* 0.2d0 (- 6.5d0 m))))
             (colour (if (= 1 (sbit (star-table-coloured stars) i))
                         (star-colour table (aref (star-table-colour-indices stars) i))
                         '(1d0 1d0 1d0)))
             (base (* +star-instance-floats+ i)))
        (replace out (star-table-directions stars) :start1 base :start2 (* 3 i) :end2 (* 3 (1+ i)))
        (loop for c in colour
              for k from 4
              do (setf (aref out (+ base k)) (coerce (* c brightness) 'single-float)))
        (setf (aref out (+ base 3)) (coerce (max 0d0 (- 3d0 m)) 'single-float))))))

(defun star-corners ()
  "Two triangles of a quad, corners at -1 and +1."
  (coerce '(-1.0 -1.0 1.0 -1.0 1.0 1.0 -1.0 -1.0 1.0 1.0 -1.0 1.0)
          '(simple-array single-float (*))))

(defun star-uniforms! (data index)
  "The frame block's stars vec4 at INDEX: radiance limit, sigma, per magnitude, max."
  (setf (aref data index) (coerce *star-radiance-limit* 'single-float)
        (aref data (+ index 1)) (coerce *star-sigma* 'single-float)
        (aref data (+ index 2)) (coerce *star-sigma-per-magnitude* 'single-float)
        (aref data (+ index 3)) (coerce *star-sigma-max* 'single-float))
  data)

(defun stars-pipeline (renderer layout format)
  "The :stars live pipeline: corners per vertex, stars per instance, into
the HDR target of FORMAT; additive (premultiplied, alpha 0), no depth test or write."
  (live-pipeline renderer :stars layout
                 `((:array-stride 8
                    :attributes ((:shader-location 0 :offset 0 :format :float32x2)))
                   (:array-stride ,(* 4 +star-instance-floats+) :step-mode :instance
                    :attributes ((:shader-location 1 :offset 0 :format :float32x4)
                                 (:shader-location 2 :offset 16 :format :float32x3))))
                 :format format :blend :premultiplied-alpha
                 :depth-stencil '(:format :depth32-float :depth-write-enabled nil
                                  :depth-compare :always)))
