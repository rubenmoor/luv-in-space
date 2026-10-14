(in-package #:luv-in-space)

;;; The Sun, Earth and Moon: constants, rotations and body-fixed frames
;;; (docs/architecture.md "Frames"), ephemerides and their Hermite cache
;;; ("Ephemerides", decision 18: Ephemerides, decision 67: Ephemeris cache
;;; and the Moon's series frame).

(defstruct (body (:constructor %make-body))
  (key nil :type keyword :read-only t)
  (name "" :type string :read-only t)
  (mu 0d0 :type double-float :read-only t)
  (radius 0d0 :type double-float :read-only t)
  (crash-radius 0d0 :type double-float :read-only t)  ; decision 34: How death works
  (parent nil :type (or null keyword) :read-only t)
  (semi-major-axis nil :type (or null double-float) :read-only t) ; mean, about the parent
  (soi-radius nil :type (or null double-float) :read-only t)      ; Laplace sphere
  (j2 0d0 :type double-float :read-only t))

(defun laplace-sphere (a mu parent-mu)
  "r = a (m/M)^(2/5)."
  (* a (expt (/ mu parent-mu) 0.4d0)))

(defun make-body (key name mu radius crash-radius &key parent semi-major-axis (j2 0d0))
  (%make-body :key key :name name :mu mu :radius radius :crash-radius crash-radius
              :parent parent :semi-major-axis semi-major-axis :j2 j2
              :soi-radius (and parent
                               (laplace-sphere semi-major-axis mu
                                               (ecase parent
                                                 (:sun +mu-sun+)
                                                 (:earth +mu-earth+))))))

(defparameter *bodies*
  (list (make-body :sun "Sun" +mu-sun+ +radius-sun+ +radius-sun+)
        (make-body :earth "Earth" +mu-earth+ +radius-earth+ (+ +radius-earth+ 100d3)
                   :parent :sun
                   :semi-major-axis (* 1.00000261d0 +au+) ; EMB, Standish, Table 1
                   :j2 +j2-earth+)
        (make-body :moon "Moon" +mu-moon+ +radius-moon+ +radius-moon+
                   :parent :earth
                   :semi-major-axis 3.84399d8))           ; Simon et al. 1994, NASA Moon fact sheet
  "Every body, in a fixed order (parents first).")

(defun body (designator)
  "The body of a key, or the body itself."
  (if (body-p designator)
      designator
      (or (find designator *bodies* :key #'body-key)
          (error "No body ~S." designator))))

;;; Rotations. A body-fixed matrix maps body-centred inertial (ICRF) axes
;;; to body-fixed ones.

(defun passive-rz (angle)
  (let ((c (cos angle)) (s (sin angle)))
    (mat3 c s 0 (- s) c 0 0 0 1)))

(defun passive-rx (angle)
  (let ((c (cos angle)) (s (sin angle)))
    (mat3 1 0 0 0 c s 0 (- s) c)))

(defun earth-rotation-revolutions (time)
  "Earth Rotation Angle in revolutions, [0, 1) (IERS Conventions 2010):
0.7790572732640 + 1.00273781191135448 D_U, the whole days of D_U dropped
before they are multiplied."
  (declare (type double-float time))
  (let ((d (t->ut1-days time)))
    (mod (+ (nth-value 1 (ffloor d)) 0.7790572732640d0 (* 0.00273781191135448d0 d))
         1d0)))

(defun earth-rotation-angle (time)
  "θ in radians, [0, 2π)."
  (* 2 pi (earth-rotation-revolutions time)))

(defun iau-body-fixed-matrix (alpha0 delta0 w)
  "R_z(W) R_x(90° − δ₀) R_z(90° + α₀) from the IAU pole and prime meridian,
in degrees (Archinal et al. 2018)."
  (mat3* (passive-rz (deg->rad w))
         (mat3* (passive-rx (deg->rad (- 90 delta0)))
                (passive-rz (deg->rad (+ 90 alpha0))))))

(defun body-fixed-matrix (body time)
  "Inertial -> body-fixed rotation of BODY at TIME; IAU WGCCRE 2015 leading
terms (Archinal et al. 2018), no periodic terms."
  (declare (type double-float time))
  (let* ((d (/ time +day+))
         (centuries (t->julian-centuries time)))
    (ecase (body-key (body body))
      (:earth (passive-rz (earth-rotation-angle time)))
      (:moon (iau-body-fixed-matrix (+ 269.9949d0 (* 0.0031d0 centuries))
                                    (+ 66.5392d0 (* 0.0130d0 centuries))
                                    (mod (+ 38.3213d0 (* 13.17635815d0 d) (* -1.4d-12 d d))
                                         360d0)))
      (:sun (iau-body-fixed-matrix 286.13d0 63.87d0 (mod (+ 84.176d0 (* 14.1844000d0 d)) 360d0))))))

(defun latitude-longitude (body r time)
  "(values latitude longitude) in radians of a body-centred inertial position;
longitude east, in (−π, π]."
  (let ((fixed (mat3*vec3 (body-fixed-matrix body time) r)))
    (with-vec3 (((x y z) fixed))
      (values (asin (/ z (vec3-norm fixed))) (atan y x)))))

(defun body-texture-uv (direction)
  "(values u v) of a body-fixed direction on an equirectangular map with the
prime meridian in the centre column and north at the top."
  (with-vec3 (((x y z) direction))
    (values (+ 0.5d0 (/ (atan y x) (* 2 pi)))
            (- 0.5d0 (/ (asin (/ z (vec3-norm direction))) pi)))))

;;; Ephemerides: the series. Positions in m, velocities in m/s, ICRF axes.

(defconstant +arcsec+ (/ pi 648000))

(defun ecliptic->icrf! (out v)
  "Rotate a J2000 ecliptic vector to ICRF by the obliquity, about x."
  (declare (type vec3 out v))
  (let ((c (cos +obliquity+)) (s (sin +obliquity+)))
    (with-vec3 (((x y z) v))
      (vec3-set! out x (- (* c y) (* s z)) (+ (* s y) (* c z))))))

(defun rotate-about-axis-rate! (r v axis angle rate)
  "Rotate R by ANGLE about AXIS (0 x, 2 z) in place and V with it, adding
the rate's rate × r to V: d/dt (R(angle) r) = R(angle) ṙ + rate (axis × R r)."
  (declare (type vec3 r v) (type (integer 0 2) axis) (type double-float angle rate))
  (let ((c (cos angle)) (s (sin angle)))
    (flet ((rot! (u)
             (with-vec3 (((x y z) u))
               (if (= axis 0)
                   (vec3-set! u x (- (* c y) (* s z)) (+ (* s y) (* c z)))
                   (vec3-set! u (- (* c x) (* s y)) (+ (* s x) (* c y)) z)))))
      (rot! r)
      (rot! v)
      (with-vec3 (((x y z) r))
        (if (= axis 0)
            (vec3-add-scaled! v v (vec3 0 (- z) y) rate)
            (vec3-add-scaled! v v (vec3 (- y) x 0) rate))))))

;; Standish, "Keplerian Elements for Approximate Positions of the Major
;; Planets" (JPL SSD), Table 1, EM Bary: value at J2000 and rate per Julian
;; century; a in au, angles in degrees: a e I L ϖ Ω.
(defparameter *emb-elements*
  '((1.00000261d0 0.00000562d0) (0.01671123d0 -0.00004392d0)
    (-0.00001531d0 -0.01294668d0) (100.46457166d0 35999.37244981d0)
    (102.93768193d0 0.32327364d0) (0d0 0d0)))

(defun solve-kepler (mean-anomaly e)
  "E with E − e sin E = M, by Newton from M + e sin M, to 1e-14 rad."
  (declare (type double-float mean-anomaly e))
  (let ((ea (+ mean-anomaly (* e (sin mean-anomaly)))))
    (declare (type double-float ea))
    (loop repeat 50
          for step = (/ (- ea (* e (sin ea)) mean-anomaly) (- 1 (* e (cos ea))))
          do (decf ea step)
          until (< (abs step) 1d-14))
    ea))

(defun standish-state (elements time)
  "(values r v) heliocentric ICRF of a Standish Table 1 element set; v is
the exact derivative of r, element rates included."
  (declare (type double-float time))
  (let* ((century (* +day+ +julian-century+))
         (centuries (/ time century)))
    (flet ((value (i) (destructuring-bind (x0 rate) (nth i elements)
                        (+ x0 (* rate centuries))))
           (rate (i) (/ (second (nth i elements)) century)))
      (let* ((a (* +au+ (value 0))) (da (* +au+ (rate 0)))
             (e (value 1)) (de (rate 1))
             (inc (deg->rad (value 2))) (dinc (deg->rad (rate 2)))
             (node (deg->rad (value 5))) (dnode (deg->rad (rate 5)))
             (peri (deg->rad (value 4))) (dperi (deg->rad (rate 4)))
             (omega (- peri node)) (domega (- dperi dnode))
             (m (- (mod (- (value 3) (value 4) -180d0) 360d0) 180d0))
             (dm (deg->rad (- (rate 3) (rate 4))))
             (ea (solve-kepler (deg->rad m) e))
             (dea (/ (+ dm (* de (sin ea))) (- 1 (* e (cos ea)))))
             (b (sqrt (- 1 (* e e))))
             (db (/ (* -1 e de) b))
             (r (vec3 (* a (- (cos ea) e)) (* a b (sin ea)) 0))
             (v (vec3 (+ (* da (- (cos ea) e)) (* a (- (* -1 (sin ea) dea) de)))
                      (+ (* da b (sin ea)) (* a db (sin ea)) (* a b (cos ea) dea))
                      0)))
        (rotate-about-axis-rate! r v 2 omega domega)
        (rotate-about-axis-rate! r v 0 inc dinc)
        (rotate-about-axis-rate! r v 2 node dnode)
        (values (ecliptic->icrf! r r) (ecliptic->icrf! v v))))))

(defun emb-series (time)
  "(values r v) of the Earth–Moon barycentre from the Sun."
  (standish-state *emb-elements* time))

;; Meeus, "Astronomical Algorithms", 2nd ed., ch. 47, table 47.A: multiples
;; of D M M' F, Σl in 1e-6°, Σr in m.
(defparameter *moon-lr-terms*
  #((0 0 1 0 6288774 -20905355)
    (2 0 -1 0 1274027 -3699111)
    (2 0 0 0 658314 -2955968)
    (0 0 2 0 213618 -569925)
    (0 1 0 0 -185116 48888)
    (0 0 0 2 -114332 -3149)
    (2 0 -2 0 58793 246158)
    (2 -1 -1 0 57066 -152138)
    (2 0 1 0 53322 -170733)
    (2 -1 0 0 45758 -204586)
    (0 1 -1 0 -40923 -129620)
    (1 0 0 0 -34720 108743)
    (0 1 1 0 -30383 104755)
    (2 0 0 -2 15327 10321)
    (0 0 1 2 -12528 0)
    (0 0 1 -2 10980 79661)
    (4 0 -1 0 10675 -34782)
    (0 0 3 0 10034 -23210)
    (4 0 -2 0 8548 -21636)
    (2 1 -1 0 -7888 24208)
    (2 1 0 0 -6766 30824)
    (1 0 -1 0 -5163 -8379)
    (1 1 0 0 4987 -16675)
    (2 -1 1 0 4036 -12831)
    (2 0 2 0 3994 -10445)
    (4 0 0 0 3861 -11650)
    (2 0 -3 0 3665 14403)
    (0 1 -2 0 -2689 -7003)
    (2 0 -1 2 -2602 0)
    (2 -1 -2 0 2390 10056)
    (1 0 1 0 -2348 6322)
    (2 -2 0 0 2236 -9884)
    (0 1 2 0 -2120 5751)
    (0 2 0 0 -2069 0)
    (2 -2 -1 0 2048 -4950)
    (2 0 1 -2 -1773 4130)
    (2 0 0 2 -1595 0)
    (4 -1 -1 0 1215 -3958)
    (0 0 2 2 -1110 0)
    (3 0 -1 0 -892 3258)
    (2 1 1 0 -810 2616)
    (4 -1 -2 0 759 -1897)
    (0 2 -1 0 -713 -2117)
    (2 2 -1 0 -700 2354)
    (2 1 -2 0 691 0)
    (2 -1 0 -2 596 0)
    (4 0 1 0 549 -1423)
    (0 0 4 0 537 -1117)
    (4 -1 0 0 520 -1571)
    (1 0 -2 0 -487 -1739)
    (2 1 0 -2 -399 0)
    (0 0 2 -2 -381 -4421)
    (1 1 1 0 351 0)
    (3 0 -2 0 -340 0)
    (4 0 -3 0 330 0)
    (2 -1 2 0 327 0)
    (0 2 1 0 -323 1165)
    (1 1 -1 0 299 0)
    (2 0 3 0 294 0)
    (2 0 -1 -2 0 8752)))

;; Table 47.B: multiples of D M M' F, Σb in 1e-6°.
(defparameter *moon-b-terms*
  #((0 0 0 1 5128122)
    (0 0 1 1 280602)
    (0 0 1 -1 277693)
    (2 0 0 -1 173237)
    (2 0 -1 1 55413)
    (2 0 -1 -1 46271)
    (2 0 0 1 32573)
    (0 0 2 1 17198)
    (2 0 1 -1 9266)
    (0 0 2 -1 8822)
    (2 -1 0 -1 8216)
    (2 0 -2 -1 4324)
    (2 0 1 1 4200)
    (2 1 0 -1 -3359)
    (2 -1 -1 1 2463)
    (2 -1 0 1 2211)
    (2 -1 -1 -1 2065)
    (0 1 -1 -1 -1870)
    (4 0 -1 -1 1828)
    (0 1 0 1 -1794)
    (0 0 0 3 -1749)
    (0 1 -1 1 -1565)
    (1 0 0 1 -1491)
    (0 1 1 1 -1475)
    (0 1 1 -1 -1410)
    (0 1 0 -1 -1344)
    (1 0 0 -1 -1335)
    (0 0 3 1 1107)
    (4 0 0 -1 1021)
    (4 0 -1 1 833)
    (0 0 1 -3 777)
    (4 0 -2 1 671)
    (2 0 0 -3 607)
    (2 0 2 -1 596)
    (2 -1 1 -1 491)
    (2 0 -2 1 -451)
    (0 0 3 -1 439)
    (2 0 2 1 422)
    (2 0 -3 -1 421)
    (2 1 -1 1 -366)
    (2 1 0 1 -351)
    (4 0 0 1 331)
    (2 -1 1 1 315)
    (2 -2 0 -1 302)
    (0 0 1 3 -283)
    (2 1 1 -1 -229)
    (1 1 0 -1 223)
    (1 1 0 1 223)
    (0 1 -2 -1 -220)
    (2 1 -1 -1 -220)
    (1 0 1 1 -185)
    (2 -1 -2 -1 181)
    (0 1 2 1 -177)
    (4 0 -2 -1 176)
    (4 -1 -1 -1 166)
    (1 0 1 -1 -164)
    (4 0 1 -1 132)
    (1 0 -1 -1 -119)
    (4 -1 0 -1 115)
    (2 -2 0 1 107)))

(defun moon-ecliptic-of-date (time)
  "(values λ β Δ) of the Moon, geocentric, mean geometric ecliptic and
equinox of date: λ and β in degrees, Δ in m (Meeus ch. 47, no nutation)."
  (declare (type double-float time))
  (let* ((c (t->julian-centuries time))
         (c2 (* c c)))
    (flet ((angle (&rest terms)
             (deg->rad (mod (loop for x in terms for k = 1d0 then (* k c)
                                  sum (* x k))
                            360d0))))
      (let* ((lp (angle 218.3164477d0 481267.88123421d0 -0.0015786d0
                        (/ 538841d0) (/ -65194000d0)))
             (d (angle 297.8501921d0 445267.1114034d0 -0.0018819d0
                       (/ 545868d0) (/ -113065000d0)))
             (m (angle 357.5291092d0 35999.0502909d0 -0.0001536d0 (/ 24490000d0)))
             (mp (angle 134.9633964d0 477198.8675055d0 0.0087414d0
                        (/ 69699d0) (/ -14712000d0)))
             (f (angle 93.2720950d0 483202.0175233d0 -0.0036539d0
                       (/ -3526000d0) (/ 863310000d0)))
             (a1 (angle 119.75d0 131.849d0))
             (a2 (angle 53.09d0 479264.290d0))
             (a3 (angle 313.45d0 481266.484d0))
             (e (- 1 (* 0.002516d0 c) (* 0.0000074d0 c2)))
             (sl 0d0) (sr 0d0) (sb 0d0))
        (declare (type double-float sl sr sb))
        (flet ((term (row)
                 (destructuring-bind (kd km kmp kf &rest coefficients) row
                   (values (+ (* kd d) (* km m) (* kmp mp) (* kf f))
                           (expt e (abs km))
                           coefficients))))
          (loop for row across *moon-lr-terms*
                do (multiple-value-bind (arg factor coefficients) (term row)
                     (incf sl (* factor (first coefficients) (sin arg)))
                     (incf sr (* factor (second coefficients) (cos arg)))))
          (loop for row across *moon-b-terms*
                do (multiple-value-bind (arg factor coefficients) (term row)
                     (incf sb (* factor (first coefficients) (sin arg))))))
        (incf sl (+ (* 3958 (sin a1)) (* 1962 (sin (- lp f))) (* 318 (sin a2))))
        (incf sb (+ (* -2235 (sin lp)) (* 382 (sin a3))
                    (* 175 (sin (- a1 f))) (* 175 (sin (+ a1 f)))
                    (* 127 (sin (- lp mp))) (* -115 (sin (+ lp mp)))))
        (values (+ (rad->deg lp) (* sl 1d-6))
                (* sb 1d-6)
                (+ 385000560d0 sr))))))

(defun precess-ecliptic (lambda beta from to)
  "(values λ β) in degrees, ecliptic coordinates of the equinox of Julian
centuries FROM carried to that of TO (Meeus eqs. 21.5 and 21.7)."
  (declare (type double-float lambda beta from to))
  (let* ((tt from) (dt (- to from))
         (eta (* +arcsec+ dt (+ (+ 47.0029d0 (* tt (+ -0.06603d0 (* 0.000598d0 tt))))
                                (* dt (+ -0.03302d0 (* 0.000598d0 tt) (* 0.00006d0 dt))))))
         (big-pi (+ (deg->rad 174.876384d0)
                    (* +arcsec+ (+ (* tt (+ 3289.4789d0 (* 0.60622d0 tt)))
                                   (* dt (+ (- (+ 869.8089d0 (* 0.50491d0 tt)))
                                            (* 0.03536d0 dt)))))))
         (p (* +arcsec+ dt (+ 5029.0966d0 (* tt (- 2.22226d0 (* 0.000042d0 tt)))
                              (* dt (- 1.11113d0 (* 0.000042d0 tt) (* 0.000006d0 dt))))))
         (l (deg->rad lambda)) (b (deg->rad beta))
         (a* (- (* (cos eta) (cos b) (sin (- big-pi l))) (* (sin eta) (sin b))))
         (b* (* (cos b) (cos (- big-pi l))))
         (c* (+ (* (cos eta) (sin b)) (* (sin eta) (cos b) (sin (- big-pi l))))))
    (values (rad->deg (- (+ p big-pi) (atan a* b*)))
            (rad->deg (asin c*)))))

(defun moon-position-series (time)
  "The Moon from Earth, ICRF, m: Meeus ch. 47 precessed to J2000."
  (declare (type double-float time))
  (multiple-value-bind (lambda beta distance) (moon-ecliptic-of-date time)
    (multiple-value-bind (lambda beta)
        (precess-ecliptic lambda beta (t->julian-centuries time) 0d0)
      (let ((l (deg->rad lambda)) (b (deg->rad beta)))
        (ecliptic->icrf! (make-vec3)
                         (vec3 (* distance (cos b) (cos l))
                               (* distance (cos b) (sin l))
                               (* distance (sin b))))))))

(defparameter *moon-velocity-half-step* 60d0
  "s; the Moon's velocity is the central difference over ±this.")

(defun moon-series (time)
  "(values r v) of the Moon from Earth."
  (declare (type double-float time))
  (let ((h *moon-velocity-half-step*))
    (values (moon-position-series time)
            (vec3-scale (vec3-sub (moon-position-series (+ time h))
                                  (moon-position-series (- time h)))
                        (/ 0.5d0 h)))))

;;; The cache (decision 67): nodes every +ephemeris-node-spacing+ s, each a
;;; pure function of its index; direct-mapped, 64 slots per quantity.
;;; Quantity 0 is the EMB from the Sun, 1 the Moon from Earth.

(defconstant +ephemeris-node-spacing+ 4096d0)
(defconstant +ephemeris-slots+ 64)

(defun make-ephemeris-cache ()
  (cons (make-array (* 2 +ephemeris-slots+) :element-type 'fixnum
                                             :initial-element most-negative-fixnum)
        (make-array (* 2 +ephemeris-slots+ 6) :element-type 'double-float
                                               :initial-element 0d0)))

(defvar *ephemeris-cache* (make-ephemeris-cache)
  "(node indices . node r v), a cache: dropping it changes no result.")

(defun clear-ephemeris-cache ()
  (setf *ephemeris-cache* (make-ephemeris-cache))
  (values))

(defun ephemeris-node-state (quantity index)
  "(values r v) of node INDEX of QUANTITY from the series."
  (let ((time (* index +ephemeris-node-spacing+)))
    (ecase quantity
      (0 (emb-series time))
      (1 (moon-series time)))))

(declaim (ftype (function ((integer 0 1) fixnum) (values fixnum &optional))
                ephemeris-node))
(defun ephemeris-node (quantity index)
  "The offset of node INDEX of QUANTITY in the cache's data, filling its
slot on a miss."
  (declare (optimize speed) (type (integer 0 1) quantity) (type fixnum index))
  (let* ((cache *ephemeris-cache*)
         (indices (car cache))
         (slot (+ (* quantity +ephemeris-slots+) (mod index +ephemeris-slots+)))
         (offset (* 6 slot)))
    (declare (type (simple-array fixnum (*)) indices))
    (unless (= (aref indices slot) index)
      (let ((data (cdr cache)))
        (declare (type (simple-array double-float (*)) data))
        (multiple-value-bind (r v) (ephemeris-node-state quantity index)
          (declare (type vec3 r v))
          (dotimes (i 3)
            (setf (aref data (+ offset i)) (aref r i)
                  (aref data (+ offset 3 i)) (aref v i))))
        (setf (aref indices slot) index)))
    offset))

(declaim (inline hermite-into))
(defun hermite-into (r-out v-out quantity time)
  "Cubic Hermite (values r v) of QUANTITY at TIME from its two nodes."
  (declare (optimize speed) (type vec3 r-out v-out) (type (integer 0 1) quantity)
           (type (double-float -1d12 1d12) time))
  (let* ((h +ephemeris-node-spacing+)
         (index (floor time h))
         (s (/ (- time (* index h)) h))
         (s2 (* s s)) (s3 (* s2 s))
         (p0 (ephemeris-node quantity index))
         (p1 (ephemeris-node quantity (1+ index)))
         (data (cdr *ephemeris-cache*))
         (h00 (+ (* 2 s3) (* -3 s2) 1d0)) (h10 (* h (+ s3 (* -2 s2) s)))
         (h01 (+ (* -2 s3) (* 3 s2)))     (h11 (* h (- s3 s2)))
         (d00 (/ (- (* 6 s2) (* 6 s)) h)) (d10 (+ (* 3 s2) (* -4 s) 1d0))
         (d11 (- (* 3 s2) (* 2 s))))
    (declare (type (simple-array double-float (*)) data))
    (dotimes (i 3)
      (let ((x0 (aref data (+ p0 i))) (v0 (aref data (+ p0 3 i)))
            (x1 (aref data (+ p1 i))) (v1 (aref data (+ p1 3 i))))
        (setf (aref r-out i) (+ (* h00 x0) (* h10 v0) (* h01 x1) (* h11 v1))
              (aref v-out i) (+ (* d00 (- x0 x1)) (* d10 v0) (* d11 v1)))))
    (values r-out v-out)))

;;; Bodies from origins. A body's heliocentric state is
;;; b EMB + (i + j f) Moon with f = μ_Moon / (μ_Earth + μ_Moon); a state
;;; relative to an origin takes the differences of the integer coefficients,
;;; so no two large values are subtracted (decision 67).

(defconstant +moon-mass-fraction+ (/ +mu-moon+ (+ +mu-earth+ +mu-moon+)))

(declaim (inline body-coefficients))
(defun body-coefficients (key)
  "(values b i j) of a body."
  (ecase key
    (:sun (values 0 0 0))
    (:earth (values 1 0 -1))
    (:moon (values 1 1 -1))))

(defun combine-into (r-out v-out key origin emb-r emb-v moon-r moon-v)
  (declare (optimize speed) (type vec3 r-out v-out emb-r emb-v moon-r moon-v))
  (multiple-value-bind (b i j) (body-coefficients key)
    (multiple-value-bind (ob oi oj) (body-coefficients origin)
      (let ((cb (float (- b ob) 1d0))
            (cm (+ (- i oi) (* (- j oj) +moon-mass-fraction+))))
        (dotimes (k 3)
          (setf (aref r-out k) (+ (* cb (aref emb-r k)) (* cm (aref moon-r k)))
                (aref v-out k) (+ (* cb (aref emb-v k)) (* cm (aref moon-v k)))))
        (values r-out v-out)))))

(declaim (ftype (function (vec3 vec3 keyword double-float keyword) (values vec3 vec3 &optional))
                body-state-into))
(defun body-state-into (r-out v-out key time origin)
  "Write the state of body KEY at TIME relative to ORIGIN (:sun, :earth or
:moon), ICRF axes, into R-OUT and V-OUT; read through the cache."
  (declare (optimize speed) (type vec3 r-out v-out) (type double-float time))
  (let ((emb-r (make-vec3)) (emb-v (make-vec3)) (moon-r (make-vec3)) (moon-v (make-vec3)))
    (declare (dynamic-extent emb-r emb-v moon-r moon-v))
    (unless (= (body-coefficients key) (body-coefficients origin))
      (hermite-into emb-r emb-v 0 time))
    (hermite-into moon-r moon-v 1 time)
    (combine-into r-out v-out key origin emb-r emb-v moon-r moon-v)))

(defun body-state (key time &optional (origin :earth))
  "(values r v) of body KEY at TIME relative to ORIGIN, from the cache."
  (body-state-into (make-vec3) (make-vec3) key (float time 1d0) origin))

(defun body-state-series (key time &optional (origin :earth))
  "body-state straight from the series, bypassing the cache."
  (let ((time (float time 1d0)))
    (multiple-value-bind (emb-r emb-v) (emb-series time)
      (multiple-value-bind (moon-r moon-v) (moon-series time)
        (combine-into (make-vec3) (make-vec3) key origin emb-r emb-v moon-r moon-v)))))
