(in-package #:luv-in-space)

;;; SI constants with their sources, the calendar and display conversions
;;; (decision 14: Units and time epoch, decision 66: Calendar dates on the
;;; 1/64 s grid). t is TDB seconds since J2000 = 2000-01-01 12:00 TT;
;;; TDB = TT, UT1 = UTC = TT - 69.184 s.

;;; Gravitational parameters, m^3/s^2
(defconstant +mu-sun+ 1.32712440041279419d20)   ; DE440, Park et al. 2021, AJ 161:105
(defconstant +mu-earth+ 3.98600435507d14)       ; DE440, Park et al. 2021
(defconstant +mu-moon+ 4.902800118d12)          ; DE440, Park et al. 2021

;;; Radii, m
(defconstant +radius-sun+ 6.957d8)              ; IAU 2015 Resolution B3, nominal
(defconstant +radius-earth+ 6378136.6d0)        ; IAU WGCCRE 2015 (Archinal et al. 2018), equatorial
(defconstant +radius-moon+ 1737400d0)           ; IAU WGCCRE 2015 (Archinal et al. 2018), mean

(defconstant +j2-earth+ 1.08263d-3)             ; EGM2008, unnormalised C20 = -J2
(defconstant +g0+ 9.80665d0)                    ; standard gravity, m/s^2, CGPM 1901
(defconstant +obliquity+                        ; J2000 obliquity 84381.448", IAU 1976 (Lieske et al. 1977)
  (* 84381.448d0 (/ pi 648000)))
(defconstant +au+ 149597870700d0)               ; m, IAU 2012 Resolution B2

;;; Time, s
(defconstant +tt-minus-utc+ 69.184d0)           ; 32.184 s + 37 leap seconds (IERS Bulletin C, since 2017)
(defconstant +day+ 86400d0)                     ; SI day
(defconstant +julian-century+ 36525d0)          ; days, IAU
(defconstant +fine-step+ (/ 1d0 64))            ; decision 16: Integrator and simulation clock

(defparameter *start-date* '(2040 1 1 0 0 0)
  "UTC (year month day hour minute second) of a new game (docs/story.md).")

;;; Calendar: integer days and exact seconds are summed as rationals and
;;; made a double only at the end.

(defconstant +j2000-jdn+ 2451545)

(defun julian-day-number (year month day)
  "Fliegel-Van Flandern 1968: the Julian Day Number of a Gregorian date."
  (declare (type integer year month day))
  (let ((a (truncate (- month 14) 12)))
    (+ (truncate (* 1461 (+ year 4800 a)) 4)
       (truncate (* 367 (- month 2 (* 12 a))) 12)
       (- (truncate (* 3 (truncate (+ year 4900 a) 100)) 4))
       day
       -32075)))

(defun jdn->date (jdn)
  "Fliegel-Van Flandern 1968, inverse: (values year month day)."
  (declare (type integer jdn))
  (let* ((l (+ jdn 68569))
         (n (floor (* 4 l) 146097))
         (l (- l (floor (+ (* 146097 n) 3) 4)))
         (i (floor (* 4000 (+ l 1)) 1461001))
         (l (+ (- l (floor (* 1461 i) 4)) 31))
         (j (floor (* 80 l) 2447))
         (day (- l (floor (* 2447 j) 80)))
         (l (floor j 11))
         (month (- (+ j 2) (* 12 l)))
         (year (+ (* 100 (- n 49)) i l)))
    (values year month day)))

(defun utc->exact-t (year month day hour minute second)
  "t as a rational: (JDN - 2451545) x 86400 + seconds of day - 43200 + (TT - UTC)."
  (+ (* (- (julian-day-number year month day) +j2000-jdn+) 86400)
     (* 3600 hour) (* 60 minute) (rational second)
     -43200
     (rationalize +tt-minus-utc+)))

(defun utc->t (year month day hour minute second)
  "Simulation time of a UTC calendar date, exact to the double."
  (float (utc->exact-t year month day hour minute second) 1d0))

(defun utc->grid-t (year month day hour minute second)
  "utc->t rounded to the nearest multiple of +fine-step+ (decision 66)."
  (let ((steps (/ 1 (rational +fine-step+))))
    (float (/ (round (* steps (utc->exact-t year month day hour minute second))) steps)
           1d0)))

(defun t->utc (time)
  "(values year month day hour minute second) of t in UTC; second is a
double, truncating it to whole seconds is the caller's."
  (multiple-value-bind (days seconds)
      (floor (+ (rational time) (- (rationalize +tt-minus-utc+)) 43200) 86400)
    (multiple-value-bind (year month day) (jdn->date (+ days +j2000-jdn+))
      (multiple-value-bind (hour rest) (floor seconds 3600)
        (multiple-value-bind (minute second) (floor rest 60)
          (values year month day hour minute (float second 1d0)))))))

(defun start-t ()
  "The grid time of *start-date*."
  (apply #'utc->grid-t *start-date*))

(defun t->ut1-days (time)
  "JD_UT1 - 2451545.0, for Earth's rotation angle."
  (declare (type double-float time))
  (/ (- time +tt-minus-utc+) +day+))

(defun t->julian-centuries (time)
  "Julian centuries of TDB since J2000, for the ephemeris series."
  (declare (type double-float time))
  (/ time (* +day+ +julian-century+)))

;;; Display conversions: instruments only.

(declaim (inline m->km m/s->km/s rad->deg deg->rad))
(defun m->km (x) (/ x 1000d0))
(defun m/s->km/s (x) (/ x 1000d0))
(defun rad->deg (x) (* x (/ 180d0 pi)))
(defun deg->rad (x) (* x (/ pi 180d0)))

(defun duration-fields (seconds)
  "(values days hours minutes seconds) of |seconds|; seconds is a double."
  (multiple-value-bind (minutes second) (floor (abs (rational seconds)) 60)
    (multiple-value-bind (hours minute) (floor minutes 60)
      (multiple-value-bind (days hour) (floor hours 24)
        (values days hour minute (float second 1d0))))))

(defun format-utc (time)
  "t as a UTC date, YYYY-MM-DD hh:mm:ss, seconds truncated."
  (multiple-value-bind (year month day hour minute second) (t->utc time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute (floor second))))
