(in-package #:luv-in-space.tests)

(defun utc-fields (time)
  (multiple-value-list (lis::t->utc time)))

(defun same-date-p (fields expected)
  "Fields equal, seconds within 1e-6 s."
  (and (equal (subseq fields 0 5) (subseq expected 0 5))
       (< (abs (- (sixth fields) (sixth expected))) 1d-6)))

;; J2000.0 is 11:58:55.816 UTC in reality (TT - UTC = 64.184 s in 2000); the
;; game holds TT - UTC at 69.184 s for every date (decision 14), so 11:58:50.816.
(define-test j2000-is-zero
  (true (< (abs (lis::utc->t 2000 1 1 11 58 50.816d0)) 1d-9)))

(define-test calendar-round-trips
  (dolist (date (list '(2000 1 1 11 58 50.816d0)
                      lis::*start-date*
                      '(2040 2 29 12 34 56)
                      '(1999 12 31 23 59 59)))
    (true (same-date-p (utc-fields (apply #'lis::utc->t date)) date) "~A" date)))

(define-test start-on-the-grid
  (let ((start (lis::start-t))
        (exact (apply #'lis::utc->t lis::*start-date*)))
    (is = 0 (nth-value 1 (floor start lis::+fine-step+)))
    (true (<= (abs (- start exact)) (/ 1d0 128)))
    (is string= "2040-01-01 00:00:00" (lis::format-utc start))
    (is string= "2040-01-02 00:00:00" (lis::format-utc (+ start lis::+day+)))
    (true (< (lis::utc->t 1800 1 1 0 0 0) start (lis::utc->t 2050 1 1 0 0 0)))))

(define-test time-scales
  (is = 0.5d0 (lis::t->ut1-days (+ lis::+tt-minus-utc+ 43200)))
  (is = 1d0 (lis::t->julian-centuries (* 36525 86400d0))))

(define-test display-conversions
  (dolist (degrees '(0d0 1d0 -45d0 90d0 123.456d0 359.9d0))
    (true (< (abs (- degrees (lis::rad->deg (lis::deg->rad degrees)))) 1d-12)))
  (is = pi (lis::deg->rad 180))
  (is = 6.8d0 (lis::m->km 6800d0))
  (is = 7.669d0 (lis::m/s->km/s 7669d0))
  (is equal '(1 2 3 4.5d0) (multiple-value-list (lis::duration-fields 93784.5d0))))
