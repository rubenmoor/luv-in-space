(in-package #:luv-in-space.tests)

;;; The overlay's glyphs, markers and labels (decision 7: Placeholder labels:
;;; dot-matrix overlay quads; decision 84: Map ribbons and markers: osculating
;;; orbit, adaptive segments, overlay markers and visibility).

(defparameter *overlay-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -.:")

(defun overlay-points (out)
  "OUT's vertices as a list of (x y) pixel positions."
  (let ((floats (lis::overlay-buffer-floats out)))
    (loop for v below (lis::overlay-buffer-count out)
          for o = (* v lis::+overlay-vertex-floats+)
          collect (list (aref floats (+ o 2)) (aref floats (+ o 3))))))

(defun overlay-covers-p (out x y)
  "Whether a triangle of OUT covers pixel position X, Y."
  (loop for (a b c) on (overlay-points out) by #'cdddr
        thereis (flet ((side (p q)
                         (- (* (- (first q) (first p)) (- y (second p)))
                            (* (- (second q) (second p)) (- x (first p))))))
                  (let ((s1 (side a b)) (s2 (side b c)) (s3 (side c a)))
                    (or (and (>= s1 0) (>= s2 0) (>= s3 0))
                        (and (<= s1 0) (<= s2 0) (<= s3 0)))))))

(define-test glyph-table-complete
  (loop for char across *overlay-characters*
        for rows = (lis::glyph char)
        do (is = 7 (length rows) "~S has no glyph" char)
           (true (every (lambda (row) (= 5 (length row))) rows) "~S is not 5 wide" char)
           (if (char= char #\Space)
               (false (some (lambda (row) (find #\# row)) rows))
               (true (some (lambda (row) (find #\# row)) rows) "~S has no lit cell" char))))

(define-test marker-shapes
  (let ((out (lis::make-overlay-buffer)))
    (dolist (kind '(:ring :diamond :cross :dot))
      (lis::overlay-clear! out)
      (lis::marker-vertices! out kind 100.0 50.0 #x40D0FF)
      (let ((h (/ (lis::marker-size kind) 2)))
        (true (plusp (lis::overlay-buffer-count out)))
        (is = 0 (mod (lis::overlay-buffer-count out) 3))
        (true (every (lambda (p) (and (<= (abs (- (first p) 100)) (+ h 1e-4))
                                      (<= (abs (- (second p) 50)) (+ h 1e-4))))
                     (overlay-points out))
              "~S outside its ~D px box" kind (* 2 h))
        (case kind
          (:ring (false (overlay-covers-p out 100.0 50.0))
                 (true (overlay-covers-p out 105.0 50.0)))
          (t (true (overlay-covers-p out 100.0 50.0) "~S misses its centre" kind)))))
    (lis::overlay-clear! out)
    (lis::marker-vertices! out :diamond 0 0 #x40D0FF 20)
    (true (some (lambda (p) (= 10 (first p))) (overlay-points out)))))

(define-test marker-colour-linear
  (let ((out (lis::make-overlay-buffer)))
    (lis::marker-vertices! out :dot 0.0 0.0 #xFF3030)
    (let ((floats (lis::overlay-buffer-floats out)))
      (is = 1.0 (aref floats 4))
      (true (< (abs (- (aref floats 5) 0.0296)) 1e-3))
      (true (< (abs (- (aref floats 6) 0.0296)) 1e-3)))))

(define-test label-placement
  (let ((out (lis::make-overlay-buffer)))
    (lis::label-at! out "AP 2000 KM" 40.0 100.0 #xE8E4D8)
    (let* ((points (overlay-points out))
           (xs (mapcar #'first points)) (ys (mapcar #'second points)))
      (is = 40.0 (reduce #'min xs))
      (is = 100.0 (/ (+ (reduce #'min ys) (reduce #'max ys)) 2))
      (is = (* 7 lis::*label-dot-px*) (- (reduce #'max ys) (reduce #'min ys)))
      ;; M, the tenth cell, ends 9 cells of 6 dots plus 5 dots in.
      (is = (+ 40.0 (* lis::*label-dot-px* (+ 54 5))) (reduce #'max xs))))
  (let ((upper (lis::make-overlay-buffer)) (lower (lis::make-overlay-buffer)))
    (lis::label-at! upper "MOON" 0 0 #xE8E4D8)
    (lis::label-at! lower "moon" 0 0 #xE8E4D8)
    (is equalp (lis::overlay-floats upper) (lis::overlay-floats lower))))

(define-test label-vertices-whole
  (let ((lit (loop for char across lis::*label*
                   sum (loop for row in (lis::glyph char) sum (count #\# row)))))
    (is = (* 6 lit lis::+overlay-vertex-floats+) (length (lis::label-vertices lis::*label*)))))

(define-test overlay-conses-nothing
  (let ((out (lis::make-overlay-buffer)))
    (is = 0 (least-bytes-consed
             (lambda ()
               (dotimes (i 1000)
                 (lis::overlay-clear! out)
                 (dolist (kind '(:ring :diamond :cross :dot))
                   (lis::marker-vertices! out kind 640.0 400.0 #x40D0FF))
                 (lis::label-at! out "PE 400 KM" 648.0 400.0 #xE8E4D8)
                 (lis::label-at! out "IMPACT" 10 20 #xFF3030)))))))
