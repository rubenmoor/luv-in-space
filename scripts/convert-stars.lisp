;;; scripts/convert-stars.lisp <hyg.csv> <out.sexp>, run by scripts/convert-stars:
;;; the HYG stars of magnitude <= 6.5 but the Sun, sorted by magnitude then
;;; HYG id, one list (hip ra-deg dec-deg mag ci) per star. Numbers are parsed
;;; as exact rationals and rounded, so the output is reproducible.

(defun split-csv-line (line)
  "The fields of LINE; quoted fields may hold commas and doubled quotes."
  (let ((fields '()) (field (make-string-output-stream)) (quoted nil) (i 0)
        (n (length line)))
    (loop while (< i n)
          do (let ((c (char line i)))
               (cond ((and quoted (char= c #\") (< (1+ i) n) (char= (char line (1+ i)) #\"))
                      (write-char #\" field) (incf i))
                     ((char= c #\") (setf quoted (not quoted)))
                     ((and (not quoted) (char= c #\,))
                      (push (get-output-stream-string field) fields))
                     (t (write-char c field))))
             (incf i))
    (push (get-output-stream-string field) fields)
    (nreverse fields)))

(defun parse-decimal (string)
  "STRING such as \"-16.716116\" as an exact rational; nil when empty."
  (let ((s (string-trim " " string)))
    (when (plusp (length s))
      (let* ((negative (char= (char s 0) #\-))
             (body (if (or negative (char= (char s 0) #\+)) (subseq s 1) s))
             (dot (position #\. body))
             (whole (subseq body 0 (or dot (length body))))
             (fraction (if dot (subseq body (1+ dot)) ""))
             (value (+ (if (plusp (length whole)) (parse-integer whole) 0)
                       (if (plusp (length fraction))
                           (/ (parse-integer fraction) (expt 10 (length fraction)))
                           0))))
        (if negative (- value) value)))))

(defun fixed (rational digits)
  "RATIONAL rounded half away from zero to DIGITS decimals, as a string."
  (let* ((scale (expt 10 digits))
         (units (* (signum rational) (floor (+ (* (abs rational) scale) 1/2)))))
    (multiple-value-bind (whole fraction) (truncate (abs units) scale)
      (format nil "~:[~;-~]~D.~v,'0D" (minusp units) whole digits fraction))))

(defun convert (in out)
  (with-open-file (stream in :external-format :utf-8)
    (let* ((header (split-csv-line (read-line stream)))
           (column (lambda (name)
                     (or (position name header :test #'string=)
                         (error "HYG column ~A missing" name))))
           (id (funcall column "id")) (hip (funcall column "hip"))
           (ra (funcall column "ra")) (dec (funcall column "dec"))
           (mag (funcall column "mag")) (ci (funcall column "ci"))
           (stars '()))
      (loop for line = (read-line stream nil)
            while line
            do (let* ((fields (split-csv-line (string-right-trim '(#\Return) line)))
                      (star-id (parse-integer (nth id fields)))
                      (m (parse-decimal (nth mag fields))))
                 (when (and m (<= m 13/2) (/= star-id 0))
                   (push (list star-id
                               (let ((h (parse-decimal (nth hip fields))))
                                 (and h (plusp h) h))
                               (* 15 (parse-decimal (nth ra fields)))
                               (parse-decimal (nth dec fields))
                               m
                               (parse-decimal (nth ci fields)))
                         stars))))
      (setf stars (sort stars (lambda (a b)
                                (or (< (fifth a) (fifth b))
                                    (and (= (fifth a) (fifth b)) (< (first a) (first b)))))))
      (with-open-file (o out :direction :output :if-exists :supersede
                             :external-format :utf-8)
        (format o ";;; assets/stars/hyg-6.5.sexp: the stars of visual magnitude <= 6.5 but~%~
;;; the Sun from the HYG database v4.4 (hyg_v44.csv.gz, codeberg.org/astronexus/hyg,~%~
;;; commit 53e3df311869e813ace5f1ad2ec4ce909f13256c of 2026-07-12).~%~
;;; Credit: HYG database by David Nash (astronexus). Licence: CC BY-SA 4.0~%~
;;; (assets/stars/LICENSE); this table is a derived work under the same licence.~%~
;;; Made by scripts/convert-stars; see assets/stars/SOURCE.md.~%~
;;; ~D stars, by magnitude then HYG id, one per line:~%~
;;; (hip ra-deg dec-deg mag ci): Hipparcos number or nil, J2000 right ascension~%~
;;; and declination in degrees, visual magnitude, B-V colour index or nil.~%"
                (length stars))
        (dolist (s stars)
          (destructuring-bind (star-id h r d m c) s
            (declare (ignore star-id))
            (format o "(~:[nil~;~:*~D~] ~A ~A ~A ~:[nil~;~:*~A~])~%"
                    h (fixed r 6) (fixed d 6) (fixed m 3) (and c (fixed c 3)))))))))

(destructuring-bind (in out) (last sb-ext:*posix-argv* 2)
  (convert in out))
