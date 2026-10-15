(in-package #:luv-in-space.tests)

;;; The overlay's glyphs, markers and labels (decision 7: Placeholder labels:
;;; dot-matrix overlay quads; decision 84: Map ribbons and markers: osculating
;;; orbit, adaptive segments, overlay markers and visibility).

(defparameter *overlay-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -.:()%/!+,°×…")

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

;;; decision 85: Minimal readout: layout, formats, colours and alarms; the
;;; texts of decision 73: Warp levels above x4: entry, holds and lowering.

(defun clock-message-texts ()
  "The refusal and lowering messages of tests/clock.lisp's cases, through the clock."
  (let ((texts '())
        (lis::*frame-step-time-limit* nil))
    (flet ((collect (clock) (dolist (m (lis::clock-messages clock)) (pushnew m texts :test #'string=))))
      (let* ((sim (lis::new-game-sim))
             (clock (clock-at sim 10)))
        (lis::controls-throttle-full (lis::sim-controls sim))
        (lis::clock-warp-up clock sim)
        (collect clock)
        (setf (lis::sim-cutoff sim) 100d0)
        (lis::clock-warp-up clock sim)
        (lis::clock-warp-up clock sim)
        (collect clock))
      (let* ((sim (lis::new-game-sim))
             (clock (clock-at sim 4)))
        (lis::controls-toggle-rate-damping (lis::sim-controls sim))
        (replace (lis::ship-state-w (lis::sim-ship sim)) '(0d0 0d0 0.02d0))
        (lis::clock-warp-up clock sim)
        (collect clock))
      (let* ((sim (lis::new-game-sim))
             (clock (clock-at sim 100)))
        (lis::clock-frame! clock sim 1/60)
        (lis::controls-throttle-full (lis::sim-controls sim))
        (lis::clock-frame! clock sim 1/60)
        (collect clock))
      (let* ((sim (lis::new-game-sim))
             (clock (clock-at sim 1000)))
        (lis::clock-frame! clock sim 1/60)
        (lis::controls-set-hold (lis::sim-controls sim) :prograde)
        (lis::clock-frame! clock sim 1/60)
        (collect clock))
      (let* ((orbit (dive-orbit (- *earth-crash-radius* 50d3)))
             (impact (lis::orbit-radius-crossings orbit *earth-crash-radius* (lis::start-t))))
        (loop for (lead level) in '((20d0 4) (100d0 100))
              for sim = (orbit-sim orbit (lis::grid-ceiling (- impact lead)))
              for clock = (clock-at sim level)
              do (lis::prediction-run! (lis::clock-prediction clock) sim)
                 (if (= level 4)
                     (lis::clock-warp-up clock sim)
                     (lis::clock-frame! clock sim 1/60))
                 (collect clock))))
    (list* (lis::dropout-message 1000000 3600d0 nil)
           (lis::dropout-message 1000 60d0 (lis::make-loss :crash :moon 0d0 0d0 0d0 0d0))
           texts)))

(define-test glyph-missing
  (is eql #\a (lis::glyph-missing "a"))
  (is eql #\a (lis::glyph-missing "ALT a"))
  (false (lis::glyph-missing "RCS OFF ABOVE ×4"))
  (dolist (seconds '(0d0 59d0 3600d0 #.(* 400 86400d0)))
    (false (lis::glyph-missing (lis::format-duration seconds)) "~S" (lis::format-duration seconds)))
  (let ((texts (clock-message-texts)))
    (dolist (prefix '("WARP REFUSED: ENGINE ON" "WARP REFUSED: ROTATING" "WARP REFUSED: IMPACT IN"
                      "WARP ×10: ENGINE ON" "WARP ×4: HOLD RELEASED" "WARP ×10: PERIAPSIS BELOW"
                      "WARP ×1,000,000: PERIAPSIS" "WARP ×1,000: MOON IMPACT AHEAD"))
      (true (find prefix texts :test (lambda (p m) (eql 0 (search p m)))) "~S in ~S" prefix texts))
    (dolist (text texts)
      (false (lis::glyph-missing text) "~S" text))))

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

(defun overlay-colour-at (out x y)
  "The linear colour of OUT's last triangle covering X, Y, the one drawn on
top, or nil."
  (let ((floats (lis::overlay-buffer-floats out)) (colour nil))
    (loop for (a b c) on (overlay-points out) by #'cdddr
          for v from 0 by 3
          when (flet ((side (p q)
                        (- (* (- (first q) (first p)) (- y (second p)))
                           (* (- (second q) (second p)) (- x (first p))))))
                 (let ((s1 (side a b)) (s2 (side b c)) (s3 (side c a)))
                   (or (and (>= s1 0) (>= s2 0) (>= s3 0))
                       (and (<= s1 0) (<= s2 0) (<= s3 0)))))
            do (let ((o (+ 4 (* v lis::+overlay-vertex-floats+))))
                 (setf colour (list (aref floats o) (aref floats (+ o 1)) (aref floats (+ o 2))))))
    colour))

(defun linear-rgb (rgb)
  (loop for shift in '(16 8 0) collect (coerce (lis::srgb-channel-linear rgb shift) 'single-float)))

(define-test label-backing
  ;; decision 104: Map labels on a near-black backing
  (let ((out (lis::make-overlay-buffer))
        (markers (lis::make-map-markers))
        (backing (linear-rgb #x0A0C0C)) (paper (linear-rgb #xE8E4D8)))
    (let ((marker (svref (lis::map-markers-items markers) 0)))
      (setf (lis::map-markers-count markers) 1
            (lis::map-marker-key marker) :periapsis (lis::map-marker-kind marker) :diamond
            (lis::map-marker-x marker) 100d0 (lis::map-marker-y marker) 50d0
            (lis::map-marker-colour marker) #x40D0FF (lis::map-marker-label marker) "AP 2000 KM"))
    (lis::map-marker-vertices! out markers)
    ;; the label from x 108, rows 43 .. 57; A's top row .###.
    (is equal backing (overlay-colour-at out 109.0 44.0))
    (is equal paper (overlay-colour-at out 111.0 44.0))
    (is equal backing (overlay-colour-at out 107.0 50.0) "the margin left")
    (is equal backing (overlay-colour-at out 109.0 41.5) "the margin above")
    (is equal backing (overlay-colour-at out (+ 108.0 (lis::label-width "AP 2000 KM") 1.5) 56.0)
        "the margin right")
    (false (overlay-colour-at out 109.0 40.5) "beyond the margin")
    (is equal (linear-rgb #x40D0FF) (overlay-colour-at out 100.0 50.0) "the diamond")
    (is = 0 (least-bytes-consed
             (lambda () (dotimes (i 1000)
                          (lis::overlay-clear! out)
                          (lis::map-marker-vertices! out markers)))))))

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

;;; The readout (decision 85: Minimal readout: layout, formats, colours and alarms)

(defun readout (sim clock &optional (real-time 0d0))
  (lis::readout-lines (lis::render-state-update! (lis::make-render-state) clock sim)
                      clock real-time))

(defun readout-texts (lines) (mapcar #'lis::line-text lines))

(defun readout-line (lines prefix)
  "The line of LINES whose text starts with PREFIX, or nil."
  (find-if (lambda (line) (eql 0 (search prefix (lis::line-text line)))) lines))

(defun readout-has (lines text)
  (find text (readout-texts lines) :test #'string=))

(defun state-lines (sim &key (clock (lis::new-clock sim)) (r nil) (v nil))
  "The readout of SIM with the render state's R and V replaced."
  (let ((state (lis::render-state-update! (lis::make-render-state) clock sim)))
    (when r (replace (lis::render-state-r state) r))
    (when v (replace (lis::render-state-v state) v))
    (lis::readout-lines state clock 0d0)))

(defun check-glyphs (lines)
  (dolist (text (readout-texts lines))
    (false (lis::glyph-missing text) "~S" text)))

(define-test readout-start-state
  (let* ((sim (lis::new-game-sim))
         (lines (readout sim (lis::new-clock sim)))
         (speed (sqrt (/ lis::+mu-earth+ 6778136.6d0))))
    (is equal '("ALT 400.0 KM" "SPD 7.669 KM/S" "AP 400.0 KM" "PE 400.0 KM" "THR 0% OFF"
                "PROP 28000 KG 100%" "MODE HOLD PROGRADE SETTLED" "WARP ×1"
                "UTC 2040-01-01 00:00:00")
        (readout-texts lines))
    (is string= (format nil "SPD ~,3F KM/S" (/ speed 1000)) (lis::line-text (second lines)))
    (dolist (line lines)
      (is = #x8A8A80 (lis::run-colour (first line)))
      (dolist (run (rest line))
        (is = #xE8E4D8 (lis::run-colour run))
        (is eq :plain (lis::run-style run))
        (false (lis::run-flash run))))
    (check-glyphs lines)))

(define-test readout-formats
  (let ((sim (lis::new-game-sim))
        (r (lis::vec3 (+ lis::+radius-earth+ 400d3) 0 0)))
    (true (readout-has (state-lines sim :r r :v (lis::vec3 0 950 0)) "SPD 950.0 M/S"))
    (true (readout-has (state-lines sim :r r :v (lis::vec3 0 1000 0)) "SPD 1.000 KM/S"))
    ;; e = 1.2 at periapsis: v² = μ (1 + e) / r
    (let ((lines (state-lines sim :r r :v (lis::vec3 0 (sqrt (/ (* 2.2d0 lis::+mu-earth+)
                                                                (lis::vec3-norm r)))
                                                     0))))
      (true (readout-has lines "AP ESCAPE"))
      (true (readout-has lines "PE 400.0 KM"))
      (check-glyphs lines)))
  (let ((sim (lis::new-game-sim)))
    (lis::controls-set-throttle (lis::sim-controls sim) 0.45d0)
    (true (readout-has (readout sim (lis::new-clock sim)) "THR 45% ON"))
    (setf (lis::ship-state-m-p (lis::sim-ship sim)) 0d0)
    (let ((lines (readout sim (lis::new-clock sim))))
      (true (readout-has lines "THR 45% OFF"))
      (true (readout-has lines "PROP 0 KG 0%"))))
  (let ((sim (lis::new-game-sim)))
    (lis::controls-toggle-rate-damping (lis::sim-controls sim))
    (setf (lis::controls-fine (lis::sim-controls sim)) t)
    (let ((lines (readout sim (lis::new-clock sim))))
      (true (readout-has lines "MODE RATE DAMP FINE") "~S" (readout-texts lines))))
  (let* ((sim (lis::new-game-sim))
         (clock (clock-at sim 1000)))
    (lis::clock-toggle-pause clock)
    (lis::clock-frame! clock sim 1/60)
    (true (readout-has (readout sim clock) "WARP ×1000 PAUSED")))
  (flet ((metered (rate)
           (let* ((sim (lis::new-game-sim))
                  (clock (clock-at sim 1000000)))
             (lis::rate-meter-record! clock 1d0 rate)
             (readout-line (readout sim clock) "WARP"))))
    (let ((line (metered 490000d0)))
      (is string= "WARP ×1000000 ! ACT ×490000" (lis::line-text line))
      (is = #xFFB000 (lis::run-colour (third line)))
      (is string= " ! ACT ×490000" (lis::run-text (third line))))
    (is string= "WARP ×1000000" (lis::line-text (metered 950000d0)))))

(define-test readout-alarms
  (let* ((lis::*frame-step-time-limit* nil)
         (sim (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3))))
         (clock (lis::new-clock sim)))
    (let ((alarm (readout-line (readout sim clock) "! PE")))
      (true alarm)
      (when alarm
        (is string= "! PE BELOW 100 KM" (lis::line-text alarm))
        (is = #xFFB000 (lis::run-colour (first alarm)))))
    (false (readout-line (readout sim clock) "EARTH IMPACT"))
    (lis::prediction-run! (lis::clock-prediction clock) sim)
    (let* ((impact (lis::prediction-event (lis::clock-prediction clock) :impact))
           (on (readout sim clock 0.1d0))
           (off (readout sim clock 0.35d0))
           (expected (format nil "EARTH IMPACT IN ~A"
                             (lis::format-duration (- impact (lis::sim-t sim))))))
      (true impact)
      (let ((run (first (readout-line on "EARTH IMPACT"))))
        (is string= expected (lis::run-text run))
        (is = #xFF3030 (lis::run-colour run))
        (is eq :inverted (lis::run-style run))
        (is eq :on (lis::run-flash run)))
      (is eq :off (lis::run-flash (first (readout-line off "EARTH IMPACT"))))
      (check-glyphs on))))

(define-test readout-messages
  (let* ((sim (lis::new-game-sim))
         (clock (clock-at sim 10)))
    (lis::controls-throttle-full (lis::sim-controls sim))
    (lis::clock-warp-up clock sim)
    (let ((line (readout-line (readout sim clock) "! WARP REFUSED")))
      (is string= "! WARP REFUSED: ENGINE ON, LIMIT ×10" (lis::line-text line))
      (is = #xFFB000 (lis::run-colour (first line))))
    (check-glyphs (readout sim clock))
    (incf (lis::clock-real-time clock) 3d0)
    (false (readout-line (readout sim clock) "! WARP")))
  (let* ((sim (lis::new-game-sim))
         (clock (lis::new-clock sim)))
    (dotimes (i 4) (lis::clock-message! clock i (format nil "MESSAGE ~D" i)))
    (lis::clock-message! clock :long (make-string 60 :initial-element #\A))
    (let ((texts (last (readout-texts (readout sim clock)) 3)))
      (is equal '("! MESSAGE 2" "! MESSAGE 3") (subseq texts 0 2))
      (is = (+ 2 40) (length (third texts)))
      (is string= (concatenate 'string "! " (make-string 39 :initial-element #\A) "…")
          (third texts)))))

(define-test readout-conses-little
  (let* ((sim (lis::new-game-sim))
         (clock (lis::new-clock sim))
         (state (lis::render-state-update! (lis::make-render-state) clock sim)))
    (true (< (least-bytes-consed (lambda () (lis::readout-lines state clock 0d0)) 5)
             16384))))
