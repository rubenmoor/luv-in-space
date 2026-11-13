(in-package #:luv-in-space.tests)

;;; The map's ribbon, CPU half (decision 84: Map ribbons and markers:
;;; osculating orbit, adaptive segments, overlay markers and visibility).

(defun apsides-orbit (periapsis-altitude apoapsis-altitude)
  "An Earth orbit of these altitudes in m, i 51.6°, periapsis at the start time."
  (let ((rp (+ lis::+radius-earth+ periapsis-altitude))
        (ra (+ lis::+radius-earth+ apoapsis-altitude)))
    (lis::make-elliptic-orbit lis::+mu-earth+ (/ (+ rp ra) 2) (/ (- ra rp) (+ ra rp))
                              (deg 51.6d0) (deg 30d0) (deg 60d0) 0d0 (lis::start-t))))

(defun eccentric-orbit (e)
  "Periapsis 400 km up with eccentricity E: an ellipse or a hyperbola."
  (let ((q (+ lis::+radius-earth+ 400d3)))
    (if (< e 1)
        (lis::make-elliptic-orbit lis::+mu-earth+ (/ q (- 1 e)) e (deg 51.6d0) (deg 30d0)
                                  (deg 60d0) 0d0 (lis::start-t))
        (lis::make-hyperbolic-orbit lis::+mu-earth+ q e (deg 51.6d0) (deg 30d0) (deg 60d0)
                                    0d0 (lis::start-t)))))

(defun conic-radius (orbit x)
  (let ((e (lis::orbit-e orbit)) (a (abs (lis::orbit-a orbit))))
    (if (< e 1) (* a (- 1 (* e (cos x)))) (* a (- (* e (cosh x)) 1)))))

(defun conic-point (orbit x)
  "The point at anomaly X (E or H) relative to the body, in double."
  (let* ((e (lis::orbit-e orbit)) (a (abs (lis::orbit-a orbit)))
         (b (* a (sqrt (abs (- 1 (* e e)))))))
    (multiple-value-bind (pv qv)
        (lis::perifocal-axes (lis::orbit-i orbit) (lis::orbit-raan orbit) (lis::orbit-argp orbit))
      (multiple-value-bind (px py)
          (if (< e 1)
              (values (* a (- (cos x) e)) (* b (sin x)))
              (values (* a (- e (cosh x))) (* b (sinh x))))
        (lis::vec3-add (lis::vec3-scale pv px) (lis::vec3-scale qv py))))))

(defun ribbon-point (ribbon s)
  (let ((p (lis::ribbon-points ribbon)))
    (lis::vec3 (aref p (* 3 s)) (aref p (+ (* 3 s) 1)) (aref p (+ (* 3 s) 2)))))

(defun map-eye (camera state)
  (lis::camera-eye camera (lis::camera-anchor-position! (lis::make-vec3) camera state)))

(defun ship-focus-map (state)
  "The map camera at ship focus, 50 m away."
  (let ((view (opened-map state)))
    (lis::map-view-cycle-focus! view)
    (lis::map-view-zoom! view 1d-9)
    (values view (lis::map-camera-update! (lis::make-camera) view state nil))))

(defun filled-ribbon (orbit camera state &key (height 800) (ribbon (lis::make-ribbon)))
  (let ((eye (map-eye camera state)))
    (lis::orbit-ribbon-points! ribbon orbit eye (lis::camera-focal-px camera height))
    (lis::orbit-ribbon-instances! ribbon eye)
    ribbon))

(defun start-map-state ()
  (orbit-render-state (start-orbit) (lis::start-t)))

(define-test ribbon-points-on-conic
  (dolist (orbit (list (start-orbit) (eccentric-orbit 0.5d0) (eccentric-orbit 0.9d0)
                       (eccentric-orbit 1.5d0)))
    (let* ((state (orbit-render-state orbit (+ (lis::start-t) 600)))
           (ribbon (filled-ribbon orbit (nth-value 1 (opened-map state)) state))
           (worst 0d0))
      (loop for s to (lis::ribbon-count ribbon)
            for x = (aref (lis::ribbon-anomalies ribbon) s)
            do (setf worst (max worst (abs (- (lis::vec3-norm (ribbon-point ribbon s))
                                              (conic-radius orbit x))))))
      (true (< worst 1d-6) "e ~A: off the conic by ~A m" (lis::orbit-e orbit) worst))))

(define-test ribbon-hyperbola-to-laplace-sphere
  (let* ((orbit (eccentric-orbit 1.5d0))
         (state (orbit-render-state orbit (lis::start-t)))
         (ribbon (filled-ribbon orbit (nth-value 1 (opened-map state)) state))
         (soi (lis::body-soi-radius (lis::body :earth)))
         (radii (lis::ribbon-radii ribbon)))
    (true (< (abs (- (aref radii 0) soi)) 1d-3))
    (true (< (abs (- (aref radii (lis::ribbon-count ribbon)) soi)) 1d-3))))

(defun pixel (rows p width height)
  "P (eye-relative, double) in pixels, or nil behind the eye."
  (destructuring-bind (x y z w) (clip rows p)
    (declare (ignore z))
    (and (> w 0) (values (* 0.5d0 width (1+ (/ x w))) (* 0.5d0 height (1+ (/ y w)))))))

(defun aimed-camera (camera p)
  "CAMERA turned to look along P (eye-relative), with its field of view."
  (let* ((x (lis::vec3-normalize p))
         (z0 (if (< (abs (aref x 2)) 0.9d0) (lis::vec3 0 0 1) (lis::vec3 1 0 0)))
         (z (lis::vec3-normalize (lis::vec3-add-scaled z0 x (- (lis::vec3-dot z0 x)))))
         (y (lis::vec3-cross z x)))
    (lis::make-camera :orientation (lis::quat-from-basis x y z)
                      :field-of-view (lis::camera-field-of-view camera))))

(define-test ribbon-splitting
  ;; Measured: 256 segments at the opening frame, 794 at ship focus 50 m.
  ;; The ship focus frame is ~75 m wide and the pieces near the ship ~650 m
  ;; long, so each midpoint in front of the eye is checked by the camera
  ;; turned towards it, where the frame would show it.
  (let* ((orbit (start-orbit))
         (state (start-map-state))
         (width 1280) (height 800))
    (is = 256 (lis::ribbon-count (filled-ribbon orbit (nth-value 1 (opened-map state)) state)))
    (multiple-value-bind (view camera) (ship-focus-map state)
      (declare (ignore view))
      (let* ((ribbon (filled-ribbon orbit camera state :height height))
             (eye (map-eye camera state))
             (forward (camera-axis camera 0))
             (anomalies (lis::ribbon-anomalies ribbon))
             (rows (make-array 16 :element-type 'single-float))
             (checked 0) (worst 0d0))
        (true (< 256 (lis::ribbon-count ribbon) 8192) "~A segments" (lis::ribbon-count ribbon))
        (dotimes (s (lis::ribbon-count ribbon))
          (let* ((chord (lis::vec3-sub (lis::vec3-scale (lis::vec3-add (ribbon-point ribbon s)
                                                                       (ribbon-point ribbon (1+ s)))
                                                        0.5d0)
                                       eye))
                 (arc (lis::vec3-sub (conic-point orbit (* 0.5d0 (+ (aref anomalies s)
                                                                    (aref anomalies (1+ s)))))
                                     eye)))
            (when (plusp (lis::vec3-dot chord forward))
              (lis::camera-clip-rows! rows (aimed-camera camera arc) (/ width height))
              (multiple-value-bind (cx cy) (pixel rows chord width height)
                (multiple-value-bind (ax ay) (pixel rows arc width height)
                  (incf checked)
                  (setf worst (max worst (sqrt (+ (expt (- ax cx) 2) (expt (- ay cy) 2))))))))))
        (true (> checked 100) "~A segments checked" checked)
        (true (<= worst 0.5d0) "chord midpoints off by ~A px" worst)))))

(define-test ribbon-colours
  (let ((cyan (list (lis::srgb-channel-linear #x40D0FF 16) (lis::srgb-channel-linear #x40D0FF 8)
                    (lis::srgb-channel-linear #x40D0FF 0)))
        (red (list (lis::srgb-channel-linear #xFF3030 16) (lis::srgb-channel-linear #xFF3030 8)
                   (lis::srgb-channel-linear #xFF3030 0))))
    (flet ((colours (orbit)
             (let* ((state (orbit-render-state orbit (lis::start-t)))
                    (ribbon (filled-ribbon orbit (nth-value 1 (opened-map state)) state))
                    (out (lis::ribbon-instances ribbon))
                    (anomalies (lis::ribbon-anomalies ribbon)))
               (loop for s below (lis::ribbon-count ribbon)
                     for o = (* s lis::+ribbon-instance-floats+)
                     for colour = (loop for i from 8 below 11 collect (aref out (+ o i)))
                     for mid = (conic-radius orbit (* 0.5d0 (+ (aref anomalies s)
                                                               (aref anomalies (1+ s)))))
                     collect (list (cond ((every (lambda (c x) (= c (coerce x 'single-float))) colour red) :red)
                                         ((every (lambda (c x) (= c (coerce x 'single-float))) colour cyan) :cyan))
                                   (< mid *earth-crash-radius*))))))
      (let ((low (colours (apsides-orbit 50d3 400d3))))
        (true (every (lambda (c) (eq (first c) (if (second c) :red :cyan))) low))
        (true (find :red low :key #'first))
        (true (find :cyan low :key #'first)))
      (true (every (lambda (c) (eq (first c) :cyan)) (colours (start-orbit)))))))

(define-test ribbon-precision
  (let ((orbit (start-orbit)) (state (start-map-state)))
    (multiple-value-bind (view camera) (ship-focus-map state)
      (declare (ignore view))
      (let* ((ribbon (filled-ribbon orbit camera state))
             (eye (map-eye camera state))
             (r (lis::render-state-r state))
             (nearest (loop with best = 0 with least = most-positive-double-float
                            for s to (lis::ribbon-count ribbon)
                            for d = (lis::vec3-norm (lis::vec3-sub (ribbon-point ribbon s) r))
                            when (< d least) do (setf best s least d)
                            finally (return best)))
             (p (ribbon-point ribbon nearest))
             (double (lis::vec3-sub p eye))
             (out (lis::ribbon-instances ribbon))
             (o (if (< nearest (lis::ribbon-count ribbon))
                    (* nearest lis::+ribbon-instance-floats+)
                    (+ (* (1- nearest) lis::+ribbon-instance-floats+) 4))))
        (true (< (lis::vec3-norm double) 1000d0))
        (true (< (loop for i below 3 maximize (abs (- (aref out (+ o i)) (aref double i)))) 1d-3))
        (true (> (loop for i below 3
                       maximize (abs (- (- (coerce (aref p i) 'single-float)
                                           (coerce (aref eye i) 'single-float))
                                        (aref double i))))
                 1d-3))))))

(define-test ribbon-conses-nothing
  (let ((orbit (start-orbit)) (state (start-map-state)) (ribbon (lis::make-ribbon)))
    (multiple-value-bind (view camera) (ship-focus-map state)
      (declare (ignore view))
      (let ((eye (map-eye camera state)) (focal (lis::camera-focal-px camera 800)))
        (is = 0 (least-bytes-consed
                 (lambda () (dotimes (i 1000)
                              (lis::orbit-ribbon-points! ribbon orbit eye focal)
                              (lis::orbit-ribbon-instances! ribbon eye)))))))))

(define-test ribbon-budget
  ;; Over budget, k falls on the farthest segments first: the one nearest the
  ;; ship keeps the largest split.
  (let* ((orbit (start-orbit)) (state (start-map-state))
         (camera (nth-value 1 (ship-focus-map state)))
         (ribbon (let ((lis::*ribbon-max-segments* 400)) (filled-ribbon orbit camera state)))
         (splits (lis::ribbon-splits ribbon))
         (distances (lis::ribbon-distances ribbon))
         (nearest (position (reduce #'min distances) distances)))
    (true (<= (lis::ribbon-count ribbon) 400))
    (is = (reduce #'max splits) (aref splits nearest))
    (true (plusp (aref splits nearest)))))

;;; Line styles and the one buffer (decision 125: Map in milestone 3).

(defun double-pixel (camera eye p width height)
  "The pixel of P (relative to the same origin as EYE) in double, or nil behind the eye."
  (let* ((d (lis::vec3-sub p eye))
         (q (lis::camera-orientation camera))
         (f (/ (tan (* 0.5d0 (lis::camera-field-of-view camera)))))
         (forward (lis::quat-rotate q (lis::vec3 1 0 0)))
         (right (lis::quat-rotate q (lis::vec3 0 1 0)))
         (down (lis::quat-rotate q (lis::vec3 0 0 1)))
         (w (lis::vec3-dot d forward)))
    (when (plusp w)
      (list (* 0.5d0 width (+ 1 (/ (* f (/ height width) (lis::vec3-dot d right)) w)))
            (* 0.5d0 height (+ 1 (/ (* lis::+clip-y-sign+ f (lis::vec3-dot d down)) w)))))))

(define-test ribbon-instance-style
  (let* ((state (start-map-state))
         (ribbon (filled-ribbon (start-orbit) (nth-value 1 (opened-map state)) state))
         (out (lis::ribbon-instances ribbon)))
    (is = 16 lis::+ribbon-instance-floats+)
    (dotimes (s (lis::ribbon-count ribbon))
      (let ((o (* s lis::+ribbon-instance-floats+)))
        (is = -1f0 (aref out (+ o 12)))
        (is = -1f0 (aref out (+ o 13)))
        (is = 2f0 (aref out (+ o 14)))
        (is = 1f0 (aref out (+ o 15)))))))

(define-test ribbon-endpoints-bit-for-bit
  ;; the milestone-2 endpoints: point − eye in double, then single
  (let* ((state (start-map-state))
         (camera (nth-value 1 (opened-map state)))
         (ribbon (filled-ribbon (start-orbit) camera state))
         (eye (map-eye camera state))
         (out (lis::ribbon-instances ribbon))
         (expected (make-array 3 :element-type 'single-float)))
    (dotimes (s (lis::ribbon-count ribbon))
      (dotimes (end 2)
        (lis::eye-relative-single (ribbon-point ribbon (+ s end)) eye expected 0)
        (dotimes (i 3)
          (is = (aref expected i) (aref out (+ (* s lis::+ribbon-instance-floats+) (* 4 end) i))))))))

(define-test ribbon-dash-phases
  (let* ((state (start-map-state))
         (camera (nth-value 1 (opened-map state)))
         (extent '(1280 800))
         (ribbon (filled-ribbon (start-orbit) camera state))
         (eye (map-eye camera state))
         (out (lis::ribbon-instances ribbon))
         (count (lis::ribbon-count ribbon))
         (end (lis::dash-phases! out 0 count (lis::ribbon-points ribbon) eye camera extent 3d0))
         (total 3d0) (worst 0d0))
    (is = 3f0 (aref out 12))
    (dotimes (s count)
      (let* ((o (* s lis::+ribbon-instance-floats+))
             (a (double-pixel camera eye (ribbon-point ribbon s) 1280 800))
             (b (double-pixel camera eye (ribbon-point ribbon (1+ s)) 1280 800))
             (length (if (and a b) (sqrt (+ (expt (- (first a) (first b)) 2)
                                           (expt (- (second a) (second b)) 2)))
                         0d0))
             (period lis::*dash-period-px*))
        (setf worst (max worst (abs (- (- (aref out (+ o 13)) (aref out (+ o 12))) length))))
        ;; continues across segments, modulo the period
        (when (< (1+ s) count)
          (let ((gap (abs (- (aref out (+ o 13)) (aref out (+ o 12 lis::+ribbon-instance-floats+))))))
            (setf worst (max worst (min (mod gap period) (- period (mod gap period)))))))
        (incf total length)))
    (true (< worst 1d-3) "worst ~A px" worst)
    (true (< (abs (- end (mod total lis::*dash-period-px*))) 1d-3))))

(define-test ribbon-dash-behind-the-eye
  ;; an endpoint behind the eye adds nothing: phases stand still
  (let* ((state (start-map-state))
         (camera (nth-value 1 (ship-focus-map state)))
         (ribbon (filled-ribbon (start-orbit) camera state))
         (eye (map-eye camera state))
         (out (lis::ribbon-instances ribbon)))
    (lis::dash-phases! out 0 (lis::ribbon-count ribbon) (lis::ribbon-points ribbon) eye camera
                       '(1280 800) 0d0)
    (true (loop for s below (lis::ribbon-count ribbon)
                for o = (* s lis::+ribbon-instance-floats+)
                for a = (double-pixel camera eye (ribbon-point ribbon s) 1280 800)
                for b = (double-pixel camera eye (ribbon-point ribbon (1+ s)) 1280 800)
                always (or (and a b) (= (aref out (+ o 12)) (aref out (+ o 13))))))))

(define-test map-lines-fill-conses-nothing
  (let* ((state (start-map-state))
         (camera (nth-value 1 (ship-focus-map state)))
         (ribbon (lis::make-ribbon))
         (lines (lis::make-map-lines))
         (orbit (start-orbit))
         (eye (map-eye camera state))
         (focal (lis::camera-focal-px camera 800)))
    (flet ((fill-all ()
             (lis::map-lines-clear! lines)
             (lis::orbit-ribbon-points! ribbon orbit eye focal)
             (loop while (< (lis::map-lines-count lines) (lis::map-lines-capacity lines))
                   do (lis::map-lines-add-orbit! lines ribbon eye camera '(1280 800) :dashed t))))
      (fill-all)
      (is = (lis::map-lines-capacity lines) (lis::map-lines-count lines))
      (is = 0 (least-bytes-consed #'fill-all)))))

;;; The map's markers (decision 84).

(defun with-bodies (state)
  "STATE with the Sun and the Moon at its time, as render-state-update! sets them."
  (let ((scratch (lis::make-vec3)) (time (lis::render-state-t state)))
    (lis::body-state-into (lis::render-state-sun state) scratch :sun time :earth)
    (lis::body-state-into (lis::render-state-moon state) scratch :moon time :earth)
    state))

(defun opened-markers (state &key (extent '(1280 800)) (view-fn #'identity))
  "STATE's markers from the map's opening frame, VIEW-FN applied to the
view first; also the camera."
  (with-bodies state)
  (let ((view (funcall view-fn (opened-map state)))
        (camera (lis::make-camera)))
    (lis::map-camera-update! camera view state nil)
    (values (lis::map-markers state camera extent (lis::make-map-markers)) camera)))

(defun marker (markers key) (lis::map-marker-ref markers key))

(defun clip-pixel (camera point &optional (width 1280) (height 800))
  "POINT (relative to Earth's centre) in pixels through camera-clip-rows!."
  (let ((rows (make-array 16 :element-type 'single-float)))
    (lis::camera-clip-rows! rows camera (/ width height))
    (pixel rows (lis::vec3-sub point (lis::camera-offset camera)) width height)))

(defun at-pixel-p (marker camera point &optional (height 800))
  (multiple-value-bind (x y) (clip-pixel camera point 1280 height)
    (<= (sqrt (+ (expt (- (lis::map-marker-x marker) x) 2) (expt (- (lis::map-marker-y marker) y) 2)))
        0.5d0)))

(defun time-at-true-anomaly (orbit theta)
  (let ((e (lis::orbit-e orbit)))
    (+ (lis::orbit-tp orbit)
       (/ (if (< e 1)
              (let ((ea (lis::true->eccentric theta e))) (- ea (* e (sin ea))))
              (let ((ha (lis::true->hyperbolic theta e))) (- (* e (sinh ha)) ha)))
          (lis::orbit-n orbit)))))

(define-test map-markers-apsides
  ;; The ship at true anomaly 135°: the opening eye looks across the apse
  ;; line, so neither apsis is behind Earth.
  (let* ((orbit (test-orbit-400x2000))
         (tp (lis::orbit-tp orbit))
         (period (lis::orbit-period orbit)))
    (multiple-value-bind (markers camera)
        (opened-markers (orbit-render-state orbit (time-at-true-anomaly orbit (deg 135d0))))
      (let ((ap (marker markers :apoapsis)) (pe (marker markers :periapsis))
            (ship (marker markers :ship)))
        (true (and ap pe ship))
        (is string= "AP 2000 KM" (lis::map-marker-label ap))
        (is string= "PE 400 KM" (lis::map-marker-label pe))
        (is eq :diamond (lis::map-marker-kind ap))
        (is = #x40D0FF (lis::map-marker-colour pe))
        (true (at-pixel-p ap camera (lis::orbit-state orbit (+ tp (/ period 2)))))
        (true (at-pixel-p pe camera (lis::orbit-state orbit tp)))
        (is eq :ring (lis::map-marker-kind ship))
        (is string= "M-07" (lis::map-marker-label ship)))))
  (let* ((orbit (eccentric-orbit 1.5d0))
         (markers (opened-markers (orbit-render-state orbit (time-at-true-anomaly orbit (deg 90d0))))))
    (true (marker markers :periapsis))
    (is string= "PE 400 KM" (lis::map-marker-label (marker markers :periapsis)))
    (false (marker markers :apoapsis))))

(define-test map-markers-impact
  (let* ((orbit (apsides-orbit 50d3 400d3))
         (t-r (+ (lis::start-t) 600))
         (impact (marker (opened-markers (orbit-render-state orbit t-r)) :impact)))
    (true impact)
    (is eq :cross (lis::map-marker-kind impact))
    (is = #xFF3030 (lis::map-marker-colour impact))
    (is string= "IMPACT" (lis::map-marker-label impact))
    (let ((point (lis::map-marker-point impact)) (time (lis::map-marker-time impact)))
      (true (< (abs (- (lis::vec3-norm point) *earth-crash-radius*)) 1d-3))
      (true (> time t-r))
      (true (< (abs (- time (lis::orbit-radius-crossings orbit *earth-crash-radius* t-r))) 1d-3))
      ;; inbound: falling there
      (multiple-value-bind (r v) (lis::orbit-state orbit time)
        (true (< (lis::vec3-norm (lis::vec3-sub r point)) 1d0))
        (true (minusp (lis::vec3-dot r v))))))
  (false (marker (opened-markers (start-map-state)) :impact)))

(define-test map-markers-visibility
  ;; The ship at periapsis, the eye in the orbit plane opposite it.
  (let* ((orbit (test-orbit-400x2000))
         (state (orbit-render-state orbit (lis::orbit-tp orbit)))
         (markers (opened-markers state :view-fn (lambda (view)
                                                    (setf (lis::map-view-elevation view) 0d0
                                                          (lis::map-view-azimuth view) pi)
                                                    view))))
    (false (marker markers :periapsis))
    (true (marker markers :apoapsis))
    (true (marker markers :ship))))

(defun body-px (state camera key height)
  "KEY's projected diameter, px: H tan(asin(R/d)) / tan(fov/2)."
  (let* ((centre (ecase key
                   (:earth (lis::vec3 0 0 0))
                   (:moon (lis::render-state-moon state))
                   (:sun (lis::render-state-sun state))))
         (d (lis::vec3-norm (lis::vec3-sub centre (lis::camera-offset camera)))))
    (/ (* height (tan (asin (/ (lis::body-radius (lis::body key)) d))))
       (tan (* 0.5d0 (lis::camera-field-of-view camera))))))

(define-test map-markers-bodies
  (let ((state (start-map-state)))
    ;; The Moon is ~7.7 px at 800 px and ~3.9 px at 400 px.
    (loop for height in '(800 400)
          do (multiple-value-bind (markers camera) (opened-markers state :extent (list 1280 height))
               (false (marker markers :earth))
               (let ((px (body-px state camera :moon height))
                     (moon (marker markers :moon)))
                 (true (< 0.2d0 (* 2 (lis::rad->deg (asin (/ lis::+radius-moon+
                                                              (lis::vec3-norm (lis::render-state-moon state))))))
                          0.6d0))
                 (if (< px 6)
                     (progn (true moon "Moon at ~,1F px" px)
                            (is string= "MOON" (lis::map-marker-label moon))
                            (is = #x8A8A80 (lis::map-marker-colour moon))
                            (true (at-pixel-p moon camera (lis::render-state-moon state) height)))
                     (false moon "Moon at ~,1F px" px)))))
    (multiple-value-bind (markers camera)
        (opened-markers state :view-fn (lambda (view) (lis::map-view-zoom! view 1d10)))
      (true (< (abs (- 1.5d13 (lis::vec3-norm (lis::camera-offset camera)))) 1d0))
      (is string= "EARTH" (lis::map-marker-label (marker markers :earth)))
      (is string= "SUN" (lis::map-marker-label (marker markers :sun)))
      (is eq :dot (lis::map-marker-kind (marker markers :sun)))
      ;; both labels kept, apart (decision 103: Map labels: placement order and overlap)
      (lis::place-map-labels! markers)
      (let ((earth (marker markers :earth)) (sun (marker markers :sun)))
        (true (and (lis::map-marker-label-side earth) (lis::map-marker-label-side sun)))
        (false (labels-overlap-p earth sun))))))

(defun labels-overlap-p (a b)
  (flet ((span (m) (let ((x (lis::map-marker-label-x m)))
                     (list x (+ x (lis::label-width (lis::map-marker-label m)))))))
    (destructuring-bind (a0 a1) (span a)
      (destructuring-bind (b0 b1) (span b)
        (and (< a0 b1) (< b0 a1)
             (< (abs (- (lis::map-marker-y a) (lis::map-marker-y b))) (* 7 lis::*label-dot-px*)))))))

(define-test map-labels-placement
  ;; The ship at apoapsis: M-07 right of the ring, AP's label to the left.
  (let* ((orbit (test-orbit-400x2000))
         (markers (opened-markers (orbit-render-state orbit (+ (lis::orbit-tp orbit)
                                                                (/ (lis::orbit-period orbit) 2))))))
    (lis::place-map-labels! markers)
    (let ((ship (marker markers :ship)) (ap (marker markers :apoapsis)))
      (true (and ship ap))
      (is eq :right (lis::map-marker-label-side ship))
      (is = (+ (lis::map-marker-x ship) 8) (lis::map-marker-label-x ship))
      (is eq :left (lis::map-marker-label-side ap))
      (is = (- (lis::map-marker-x ap) 8) (+ (lis::map-marker-label-x ap)
                                             (lis::label-width "AP 2000 KM")))
      (dotimes (i (lis::map-markers-count markers))
        (dotimes (j i)
          (let ((a (svref (lis::map-markers-items markers) i))
                (b (svref (lis::map-markers-items markers) j)))
            (when (and (lis::map-marker-label-side a) (lis::map-marker-label-side b))
              (false (labels-overlap-p a b)))))))))

(define-test map-marker-labels-have-glyphs
  (let ((labels '()))
    (flet ((collect (markers)
             (dotimes (i (lis::map-markers-count markers))
               (push (copy-seq (lis::map-marker-label (svref (lis::map-markers-items markers) i)))
                     labels))))
      (collect (opened-markers (orbit-render-state (apsides-orbit 50d3 400d3) (lis::start-t))))
      (collect (opened-markers (orbit-render-state (eccentric-orbit 1.5d0) (lis::start-t))))
      (collect (opened-markers (start-map-state)
                               :extent '(1280 400)
                               :view-fn (lambda (view) (lis::map-view-zoom! view 1d10)))))
    (dolist (want '("M-07" "IMPACT" "EARTH" "SUN"))
      (true (member want labels :test #'string=) "no ~A" want))
    (dolist (label labels)
      (loop for c across label
            do (true (lis::glyph c) "~S in ~S has no glyph" c label)))
    (is string= "PE -1234 KM"
        (lis::write-altitude-label! (lis::label-buffer) "PE " (- lis::+radius-earth+ 1234.4d3)))
    (is string= "AP 0 KM" (lis::write-altitude-label! (lis::label-buffer) "AP " lis::+radius-earth+))))

(define-test map-markers-cons-nothing
  (let ((state (with-bodies (orbit-render-state (apsides-orbit 50d3 400d3) (+ (lis::start-t) 600))))
        (markers (lis::make-map-markers))
        (extent (list 1280 800)))
    (multiple-value-bind (view camera) (opened-map state)
      (declare (ignore view))
      (lis::map-markers state camera extent markers)
      (true (lis::map-marker-ref markers :impact))
      (let ((out (lis::make-overlay-buffer)))
        (is = 0 (least-bytes-consed
                 (lambda () (dotimes (i 1000)
                              (lis::map-markers state camera extent markers)
                              (lis::map-marker-vertices! (lis::overlay-clear! out) markers)))
                 5))))))

;;; The predicted trajectory (decision 125: Map in milestone 3; decision
;;; 116: Prediction in milestone 3: horizon, target nodes, closest approach,
;;; relative nodes, budget).

(defun trajectory-fixture (sim &key (height 800))
  "(values state camera pred ribbon eye focal) for SIM at its start: the
clock's prediction run to its horizon, the map opened on the render state."
  (let* ((clock (lis::new-clock sim))
         (state (with-bodies (lis::render-state-update! (lis::make-render-state) clock sim)))
         (pred (lis::clock-prediction clock)))
    (lis::prediction-run! pred sim)
    (let ((camera (nth-value 1 (opened-map state)))
          (ribbon (lis::make-ribbon)))
      (let ((eye (map-eye camera state)))
        (lis::trajectory-ribbon-points! ribbon pred state eye (lis::camera-focal-px camera height))
        (values state camera pred ribbon eye (lis::camera-focal-px camera height))))))

(defun ribbon-times (ribbon)
  (loop for s to (lis::ribbon-count ribbon) collect (aref (lis::ribbon-times ribbon) s)))

(define-test trajectory-points-on-the-prediction
  (multiple-value-bind (state camera pred ribbon) (trajectory-fixture (lis::new-game-sim))
    (declare (ignore camera))
    (true (plusp (lis::ribbon-count ribbon)))
    (true (equalp (ribbon-point ribbon 0) (lis::render-state-r state)))
    (is = (lis::render-state-t state) (aref (lis::ribbon-times ribbon) 0))
    (let ((out (lis::make-vec3)) (worst 0d0))
      (loop for s from 1 to (lis::ribbon-count ribbon)
            for time = (aref (lis::ribbon-times ribbon) s)
            do (true (lis::prediction-position! out pred time) "outside at ~F" time)
               (setf worst (max worst (/ (lis::vec3-norm (lis::vec3-sub out (ribbon-point ribbon s)))
                                         (lis::vec3-norm out)))))
      (true (< worst 1d-6) "worst relative ~A" worst))
    ;; the times rise, from t_r to the prediction's horizon
    (let ((times (ribbon-times ribbon)))
      (true (apply #'< times))
      (true (>= (car (last times)) (- (lis::prediction-end pred) 10))))))

(define-test trajectory-from-between-nodes
  ;; t_r between two stored nodes: r first, then the Hermite's own points
  (let* ((sim (lis::new-game-sim))
         (clock (lis::new-clock sim))
         (state (with-bodies (lis::render-state-update! (lis::make-render-state) clock sim)))
         (pred (lis::clock-prediction clock)))
    (lis::prediction-run! pred sim)
    (let ((time (+ (lis::start-t) 700.37d0)))
      (setf (lis::render-state-t state) time)
      (lis::prediction-position! (lis::render-state-r state) pred time)
      (let* ((camera (nth-value 1 (opened-map state)))
             (ribbon (lis::make-ribbon))
             (eye (map-eye camera state)))
        (true (lis::trajectory-ribbon-points! ribbon pred state eye (lis::camera-focal-px camera 800)))
        (true (equalp (ribbon-point ribbon 0) (lis::render-state-r state)))
        (let ((times (ribbon-times ribbon)) (out (lis::make-vec3)))
          (is = time (first times))
          (true (apply #'< times))
          (loop for s from 1 to (lis::ribbon-count ribbon)
                do (lis::prediction-position! out pred (nth s times))
                   (true (< (lis::vec3-norm (lis::vec3-sub out (ribbon-point ribbon s))) 1d-3))))))))

(defun piece-error-px (pred ribbon s eye focal)
  "The worst distance, in px from EYE, of the prediction's path between ribbon
points S and S + 1 from their chord, at 8 times."
  (let* ((a (ribbon-point ribbon s)) (b (ribbon-point ribbon (1+ s)))
         (t0 (aref (lis::ribbon-times ribbon) s)) (t1 (aref (lis::ribbon-times ribbon) (1+ s)))
         (d (lis::vec3-sub b a)) (dd (lis::vec3-dot d d)) (worst 0d0) (p (lis::make-vec3)))
    (loop for m from 1 to 8
          do (lis::prediction-position! p pred (+ t0 (* (- t1 t0) (/ m 9d0))))
             (let* ((u (max 0d0 (min 1d0 (/ (lis::vec3-dot (lis::vec3-sub p a) d) dd))))
                    (off (lis::vec3-norm (lis::vec3-sub p (lis::vec3-add-scaled a d u)))))
               (setf worst (max worst (/ (* off focal) (lis::vec3-norm (lis::vec3-sub p eye)))))))
    worst))

(define-test trajectory-piece-error
  ;; Every piece within 0.25 px of the stored path unless the budget lowered it
  (dolist (sim (list (lis::new-game-sim)
                     (orbit-sim (apsides-orbit 300d3 2000d3))))
    (multiple-value-bind (state camera pred ribbon eye focal) (trajectory-fixture sim)
      (declare (ignore state camera))
      (let ((limit (max 0.25d0 (lis::ribbon-bound ribbon))) (worst 0d0))
        (loop for s from 1 below (lis::ribbon-count ribbon)
              do (setf worst (max worst (piece-error-px pred ribbon s eye focal))))
        (true (<= worst (* 1.02d0 limit)) "worst ~,3F px, bound ~,3F" worst limit)))))

(define-test trajectory-merges-and-splits
  ;; Far away the dense nodes merge into runs; up close single intervals split.
  (multiple-value-bind (state camera pred ribbon eye focal) (trajectory-fixture (lis::new-game-sim))
    (declare (ignore camera eye focal))
    (true (< (lis::ribbon-count ribbon) (/ (lis::prediction-count pred) 4))
          "~D segments for ~D nodes" (lis::ribbon-count ribbon) (lis::prediction-count pred))
    (multiple-value-bind (view camera) (ship-focus-map state)
      (declare (ignore view))
      (let ((close (lis::make-ribbon)))
        (lis::trajectory-ribbon-points! close pred state (map-eye camera state)
                                        (lis::camera-focal-px camera 800))
        (true (> (lis::ribbon-count close) (lis::ribbon-count ribbon))
              "~D segments close, ~D far" (lis::ribbon-count close) (lis::ribbon-count ribbon))
        (true (<= (lis::ribbon-count close) 8192))))))

(define-test trajectory-budget
  ;; 20,000 nodes fit 8,192 segments; a small budget raises the bound
  (let* ((sim (lis::new-game-sim))
         (state (with-bodies (lis::render-state-update! (lis::make-render-state)
                                                         (lis::new-clock sim) sim)))
         (pred (lis::new-prediction :until (+ (lis::start-t) (* 9 (lis::osculating-period sim))))))
    (lis::prediction-run! pred sim)
    (setf (lis::render-state-prediction state) pred)
    (progn
      (is = lis::*prediction-capacity* (lis::prediction-count pred))
      (let ((ribbon (lis::make-ribbon)))
        (multiple-value-bind (view camera) (opened-map state)
          (declare (ignore view))
          (lis::trajectory-ribbon-points! ribbon pred state (map-eye camera state)
                                          (lis::camera-focal-px camera 800)))
        (true (<= 1 (lis::ribbon-count ribbon) 8192)))
      (multiple-value-bind (view camera) (ship-focus-map state)
        (declare (ignore view))
        (let ((close (lis::make-ribbon)) (eye (map-eye camera state))
              (focal (lis::camera-focal-px camera 800)))
          (true (lis::trajectory-ribbon-points! close pred state eye focal))
          (true (<= (lis::ribbon-count close) 8192))
          (let ((tight (let ((lis::*ribbon-max-segments* 200))
                         (lis::make-ribbon :capacity 200))))
            (true (lis::trajectory-ribbon-points! tight pred state eye focal))
            (true (<= (lis::ribbon-count tight) 200))
            (true (> (lis::ribbon-bound tight) lis::*ribbon-max-error-px*))
            (let ((worst 0d0))
              (loop for s from 1 below (lis::ribbon-count tight)
                    do (setf worst (max worst (piece-error-px pred tight s eye focal))))
              (true (<= worst (* 1.02d0 (lis::ribbon-bound tight)))
                    "worst ~,3F px, bound ~,3F" worst (lis::ribbon-bound tight)))))))))

(defun deorbit-trajectory-sim ()
  "The deorbit's dive at its apoapsis: periapsis 50 km below the crash radius."
  (orbit-sim (dive-orbit (- *earth-crash-radius* 50d3))))

(define-test trajectory-ends-red-at-the-impact
  (let ((sim (deorbit-trajectory-sim)))
    (multiple-value-bind (state camera pred ribbon eye) (trajectory-fixture sim)
      (declare (ignore camera))
      (true (lis::prediction-impact pred))
      (let* ((count (lis::ribbon-count ribbon))
             (end (ribbon-point ribbon count))
             (loss-r (lis::ship-state-r (lis::sim-ship (lis::prediction-sim pred)))))
        (true (< (lis::vec3-norm (lis::vec3-sub end loss-r)) 1d-6))
        (is = (lis::loss-time (lis::prediction-impact pred))
            (aref (lis::ribbon-times ribbon) count))
        ;; the piece that ends at the crash radius is red, the others cyan
        (let ((lines (lis::make-map-lines)))
          (lis::orbit-ribbon-instances! ribbon eye :out (lis::map-lines-instances lines))
          (let* ((out (lis::map-lines-instances lines))
                 (red (lis::srgb-channel-linear lis::*ribbon-crash-colour* 16))
                 (cyan-red (lis::srgb-channel-linear lis::*ribbon-colour* 16)))
            (is = (coerce red 'single-float) (aref out (+ (* (1- count) 16) 8)))
            (loop for s below (1- count)
                  do (is = (coerce cyan-red 'single-float) (aref out (+ (* s 16) 8))))))
        ;; the marker at the loss point, where the map can see it
        (let ((found nil))
          (loop for az from 0 below 360 by 30
                until found
                do (multiple-value-bind (view camera) (opened-map state)
                     (declare (ignore camera))
                     (setf (lis::map-view-azimuth view) (deg az))
                     (let ((camera (lis::map-camera-update! (lis::make-camera) view state nil)))
                       (setf found (marker (lis::map-markers state camera '(1280 800)
                                                              (lis::make-map-markers))
                                           :impact)))))
          (true found)
          (when found
            (true (< (lis::vec3-norm (lis::vec3-sub (lis::map-marker-point found) loss-r)) 1d0))
            (is = (lis::loss-time (lis::prediction-impact pred)) (lis::map-marker-time found))))))))

(define-test trajectory-fallback-to-the-osculating-orbit
  (let* ((sim (lis::new-game-sim))
         (state (with-bodies (lis::render-state-update! (lis::make-render-state)
                                                         (lis::new-clock sim) sim)))
         (camera (nth-value 1 (opened-map state)))
         (ribbon (lis::make-ribbon)) (lines (lis::make-map-lines))
         (eye (map-eye camera state)) (extent '(1280 800)))
    ;; the clock's prediction has its first node only: no trajectory
    (false (lis::trajectory-ribbon-points! ribbon (lis::new-prediction) state eye 1000d0))
    (setf (lis::render-state-prediction state) (lis::new-prediction))
    (lis::map-lines-fill! lines ribbon state camera eye extent)
    (let ((osculating (filled-ribbon (start-orbit) camera state)))
      (is = (lis::ribbon-count osculating) (lis::map-lines-count lines)))
    ;; and without a reference at all
    (setf (lis::render-state-prediction state) nil)
    (lis::map-lines-fill! lines ribbon state camera eye extent)
    (true (>= (lis::map-lines-count lines) 256))
    ;; the osculating impact marker stays where no trajectory is drawn
    (let ((impact-state (orbit-render-state (apsides-orbit 50d3 400d3) (+ (lis::start-t) 600))))
      (true (marker (opened-markers impact-state) :impact)))))

(define-test trajectory-conses-nothing
  (multiple-value-bind (state camera pred ribbon eye focal) (trajectory-fixture (lis::new-game-sim))
    (let ((lines (lis::make-map-lines)))
      (setf (lis::render-state-prediction state) pred)
      (lis::map-lines-fill! lines ribbon state camera eye '(1280 800))
      (is = 0 (least-bytes-consed
               (lambda () (dotimes (i 20)
                            (lis::trajectory-ribbon-points! ribbon pred state eye focal)))
               5))
      (is = 0 (least-bytes-consed
               (lambda () (dotimes (i 20)
                            (lis::map-lines-fill! lines ribbon state camera eye '(1280 800))))
               5)))))

#+(or)
(progn
  ;; The worst case per frame, outside scripts/test: 20,000 nodes, the eye
  ;; near the ship (many splits). Evaluate in the test package.
  (let* ((sim (lis::new-game-sim))
         (state (with-bodies (lis::render-state-update! (lis::make-render-state)
                                                         (lis::new-clock sim) sim)))
         (pred (lis::new-prediction :until (+ (lis::start-t) (* 9 (lis::osculating-period sim))))))
    (lis::prediction-run! pred sim)
    (setf (lis::render-state-prediction state) pred)
    (multiple-value-bind (view camera) (ship-focus-map state)
      (declare (ignore view))
      (let ((ribbon (lis::make-ribbon)) (eye (map-eye camera state))
            (focal (lis::camera-focal-px camera 800)))
        (time (dotimes (i 1000) (lis::trajectory-ribbon-points! ribbon pred state eye focal)))))))

;;; The target, the candidates, closest approach and the relative nodes
;;; (decision 125: Map in milestone 3).

(defun target-markers (sim &key (setup #'identity))
  "(values markers camera pred state) for SIM at its start with its prediction
run; SETUP is called with the state before the markers."
  (multiple-value-bind (state camera pred) (trajectory-fixture sim)
    (funcall setup state)
    (values (lis::map-markers state camera '(1280 800) (lis::make-map-markers)) camera pred state)))

(defun visible-markers (sim keys)
  "(values markers camera pred) for SIM from the first of several azimuths,
looking down from 80°, that shows every marker of KEYS (Earth hides some)."
  (multiple-value-bind (state camera pred) (trajectory-fixture sim)
    (declare (ignore camera))
    (let ((view (opened-map state)))
      (setf (lis::map-view-elevation view) (lis::deg->rad 80d0))
      (loop for degrees from 0 below 360 by 15
            do (setf (lis::map-view-azimuth view) (lis::deg->rad degrees))
               (let* ((camera (lis::map-camera-update! (lis::make-camera) view state nil))
                      (markers (lis::map-markers state camera '(1280 800) (lis::make-map-markers))))
                 (when (every (lambda (key) (marker markers key)) keys)
                   (return (values markers camera pred))))
            finally (error "No azimuth shows ~S." keys)))))

(defun candidate-marker (markers key)
  (loop for i below (lis::map-markers-count markers)
        for m = (svref (lis::map-markers-items markers) i)
        when (and (eq (lis::map-marker-key m) :candidate) (eq (lis::map-marker-target m) key))
          return m))

(defun tern-sim ()
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-model sim) :point)
    (lis::sim-set-target! sim :tern-station)
    sim))

(define-test map-markers-target-and-candidates
  (multiple-value-bind (markers camera pred state) (target-markers (tern-sim))
    (declare (ignore pred))
    (let ((target (marker markers :target)))
      (true target)
      (is eq :box (lis::map-marker-kind target))
      (is = #xE040E0 (lis::map-marker-colour target))
      (is = 12 (lis::map-marker-size target))
      (is string= "TERN STATION" (lis::map-marker-label target))
      (true (at-pixel-p target camera (lis::render-state-target-r state)))
      (false (candidate-marker markers :tern-station)))
    (let ((candidate (candidate-marker markers :highport)))
      (true candidate)
      (is eq :box (lis::map-marker-kind candidate))
      (is = #x8A8A80 (lis::map-marker-colour candidate))
      (is = 8 (lis::map-marker-size candidate))
      (is string= "HIGHPORT" (lis::map-marker-label candidate)))
    ;; Skua Yard's drop point is a candidate too
    (true (candidate-marker markers :skua-yard)))
  ;; behind Earth: the target's box stays, a candidate's goes
  (let ((behind (lambda (state)
                  (let* ((m (lis::make-map-markers))
                         (camera (nth-value 1 (opened-map state))))
                    (lis::map-markers state camera '(1280 800) m)
                    (let ((p (lis::vec3-scale (lis::vec3-normalize (lis::map-markers-eye m))
                                              -7d6))
                          (i (position :fulmar-depot lis::*objects* :key #'lis::object-def-key)))
                      (replace (lis::render-state-target-r state) p)
                      (replace (lis::object-render-r (svref (lis::render-state-objects state) i)) p))))))
    (multiple-value-bind (markers) (target-markers (tern-sim) :setup behind)
      (true (marker markers :target))
      (false (candidate-marker markers :fulmar-depot))))
  ;; a candidate whose bound projects to 6 px or more has no box
  (let ((near (lambda (state)
                (let* ((m (lis::make-map-markers))
                       (camera (nth-value 1 (opened-map state))))
                  (lis::map-markers state camera '(1280 800) m)
                  (let* ((i (position :highport lis::*objects* :key #'lis::object-def-key))
                         (bound (lis::object-def-bound (lis::object-render-def
                                                        (svref (lis::render-state-objects state) i)))))
                    (replace (lis::object-render-r (svref (lis::render-state-objects state) i))
                             (lis::vec3-add-scaled (lis::map-markers-eye m) (lis::map-markers-forward m)
                                                   (* 20 bound))))))))
    (multiple-value-bind (markers) (target-markers (tern-sim) :setup near)
      (false (candidate-marker markers :highport))
      (true (candidate-marker markers :k-17)))))

(define-test map-markers-closest-approach
  (let ((sim (fulmar-sim)))
    (kepler-target! sim :fulmar-depot)
    (dolist (key '(:closest-approach :closest-approach-target))
      (multiple-value-bind (markers camera pred) (visible-markers sim (list key))
        (multiple-value-bind (time distance) (lis::prediction-closest-approach pred)
          (true time)
          (let ((marker (marker markers key)) (out (lis::make-vec3)))
            (is eq :triangle (lis::map-marker-kind marker))
            (if (eq key :closest-approach)
                (progn
                  (is = #x40D0FF (lis::map-marker-colour marker))
                  (is string= (lis::write-distance-label! (lis::label-buffer) "CA " distance)
                      (lis::map-marker-label marker))
                  (true (lis::prediction-position! out pred time)))
                (progn
                  (is = #xE040E0 (lis::map-marker-colour marker))
                  (false (lis::map-marker-label marker))
                  (true (lis::prediction-target-position! out pred time))))
            (true (< (lis::vec3-norm (lis::vec3-sub out (lis::map-marker-point marker))) 1d-6))
            (true (at-pixel-p marker camera out)))))))
  (flet ((label (d) (copy-seq (lis::write-distance-label! (lis::label-buffer) "CA " d))))
    (is string= "CA 340 M" (label 340d0))
    (is string= "CA 12.3 KM" (label 12345d0))
    (is string= "CA 2370 KM" (label 2.37d6))
    (is string= "CA 1.0 KM" (label 999.6d0))))

(define-test map-markers-relative-nodes
  (let* ((sim (lis::new-game-sim))
         (ship (lis::sim-ship sim))
         (t0 (lis::sim-t sim))
         (orbit (lis::state->orbit (lis::ship-state-r ship) (lis::ship-state-v ship)
                                   lis::+mu-earth+ t0))
         (turn (lis::quat-from-axis-angle (lis::vec3 0 0 1) (lis::deg->rad 1d0))))
    (setf (lis::sim-model sim) :point)
    (multiple-value-bind (r v) (lis::orbit-state orbit (+ t0 2000))
      (rails-target! sim :tern-station (lis::quat-rotate turn r) (lis::quat-rotate turn v)))
    (dolist (key '(:ascending :descending))
      (multiple-value-bind (markers camera pred) (visible-markers sim (list key))
        (let ((node (marker markers key)) (out (lis::make-vec3))
              (time (lis::prediction-relative-node pred key)))
          (is string= (if (eq key :ascending) "AN 1.0°" "DN 1.0°") (lis::map-marker-label node))
          (is eq :diamond (lis::map-marker-kind node))
          (is = #xE040E0 (lis::map-marker-colour node))
          (is = time (lis::map-marker-time node))
          (true (lis::prediction-position! out pred time))
          (true (< (lis::vec3-norm (lis::vec3-sub out (lis::map-marker-point node))) 1d-6))
          (true (at-pixel-p node camera out))))))
  (flet ((label (prefix deg)
           (copy-seq (lis::write-angle-label! (lis::label-buffer) prefix (lis::deg->rad deg)))))
    (is string= "AN 0.25°" (label "AN " 0.25d0))
    (is string= "DN 5.5°" (label "DN " 5.5d0))
    (is string= "AN 23°" (label "AN " 23.4d0))))

(define-test map-markers-skua-yard-cluster
  ;; Highport and Skua Yard, 1 px apart: their labels do not overlap, the
  ;; target's stays.
  (let ((sim (lis::new-game-sim)))
    (setf (lis::sim-model sim) :point)
    (lis::sim-set-target! sim :skua-yard)
    (multiple-value-bind (state camera) (trajectory-fixture sim)
      (declare (ignore camera))
      (let* ((view (opened-map state))
             (camera (progn (lis::map-view-zoom! view 30d0)
                            (lis::map-camera-update! (lis::make-camera) view state nil)))
             (markers (lis::map-markers state camera '(1280 800) (lis::make-map-markers))))
        (lis::place-map-labels! markers)
        (let ((target (marker markers :target)))
          (true target)
          (true (lis::map-marker-label-side target)))
        (dotimes (i (lis::map-markers-count markers))
          (dotimes (j i)
            (let ((a (svref (lis::map-markers-items markers) i))
                  (b (svref (lis::map-markers-items markers) j)))
              (when (and (lis::map-marker-label-side a) (lis::map-marker-label-side b))
                (false (labels-overlap-p a b))))))))))

(define-test map-markers-new-labels-have-glyphs
  (dolist (label '("CA 340 M" "CA 12.3 KM" "AN 1.0°" "DN 0.25°" "BURN 1 12.5 M/S" "AIM"
                   "TERN STATION" "FULMAR DEPOT" "DERELICT K-17" "HIGHPORT" "SKUA YARD"))
    (loop for c across label
          do (true (lis::glyph c) "~S in ~S has no glyph" c label)))
  (dolist (def lis::*objects*)
    (loop for c across (lis::object-def-name def)
          do (true (lis::glyph c) "~S in ~S has no glyph" c (lis::object-def-name def)))))

(define-test map-pick-nearest
  (multiple-value-bind (markers) (target-markers (tern-sim))
    (let* ((target (marker markers :target))
           (x (lis::map-marker-x target)) (y (lis::map-marker-y target)))
      (is eq target (lis::map-pick markers (+ x 6) (- y 6)))
      (false (lis::map-pick markers (+ x 11) y))
      (false (lis::map-pick markers 5 790))
      (let ((highport (candidate-marker markers :highport)))
        (is eq highport (lis::map-pick markers (lis::map-marker-x highport)
                                       (lis::map-marker-y highport))))
      ;; the ship's ring is clickable, the Moon or the Sun is not
      (let ((ship (marker markers :ship)))
        (is eq ship (lis::map-pick markers (lis::map-marker-x ship) (lis::map-marker-y ship)))))
    (is = 0 (least-bytes-consed
             (lambda () (dotimes (i 1000) (lis::map-pick markers 100.5d0 200.5d0)))
             5))))

(define-test map-markers-target-cons-nothing
  ;; With a target and the candidates; the prediction's queries (closest
  ;; approach, nodes) return boxed doubles, a few dozen bytes a frame.
  (multiple-value-bind (markers camera pred state) (target-markers (tern-sim))
    (declare (ignore pred))
    (setf (lis::render-state-prediction state) nil)
    (let ((out (lis::make-overlay-buffer)))
      (lis::map-markers state camera '(1280 800) markers)
      (is = 0 (least-bytes-consed
               (lambda () (dotimes (i 1000)
                            (lis::map-markers state camera '(1280 800) markers)
                            (lis::map-marker-vertices! (lis::overlay-clear! out) markers)))
               5)))))

;;; The plan on the map: the dashed path, the burn markers, the goal orbit
;;; and the kept plan (decision 125: Map in milestone 3; the Hohmann 400 → 800
;;; km plan of the autopilot's tests, :point).

(defun plan-fixture (mode &key (goal 800d3) advance-to)
  "(values state camera pred model plan job sim lines eye) with the Hohmann
plan to GOAL m shown (:shown), engaged and flown to ADVANCE-TO if given
(:engaged) or engaged and then disengaged (:kept); the lines filled."
  (let* ((sim (model-sim :point))
         (job (lis::new-autopilot-job sim))
         (plan (plan-now sim :change-orbit (list goal) job)))
    (when (member mode '(:engaged :kept))
      (lis::sim-engage-plan! sim plan)
      (when advance-to (step-sim sim 10000000 1000 advance-to)))
    (let* ((clock (lis::new-clock sim))
           (model (lis::clock-autopilot clock)))
      (lis::autopilot-keep-shot! model plan (lis::autopilot-job-shot job))
      (ecase mode
        (:shown (setf (lis::autopilot-model-plan model) plan)
         (lis::autopilot-set-state! model :shown))
        (:engaged (lis::autopilot-engaged! model plan))
        (:kept (lis::autopilot-engaged! model plan)
         (lis::autopilot-stopped! model "AUTOPILOT: ABORT")))
      (let* ((state (with-bodies (lis::render-state-update! (lis::make-render-state) clock sim)))
             (pred (lis::clock-prediction clock)))
        (lis::prediction-run! pred sim)
        (let* ((view (opened-map state))
               (camera (progn (setf (lis::map-view-elevation view) (deg 90)
                                    (lis::map-view-distance view) 1d8 (lis::map-view-target view) 1d8)
                              (lis::map-camera-update! (lis::make-camera) view state nil)))
               (eye (map-eye camera state))
               (lines (lis::make-map-lines)))
          (lis::map-lines-fill! lines (lis::make-ribbon) state camera eye '(1280 800))
          (values state camera pred model plan job sim lines eye))))))

(defun line-segments (lines &key width dashed)
  "The instances of LINES (as lists of their floats) of WIDTH px (default
any), DASHED (true: a phase ≥ 0; :solid: −1; default any), in order."
  (let ((out (lis::map-lines-instances lines)))
    (loop for s below (lis::map-lines-count lines)
          for o = (* s lis::+ribbon-instance-floats+)
          for phase = (aref out (+ o 12))
          when (and (or (null width) (= width (aref out (+ o 14))))
                    (or (null dashed) (if (eq dashed :solid) (< phase 0) (>= phase 0))))
            collect (loop for k below lis::+ribbon-instance-floats+ collect (aref out (+ o k))))))

(defun segment-from (segment eye)
  "The instance's from point relative to Earth's centre."
  (lis::vec3-add (lis::vec3 (first segment) (second segment) (third segment)) eye))

(defun plan-point-near-p (segment eye pred time)
  "SEGMENT's from point lies on PRED's path at TIME (single precision at its distance from the eye)."
  (let ((expected (lis::make-vec3)))
    (and (lis::prediction-position! expected pred time)
         (< (lis::vec3-norm (lis::vec3-sub (segment-from segment eye) expected))
            (+ 1d0 (* 1d-6 (lis::vec3-norm (lis::vec3-sub expected eye))))))))

(define-test autopilot-keeps-the-final-shot
  (let* ((sim (model-sim :point))
         (clock (lis::new-clock sim))
         (model (lis::clock-autopilot clock)))
    (is eq nil (lis::autopilot-map-plan model))
    (lis::autopilot-start-job! model clock sim :change-orbit '(800d3) nil)
    (lis::autopilot-job-work! (lis::autopilot-model-job model) most-positive-fixnum)
    (lis::autopilot-job-poll! model clock sim)
    (multiple-value-bind (plan shot mode) (lis::autopilot-map-plan model)
      (is eq :shown mode)
      (is eq plan (lis::autopilot-model-plan model))
      (let ((job-shot (lis::autopilot-job-shot (lis::autopilot-model-job model))))
        (true shot)
        (false (eq shot job-shot))
        (is = (lis::prediction-count job-shot) (lis::prediction-count shot))
        (true (equalp (subseq (lis::prediction-nodes job-shot) 0 (* lis::+node-size+ (lis::prediction-count job-shot)))
                      (subseq (lis::prediction-nodes shot) 0 (* lis::+node-size+ (lis::prediction-count shot)))))
        (is = (lis::prediction-end job-shot) (lis::prediction-end shot))
        (is = (lis::prediction-cut-count job-shot) (lis::prediction-cut-count shot))))
    (lis::autopilot-engage-shown! model clock sim)
    (is eq :engaged (nth-value 2 (lis::autopilot-map-plan model)))
    (lis::autopilot-stopped! model "AUTOPILOT: ABORT")
    (is eq :kept (nth-value 2 (lis::autopilot-map-plan model)))
    (lis::autopilot-reset! model)
    (is eq nil (lis::autopilot-map-plan model))))

(define-test map-plan-shown
  (multiple-value-bind (state camera pred model plan job sim lines eye) (plan-fixture :shown)
    (declare (ignore camera sim))
    (is eq :shown (nth-value 2 (lis::autopilot-map-plan model)))
    (let* ((burns (lis::burn-plan-burns plan))
           (solid (line-segments lines :width 2f0 :dashed :solid))
           (dashed (line-segments lines :dashed t))
           (shot (lis::autopilot-job-shot job)))
      (true solid) (true dashed)
      ;; the solid line is the coast prediction to its horizon, the dashes start on the shot at the first burn
      (true (< (lis::vec3-norm (lis::vec3-sub (segment-from (first solid) eye) (lis::render-state-r state)))
               (+ 1d0 (* 1d-6 (lis::vec3-norm (lis::vec3-sub (lis::render-state-r state) eye))))))
      (is = 0 (length (line-segments lines :width 1f0 :dashed t)))
      (let ((first-dash (first dashed)))
        (true (plan-point-near-p first-dash eye shot (lis::planned-burn-start (svref burns 0)))
              "the first dash at the first burn's start"))
      (is = 0f0 (nth 12 (first dashed)))
      ;; the thin goal orbit, solid
      (true (line-segments lines :width 1f0 :dashed :solid)))))

(define-test map-plan-engaged
  (multiple-value-bind (state camera pred model plan job sim lines eye) (plan-fixture :engaged)
    (declare (ignore state camera model job sim))
    (let* ((burns (lis::burn-plan-burns plan))
           (dashed (line-segments lines :width 2f0 :dashed t)))
      (true dashed)
      (true (plan-point-near-p (first dashed) eye pred (lis::planned-burn-start (svref burns 0)))
            "the dashes start at the next burn's start"))))

(define-test map-plan-after-the-first-cut
  (let* ((probe (let ((sim (model-sim :point)))
                  (plan-now sim :change-orbit '(800d3))))
         (burns (lis::burn-plan-burns probe))
         (between (* 0.5d0 (+ (lis::planned-burn-cut-t (svref burns 0))
                              (lis::planned-burn-start (svref burns 1))))))
    (multiple-value-bind (state camera pred model plan job sim lines eye)
        (plan-fixture :engaged :advance-to between)
      (declare (ignore camera model job sim))
      (let ((dashed (line-segments lines :width 2f0 :dashed t)))
        (true (<= (lis::planned-burn-cut-t (svref (lis::burn-plan-burns plan) 0))
                  (lis::render-state-t state)))
        (true dashed)
        (true (plan-point-near-p (first dashed) eye pred
                                 (lis::planned-burn-start (svref (lis::burn-plan-burns plan) 1)))
              "solid to the second burn")))))

(define-test map-plan-kept-is-grey
  (multiple-value-bind (state camera pred model plan job sim lines eye) (plan-fixture :kept)
    (declare (ignore pred model plan job sim eye state camera))
    (let ((grey (coerce (lis::srgb-channel-linear lis::*plan-kept-colour* 16) 'single-float))
          (dashed (line-segments lines :dashed t)))
      (true dashed)
      (true (every (lambda (segment) (= grey (nth 8 segment))) dashed) "every kept dash is grey")
      (true (every (lambda (segment) (= grey (nth 8 segment))) (line-segments lines :width 1f0))
            "the kept goal orbit is grey"))))

(defun plan-burn-markers (mode &rest args)
  "(values burn markers, plan, pred, shot, state) of a plan-fixture."
  (multiple-value-bind (state camera pred model plan job) (apply #'plan-fixture mode args)
    (declare (ignore job))
    (let ((markers (lis::map-markers state camera '(1280 800) (lis::make-map-markers))))
      (values (loop for i below (lis::map-markers-count markers)
                    for m = (svref (lis::map-markers-items markers) i)
                    when (eq (lis::map-marker-key m) :burn) collect m)
              plan pred (nth-value 1 (lis::autopilot-map-plan model)) state))))

(defun dv-label (n dv)
  (format nil "BURN ~D ~A M/S" n
          (cond ((>= dv 99.5d0) (format nil "~D" (round dv)))
                ((>= dv 0.995d0) (format nil "~,1F" dv))
                (t (format nil "~,2F" dv)))))

(define-test map-markers-burns
  ;; The camera looks from above, the markers on the far side of Earth are hidden.
  (flet ((check (mode &rest args)
           (multiple-value-bind (markers plan pred shot) (apply #'plan-burn-markers mode args)
             (let ((burns (lis::burn-plan-burns plan)) (point (lis::make-vec3)))
               (true markers)
               (dolist (marker markers)
                 (let* ((n (position (lis::map-marker-time marker) burns
                                     :key (lambda (b) (* 0.5d0 (+ (lis::planned-burn-start b)
                                                                  (lis::planned-burn-cut-t b))))))
                        (burn (svref burns n)))
                   (true n)
                   (true (or (and (eq mode :engaged) (lis::prediction-position! point pred (lis::map-marker-time marker)))
                             (lis::prediction-position! point shot (lis::map-marker-time marker))))
                   (true (< (lis::vec3-norm (lis::vec3-sub point (lis::map-marker-point marker))) 1d-6))
                   (is string= (dv-label (1+ n) (lis::planned-burn-dv burn)) (lis::map-marker-label marker))
                   (is = (if (eq mode :kept) lis::*plan-kept-colour* lis::*marker-colour*)
                       (lis::map-marker-colour marker))
                   (is eq :plus (lis::map-marker-kind marker))))
               (length markers)))))
    (is = 2 (check :shown))
    (is = 2 (check :engaged))
    (is = 2 (check :kept))
    (let* ((probe (plan-now (model-sim :point) :change-orbit '(800d3)))
           (burns (lis::burn-plan-burns probe))
           (between (* 0.5d0 (+ (lis::planned-burn-cut-t (svref burns 0))
                                (lis::planned-burn-start (svref burns 1))))))
      (is = 1 (check :engaged :advance-to between)))))

(define-test map-goal-orbit-at-the-goal-altitude
  (multiple-value-bind (state camera pred model plan) (plan-fixture :shown)
    (declare (ignore pred model))
    (let* ((ribbon (lis::make-ribbon))
           (eye (map-eye camera state)))
      (lis::orbit-ribbon-points! ribbon (lis::burn-plan-goal-orbit plan) eye
                                 (lis::camera-focal-px camera 800))
      (let ((worst 0d0))
        (dotimes (s (1+ (lis::ribbon-count ribbon)))
          (setf worst (max worst (abs (- (lis::vec3-norm (ribbon-point ribbon s))
                                         (+ lis::+radius-earth+ 800d3))))))
        (true (< worst 100d0) "goal orbit off the 800 km altitude by ~,3F m" worst)))))

(define-test map-lines-plan-cons-nothing
  (multiple-value-bind (state camera pred model plan job sim lines eye) (plan-fixture :shown)
    (declare (ignore pred model plan job sim))
    (let ((ribbon (lis::make-ribbon)))
      (lis::map-lines-fill! lines ribbon state camera eye '(1280 800))
      (is = 0 (least-bytes-consed
               (lambda () (dotimes (i 20) (lis::map-lines-fill! lines ribbon state camera eye '(1280 800))))
               5)))))
