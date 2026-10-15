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
