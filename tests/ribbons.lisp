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
