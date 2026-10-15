(in-package #:luv-in-space.tests)

;;; places.lisp: the objects and the places (docs/design.md "Earth-orbit
;;; points of interest", "Rigid bodies: attitude, mass, shapes and fixtures").

(defun object-def* (key) (lis::object-definition key))

(defun object-orbit (def)
  (lis::rails-segment-orbit (first (lis::object-def-rails def))))

(defun def-state (def time)
  "(values r v q) of DEF's object on its rails at TIME."
  (let ((r (lis::make-vec3)) (v (lis::make-vec3)) (q (lis::make-quat)) (w (lis::make-vec3)))
    (assert (lis::object-state-into! r v q w (lis::new-object def) time))
    (values r v q)))

;; The design's tables as printed: key, mass (kg), tensor entries (xx yy zz
;; xy xz yz), shapes ((cx cy cz sx sy sz) or (cx cy cz r)), fixtures ((pin
;; axis) …), bound (m). Rocks: r, I and the pin as the table rounds them.
(defparameter *design-table*
  '((:tern-station 85000 (3.59d6 6.46d6 9.88d6 0 0 0)
     ((1.33d0 0 0 32.5d0 4.2d0 4.2d0) (-2.92d0 0 0 4.0d0 28.0d0 1.5d0)
      (-2.92d0 26.5d0 0 8.0d0 25.0d0 0.5d0) (-2.92d0 -26.5d0 0 8.0d0 25.0d0 0.5d0))
     (((-10.92d0 0 -2.40d0) (0 0 -1))) 39.8d0)
    (:fulmar-depot 60000 (0.793d6 4.65d6 5.12d6 0 0 0)
     ((14.22d0 0 0 0.3d0 16.0d0 16.0d0) (2.77d0 0 0 21.5d0 13.8d0 6.5d0)
      (-11.98d0 0 0 8.0d0 1.5d0 8.0d0) (-13.98d0 0 -16.30d0 3.6d0 3.6d0 24.0d0))
     (((-13.98d0 0 4.30d0) (0 0 1)) ((-13.98d0 0 -4.30d0) (0 0 -1))) 33.7d0)
    (:k-17 2500 (4.89d3 5.65d4 5.65d4 0 0 0)
     ((-1.05d0 0 0 14.2d0 3.05d0 3.05d0)) (((6.35d0 0 0) (1 0 0))) 8.5d0)
    (:highport 300000 (1.21d8 1.60d7 1.07d8 0 2.4d4 0)
     ((0.02d0 0 -4.84d0 6.5d0 60.0d0 6.5d0) (0.02d0 0 13.41d0 6.0d0 6.0d0 30.0d0)
      (0.02d0 50.0d0 -4.84d0 0.5d0 40.0d0 20.0d0) (0.02d0 -50.0d0 -4.84d0 0.5d0 40.0d0 20.0d0)
      (0.02d0 0 -20.84d0 10.0d0 44.0d0 19.5d0))
     (((-3.28d0 0 3.41d0) (-1 0 0)) ((0.02d0 20.0d0 -11.39d0) (0 0 -1))
      ((0.02d0 -20.0d0 -11.39d0) (0 0 -1)))
     72.6d0)
    (:skua-yard-1 20000 (1.43d4 1.43d4 1.43d4 0 0 0) ((0 0 0 1.34d0)) (((1.64d0 0 0) (1 0 0))) 2.44d0)
    (:skua-yard-2 35000 (3.63d4 3.63d4 3.63d4 0 0 0) ((0 0 0 1.61d0)) (((1.91d0 0 0) (1 0 0))) 2.71d0)
    (:skua-yard-3 50000 (6.58d4 6.58d4 6.58d4 0 0 0) ((0 0 0 1.81d0)) (((2.11d0 0 0) (1 0 0))) 2.91d0)
    (:skua-yard-4 80000 (1.44d5 1.44d5 1.44d5 0 0 0) ((0 0 0 2.12d0)) (((2.42d0 0 0) (1 0 0))) 3.22d0)
    (:skua-yard-5 100000 (2.09d5 2.09d5 2.09d5 0 0 0) ((0 0 0 2.29d0)) (((2.59d0 0 0) (1 0 0))) 3.39d0)
    (:skua-yard-6 150000 (4.11d5 4.11d5 4.11d5 0 0 0) ((0 0 0 2.62d0)) (((2.92d0 0 0) (1 0 0))) 3.72d0)
    (:pod-14 12000 (1.25d4 2.22d4 2.22d4 0 0 0) ((0 0 0 4.0d0 2.5d0 2.5d0)) (((2.30d0 0 0) (1 0 0))) 3.1d0)
    (:rock-mover 1087000 (1.07d7 2.18d7 2.19d7 0 -7.9d4 0)
     ((-16.61d0 0 0.01d0 23.8d0 7.2d0 3.6d0) (-8.56d0 0 -3.49d0 4.5d0 3.0d0 3.0d0) (0.59d0 0 0.01d0 5.0d0))
     () 29.2d0)))

(defun rounds-to (value printed)
  "VALUE shows as PRINTED to PRINTED's three significant figures, or its two
decimals below 100."
  (let ((step (if (< (abs printed) 100)
                  0.01d0
                  (expt 10d0 (- (floor (log (abs printed) 10)) 2)))))
    (<= (abs (- value printed)) (* 0.5000001d0 step))))

(define-test places-match-the-design
  (is equal (mapcar #'first *design-table*) (mapcar #'lis::object-def-key lis::*objects*))
  (loop for (key mass tensor shapes fixtures bound) in *design-table*
        for def = (object-def* key)
        for m = (lis::object-def-tensor def)
        do (is = mass (lis::object-def-mass def))
           (loop for printed in tensor
                 for (i j) in '((0 0) (1 1) (2 2) (0 1) (0 2) (1 2))
                 do (is = (aref m (+ (* 3 i) j)) (aref m (+ (* 3 j) i)))
                    (true (if (zerop printed)
                              (zerop (aref m (+ (* 3 i) j)))
                              (rounds-to (aref m (+ (* 3 i) j)) printed))
                          "~A I~D~D ~F" key i j (aref m (+ (* 3 i) j))))
           (is = (length shapes) (length (lis::object-def-shapes def)))
           (loop for printed in shapes
                 for shape in (lis::object-def-shapes def)
                 for c = (lis::shape-centre shape)
                 do (true (every #'rounds-to (coerce c 'list) (subseq printed 0 3)) "~A centre" key)
                    (if (= (length printed) 4)
                        (true (rounds-to (lis::shape-radius shape) (fourth printed)) "~A r" key)
                        (true (every #'rounds-to (coerce (lis::shape-size shape) 'list)
                                     (subseq printed 3))
                              "~A size" key)))
           (is = (length fixtures) (length (lis::object-def-fixtures def)))
           (loop for (pin axis) in fixtures
                 for fixture in (lis::object-def-fixtures def)
                 do (true (every #'rounds-to (coerce (lis::fixture-pin fixture) 'list) pin) "~A pin" key)
                    (is equalp (apply #'lis::vec3 axis) (lis::fixture-axis fixture)))
           (true (<= (abs (- (lis::object-def-bound def) bound)) 0.1d0) "~A bound" key))
  ;; Kinds and fixture flags.
  (is equal '(:berth :berth :towable :berth :towable :towable :towable :towable :towable
              :towable :towable :fixed)
      (mapcar #'lis::object-def-kind lis::*objects*))
  (let ((plant (first (lis::object-def-fixtures (object-def* :highport)))))
    (true (lis::fixture-receiving plant))
    (true (lis::fixture-free plant)))
  (is equal '(t nil) (mapcar #'lis::fixture-free (lis::object-def-fixtures (object-def* :fulmar-depot))))
  (is equal '(t nil nil) (mapcar #'lis::fixture-free (lis::object-def-fixtures (object-def* :highport)))))

(define-test places-formulas
  ;; Rocks: (2/5) m r², r (3m / 4πρ)^(1/3); the pod a box.
  (dolist (key '(:skua-yard-1 :skua-yard-2 :skua-yard-3 :skua-yard-4 :skua-yard-5 :skua-yard-6))
    (let* ((def (object-def* key))
           (m (lis::object-def-mass def))
           (r (lis::shape-radius (first (lis::object-def-shapes def))))
           (tensor (lis::object-def-tensor def)))
      (true (< (abs (- r (expt (/ (* 3 m) (* 4 pi 2000)) 1/3))) 0.005d0) "~A r ~F" key r)
      (dotimes (i 9)
        (let ((expected (if (member i '(0 4 8)) (* 2/5 m r r) 0d0)))
          (true (<= (abs (- (aref tensor i) expected)) (* 1d-9 (abs expected))) "~A I ~D" key i)))))
  (let ((tensor (lis::object-def-tensor (object-def* :pod-14)))
        (box (lis::shape-tensor :box 12000d0 '(4.0d0 2.5d0 2.5d0))))
    (dotimes (i 9)
      (true (<= (abs (- (aref tensor i) (aref box i))) (* 1d-9 (abs (aref box i)))))))
  ;; Tern Station from its parts (reference: the middle module's centre).
  (let ((second (lis::make-mat3)) (mass 0d0) (first (lis::make-vec3)))
    (loop for (m centre shape size)
            in '((20000 (-8 0 0) :cylinder (8 4.2d0 4.2d0)) (20000 (0 0 0) :cylinder (8 4.2d0 4.2d0))
                 (20000 (8 0 0) :cylinder (8 4.2d0 4.2d0)) (6000 (14 0 0) :cylinder (4 4.2d0 4.2d0))
                 (9000 (18.25d0 0 0) :cylinder (4.5d0 3.9d0 3.9d0))
                 (4000 (0 0 0) :box (1.5d0 28 1.5d0))
                 (1000 (0 8 0) :box (4 10 0)) (1000 (0 -8 0) :box (4 10 0))
                 (2000 (0 26.5d0 0) :box (8 25 0)) (2000 (0 -26.5d0 0) :box (8 25 0)))
          for c = (apply #'lis::vec3 centre)
          do (incf mass m)
             (lis::vec3-add-scaled! first first c (float m 1d0))
             (lis::add-mass! second (float m 1d0) c (lis::shape-tensor shape (float m 1d0) size)))
    (let ((com (lis::vec3-scale first (/ mass))))
      (is = 85000d0 mass)
      (true (< (abs (- (aref com 0) 2.92d0)) 0.005d0))
      (lis::add-mass! second (- mass) com (lis::make-mat3))
      (let ((tensor (lis::object-def-tensor (object-def* :tern-station))))
        (dolist (i '(0 4 8))
          (true (< (relative-error (aref tensor i) (aref second i)) 0.02d0)
                "I~D ~,3E from the parts ~,3E" i (aref tensor i) (aref second i)))))))

(defun box-extent (shape axis)
  "The farthest point of SHAPE along the unit AXIS."
  (let ((c (lis::shape-centre shape)))
    (+ (lis::vec3-dot c axis)
       (if (eq (lis::shape-kind shape) :sphere)
           (lis::shape-radius shape)
           (loop for i below 3 sum (* 0.5d0 (abs (* (aref axis i) (aref (lis::shape-size shape) i)))))))))

(defun on-line-p (shape pin axis)
  "Whether the line through PIN along AXIS meets SHAPE."
  (let ((d (lis::vec3-sub pin (lis::shape-centre shape))))
    (if (eq (lis::shape-kind shape) :sphere)
        (< (lis::vec3-norm (lis::vec3-cross d axis)) (lis::shape-radius shape))
        (loop for i below 3
              always (or (/= 0 (aref axis i))
                         (<= (abs (aref d i)) (* 0.5d0 (aref (lis::shape-size shape) i))))))))

(define-test places-bounds-and-plates
  (dolist (def lis::*objects*)
    (let ((needed 0d0))
      (dolist (shape (lis::object-def-shapes def))
        (let ((c (lis::shape-centre shape)))
          (setf needed
                (max needed
                     (if (eq (lis::shape-kind shape) :sphere)
                         (+ (lis::vec3-norm c) (lis::shape-radius shape))
                         (sqrt (loop for i below 3
                                     sum (expt (+ (abs (aref c i))
                                                  (* 0.5d0 (aref (lis::shape-size shape) i)))
                                               2))))))))
      (dolist (fixture (lis::object-def-fixtures def))
        (setf needed (max needed (+ (lis::vec3-norm (lis::fixture-pin fixture)) 0.8d0))))
      (true (>= (lis::object-def-bound def) (- needed 1d-9))
            "~A bound ~F < ~F" (lis::object-def-key def) (lis::object-def-bound def) needed))
    ;; A free fixture's plate stands 0.3 m proud of the shapes on its axis;
    ;; an occupied one is inside its scenery ship's shapes.
    (dolist (fixture (lis::object-def-fixtures def))
      (when (lis::fixture-free fixture)
        (let* ((pin (lis::fixture-pin fixture))
               (axis (lis::fixture-axis fixture))
               (face (loop for shape in (lis::object-def-shapes def)
                           when (on-line-p shape pin axis)
                             maximize (box-extent shape axis)))
               (proud (- (lis::vec3-dot pin axis) face)))
          (true (< (abs (- proud 0.3d0)) 0.005d0)
                "~A ~A ~,3F m proud" (lis::object-def-key def) (lis::fixture-name fixture) proud))))))

(define-test places-orbits-and-attitudes
  (dolist (def lis::*objects*)
    (let ((orbit (object-orbit def))
          (attitude (lis::object-def-attitude def)))
      (is = (lis::deg->rad 90d0) (lis::orbit-i orbit))
      (is = (lis::deg->rad 270d0) (lis::orbit-raan orbit))
      (is = 0d0 (lis::rails-segment-raan-rate (first (lis::object-def-rails def))))
      (is eq :horizon (lis::rails-segment-end-kind (first (lis::object-def-rails def))))
      (when (eq (lis::object-def-kind def) :towable)
        (if (eq (lis::attitude-kind attitude) :spin)
            (progn (is equalp (lis::vec3 1 0 0) (lis::attitude-axis attitude))
                   (true (< 0 (lis::attitude-rate attitude) (lis::deg->rad 0.4000001d0))))
            (is eq :inertial (lis::attitude-kind attitude))))))
  (is equal '(:lvlh :sun-pointing :spin :sun-pointing :inertial :inertial)
      (mapcar (lambda (key) (lis::attitude-kind (lis::object-def-attitude (object-def* key))))
              '(:tern-station :fulmar-depot :k-17 :highport :pod-14 :rock-mover)))
  ;; K-17's +x along its orbit's anti-normal, at the start and a day later.
  (let ((def (object-def* :k-17)))
    (dolist (time (list (lis::start-t) (+ (lis::start-t) lis::+day+ 0.5d0)))
      (multiple-value-bind (r v q) (def-state def time)
        (let ((miss (angle-between (lis::quat-rotate q (lis::vec3 1 0 0))
                                   (lis::vec3-cross v r))))
          (true (< miss 1d-9) "K-17 ~,2E rad" miss)))))
  ;; The yard's fixtures face back along the orbit at apogee (POD 14 and
  ;; every rock alike); the rock mover holds its LVLH attitude of the epoch.
  (dolist (key '(:pod-14 :skua-yard-1 :skua-yard-3 :skua-yard-6))
    (let* ((def (object-def* key))
           (apogee (lis::orbit-next-apsis (object-orbit def) (lis::start-t) :apoapsis)))
      (multiple-value-bind (r v q) (def-state def apogee)
        (declare (ignore r))
        (let ((miss (angle-between (lis::quat-rotate q (lis::vec3 1 0 0)) (lis::vec3-scale v -1d0))))
          (true (< miss (lis::deg->rad 0.01d0)) "~A ~,4F°" key (lis::rad->deg miss))))))
  (multiple-value-bind (r v q) (def-state (object-def* :rock-mover) (lis::start-t))
    (true (< (angle-between (lis::quat-rotate q (lis::vec3 1 0 0)) v) (lis::deg->rad 0.5d0)))
    (true (< (angle-between (lis::quat-rotate q (lis::vec3 0 0 1)) (lis::vec3-scale r -1d0)) 1d-12))))

(define-test places-yard-layout
  ;; At Skua Yard 1's first apogee: the string 666.7 m apart, the rock mover
  ;; 2 km behind Skua Yard 6, Skua Yard 1 29.5 km behind Highport. The
  ;; design's M₀ to 1e-5° is 2.3 m at apogee: the gaps are 665.6 and 667.9 m,
  ;; so 1.5 m, not the plan's 1 m.
  (let* ((apogee (lis::orbit-next-apsis (object-orbit (object-def* :skua-yard-1)) (lis::start-t)
                                        :apoapsis))
         (string '(:skua-yard-3 :pod-14 :skua-yard-5 :skua-yard-1 :skua-yard-2 :skua-yard-4
                   :skua-yard-6))
         (r (mapcar (lambda (key) (def-state (object-def* key) apogee)) string)))
    (loop for (a b) on r
          for key in string
          while b
          do (let ((d (lis::vec3-norm (lis::vec3-sub a b))))
               (true (< (abs (- d 666.7d0)) 1.5d0) "after ~A: ~,1F m" key d)))
    (let ((d (lis::vec3-norm (lis::vec3-sub (car (last r))
                                            (def-state (object-def* :rock-mover) apogee)))))
      (true (< (abs (- d 2000d0)) 3d0) "rock mover ~,1F m" d))
    (let ((d (lis::vec3-norm (lis::vec3-sub (def-state (object-def* :highport) apogee)
                                            (fourth r)))))
      (true (< (abs (- d 29.5d3)) 500d0) "Highport ~,1F km" (/ d 1000)))))

(define-test places-presets
  (is equal '(:tern-station :fulmar-depot :k-17 :highport :skua-yard)
      (mapcar #'lis::place-key lis::*places*))
  (dolist (key '(:tern-station :fulmar-depot :k-17 :highport))
    (is eq key (lis::place-object (lis::place key)))
    (true (lis::object-definition key)))
  (fail (lis::place :peary-depot))
  ;; The drop point: Skua Yard 3's slot.
  (let ((drop (lis::place :skua-yard))
        (time (+ (lis::start-t) 1000d0)))
    (false (lis::place-object drop))
    (is equalp (def-state (object-def* :skua-yard-3) time)
        (rails-point (lis::place-rails drop) time))))
