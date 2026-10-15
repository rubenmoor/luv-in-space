(in-package #:luv-in-space)

;;; The objects in Earth orbit and the places the autopilot's presets go to
;;; (docs/design.md "Earth-orbit points of interest", "Rigid bodies: attitude,
;;; mass, shapes and fixtures"; decision 47: Rigid bodies of the places,
;;; decision 110: Objects in the simulation: places.lisp's load position, the
;;; object struct and promotion checks). The tables' values are data, not
;;; recomputed from the parts.

;;; Elements (decision 38: Start orbit: polar): every place at i 90°, Ω 270°,
;;; epoch the start date.

(defparameter *highport-elements* '(:a 41878100d0 :e 0.82382d0 :argp 336d0)
  "m and degrees, docs/design.md \"Earth-orbit points of interest\".")

(defun place-orbit (mean-anomaly &key (a (+ +radius-earth+ 420d3)) (e 0d0) (argp 0d0))
  "A polar orbit, Ω 270°, at MEAN-ANOMALY (degrees; a circular orbit's u₀)
at (start-t)."
  (make-elliptic-orbit +mu-earth+ a e (deg->rad 90d0) (deg->rad 270d0) (deg->rad argp)
                       (deg->rad mean-anomaly) (start-t)))

(defun circular-orbit (altitude u0)
  (place-orbit u0 :a (+ +radius-earth+ altitude)))

(defun highport-orbit (mean-anomaly)
  (apply #'place-orbit mean-anomaly *highport-elements*))

(defun lvlh-quat (orbit time)
  "The LVLH attitude on ORBIT at TIME."
  (multiple-value-bind (r v) (orbit-state orbit time)
    (object-attitude-into! (make-quat) (make-vec3) (make-attitude :lvlh) r v time)))

(defun yard-quat ()
  "POD 14's and the rocks' attitude (decision 111: Fixture axes of Derelict
K-17 and Skua Yard's rocks; decision 133: The places' data: rock radii, the
yard's attitude, display names and the target state): the LVLH attitude at
the yard's first apogee turned 180° about z, so +x faces back along the
orbit at each apogee."
  (let ((orbit (highport-orbit 179.87d0)))
    (quat-mul (lvlh-quat orbit (orbit-next-apsis orbit (start-t) :apoapsis))
              (quat 0 0 0 1))))

(defun k-17-quat (orbit)
  "+x along ORBIT's anti-normal (the LVLH +y at the epoch), y nadir, z along
the motion (decision 111)."
  (quat-mul (lvlh-quat orbit (start-t)) (quat 0.5d0 0.5d0 0.5d0 0.5d0)))

;;; Definitions

(defun place-box (cx cy cz sx sy sz)
  (make-shape :box (vec3 cx cy cz) :size (vec3 sx sy sz)))

(defun place-sphere (cx cy cz radius)
  (make-shape :sphere (vec3 cx cy cz) :radius (float radius 1d0)))

(defun place-fixture (name pin axis kind &rest flags)
  (apply #'make-fixture name (apply #'vec3 pin) (apply #'vec3 axis) kind flags))

(defun place-object-def (key name kind mass tensor shapes fixtures attitude bound orbit
                         &key depot)
  (make-object-def key name kind (float mass 1d0) (apply #'mat3 tensor) shapes fixtures attitude
                   (float bound 1d0) (make-rails orbit (start-t)) :depot depot))

(defun yard-rock (key name mass radius tensor spin bound mean-anomaly)
  "A Skua Yard rock (docs/design.md \"Rocks\"): a sphere, its fixture 0.3d0 m
proud on +x, spinning about it at SPIN °/s. RADIUS (3m / 4πρ)^(1/3) to the
mm; TENSOR (2/5) m r² of it; BOUND the pin + 0.8d0 m."
  (place-object-def key name :towable mass (list tensor 0 0  0 tensor 0  0 0 tensor)
                (list (place-sphere 0 0 0 radius))
                (list (place-fixture "fixture" (list (+ radius 0.3d0) 0 0) '(1 0 0) :towable))
                (make-attitude :spin :q (yard-quat) :rate (deg->rad spin) :epoch (start-t))
                bound (highport-orbit mean-anomaly)))

(defun make-objects ()
  (let ((k-17 (circular-orbit 800d3 270d0))
        (mover (highport-orbit 179.85239d0)))
    (list
     (place-object-def :tern-station "TERN STATION" :berth 85000
                   '(3.59d6 0 0  0 6.46d6 0  0 0 9.88d6)
                   (list (place-box 1.33d0 0 0  32.5d0 4.2d0 4.2d0)
                         (place-box -2.92d0 0 0  4.0d0 28.0d0 1.5d0)
                         (place-box -2.92d0 26.5d0 0  8.0d0 25.0d0 0.5d0)
                         (place-box -2.92d0 -26.5d0 0  8.0d0 25.0d0 0.5d0))
                   (list (place-fixture "berth" '(-10.92d0 0 -2.40d0) '(0 0 -1) :berth))
                   (make-attitude :lvlh) 39.8d0 (circular-orbit 420d3 20d0))
     (place-object-def :fulmar-depot "FULMAR DEPOT" :berth 60000
                   '(0.793d6 0 0  0 4.65d6 0  0 0 5.12d6)
                   (list (place-box 14.22d0 0 0  0.3d0 16.0d0 16.0d0)
                         (place-box 2.77d0 0 0  21.5d0 13.8d0 6.5d0)
                         (place-box -11.98d0 0 0  8.0d0 1.5d0 8.0d0)
                         (place-box -13.98d0 0 -16.30d0  3.6d0 3.6d0 24.0d0))
                   (list (place-fixture "A" '(-13.98d0 0 4.30d0) '(0 0 1) :berth)
                         (place-fixture "B" '(-13.98d0 0 -4.30d0) '(0 0 -1) :berth :free nil))
                   (make-attitude :sun-pointing) 33.7d0 (circular-orbit 500d3 180d0)
                   :depot t)
     (place-object-def :k-17 "K-17" :towable 2500
                   '(4.89d3 0 0  0 5.65d4 0  0 0 5.65d4)
                   (list (place-box -1.05d0 0 0  14.2d0 3.05d0 3.05d0))
                   (list (place-fixture "fixture" '(6.35d0 0 0) '(1 0 0) :towable))
                   (make-attitude :spin :q (k-17-quat k-17) :rate (deg->rad 0.3d0)
                                        :epoch (start-t))
                   8.5d0 k-17)
     (place-object-def :highport "HIGHPORT" :berth 300000
                   '(1.21d8 0 2.4d4  0 1.60d7 0  2.4d4 0 1.07d8)
                   (list (place-box 0.02d0 0 -4.84d0  6.5d0 60.0d0 6.5d0)
                         (place-box 0.02d0 0 13.41d0  6.0d0 6.0d0 30.0d0)
                         (place-box 0.02d0 50.0d0 -4.84d0  0.5d0 40.0d0 20.0d0)
                         (place-box 0.02d0 -50.0d0 -4.84d0  0.5d0 40.0d0 20.0d0)
                         (place-box 0.02d0 0 -20.84d0  10.0d0 44.0d0 19.5d0))
                   (list (place-fixture "plant" '(-3.28d0 0 3.41d0) '(-1 0 0) :berth :receiving t)
                         (place-fixture "arm 1" '(0.02d0 20.0d0 -11.39d0) '(0 0 -1) :berth :free nil)
                         (place-fixture "arm 2" '(0.02d0 -20.0d0 -11.39d0) '(0 0 -1) :berth :free nil))
                   (make-attitude :sun-pointing) 72.6d0 (highport-orbit 180d0)
                   :depot t)
     ;; Skua Yard's rocks; their places in the string: docs/design.md "Layout".
     (yard-rock :skua-yard-1 "SKUA YARD 1" 20000 1.337d0 14300.552d0 0.10d0 2.437d0 179.87000d0)
     (yard-rock :skua-yard-2 "SKUA YARD 2" 35000 1.611d0 36334.494d0 0.20d0 2.711d0 179.86707d0)
     (yard-rock :skua-yard-3 "SKUA YARD 3" 50000 1.814d0 65811.92d0 0.15d0 2.914d0 179.87880d0)
     (yard-rock :skua-yard-4 "SKUA YARD 4" 80000 2.122d0 144092.288d0 0.25d0 3.222d0 179.86413d0)
     (yard-rock :skua-yard-5 "SKUA YARD 5" 100000 2.285d0 208849d0 0.05d0 3.385d0 179.87293d0)
     (yard-rock :skua-yard-6 "SKUA YARD 6" 150000 2.616d0 410607.36d0 0.30d0 3.716d0 179.86120d0)
     ;; A standard cargo pod (decision 63: POD 14 and Skua Yard's layout from
     ;; milestone 3).
     (place-object-def :pod-14 "POD 14" :towable 12000
                   '(12500d0 0 0  0 22250d0 0  0 0 22250d0)
                   (list (place-box 0 0 0  4.0d0 2.5d0 2.5d0))
                   (list (place-fixture "fixture" '(2.30d0 0 0) '(1 0 0) :towable))
                   (make-attitude :inertial :q (yard-quat))
                   3.1d0 (highport-orbit 179.87587d0))
     (place-object-def :rock-mover "GRINDSTONE" :fixed 1087000
                   '(1.07d7 0 -7.9d4  0 2.18d7 0  -7.9d4 0 2.19d7)
                   (list (place-box -16.61d0 0 0.01d0  23.8d0 7.2d0 3.6d0)
                         (place-box -8.56d0 0 -3.49d0  4.5d0 3.0d0 3.0d0)
                         (place-sphere 0.59d0 0 0.01d0 5.0d0))
                   '()
                   (make-attitude :inertial :q (lvlh-quat mover (start-t)))
                   29.2d0 mover))))

(defparameter *objects* (make-objects)
  "The object definitions in the sim's fixed order.")

(defun object-definition (key)
  (or (find key *objects* :key #'object-def-key)
      (error "No object ~S." key)))

;;; Places: the autopilot's presets, "Rendezvous: <place>"

(defstruct (place (:constructor make-place (key name &key object rails)))
  (key nil :type keyword :read-only t)
  (name "" :type string :read-only t)
  (object nil :type (or null keyword) :read-only t) ; the target object's key
  (rails nil :type list :read-only t))              ; or a point on rails

(defparameter *places*
  (list (make-place :tern-station "TERN STATION" :object :tern-station)
        (make-place :fulmar-depot "FULMAR DEPOT" :object :fulmar-depot)
        (make-place :k-17 "DERELICT K-17" :object :k-17)
        (make-place :highport "HIGHPORT" :object :highport)
        ;; The drop point, Skua Yard 3's slot (docs/design.md "The terminal
        ;; path and crowded targets").
        (make-place :skua-yard "SKUA YARD"
                    :rails (make-rails (highport-orbit 179.87880d0) (start-t)))))

(defun place (key)
  (or (find key *places* :key #'place-key)
      (error "No place ~S." key)))

(defun place-target-state-into! (r v place sim)
  "PLACE's state at SIM's t into R, V (m, m/s, Earth-centred): its object's
(object-state-into!; nil when removed) or the drop point's rails. True or
nil."
  (let ((time (sim-t sim)))
    (if (place-object place)
        (let ((q (make-quat)) (w (make-vec3)))
          (declare (dynamic-extent q w))
          (and (object-state-into! r v q w (sim-object sim (place-object place)) time) t))
        (progn (rails-state-into! r v (rails-segment-at (place-rails place) time) time)
               t))))
