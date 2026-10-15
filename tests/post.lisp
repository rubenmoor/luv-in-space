(in-package #:luv-in-space.tests)

;;; post.lisp: the exposure rule and its easing (decision 76: Frame passes,
;;; radiance scale and exposure). The GPU side is in tests/render.lisp.

(defun sim-render-state (sim)
  "SIM's state as a frame draws it."
  (lis::render-state-update! (lis::make-render-state) (lis::new-clock sim) sim))

(defun start-sim-at (u)
  "The start orbit with the ship at argument of latitude U degrees."
  (let ((lis::*start-orbit* (list* :u (float u 1d0) lis::*start-orbit*)))
    (lis::new-game-sim)))

(defun look-along! (camera direction)
  "CAMERA's view axis along DIRECTION from the ship, any roll."
  (let* ((x (lis::vec3-normalize direction))
         (helper (if (< (abs (aref x 2)) 0.9d0) (lis::vec3 0 0 1) (lis::vec3 1 0 0)))
         (y (lis::vec3-normalize (lis::vec3-cross helper x)))
         (z (lis::vec3-cross x y)))
    (lis::quat-from-basis! (lis::camera-orientation camera) x y z)
    (fill (lis::camera-offset camera) 0d0)
    camera))

(defun default-camera (state)
  (lis::outside-camera-default! (lis::make-camera) (lis::render-state-r state)
                                (lis::render-state-v state)))

(defun exposure (camera state &optional (view :outside))
  (lis::exposure-target camera state view))

(define-test exposure-start-sunlit
  (let ((state (sim-render-state (lis::new-game-sim))))
    (is = lis::*exposure-sunlit* (exposure (default-camera state) state))))

(define-test exposure-shadow-deep
  ;; u = 180 deg of the start orbit is in Earth's shadow.
  (let* ((state (sim-render-state (start-sim-at 180)))
         (r (lis::render-state-r state))
         (sun (lis::vec3-normalize (lis::render-state-sun state))))
    (true (minusp (lis::vec3-dot r sun)))
    (true (< (lis::vec3-norm (lis::vec3-cross r sun)) lis::+radius-earth+))
    (is = lis::*exposure-deep* (exposure (default-camera state) state))
    (is = lis::*exposure-deep* (exposure (look-along! (lis::make-camera) r) state))))

(define-test exposure-sun-in-view
  (let* ((state (sim-render-state (lis::new-game-sim)))
         (to-sun (lis::vec3-sub (lis::render-state-sun state) (lis::render-state-r state)))
         (camera (look-along! (lis::make-camera) to-sun)))
    (is = lis::*exposure-sunlit* (exposure camera state))
    ;; Away from the Sun and from Earth: deep.
    (look-along! camera (lis::vec3-add (lis::vec3-scale (lis::vec3-normalize
                                                         (lis::vec3-cross (lis::render-state-r state)
                                                                          to-sun))
                                                        2d0)
                                       (lis::vec3-normalize (lis::render-state-r state))))
    (is = lis::*exposure-deep* (exposure camera state))))

(define-test exposure-crescent-deep
  ;; Earth at phase 170 deg fills the view; the Sun behind it is hidden.
  (let* ((state (lis::make-render-state))
         (phase (deg 170))
         (camera (lis::make-camera)))
    (lis::vec3-set! (lis::render-state-r state) 7000d3 0d0 0d0)
    (lis::vec3-set! (lis::render-state-sun state)
                    (* lis::+au+ (cos phase)) (* lis::+au+ (sin phase)) 0d0)
    (lis::vec3-set! (lis::render-state-moon state) 0d0 0d0 3.844d8)
    (look-along! camera (lis::vec3 -1 0 0))
    (is = lis::*exposure-deep* (exposure camera state))
    ;; At phase 90 deg the same view is sunlit.
    (lis::vec3-set! (lis::render-state-sun state) 0d0 lis::+au+ 0d0)
    (is = lis::*exposure-sunlit* (exposure camera state))))

(define-test exposure-map
  (let ((state (sim-render-state (lis::new-game-sim))))
    (is = lis::*exposure-map* (exposure (default-camera state) state :map))))

(define-test exposure-easing
  ;; After one time constant, 1 - 1/e of the way in log2.
  (let* ((tau lis::*exposure-time-constant*)
         (eased (lis::exposure-ease 1d0 16d0 tau))
         (fraction (/ (log eased 2d0) 4d0)))
    (true (< (abs (- fraction (- 1 (exp -1d0)))) 1d-12) "fraction ~A" fraction)
    (is = 2d0 (lis::exposure-ease 2d0 16d0 0d0))
    (true (< (abs (- (lis::exposure-ease 16d0 1d0 (* 50 tau)) 1d0)) 1d-12))))

(define-test exposure-snaps
  ;; The first frame and captures (elapsed nil) take the target; later
  ;; frames ease.
  (let* ((game (make-instance 'lis::game))
         (state (lis::game-render-state game))
         (extent '(1280 800)))
    (lis::render-state-update! state (lis::new-clock (start-sim-at 180)) (start-sim-at 180))
    (lis::outside-camera-default! (lis::game-camera game) (lis::render-state-r state)
                                  (lis::render-state-v state))
    (is = lis::*exposure-deep* (lis::update-exposure! game extent 0.016d0))
    (setf (lis::game-exposure game) lis::*exposure-sunlit*)
    (let ((eased (lis::update-exposure! game extent 0.5d0)))
      (true (< lis::*exposure-sunlit* eased lis::*exposure-deep*)))
    (is = lis::*exposure-deep* (lis::update-exposure! game extent nil))))

(defun ship-camera (state distance)
  "A camera DISTANCE m from the ship, looking at it along a direction away
from Earth and the Sun."
  (let* ((r (lis::render-state-r state))
         (away (lis::vec3-add (lis::vec3-scale (lis::vec3-normalize
                                                (lis::vec3-cross r (lis::render-state-sun state)))
                                               2d0)
                              (lis::vec3-normalize r)))
         (camera (look-along! (lis::make-camera) away)))
    (lis::vec3-scale! (lis::camera-offset camera) (lis::vec3-normalize away) (- distance))
    camera))

(define-test exposure-sunlit-ship
  ;; decision 79: Sunlight at a point and the sunlit ship in the exposure
  ;; rule: the lit ship near the camera decides the exposure, not the black
  ;; behind it; far away or in Earth's shadow it does not.
  (let ((state (sim-render-state (lis::new-game-sim))))
    (true (< (abs (- 1 (lis::render-state-sun-fraction state))) 1d-12))
    (is = lis::*exposure-sunlit* (exposure (ship-camera state 40d0) state))
    (is = lis::*exposure-deep* (exposure (ship-camera state 1000d0) state))
    (is = lis::*exposure-map* (exposure (ship-camera state 40d0) state :map)))
  (let ((state (sim-render-state (start-sim-at 180))))
    (is = 0d0 (lis::render-state-sun-fraction state))
    (is = lis::*exposure-deep* (exposure (ship-camera state 40d0) state))))
