(in-package #:luv-in-space.tests)

;;; input.lisp: the held-key map, the event path, taps, focus and quit
;;; (decision 81: Input in milestone 2: held keys per frame, taps, gating
;;; and the shared key map). A game without a canvas or renderer.

(defun input-game ()
  (let ((sim (lis::new-game-sim)))
    (make-instance 'lis::game :sim sim :clock (lis::new-clock sim))))

(defun key-event (game kind key &key modifiers repeat)
  (lis::handle-event game (make-instance (ecase kind
                                           (:press 'luv:canvas-key-press-event)
                                           (:release 'luv:canvas-key-release-event))
                                         :timestamp 0 :key-name key
                                         :modifiers modifiers :repeat-p repeat)))

(defun window-event (game class)
  (lis::handle-event game (make-instance class :timestamp 0)))

(defun input-frame (game)
  (lis::input-frame! game 1/60))

(defun game-controls (game) (lis::sim-controls (lis::game-sim game)))

(defun rotation-keys (game) (lis::controls-rotation-keys (game-controls game)))
(defun translation-keys (game) (lis::controls-translation-keys (game-controls game)))

(define-test input-held-keys
  (let ((game (input-game)))
    (is eq :hold (lis::controls-attitude-mode (game-controls game)))
    (key-event game :press :w)
    (input-frame game)
    (is equalp #(0 -1 0) (rotation-keys game))
    (is eq :rate-damping (lis::controls-attitude-mode (game-controls game)))
    (input-frame game)
    (is equalp #(0 -1 0) (rotation-keys game))
    (key-event game :release :w)
    (input-frame game)
    (is equalp #(0 0 0) (rotation-keys game))
    (key-event game :press :w)
    (key-event game :press :s)
    (input-frame game)
    (is equalp #(0 0 0) (rotation-keys game)))
  (loop for (key kind axis sign) in lis::*flight-keys*
        for index = (position axis (if (eq kind :rotation) '(:roll :pitch :yaw) '(:x :y :z)))
        do (let ((game (input-game))
                 (expected (vector 0 0 0)))
             (setf (aref expected index) sign)
             (key-event game :press key)
             (input-frame game)
             (is equalp expected (if (eq kind :rotation) (rotation-keys game) (translation-keys game))
                 "~A" key)
             (is equalp #(0 0 0) (if (eq kind :rotation) (translation-keys game) (rotation-keys game))
                 "~A" key))))

(define-test input-tap
  (let ((game (input-game)))
    (key-event game :press :w)
    (key-event game :release :w)
    (input-frame game)
    (is equalp #(0 -1 0) (rotation-keys game))
    (input-frame game)
    (is equalp #(0 0 0) (rotation-keys game))))

(define-test input-repeat-and-stray-release
  (let ((game (input-game)))
    (key-event game :press :w :repeat t)
    (key-event game :release :s)
    (input-frame game)
    (is equalp #(0 0 0) (rotation-keys game))
    (is eq :hold (lis::controls-attitude-mode (game-controls game)))
    (is eq nil (lis::input-held (lis::game-input game)))
    (is eq nil (lis::input-released (lis::game-input game)))))

(define-test input-focus-loss
  (let ((game (input-game)))
    (lis::controls-set-throttle (game-controls game) 0.5d0)
    (key-event game :press :w)
    (key-event game :press :h)
    (input-frame game)
    (is equalp #(0 -1 0) (rotation-keys game))
    (is equalp #(1 0 0) (translation-keys game))
    (window-event game 'luv:canvas-window-focus-lost-event)
    (input-frame game)
    (is equalp #(0 0 0) (rotation-keys game))
    (is equalp #(0 0 0) (translation-keys game))
    (is = 0.5d0 (lis::controls-throttle (game-controls game)))))

(define-test input-quit
  (flet ((quits-p (thunk)
           (let ((game (input-game)))
             (funcall thunk game)
             (lis::game-quit-requested-p game))))
    (true (quits-p (lambda (g) (key-event g :press :escape))))
    (true (quits-p (lambda (g) (key-event g :press :q :modifiers '(:control)))))
    (true (quits-p (lambda (g) (window-event g 'luv:canvas-window-close-request-event))))
    (false (quits-p (lambda (g) (key-event g :press :q))))))

(define-test controls-apply-held-keys
  (let ((c (lis::new-controls)))
    (lis::controls-apply-held-keys! c '(:a :d :q))
    (is equalp #(-1 0 0) (lis::controls-rotation-keys c))
    (is eq :hold (lis::controls-attitude-mode c))
    (lis::controls-apply-held-keys! c '())
    (is equalp #(0 0 0) (lis::controls-rotation-keys c))))
