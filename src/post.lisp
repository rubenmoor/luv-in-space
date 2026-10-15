(in-package #:luv-in-space)

;;; HDR, bloom, tone map and exposure (decision 76: Frame passes, radiance
;;; scale and exposure; docs/architecture.md "Rendering precision"). The
;;; scene pass draws into the :hdr target; the bloom chain runs down to 1/16
;;; and back up; the tone map mixes it into the scene and writes the canvas
;;; or capture target. Exposure is applied only there.

(defparameter *bloom-strength* 0.04 "The bloom chain's share of the tone map's input.")
(defparameter *exposure-sunlit* 1.0d0 "The Sun or a lit body in view.")
(defparameter *exposure-deep* 16.0d0 "Deep space: stars.")
(defparameter *exposure-map* 1.0d0 "The map's fixed exposure; part 8 sets it.")
(defparameter *exposure-time-constant* 1.0d0 "Real seconds; log2 easing.")
(defparameter *exposure-sun-margin* (* 5 (/ pi 180)) "Beyond the half-diagonal, radians.")
(defparameter *exposure-coverage* 0.05d0 "Least coverage, and coverage x lit fraction.")
(defparameter *ship-bounding-radius* 10.0d0
  "m; decision 79's ship term until the meshes part computes the stack's.")
(defparameter *exposure-ship-coverage* 0.005d0
  "Least coverage x sun-fraction of the ship in the outside view (decision 79).")

(defconstant +hdr-format+ :rgba16-float)
(defconstant +bloom-levels+ 4 "1/2 to 1/16.")

;;; Exposure: pure, no GPU.

(defun vec3-angle (a b)
  (atan (vec3-norm (vec3-cross a b)) (vec3-dot a b)))

(defun view-cone (camera aspect)
  "The half-diagonal and the half-angle of the cone with the frame's solid angle."
  (let* ((a (* 0.5d0 (camera-field-of-view camera)))
         (b (atan (* (tan a) aspect)))
         (solid (* 4 (asin (* (sin a) (sin b))))))
    (values (atan (* (tan a) (sqrt (+ 1 (* aspect aspect)))))
            (acos (- 1 (/ solid (* 2 pi)))))))

(defun exposure-target (camera state view &key (aspect 1.6d0))
  "The exposure the view calls for (decision 76; decision 93: Exposure rule
as built: hidden Sun, coverage within the view): *exposure-map* for :map;
*exposure-sunlit* when the Sun's centre, not hidden by a body, is within the
half-diagonal plus *exposure-sun-margin* of the view axis, or when Earth or
the Moon covers at least *exposure-coverage* of the frame (its disc's
overlap with the frame's equivalent cone, flat) and coverage x lit fraction
(1 + cos phase) / 2 is at least as much, or, in the :outside view, when the
ship (a sphere of *ship-bounding-radius* at the camera's anchor) covers the
frame so that coverage x its sun-fraction is at least
*exposure-ship-coverage* (decision 79: Sunlight at a point and the sunlit
ship in the exposure rule; no ship term with the eye inside that sphere);
else *exposure-deep*."
  (when (eq view :map)
    (return-from exposure-target *exposure-map*))
  (let* ((aspect (float aspect 1d0))
         (eye (camera-eye camera (render-state-r state)))
         (forward (quat-rotate (camera-orientation camera) (vec3 1 0 0)))
         (sun (vec3-sub (render-state-sun state) eye))
         (bodies (list (list (vec3 0 0 0) +radius-earth+)
                       (list (render-state-moon state) +radius-moon+))))
    (multiple-value-bind (half-diagonal cone) (view-cone camera aspect)
      (flet ((disc (body)
               ;; The body relative to the eye and its angular radius.
               (destructuring-bind (centre radius) body
                 (let* ((c (vec3-sub centre eye)) (d (vec3-norm c)))
                   (values c (if (<= d radius) pi (asin (/ radius d))))))))
        (if (or (and (<= (vec3-angle forward sun) (+ half-diagonal *exposure-sun-margin*))
                     (notany (lambda (body)
                               (multiple-value-bind (c rho) (disc body)
                                 (and (< (vec3-norm c) (vec3-norm sun))
                                      (< (vec3-angle c sun) rho))))
                             bodies))
                (some (lambda (body)
                        (multiple-value-bind (c rho) (disc body)
                          (let* ((coverage (min 1d0 (/ (lens-area rho cone (vec3-angle forward c))
                                                       (* pi cone cone))))
                                 (phase (vec3-angle (vec3-sub sun c) (vec3-scale c -1d0)))
                                 (lit (* 0.5d0 (+ 1 (cos phase)))))
                            (and (>= coverage *exposure-coverage*)
                                 (>= (* coverage lit) *exposure-coverage*)))))
                      bodies)
                (and (eq view :outside)
                     (let ((d (vec3-norm (camera-offset camera))))
                       (and (> d *ship-bounding-radius*)
                            (let* ((rho (asin (/ *ship-bounding-radius* d)))
                                   (ship (vec3-scale (camera-offset camera) -1d0))
                                   (coverage (min 1d0 (/ (lens-area rho cone (vec3-angle forward ship))
                                                         (* pi cone cone)))))
                              (>= (* coverage (render-state-sun-fraction state))
                                  *exposure-ship-coverage*))))))
            *exposure-sunlit*
            *exposure-deep*)))))

(defun exposure-ease (current target elapsed)
  "CURRENT eased toward TARGET over ELAPSED real seconds, exponentially in
log2 with *exposure-time-constant*."
  (let ((k (exp (- (/ elapsed *exposure-time-constant*)))))
    (expt 2d0 (+ (log target 2d0) (* k (- (log current 2d0) (log target 2d0)))))))

(defun tone-map-srgb (x exposure)
  "The tone-map pass's byte for radiance X at EXPOSURE, bloom aside: ACES
(Narkowicz) clamped to [0, 1], sRGB-encoded; shaders.lisp's tone-map-fragment."
  (let* ((x (* (max 0d0 (float x 1d0)) exposure))
         (c (max 0d0 (min 1d0 (/ (* x (+ (* x 2.51d0) 0.03d0))
                                 (+ (* x (+ (* x 2.43d0) 0.59d0)) 0.14d0))))))
    (round (* 255 (if (<= c 0.0031308d0)
                      (* c 12.92d0)
                      (- (* 1.055d0 (expt c (/ 2.4d0))) 0.055d0))))))

;;; The GPU side

(defun post-vertices ()
  "One fullscreen triangle per source level k = 0 .. 4: x, y and 2^k."
  (coerce (loop for k to +bloom-levels+
                append (loop for (x y) in '((-1 -1) (3 -1) (-1 3))
                             append (list (float x) (float y) (float (expt 2 k)))))
          '(simple-array single-float (*))))

(defun post-pipelines (renderer layout)
  "The bloom roles in the HDR format and the tone map in the target's."
  (let ((buffers '((:array-stride 12
                    :attributes ((:shader-location 0 :offset 0 :format :float32x3))))))
    (live-pipeline renderer :bloom-down layout buffers :format +hdr-format+)
    (live-pipeline renderer :bloom-up layout buffers :format +hdr-format+)
    (live-pipeline renderer :tone-map layout buffers)))

(defun level-extent (extent k)
  (mapcar (lambda (n) (max 1 (floor n (expt 2 k)))) extent))

;; luv's Vulkan passes only clear (docs/luv-capabilities.md "Rendering"), so
;; the way up writes its own levels U3 to U1 instead of adding into D3 to D1.
(defun post-views (renderer extent)
  "Views of D0 (:hdr) to D4 and U1 to U3, made or remade for EXTENT."
  (loop for name in '(:hdr :bloom-1 :bloom-2 :bloom-3 :bloom-4 :bloom-up-1 :bloom-up-2 :bloom-up-3)
        for k in '(0 1 2 3 4 1 2 3)
        collect (values (render-target renderer name (level-extent extent k) +hdr-format+
                                       :usage '(:render-attachment :texture-binding)))))

(defun post-sources (views)
  "Per pass, in order, the views it samples: (source base)."
  (destructuring-bind (d0 d1 d2 d3 d4 u1 u2 u3) views
    (list (list d0 d0) (list d1 d1) (list d2 d2) (list d3 d3)
          (list d4 d3) (list u3 d2) (list u2 d1)
          (list u1 d0))))

(defun encode-bloom (renderer encoder views bind-groups vertices)
  "The chain: D0 down to D4; U3 = D3 + tent(D4), U2 = D2 + tent(U3),
U1 = D1 + tent(U2). BIND-GROUPS as post-sources; VERTICES' triangle k
carries level k's scale."
  (destructuring-bind (d0 d1 d2 d3 d4 u1 u2 u3) views
    (declare (ignore d0))
    (loop for (role destination source-level) in `((:bloom-down ,d1 0) (:bloom-down ,d2 1)
                                                   (:bloom-down ,d3 2) (:bloom-down ,d4 3)
                                                   (:bloom-up ,u3 4) (:bloom-up ,u2 3)
                                                   (:bloom-up ,u1 2))
          for bind-group in bind-groups
          do (let ((pass (luv:begin-render-pass
                          encoder
                          (luv:make-render-pass-descriptor
                           :color-attachments
                           `((:view ,destination :load-op :clear :store-op :store
                              :clear-value #(0.0 0.0 0.0 1.0)))))))
               (luv:set-pipeline pass (renderer-pipeline renderer role))
               (luv:set-bind-group pass 0 bind-group)
               (luv:set-vertex-buffer pass 0 vertices)
               (luv:draw pass 3 1 (* 3 source-level))
               (luv:end-pass pass)))))

(defun draw-tone-map (renderer pass bind-group vertices)
  "Into the open PASS on the target: D0 mixed with U1, exposed, tone-mapped."
  (luv:set-pipeline pass (renderer-pipeline renderer :tone-map))
  (luv:set-bind-group pass 0 bind-group)
  (luv:set-vertex-buffer pass 0 vertices)
  (luv:draw pass 3 1 3))
