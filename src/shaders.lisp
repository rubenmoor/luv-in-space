(in-package #:luv-in-space.shaders)

;;; PLACEHOLDER(76): the placeholder Earth sphere sampling the Blue Marble;
;;; the stars at infinity (src/stars.lisp); screen-space quads for the
;;; overlay's labels and the tests' quads; the post passes (decision 76: Frame passes, radiance scale and exposure).

;; One uniform block for every pipeline: clip rows for camera-relative
;; points, the body relative to the eye (w: radius), the sun direction,
;; screen (xy: clip units per pixel), the rows of Earth's body-fixed
;; matrix (ICRF -> body-fixed), post (exposure, bloom strength, sRGB
;; encoding 0 or 1) and stars (radiance limit, sigma, sigma per magnitude,
;; sigma max; src/stars.lisp). RESOURCES in OPTIONS come after the block.
(defmacro define-frame-shader (name (&rest options &key resources &allow-other-keys)
                               &body body)
  `(define-shader ,name
       (,@(loop for (key value) on options by #'cddr
                unless (eq key :resources) append (list key value))
        :resources ((frame :uniform-block :binding 0
                     :members ((clip-x :vec4) (clip-y :vec4)
                               (clip-z :vec4) (clip-w :vec4)
                               (body :vec4) (sun :vec4) (screen :vec4)
                               (fixed-x :vec4) (fixed-y :vec4) (fixed-z :vec4)
                               (post :vec4) (stars :vec4)))
                    ,@resources))
     ,@body))

;; The language has no atan or asin (docs/luv-capabilities.md "Other
;; findings"): Abramowitz-Stegun 4.4.49 on [0, 1], error below 1e-5 rad;
;; arcsin, as CL:ASIN is locked.
(define-shader-function atan-unit (x)
  (let* ((x2 (* x x)))
    (* x (+ 0.999866 (* x2 (+ -0.3302995 (* x2 (+ 0.180141
                                                   (* x2 (+ -0.085133 (* x2 0.0208351)))))))))))

(define-shader-function atan2 (y x)
  (let* ((ax (abs x)) (ay (abs y))
         (a (atan-unit (/ (min ax ay) (max (max ax ay) 1e-20))))
         (b (if (> ay ax) (- 1.5707964 a) a))
         (c (if (< x 0.0) (- 3.1415927 b) b)))
    (if (< y 0.0) (- 0.0 c) c)))

(define-shader-function arcsin (z)
  (atan2 z (sqrt (max (- 1.0 (* z z)) 0.0))))

(define-frame-shader earth-vertex
    (:stage :vertex
     :inputs ((unit :vec3 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)
               (normal :vec3 :location 0)))
  (let* ((point (vec4 (+ (swizzle body :xyz) (* unit (swizzle body :w))) 1.0)))
    (set-output clip-position
                (vec4 (dot clip-x point) (dot clip-y point)
                      (dot clip-z point) (dot clip-w point)))
    (set-output normal unit)))

;; The Blue Marble at the body-fixed normal (docs/architecture.md "Frames":
;; u = 0.5 + atan2(y, x)/2π, v = 0.5 − asin(z)/π), lit by the Sun in ICRF.
(define-frame-shader earth-fragment
    (:stage :fragment
     :inputs ((normal :vec3 :location 0))
     :outputs ((color :vec4 :location 0))
     :resources ((earth :texture-2d :binding 1)
                 (earth-sampler :sampler :binding 2)))
  (let* ((lit (normalize normal))
         (n (vec3 (dot (swizzle fixed-x :xyz) lit) (dot (swizzle fixed-y :xyz) lit)
                  (dot (swizzle fixed-z :xyz) lit)))
         (uv (vec2 (+ 0.5 (/ (atan2 (swizzle n :y) (swizzle n :x)) 6.2831855))
                   (- 0.5 (/ (arcsin (clamp (swizzle n :z) -1.0 1.0)) 3.1415927))))
         (albedo (swizzle (sample earth earth-sampler uv) :rgb))
         (diffuse (clamp (dot lit (swizzle sun :xyz)) 0.0 1.0)))
    (set-output color (vec4 (+ (* albedo diffuse) (* albedo 0.02)) 1.0))))

;; A star at infinity (decision 78: Sky in numbers): its direction through
;; the clip rows with w 0, so clip z is 0; a quad of 4 sigma pixels. STAR:
;; direction, magnitudes brighter than 3.0.
(define-frame-shader star-vertex
    (:stage :vertex
     :inputs ((corner :vec2 :location 0)
              (star :vec4 :location 1)
              (light :vec3 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (offset :vec2 :location 0)
               (shade :vec3 :location 1)
               (sigma :float :location 2)))
  (let* ((point (vec4 (swizzle star :xyz) 0.0))
         (w (dot clip-w point))
         (size (min (+ (swizzle stars :y) (* (swizzle stars :z) (swizzle star :w)))
                    (swizzle stars :w)))
         (pixels (* corner (* 4.0 size))))
    (set-output clip-position
                (vec4 (+ (dot clip-x point) (* (swizzle pixels :x) (swizzle screen :x) w))
                      (+ (dot clip-y point) (* (swizzle pixels :y) (swizzle screen :y) w))
                      (dot clip-z point) w))
    (set-output offset pixels)
    (set-output shade (* light (swizzle stars :x)))
    (set-output sigma size)))

;; Additive through premultiplied alpha: alpha 0.
(define-frame-shader star-fragment
    (:stage :fragment
     :inputs ((offset :vec2 :location 0)
              (shade :vec3 :location 1)
              (sigma :float :location 2))
     :outputs ((color :vec4 :location 0)))
  (set-output color (vec4 (* shade (exp (/ (* -0.5 (dot offset offset)) (* sigma sigma))))
                          0.0)))

;; ANCHOR is a point in clip space, OFFSET a displacement in pixels.
(define-frame-shader overlay-vertex
    (:stage :vertex
     :inputs ((anchor :vec2 :location 0)
              (offset :vec2 :location 1)
              (tint :vec3 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (shade :vec3 :location 0)))
  (let* ((point (+ anchor (* offset (swizzle screen :xy)))))
    (set-output clip-position (vec4 point 0.0 1.0))
    (set-output shade tint)))

(define-frame-shader overlay-fragment
    (:stage :fragment
     :inputs ((shade :vec3 :location 0))
     :outputs ((color :vec4 :location 0)))
  (set-output color (vec4 shade 1.0)))

;;; Post (src/post.lisp). A fullscreen triangle whose third coordinate is
;;; the source level's scale 2^k, so its texel is 1 / floor(size / 2^k).

(define-frame-shader post-vertex
    (:stage :vertex
     :inputs ((corner :vec3 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)
               (uv :vec2 :location 0)
               (texel :vec2 :location 1)))
  (let* ((scale (swizzle corner :z))
         (size (vec2 (/ 2.0 (swizzle screen :x)) (/ 2.0 (swizzle screen :y)))))
    (set-output clip-position (vec4 (swizzle corner :xy) 0.0 1.0))
    (set-output uv (* (+ (swizzle corner :xy) (vec2 1.0 1.0)) 0.5))
    (set-output texel (/ (vec2 1.0 1.0)
                         (max (floor (/ (+ size (vec2 0.5 0.5)) scale)) (vec2 1.0 1.0))))))

;; [0, 6e4]; NaN to 0, as every comparison with it is false.
(define-shader-function finite-radiance (x)
  (if (>= x 0.0) (if (<= x 60000.0) x 60000.0) 0.0))

(define-shader-function finite-radiance3 (c)
  (vec3 (finite-radiance (swizzle c :x)) (finite-radiance (swizzle c :y))
        (finite-radiance (swizzle c :z))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tap-form (dx dy)
    "The source at UV + (DX, DY) texels, made finite."
    `(finite-radiance3
      (swizzle (sample source source-sampler (+ uv (* texel (vec2 ,(float dx) ,(float dy))))) :rgb)))

  (defun taps-form (weight offsets)
    `(* (+ ,@(loop for (dx dy) in offsets collect (tap-form dx dy))) ,weight))

  ;; Jimenez's 13-tap downsample (Call of Duty, SIGGRAPH 2014).
  (defun down-form ()
    `(+ ,(taps-form 0.125 '((0 0)))
        ,(taps-form 0.03125 '((-2 -2) (2 -2) (-2 2) (2 2)))
        ,(taps-form 0.0625 '((0 -2) (0 2) (-2 0) (2 0)))
        ,(taps-form 0.125 '((-1 -1) (1 -1) (-1 1) (1 1)))))

  ;; The 3x3 tent, weights 1 2 1 / 2 4 2 / 1 2 1 over 16.
  (defun tent-form ()
    `(+ ,(taps-form 0.25 '((0 0)))
        ,(taps-form 0.125 '((0 -1) (0 1) (-1 0) (1 0)))
        ,(taps-form 0.0625 '((-1 -1) (1 -1) (-1 1) (1 1))))))

(defmacro define-post-fragment (name &body body)
  `(define-frame-shader ,name
       (:stage :fragment
        :inputs ((uv :vec2 :location 0) (texel :vec2 :location 1))
        :outputs ((color :vec4 :location 0))
        :resources ((source :texture-2d :binding 1)
                    (source-sampler :sampler :binding 2)
                    (base :texture-2d :binding 3)))
     ,@body))

;; Narkowicz's fit, as Luvcraft's aces-filmic.
(define-shader-function aces-filmic (x)
  (clamp (/ (* x (+ (* x 2.51) (vec3 0.03 0.03 0.03)))
            (+ (* x (+ (* x 2.43) (vec3 0.59 0.59 0.59))) (vec3 0.14 0.14 0.14)))
         (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0)))

(define-shader-function srgb-encode (x)
  (if (<= x 0.0031308) (* x 12.92) (- (* 1.055 (expt x 0.41666666)) 0.055)))

;; The bloom levels: SOURCE the level above or below, BASE the level of this
;; size where one is added.
(define-post-fragment bloom-down-fragment
  (set-output color (vec4 #.(down-form) 1.0)))

(define-post-fragment bloom-up-fragment
  (set-output color (vec4 (+ (finite-radiance3 (swizzle (sample base source-sampler uv) :rgb))
                             #.(tent-form))
                          1.0)))

;; BASE the scene, SOURCE the bloom: scene (1 - s) + bloom s, exposed, ACES,
;; sRGB-encoded unless the target's format does it.
(define-post-fragment tone-map-fragment
  (let* ((scene (finite-radiance3 (swizzle (sample base source-sampler uv) :rgb)))
         (c (aces-filmic (* (mix scene #.(tent-form) (swizzle post :y)) (swizzle post :x)))))
    (set-output color
                (if (> (swizzle post :z) 0.5)
                    (vec4 (srgb-encode (swizzle c :x)) (srgb-encode (swizzle c :y))
                          (srgb-encode (swizzle c :z)) 1.0)
                    (vec4 c 1.0)))))


;; Roles for luv's live shader pipelines.  Each method returns the current
;; definition; a re-evaluated DEFINE-SHADER bumps luv's source revision, so
;; the renderer's pipelines rebuild at the next frame.
(defmethod shader-specification-for ((role (eql :earth)) (stage (eql :vertex)))
  (earth-vertex))
(defmethod shader-specification-for ((role (eql :earth)) (stage (eql :fragment)))
  (earth-fragment))
(defmethod shader-specification-for ((role (eql :overlay)) (stage (eql :vertex)))
  (overlay-vertex))
(defmethod shader-specification-for ((role (eql :overlay)) (stage (eql :fragment)))
  (overlay-fragment))
(defmethod shader-specification-for ((role (eql :stars)) (stage (eql :vertex)))
  (star-vertex))
(defmethod shader-specification-for ((role (eql :stars)) (stage (eql :fragment)))
  (star-fragment))
(defmethod shader-specification-for ((role (eql :quads)) (stage (eql :vertex)))
  (overlay-vertex))
(defmethod shader-specification-for ((role (eql :quads)) (stage (eql :fragment)))
  (overlay-fragment))
(macrolet ((post-roles (&rest pairs)
             `(progn
                ,@(loop for (role fragment) on pairs by #'cddr
                        collect `(defmethod shader-specification-for
                                     ((role (eql ,role)) (stage (eql :vertex)))
                                   (post-vertex))
                        collect `(defmethod shader-specification-for
                                     ((role (eql ,role)) (stage (eql :fragment)))
                                   (,fragment))))))
  (post-roles :bloom-down bloom-down-fragment :bloom-up bloom-up-fragment :tone-map tone-map-fragment))
