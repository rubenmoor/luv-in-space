(in-package #:luv-in-space.shaders)

;;; PLACEHOLDER(76): the placeholder Earth sphere sampling the Blue Marble;
;;; screen-space quads for the placeholder stars and the overlay's labels.

;; One uniform block for every pipeline: clip rows for camera-relative
;; points, the body relative to the eye (w: radius), the sun direction,
;; screen (xy: clip units per pixel), and the rows of Earth's body-fixed
;; matrix (ICRF -> body-fixed). RESOURCES in OPTIONS come after the block.
(defmacro define-frame-shader (name (&rest options &key resources &allow-other-keys)
                               &body body)
  `(define-shader ,name
       (,@(loop for (key value) on options by #'cddr
                unless (eq key :resources) append (list key value))
        :resources ((frame :uniform-block :binding 0
                     :members ((clip-x :vec4) (clip-y :vec4)
                               (clip-z :vec4) (clip-w :vec4)
                               (body :vec4) (sun :vec4) (screen :vec4)
                               (fixed-x :vec4) (fixed-y :vec4) (fixed-z :vec4)))
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
