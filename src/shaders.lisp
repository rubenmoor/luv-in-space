(in-package #:luv-in-space.shaders)

;;; PLACEHOLDER(23): the walking skeleton's shaders, an Earth sphere and
;;; screen-space quads (stars, the label).

;; One uniform block for both pipelines: clip rows for camera-relative
;; points, the body relative to the eye (w: radius), the sun direction, and
;; screen (xy: clip units per pixel).
(defmacro define-frame-shader (name (&rest options) &body body)
  `(define-shader ,name
       (,@options
        :resources ((frame :uniform-block :binding 0
                     :members ((clip-x :vec4) (clip-y :vec4)
                               (clip-z :vec4) (clip-w :vec4)
                               (body :vec4) (sun :vec4) (screen :vec4)))))
     ,@body))

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

;; Oceans, land where a few summed waves exceed a threshold, ice caps.
(define-frame-shader earth-fragment
    (:stage :fragment
     :inputs ((normal :vec3 :location 0))
     :outputs ((color :vec4 :location 0)))
  (let* ((n (normalize normal))
         (waves (+ (* (sin (* 5.0 (swizzle n :x))) (sin (+ (* 4.0 (swizzle n :y)) 1.0))
                      (sin (* 3.0 (swizzle n :z))))
                   (* 0.35 (sin (+ (* 9.0 (swizzle n :x)) (* 7.0 (swizzle n :z)))))
                   (* 0.2 (sin (* 13.0 (swizzle n :y))))))
         (land (smoothstep 0.18 0.24 waves))
         (latitude (abs (swizzle n :y)))
         (ocean (mix (vec3 0.02 0.08 0.35) (vec3 0.05 0.2 0.55) (- 1.0 latitude)))
         (ground (mix (vec3 0.12 0.35 0.08) (vec3 0.45 0.33 0.15)
                      (smoothstep 0.3 0.6 (- waves 0.2))))
         (ice (smoothstep 0.82 0.88 latitude))
         (albedo (mix (mix ocean ground land) (vec3 0.9 0.92 0.95) ice))
         (diffuse (clamp (dot n (swizzle sun :xyz)) 0.0 1.0)))
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
