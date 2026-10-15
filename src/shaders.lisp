(in-package #:luv-in-space.shaders)

;;; The stars at infinity (src/stars.lisp); the Sun, the Moon, Earth and
;;; Earth's halo as sphere impostors (src/impostors.lisp); screen-space quads
;;; for the overlay's labels and the tests' quads; the post passes (decision
;;; 76: Frame passes, radiance scale and exposure).

;; One uniform block for every pipeline: clip rows for camera-relative
;; points, the halo's day colour (w: its radiance), the direction to the Sun
;; (w: (1 AU / d)^2), screen (xy: clip units per pixel), the rows of Earth's
;; body-fixed matrix (ICRF -> body-fixed), post (exposure, bloom strength,
;; sRGB encoding 0 or 1), stars (radiance limit, sigma, sigma per magnitude,
;; sigma max; src/stars.lisp), per impostor four vec4s (centre and radius;
;; quad axis u and c.c - r^2; quad axis v; shade; src/impostors.lisp), and
;; the halo's dusk colour. RESOURCES in OPTIONS come after the block.
(defmacro define-frame-shader (name (&rest options &key resources &allow-other-keys)
                               &body body)
  `(define-shader ,name
       (,@(loop for (key value) on options by #'cddr
                unless (eq key :resources) append (list key value))
        :resources ((frame :uniform-block :binding 0
                     :members ((clip-x :vec4) (clip-y :vec4)
                               (clip-z :vec4) (clip-w :vec4)
                               (halo-day :vec4) (sun :vec4) (screen :vec4)
                               (fixed-x :vec4) (fixed-y :vec4) (fixed-z :vec4)
                               (post :vec4) (stars :vec4)
                               (sun-c :vec4) (sun-u :vec4) (sun-v :vec4) (sun-shade :vec4)
                               (moon-c :vec4) (moon-u :vec4) (moon-v :vec4) (moon-shade :vec4)
                               (earth-c :vec4) (earth-u :vec4) (earth-v :vec4) (earth-shade :vec4)
                               (halo-c :vec4) (halo-u :vec4) (halo-v :vec4) (halo-shade :vec4)
                               (halo-dusk :vec4)))
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

;;; Sphere impostors (src/impostors.lisp; docs/architecture.md "Rendering
;;; precision"). CORNER: x, y and 1 for the full-screen triangle, 0 for the
;;; quad c + x u + y v over the tangent cone.

(define-shader-function impostor-clip (corner centre u v rx ry rz rw)
  (let* ((point (vec4 (+ (swizzle centre :xyz) (* (swizzle u :xyz) (swizzle corner :x))
                         (* (swizzle v :xyz) (swizzle corner :y)))
                      1.0))
         (quad (vec4 (dot rx point) (dot ry point) (dot rz point) (dot rw point))))
    (if (> (swizzle corner :z) 0.5)
        (vec4 (swizzle corner :x) (swizzle corner :y) 0.0 1.0)
        quad)))

;; The unit view ray through PIXEL (frag-coord): forward plus the NDC
;; offsets over the clip rows' focal scales; SCREEN.xy is 2 / size.
(define-shader-function view-ray (pixel screen rx ry rw)
  (let* ((right (swizzle rx :xyz)) (down (swizzle ry :xyz))
         (x (- (* (swizzle pixel :x) (swizzle screen :x)) 1.0))
         (y (- (* (swizzle pixel :y) (swizzle screen :y)) 1.0)))
    (normalize (+ (swizzle rw :xyz) (* right (/ x (dot right right)))
                  (* down (/ y (dot down down)))))))

;; h² = r² − |b d − c|² = b² − (c·c − r²), b = d·c, without the large terms
;; cancelling; negative: a miss.
(define-shader-function sphere-half-chord2 (ray centre radius b)
  (let* ((q (- (* ray b) centre)))
    (- (* radius radius) (dot q q))))

;; The stable roots: t = k / (b + h) and its partner, k = c·c − r² from
;; double on the CPU.
(define-shader-function ray-sphere-near (b h2 k)
  (/ k (+ b (sqrt (max h2 0.0)))))

(define-shader-function ray-sphere-far (b h2 k)
  (let* ((h (sqrt (max h2 0.0))))
    (if (>= b 0.0) (+ b h) (/ k (- b h)))))

;; The near hit's normal, (b d − c − h d) / r, from the small terms.
(define-shader-function sphere-near-normal (ray centre radius b h2)
  (/ (- (* ray b) centre (* ray (sqrt (max h2 0.0)))) radius))

;; Reversed-Z: near / (t d·forward) (decision 4: Depth strategy).
(define-shader-function impostor-depth (tt ray rz rw)
  (/ (swizzle rz :w) (* tt (dot ray (swizzle rw :xyz)))))

(defmacro define-impostor-vertex (name centre u v)
  `(define-frame-shader ,name
       (:stage :vertex
        :inputs ((corner :vec3 :location 0))
        :outputs ((clip-position :vec4 :built-in :position)))
     (set-output clip-position (impostor-clip corner ,centre ,u ,v clip-x clip-y clip-z clip-w))))

(define-impostor-vertex sun-vertex sun-c sun-u sun-v)
(define-impostor-vertex moon-vertex moon-c moon-u moon-v)
(define-impostor-vertex earth-vertex earth-c earth-u earth-v)
(define-impostor-vertex halo-vertex halo-c halo-u halo-v)

;; The Sun's disc (decision 78): SUN-SHADE.xyz the centre radiance's colour,
;; .w the limb darkening u: I/I0 = 1 − u (1 − μ), μ = h / r.
(define-frame-shader sun-fragment
    (:stage :fragment
     :inputs ((pixel :vec4 :built-in :frag-coord))
     :outputs ((color :vec4 :location 0)
               (depth :float :built-in :frag-depth)))
  (let* ((ray (view-ray pixel screen clip-x clip-y clip-w))
         (centre (swizzle sun-c :xyz))
         (radius (swizzle sun-c :w))
         (b (dot ray centre))
         (h2 (sphere-half-chord2 ray centre radius b))
         (tt (ray-sphere-near b h2 (swizzle sun-u :w)))
         (mu (/ (sqrt (max h2 0.0)) radius)))
    (when (or (< h2 0.0) (<= tt 0.0))
      (discard))
    (set-output color (vec4 (* (swizzle sun-shade :xyz)
                               (- 1.0 (* (swizzle sun-shade :w) (- 1.0 mu))))
                            1.0))
    (set-output depth (impostor-depth tt ray clip-z clip-w))))

;; The Moon: grey Lambert, MOON-SHADE.xyz the direction to the Sun, .w the
;; albedo × (1 AU / d)².
(define-frame-shader moon-fragment
    (:stage :fragment
     :inputs ((pixel :vec4 :built-in :frag-coord))
     :outputs ((color :vec4 :location 0)
               (depth :float :built-in :frag-depth)))
  (let* ((ray (view-ray pixel screen clip-x clip-y clip-w))
         (centre (swizzle moon-c :xyz))
         (radius (swizzle moon-c :w))
         (b (dot ray centre))
         (h2 (sphere-half-chord2 ray centre radius b))
         (tt (ray-sphere-near b h2 (swizzle moon-u :w)))
         (n (sphere-near-normal ray centre radius b h2))
         (light (* (swizzle moon-shade :w) (max (dot n (swizzle moon-shade :xyz)) 0.0))))
    (when (or (< h2 0.0) (<= tt 0.0))
      (discard))
    (set-output color (vec4 light light light 1.0))
    (set-output depth (impostor-depth tt ray clip-z clip-w))))

(define-shader-function blue-marble-uv (n)
  (vec2 (+ 0.5 (/ (atan2 (swizzle n :y) (swizzle n :x)) 6.2831855))
        (- 0.5 (/ (arcsin (clamp (swizzle n :z) -1.0 1.0)) 3.1415927))))

;; Earth (decision 78): the Blue Marble at the hit's body-fixed normal
;; (docs/architecture.md "Frames"), level 0 of the full map and of its 1/8
;; copy mixed by the pixel's footprint in full texels, log8 from 1 to 8;
;; lit by the Sun, plus the rim on the lit limb. EARTH-SHADE: rim strength,
;; a full texel in m, ln 8.
(define-frame-shader earth-fragment
    (:stage :fragment
     :inputs ((pixel :vec4 :built-in :frag-coord))
     :outputs ((color :vec4 :location 0)
               (depth :float :built-in :frag-depth))
     :resources ((earth :texture-2d :binding 1)
                 (earth-sampler :sampler :binding 2)
                 (earth-coarse :texture-2d :binding 3)))
  (let* ((ray (view-ray pixel screen clip-x clip-y clip-w))
         (centre (swizzle earth-c :xyz))
         (radius (swizzle earth-c :w))
         (b (dot ray centre))
         (h2 (sphere-half-chord2 ray centre radius b))
         (tt (ray-sphere-near b h2 (swizzle earth-u :w)))
         (n (sphere-near-normal ray centre radius b h2))
         (fixed (vec3 (dot (swizzle fixed-x :xyz) n) (dot (swizzle fixed-y :xyz) n)
                      (dot (swizzle fixed-z :xyz) n)))
         (uv (blue-marble-uv fixed))
         (mu (clamp (- 0.0 (dot n ray)) 0.0 1.0))
         (pixel-angle (/ (swizzle screen :y) (sqrt (dot (swizzle clip-y :xyz) (swizzle clip-y :xyz)))))
         (footprint (/ (* tt pixel-angle) (* (max mu 0.02) (swizzle earth-shade :y))))
         (coarse (clamp (/ (log (max footprint 1e-6)) (swizzle earth-shade :z)) 0.0 1.0))
         (albedo (mix (swizzle (sample-level earth earth-sampler uv 0.0) :rgb)
                      (swizzle (sample-level earth-coarse earth-sampler uv 0.0) :rgb)
                      coarse))
         (cos-sun (dot n (swizzle sun :xyz)))
         (light (* (max cos-sun 0.0) (swizzle sun :w)))
         (rim (* (swizzle earth-shade :x) (expt (- 1.0 mu) 4.0)
                 (clamp (+ cos-sun 0.2) 0.0 1.0) (swizzle sun :w))))
    (when (or (< h2 0.0) (<= tt 0.0))
      (discard))
    (set-output color (vec4 (+ (* albedo light) (* (swizzle halo-day :xyz) rim)) 1.0))
    (set-output depth (impostor-depth tt ray clip-z clip-w))))

;; Earth's halo, the shell up to the crash sphere (decision 78): the ray's
;; chord through it, to the surface when it meets Earth, from the eye when
;; the eye is inside, over the tangent chord, to HALO-SHADE.x; blue by day,
;; dusk within HALO-SHADE.z (sin 6 deg) of the Sun's horizon at the chord's
;; point nearest Earth's centre, nothing that far below. Depth at the
;; chord's start, or its end with the eye inside, so nearer things hide it.
;; HALO-SHADE: power, the tangent chord, sin 6 deg.
(define-frame-shader halo-fragment
    (:stage :fragment
     :inputs ((pixel :vec4 :built-in :frag-coord))
     :outputs ((color :vec4 :location 0)
               (depth :float :built-in :frag-depth)))
  (let* ((ray (view-ray pixel screen clip-x clip-y clip-w))
         (centre (swizzle halo-c :xyz))
         (k (swizzle halo-u :w))
         (b (dot ray centre))
         (h2 (sphere-half-chord2 ray centre (swizzle halo-c :w) b))
         (h (sqrt (max h2 0.0)))
         (inside (if (<= k 0.0) 1.0 0.0))
         (near (if (> inside 0.5) 0.0 (ray-sphere-near b h2 k)))
         (far (ray-sphere-far b h2 k))
         (ground-h2 (sphere-half-chord2 ray centre (swizzle earth-c :w) b))
         (ground (ray-sphere-near b ground-h2 (swizzle earth-u :w)))
         (hits (if (< ground-h2 0.0) 0.0 (if (> ground 0.0) 1.0 0.0)))
         (end (if (> hits 0.5) ground far))
         (chord (if (> hits 0.5) (- ground near) (if (> inside 0.5) far (* 2.0 h))))
         (closest (clamp b near end))
         (sin-sun (dot (normalize (- (* ray closest) centre)) (swizzle sun :xyz)))
         (band (/ sin-sun (swizzle halo-shade :z)))
         (colour (mix (swizzle halo-dusk :xyz) (swizzle halo-day :xyz) (clamp band 0.0 1.0)))
         (strength (* (swizzle halo-day :w) (clamp (+ 1.0 band) 0.0 1.0)
                      (expt (clamp (/ chord (swizzle halo-shade :y)) 0.0 1.0)
                            (swizzle halo-shade :x)))))
    (when (or (< h2 0.0) (<= far 0.0))
      (discard))
    (set-output color (vec4 (* colour strength) 0.0))
    (set-output depth (impostor-depth (if (> inside 0.5) end near) ray clip-z clip-w))))

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

  ;; The 3x3 tent, weights 1 2 1 / 2 4 2 / 1 2 1 over 16; the tone map's last step.
  (defun tent-form ()
    `(+ ,(taps-form 0.25 '((0 0)))
        ,(taps-form 0.125 '((0 -1) (0 1) (-1 0) (1 0)))
        ,(taps-form 0.0625 '((-1 -1) (1 -1) (-1 1) (1 1)))))

  (defun disc-form (&optional (rings '((1.0 6 0.0) (2.0 12 15.0))) (sigma 1.0))
    "19 taps in a disc, bilinear: the centre and RINGS (radius in texels, taps, first
angle in degrees), weighted by a Gaussian of SIGMA texels, normalised."
    (let* ((taps (cons (list 0.0 0.0)
                       (loop for (radius count start) in rings
                             append (loop for i below count
                                          for a = (* (/ pi 180) (+ start (* i (/ 360.0 count))))
                                          collect (list (* radius (cos a)) (* radius (sin a)))))))
           (weights (mapcar (lambda (tap)
                              (exp (- (/ (+ (expt (first tap) 2) (expt (second tap) 2))
                                         (* 2 sigma sigma)))))
                            taps))
           (total (reduce #'+ weights)))
      `(+ ,@(loop for (dx dy) in taps for w in weights
                  collect `(* ,(tap-form dx dy) ,(float (/ w total) 1.0)))))))

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
;; size where one is added. The way up uses the round kernel: the tent's
;; square footprint showed in the Sun's saturated glare (task 145).
(define-post-fragment bloom-down-fragment
  (set-output color (vec4 #.(down-form) 1.0)))

(define-post-fragment bloom-up-fragment
  (set-output color (vec4 (+ (finite-radiance3 (swizzle (sample base source-sampler uv) :rgb))
                             #.(disc-form))
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
(defmethod shader-specification-for ((role (eql :earth-halo)) (stage (eql :vertex)))
  (halo-vertex))
(defmethod shader-specification-for ((role (eql :earth-halo)) (stage (eql :fragment)))
  (halo-fragment))
(defmethod shader-specification-for ((role (eql :overlay)) (stage (eql :vertex)))
  (overlay-vertex))
(defmethod shader-specification-for ((role (eql :overlay)) (stage (eql :fragment)))
  (overlay-fragment))
(defmethod shader-specification-for ((role (eql :stars)) (stage (eql :vertex)))
  (star-vertex))
(defmethod shader-specification-for ((role (eql :stars)) (stage (eql :fragment)))
  (star-fragment))
(defmethod shader-specification-for ((role (eql :sun)) (stage (eql :vertex)))
  (sun-vertex))
(defmethod shader-specification-for ((role (eql :sun)) (stage (eql :fragment)))
  (sun-fragment))
(defmethod shader-specification-for ((role (eql :moon)) (stage (eql :vertex)))
  (moon-vertex))
(defmethod shader-specification-for ((role (eql :moon)) (stage (eql :fragment)))
  (moon-fragment))
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
