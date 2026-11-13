(in-package #:luv-in-space.shaders)

;;; The stars at infinity (src/stars.lisp); the Sun, the Moon, Earth and
;;; Earth's halo as sphere impostors (src/impostors.lisp); screen-space quads
;;; for the overlay's labels and the tests' quads; the post passes (decision
;;; 76: Frame passes, radiance scale and exposure).

;; One uniform block for every pipeline: clip rows for camera-relative
;; points, the halo's day colour (w: its radiance), the direction to the Sun
;; (w: (1 AU / d)^2), screen (xy: clip units per pixel; z, w: the ribbon's
;; width in pixels and radiance), the rows of Earth's
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

;;; The Mule (src/meshes.lisp; decision 80: The Mule's meshes in numbers:
;;; geometry, draws, materials and lights). Vertices in ship body axes from
;;; R; the SHIP block: the rows of q (body -> ICRF), R relative to the eye
;;; (w: the glows' least sigma in pixels), the gimbal point and the rows of
;;; the engine's trim rotation, then the light in body axes: the Sun's
;;; direction (w: (1 AU / d)^2 x sun-fraction),
;;; the direction to Earth (w: earthshine), the earthshine tint (w: ambient),
;;; the eye (w: lights on), the emissive colour, then the floodlight: its lamp
;; position (w: its intensity, 0 when off) and beam axis in body axes
;; (decision 124). MATERIALS: two vec4s per
;;; row of *ship-materials* (r g b specular weight; exponent emission).
(defmacro define-ship-shader (name (&rest options) &body body)
  `(define-frame-shader ,name
       (,@options
        :resources ((decal :texture-2d :binding 1)
                    (decal-sampler :sampler :binding 2)
                    (ship :uniform-block :binding 3
                     :members ((ship-x :vec4) (ship-y :vec4) (ship-z :vec4) (ship-origin :vec4)
                               (gimbal :vec4) (trim-x :vec4) (trim-y :vec4) (trim-z :vec4)
                               (ship-sun :vec4) (ship-earth :vec4) (ship-tint :vec4)
                               (ship-eye :vec4) (ship-emissive :vec4)
                               (flood-position :vec4) (flood-axis :vec4)))
                    (materials :storage-buffer :binding 4 :element :vec4)))
     ,@body))

(define-shader-function turn (p rx ry rz)
  (vec3 (dot (swizzle rx :xyz) p) (dot (swizzle ry :xyz) p) (dot (swizzle rz :xyz) p)))

;; A body point through q, plus R relative to the eye, through the clip rows.
(define-shader-function ship-clip (p sx sy sz origin rx ry rz rw)
  (let* ((point (vec4 (+ (turn p sx sy sz) (swizzle origin :xyz)) 1.0)))
    (vec4 (dot rx point) (dot ry point) (dot rz point) (dot rw point))))

;; The baked hull, and the engine (drawn as instance 1) turned by its trim
;; through the gimbal. SURFACE: material, the two parameters.
(define-ship-shader ship-mesh-vertex
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (normal :vec3 :location 1)
              (params :vec3 :location 2)
              (instance :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (body :vec3 :location 0)
               (body-normal :vec3 :location 1)
               (surface :vec3 :location 2)))
  (let* ((g (swizzle gimbal :xyz))
         (engine (if (> instance (uint 0.0)) 1.0 0.0))
         (p (mix position (+ g (turn (- position g) trim-x trim-y trim-z)) engine))
         (n (mix normal (turn normal trim-x trim-y trim-z) engine)))
    (set-output clip-position (ship-clip p ship-x ship-y ship-z ship-origin clip-x clip-y clip-z clip-w))
    (set-output body p)
    (set-output body-normal n)
    (set-output surface params)))

;; A unit primitive placed by its instance: origin + basis (scale x local);
;; normals through basis (n / scale); the axial parameters scaled to metres,
;; the material the instance's.
(define-ship-shader ship-instanced-vertex
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (normal :vec3 :location 1)
              (params :vec3 :location 2)
              (origin :vec3 :location 3)
              (axis-x :vec3 :location 4)
              (axis-y :vec3 :location 5)
              (axis-z :vec3 :location 6)
              (scale :vec4 :location 7))
     :outputs ((clip-position :vec4 :built-in :position)
               (body :vec3 :location 0)
               (body-normal :vec3 :location 1)
               (surface :vec3 :location 2)))
  (let* ((s (swizzle scale :xyz))
         (local (* position s))
         (p (+ origin (* axis-x (swizzle local :x)) (* axis-y (swizzle local :y))
               (* axis-z (swizzle local :z))))
         (m (/ normal s))
         (n (normalize (+ (* axis-x (swizzle m :x)) (* axis-y (swizzle m :y))
                          (* axis-z (swizzle m :z))))))
    (set-output clip-position (ship-clip p ship-x ship-y ship-z ship-origin clip-x clip-y clip-z clip-w))
    (set-output body p)
    (set-output body-normal n)
    (set-output surface (vec3 (swizzle scale :w) (* (swizzle params :y) (swizzle s :x))
                              (* (swizzle params :z) (swizzle s :x))))))

;; Hoskins's hash without sine, [0, 1).
(define-shader-function hash13 (p)
  (let* ((q (fract (* p 0.1031)))
         (r (+ q (vec3 (dot q (+ (swizzle q :yzx) (vec3 33.33 33.33 33.33)))
                       (dot q (+ (swizzle q :yzx) (vec3 33.33 33.33 33.33)))
                       (dot q (+ (swizzle q :yzx) (vec3 33.33 33.33 33.33)))))))
    (fract (* (+ (swizzle r :x) (swizzle r :y)) (swizzle r :z)))))

;; Value noise on the unit lattice, smooth, [0, 1).
(define-shader-function value-noise (p)
  (let* ((i (floor p))
         (f (fract p))
         (u (* f f (- (vec3 3.0 3.0 3.0) (* f 2.0))))
         (a (mix (hash13 i) (hash13 (+ i (vec3 1.0 0.0 0.0))) (swizzle u :x)))
         (b (mix (hash13 (+ i (vec3 0.0 1.0 0.0))) (hash13 (+ i (vec3 1.0 1.0 0.0))) (swizzle u :x)))
         (c (mix (hash13 (+ i (vec3 0.0 0.0 1.0))) (hash13 (+ i (vec3 1.0 0.0 1.0))) (swizzle u :x)))
         (d (mix (hash13 (+ i (vec3 0.0 1.0 1.0))) (hash13 (+ i (vec3 1.0 1.0 1.0))) (swizzle u :x))))
    (mix (mix a b (swizzle u :y)) (mix c d (swizzle u :y)) (swizzle u :z))))

;; The foil's crinkle: N tilted against the gradient of value noise of
;; the body position, cell 0.08 m, on a lattice turned off the body axes so
;; it shows no squares; AMPLITUDE 0.25 (decision 80), the peeling foil's 0.6.
(define-shader-function foil-normal (n p amplitude)
  (let* ((q (/ (vec3 (dot p (vec3 0.667 0.667 -0.333)) (dot p (vec3 -0.333 0.667 0.667))
                     (dot p (vec3 0.667 -0.333 0.667)))
               0.08))
         (h (value-noise q))
         (e 0.3)
         (g (/ (vec3 (- (value-noise (+ q (vec3 e 0.0 0.0))) h)
                     (- (value-noise (+ q (vec3 0.0 e 0.0))) h)
                     (- (value-noise (+ q (vec3 0.0 0.0 e))) h))
               e))
         (body-g (+ (* (vec3 0.667 -0.333 0.667) (swizzle g :x))
                    (* (vec3 0.667 0.667 -0.333) (swizzle g :y))
                    (* (vec3 -0.333 0.667 0.667) (swizzle g :z))))
         (tangent (- body-g (* n (dot body-g n)))))
    (normalize (- n (* tangent amplitude)))))

;; The rows the fragment names, in row order: the test checks them against
;; *ship-material-names*.
(defparameter *ship-shader-material-rows*
  '(:steel :lh2-foil :lox-foil :cab-paint :stencil :cairn :red-paint :solar-cells
    :radiator :aluminium :nozzle :throat :glass :hatch :decal
    :decal-m04 :decal-m05 :decal-tern :decal-pod-14 :rock :net :scorched :peeling-foil
    :fixture :boreal-green))

;; One fragment for both pipelines. Material rows (src/meshes.lisp
;; *ship-material-names*): 0 steel, 1 LH2 foil, 2 LOX foil, 6 red paint, 10
;; nozzle, 11 throat, 13 hatch, 14 to 18 the decals (one atlas row each), 19
;; rock, 20 net, 21 scorched, 22 peeling foil, 23 fixture. Steel darker x0.6
;; within 0.06 m of a tube's end; the bell's first 0.15 m the throat's (the
;; only nozzle part over 1 m long); the hatch's 0.05 m border red; the decal
;; over the paint; the net's cords 0.04 m wide every 0.45 m on the rock
;; (the surface parameters its arc lengths along latitude and longitude); the
;; scorched steel's streaks of value noise along x; the peeling foil's patches
;; where the noise exceeds 0.55 showing scorched steel; the fixture's black
;; cross from the plate's centre coordinates (decision 124).
;; Light: the Sun, earthshine tinted, ambient; Blinn-Phong in the Sun; the
;; floodlight: intensity / max(d, 2 m)² x the cosine on the face x a cone,
;; smoothstep of the beam's cosine from 25° (0.9063) to 15° (0.9659).
(define-ship-shader ship-fragment
    (:stage :fragment
     :inputs ((body :vec3 :location 0)
              (body-normal :vec3 :location 1)
              (surface :vec3 :location 2))
     :outputs ((color :vec4 :location 0)))
  (let* ((id (uint (+ (swizzle surface :x) 0.5)))
         (axial (swizzle surface :y))
         (span (swizzle surface :z))
         (throat (and (= id (uint 10.0)) (> span 1.0) (< axial 0.15)))
         (border (and (= id (uint 13.0))
                      (or (> (abs axial) 0.35) (> (abs span) 0.55))))
         (cord (and (= id (uint 20.0))
                    (or (< (abs (- (fract (+ (/ axial 0.45) 0.5)) 0.5)) 0.044)
                        (< (abs (- (fract (+ (/ span 0.45) 0.5)) 0.5)) 0.044))))
         (patchy (value-noise (+ (* body 1.3) (vec3 11.0 5.0 3.0))))
         (patch (and (= id (uint 22.0)) (> patchy 0.55)))
         (row (if throat (uint 11.0)
                  (if border (uint 6.0)
                      (if (= id (uint 20.0)) (if cord (uint 20.0) (uint 19.0))
                          (if patch (uint 21.0) id)))))
         (m0 (buffer-element materials (* row (uint 2.0))))
         (m1 (buffer-element materials (+ (* row (uint 2.0)) (uint 1.0))))
         (weld (if (and (= id (uint 0.0)) (> span 0.0)
                        (or (< axial 0.06) (< (- span axial) 0.06)))
                   0.6 1.0))
         (decal-row (max (- (swizzle surface :x) 14.0) 0.0))
         (texel (sample decal decal-sampler (vec2 axial (/ (+ span decal-row) 5.0))))
         (is-decal (and (> (swizzle surface :x) 13.5) (< (swizzle surface :x) 18.5)))
         (streak (+ 0.55 (* 0.45 (value-noise (vec3 (* (swizzle body :x) 0.5)
                                                    (* (swizzle body :y) 5.0)
                                                    (* (swizzle body :z) 5.0))))))
         (target (and (= id (uint 23.0))
                      (or (< (abs axial) 0.025) (< (abs span) 0.025))
                      (< (+ (* axial axial) (* span span)) 0.2025)))
         (paint (swizzle m0 :xyz))
         (albedo (* (if is-decal (mix paint (swizzle texel :rgb) (swizzle texel :w)) paint)
                    weld
                    (if (or (= id (uint 21.0)) patch) streak 1.0)
                    (if target 0.04 1.0)))
         (flat (normalize body-normal))
         (amplitude (if (or (= id (uint 1.0)) (= id (uint 2.0))) 0.25
                        (if (and (= id (uint 22.0)) (<= patchy 0.55)) 0.6 0.0)))
         (n (if (> amplitude 0.0) (foil-normal flat body amplitude) flat))
         (s (swizzle ship-sun :xyz))
         (v (normalize (- (swizzle ship-eye :xyz) body)))
         (h (normalize (+ s v)))
         (sun (* (max (dot n s) 0.0) (swizzle ship-sun :w)))
         (earth (* (max (dot n (swizzle ship-earth :xyz)) 0.0) (swizzle ship-earth :w)))
         (to-lamp (- (swizzle flood-position :xyz) body))
         (lamp-distance (max (sqrt (dot to-lamp to-lamp)) 0.001))
         (lamp-direction (/ to-lamp lamp-distance))
         (beam (- 0.0 (dot lamp-direction (swizzle flood-axis :xyz))))
         (fade (max 0.0 (min 1.0 (/ (- beam 0.9063) 0.0596))))
         (cone (* fade fade (- 3.0 (* 2.0 fade))))
         (reach (max lamp-distance 2.0))
         (flood (/ (* (max (dot n lamp-direction) 0.0) cone (swizzle flood-position :w))
                   (* reach reach)))
         (light (+ (* (vec3 1.0 1.0 1.0) sun) (* (swizzle ship-tint :xyz) earth)
                   (* (vec3 1.0 1.0 1.0) (swizzle ship-tint :w))
                   (* (vec3 1.0 1.0 1.0) flood)))
         (specular (* (swizzle m0 :w) (expt (max (dot n h) 0.0) (swizzle m1 :x)) sun))
         (glow (* (swizzle m1 :y) (swizzle ship-eye :w))))
    (set-output color (vec4 (+ (* albedo light) (* (vec3 1.0 1.0 1.0) (+ specular glow))
                               (swizzle ship-emissive :xyz))
                            1.0))))

;; The Mule's glows (src/meshes.lisp ship-glows!): a camera-facing quad of
;; 4 sigma around CENTRE.xyz (relative to the eye) pulled towards the eye by
;; SHADE.w m, sigma CENTRE.w m but at least SHIP-ORIGIN.w pixels, the
;; radiance then scaled down by the area ratio.
(define-ship-shader ship-glow-vertex
    (:stage :vertex
     :inputs ((corner :vec2 :location 0)
              (centre :vec4 :location 1)
              (shade :vec4 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (offset :vec2 :location 0)
               (radiance :vec3 :location 1)
               (sigma :float :location 2)))
  (let* ((c (swizzle centre :xyz))
         (d (max (sqrt (dot c c)) 0.001))
         (point (vec4 (* c (- 1.0 (/ (min (swizzle shade :w) (* d 0.5)) d))) 1.0))
         (w (dot clip-w point))
         (focal (sqrt (dot (swizzle clip-y :xyz) (swizzle clip-y :xyz))))
         (natural (/ (* (swizzle centre :w) focal) (* (max w 1e-6) (swizzle screen :y))))
         (size (max natural (swizzle ship-origin :w)))
         (pixels (* corner (* 4.0 size))))
    (set-output clip-position
                (vec4 (+ (dot clip-x point) (* (swizzle pixels :x) (swizzle screen :x) w))
                      (+ (dot clip-y point) (* (swizzle pixels :y) (swizzle screen :y) w))
                      (dot clip-z point) w))
    (set-output offset pixels)
    (set-output radiance (* (swizzle shade :xyz) (/ (* natural natural) (* size size))))
    (set-output sigma size)))

;; Additive through premultiplied alpha: alpha 0. The Gaussian less its
;; value at 4 sigma, so a bright glow ends round and smooth inside the quad.
(define-ship-shader ship-glow-fragment
    (:stage :fragment
     :inputs ((offset :vec2 :location 0)
              (radiance :vec3 :location 1)
              (sigma :float :location 2))
     :outputs ((color :vec4 :location 0)))
  (let* ((g (exp (/ (* -0.5 (dot offset offset)) (* sigma sigma))))
         (edge 0.00033546262))
    (set-output color (vec4 (* radiance (/ (max (- g edge) 0.0) (- 1.0 edge))) 0.0))))

;;; The map's ribbon (src/ribbons.lisp; decision 84: Map ribbons and
;;; markers: osculating orbit, adaptive segments, overlay markers and
;;; visibility): one segment per instance, FROM and TO relative to the eye,
;;; a 6-vertex quad (CORNER: along 0 or 1, across ±1) SCREEN.z px wide plus
;;; a 1 px fringe; an endpoint behind the near plane moved along the segment
;;; to it, a segment wholly behind collapsed. ACROSS: pixels from the centre line.
(define-frame-shader ribbon-vertex
    (:stage :vertex
     :inputs ((corner :vec2 :location 0)
              (from :vec4 :location 1)
              (to :vec4 :location 2)
              (tint :vec4 :location 3))
     :outputs ((clip-position :vec4 :built-in :position)
               (across :float :location 0)
               (shade :vec3 :location 1)))
  (let* ((a0 (vec4 (dot clip-x from) (dot clip-y from) (dot clip-z from) (dot clip-w from)))
         (b0 (vec4 (dot clip-x to) (dot clip-y to) (dot clip-z to) (dot clip-w to)))
         (limit (* (swizzle clip-z :w) 1.001))
         (wa (swizzle a0 :w)) (wb (swizzle b0 :w))
         (a (if (< wa limit) (mix a0 b0 (/ (- limit wa) (- wb wa))) a0))
         (b (if (< wb limit) (mix b0 a0 (/ (- limit wb) (- wa wb))) b0))
         (pixels (swizzle screen :xy))
         (sa (/ (/ (swizzle a :xy) (swizzle a :w)) pixels))
         (sb (/ (/ (swizzle b :xy) (swizzle b :w)) pixels))
         (along (normalize (+ (- sb sa) (vec2 1e-6 0.0))))
         (normal (vec2 (- 0.0 (swizzle along :y)) (swizzle along :x)))
         (half (+ (* 0.5 (swizzle screen :z)) 0.5))
         (p (mix a b (swizzle corner :x)))
         (offset (* normal (* (swizzle corner :y) half (swizzle p :w)) pixels))
         (visible (if (and (< wa limit) (< wb limit)) 0.0 1.0)))
    (set-output clip-position (* (vec4 (+ (swizzle p :xy) offset) (swizzle p :zw)) visible))
    (set-output across (* (swizzle corner :y) half))
    (set-output shade (swizzle tint :xyz))))

;; Additive through premultiplied alpha: alpha 0. Radiance SHADE x SCREEN.w
;; over the exposure, so the line reads the same at any exposure; the
;; fringe's coverage antialiases the edges.
(define-frame-shader ribbon-fragment
    (:stage :fragment
     :inputs ((across :float :location 0)
              (shade :vec3 :location 1))
     :outputs ((color :vec4 :location 0)))
  (let* ((coverage (clamp (- (+ (* 0.5 (swizzle screen :z)) 0.5) (abs across)) 0.0 1.0)))
    (set-output color (vec4 (* shade (/ (* (swizzle screen :w) coverage) (swizzle post :x))) 0.0))))

;;; The cab's light and openings (src/cockpit.lisp; decision 126: Cockpit
;;; structure and view in milestone 3), and the cab block: clip rows of the head
;;; camera in body axes from the eye, screen (clip units per pixel x, y), params
;;; (1 / exposure, the lit text's constant factor, the ambient), the Sun's
;;; direction in body axes (w: (1 AU / d)^2 x sun-fraction, for the sunlit
;;; patch), the two LED strips (x of the first sample, the step, y, z; eight
;;; samples each), the light (tint, level), the window (x, half width, half
;;; height, slack), and the portholes (centre and radius; axis and half depth).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cab-block-members*
    '((clip-x :vec4) (clip-y :vec4) (clip-z :vec4) (clip-w :vec4)
      (screen :vec4) (params :vec4) (sun :vec4) (strip-a :vec4) (strip-b :vec4)
      (light :vec4) (window :vec4) (hole-a :vec4) (hole-a-axis :vec4)
      (hole-b :vec4) (hole-b-axis :vec4)))

  (defun strip-sum-form (strip)
    "The sum over a strip's eight point samples of the Lambert term at P with normal N."
    `(+ ,@(loop for k below 8
                collect `(cab-point-term p n (vec3 (+ (swizzle ,strip :x)
                                                      (* ,(float k) (swizzle ,strip :y)))
                                                   (swizzle ,strip :z) (swizzle ,strip :w)))))))

;; One point light: n . l / d^2, l the unit direction to the sample S.
(define-shader-function cab-point-term (p n s)
  (let* ((d (- s p))
         (r2 (max (dot d d) 0.0004)))
    (/ (max (dot n d) 0.0) (* r2 (sqrt r2)))))

;; The radiance factor at P with unit normal N: the strips' samples x the
;; level x the tint, plus the ambient (src/cockpit.lisp cab-irradiance).
(define-shader-function cab-irradiance (p n strip-a strip-b light ambient)
  (let* ((sum #.`(+ ,(strip-sum-form 'strip-a) ,(strip-sum-form 'strip-b))))
    (+ (* (swizzle light :xyz) (* (swizzle light :w) sum))
       (vec3 ambient ambient ambient))))

;; 1.0 where P is in the window's rectangle (in the front wall's plane) or a
;; porthole's cylinder, else 0.0 (src/cockpit.lisp cab-opening-p).
(define-shader-function cab-opening-cut (p window hole-a axis-a hole-b axis-b)
  (let* ((in-window (and (< (abs (- (swizzle p :x) (swizzle window :x))) (swizzle window :w))
                         (< (abs (swizzle p :y)) (swizzle window :y))
                         (< (abs (swizzle p :z)) (swizzle window :z))))
         (da (- p (swizzle hole-a :xyz)))
         (along-a (dot da (swizzle axis-a :xyz)))
         (in-a (and (< (abs along-a) (swizzle axis-a :w))
                    (< (- (dot da da) (* along-a along-a))
                       (* (swizzle hole-a :w) (swizzle hole-a :w)))))
         (db (- p (swizzle hole-b :xyz)))
         (along-b (dot db (swizzle axis-b :xyz)))
         (in-b (and (< (abs along-b) (swizzle axis-b :w))
                    (< (- (dot db db) (* along-b along-b))
                       (* (swizzle hole-b :w) (swizzle hole-b :w))))))
    (if (or in-window in-a in-b) 1.0 0.0)))

;; 1.0 where the ray from P along the unit direction S (to the Sun) passes
;; through the window's rectangle or a porthole's disc, else 0.0
;; (src/cockpit.lisp sun-patch-p).
(define-shader-function sun-patch (p s window hole-a axis-a hole-b axis-b)
  (let* ((sx (max (swizzle s :x) 0.001))
         (tw (/ (- (swizzle window :x) (swizzle p :x)) sx))
         (in-window (and (> (swizzle s :x) 0.001)
                         (> tw 0.0)
                         (< (abs (+ (swizzle p :y) (* tw (swizzle s :y)))) (swizzle window :y))
                         (< (abs (+ (swizzle p :z) (* tw (swizzle s :z)))) (swizzle window :z))))
         (fa (dot s (swizzle axis-a :xyz)))
         (ta (/ (dot (- (swizzle hole-a :xyz) p) (swizzle axis-a :xyz)) (max fa 0.001)))
         (da (- (+ p (* s ta)) (swizzle hole-a :xyz)))
         (in-a (and (> fa 0.001) (> ta 0.0)
                    (< (dot da da) (* (swizzle hole-a :w) (swizzle hole-a :w)))))
         (fb (dot s (swizzle axis-b :xyz)))
         (tb (/ (dot (- (swizzle hole-b :xyz) p) (swizzle axis-b :xyz)) (max fb 0.001)))
         (db (- (+ p (* s tb)) (swizzle hole-b :xyz)))
         (in-b (and (> fb 0.001) (> tb 0.0)
                    (< (dot db db) (* (swizzle hole-b :w) (swizzle hole-b :w))))))
    (if (or in-window in-a in-b) 1.0 0.0)))

;; The cab's meshes and instances: positions in body axes from the eye, the
;; clip rows of the cab block (the world camera's projection in body axes, so
;; depth agrees with the scene's). SURFACE: material, two parameters.
;; MATERIALS: two vec4s per row of *cab-materials* (r g b specular weight;
;; exponent emission).
(defmacro define-cab-shader (name (&rest options) &body body)
  `(define-shader ,name
       (,@options
        :resources ((cab :uniform-block :binding 0 :members #.*cab-block-members*)
                    (materials :storage-buffer :binding 1 :element :vec4)))
     ,@body))

(define-shader-function cab-clip (p rx ry rz rw)
  (let* ((point (vec4 p 1.0)))
    (vec4 (dot rx point) (dot ry point) (dot rz point) (dot rw point))))

(define-cab-shader cab-mesh-vertex
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (normal :vec3 :location 1)
              (params :vec3 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (body :vec3 :location 0)
               (body-normal :vec3 :location 1)
               (surface :vec3 :location 2)))
  (let* ((p position))
    (set-output clip-position (cab-clip p clip-x clip-y clip-z clip-w))
    (set-output body p)
    (set-output body-normal normal)
    (set-output surface params)))

(define-cab-shader cab-instanced-vertex
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (normal :vec3 :location 1)
              (params :vec3 :location 2)
              (origin :vec3 :location 3)
              (axis-x :vec3 :location 4)
              (axis-y :vec3 :location 5)
              (axis-z :vec3 :location 6)
              (scale :vec4 :location 7))
     :outputs ((clip-position :vec4 :built-in :position)
               (body :vec3 :location 0)
               (body-normal :vec3 :location 1)
               (surface :vec3 :location 2)))
  (let* ((s (swizzle scale :xyz))
         (local (* position s))
         (p (+ origin (* axis-x (swizzle local :x)) (* axis-y (swizzle local :y))
               (* axis-z (swizzle local :z))))
         (m (/ normal s))
         (n (normalize (+ (* axis-x (swizzle m :x)) (* axis-y (swizzle m :y))
                          (* axis-z (swizzle m :z))))))
    (set-output clip-position (cab-clip p clip-x clip-y clip-z clip-w))
    (set-output body p)
    (set-output body-normal n)
    (set-output surface (vec3 (swizzle scale :w) (* (swizzle params :y) (swizzle s :x))
                              (* (swizzle params :z) (swizzle s :x))))))

;; The rows the fragment names, in row order: the test checks them against
;; *cab-material-names*.
(defparameter *cab-shader-material-rows*
  '(:lining :steel-angle :tread :eclss :red :tape :marker :black-plastic :grey-plastic
    :green-surplus :beige :aluminium :screen-face :cable-black :cable-grey :cable-red
    :cable-yellow :kapton :cardboard :sheet-metal :led :fabric :seat :hatch :plate
    :channel-x :channel-y :channel-z))

;; One fragment for both pipelines. The lining (row 0) is cut where the window
;; and the portholes open it; the tread plate (row 2) gets its diamonds from
;; the position (2.5 cm along x, 5 cm along y, factor 0.72 between them).
;; Light: the strips and the ambient at the fragment (cab-irradiance), the
;; normal turned to the eye; the material's emission added.
(define-cab-shader cab-fragment
    (:stage :fragment
     :inputs ((body :vec3 :location 0)
              (body-normal :vec3 :location 1)
              (surface :vec3 :location 2))
     :outputs ((color :vec4 :location 0)))
  (let* ((id (uint (+ (swizzle surface :x) 0.5)))
         (cut (if (= id (uint 0.0))
                  (cab-opening-cut body window hole-a hole-a-axis hole-b hole-b-axis)
                  0.0))
         (m0 (buffer-element materials (* id (uint 2.0))))
         (m1 (buffer-element materials (+ (* id (uint 2.0)) (uint 1.0))))
         (diamond (if (= id (uint 2.0))
                      (if (< (+ (abs (- (fract (* (swizzle body :x) 40.0)) 0.5))
                                (abs (- (fract (* (swizzle body :y) 20.0)) 0.5)))
                             0.38)
                          1.0 0.72)
                      1.0))
         (idf (swizzle surface :x))
         (slot-coord (if (< idf 25.5) (swizzle body :x) (if (< idf 26.5) (swizzle body :y) (swizzle body :z))))
         (slotted (if (> idf 24.5)
                      (if (< (fract (* slot-coord 40.0)) 0.35) 0.35 1.0)
                      1.0))
         (flat (normalize body-normal))
         (n (* flat (if (> (dot flat body) 0.0) -1.0 1.0)))
         (paint (* (swizzle m0 :xyz) (* diamond slotted)))
         (s (swizzle sun :xyz))
         (patch (* (sun-patch body s window hole-a hole-a-axis hole-b hole-b-axis)
                   (* (max (dot n s) 0.0) (swizzle sun :w))))
         (lit (+ (cab-irradiance body n strip-a strip-b light (swizzle params :z))
                 (vec3 patch patch patch))))
    (when (> cut 0.5)
      (discard))
    (set-output color (vec4 (+ (* paint lit) (* (swizzle m0 :xyz) (swizzle m1 :y))) 1.0))))

;; The glass of the window and the portholes (src/cockpit.lisp
;; cab-glass-colour): SURFACE x is the kind (0 window, 1 porthole), y and z the
;; position across it from -1 to 1. Premultiplied: alpha = 1 - transmission
;; (0.95 at the centre to 0.88 at the edge), the colour the reflection of the
;; cab light, tinted a faint green.
(define-cab-shader cab-glass-fragment
    (:stage :fragment
     :inputs ((body :vec3 :location 0)
              (body-normal :vec3 :location 1)
              (surface :vec3 :location 2))
     :outputs ((color :vec4 :location 0)))
  (let* ((u (swizzle surface :y))
         (v (swizzle surface :z))
         (edge (min 1.0 (if (< (swizzle surface :x) 0.5)
                            (max (abs u) (abs v))
                            (sqrt (+ (* u u) (* v v))))))
         (alpha (+ 0.05 (* 0.07 edge)))
         (flat (normalize body-normal))
         (n (* flat (if (> (dot flat body) 0.0) -1.0 1.0)))
         (lit (cab-irradiance body n strip-a strip-b light (swizzle params :z))))
    (set-output color (vec4 (* (* alpha 0.5) (* lit (vec3 0.85 1.0 0.9))) alpha))))

;;; Slug text on planes (src/text.lisp; decision 126: Cockpit structure and
;;; view in milestone 3), after Luvcraft's :slug-world-text vertex stage
;;; (luvcraft/shaders.lisp, luv by Mikael Brockman): the glyph quad grows past
;;; its outline by a fixed number of pixels per corner, so Slug's pixel
;;; filter is always inside it; positions go through the clip rows of the
;;; block the caller fills (the frame the records are in), reversed-Z, w the
;;; depth. LOOK: kind (0 emissive, 1 lit), factor. The ink in the record's
;;; spare lanes is the colour: emissive ink x factor / exposure, lit ink x
;;; (the block's constant factor + the cab's irradiance at the glyph's origin
;;; on its plane, turned to the eye). The fragment stage is luv's.
(define-shader cab-text-vertex
    (:stage :vertex
     :inputs ((corner :vec3 :location 0)
              (origin :vec3 :location 1)
              (right-edge :vec3 :location 2)
              (up-edge :vec3 :location 3)
              (outline-low :vec3 :location 4)
              (outline-high :vec3 :location 5)
              (atlas-input :vec3 :location 6)
              (band-low :vec3 :location 7)
              (band-high :vec3 :location 8)
              (look :vec3 :location 9))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-atlas-base :vec2 :location 1 :interpolation :flat)
               (render-band-bounds :vec4 :location 2 :interpolation :flat)
               (render-band-counts :vec2 :location 3 :interpolation :flat)
               (render-color :vec4 :location 4 :interpolation :flat))
     :resources ((text-block :uniform-block :binding 2
                  :members #.*cab-block-members*)))
  (let* ((c (swizzle corner :xy))
         (h0 (vec4 (+ origin (* right-edge (swizzle c :x)) (* up-edge (swizzle c :y))) 1.0))
         (w0 (max (dot clip-w h0) 0.001))
         (nx (/ (dot clip-x h0) w0))
         (ny (/ (dot clip-y h0) w0))
         (re (vec4 right-edge 0.0))
         (ue (vec4 up-edge 0.0))
         (rx (/ (- (dot clip-x re) (* nx (dot clip-w re))) (* w0 (swizzle screen :x))))
         (ry (/ (- (dot clip-y re) (* ny (dot clip-w re))) (* w0 (swizzle screen :y))))
         (ux (/ (- (dot clip-x ue) (* nx (dot clip-w ue))) (* w0 (swizzle screen :x))))
         (uy (/ (- (dot clip-y ue) (* ny (dot clip-w ue))) (* w0 (swizzle screen :y))))
         (right-pixels (sqrt (+ (* rx rx) (* ry ry))))
         (up-pixels (sqrt (+ (* ux ux) (* uy uy))))
         (dilation (* luv.slug:slug-dilation-pixels luv.slug:slug-filter-width))
         (dc (+ c (* (- (* c 2.0) (vec2 1.0 1.0))
                     (vec2 (/ dilation (max right-pixels 0.001))
                           (/ dilation (max up-pixels 0.001))))))
         (h (vec4 (+ origin (* right-edge (swizzle dc :x)) (* up-edge (swizzle dc :y))) 1.0))
         (ink (vec3 (swizzle atlas-input :z) (swizzle band-low :z) (swizzle band-high :z)))
         (plane (vec3 (- (* (swizzle right-edge :y) (swizzle up-edge :z))
                         (* (swizzle right-edge :z) (swizzle up-edge :y)))
                      (- (* (swizzle right-edge :z) (swizzle up-edge :x))
                         (* (swizzle right-edge :x) (swizzle up-edge :z)))
                      (- (* (swizzle right-edge :x) (swizzle up-edge :y))
                         (* (swizzle right-edge :y) (swizzle up-edge :x)))))
         (flat (normalize (+ plane (vec3 0.0 0.0 0.000001))))
         (n (* flat (if (> (dot flat origin) 0.0) -1.0 1.0)))
         (lit (cab-irradiance origin n strip-a strip-b light (swizzle params :z)))
         (shade (if (< (swizzle look :x) 0.5)
                    (* ink (* (swizzle look :y) (swizzle params :x)))
                    (* ink (+ lit (vec3 (swizzle params :y) (swizzle params :y) (swizzle params :y)))))))
    (set-output clip-position (vec4 (dot clip-x h) (dot clip-y h) (dot clip-z h) (dot clip-w h)))
    (set-output render-coordinate
                (+ (swizzle outline-low :xy)
                   (* (- (swizzle outline-high :xy) (swizzle outline-low :xy)) dc)))
    (set-output render-atlas-base (swizzle atlas-input :xy))
    (set-output render-band-bounds (vec4 (swizzle band-low :xy) (swizzle band-high :xy)))
    (set-output render-band-counts (vec2 (swizzle outline-low :z) (swizzle outline-high :z)))
    (set-output render-color (vec4 shade 1.0))))

;; luv's slug-atlas-fragment-specification with the atlas address and the
;; band counts flat: smooth interpolation of the large integer-valued floats
;; drifts by an ulp across a quad, so uint() floored them one too low per
;; pixel (horizontal gaps in the strokes, task 281). Else as luv's.
(define-shader cab-text-fragment
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (atlas-base :vec2 :location 1 :interpolation :flat)
              (band-bounds :vec4 :location 2 :interpolation :flat)
              (band-counts :vec2 :location 3 :interpolation :flat)
              (render-color :vec4 :location 4 :interpolation :flat))
     :resources ((band-data :uint-texture-2d :binding 0)
                 (curve-data :texture-2d :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((one (luv.shader:uint 1.0))
         (width (luv.shader:uint 4096.0))
         (band-base (luv.shader:uint (+ (swizzle atlas-base :x) 0.5)))
         (curve-base (luv.shader:uint (+ (swizzle atlas-base :y) 0.5)))
         (horizontal-band-count (luv.shader:uint (+ (swizzle band-counts :x) 0.5)))
         (vertical-band-count (luv.shader:uint (+ (swizzle band-counts :y) 0.5)))
         (pixels-per-em (luv.slug::slug-pixels-per-em render-coordinate))
         (horizontal-position
           (clamp (/ (- (swizzle render-coordinate :y) (swizzle band-bounds :y))
                     (max (- (swizzle band-bounds :w) (swizzle band-bounds :y))
                          luv.slug:slug-root-epsilon))
                  0.0 1.0))
         (vertical-position
           (clamp (/ (- (swizzle render-coordinate :x) (swizzle band-bounds :x))
                     (max (- (swizzle band-bounds :z) (swizzle band-bounds :x))
                          luv.slug:slug-root-epsilon))
                  0.0 1.0))
         (horizontal-band-candidate
           (luv.shader:uint (* horizontal-position (luv.shader:float horizontal-band-count))))
         (vertical-band-candidate
           (luv.shader:uint (* vertical-position (luv.shader:float vertical-band-count))))
         (horizontal-band
           (if (< horizontal-band-candidate horizontal-band-count)
               horizontal-band-candidate
               (- horizontal-band-count one)))
         (vertical-band
           (if (< vertical-band-candidate vertical-band-count)
               vertical-band-candidate
               (- vertical-band-count one)))
         (horizontal-header-address (+ band-base horizontal-band))
         (vertical-header-address (+ band-base horizontal-band-count vertical-band))
         (horizontal-header-location
           (luv.shader:uvec2 (mod horizontal-header-address width)
                             (/ horizontal-header-address width)))
         (vertical-header-location
           (luv.shader:uvec2 (mod vertical-header-address width)
                             (/ vertical-header-address width)))
         (horizontal-header (texel-load band-data horizontal-header-location))
         (vertical-header (texel-load band-data vertical-header-location))
         (horizontal-count (swizzle horizontal-header :x))
         (horizontal-offset (swizzle horizontal-header :y))
         (vertical-count (swizzle vertical-header :x))
         (vertical-offset (swizzle vertical-header :y))
         (horizontal
           (counted-fold (index horizontal-count state (vec3 0.0 0.0 0.0)
                          :until (luv.slug::slug-band-done-p state))
             (let* ((entry-address (+ band-base horizontal-offset index))
                    (entry-location (luv.shader:uvec2 (mod entry-address width)
                                                      (/ entry-address width)))
                    (local-location (swizzle (texel-load band-data entry-location) :xy))
                    (curve-address (+ curve-base (swizzle local-location :x)
                                      (* (swizzle local-location :y) width)))
                    (curve-location (luv.shader:uvec2 (mod curve-address width)
                                                      (/ curve-address width)))
                    (next-location (luv.shader:uvec2 (mod (+ curve-address one) width)
                                                     (/ (+ curve-address one) width)))
                    (curve (texel-load curve-data curve-location))
                    (next (texel-load curve-data next-location)))
               (luv.slug::slug-horizontal-band-step state curve next render-coordinate
                                                    pixels-per-em))))
         (vertical
           (counted-fold (index vertical-count state (vec3 0.0 0.0 0.0)
                          :until (luv.slug::slug-band-done-p state))
             (let* ((entry-address (+ band-base vertical-offset index))
                    (entry-location (luv.shader:uvec2 (mod entry-address width)
                                                      (/ entry-address width)))
                    (local-location (swizzle (texel-load band-data entry-location) :xy))
                    (curve-address (+ curve-base (swizzle local-location :x)
                                      (* (swizzle local-location :y) width)))
                    (curve-location (luv.shader:uvec2 (mod curve-address width)
                                                      (/ curve-address width)))
                    (next-location (luv.shader:uvec2 (mod (+ curve-address one) width)
                                                     (/ (+ curve-address one) width)))
                    (curve (texel-load curve-data curve-location))
                    (next (texel-load curve-data next-location)))
               (luv.slug::slug-vertical-band-step state curve next render-coordinate
                                                  pixels-per-em))))
         (coverage (luv.slug::slug-combine-band-coverage
                    (swizzle horizontal :x) (swizzle horizontal :y)
                    (swizzle vertical :x) (swizzle vertical :y))))
    (set-output color-output (* render-color coverage))))

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

(define-frame-shader quads-fragment
    (:stage :fragment
     :inputs ((shade :vec3 :location 0))
     :outputs ((color :vec4 :location 0)))
  (set-output color (vec4 shade 1.0)))

(define-shader-function srgb-encode (x)
  (if (<= x 0.0031308) (* x 12.92) (- (* 1.055 (expt x 0.41666666)) 0.055)))

;; SHADE is linear, sRGB-encoded unless the target's format does it, so a
;; colour given in sRGB lands exactly.
(define-frame-shader overlay-fragment
    (:stage :fragment
     :inputs ((shade :vec3 :location 0))
     :outputs ((color :vec4 :location 0)))
  (set-output color
              (if (> (swizzle post :z) 0.5)
                  (vec4 (srgb-encode (swizzle shade :x)) (srgb-encode (swizzle shade :y))
                        (srgb-encode (swizzle shade :z)) 1.0)
                  (vec4 shade 1.0))))

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
(defmethod shader-specification-for ((role (eql :ship-mesh)) (stage (eql :vertex)))
  (ship-mesh-vertex))
(defmethod shader-specification-for ((role (eql :ship-mesh)) (stage (eql :fragment)))
  (ship-fragment))
(defmethod shader-specification-for ((role (eql :ship-instanced)) (stage (eql :vertex)))
  (ship-instanced-vertex))
(defmethod shader-specification-for ((role (eql :ship-instanced)) (stage (eql :fragment)))
  (ship-fragment))
(defmethod shader-specification-for ((role (eql :ship-glow)) (stage (eql :vertex)))
  (ship-glow-vertex))
(defmethod shader-specification-for ((role (eql :ship-glow)) (stage (eql :fragment)))
  (ship-glow-fragment))
(defmethod shader-specification-for ((role (eql :ribbon)) (stage (eql :vertex)))
  (ribbon-vertex))
(defmethod shader-specification-for ((role (eql :ribbon)) (stage (eql :fragment)))
  (ribbon-fragment))
(defmethod shader-specification-for ((role (eql :cab-text)) (stage (eql :vertex)))
  (cab-text-vertex))
(defmethod shader-specification-for ((role (eql :cab-text)) (stage (eql :fragment)))
  (cab-text-fragment))
(defmethod shader-specification-for ((role (eql :cab-glass)) (stage (eql :vertex)))
  (cab-mesh-vertex))
(defmethod shader-specification-for ((role (eql :cab-glass)) (stage (eql :fragment)))
  (cab-glass-fragment))
(defmethod shader-specification-for ((role (eql :cab-mesh)) (stage (eql :vertex)))
  (cab-mesh-vertex))
(defmethod shader-specification-for ((role (eql :cab-mesh)) (stage (eql :fragment)))
  (cab-fragment))
(defmethod shader-specification-for ((role (eql :cab-instanced)) (stage (eql :vertex)))
  (cab-instanced-vertex))
(defmethod shader-specification-for ((role (eql :cab-instanced)) (stage (eql :fragment)))
  (cab-fragment))
(defmethod shader-specification-for ((role (eql :quads)) (stage (eql :vertex)))
  (overlay-vertex))
(defmethod shader-specification-for ((role (eql :quads)) (stage (eql :fragment)))
  (quads-fragment))
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
