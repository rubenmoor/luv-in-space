# Architecture

This document is the technical ground of Luv in Space: how the game is built on [luv](https://github.com/mbrock/luv), how its source is laid out, and the technical choices (units, frames, integration, testing) every later milestone implements from. It rests on `docs/luv-capabilities.md`, which records what luv offers and lacks, and on the walking skeleton in `src/`, `tests/`, `scripts/` and `flake.nix`. Where a choice depends on the game design, it gives the architecture's default and the constraint it puts on `docs/design.md`. Commands to build, run, test and screenshot are in `AGENTS.md` "Commands" and not repeated here; lasting choices are pm decisions, cited by id and subject.

## Dependency on luv

The game is its own renderer and simulation on luv's HAL, shader language, SDL3 canvas and capture path. luv comes in as a pinned flake input; nothing in `../luv` is changed or loaded from there, and what luv lacks is written here and listed in `docs/luv-wishlist.md`.

### Flake input and package
- Input `github:mbrock/luv` pinned to `6abc9d10ec8991c2cfe92888b84946372d6030b9` in `flake.nix` and `flake.lock`; `nixpkgs` follows luv's so luv's binary cache matches (`decision 1: How the game consumes luv`).
- Dev shell, package and check all come from `luv.lib.environmentFor <system>`: the dev shell from `developmentEnvironment`, `developmentPackages` and `developmentEnvironmentHook`, plus `pm` and `git`; the check from the `slim…` variants.
- luv's Lisp sources: the input's store path on `CL_SOURCE_REGISTRY` (on Darwin a writable copy, since luv writes its Objective-C bridge next to its sources; `docs/luv-wishlist.md`).
- Package: `applicationPackage` in `flake.nix` is a copy of luv's unexported recipe, adapted: source is `gameSource` (`luv-in-space.asd`, `src/`, `scripts/build.lisp`), `sbcl --script scripts/build.lisp` dumps `luv-in-space/program`, the FASL cache ships under `lib/fasl/`, no fonts or Luvcraft extras.
- Launcher (`bin/luv-in-space`), scripts and the check unset `LD_LIBRARY_PATH`: a host one breaks luv's SDL3 (`docs/luv-capabilities.md` "Packaging").
- Binary cache `luv.swa.sh` in `nixConfig`; it serves the dependency closure, so only our Lisp image builds locally.
- Platforms: `x86_64-linux`, `aarch64-linux`, `aarch64-darwin`; only the first is tested (`decision 6: Target platforms`).
- Moving the pin: a task of its own that bumps `flake.lock`, reruns `scripts/test`, `scripts/screenshot` and `nix flake check`, and updates the revision in `docs/luv-capabilities.md`.

### ASDF systems
- `luv-in-space` depends on `"luv"` only; it brings the HAL, canvas, `luv/shader` and its lowerings, `luv/arithmetic`, Slug text, captures, live artifacts and `audio-sink` (`decision 3: luv ASDF systems the game depends on`).
- `luv-in-space/tests` adds `"luv/test-support"` and `"parachute"`; `luv-in-space/program` builds the executable (`decision 9: Layout of systems and directories`).
- Nothing from `luvcraft/*` or `luft/*`: their ideas (sky, bloom, text runs, saves) are copied and adapted, since loading them pulls in the block game.
- A new luv system becomes a dependency only through a recorded decision (`pm decision record`, superseding or extending `decision 3`), then the `:depends-on` line, then a note in `docs/luv-capabilities.md` "ASDF systems".
- Known candidates, not yet dependencies:
  - `luv/production`: a latest-value worker for trajectory prediction off the frame thread ("Time and integration" decides).
  - `luv/arithmetic` quantity declarations at boundaries (save files, instruments); it is already loaded through `luv`, so using it needs no new dependency, only a rule; the integrator stays on plain `double-float`.
  - `luv/ghostty`: only if `docs/design.md` keeps a real terminal screen; needs the full dev shell, not `slim`.

### What the game uses from luv and what it writes
- From luv:
  - HAL: devices, buffers, textures, samplers, bind groups, render pipelines, encoders, `draw`/`draw-indexed` with instancing ("Rendering").
  - Shader language: `define-shader` in `luv-in-space.shaders`; every pipeline a luv live shader pipeline (`make-live-shader-pipeline`, a role keyword, `shader-specification-for` methods), returned by `luv:application-live-artifacts` and refreshed at the top of each frame (`decision 11: Render pipelines are luv live shader pipelines`).
  - SDL3 canvas: windows, cadence clock, key and pointer events, relative pointer mode ("Windowing and input").
  - `capture-application-screenshot` for screenshots and the GPU test (`decision 2: Screenshots and GPU test: offscreen capture on a hidden canvas, headless on lavapipe`).
  - `sdl3-image` (loaded by `luv`) for decoding textures ("Rendering", image loading).
  - `audio-sink` for sound ("Audio").
  - Slug glyph rendering and HarfBuzz shaping for instrument text ("HUD and instruments").
- Written here, each after its section in `docs/luv-capabilities.md` "Gaps and workarounds":
  - Camera and projection: reversed-Z, infinite far plane, clip-y flip in one function ("Depth range across 10^12 m"; `decision 4: Depth strategy`).
  - Camera-relative conversion from `double-float` world state ("Double-precision positions"; `decision 5: Precision of positions`).
  - Sphere impostors for the Sun, planets and moons, Blue Marble Earth ("Spheres and planet surfaces").
  - Stars from a catalogue as instanced quads at infinity ("Starfield").
  - HDR target, bloom, tone map, exposure ("Sun glare and brightness").
  - Screen-space ribbons and markers ("Orbit lines and markers").
  - Text runs (a copy of Luvcraft's `text.lisp`) and cockpit screen textures ("HUD and instruments"); dot-matrix placeholder labels until then (`decision 7: Placeholder labels: dot-matrix overlay quads`).
  - Instanced meshes for ships and asteroids ("Instancing many objects").
  - Physics: clock, warp, integrator, Kepler propagation, ephemerides ("Fixed physics step and time warp"; "Physics and SIMD").
  - Audio mixer and synthesised voices ("Audio"); saves ("Save files"); gamepad polling ("Gamepad and joystick").

### Dev loop
- Commands: `AGENTS.md` "Commands" (`./sly`, `scripts/test`, `scripts/screenshot`, `nix build`, `nix run`, `nix flake check`); `decision 8: Live image: ./sly wraps luv's client from the flake input` and `decision 10: Test and screenshot commands`.
- Inner loop: `./sly start`, `./sly play`, then `./sly eval '(defun …)' --package LUV-IN-SPACE` or a re-evaluated `define-frame-shader` in `LUV-IN-SPACE.SHADERS`; `./sly screenshot` to look; `./sly status` and `./sly failures` after each change; `./sly resume` after a parked frame.
- Before a commit: `scripts/test` (and `scripts/screenshot` when the picture changed); `nix build` when `flake.nix`, the `.asd` or the build script changed; `nix flake check` when the test setup changed.
- Live-redefinable code, the rules:
  - All mutable state lives in the `game` object (`*game*`): simulation state, controls, render resources; no other mutable globals except caches.
  - `defparameter` for tunables (re-read on every call, so `./sly eval '(defparameter …)'` takes effect), `defvar` only for `*game*` and caches.
  - Callbacks handed to luv (clock, event handler) are lambdas that call named functions (`(draw game)`, `(handle-event game event)`); never pass `#'foo` or store a function object in a slot the loop calls, since a redefinition would not reach it.
  - New slots: add them to `defclass game` with an `:initform`; CLOS updates the live instance.
  - Render resources are made by functions that the frame can call again (cached by size, as `depth-view`), so a changed layout means a restart of `play`, not of the image.
  - Shaders only through `define-shader` and live pipelines (`decision 11`); a broken one keeps the last good build and `./sly status` names it.
  - Pure functions (math, physics, meshes) take and return values, so tests and `./sly eval` call them without a window.

### luv wishlist
- A gap met while building goes first into `docs/luv-capabilities.md` (the finding, how it was checked), then the workaround here, then a line in `docs/luv-wishlist.md`: the item, the finding it rests on, why it would serve other luv programs, and the workaround here.
- When luv offers the item at a pinned revision, a task switches over and removes the workaround and the wishlist line.

## Module layout

The game is one ASDF system with flat files in `src/`, loaded serially in the order below; files group by subject, not by package (`decision 12: Module layout and package split`). The skeleton's `package`, `shaders`, `main` and `capture` stay; its `scene.lisp` is the skeleton's renderer and is replaced by the rendering files in `milestone 2` (`decision 9: Layout of systems and directories`).

### Packages
- Two packages, as now: `luv-in-space` for all game code and `luv-in-space.shaders` (uses `luv.shader`) for every `define-shader`; tests in `luv-in-space.tests`.
- Why one game package: `./sly eval … --package LUV-IN-SPACE` reaches every function without package prefixes or export lists to keep in step while redefining live; the codebase stays small enough (a few thousand lines) that name clashes are handled by prefixes (`orbit-…`, `ship-…`).
- Why shaders apart: `luv.shader` defines operators that clash with `cl` (`step`, maths); shader code reads as shader code.
- Split when needed: a file whose names clash or that grows past ~1,500 lines may get its own package; that supersedes `decision 12`.

### Files in load order
In brackets: the milestone that first creates the file; later milestones extend it.

- `package.lisp` (skeleton): the packages.
- `math.lisp` (milestone 2): `double-float` vec3 and quaternion on the CPU, specialised arrays; conversion to `single-float` arrays at the GPU boundary; needs package.
- `units.lisp` (milestone 2): SI constants: G·M and radii of bodies, epoch, conversions for display (km, km/s) with their source; needs math.
- `bodies.lisp` (milestone 2: Sun, Earth, Moon; 4: planets): bodies (Sun, Earth, Moon, planets): μ, radius, rotation, ephemeris function of time; needs units.
- `orbits.lisp` (milestone 2): orbital elements ↔ state vectors, Kepler propagation, apsides, periods; needs math, bodies.
- `physics.lisp` (milestone 2): state vectors (position, velocity, attitude, angular velocity), gravity, the integrator with forces and torques as arguments, frames of integration; needs orbits.
- `ship.lisp` (milestone 2): ship definition: frame, modules, mass, thrusters, propellant, rigid-body attitude; needs physics.
- `controls.lisp` (milestone 2): the control state (throttle, RCS, attitude commands) that input, autopilot and test scripts all write; needs ship.
- `clock.lisp` (milestone 2): simulation clock, fixed steps, time warp levels and their rules, rails switching; needs physics, controls.
- `hazards.lisp` (milestone 2: crash; 4: Sun): crash detection against bodies, the Sun's heat, death; needs physics, bodies.
- `prediction.lisp` (milestone 3): trajectory prediction for orbit lines, instruments and the autopilot; needs orbits, clock.
- `autopilot.lisp` (milestone 3): orbit targeting, burns, presets; writes `controls`; needs prediction, controls.
- `places.lisp` (milestone 3: Earth orbit; 4: Moon, belt): points of interest in Earth orbit, asteroids, destinations of presets; needs bodies, orbits.
- `quests.lisp` (milestone 5): the quest line: state, triggers, messages; needs places, autopilot.
- `saves.lisp` (milestone 5): versioned s-expression save files, atomic writes, under `$XDG_DATA_HOME/luv-in-space/`; needs quests, ship, clock.
- `shaders.lisp` (skeleton): all `define-shader`s and `shader-specification-for` methods (package `luv-in-space.shaders`); split into `shaders-<pass>.lisp` after it when it grows; needs package.
- `assets.lisp` (milestone 2): locating and loading assets: textures through `sdl3-image`, the star table; needs package.
- `gpu.lisp` (milestone 2): HAL helpers: buffers, live pipelines, views and size-cached targets (from the skeleton's `scene.lisp`); needs shaders.
- `camera.lisp` (milestone 2): camera state, the one projection function (reversed-Z, infinite far, clip-y flip), camera-relative conversion; needs math, gpu.
- `stars.lisp` (milestone 2): star table to instanced quads at infinity; needs assets, camera.
- `impostors.lisp` (milestone 2): sphere impostors for the Sun, planets and moons; Earth's texture; needs bodies, assets, camera.
- `meshes.lisp` (milestone 2: ship; 4: asteroids): generated meshes (ship frame, modules, asteroids) and their instanced draws; needs ship, camera.
- `ribbons.lisp` (milestone 2: the current orbit; 3: predicted trajectories): screen-space ribbons and markers: orbit lines, trajectories, targets; needs orbits, camera, later prediction.
- `post.lisp` (milestone 2): HDR target, bloom, tone map, exposure; needs gpu.
- `overlay.lisp` (milestone 2): screen-space 2D pass: dot-matrix labels (`decision 7`) and the minimal flight readout; needs gpu.
- `text.lisp` (milestone 3): Slug text runs (adapted copy of Luvcraft's `text.lisp`, attributed in its header); needs gpu.
- `cockpit.lisp` (milestone 3): the DIY cockpit: geometry, screens rendered to textures, switches, instruments; needs text, ship, prediction.
- `render.lisp` (milestone 2): the frame: passes in order, encoding everything above into the canvas or a capture; needs all rendering files.
- `input.lisp` (milestone 2): canvas events to `controls`; key map; gamepad polling later; needs controls.
- `audio.lisp` (milestone 6 by default; earlier if `docs/design.md` says so): `audio-sink`, the Lisp mixer, synthesised voices; silent when no device; needs ship, controls.
- `title.lisp` (milestone 6): title screen, introduction, in-game controls help; needs overlay, text.
- `main.lisp` (skeleton): the `game` object, start/stop, the frame function, `main`; needs everything.
- `capture.lisp` (skeleton): screenshots and `capture-scene`; needs main.

- The simulation files (`math` to `saves`) never call the HAL or SDL, so tests load and run them without a canvas; rendering files read simulation state and never change it.
- `scene.lisp` stays until `render.lisp` and its parts replace it; `PLACEHOLDER(23)` and `tests/scene.lisp` go with it (its checks move to `tests/render.lisp`).
- Tests mirror modules: `tests/<module>.lisp` for each file with logic (`tests/orbits.lisp`, `tests/physics.lisp`, `tests/clock.lisp`, …), listed in `luv-in-space/tests` in the same order; `tests/package.lisp` and `tests/capture.lisp` stay; GPU tests only in `tests/render.lisp` and `tests/capture.lisp`.
- A file named above may be merged into a neighbour if it stays under ~100 lines; a new file is added to this list by the task that creates it.

### Assets
- Recorded as `decision 13: Asset storage and attribution`.
- `assets/` at the repo root holds runtime data, committed to git; one subdirectory per kind with its own `SOURCE.md`: `assets/earth/`, `assets/stars/`, later `assets/moon/`, `assets/planets/`.
- Size rule: a committed file stays ≤ 5 MB and `assets/` ≤ 25 MB in total; anything larger (e.g. the 21600×10800 Blue Marble, ~2 km per pixel) comes from a Nix `fetchurl` with a pinned hash into a separate derivation, never committed, and the game falls back to the committed version when it is absent.
- Earth: the Blue Marble Next Generation 5400×2700 JPEG (1.8 MB, `docs/luv-capabilities.md` "Spheres and planet surfaces") committed as `assets/earth/blue-marble-5400.jpg`; decoded at start through `sdl3-image`.
- Stars: HYG filtered to magnitude ≤ 6.5 (~8,900 stars), as a derived table `assets/stars/hyg-6.5.sexp` (right ascension, declination, visual magnitude, colour index; a few hundred KB). A converter `scripts/convert-stars` downloads the HYG CSV to a temporary directory, filters it and writes the table; the full CSV is not committed.
- Attribution and licences:
  - `assets/<kind>/SOURCE.md`: source URL, version or date fetched, licence, required credit, how the file was derived (command).
  - Earth: credit "NASA Earth Observatory (Blue Marble Next Generation)"; no NASA logo, no implied endorsement (NASA media guidelines, quoted in `docs/luv-capabilities.md`).
  - Stars: `assets/stars/LICENSE` with CC BY-SA 4.0; the derived table is under CC BY-SA 4.0 with credit to the HYG database (astronexus), stated in its `SOURCE.md` and in the table's header comment; the game's code is not affected by ShareAlike.
  - `CREDITS.md` at the root collects every credit, and the game shows it (title or credits screen, `milestone 6: Release: one command to play, a README for people`); `README.md` links it.
- Packaging: `gameSource` and `testSource` in `flake.nix` include `assets/`; the package copies `assets/` to `$out/share/luv-in-space/assets` (the build's copy of the tree is deleted after the dump) and the launcher exports `LUV_IN_SPACE_ASSETS` to it; `assets.lisp` reads that variable and falls back to `(asdf:system-relative-pathname "luv-in-space" "assets/")` in the dev shell and the live image.
- The first task that adds an asset (`milestone 2`) does the packaging change and checks it with `nix build` and `nix run`.

## Units and coordinates

Every quantity in the simulation is SI in `double-float`; time is TDB seconds since J2000; the ship lives in an inertial frame centred on its dominant body, and all inertial frames share the same axes. The GPU sees only camera-relative single floats, projected by one function with reversed-Z and an infinite far plane. These rules rest on `decision 4: Depth strategy`, `decision 5: Precision of positions`, `decision 14: Units and time epoch` and `decision 15: Coordinate frames`; the code lives in `math.lisp`, `units.lisp`, `bodies.lisp` and `camera.lisp` (`decision 12: Module layout and package split`).

### Units
- SI throughout: m, kg, s, N, m/s, rad; `double-float` on the CPU, no unit suffixes in names (`decision 14: Units and time epoch`). Display units (km, km/s, degrees, minutes, UTC dates) exist only in instrument and label code, through conversion functions in `units.lisp`.
- Why not km or AU: a double has the same 53-bit relative precision at every scale, so another unit only moves the exponent and adds conversions. Resolution (one ulp) is ~1e-9 m at LEO radius (6.8×10^6 m), ~6e-8 m at the Moon (3.8×10^8 m), ~3e-5 m at 1 AU (1.5×10^11 m), ~1e-3 m at Neptune (4.5×10^12 m): ample everywhere. SI also keeps rocket formulas plain (thrust N, mass flow kg/s, exhaust velocity m/s).
- Gravity as μ = GM in m³/s², never G and M apart (G is known to ~1e-5, μ to ~1e-10).
- Sources, cited in `units.lisp` next to each constant:
  - μ: JPL DE440 (Park et al. 2021, "The JPL Planetary and Lunar Ephemerides DE440 and DE441", AJ 161:105), the ephemeris behind JPL Horizons, so spot checks agree: Sun 1.32712440041279419×10^20, Earth 3.98600435507×10^14, Moon 4.902800118×10^12 m³/s²; planets (milestone 4) from the same table (planet-system values). Verify each digit against the paper when `units.lisp` is written.
  - Radii: IAU 2015 Resolution B3 for the Sun (6.957×10^8 m); IAU WGCCRE 2015 (Archinal et al. 2018) for Earth (equatorial 6,378,136.6 m), Moon (mean 1,737,400 m) and the planets.
  - Bodies are spheres: Earth at its equatorial radius, so the altitude shown matches published orbit altitudes near the equator and errs by at most 21 km at the poles; oblateness of the shape is cut (J2 in the gravity field is for "Time and integration").
- Angles in radians in code; degrees only on display. Angular rates in rad/s.
- `luv/arithmetic` quantity types are not used in the simulation (plain `double-float` is the unit discipline); at boundaries see "ASDF systems".

### Time
- Simulation time `t`: a `double-float`, TDB seconds since J2000 = 2000-01-01 12:00:00 TT (JD 2451545.0) (`decision 14`). Horizons, DE440 and the IAU rotation models use the same epoch, so their values go in without shifting.
- Resolution: one ulp of `t` is ~1.2e-7 s at 30 years after J2000 (9.5×10^8 s) and ~2.4e-7 s at 60 years: far below any step.
- Steps are power-of-two fractions of a second (e.g. 1/16 s) or whole seconds, so `t + dt` is exact and the clock neither drifts nor depends on the frame rate (determinism; the step itself is "Time and integration"'s).
- Time scales: TDB − TT (< 2 ms) is ignored; UT1 = UTC = TT − 69.184 s (37 leap seconds as of 2017; no future leap seconds). UTC is for display and Earth's rotation angle.
- Calendar: `units.lisp` converts a Gregorian UTC date to `t` (Julian date by the Fliegel–Van Flandern algorithm, then `(JD_TT − 2451545.0) × 86400`) and back for display.
- The game's start date: a calendar constant `*start-date*` in `units.lisp`, chosen by `docs/story.md`; default 2040-01-01 00:00 UTC until then. A new game sets `t` from it; a save stores `t` itself.
- No absolute time on the GPU: a float32 of `t` resolves only ~64 s. Shader animation gets phases computed in double and reduced (`mod` period) on the CPU.

### Frames
All inertial frames share one set of axes, ICRF (EME2000: J2000 mean equator and equinox; x toward the vernal equinox, z toward the celestial north pole), so moving between them is a translation and an attitude is the same quaternion in all of them (`decision 15: Coordinate frames`). Precession and nutation are ignored: Earth's pole stays at ICRF z, ~0.15° off the true pole in 2026 and ~0.2° by the 2040s.

- Heliocentric inertial frame: origin the Sun's centre. Positions of the planets (and, through Earth, the Moon) come from ephemerides in it ("Time and integration" chooses them); ephemerides given in ecliptic J2000 are rotated by the obliquity ε = 84381.448″ (23.4392911°) about x.
- Body-centred inertial frames (Earth-, Moon-, planet-centred): heliocentric minus the body's position and velocity. The frame of integration is one of them:
  - The ship is integrated in the frame of its dominant body: the innermost body whose Laplace sphere of influence, r = a (m/M)^(2/5) with `a` the body's semi-major axis about its primary, contains the ship; the Sun otherwise. Earth ~9.25×10^8 m, Moon ~6.6×10^7 m (in Earth's).
  - Why not heliocentric: there the ship's coordinates are ~1.5×10^11 m and resolve ~3e-5 m, so roundoff over 10^6 steps reaches metres and blurs the drift tests; Earth-centred, LEO resolves ~1e-9 m. Orbital elements and most instruments also want the body-centred state directly.
  - The frame accelerates with its body; the force model ("Time and integration") includes the frame's acceleration as the indirect term of each third body, so the trajectory does not depend on the frame chosen.
  - Re-centring: at a step boundary, never inside a step; when the ship leaves the current body's sphere by 2% (r > 1.02 r_SOI) or enters a child's sphere by 2% (r < 0.98 r_SOI), state ← state + (r, v) of the old origin − (r, v) of the new one, computed in double from the ephemerides at `t`. The hysteresis keeps a ship on the boundary from flipping each step. The camera's position is stored relative to the ship (or to a body), so it moves with the re-centring and the picture does not jump.
- Orbital elements and instruments: from the state in a body-centred frame. Earth orbits are reported against Earth's equator (= ICRF xy-plane, so no rotation); the Moon's and planets' orbiters against the body's equator (rotation from its IAU pole); heliocentric orbits against the ecliptic J2000 (rotation by ε). The elements' reference plane is a display choice; the state stays in ICRF axes.
- Local frames for instruments and the autopilot, built from the body-centred state each time they are needed:
  - Orbital frame: prograde (along v), normal (along r × v), radial out (completes the right-handed triple); burns and attitude holds are given in it.
  - Local horizon frame: north, east, down at the ship's position over the rotating body; heading, flight-path angle, ground speed.
- Body-fixed rotating frames: rotation models of IAU WGCCRE 2015 (Archinal et al. 2018): pole right ascension and declination and prime-meridian angle W(t); the leading terms only.
  - Earth: z = ICRF z (pole fixed, as above); x through the prime meridian on the equator, y at 90° E; rotation about z by the Earth Rotation Angle θ = 2π (0.7790572732640 + 1.00273781191135448 D_U), D_U = JD_UT1 − 2451545.0 (IERS Conventions 2010), reduced mod 2π in double. Earth-fixed = R_z(−θ) · (Earth-centred inertial).
  - Used for: latitude, longitude and ground track, the crash point, launch and landing sites if any, and the texture lookup.
  - Texture longitude: the Blue Marble equirectangular map runs from 180° W at the left edge through Greenwich in the centre column to 180° E, 90° N at the top. A direction (x, y, z) in Earth-fixed axes samples u = 0.5 + atan2(y, x)/(2π), v = 0.5 − asin(z)/π; so body-fixed x lands on the centre column.
  - Moon: its IAU model (pole, W(t)); its prime meridian faces mean Earth, so the near side stays toward Earth. Planets (milestone 4): their IAU models, or a fixed axis and period where only seen from afar.
- Ship body frame (FRD): x forward (the main engine's thrust direction), y right, z down; right-handed. Positive pitch raises the nose, positive yaw turns it right, positive roll lowers the right side. Thrusters, modules, the cockpit's geometry and the pilot's eye point are given in it (metres from the ship's centre of mass or a fixed reference point, the design's choice). Attitude: a unit quaternion `q`, body → inertial, v_inertial = q v_body q*; angular velocity in body axes.
- Camera frame: also FRD (x the view direction, y right, z down), a position and a quaternion. Cockpit view: ship position and attitude composed with the eye point and the head's look rotation; outside view: an orbit around the ship (or a body) with its own quaternion. `camera.lisp` turns it into clip rows.

Where each frame is used:
- Heliocentric: ephemerides, re-centring, the Sun's distance and direction (heat, lighting).
- Body-centred inertial: integration, rails propagation, elements, prediction, autopilot, orbit lines.
- Body-fixed: latitude/longitude, crash point, textures, ground track.
- Orbital and local horizon: instruments, attitude holds, burn directions.
- Ship body: thrust, torques, cockpit geometry, RCS layout.
- Camera: rendering only.

### Rendering precision
- Camera-relative, then single (`decision 5: Precision of positions`): each frame, for every drawn thing, `(position − camera position)` in double, in the frame of integration (or through the heliocentric frame for bodies, see the pitfalls), then converted to `single-float`. The GPU gets offsets in ICRF axes and the clip rows; no world-space positions, no world matrices.
- One projection function in `camera.lisp` (`decision 4: Depth strategy`): from the camera quaternion and the vertical field of view f = 1 / tan(fov/2), it returns four clip rows applied to a camera-relative point p (ICRF axes):
  - clip x = (f / aspect) (p · right), clip y = s f (p · down), clip z = near, clip w = p · forward; near = 0.1 m, so depth = near / (p · forward): reversed-Z with an infinite far plane, `:depth32-float`, compare `:greater`, clear 0.0.
  - s = +1 on Vulkan, where clip y points down (luv's Vulkan path; `docs/luv-capabilities.md` "Other findings"); s = −1 for a backend with y up. The sign is in this function only. The skeleton's `uniform-data` in `src/scene.lisp` is this function in its first form (right/up/forward, y flipped by negating `up`); `camera.lisp` replaces it in `milestone 2`.
  - Clip rows go to the GPU as single floats; they hold only directions and the focal scale, so single precision is enough.
- Draw order in one pass, reversed-Z throughout:
  1. Stars at infinity: directions only (no translation), clip z = 0 so depth is 0, compare `:always`, no depth write, additive.
  2. Sun, planets, moons as ray-traced impostors writing `:frag-depth`.
  3. Ships, asteroids, the cockpit: meshes in their body frames, with a per-draw model rotation (single, from the quaternion) and a camera-relative translation (double, then single).
  4. Orbit lines and markers: ribbons, depth-tested, no depth write, additive.
  5. Screen-space overlay (labels, the flight readout) last, compare `:always`.
- The cockpit is in the same pass: near 0.1 m resolves a panel at 0.5 m well; a separate cockpit pass with its own depth clear stays the fallback (`decision 4`). Cockpit geometry is in ship body axes and the camera sits inside the ship, so its translation is small and exact in single.
- Impostors: the CPU sends the body's camera-relative centre c (single), its radius, `c·c − r²` as `h (2r + h)` with h = |c| − r computed in double, and the body-fixed rotation (a 3×3 matrix, single) for the texture lookup. The shader solves the ray–sphere hit with the stable root (t = (c·c − r²) / (b + √(b² − (c·c − r²))), b = d·c) so the near hit keeps its precision at low altitude.
- Interpolation: when the step is long against the frame, rendering interpolates between the last two states in double before subtracting the camera; a re-centring between them is applied to both.

### Floating-origin pitfalls
- Subtract before converting: `(coerce (- p c) 'single-float)`, never `(- (coerce p …) (coerce c …))`; positions from different frames are brought into one frame in double first.
- Body positions relative to the camera go through the frame of integration: (body − origin) from the ephemerides in double, minus (camera − origin). For the Moon seen from LEO that is 3.8×10^8 m minus 7×10^6 m, both exact to ~1e-7 m; never through two heliocentric 1.5×10^11 m values converted separately.
- Cancelling terms in double on the CPU: `c·c − r²` as `h (2r + h)` (`|c|²` near Earth is ~5×10^13 m², its float error ~3×10^6 m²); altitude as `|r| − R` in double; small Δv and position differences between predicted and actual trajectories in double.
- Orbit lines: vertices are generated in double relative to their body (cached until the orbit changes) and made camera-relative each frame: (vertex + body − camera) in double, then single. A cache of single vertices relative to the body would jitter by ~0.5 m near the ship in LEO.
- Shaders never see absolute time or large angles: Earth's rotation, blinking lights and noise phases are reduced on the CPU.
- Quaternions are renormalised in double after each integration step; matrices for the GPU are derived from them each frame, never accumulated in single.
- Long-lived single-float caches of anything that moves with the simulation (positions, trajectories) are a bug; caches are double and body-relative.

## Time and integration

PLACEHOLDER(34): written by `task 34: Architecture: integrator, gravity model, ephemerides, time warp, collisions and prediction`.

## Testing

PLACEHOLDER(35): written by `task 35: Architecture: testing section with drift bounds; read-through against the other documents`.
