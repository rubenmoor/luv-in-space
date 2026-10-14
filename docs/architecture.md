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
  - `luv/production`: a latest-value worker for trajectory prediction off the frame thread; not used: prediction runs on the frame thread (`decision 20: Trajectory prediction on the frame thread`).
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

The ship is integrated with classical RK4 on a fixed, deterministic step in its dominant body's frame, against the Sun, Earth, Moon and later the planets moving on rails from analytic ephemerides; Earth's J2 is in. The same integrator, step rule and force model drive flight at every warp level, trajectory prediction and the autopilot's refined burns, so what the map shows is what the ship flies. Only objects other than the ship use Kepler rails. These rules are `decision 16: Integrator and simulation clock`, `decision 17: Gravity model`, `decision 18: Ephemerides`, `decision 19: Time-warp rules` and `decision 20: Trajectory prediction on the frame thread`; the code lives in `physics.lisp`, `clock.lisp`, `bodies.lisp`, `orbits.lisp`, `hazards.lisp` and `prediction.lisp` (`decision 12: Module layout and package split`).

### The clock
- Each frame: `acc += min(elapsed, 0.1 s) × warp` in `double-float`; the 0.1 s cap absorbs stalls (debugger, a parked `./sly` frame, window drags).
- While `acc ≥ h` (the next step, see below): step, `t += h`, `acc −= h`; at most 4,096 steps per frame and at most 4 ms of steps (checked every 64 steps). What remains beyond the cap is dropped (`acc` clamped below one step): the sim slows rather than stalls, and the warp readout shows the actual rate, measured over the last second.
- The step count per frame never changes a trajectory: steps depend on the state, the controls and the warp level, not on the frame rate or the cap.
- Controls (input, autopilot, test scripts write `controls`) are sampled once per frame and held over each step (zero-order hold).
- Rendering lags the simulation by one step and interpolates between the last two states: cubic Hermite for positions (from positions and velocities, in double), slerp for attitude; a re-centring between them is applied to both (see "Rendering precision"). At ×1 the lag is ≤ 1/64 s.
- Elapsed time comes from luv's canvas frame function; nothing in the simulation reads a wall clock.

### Integrator
- Classical fixed-step RK4 on the whole ship state: position r and velocity v (body-centred inertial, ICRF axes), attitude quaternion q (body → inertial), angular velocity ω (body axes), propellant mass m_p (`decision 16: Integrator and simulation clock`).
- Derivatives: ṙ = v; v̇ = gravity(t, r) + q (F_body / m) q*; q̇ = ½ q ⊗ (0, ω); I ω̇ = τ − ω × I ω (Euler's equations, principal axes, I from the current propellant load, held over a step); ṁ_p = −Σ F_i / (I_sp,i g₀), g₀ = 9.80665 m/s².
- After each step: q renormalised in double; propellant that would go negative within a step instead runs the last step at the throttle fraction that empties it exactly, so the step stays on the grid.
- Why RK4 over a symplectic integrator: what players, rendezvous, prediction and the autopilot notice is phase (along-track) error, and RK4's is ~10^5 times smaller than velocity Verlet's at the same cost per orbit (measured below); symplectic schemes bound energy error only with a constant step and a separable, conservative system, and thrust, mass flow, attitude and a state-dependent step break all three. RK4's energy drift is secular but measured at 1e-11 per 1,000 LEO orbits.
- Step, `h`:
  - Fine: 1/64 s at warp ×1 and ×4 (time warp below), whatever the state. It keeps input latency under a frame, resolves RCS pulses and rotation (ω h ≤ 0.02 rad at 1 rad/s), and is far below orbital accuracy needs.
  - Coarse (above ×4): h = η · min over gravitating bodies of √(d³/μ), d the distance to each body's centre, η = 1/256, rounded down to a power of two and clamped to [1/64 s, 16,384 s]; under thrust also h ≤ 1 s. 2π/η ≈ 1,600 to 3,200 steps per orbit after rounding, at any eccentricity or distance (shorter near periapsis, longer at apoapsis): 2 s in a 400 km LEO, 512 s on a translunar coast, 16,384 s heliocentric.
  - All steps are powers of two ≥ 1/64 s, and scheduled events (burn start and end, autopilot commands) are on the 1/64 s grid, so `t` stays a multiple of 1/64 s and exact (`decision 14: Units and time epoch`); a step is shortened to land on the next event.
- Re-centring after a step, never inside one (`decision 15: Coordinate frames`).
- Cost: measured ~180 ns per RK4 step with Earth point mass, J2, and the Sun and Moon as third bodies (typed `double-float` SBCL, x86_64 i5-12500H; body positions from `sin`/`cos` as a stand-in for the ephemeris cache), ~50 ns for a point mass alone. Attitude and thrust add a few tens of ns; the 4,096-step cap is ~1 ms.

### Measured drift
A throwaway SBCL script (not committed) integrated two-body Earth orbits (μ = 3.98600435507×10^14 m³/s², periapsis 6,778 km, i = 51.6°) and compared with the exact solution after a whole number of periods; relative energy error ΔE/E, relative angular-momentum error ΔL/L and position error at the end.

| Orbit | Integrator, step | Orbits | Steps/orbit | ΔE/E | ΔL/L | Position error |
|---|---|---|---|---|---|---|
| circular | RK4, 1/64 s | 100 | 355,421 | 1.3e-12 | 6.5e-13 | 4 mm |
| circular | RK4, 1 s | 1,000 | 5,554 | 4e-14 | 2e-14 | 5 mm |
| circular | RK4, 2 s (η = 1/256) | 1,000 | 2,777 | 1.0e-11 | 5.2e-12 | 0.36 m |
| circular | RK4, 4 s (η = 1/128) | 1,000 | 1,388 | 3.3e-10 | 1.7e-10 | 11 m |
| circular | RK4, 16 s | 1,000 | 347 | 3.4e-7 | 1.7e-7 | 11 km |
| circular | Verlet, 1 s | 1,000 | 5,554 | 2e-13 | 1e-13 | 18 km |
| e = 0.5 | RK4, η = 1/256 | 1,000 | 2,472 | 8.4e-11 | 2.8e-11 | 10 m |
| e = 0.5 | RK4, η = 1/512 | 1,000 | 4,944 | 2.7e-12 | 7.6e-13 | 0.36 m |
| e = 0.5 | Verlet, η = 1/512 | 1,000 | 4,944 | 2.4e-6 | 3e-14 | 31 km |
| e = 0.9 | RK4, η = 1/256 | 1,000 | 4,355 | 1.4e-9 | 4.1e-11 | 1.6 km |

- At the 1/64 s fine step the error is roundoff, not truncation (~1e-12 after 3.5×10^7 steps).
- With η = 1/256 one LEO orbit costs ~2,800 steps (~0.5 ms), and 1,000 orbits (two months) drift less than a metre; e = 0.9 orbits are rarer and still within 2 km per 1,000 orbits.
- Coarse-step drift with third bodies and J2 is not a conservation test (energy is not conserved in a moving frame); `task 35: Architecture: testing section with drift bounds; read-through against the other documents` sets the test bounds on two-body runs from these numbers.

### Gravity model
- n-body against on-rails bodies, in the ship's dominant-body frame (`decision 17: Gravity model`; `decision 15: Coordinate frames`): the central body's point mass plus each third body's direct minus indirect term, μ_b ((d_b − r)/|d_b − r|³ − d_b/|d_b|³), d_b the body's position in the frame.
  - Earth frame: Earth (with J2), Sun, Moon.
  - Moon frame: Moon, Earth (point mass), Sun.
  - Sun frame: Earth, Moon (separately, not the barycentre) and, from `milestone 4`, the planets as point masses with system μ.
  - Planet frames (milestone 4): planet, Sun.
- Earth's J2 in: J2 = 1.08263×10^-3 (EGM2008), with Earth's equatorial radius; Earth's pole is ICRF z (`decision 15`), so the term is a few multiplications in the frame's own axes. It is the largest perturbation in LEO (~1e-2 m/s², against ~1e-6 m/s² from the Moon) and makes orbit planes regress ~5° per day at 51.6°, which a player can see and sun-synchronous orbits rely on.
- Out, with the reason: other zonal and tesseral terms, the Moon's J2 (invisible at game scale); drag and an atmosphere (needs a model and matters only below ~300 km; the design may add an exponential atmosphere as a force); solar radiation pressure (~1e-7 m/s² for a dense ship); relativity.
- Patched conics and Kepler elements are not the ship's motion; they serve osculating elements for instruments and orbit lines, first guesses for the autopilot (Hohmann, Lambert) which then refines its burns by shooting with the integrator, and rails for other objects.

### Ephemerides
- Sun: fixed at the heliocentric origin (`decision 18: Ephemerides`); its barycentric wobble is ignored.
- Planets and the Earth–Moon barycentre: Standish, "Keplerian Elements for Approximate Positions of the Major Planets" (JPL Solar System Dynamics), Table 1: J2000 ecliptic elements and their rates per Julian century, valid 1800–2050, errors of tens of arcseconds (~10^3–10^4 km). Solve Kepler's equation (Newton from M + e sin M, to 1e-14 rad), rotate ecliptic → ICRF by ε (see "Frames"). Velocities from the same elements analytically.
- Moon: the truncated ELP-2000/82 series in Meeus, "Astronomical Algorithms" (2nd ed., 1998), ch. 47: geocentric ecliptic longitude, latitude and distance of date (~10″, ~20 km), precessed to the J2000 ecliptic (general precession in longitude), then ε; velocity by central difference of ±60 s in double (error ~1e-5 m/s).
- Earth: barycentre − μ_Moon / (μ_Earth + μ_Moon) × geocentric Moon, so Earth wobbles ~4,700 km monthly around the barycentre.
- Cache: each body is read through cubic Hermite nodes (position and velocity) at fixed spacing, 4,096 s for Earth and Moon and 65,536 s for the planets; a node is a pure function of its index, computed on first use and kept in a small table in the `game` object. Interpolation error is below 2 cm for the Moon; the integrator asks four times per step and pays a lookup, not a series.
- Time range: Table 1 ends at 2050-01-01. The default start date is 2040 ("Time"); the design keeps game time within the decade, or a task switches to Standish Table 2a/2b (3000 BC–3000 AD, coarser). A test asserts the start date is inside the range.
- Spot checks against JPL Horizons are `task 35: Architecture: testing section with drift bounds; read-through against the other documents`'s.

### Rails
- The ship is never on rails: at every warp level it is integrated (`decision 17`). The step rule makes coasting at high warp cheap (2 s steps in LEO), so the switch and its jump are avoided rather than handled.
- Bodies (Sun, planets, Moon) are always on rails from the ephemerides.
- Other objects (stations, other ships, asteroids) are on Kepler rails around their body: elements at an epoch, propagated exactly for any t; around Earth with J2's secular rates (Ω̇, ω̇, Ṁ) so stations precess like the ship (`decision 19: Time-warp rules`).
- Promotion: an object within 100 km of the ship becomes integrated with the ship's force model and step (elements → state at the current t, exact at that instant); beyond 200 km it is demoted (state → osculating elements, plus the secular rates). A station on rails ignoring the Moon and J2's short-period terms would drift metres per hour against a ship parked beside it.
- Round trip state → elements → state reproduces the state to ~1e-9 relative (`task 35` tests it); near-circular and near-equatorial orbits use non-singular forms (argument of latitude, true longitude) inside `orbits.lisp`.

### Time warp
Levels and rules are `decision 19: Time-warp rules`; where they touch play, the design picks within them.

- Levels: ×1, ×4, ×10, ×100, ×1,000, ×10,000, ×100,000, ×1,000,000. The design may drop levels, not add finer step rules.
- ×1 and ×4: the fine 1/64 s step, full rotational dynamics, all controls.
- Above ×4: the coarse step; attitude is kinematic: held inertial or in the orbital frame (prograde, normal, radial), integrated as the hold's rotation, no Euler's equations; manual RCS is off. Entering needs a hold or |ω| < 0.01 rad/s.
- Thrust: allowed up to ×1,000 under an attitude hold (long low-thrust burns of tugs); the design may lower the limit, not raise it above ×1,000 without a new decision.
- Budget: a level is never refused for cost; in LEO ×100,000 fits the 4,096-step cap (~830 steps per frame at 60 Hz), ×1,000,000 runs at the cap and the readout shows the actual rate (~×490,000). The design may grey out levels whose actual rate is below the nominal one, or add altitude limits for comfort.
- Drop-outs, from the prediction's events: the clock lowers the warp so that the next event arrives no sooner than 3 s real time, and goes to ×1 at it:
  - impact, or a periapsis below a crash radius;
  - a sphere-of-influence change (design default: stop; it may let warp continue across);
  - a scheduled burn, at its start minus its lead time (attitude settle; design default 60 s);
  - entering a Sun hazard zone (the design's rule).
- Warp up is refused while thrust is on above the thrust limit, while rotating without a hold, or with an event less than 3 s real time away at the new level.
- Changing warp never changes the state; only the step size changes, at a step boundary.

### Collisions and the Sun
- Crash spheres: one per body, radius R + h_crash in its frame; defaults h_crash = 0 for all bodies and 100 km for Earth (entry interface: a ship without a heat shield does not survive entry); the design picks the radii and what a crash means. Bodies are spheres (`decision 14`).
- Swept test after each step, against the central body and every third body with a crash sphere (the Moon from Earth's frame): the step's path relative to the body is the cubic Hermite of the two relative states; the chord's closest approach minus the bound h² |a|_max / 8 on the curve's sag is compared with the radius, and only then refined (Newton on d|p(s)|²/ds over the Hermite). With the coarse step rule the sag is ~r / 500,000, so no warp level can tunnel through a body or skip a periapsis below the surface.
- An impact gives its exact time (on the Hermite) and the point in the body-fixed frame (latitude, longitude) for the crash view.
- Sun: the flux S = L☉ / (4π d²), L☉ = 3.828×10^26 W (IAU 2015 B3), 1,361 W/m² at 1 AU, computed each step from the heliocentric distance, zero in a body's shadow (cylindrical shadow of Earth, Moon, planets). Offered to the design: the radiative-equilibrium temperature of a plate facing the Sun, T = (α S / (2 ε σ))^(1/4) (331 K at 1 AU for α = ε), and a hull temperature that relaxes towards it with a time constant (updated exactly per step for constant flux). The rule that burns and kills is the design's; the Sun's sphere at R☉ is the last crash sphere.

### Trajectory prediction
- On the frame thread (`decision 20: Trajectory prediction on the frame thread`): `prediction.lisp` runs the flight's integrator, force model and coarse step rule from a copy of the current state, with planned burns as scheduled controls, and keeps every step's state (double, relative to its body) for ribbons and event search; `luv/production` is not a dependency (`decision 3: luv ASDF systems the game depends on` stands).
- Budget: 1 ms per frame, resumable; one LEO orbit is ~2,800 steps (~0.5 ms), a translunar coast a few thousand, so a full prediction takes one to a few frames.
- Horizon: until the first of one osculating period (two in the map view), an SOI change plus one period (or periapsis) in the new body, an impact, 30 days inside a planet's sphere or 2 years heliocentric, 20,000 steps.
- Resolution: one point per step (~2,800 per LEO orbit); the ribbon code decimates to its vertex budget; events (apsides, nodes, SOI changes, impact, closest approach to a target) are found by root-finding on the Hermite between stored states.
- Recompute: while coasting the prediction stays valid (determinism) and only its tail is extended; under thrust or after a control change it restarts each frame with a one-period horizon; it also restarts when it deviates from the flown state by more than 1 m (fine and coarse steps differ by millimetres per orbit).
- The autopilot shoots with the same function (differential correction of burn times and Δv); its iterations share the budget and may take several frames.
- Revisit, recording `luv/production` as a dependency, only if a profile shows prediction above 2 ms per frame (many promoted objects, autopilot iterations).

### Determinism
- Same initial state and the same controls and warp level per step give the same trajectory bit for bit on one build and platform: fixed steps chosen from the state, no wall clock, frame rate or step cap in the simulation, ephemeris nodes as pure functions of their index, no hash-table iteration order or unseeded randomness in simulation code (random state, if any, in the `game` object with a seed).
- SBCL does not contract multiply–add, so arithmetic is plain IEEE double; `sin`, `cos`, `atan2`, `exp` come from libm and may differ in the last bit between x86_64 and aarch64, so cross-platform comparisons use tolerances.
- Replays and scripted runs record controls and warp keyed by step index (`t` on the 1/64 s grid), not by frame; `task 35` uses this for scripted burns.
- A function redefined in the live image changes the trajectory from that step on, as expected; tests run in a fresh image.

## Testing

PLACEHOLDER(35): written by `task 35: Architecture: testing section with drift bounds; read-through against the other documents`.
