# Architecture

This document is the technical ground of Luv in Space: how the game is built on [luv](https://github.com/mbrock/luv), how its source is laid out, and the technical choices (units, frames, integration, testing) every later milestone implements from. It rests on `docs/luv-capabilities.md`, which records what luv offers and lacks, and on the walking skeleton in `src/`, `tests/`, `scripts/` and `flake.nix`. Where a choice depends on the game design, it gives the architecture's default and the constraint it puts on `docs/design.md`. Commands to build, run, test and screenshot are in `AGENTS.md` "Commands" and not repeated here; lasting choices are pm decisions, cited by id and subject.

## Dependency on luv

The game is its own renderer and simulation on luv's HAL, shader language, SDL3 canvas and capture path. luv comes in as a pinned flake input; nothing in `../luv` is changed or loaded from there, and what luv lacks is written here and listed in `docs/luv-wishlist.md`.

### Flake input and package
- Input `github:mbrock/luv` pinned to `6abc9d10ec8991c2cfe92888b84946372d6030b9` in `flake.nix` and `flake.lock`; `nixpkgs` follows luv's so luv's binary cache matches (`decision 1: How the game consumes luv`).
- Dev shell, package and check all come from `luv.lib.environmentFor <system>`: the dev shell from `developmentEnvironment`, `developmentPackages` and `developmentEnvironmentHook`, plus `pm` and `git`; the check from the `slim…` variants.
- luv's Lisp sources: the input's store path on `CL_SOURCE_REGISTRY` (on Darwin a writable copy, since luv writes its Objective-C bridge next to its sources; `docs/luv-wishlist.md`).
- Package: `applicationPackage` in `flake.nix` is a copy of luv's unexported recipe, adapted: source is `gameSource` (`luv-in-space.asd`, `src/`, `scripts/build.lisp`), `sbcl --script scripts/build.lisp` dumps `luv-in-space/program`, the FASL cache ships under `lib/fasl/`, no fonts or Luvcraft extras.
- Launcher (`bin/luv-in-space`), scripts and the check unset `LD_LIBRARY_PATH`: a host one breaks luv's SDL3 (`docs/luv-capabilities.md` "Packaging"). `scripts/test`, `scripts/screenshot` and `./sly` always re-enter `nix develop` (~0.6 s), whose hook unsets it, unless `LIS_DEV_SHELL=1` says they already did (the check sets it); an inherited dev shell (direnv) is not trusted.
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
  - `luv/ghostty`: not used: `docs/design.md` cuts the terminal screen (`decision 36: Cockpit instruments: Slug screens, dot-matrix strip, gauges and lamps; Ghostty out`); it would need the full dev shell, not `slim`.

### What the game uses from luv and what it writes
- From luv:
  - HAL: devices, buffers, textures, samplers, bind groups, render pipelines, encoders, `draw`/`draw-indexed` with instancing ("Rendering").
  - Shader language: `define-shader` in `luv-in-space.shaders`; every pipeline a luv live shader pipeline (`make-live-shader-pipeline`, a role keyword, `shader-specification-for` methods), returned by `luv:application-live-artifacts` and refreshed at the top of each frame (`decision 11: Render pipelines are luv live shader pipelines`).
  - SDL3 canvas: windows, cadence clock, key and pointer events, relative pointer mode ("Windowing and input").
  - `capture-application-screenshot` for screenshots and the GPU test (`decision 2: Screenshots and GPU test: offscreen capture on a hidden canvas, headless on lavapipe`).
  - `sdl3-image` (loaded by `luv`) for decoding textures ("Rendering", image loading).
  - `audio-sink` for sound ("Audio"; opened, fed and closed on SDL's `dummy` driver in run 35).
  - Slug glyph rendering and HarfBuzz shaping for instrument text ("HUD and instruments").
- Written here, each after its section in `docs/luv-capabilities.md` "Gaps and workarounds", in the files of "Files in load order":
  - Camera and projection: reversed-Z, infinite far plane, clip-y flip in one function ("Depth range across 10^12 m"; `decision 4: Depth strategy`): `camera.lisp` (the skeleton's `scene.lisp` until then).
  - Camera-relative conversion from `double-float` world state ("Double-precision positions"; `decision 5: Precision of positions`): `math.lisp` (double vectors, the single-float boundary), `camera.lisp`.
  - Sphere impostors for the Sun, planets and moons, Blue Marble Earth ("Spheres and planet surfaces"): `impostors.lisp`; texture decoding through `sdl3-image` into a rank-2 array in `assets.lisp`.
  - Stars from a catalogue as instanced quads at infinity ("Starfield"): `stars.lisp`, the table from `scripts/convert-stars`.
  - HDR target, bloom, tone map, exposure ("Sun glare and brightness"): `post.lisp`.
  - Screen-space ribbons and markers ("Orbit lines and markers"): `ribbons.lisp`.
  - Text runs (a copy of Luvcraft's `text.lisp`) and cockpit screens: Slug text and ribbons drawn on the screens' planes, the grapple camera the only render target ("HUD and instruments"; `decision 36: Cockpit instruments: Slug screens, dot-matrix strip, gauges and lamps; Ghostty out`); dot-matrix placeholder labels until then (`decision 7: Placeholder labels: dot-matrix overlay quads`): `text.lisp`, `cockpit.lisp`, `overlay.lisp` (the skeleton's `scene.lisp` until then).
  - Instanced meshes for ships and asteroids ("Instancing many objects"): `meshes.lisp`.
  - Physics: clock, warp, integrator, Kepler propagation, ephemerides ("Fixed physics step and time warp"; "Physics and SIMD"): `clock.lisp`, `physics.lisp`, `orbits.lisp`, `bodies.lisp`.
  - Audio mixer and synthesised voices ("Audio"): `audio.lisp`; saves ("Save files"): `saves.lisp`; gamepad polling ("Gamepad and joystick"): `input.lisp`.
  - From "Other findings": shader functions for `atan2` and friends (no `atan`/`asin`/`acos`/`tan`) in `shaders.lisp`; the failed-live-artifact report in `main.lisp` (`report-failed-live-artifacts`, called by our `./sly status`); launchers without `LD_LIBRARY_PATH` in `flake.nix` ("Flake input and package"); the `./sly` client in `scripts/sly-client.lisp` ("Dev loop").

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
- `math.lisp` (milestone 2): `double-float` vec3 `(simple-array double-float (3))`, quaternion `(simple-array double-float (4))` as (w x y z), unit, body → inertial, and mat3 `(simple-array double-float (9))` row-major on the CPU; plain functions return fresh arrays, hot ones have in-place `!` twins with the destination first (`vec3-add! out a b`); `relative-single` the one conversion to `single-float` arrays at the GPU boundary (`decision 65: Vectors and quaternions in math.lisp`); needs package.
- `units.lisp` (milestone 2): SI constants: G·M and radii of bodies, epoch, conversions for display (km, km/s) with their source; needs math.
- `bodies.lisp` (milestone 2: Sun, Earth, Moon; 4: planets): bodies (Sun, Earth, Moon, planets): μ, radius, rotation, ephemeris function of time; needs units.
- `orbits.lisp` (milestone 2; 4: hyperbolas): orbital elements ↔ state vectors for ellipses and hyperbolas, Kepler propagation, apsides, periods, crossing times of a radius; needs math, bodies.
- `physics.lisp` (milestone 2; 3: objects): state vectors (position, velocity, attitude, angular velocity), gravity, the integrator with forces and torques as arguments, frames of integration; from `milestone 3` the object type that `clock.lisp`, `hazards.lisp` and `places.lisp` share (rails segments or promoted state, removal, mass, inertia tensor, attitude mode, collision shapes, fixtures; "Rails"), the rails segment builder and the swept-sphere test that it and `hazards.lisp` share; needs orbits.
- `ship.lisp` (milestone 2; 3: grapple, stack, berth): ship definition: frame, modules, mass, thrusters, propellant, rigid-body attitude, collision boxes; from `milestone 3` the grapple's state, the stack with a load, the berthed state and propellant transfer; needs physics.
- `controls.lisp` (milestone 2): the control state (throttle, RCS, attitude commands) that input, autopilot and test scripts all write; needs ship.
- `clock.lisp` (milestone 2; 3: checkpoints after plans and berthing): simulation clock, fixed steps, time warp levels and their rules, rails switching; the simulation state as one copyable object, and checkpoints as in-memory snapshots of it ("Checkpoints"); needs physics, controls; asks `prediction.lisp`, loaded later, for the next event (a call at run time, not a load dependency).
- `hazards.lisp` (milestone 2: crash; 3: contact; 4: Sun, life support): crash detection against bodies for the ship and promoted objects (an object's crash removes it), contact with objects (capture, bounce, death), the hull temperature, consumables, death and its cause; needs physics, bodies, ship.
- `prediction.lisp` (milestone 2: events for warp drop-outs and flight scripts; 3: trajectories for orbit lines, instruments and the autopilot): trajectory prediction; needs orbits, clock, hazards.
- `autopilot.lisp` (milestone 3; 4: across spheres of influence, hyperbolic targets): orbit targeting, burns, presets; writes `controls`; needs prediction, controls.
- `places.lisp` (milestone 3: Earth orbit, the scenery ships, POD 14; 4: Moon, the passing rock, real asteroids): points of interest in Earth orbit, asteroids, destinations of presets; from `milestone 5` the story ships' schedules ("Story ships' schedules"); needs bodies, orbits.
- `quests.lisp` (milestone 5): the quest line: state, triggers, messages; needs places, autopilot.
- `saves.lisp` (milestone 5): versioned s-expression save files, atomic writes, under `$XDG_DATA_HOME/luv-in-space/`; writes the clock's snapshots as `checkpoint.sexp` and `resume.sexp` (`decision 35: Saving and audio`); needs quests, ship, clock.
- `flights.lisp` (milestone 2; 3: pilot actions, stated starts; 5: quests, chained starts): flight scripts: read a script, write its actions to `controls` per step, the terminal approach, start states, run it headless or in the live game ("Scripted runs"); the last simulation file, so new simulation files go before it; needs controls, clock, prediction, and from their milestones autopilot, places, quests, saves.
- `shaders.lisp` (skeleton): all `define-shader`s and `shader-specification-for` methods (package `luv-in-space.shaders`); split into `shaders-<pass>.lisp` after it when it grows; needs package.
- `assets.lisp` (milestone 2): locating and loading assets: textures through `sdl3-image`, the star table; needs package.
- `gpu.lisp` (milestone 2): HAL helpers: buffers, live pipelines, views and size-cached targets (from the skeleton's `scene.lisp`); needs shaders.
- `camera.lisp` (milestone 2; 3: cockpit view): camera state, the one projection function (reversed-Z, infinite far, clip-y flip), camera-relative conversion; needs math, gpu.
- `stars.lisp` (milestone 2): star table to instanced quads at infinity; needs assets, camera.
- `impostors.lisp` (milestone 2; 3: Earth's night lights and clouds; 4: the Moon's textures, planets): sphere impostors for the Sun, planets and moons; Earth's texture; needs bodies, assets, camera.
- `meshes.lisp` (milestone 2: ship; 3: places and other ships; 4: asteroids): generated meshes (ship frame, modules, stations, depots, other ships, asteroids) and their instanced draws; needs ship, camera.
- `ribbons.lisp` (milestone 2: the current orbit; 3: predicted trajectories; 4: bodies' orbits, the Sun's danger circles): screen-space ribbons and markers: orbit lines, trajectories, targets; needs orbits, camera, later prediction.
- `post.lisp` (milestone 2): HDR target, bloom, tone map, exposure; needs gpu.
- `overlay.lisp` (milestone 2; 3: the message line and the autopilot panel): screen-space 2D pass: dot-matrix labels (`decision 7`) and the minimal flight readout; needs gpu.
- `text.lisp` (milestone 3): Slug text runs (adapted copy of Luvcraft's `text.lisp`, attributed in its header); needs gpu.
- `cockpit.lisp` (milestone 3; 4: Sun and life-support instruments; 5: RADIO, JOB and tape notes): the DIY cockpit: geometry, screens (Slug text on their planes), the grapple camera's render target, dot-matrix strip, gauges, lamps, buttons and picking, messages; needs text, ship, prediction.
- `render.lisp` (milestone 2): the frame: passes in order, encoding everything above into the canvas or a capture; needs all rendering files.
- `input.lisp` (milestone 2; 6: gamepad polling, a "could"): canvas events to `controls`; key map; needs controls.
- `audio.lisp` (milestone 6, a "should" in `docs/design.md` "Feature list"): `audio-sink`, the Lisp mixer, synthesised voices; silent when no device; needs ship, controls.
- `title.lisp` (milestone 6): title screen, introduction, in-game controls help, the Escape menu and the credits screen (`decision 13: Asset storage and attribution`, `decision 42: Scope: what each milestone builds`); needs overlay, text.
- `main.lisp` (skeleton): the `game` object, start/stop, the frame function, `main`; needs everything.
- `capture.lisp` (skeleton): screenshots and `capture-scene`; needs main.

- The simulation files (`math` to `flights`) never call the HAL or SDL, so tests load and run them without a canvas; rendering files read simulation state and never change it.
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
- The game's start date: a calendar constant `*start-date*` in `units.lisp`, 2040-01-01 00:00 UTC, kept by `docs/story.md` "Start date" (`decision 50: Story: start date and the passing rock's date`). A new game sets `t` from it; a save stores `t` itself.
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

The ship is integrated with classical RK4 on a fixed, deterministic step in its dominant body's frame, against the Sun, Earth, Moon and later the planets moving on rails from analytic ephemerides; Earth's J2 is in. The same integrator, step rule and force model drive flight at every warp level, trajectory prediction and the autopilot's refined burns, so what the map shows is what the ship flies. Only objects other than the ship use Kepler rails. These rules are `decision 16: Integrator and simulation clock`, `decision 17: Gravity model`, `decision 18: Ephemerides`, `decision 19: Time-warp rules` (its drop-out events extended by `decision 43: Warp drop-outs for contact and life support`), `decision 20: Trajectory prediction on the frame thread`, `decision 44: Contact, capture and berthing in the simulation` and `decision 46: Rails across spheres, hyperbolas and re-entry`; the code lives in `physics.lisp`, `ship.lisp`, `clock.lisp`, `bodies.lisp`, `orbits.lisp`, `hazards.lisp` and `prediction.lisp` (`decision 12: Module layout and package split`).

### The clock
- Each frame: `acc += min(elapsed, 0.1 s) × warp` in `double-float`; the 0.1 s cap absorbs stalls (debugger, a parked `./sly` frame, window drags).
- While `acc ≥ h` (the next step, see below): step, `t += h`, `acc −= h`; at most 4,096 steps per frame and at most 4 ms of steps (checked every 64 steps). What remains beyond the cap is dropped (`acc` clamped below one step): the sim slows rather than stalls, and the warp readout shows the actual rate, measured over the last second.
- The step count per frame never changes a trajectory: steps depend on the state, the controls and the warp level, not on the frame rate or the cap.
- Controls (input, autopilot, test scripts write `controls`) are sampled once per frame and held over each step (zero-order hold).
- Rendering lags the simulation by one step and interpolates between the last two states: cubic Hermite for positions (from positions and velocities, in double), slerp for attitude; a re-centring between them is applied to both (see "Rendering precision"). At ×1 the lag is ≤ 1/64 s.
- Elapsed time comes from luv's canvas frame function; nothing in the simulation reads a wall clock.

### Integrator
- Classical fixed-step RK4 on the whole ship state: position r and velocity v (body-centred inertial, ICRF axes), attitude quaternion q (body → inertial), angular velocity ω (body axes), propellant mass m_p (`decision 16: Integrator and simulation clock`).
- Derivatives: ṙ = v; v̇ = gravity(t, r) + q (F_body / m) q*; q̇ = ½ q ⊗ (0, ω); I ω̇ = τ − ω × I ω (Euler's equations with the full symmetric inertia tensor I in body axes about the current centre of mass, and its inverse, from `ship.lisp` for the current propellant load and any grappled load, held over a step; not diagonal, as the cab tilts the principal axes, `docs/design.md` "Inertia"); ṁ_p = −Σ F_i / (I_sp,i g₀), g₀ = 9.80665 m/s².
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
- Coarse-step drift with third bodies and J2 is not a conservation test (energy is not conserved in a moving frame); the test bounds are set on two-body runs from these numbers ("Drift bounds").

### Gravity model
- n-body against on-rails bodies, in the ship's dominant-body frame (`decision 17: Gravity model`; `decision 15: Coordinate frames`): the central body's point mass plus each third body's direct minus indirect term, μ_b ((d_b − r)/|d_b − r|³ − d_b/|d_b|³), d_b the body's position in the frame.
  - Earth frame: Earth (with J2), Sun, Moon.
  - Moon frame: Moon, Earth (point mass), Sun.
  - Sun frame: Earth, Moon (separately, not the barycentre) and, from `milestone 4`, the planets as point masses with system μ.
  - Planet frames (milestone 4): planet, Sun.
- Earth's J2 in: J2 = 1.08263×10^-3 (EGM2008), with Earth's equatorial radius; Earth's pole is ICRF z (`decision 15`), so the term is a few multiplications in the frame's own axes. It is the largest perturbation in LEO (~1e-2 m/s², against ~1e-6 m/s² from the Moon) and makes orbit planes regress ~5° per day at 51.6°, which a player can see and sun-synchronous orbits rely on; a polar orbit keeps its plane, which is why the start orbit and every Earth-orbit place are polar (`decision 38: Start orbit: polar`).
- Out, with the reason: other zonal and tesseral terms, the Moon's J2 (invisible at game scale); drag and an atmosphere (needs a model and matters only below ~300 km; the design may add an exponential atmosphere as a force); solar radiation pressure (~1e-7 m/s² for a dense ship); relativity.
- Patched conics and Kepler elements are not the ship's motion; they serve osculating elements for instruments and orbit lines, first guesses for the autopilot (Hohmann, Lambert) which then refines its burns by shooting with the integrator, and rails for other objects.

### Ephemerides
- Sun: fixed at the heliocentric origin (`decision 18: Ephemerides`); its barycentric wobble is ignored.
- Planets and the Earth–Moon barycentre: Standish, "Keplerian Elements for Approximate Positions of the Major Planets" (JPL Solar System Dynamics), Table 1: J2000 ecliptic elements and their rates per Julian century, valid 1800–2050, errors of tens of arcseconds (~10^3–10^4 km). Solve Kepler's equation (Newton from M + e sin M, to 1e-14 rad), rotate ecliptic → ICRF by ε (see "Frames"). Velocities from the same elements analytically.
- Moon: the truncated ELP-2000/82 series in Meeus, "Astronomical Algorithms" (2nd ed., 1998), ch. 47: geocentric ecliptic longitude, latitude and distance of date (~10″, ~20 km), precessed to the J2000 ecliptic (general precession in longitude), then ε; velocity by central difference of ±60 s in double (error ~1e-5 m/s).
- Earth: barycentre − μ_Moon / (μ_Earth + μ_Moon) × geocentric Moon, so Earth wobbles ~4,700 km monthly around the barycentre.
- Cache: each body is read through cubic Hermite nodes (position and velocity) at fixed spacing, 4,096 s for Earth and Moon and 65,536 s for the planets; a node is a pure function of its index, computed on first use and kept in a small table in the `game` object. Interpolation error is below 2 cm for the Moon; the integrator asks four times per step and pays a lookup, not a series.
- Time range: Table 1 ends at 2050-01-01. The default start date is 2040 ("Time"); the design keeps game time within the decade, or a task switches to Standish Table 2a/2b (3000 BC–3000 AD, coarser). A test asserts the start date is inside the range.
- Spot checks against JPL Horizons: "Rails, orbits and frames" under "Testing".

### Rails
- The ship is never on rails: at every warp level it is integrated (`decision 17`). The step rule makes coasting at high warp cheap (2 s steps in LEO), so the switch and its jump are avoided rather than handled.
- Bodies (Sun, planets, Moon) are always on rails from the ephemerides.
- Other objects (stations, other ships, asteroids) are on Kepler rails around their body: elements at an epoch, propagated exactly for any t; around Earth with J2's secular rates (Ω̇, ω̇, Ṁ) so stations precess like the ship (`decision 19: Time-warp rules`).
- Promotion: an object within 100 km of the ship becomes integrated with the ship's force model and step (elements → state at the current t, exact at that instant); beyond 200 km it is demoted (state → osculating elements, plus the secular rates). A station on rails ignoring the Moon and J2's short-period terms would drift metres per hour against a ship parked beside it.
- Round trip state → elements → state reproduces the state to ~1e-9 relative ("Rails, orbits and frames" tests it); near-circular and near-equatorial orbits use non-singular forms (argument of latitude, true longitude) inside `orbits.lisp`, near-parabolic ones the forms under "Hyperbolic rails".
- Objects are rigid bodies (`decision 44: Contact, capture and berthing in the simulation`), one type in `physics.lisp`, instances from `places.lisp`: mass, inertia tensor, collision shapes, fixtures, and an attitude mode:
  - Towable objects (pods, bagged rocks, Derelict K-17): on rails a constant spin about a principal axis, q(t) in closed form; promoted, q and ω are integrated torque-free (Euler's equations with the object's tensor) alongside r and v, so a contact or a capture can change them.
  - Berth objects (stations, depots; also the parked rock mover, which has no free fixture): attitude kinematic from their mode (held in the orbital frame, inertial, or Sun-pointing, as `places.lisp` gives it; per object in `docs/design.md` "Rigid bodies: attitude, mass, shapes and fixtures", `decision 47: Rigid bodies of the places`), on rails and promoted alike; immovable in contacts (infinite mass), so a pilot never changes their orbits.
- Berthed (`decision 33: Grapple tolerances, contact and berthing`): the ship is not integrated; each step it is placed at its station's fixture, with the pose relative to the fixture fixed at the latch: r = r_st + q_st p, v = v_st + ω_st × (q_st p) (ω_st in inertial axes), q = q_st q_rel, ω = ω_st in ship body axes. The station is within 100 km, so it is promoted and integrated: the ship still rides an integrated trajectory, never Kepler rails (`decision 19`). Engine and RCS are locked; propellant changes only by transfer (10 kg/s, `decision 26: Refuelling and economy`); hull temperature and consumables run on; warp follows the coasting rules.
- Release from a berth: the ship's state is the held one at that step boundary, and it is integrated from there with the station's velocity at the fixture point; contacts with the station are ignored until the ship's boxes no longer overlap its shapes ("Collisions and the Sun").
- A towable load, grappled, leaves the objects and becomes part of the ship's stack (`ship.lisp`): its pose in ship body axes is fixed at the capture, and the integrator takes the stack's mass, CoM and tensor ("Integrator"). Released, it becomes a promoted object again with the stack's motion at its own CoM (v = v_cm + ω × (r_o − r_cm), the stack's ω), and is ignored by contact until the two separate, like a berth release.
- Rails across spheres of influence, hyperbolic rails and the removal of objects (a crash sphere met, a delivery) follow below; they are `decision 46: Rails across spheres, hyperbolas and re-entry`, which refines `decision 19`'s rails (`docs/design.md` "Asteroids" and "Earth-orbit points of interest" need them).

### Hyperbolic rails
- Rails elements cover any conic: ellipses as above and hyperbolas (e > 1) for objects passing a body, such as the passing rock past Earth (`docs/design.md` "Asteroids": v∞ 0.5–1.0 km/s, perigee radius q = 7,378 km, so e = 1 + q v∞²/μ = 1.0046–1.0185 and a = −μ/v∞² = −1.59×10^9 to −3.99×10^8 m). `orbits.lisp` converts both ways for e > 1 (`milestone 4`).
- Stored as q, e, i, Ω, ω and the time of periapsis passage t_p: unlike a and M, these stay finite as e → 1.
- Propagation: n = √(μ / (−a)³), M = n (t − t_p), the hyperbolic Kepler equation M = e sinh H − H solved by Newton from H₀ = sign(M) ln(2|M|/e + 1.8) to 1e-14 relative in H; exact for any t, like the elliptic case.
- Near-parabolic forms, so a hyperbola with e − 1 ~ 0.005 keeps its digits at perigee: M = (e − 1) sinh H + (sinh H − H), with sinh H − H from its series (H³/6 + H⁵/120 + …, eight terms) for |H| < 0.5; cosh H − 1 = 2 sinh²(H/2); the perifocal position as x = q (1 − (cosh H − 1)/(e − 1)), y = q √((e + 1)/(e − 1)) sinh H, never as a × (a small difference).
- A state → elements with |e − 1| < 1e-6 (a release at almost exactly escape speed) is nudged to e = 1 ± 1e-6, keeping its side, by scaling the speed: a change below 1 cm/s. No parabolic case exists in code.
- Pure two-body: J2's secular rates are orbit averages and apply only to Earth-centred ellipses; a pass lasts days, and within 100 km of the ship the object is integrated anyway.
- Display: a hyperbola has a periapsis, no apoapsis or period (the instruments show "—"); the map draws the branch from its segment's start to its end.

### Rails across spheres of influence
- An object on rails holds a list of segments: start and end time, central body, elements, and how the segment ends (leaving the body's sphere, entering a child's sphere, meeting the crash sphere, or the horizon). `place-state(t)` finds the segment containing t by binary search and propagates its elements: still a pure function of t, returning the state relative to that segment's body; callers bring it into their frame through the ephemerides in double, as for bodies ("Floating-origin pitfalls").
- Spheres as the ship's: the Laplace sphere r = a (m/M)^(2/5) (`decision 15: Coordinate frames`; Earth ~9.25×10^8 m, Moon ~6.6×10^7 m). Rails switch at the exact boundary, without the ship's 2 % hysteresis: segments are built once, not per step, so nothing flips; a grazing pass gives two short segments.
- Switch at t_s: the state from the old segment at t_s, plus the old origin's and minus the new origin's position and velocity from the ephemerides at t_s, in double, then state → elements around the new body; the same transform as the ship's re-centring ("Frames").
- Built once per object, in `physics.lisp`, from one element set around one body at an epoch: the definition in `places.lisp`, or the osculating elements at a demotion or a release. Forward to the end of the ephemerides' range (2050-01-01, `decision 18: Ephemerides`) and, for a definition, also backward to the start date (time reversed). Each segment ends at the first of:
  - leaving the central body's sphere: a radius crossing of the conic, in closed form (as the crash crossing below);
  - entering a child body's sphere (the Moon's from Earth's, Earth's from the Sun's): the relative distance to the child's ephemeris, sampled at the coarse step rule against the child ("Integrator"), each interval tested by the swept-sphere test of the crash test (cubic Hermite of the relative states, chord's closest approach minus the sag bound, "Collisions and the Sun") against the sphere's radius, a hit refined by Newton to 1e-6 s;
  - meeting the central body's crash sphere ("Removal of objects").
- Fast path: a conic that cannot reach a child's sphere (apoapsis below the child's least distance minus its sphere) and stays inside its body's sphere is one segment with no search; every Earth-orbit place is (Highport's apogee radius 76,378 km against the Moon's ≥ 356,000 km minus 66,000 km).
- At most 16 segments per object; past that the last segment runs to the horizon and the log warns. The passing rock has three (heliocentric, a geocentric hyperbola, heliocentric), built at load in well under 1 ms.
- The passing rock, 2040 BQ7, defined by its geocentric hyperbola (perigee 2040-03-12 12:00 UTC, `docs/story.md`): it enters Earth's sphere 7.2 days before perigee at v∞ 0.5 km/s (6.0 days at 1.0 km/s; the hyperbolic Kepler equation from r = 9.25×10^8 m), passes perigee at 10,407–10,443 m/s (√(v∞² + 2μ/q)) and leaves symmetrically. Grappled, it leaves the rails for the stack ("Rails").
- Promotion takes the state from the segment at t, as before; demotion builds a new list from the osculating elements at t, around the object's dominant body by the ship's rule without hysteresis, forward only. A changed list is part of the simulation state ("Checkpoints"); an object's definition is not, as it rebuilds bit for bit.
- J2's secular rates only in Earth-centred elliptic segments; heliocentric, lunar and hyperbolic segments are pure two-body.
- Display (`ribbons.lisp`): the map draws the current segment's conic over its time span, around its body; the next segment from its own body.

### Removal of objects
An object leaves the simulation in one of two ways: it meets a crash sphere (re-entry), or a place takes it in (delivery). Both end in the same removal.

- Re-entry: an object whose path meets its central body's crash sphere (radii from `decision 34: How death works`: Earth R + 100 km, the entry interface) is removed at the crossing: it burns up (Earth) or hits the ground (bodies without air). Drag stays out (`decision 40: Earth's look and drag`), so this is the only way an orbit ends; it is how Derelict K-17's job ends (`docs/design.md` "Earth-orbit points of interest").
- Re-entry on rails: the end of its segment, in closed form. J2's secular rates leave a and e unchanged, so the radius is the conic's: the inbound crossing of r_c has true anomaly θ_c = −arccos((p/r_c − 1)/e), and through its eccentric (or hyperbolic) and mean anomaly, the segment's own mean-anomaly rate (n plus J2's Ṁ) gives t_c, to 1e-6 s. A segment with periapsis radius ≥ r_c has none.
- Re-entry promoted (within 100 km of the ship, as the derelict is right after its release): `hazards.lisp` runs the ship's crash test on each promoted object after each step, the same function (swept test on the cubic Hermite, Newton refinement, "Collisions and the Sun"); the impact time is the removal time.
- Delivery (`decision 53: Deliveries, the stranded offer, checkpoint floors and Skua Yard's layout`; `docs/design.md` "Towing", `docs/story.md` "Rules for every quest"): `hazards.lisp`, at the step a towable load is released, checks its CoM against every receiving fixture of a promoted object (a fixture flag from `places.lisp`: Highport's plant, Peary Depot's berth): within 100 m of the pin and at most 0.05 m/s relative to the object's velocity at the pin gives a pending delivery (load, fixture, release time) in the simulation state. After each step the load must be within 150 m of the pin; at release + 600 s, a scheduled time on the 1/64 s grid that ends a coarse step like a burn's start, it is removed with cause `:delivered`. Leaving 150 m, a new grapple, or the receiving object's removal clears the pending delivery. The load may be on rails meanwhile (the ship left beyond 200 km): the check reads `place-state`.
- Removal: the object is marked removed in the simulation state, with time, body, cause (`:burned-up` for Earth's crash sphere, `:impact` for a body without air, `:delivered`) and point (latitude and longitude, body-fixed, for a crash; the receiving place and fixture for a delivery); from then it is not drawn, promoted, tested for contact, an event in the prediction or a target (the target is cleared). Its id stays, so `quests.lisp` can ask whether, when and how it was removed; `place-state` of a removed object after t_c returns nil.
- The player learns it by a SHIP message ("K-17 BURNED UP", "SKUA YARD 3 DELIVERED"; `docs/design.md` "Messages and radio") at the removal, at any warp level: not a warp drop-out (`decision 43: Warp drop-outs for contact and life support` stands); the map marks a crash's point.
- Derelict K-17 released at apogee with an 800 × 90 km orbit: a = 6,823,136.6 m, e = 0.052029, period 93.48 min; it crosses R + 100 km at θ = −14.35°, 2,603.07 s after apogee and 201.4 s before perigee (the test's reference).

### Story ships' schedules
THRIFT and TANKER 3 move between parking slots (`milestone 5`, "should"; `docs/story.md` "Other ships", `docs/design.md` "Earth-orbit points of interest" and "The Moon"): they leave one slot and appear in the next, never drawn in transit, and each switch waits while the player's ship is within 200 km of the slot (`decision 27: Other ships`). The lasting choice is `decision 58: Story ships' slots at Fulmar Depot and Peary Depot, schedules and clear plans`. Code: `places.lisp` (schedules), `clock.lisp` (the rule), `quests.lisp` (the messages).

- Schedule (`places.lisp`): an ordered list of stays (slot, from, until); a slot is a body, elements at the epoch and the inertial attitude, defined as a place is; times are UTC on the 1/64 s grid; the first stay is from the start date, the last has no until; between one stay's until and the next one's from is the transit gap.
  - THRIFT: Highport's orbit +5 km (start – 2040-01-21 06:00); Peary Depot's orbit, u₀ + 180° (2040-01-26 06:00 – ).
  - TANKER 3: Highport's orbit +2 km (start – 2040-01-21 06:00); Fulmar Depot's orbit, +2 km (2040-01-22 06:00 – 2040-03-06 06:00); Highport's orbit +2 km (2040-03-07 06:00 – ).
- Switch record, in the simulation state: per story ship, the actual times of its switches so far (a departure, then an arrival, per gap; at most four). An even count means in its slot (the stay after the last arrival), odd in transit; restored with checkpoints and written by saves ("Checkpoints").
- `place-state(t)` of a story ship is a pure function of t and its record: in a stay from its actual arrival to its actual departure (or on, if not yet departed), the stay's rails segments, built from the slot's definition like a place's ("Rails across spheres of influence"; a demotion's osculating list replaces them for that stay); in transit nil. In transit it is not drawn, on the map, promoted, tested for contact, a prediction event or a target, like a removed object, but its id stays.
- The rule (`clock.lisp`, after each step, after `hazards.lisp` and demotion, before `quests.lisp`): a ship whose next switch is due (its scheduled time ≤ t) switches at t if the stack's CoM is more than 200 km from the slot concerned at t (a departure: the ship's own `place-state`; an arrival: the next slot's rails), both brought into one frame through the ephemerides in double; otherwise nothing, and it is tested again after the next step.
  - Scheduled times are clock times on the grid that end a step (as a delivery's release + 600 s, "Removal of objects"), without a warp drop-out: an undeferred switch lands exactly on its time at every warp; a deferred one at the end of the first step beyond 200 km, so the same for the same controls and warp per step ("Determinism").
  - Beyond 200 km a ship is on rails (demoted), so a departure never removes an integrated object; an arrival is at least 200 km from the player, so never promoted at once. Nothing appears or vanishes in view.
  - Example: berthed at Highport on 2040-01-21 06:00, the player is 2–52 km from both slots, so THRIFT and TANKER 3 wait, and D2 and D3 with them, until the ship leaves 200 km.
- Prediction and planning (`prediction.lisp`, the plan check of `docs/design.md` "The terminal path and crowded targets"): for t ahead of the simulation, the same rule on the prediction's own steps and predicted path; it may switch a coarse step apart from the flight, where both are beyond 200 km and nothing depends on it.
- Messages (`quests.lisp`): D2 at THRIFT's first departure, D3 at TANKER 3's first departure, 7f's "THRIFT's arrival at the Moon" at THRIFT's first arrival (`docs/story.md` "The quest line"), all from the record's actual times; the light time from the sender's position then. Without moving ships (the "should" dropped) there is no schedule rule: those messages use the scheduled times and THRIFT's scheduled slot (`docs/story.md` "If a 'should' is dropped").
- Cost: two distance tests per step, only while a switch is due.

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
  - entering a Sun hazard zone: within 0.551 AU of the Sun, the 350 K warning distance of `decision 34: How death works`;
  - a predicted contact with an object: the stack's bounding sphere and the object's meeting on the prediction, for promoted objects and objects on rails alike (rails positions are good to metres, enough to stop in time), except the station the ship is berthed at and an object excluded after a release; the box test that decides the contact runs at ×1 ("Collisions and the Sun");
  - the life-support alarms at 30 and 7 days left and the end of consumables: times in closed form from the consumables and the crew, rounded up to the 1/64 s grid, scheduled like a burn (no lead time); recomputed when consumables are restocked.
- The last two and the Sun's distance come from `decision 43: Warp drop-outs for contact and life support`, which extends `decision 19`'s list of events and keeps its rule.
- Warp up is refused while thrust is on above the thrust limit, while rotating without a hold, or with an event less than 3 s real time away at the new level.
- Changing warp never changes the state; only the step size changes, at a step boundary.

### Collisions and the Sun
- Crash spheres: one per body, radius R + h_crash in its frame; defaults h_crash = 0 for all bodies and 100 km for Earth (entry interface: a ship without a heat shield does not survive entry); the design picks the radii and what a crash means. Bodies are spheres (`decision 14`).
- Swept test after each step, against the central body and every third body with a crash sphere (the Moon from Earth's frame): the step's path relative to the body is the cubic Hermite of the two relative states; the chord's closest approach minus the bound h² |a|_max / 8 on the curve's sag is compared with the radius, and only then refined (Newton on d|p(s)|²/ds over the Hermite). With the coarse step rule the sag is ~r / 500,000, so no warp level can tunnel through a body or skip a periapsis below the surface.
- An impact gives its exact time (on the Hermite) and the point in the body-fixed frame (latitude, longitude) for the crash view.
- Crash radii and what follows are the design's (`decision 34: How death works`, `docs/design.md` "Dying and what follows"): Earth R + 100 km, Moon R, Venus R + 200 km, Mars R + 125 km, gas giants 1,000 km above the 1-bar radius, the Sun at R☉.
- Sun (`milestone 4`): the flux S = L☉ / (4π d²), L☉ = 3.828×10^26 W (IAU 2015 B3), 1,361 W/m² at 1 AU, from the heliocentric distance at the step's start and held over the step, zero in a body's shadow (cylindrical shadow of Earth, Moon, planets). The plain plate, T = (α S / (2 ε σ))^(1/4), gives 331 K at 1 AU for α = ε; the design's rule (`decision 34`) adds the cab's heat and its paint:
  - Hull temperature T, a scalar of the simulation state outside RK4, relaxing towards T_eq = ((α S + q) / (2 ε σ))^(1/4) with α = 0.30, ε = 0.85, q = 100 W/m², σ = 5.670374×10⁻⁸ W m⁻² K⁻⁴ (269 K at 1 AU, 179 K in shadow); per step exactly, T ← T_eq + (T − T_eq) e^(−h/τ), τ = 900 s. A new game starts at T_eq.
  - Thresholds: warning 350 K (T_eq there at 0.551 AU), danger 400 K (0.415 AU), death 450 K (0.326 AU). A crossing inside a step has its time in closed form, s = τ ln((T − T_eq) / (T_th − T_eq)); death stops the simulation at it. The warp drop-out is by distance, entering 0.551 AU ("Time warp").
- Contact with objects (`milestone 3`; `decision 33: Grapple tolerances, contact and berthing`, `decision 44: Contact, capture and berthing in the simulation`), after each step and the crash test, against every promoted object (within 100 km) not excluded by a recent release:
  - Shapes: the ship's three boxes in body axes (truss with tanks and engine 17.0 × 3.6 × 3.6 m, cab 4.5 × 3.0 × 3.0, grapple cone 1.6 × 1.6 × 1.6; `docs/design.md` "Grappling and berthing"), plus a grappled load's shapes; objects' spheres or boxes from `places.lisp`; each with a bounding sphere about its CoM.
  - Broad phase: the step's relative path (object to stack CoM) as the cubic Hermite of the two relative states; the chord's closest approach minus the sag bound h² |a_rel|_max / 8 against the sum of the bounding radii, as for crash spheres. Most steps end here.
  - Narrow phase: the step is cut into substeps so that relative motion, translation plus |ω| times the bounding radius for each body, is at most 0.25 m per substep (no tunnelling through a 1.6 m box); positions from the relative Hermite, attitudes by slerp. At each substep:
    - Grapple first, while armed: the cone's tip (16.6 m from R along +x) crossing a fixture's plate within 0.8 m of its pin is a grapple contact; the tolerances (closing speed 0.01–0.30 m/s, lateral offset ≤ 0.30 m, angle ≤ 10°, relative rotation ≤ 0.5°/s) are evaluated at the crossing.
    - Then boxes and spheres: separating axes (15 for two boxes; closest point for a sphere); the first overlapping substep is bisected to the first overlap time t_c within 1e-4 s; the contact normal n is the axis of least penetration there, the contact point the midpoint of the two shapes' deepest points along it.
    - Only approaching contacts count: the relative velocity of the two contact points (v + ω × r for each body) along n is negative; touching or separating shapes are ignored, so a stack resting against a station does not chatter.
  - Response, at the 1/64 s grid point at or before t_c: the step is redone, shortened to end there (as a step is shortened to land on a scheduled event, "Integrator"), so `t` stays on the grid; the gap left is at most 1.6 cm at 1 m/s. Normal, point and speeds are taken at t_c.
    - Capture, towable fixture: a perfectly inelastic coupling into one stack at the pose of the moment: m = m_s + m_o; r and v of the stack's CoM by mass; angular momentum about the new CoM, L = Σ (I_i ω_i + m_i (r_i − r_cm) × (v_i − v_cm)) in inertial axes, kept, ω = I_stack⁻¹ L with the stack's tensor by parallel axes. The claws' 2 s are a state of the grapple (release locked), not a motion.
    - Capture, berth fixture: the ship takes the fixture point's velocity and the station's rotation and is held there ("Rails"); the station takes nothing.
    - Missed capture or other contact at a normal speed of at most 1.0 m/s: a frictionless impulse along n with restitution 0.3, j = −1.3 v_n / (1/m_s + 1/m_o + n · ((I_s⁻¹ (r_s × n)) × r_s) + n · ((I_o⁻¹ (r_o × n)) × r_o)), applied to both bodies' v and ω; a berth object's terms are zero (immovable).
    - Above 1.0 m/s: death at t_c (state from the Hermite, for the LOST panel), the cause, object and speed recorded.
  - Excluded: the object the ship just released (a berth or a load) until the ship's boxes and its shapes no longer overlap; objects on rails (beyond 100 km) are only seen by the prediction's bounding-sphere event ("Time warp").
  - Cost: one bounding-sphere test per promoted object per step (a few at most: at Skua Yard its six rocks, the rock mover, and Highport with THRIFT and TANKER 3 30–35 km away); the narrow phase only in steps where spheres meet, a few dozen box tests.
- Life support (`milestone 4`, `decision 34`): consumables c in person-days, 180 when full, a scalar of the simulation state; per step c ← c − n_crew h / 86,400 exactly, restocked while berthed at a depot or station; the alarms at 30 and 7 days and death at 0 are scheduled events at their closed-form times, so death lands on the grid.

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
- Replays and scripted runs record controls and warp keyed by step index (`t` on the 1/64 s grid), not by frame; flight scripts use the same keying ("Scripted runs").
- A function redefined in the live image changes the trajectory from that step on, as expected; tests run in a fresh image.

### Checkpoints
- The simulation state is one object in `clock.lisp`: `t`, the dominant body, the ship's state, controls, grapple state, load and its pose, berth, hull temperature, consumables, target, the promoted objects' states, changed rails segment lists and removed objects, the story ships' switch records ("Story ships' schedules"), later quest state; the clock's accumulator and warp level are not part of it. The same list `saves.lisp` writes (`decision 35: Saving and audio`).
- A checkpoint is a deep copy of it, taken only in a safe state (coasting, no impact or contact predicted, hull below the warning; `docs/design.md` "Saving"), with the throttle off. Restoring copies it back; from there the simulation runs bit for bit as it did from the original ("Determinism").
- Quest floors (`milestone 5`, `decision 53: Deliveries, the stranded offer, checkpoint floors and Skua Yard's layout`, `docs/design.md` "Saving", `docs/story.md` "Rules for every quest"): the quest state in the simulation state holds the current step's Δv floor (v_f, m_L); the checkpoint function in `clock.lisp` reads it from there (no load dependency on `quests.lisp`) and declines while the ship's Δv remaining, v_e ln((m_dry + m_L + m_p) / (m_dry + m_L)) from `ship.lisp`, is below v_f; m_L is the step's load mass or, if it names none, the latched load's. Every checkpoint is vetoed alike: a step's start, berthing, a completed plan, a quest's own. Floors are 0 without a quest.
- The stranded offer (`docs/design.md` "Dying and what follows") restores the latest checkpoint by the same function as the LOST panel's Return.
- Before `milestone 5` checkpoints live in memory only (`decision 34: How death works`): `milestone 2` keeps the start state; `milestone 3` adds one after a completed autopilot plan and on berthing; death's Return restores the latest. From `milestone 5`, `saves.lisp` writes the same snapshot to `checkpoint.sexp`, and quests take one at each step.

## Testing

Physics is tested headless, in the same SBCL process as the rest of `scripts/test`, against bounds set from the drift measured above with a margin of ten or more; rendering is tested by one frame on a hidden canvas with image checks; flights are driven by scripts that end in screenshots, which is how an agent demonstrates a milestone. Simulation files never call the HAL or SDL (`decision 12: Module layout and package split`), so every test below except the GPU and flight-screenshot ones runs without a canvas. The bounds and the flight scripts are `decision 21: Physics test bounds and flight scripts`, the scripts' pilot actions, start states and the quest play-through `decision 56: Flight-script actions, start states and the quest play-through`. How to build, run, test and screenshot is in `AGENTS.md` "Commands", not repeated here.

### Rules for tests
- One `tests/<module>.lisp` per file with logic, in `luv-in-space.tests`, listed in `luv-in-space/tests` in load order ("Files in load order").
- Each test is created by the task that creates or changes the code it checks; the bounds below are the defaults, and a task that loosens one records why in the test's comment.
- Time budget: the whole `scripts/test` stays under ~15 s on this machine (lavapipe frame included); physics tests together under ~5 s. Runs are sized to fit (step counts below assume ~50 ns per point-mass step, ~200 ns per full ship step, "Integrator").
- Tests run in a fresh image, never read a wall clock, and compare simulation results bit for bit where determinism promises it ("Determinism"), with tolerances elsewhere.
- Reference values come from closed forms (Kepler's equation, the rocket equation) or committed constants with their source; never from a previous run of the same code.

### Drift bounds
The ship is never on rails (`decision 17: Gravity model`, `decision 19: Time-warp rules`), so the drift tests cover the two step rules the ship flies with, the fine step (×1, ×4) and the coarse step (above ×4), rather than a rails mode. They run two-body: Earth as a point mass, no J2, no third bodies, since energy is not conserved in a moving frame with perturbations.

- Setup: μ = 3.98600435507×10^14 m³/s² (`units.lisp`), periapsis radius 6,778 km, i = 51.6° (a test orbit, inclined so no axis is special; the polar start orbit is tested under "Rails, orbits and frames"), Ω = 30°, ω = 40°, starting at periapsis; no thrust, no torque, attitude integrated alongside (it must not disturb the orbit).
- Measured: relative energy error |E − E₀| / |E₀| with E = v²/2 − μ/r, relative angular-momentum error |L − L₀| / |L₀| with L = r × v (vector norm of the difference), and position error against the exact Kepler solution at the same `t` (`orbits.lisp`), all in double; sampled at every whole period and the maximum kept.
- Through the integrator directly (`physics.lisp`, step rule from `clock.lisp`):

| Orbit | Step | Orbits | Steps | ΔE/E < | ΔL/L < | Position error < |
|---|---|---|---|---|---|---|
| circular | fine, 1/64 s | 10 | 3.6×10^6 | 1e-11 | 1e-11 | 0.05 m |
| e = 0.5 | fine, 1/64 s | 2 | 2.0×10^6 | 1e-11 | 1e-11 | 0.05 m |
| circular | coarse (η = 1/256, 2 s) | 1,000 | 2.8×10^6 | 1e-10 | 1e-10 | 5 m |
| e = 0.5 | coarse | 1,000 | 2.5×10^6 | 1e-9 | 1e-9 | 100 m |
| e = 0.9 | coarse | 200 | 8.7×10^5 | 1e-8 | 1e-9 | 5 km |

- The bounds are ~10× the measured values of "Measured drift" (the fine-step rows from the 100-orbit run, which bounds a shorter one); e = 0.9 runs 200 orbits to stay in the time budget, against a 1,000-orbit measurement.
- Through the clock at each warp level (`tests/clock.lisp`): the clock is fed simulated frames (elapsed 1/60 s), and the state at each step must equal, bit for bit, the direct integrator run above at the same `t`:
  - ×1 and ×4: the fine step, over 600 s of simulated time (36,000 fine steps at ×1).
  - ×10, ×100, ×1,000, ×10,000, ×100,000, ×1,000,000: the coarse step, over 10 circular orbits each; ×1,000,000 hits the 4,096-step cap every frame, and the test checks the cap holds and the trajectory is unchanged ("The clock").
  - The same run with frames of 1/144 s and with irregular elapsed times (a fixed pseudo-random list in the test) gives the same states: the frame rate never changes a trajectory.
- Rotation: a torque-free asymmetric body (I = diag(1, 2, 3)×10^4 kg m², |ω| = 0.1 rad/s) for 1,000 s at the fine step keeps rotational energy and |I ω| (in inertial axes) within 1e-9 relative and |q| within 1e-12 of 1.
  - The same with a non-diagonal tensor, I = R diag(1, 2, 3)×10^4 kg m² Rᵀ with R a 10° rotation about body y (I_xz ≠ 0, as the Mule's), same ω and bounds.
- Thrust (`tests/ship.lisp`): a constant-thrust burn in field-free space matches the rocket equation, Δv = I_sp g₀ ln(m₀/m₁), within 1e-9 relative; a burn that empties the tanks ends with m_p = 0 exactly and the ship's mass at its dry mass; a burn under the 1 s thrust cap above ×4 matches the same burn at the fine step within 1e-6 relative in Δv.

### Rails, orbits and frames
- Kepler round trip (`tests/orbits.lisp`): state → elements → state reproduces position and velocity within 1e-9 relative, for circular equatorial, circular polar, retrograde (i = 150°), e = 0.5, e = 0.9 and a hyperbola (e = 1.5), each at several anomalies; elements → state → elements likewise, through the non-singular forms where the classical angles are undefined.
- Hyperbolic rails (`tests/orbits.lisp`, "Hyperbolic rails"): for the passing rock's hyperbolas (q = 7,378 km, e = 1.0046 and 1.0185) and e = 1.5 and 3:
  - Round trips state → elements → state and elements → state → elements within 1e-9 relative at H = 0, ±0.01, ±1 and at Earth's sphere radius; propagating t₀ → t₁ → t₀ returns the state within 1e-9 relative.
  - Kepler's equation's residual below 1e-14 relative for |M| from 1e-8 to 1e3; the speed at perigee √(v∞² + 2μ/q) (10,406.7 m/s at v∞ 0.5 km/s) within 1e-12 relative; the time from Earth's sphere to perigee 7.187 days at v∞ 0.5 km/s within 1 s.
  - Rails against the integrator (coarse step, two-body) from the sphere to perigee: within 100 m (an unmeasured default; the task that writes it measures and records the bound).
  - A state with |e − 1| < 1e-6 gives e = 1 ± 1e-6 on its side, the speed changed by less than 1 cm/s.
- Rails across spheres (`tests/physics.lisp`, "Rails across spheres of influence"):
  - The passing rock's list has three segments, Sun → Earth → Sun; at each switch the two segments' states at t_s, brought into one frame in double, agree within 1e-2 m and 1e-6 m/s, and the switch lies on the sphere within 1 m; sampled 1/64 s either side, position and velocity are continuous (difference equal to velocity × interval within 1e-2 m).
  - The geocentric segment's perigee time and radius equal the definition's within 1e-6 s and 1e-3 m; building the list twice gives it bit for bit; the forward and backward builds meet at the definition's epoch.
  - `place-state(t)` is the same at a t whichever t was asked before (pure); an Earth-orbit place (Highport) has one segment and skips the child search; a geocentric ellipse reaching the Moon's sphere gets a Moon segment between two Earth ones (crafted case).
- Re-entry (`tests/hazards.lisp`, "Removal of objects"), Derelict K-17 released at apogee into 800 × 90 km:
  - On rails (two-body): removed 2,603.07 s after apogee (the closed form, θ = −14.35°) within 1e-6 s; with J2's secular rates, at the time from the rates.
  - Promoted, integrated two-body from apogee at the fine and at the coarse step: removed within 1e-2 s of the closed form, the point within 0.01°, like the ship's crash test.
  - After removal: not drawn, not a target, no contact or prediction event, `place-state` nil; a checkpoint restored before t_c brings it back and it burns up at the same t_c bit for bit; an orbit with perigee 1 m above the crash radius is never removed.
- Kepler propagation against the integrator: the same orbit propagated on rails and integrated (coarse step, two-body) for 10 orbits agrees within the coarse position bound above.
- Rails with J2 (`tests/clock.lisp` or `tests/places.lisp`): an object on rails with J2 secular rates and the integrated ship with J2 alone, from the same state in a 400 km, 51.6° orbit, agree on the node Ω within 0.05° after one day (short-period terms averaged over the last orbit); Ω̇ matches the closed form −(3/2) n J2 (R/p)² cos i within 1e-9 relative. The polar start orbit (`decision 38: Start orbit: polar`): its closed-form |Ω̇| is below 1e-15 rad/s, and the ship integrated with J2 alone for 7 days keeps its node within 1e-6° of 270° (J2's force lies in a polar orbit's plane).
- Promotion and demotion (`decision 19`): promoting an object at `t` and demoting it in the next step moves it by less than 1 mm; an object promoted beside the ship stays within 1 m of a reference integration after 1 hour.
- Frame transforms (`tests/physics.lisp`, `tests/bodies.lisp`; the quaternion tests in `tests/math.lisp`):
  - Re-centring Earth → Sun → Earth at a step boundary returns the LEO state within 1e-3 m and 1e-6 m/s (heliocentric resolution ~3e-5 m, "Units").
  - Frame independence: a translunar coast integrated with re-centring (Earth → Moon → Earth) and one kept in Earth's frame throughout agree within 10 m at the Moon's sphere boundary; the hysteresis keeps a ship parked on the boundary from re-centring more than once.
  - Earth Rotation Angle at J2000.0 (UT1) is 0.7790572732640 rev, and advances by 1.00273781191135448 rev per UT1 day; body-fixed x lands on the texture's centre column (u = 0.5, v = 0.5) and the north pole on v = 0 ("Frames").
  - Quaternions and FRD (`tests/math.lisp`): body → inertial → body round trips within 2e-15 (two rotations of a unit vector reach ~5 ulp, 1.2e-15, per component); a positive pitch rate turns the nose from +x towards −z (body); a positive yaw rate towards +y; a positive roll rate +y towards +z.
  - Projection (`tests/camera.lisp`): a point straight ahead at distance d gets depth 0.1/d, a point to the right positive clip x, a point below positive clip y on Vulkan (`decision 4: Depth strategy`); `h (2r + h)` agrees with |c|² − r² computed in rationals within 1e-12 relative, and the single-float difference does not.
- Ephemeris spot checks (`tests/bodies.lisp`, `decision 18: Ephemerides`):
  - A handful of JPL Horizons vectors committed as constants in the test file, each with its query in a comment (target, centre, ICRF axes, TDB, the date fetched): the Earth–Moon barycentre, Mars and Jupiter heliocentric, and the Moon geocentric, at three dates within the game's range (e.g. 2026-01-01, 2040-01-01 and 2049-06-01 TDB).
  - Bounds from the models' stated accuracy: planet and barycentre directions within 1′ (Mercury to Mars, the barycentre) or 15′ (Jupiter, Saturn), distances within 1e-4 relative; the Moon's direction within 1′, its distance within 50 km.
  - The cache: Hermite-interpolated positions match the series within 2 cm (Moon), and a node is the same bit for bit whether computed first or after others ("Ephemerides"); the start date lies inside 1800–2050.
- Units (`tests/units.lisp`): the calendar round trip (UTC date → `t` → date) for J2000.0, the default start date and a leap-year day; `t` at J2000.0 is 0.

### Warp, events and collisions
- Warp transitions without jumps (`tests/clock.lisp`): switching warp at a step boundary leaves the state bit for bit unchanged; the next step uses the new step size; switching ×1 → ×1,000 → ×1 over one orbit stays within the coarse position bound of a run that never left ×1.
- Warp refusals and drop-outs ("Time warp"): warp up is refused while thrusting above the thrust limit, while rotating without a hold, and when an event is closer than 3 s real time at the new level; at ×1,000,000 towards a periapsis below the crash radius, the clock lowers the warp so that the impact arrives no sooner than 3 s real time, and is at ×1 at the impact; likewise before an SOI change, a scheduled burn's lead time, entering 0.551 AU from the Sun, a predicted contact (a ship coasting at 2 m/s towards a promoted 50 t sphere 5 km away, at ×1,000) and the life-support alarms at 30 and 7 days (`decision 43: Warp drop-outs for contact and life support`); warp up is refused with any of them closer than 3 s real time.
- Crash detection (`tests/hazards.lisp`):
  - Radial free fall from 1,000 km: impact time within 1e-3 s of the closed form for radial Kepler motion, at the fine and at the coarse step.
  - An orbit with periapsis 50 km below the crash radius, run at the coarse step and through the clock at ×1,000,000: the impact is found on the first pass, its time within 1e-2 s of the Kepler solution, its latitude and longitude within 0.01° of the closed-form point (body-fixed, "Frames").
  - A grazing orbit with periapsis 1 m above the crash radius gives no impact; a fast hyperbolic pass through the Moon's crash sphere at the largest step allowed there is caught (no tunnelling).
  - Sun: the flux at 1 AU is 1,361 W/m² within 0.1%, zero in Earth's shadow, and the plate temperature at 1 AU 331 K for α = ε ("Collisions and the Sun"); the hull-temperature update is exact for constant flux (one step of h equals 2^k steps of h/2^k within 1e-12).
  - The design's hull rule: T_eq is 269 K at 1 AU and 179 K in shadow within 1 K, and reaches 350, 400 and 450 K at 0.551, 0.415 and 0.326 AU within 0.001 AU; a ship held at 0.30 AU dies at the closed-form crossing time of 450 K within 1e-6 s; at 0.60 AU it never warns.
- Contact (`tests/hazards.lisp`, `decision 33: Grapple tolerances, contact and berthing`, `decision 44: Contact, capture and berthing in the simulation`); a wet Mule at the fine step against promoted objects in a 500 km orbit:
  - Capture inside tolerance: armed, on a 50 t towable sphere's fixture axis at 0.10 m/s, offset 0.1 m, angle 5°: a capture at the touching step, the stack's mass 85,961 kg and its CoM and tensor those of `docs/design.md` "Inertia" (wet + 50 t) within 1e-9 relative for the same pose.
  - Momentum in a capture: off axis by 0.2 m, at 0.25 m/s, with both bodies rotating (0.3°/s): the stack's linear momentum and its angular momentum about the system's CoM equal those before within 1e-12 relative; kinetic energy falls.
  - Missed captures, one tolerance just outside each (0.31 m/s, 0.31 m, 10.5°, 0.6°/s), and a 150 t fixture: a bounce, the message's reason, no capture.
  - Bounce: the hull's truss box head-on into a 50 t sphere at 0.50 m/s: the normal relative speed after is 0.3 × before within 1e-9 relative, linear and angular momentum conserved within 1e-12; against a berth object (immovable) the ship's normal speed reverses to 0.15 m/s and the station's state is unchanged bit for bit.
  - Death: 0.99 m/s bounces, 1.01 m/s kills, with t_c within 1e-4 s of the closed form for straight-line motion.
  - No tunnelling: the cone's box passing a 2.7 m sphere at 100 m/s relative, 1.0 m off centre, is caught at the fine step and in one coarse step of 2 s.
  - Only approaching contacts: a stack touching an object at 0 or receding normal speed gets no impulse; after a release, no contact with the released object until the shapes separate.
  - Every contact response leaves `t` on the 1/64 s grid.
- Berthing (`tests/ship.lisp` or `tests/hazards.lisp`):
  - Captured at a berth fixture of a promoted station, then one orbit at ×1,000: the ship's pose relative to the fixture stays within 1e-6 m and 1e-9 rad of the latch pose; throttle and RCS commands have no effect; propellant changes only by transfer, 10 kg/s × t exactly until full.
  - Release: the ship's velocity equals the station's at the fixture point within 1e-12 relative; coasting 100 s after the release, the ship stays within 1 cm of where the held pose would be.
- Life support (`tests/hazards.lisp`): consumables fall by n_crew h / 86,400 per step exactly; the 30-day alarm and death at 0 land at their closed-form times on the 1/64 s grid; restocking while berthed moves the alarm events.
- Story ships' schedules (`tests/clock.lisp`, "Story ships' schedules"; `milestone 5`): a crafted story ship with two stays in a 500 km circular polar orbit (slot A at u 0°, until t_d; slot B at u 180°, from t_d + 3,600 s), the ship two-body:
  - Undeferred, the ship 1,000 km from both slots: it departs exactly at t_d and arrives exactly at t_d + 3,600 s, at the fine step and through the clock at ×1,000 (the same `t` bit for bit); `place-state` nil between, not drawn, promoted, a contact or a target; slot B's state after the arrival equals its definition's within 1e-9 relative.
  - Deferred departure: the ship on a 520 km circular orbit, 150 km behind slot A at t_d and falling back from it: at t_d and after every step while within 200 km the ship is still in slot A (`place-state` non-nil, the record unchanged); it departs at the end of the first step whose end state is beyond 200 km, the previous step's end within 200 km; likewise a deferred arrival with the ship near slot B.
  - A checkpoint restored before t_d gives the same switch times bit for bit; the record survives the saves round trip; `tests/quests.lisp`: D2 arrives at the actual (deferred) departure plus the light time, not at the scheduled time.
- Checkpoints (`tests/clock.lisp`): restoring an in-memory snapshot and running N steps gives states equal bit for bit to N steps from the original; a death followed by Return restores the latest checkpoint.
- Delivery (`tests/hazards.lisp`, "Removal of objects"), a 50 t towable sphere released by a wet Mule beside a promoted Highport (two-body, fine step and the coarse step at ×1,000): released 80 m from the plant fixture's pin at 0.04 m/s relative: removed with cause `:delivered` at release + 600 s exactly on the grid, the same `t` at both steps; at 0.06 m/s, or 110 m out, or a drift that crosses 150 m before 600 s, or grappled again at 300 s: never removed; a checkpoint restored before the removal brings the load back and it is removed at the same `t` bit for bit.
- Floors and the stranded offer (`tests/quests.lisp`, `milestone 5`): a step with a floor of 300 m/s and 2.5 t: the checkpoint at its start is taken with 4,000 kg of propellant and vetoed with 500 kg (also at berthing and a completed plan), the previous one kept; Δv remaining matches the closed form within 1e-12 relative. Stranded by a floor, by an escape orbit (e = 1.2 past perigee) and by a removed step object: the offer is sent at exactly 600 s after the condition began (plus the light time), not at all if the condition breaks at 599 s; Y restores the latest checkpoint bit for bit, as Return does.
- Prediction (`tests/prediction.lisp`, `decision 20: Trajectory prediction on the frame thread`): while coasting, the predicted state after one orbit equals the flown one bit for bit; its events (apsides, an impact, an SOI change) match those the flight then meets; a prediction split over budgeted frames equals one computed at once.
- Determinism: the same flight script ("Scripted runs") run twice gives bit-identical states at every checkpoint.

### Saves
- `tests/saves.lisp` with `XDG_DATA_HOME` pointing at a temporary directory (`docs/luv-capabilities.md` "Save files"):
  - Round trip: save, load, and every double in the state (`t`, the ship's state, propellant, quest state) is equal bit for bit (doubles printed readably with `*read-default-float-format*` bound to `double-float`).
  - Continuation: N steps after a load equal N steps without the save, bit for bit.
  - Safety: a file with two forms, `#.` read-eval, or an unknown format version is rejected with a condition; a write interrupted before the rename leaves the old save intact.

### GPU tests
- As `decision 2: Screenshots and GPU test: offscreen capture on a hidden canvas, headless on lavapipe` and `decision 10: Test and screenshot commands`: one frame (or a few) on a hidden canvas through `capture-scene`, on the GPU with a display and on lavapipe without one, and in `nix flake check`'s sandbox; only in `tests/render.lisp` and `tests/capture.lisp`.
- Image checks, never pixel-exact comparisons (lavapipe and hardware GPUs differ in the last bits, and assets or shaders change the picture): the PNG's size and signature; regions that must be lit or dark (Earth's disc at a known camera pose, the background); a range of mean luminance; no NaN-coloured or fully black frame; the count of bright pixels when only stars are in view; `report-failed-live-artifacts` returns 0 after the frame (`decision 11: Render pipelines are luv live shader pipelines`).
- Scenes for GPU tests are set from a state (camera pose, `t`, ship state), not from flying there, so each check is one frame.
- The skeleton's `tests/capture.lisp` (`offscreen-frame`) is the first of these; its centre-pixel check moves to `tests/render.lisp` with `scene.lisp` ("Files in load order").

### Scripted runs
A flight script drives the game the way a player would, through the `controls` that input and the autopilot also write ("Files in load order"), so a burn, an orbit raise, a rendezvous, a grapple and in the end the whole quest line can be flown headless, tested, and shown in screenshots by an agent. Scripts are data keyed by simulation time on the 1/64 s grid, so the runs are deterministic ("Determinism"). The vocabulary is `decision 21: Physics test bounds and flight scripts`, extended for the quests by `decision 56: Flight-script actions, start states and the quest play-through`.

#### Steps
- A script is a list of steps in a file `flights/<name>.sexp` (data, read safely as saves are: no `#.`, symbols only as keywords), run in order; each step is an action or a wait.
  - `(:at 0 :warp 1)`, `(:after 10 :hold :prograde)`, `(:after 30 :throttle 1.0)`, `(:after-dv 25.0 :throttle 0)` (cut-off by Δv delivered), `(:wait :apoapsis)`, `(:wait :periapsis)`, `(:wait :impact)`, `(:wait :altitude-below 120e3)`, `(:screenshot "after-burn")`, `(:expect :periapsis-above 400e3)`.
  - Times are relative to the end of the previous step (`:after`; a bare action is `:after 0`) or absolute from the script's start (`:at`), rounded to the 1/64 s grid; `(:after 300)` without an action only lets 300 s pass.
  - Actions are applied at their step by a per-step hook of the clock, in `run-flight` (headless) and `fly` (live) alike, so both give the same states bit for bit; a key an action presses acts on the next step, as a key pressed in play.
- Waits on the prediction's events (apsides, impact, SOI change) are exact and cheap at any warp; waits on conditions are tested after each step, after `hazards.lisp` and `quests.lisp`, so they too are exact on the step grid.
- Every wait has a limit, `:within <s>` of game time (default 30 days); a wait that runs out ends the script, failed, with the wait and the state.
- Failure: a failed `:expect` is recorded and the script goes on; a wait out of time, a refused action (the reason the game gives), a death, or a stranded offer ("Floors and the stranded offer"), unless the script expects it with `(:expect :lost <cause>)` or `(:expect :offer)`, ends the script, failed.
- Quest steps are written as strings, `"2.3"`, never as numbers (a float would read `9.10` as `9.1`).

#### Actions
Each action is what a key (or a click) does in play (`docs/design.md` "Controls", `decision 29: Control scheme: key map, control modes, gamepad`); none sets the state directly, except `:start`. In brackets the milestone that first needs it.

| Action | The pilot's keys | What it does in the script |
|---|---|---|
| `(:start <form>)` (2; 3; 5) | — | the first step only: the state the script starts from ("Start states") |
| `(:warp <level>)` (2) | . , slash | the level 1 … 1,000,000; refused as in play |
| `(:warp :coast)` (2) | . | from now on raise warp after every drop-out to the highest level the clock accepts, at most ×4 while the autopilot is in its terminal phase or keeping station, or the ship is within 1 km of an object and not berthed (close work: RCS and contacts at the fine step), and ×1 during `:approach` and `:translate`; `(:warp 1)` ends it |
| `(:hold <mode>)` (2; 3) | 1–0, R; T | `:prograde`, `:retrograde`, `:normal`, `:anti-normal`, `:radial-out`, `:radial-in`, `:inertial`; `:target`, `:anti-target`, `:rel-prograde`, `:rel-retrograde` (3); `nil` releases to rate damping |
| `(:throttle <f>)` (2) | Shift, Ctrl, Z, X | the lever: 0 or 0.1 to 1.0 |
| `(:after-dv <m/s> :throttle 0)` (2) | X | cut when the Δv delivered since the throttle went on reaches it |
| `(:keys (<key> …) :for <s>)` (2) | any | hold luv physical keys for a time, as fingers do: manual turns and translation |
| `(:fine <t/nil>)` (2) | F | fine control |
| `(:translate <axis> :dv <m/s>)` (3) | H/N, I/K, J/L | hold the translation key of `:fwd`, `:aft`, `:up`, `:down`, `:left` or `:right` until the Δv delivered along it reaches the value, at ×1 (it lowers warp); to back off after a release |
| `(:target <id>)` (3) | ] [ | cycle until the target is the place or load `id`; refused if it is removed or unknown |
| `(:autopilot <preset> <goal> … [:range <m>] [:limit <s>])` (3; 4) | O, Up/Down, Left/Right, Return, Return | pick, plan, engage; refused with the planner's reason ("NO CLEAR APPROACH", "BELOW 150 KM", …) |
| `(:autopilot :off)` (3) | Backspace | disengage |
| `(:grapple :arm)`, `(:grapple :stow)` (3) | G | press G if the grapple is not already in that state |
| `(:release)` (3) | G | release a latched load or leave a berth; refused (engine on, rotating over 0.5°/s, claws closing) fails |
| `(:transfer :on)`, `(:transfer :off)` (3) | B | propellant transfer, berthed at a depot; it stops itself when full |
| `(:approach <fixture> [:speed 0.1] [:tries 3])` (3) | F, W/S/A/D, H/N, I/K, J/L | fly the last metres to capture by hand ("The terminal approach") |
| `(:ack)` (5) | Y | acknowledge the oldest unacknowledged RADIO marked Y; if none has arrived yet, wait for it (`:within` 1 h) |
| `(:screenshot "<label>")` (2) | — | a checkpoint in the record; a PNG with `scripts/fly` |

- Presets and goals of `:autopilot` (`docs/design.md` "The autopilot"): `:circularise :ap` / `:pe`, `:set-ap <alt>`, `:set-pe <alt>`, `:hohmann <alt>`, `:plane <inc>` or `:plane :target`, `:rendezvous <place, load or fixture>` (3); `:tli <place>`, `:capture <ap>`, `:return`, and `:rendezvous` across spheres of influence or to a target on a hyperbola (4). A rendezvous whose goal is a place with several fixtures stops at its first free berth fixture unless the goal names one, `(:fulmar-depot :a)`.
- `:range` (default 100 m, up to 1 km) is the stop point's distance from the fixture's pin along its axis, for the grapple cone's tip, the point of "on station" (`docs/story.md` "Rules for every quest"); the design's clearance rule still refuses a stop point under 100 m from the target's CoM (`docs/design.md` "The terminal path and crowded targets"). With a load latched the load's CoM is nearer by its fixture-to-CoM distance (~2 m), but station keeping's ±5 m can leave it past 100 m, so for a delivery the pilot closes the last metres by hand (`close-in` under "The quest play-through"), as `docs/story.md` 6.3's job card asks.
- Fixtures are written `(<place> <fixture>)`, or a load's id for its one fixture (`:k-17`, `:pod-14`, `:skua-yard-3`, `:bq7`; `docs/story.md` "Rules for every quest" gives the ids).

#### Waits and expectations
- `(:wait <condition>)` waits until the condition holds; `(:expect <condition>)` checks it now and records a pass or a failure. A condition is a keyword and its arguments, written flat: `(:wait :quest-step "2.3")`, `(:expect :removed :k-17 :burned-up)`.
- Conditions (`h` an altitude above the dominant body's sphere, as the instruments show):
  - Events of the prediction (2; 4 SOI): `:apoapsis`, `:periapsis`, `:impact`, `:soi <body>`.
  - Orbit (2): `:altitude-below h`, `:periapsis-below h`, `:periapsis-above h`, `:apoapsis-below h`, `:apoapsis-above h`, `:dominant <body>` (4); `:crashed`; `:sun-distance-below <m>`, `:sun-distance-above <m>` (4; the stack's CoM from the Sun's centre, heliocentric, in m as everything else: 0.551 AU is `8.2429e10`).
  - Attitude (2): `:settled` (the hold reports settled), `:rate-below <°/s>`.
  - Time (2): `:date "2040-01-21"` or `"2040-01-21T06:00:00"` (UTC; a scheduled time on the grid, so a coarse step lands on it exactly, as a burn's start).
  - Ship (2; 3): `:propellant-above <kg>`, `:propellant-full`, `:latched`, `:latched <id>`, `:berthed <fixture>`, `:on-station <fixture>` (the story's term), `:removed <id> [<cause>]`; `:hull-above <K>`, `:hull-below <K>` (4; the hull temperature of "Collisions and the Sun", exact on the step grid: a wait on `:hull-above 450` ends at the death's closed-form time).
  - Clock (2; 4): `:drop-out <kind>` (a wait only: the clock dropped warp to ×1 at an event of that kind since the wait began; kinds `:impact`, `:soi`, `:burn`, `:contact` (3), `:sun-zone` (4, entering 0.551 AU), `:life-support` (4); "Time warp").
  - Autopilot (3): `:autopilot-done` (the plan's last burn flown and, for a rendezvous, on station; it then keeps station until `:off`).
  - Quests (5): `:quest-step "n.m"` (that step is the current one: given, the previous completed), `:quest-skipped <n>`, `:quest-line-done` (step 9.7, or the ending alone if the passing rock is dropped).
- Expectations only, on the run's record (3; 4):
  - `:dv-below <m/s>`: Δv delivered by engine and RCS, ∫ |F| / m dt, since the last `(:autopilot …)` engaged.
  - `:date-between "<UTC>" "<UTC>"`; `:elapsed-below <s>` since the last `(:autopilot …)` engaged.
  - `:burn <kind> :between "<UTC>" "<UTC>"`, `:since-burn <kind> :below <s>`: the plan's first burn of that kind. The autopilot labels its burns `:departure` (TLI, TEI, a Lambert departure, a Hohmann's first), `:mcc`, `:capture` (LOI, Earth capture, with any phasing merged into it), `:phasing`, `:arrival` (the braking at the aim point), `:terminal`, `:trim`; the record keeps each burn's kind, start, end and Δv.
  - `:clearance-above <m> [:except (<id> …)] [:only (<id> …)] [:until :aim-point]`: the least distance from the stack's CoM to any object's CoM (or only those listed) since the last `(:autopilot …)` engaged; `:until :aim-point` ends the span at the `:arrival` burn's start, for the target itself (the plan check of `docs/design.md` "The terminal path and crowded targets").
  - `:aim-point <n>`: the plan's aim point was candidate n of `docs/design.md` "The terminal path and crowded targets".
  - `:lost <cause>`, `:offer`: a death or a stranded offer happened (and does not fail the script); causes `:crash` (2), `:contact` (3), `:heat` and `:life-support` (4), the LOST panel's (`decision 34: How death works`).

#### Start states
- `(:start :new-game)` (2): the start state (`docs/design.md` "The start"); also when a script has no `:start`.
- `(:start :state (<key> <value> …))` (3): a stated state, built from the start state with these changes, deterministically by one function of `flights.lisp` that GPU tests use for their scenes too ("GPU tests"):
  - `:date`: a UTC string, or `(:periapsis <place> [:before <s>] [:after "<UTC>"])` / `(:apoapsis …)`: the place's first such passage after the date (default the start date), less `:before`, on the grid.
  - `:orbit`: `(:circular <alt> [:u <°>] [:lead (<place> <°>)])` in the common polar plane (i 90°, Ω 270°), with the argument of latitude u or the place's lead over the ship; `(:near <place> :behind <m>)` / `:above <m>` / `:at-slot` (the place's rails state at the date, offset along −v or radially out, at the place's velocity); `(:on-axis <fixture> <m>)` (on the fixture's axis at m from the pin, +x along its inward axis, at the object's velocity there); `(:elements <body> a e i Ω ω M)`.
  - `:berthed <fixture>`: latched at the berth pose (the cone's tip on the pin, +x along the inward axis); replaces `:orbit`.
  - `:grappled <load>`: the load latched at the capture pose; the stack's CoM goes where `:orbit` puts the ship, or the load's own CoM with `(:near <load> :at-slot)`.
  - `:propellant` (default 28,000 kg), `:consumables` (default 180 days), `:removed ((<id> <cause>) …)`, `:target <id>`; the hull temperature at T_eq of the state.
  - `:quest "n.m"` (5): quest state at that step's start, earlier quests done, the date messages before the date sent; default no quest.
  - Controls: rate damping with a prograde hold (none when berthed), throttle off, ×1.
- `(:start :after "<script>")` (5): the end state of that script. The end state of every script is its simulation state when it ends ("Checkpoints"); before the next script starts from it, it is written and read back through `saves.lisp`'s format (to a string; `tests/saves.lisp` holds the round trip bit for bit), so each link of the chain is a save, as a player's quit and Continue would be: warp ×1 and no autopilot plan. In one process the end states are kept in memory by script name; a script asked for that has not run yet in the process runs first, headless and without screenshots, and so on down the chain.
- `(:start :file "<path>")` (5): a `checkpoint.sexp` or `resume.sexp` written by `saves.lisp`; to fly on from a player's state.

#### The terminal approach
`(:approach <fixture> :speed 0.1)` is a pilot, not the autopilot (which never grapples, `decision 32: Autopilot scope and safety rules`): a closed loop in `flights.lisp` that reads only what the cockpit shows, the "Grapple range and alignment" instrument (range, closing speed, the pin's lateral offset in body y and z, angle, relative rotation) and the TARGET screen's relative velocity, and writes only key presses to `controls` at ×1, the keys of `docs/design.md` "Grappling and berthing". It is written once and serves every grapple and berth of the quests and of `milestone 3`'s tests.

- Start: the fixture within the range finder (500 m ahead of the nose, its inward axis within 30° of +x), typically the autopilot's station at 100 m; otherwise it fails, "NO FIXTURE". It disengages the autopilot (Backspace), sets ×1, fine control (F) and rate damping, and arms the grapple (G).
- Ticks every 1/16 s (4 fine steps), a quick pilot's rate; each tick holds or releases keys:
  - Attitude: pitch and yaw keys (fine, 0.5°/s) turn +x onto the fixture's inward axis, pressed while the angle exceeds 1° and released inside 0.5°; rate damping stops the turn. No roll. A station held in its orbital frame turns 0.065°/s in LEO, which this follows.
  - Lateral: the wanted lateral velocity is −offset / 60 s, at most 0.05 m/s per axis; a translation key is held for one tick when the lateral relative velocity differs from it by more than 5 mm/s (one tick of fine translation is 3 mm/s wet, 1.3 mm/s with 100 t).
  - Along the axis: close at `:speed` (0.02 to 0.25 m/s, default 0.1, inside the capture window of 0.01 to 0.30 m/s), H or N held for a tick when the closing speed is off by more than 0.01 m/s; at 20 m it stops and holds until the offset is under 0.15 m, the angle under 3° and the lateral speed under 0.01 m/s, then closes at `:speed` to contact.
- End: latched (the claws closed, 2 s) or berthed: rate damping on, fine control off, done. A missed capture (a bounce, "NO CAPTURE: …"): back to 20 m and again, up to `:tries`; then it fails with the last reason. Out of time after 2 h of game time, or a contact over 1.0 m/s (death): fails.
- Spinning loads (Derelict K-17 0.3°/s, 2040 BQ7 0.4°/s) turn about their fixture axis, so the pin stands still and the relative rotation stays under 0.5°/s; nothing changes. With 100 m at 0.1 m/s an approach takes ~20 min of game time, 77,000 fine steps.

#### Running scripts
- `flights.lisp` (milestone 2; simulation only): reads a script, applies its actions at their steps, and runs the simulation headless as fast as steps allow (`run-flight`), returning a record (`t`, altitude, apoapsis, periapsis, speed, propellant, crash state, quest step) at each `:screenshot` and `:expect`, every autopilot burn, every failure, and the end state. The same function drives a live game frame by frame (`fly`), so `./sly eval '(fly "raise-orbit")' --package LUV-IN-SPACE` shows the flight in the window. It loads last among the simulation files, after `saves.lisp` ("Files in load order"), since its actions call the autopilot, places, quests and saves; in `milestone 2` that is right after `prediction.lisp`.
- Headless, warp sets only the step (fine at ×1 and ×4, coarse above), the thrust limits, the refusals and the drop-outs; "3 s of real time" in the drop-out rule is 3 s at the nominal level. Nothing reads a wall clock.
- Screenshots: `capture.lisp` renders each `:screenshot` checkpoint's state in one frame on a hidden canvas (as `capture-scene`, with the state set rather than flown, "GPU tests"); `scripts/fly <name> <out-dir> [--from <save file>]` (milestone 2; added to `AGENTS.md` "Commands" by its task) runs a script (and, for `:start :after`, the chain before it), writes `<out-dir>/<n>-<label>.png` per checkpoint and `<out-dir>/<name>.end.sexp`, prints one line per checkpoint and burn, and exits 1 on a failure; `--from` replaces the script's `:start`.

#### Scripts by milestone
- `milestone 2`:
  - `burn`: a 10 s prograde burn from the start orbit; screenshots before and during the burn.
  - `raise-orbit`: a Hohmann-like raise from the start orbit to a higher circular one, two burns at periapsis and apoapsis; expects both apsides raised by the planned amount within 1%.
  - `deorbit`: a retrograde burn that lowers periapsis below the crash radius, then waits for the impact; expects a crash at the predicted time and shows the crash and what follows (`docs/design.md`); `(:expect :lost :crash)`.
- `milestone 3`: `preset-<place>` for each Earth-orbit place (`tern-station`, `fulmar-depot`, `k-17`, `highport`, `skua-yard`) from the start state: engage "Rendezvous: <place>", `(:wait :autopilot-done)`, expect on station, Δv within 3 % of `docs/design.md` "Earth-orbit points of interest" plus 20 m/s of terminal phase and 3 m/s for a transfer that passes the target clear, and `:clearance-above <bound + 200> :only (<place>) :until :aim-point` (bounds in `docs/design.md` "Rigid bodies: attitude, mass, shapes and fixtures": Tern Station 240, Fulmar Depot 234, K-17 209, Highport 273); `approach-tern` (from `(:on-axis (:tern-station :berth) 100)`: berthed) and `approach-k-17` (from `(:on-axis :k-17 100)`: latched, the stack's mass the ship's plus K-17's 2,500 kg); and the rendezvous checks of `docs/design.md` "Rendezvous: search, phasing and the approach", "Scripted runs":

| Script | `:start :state` | Flies | Expects |
|---|---|---|---|
| `rendezvous-fulmar-phasing` | `:date "2040-01-01"`, `:orbit (:circular 400e3 :lead (:fulmar-depot 297.96))`: 296° beyond the Hohmann's phase angle 1.96° (180° less Fulmar Depot's motion over the 2,807.6 s transfer) | `(:autopilot :rendezvous (:fulmar-depot :a))` | `:on-station (:fulmar-depot :a)`, `:elapsed-below 175620` (48 h 47 min), `:dv-below 114`, `:clearance-above 234 :only (:fulmar-depot) :until :aim-point` (the final transfer from the drift orbit passes the depot ≥ 200 m beyond its 33.7 m bound) |
| `rendezvous-skua-yard` | `:date (:periapsis :highport :before 3600)`, `:berthed (:highport :plant)`; then `(:release)`, `(:translate :aft :dv 0.2)`, `(:after 300)` | `(:autopilot :rendezvous :skua-yard-5)` | `:on-station :skua-yard-5`, `:elapsed-below 93600` (one phasing revolution of 23.7 h, the wait for perigee and the terminal phase: 26 h), `:dv-below 21`, `:clearance-above 500 :except (:skua-yard-5)` |
| `skua-yard-apogee-approach` | `:date (:apoapsis :highport)`, `:orbit (:near :skua-yard-5 :behind 1000)` | `(:autopilot :rendezvous :skua-yard-5)` | `:aim-point 2`, `:clearance-above 560 :except (:skua-yard-5)`, `:on-station :skua-yard-5` |

- `milestone 4`:

| Script | `:start :state` | Flies | Expects |
|---|---|---|---|
| `rendezvous-peary` | `:date "2040-01-19"`, `:orbit (:near :pod-14 :at-slot)`, `:grappled :pod-14` | `(:autopilot :rendezvous (:peary-depot :berth))` | `:burn :departure :between "2040-01-20T05:01" "2040-01-20T07:01"`, `:since-burn :capture :below 18000` (2 Peary periods of 7,067 s and 1 h of terminal phase), `:since-burn :departure :below 561600` (6.5 days), `:dv-below 1342`, `:on-station (:peary-depot :berth)` |
| `return-highport` | `:date "2040-01-26T12:00:00"`, `:berthed (:peary-depot :berth)`, `:removed ((:skua-yard-3 :delivered) (:pod-14 :delivered))`; then `(:release)`, `(:translate :aft :dv 0.2)`, `(:after 300)` | `(:autopilot :rendezvous (:highport :plant))` | `:since-burn :capture :below 170640` (47.4 h), `:dv-below 1345` (1,273 + 30 + 2 m/s of phasing, + 3 %), `:on-station (:highport :plant)` |
| `intercept-bq7` ("should") | `:date "2040-03-05"`, `:orbit (:near :highport :behind 1000)` | `(:autopilot :rendezvous :bq7)`, then `(:approach :bq7)`, `(:autopilot :rendezvous :skua-yard)` | first leg: `:burn :departure :between "2040-03-05T15:56" "2040-03-05T16:56"`, `:date-between "2040-03-10T20:00" "2040-03-11T02:00"` at `:autopilot-done`, `:dv-below 2101`, `:on-station :bq7`; then `:latched :bq7`; second leg: `:dv-below 536` (the capture's 495, ≤ 5 of phasing over it and 20 of terminal phase, + 3 %), `:date-between "2040-03-12T12:00" "2040-03-13T15:00"` at `:autopilot-done` |
| `sun-dive` | `:date "2040-03-24"`, `:orbit (:circular 400e3 :u 23)`, full tanks | by hand (the autopilot plans nothing inside 0.551 AU): the prograde hold, two full-throttle burns cut by `:after-dv`, 2,000 m/s at the start and 4,600 m/s centred on the next perigee, then `(:warp :coast)` | apoapsis 19,000–20,000 km after the first burn; `:dominant :sun`; `:drop-out :sun-zone` at 0.550–0.552 AU between 2040-06-03 and 06-08; `:hull-above 400` (the danger mark) at 0.405–0.416 AU; `:hull-above 450` and `(:expect :lost :heat)` inside 0.326 AU between 2040-06-23 and 06-28 |

- `sun-dive`, the test of `milestone 4`'s Done when "Flying towards the Sun kills the pilot" (`docs/design.md` "The Sun", `decision 34: How death works`), written out; its figures from a throwaway two-body script (not committed: the finite burns integrated in Earth's field, patched to the Sun at Earth's sphere, the hull rule on the heliocentric ellipse), so the task that writes `flights/sun-dive.sexp` tunes `:u` and the wait before the second burn with the flight's own integrator and keeps the expectations' ranges:

```lisp
((:start :state (:date "2040-03-24" :orbit (:circular 400e3 :u 23)))
 (:warp :coast)                         ; burns with a cut-off fly at x100
 (:throttle 1.0)                        ; the start state's prograde hold
 (:after-dv 2000.0 :throttle 0)         ; 568 s, 13,112 kg: 453 x 19,482 km
 (:expect :apoapsis-above 19.0e6)
 (:expect :apoapsis-below 20.0e6)
 (:wait :apoapsis)
 (:after 10078 :throttle 1.0)           ; half the 20,798 s period less half the burn
 (:after-dv 4600.0 :throttle 0)         ; 641 s centred on perigee; ~90 kg left
 (:wait :soi :sun)                      ; Earth's sphere left ~1.4 days on
 (:expect :dominant :sun)
 (:screenshot "leaving-earth")
 (:wait :drop-out :sun-zone :within 7776000)        ; 0.551 AU, ~73.5 days on
 (:expect :sun-distance-below 8.2578e10)            ; 0.552 AU
 (:expect :sun-distance-above 8.2279e10)            ; 0.550 AU
 (:expect :date-between "2040-06-03" "2040-06-08")
 (:wait :hull-above 350 :within 21600)              ; the warning, ~1 h later
 (:screenshot "warning")
 (:wait :hull-above 400 :within 1296000)            ; the danger mark, ~10.7 days on
 (:expect :sun-distance-below 6.2233e10)            ; 0.416 AU
 (:expect :sun-distance-above 6.0587e10)            ; 0.405 AU
 (:screenshot "danger")
 (:wait :hull-above 450 :within 1296000)            ; death, ~9.4 days on
 (:expect :lost :heat)
 (:expect :sun-distance-below 4.8769e10)            ; 0.326 AU
 (:expect :date-between "2040-06-23" "2040-06-28")
 (:screenshot "lost"))
```

  - Why near an equinox: the polar plane (i 90°, Ω 270°, normal along −x) holds every departure asymptote of the start orbit, and the dive needs one against Earth's heliocentric velocity. That velocity lies in the plane only near the equinoxes (Earth at ecliptic longitude 180° or 0°); on 2040-01-01 it is 80° out of it and no tank reaches 0.326 AU (an impulsive v∞ of 35 km/s). Near 2040-03-20 the impulsive cost is the design's (v∞ 8.94 km/s, 6,386 m/s from 400 km), rising ~10 m/s within 5 days of the equinox. 2040-03-24 rather than 03-20: on 03-20 the escape passes 8,200 km from the Moon, on 03-24 288,000 km (outside its 66,000 km sphere), with the same perihelion.
  - Why two burns: one full-throttle burn of all 28 t from 400 km (1,213 s, about a fifth of an orbit, best started at u 1°) gives v∞ 8.87 km/s and a perihelion of 0.329 AU, where T_eq is 448 K: the pilot lives. Raising the apogee first and burning the rest centred on perigee loses less to gravity: v∞ 9.20 km/s, perihelion 0.317 AU (a 0.657 AU, e 0.518, 0.2° from the ecliptic), 90 kg left; the second burn cut at 4,400 m/s instead leaves 0.329 AU, so the margin is ~200 m/s.
  - Timeline: burns 2040-03-24 00:00–06:00; Earth's sphere left 03-25 09:00; 0.551 AU 2040-06-05 13:00, 350 K an hour later; 400 K 06-16 06:00 at 0.415 AU; 450 K 06-25 17:00 at 0.3255 AU, 3.4 days before perihelion; 94 days of game time, inside the ephemerides (2050) and the 180 person-days.
  - Run time: ~1,200 steps of ≤ 1 s under thrust at ×100, ~1,300 coarse steps on the coast to perigee, ~450 to Earth's sphere and ~1,100 heliocentric (4,096 s at the sphere, 16,384 s at 1 AU, 2,048 s at 0.33 AU), plus the prediction's and a few fine steps at each drop-out: ~5,000 steps, well under 1 s headless, so it fits `scripts/test`'s budget ("Rules for tests").

- `milestone 5`: the quest play-through below.

#### The quest play-through
`milestone 5`'s Done when, "a headless test plays through the whole quest line", is nine scripts `flights/quest-1.sexp` … `flights/quest-9.sexp`, one per quest of `docs/story.md` "The quest line", each starting from the previous one's end state, and a test that chains them to the ending. Every script flies its quest as a pilot would, by the keys, the autopilot and `:approach`; no step is completed by setting state ("no shortcuts around piloting", `milestone 5`).

- Each script starts `(:start :after "quest-<n−1>")` (quest 1: `:new-game`), expects the quest's first step, flies the steps, and ends when the next quest's first step is given (quest 9: `:quest-line-done`), with expectations on the end state: the berth or the orbit `docs/story.md` gives as the next quest's "Expected state at start", and propellant at least the "Running checks" "At end" less 560 kg (2 % of 28 t).
- Quest 1, written out (the manual parts: turns by keys, burns cut by a condition):

```lisp
((:start :new-game)
 (:ack)                                ; 1.1: CAIRN OPS at t = 10 s, Y
 (:expect :quest-step "1.2")
 (:hold nil)                           ; T: release the start's prograde hold
 (:keys (:a) :for 18)                  ; yaw left 90° at 5°/s: the nose to the orbit normal (−y in FRD)
 (:wait :rate-below 0.1)
 (:wait :quest-step "1.3")
 (:hold :prograde)
 (:wait :quest-step "1.4")
 (:throttle 0.1)                       ; 10,180 N: 2.83 m/s in ~10 s
 (:wait :apoapsis-above 410e3)
 (:throttle 0)
 (:wait :quest-step "1.5")
 (:hold :retrograde)
 (:wait :settled)
 (:throttle 0.1)
 (:wait :apoapsis-below 404e3)
 (:throttle 0)
 (:wait :quest-step "1.6")
 (:screenshot "one-orbit")
 (:warp :coast)
 (:wait :quest-step "2.1")
 (:expect :periapsis-above 395e3)
 (:expect :propellant-above 27359))
```

- Quest 3, written out (the push below the autopilot's floor by hand, `docs/story.md` 3.3–3.5):

```lisp
((:start :after "quest-2")             ; berthed at Tern, 3.1 given
 (:release)
 (:translate :aft :dv 0.2)
 (:after 300)
 (:target :k-17)
 (:autopilot :rendezvous :k-17)
 (:warp :coast)
 (:wait :quest-step "3.2")           ; on station at K-17's fixture
 (:approach :k-17)
 (:wait :quest-step "3.3")           ; latched; 800 km circular
 (:hold :retrograde)
 (:wait :settled)
 (:throttle 1.0)                       ; 196 m/s with the 36 t stack, ~70 s
 (:wait :periapsis-below 90e3)
 (:throttle 0)
 (:expect :periapsis-above 80e3)
 (:wait :rate-below 0.1)
 (:release)                            ; 3.4: at apogee
 (:translate :aft :dv 0.3)
 (:wait :quest-step "3.5")
 (:hold :prograde)
 (:wait :settled)
 (:throttle 1.0)
 (:wait :periapsis-above 160e3)
 (:throttle 0)
 (:wait :quest-step "3.6")
 (:autopilot :hohmann 400e3)
 (:warp :coast)
 (:wait :removed :k-17)
 (:expect :removed :k-17 :burned-up)
 (:wait :autopilot-done)
 (:wait :quest-step "4.1")
 (:expect :periapsis-above 395e3)
 (:expect :apoapsis-below 405e3)
 (:expect :propellant-above 22310))
```

- The others, as their action sequences (`wait n.m` is `(:wait :quest-step "n.m")`; `coast` is `(:warp :coast)`, `leave` is `(:release) (:translate :aft :dv 0.2) (:after 300)`; `close-in` is `(:wait :autopilot-done) (:autopilot :off) (:translate :fwd :dv 0.05) (:after 200) (:translate :aft :dv 0.05)`, 10 m nearer, the load's CoM ≤ 93 m from the pin and still within 0.05 m/s; `let-go` is `(:wait :rate-below 0.1) (:release) (:translate :aft :dv 0.1)`):

| Script | Steps | Ends with |
|---|---|---|
| `quest-2` | `(:target :tern-station)`, `(:autopilot :rendezvous :tern-station)`, coast, wait 2.3, `(:ack)`, `(:approach (:tern-station :berth))`, wait 3.1 | `:berthed (:tern-station :berth)`, propellant > 27,105 |
| `quest-4` | `(:target :fulmar-depot)`, `(:autopilot :rendezvous (:fulmar-depot :a))`, coast, wait 4.2, `(:ack)`, `(:approach (:fulmar-depot :a))`, wait 4.3, `(:transfer :on)`, wait 5.1 | berthed at A, `:propellant-full` |
| `quest-5` | leave, `(:target :highport)`, `(:autopilot :rendezvous (:highport :plant))`, coast, wait 5.2, `(:approach (:highport :plant))`, wait 5.3, `(:transfer :on)`, wait 6.1 | berthed at the plant, full |
| `quest-6` | leave, `(:target :skua-yard-3)`, `(:autopilot :rendezvous :skua-yard-3)`, coast, wait 6.2, `(:approach :skua-yard-3)`, wait 6.3, `(:autopilot :rendezvous (:highport :plant))`, coast, close-in, wait 6.4, let-go, wait 6.5, `(:approach (:highport :plant))`, `(:transfer :on)`, wait 7.1 | `:removed :skua-yard-3 :delivered`, berthed, full |
| `quest-7` | leave, `(:target :pod-14)`, `(:autopilot :rendezvous :pod-14)`, coast, `(:wait :autopilot-done)`, `(:approach :pod-14)`, wait 7.2, `(:target :peary-depot)`, `(:autopilot :rendezvous (:peary-depot :berth))`, coast, wait 7.3, close-in, let-go, wait 7.4, `(:approach (:peary-depot :berth))`, `(:transfer :on)`, wait 7.5, leave, `(:target :highport)`, `(:autopilot :rendezvous (:highport :plant))`, coast, `(:wait :autopilot-done)`, `(:approach (:highport :plant))`, `(:transfer :on)`, wait 8.1 | `:burn :departure :between "2040-01-20T05:01" "2040-01-20T07:01"` on the way out; `:removed :pod-14 :delivered`; berthed at the plant before 2040-02-03, full |
| `quest-8` | as `quest-6` with `:skua-yard-5`, from wait 8.1 to wait 9.1 | `(:expect :quest-step "8.1")` at the start (not skipped); `:removed :skua-yard-5 :delivered`, berthed, full |
| `quest-9` | coast, `(:wait :date "2040-03-03")`, wait 9.2, `(:ack)`, leave, `(:target :bq7)`, `(:autopilot :rendezvous :bq7)`, coast, wait 9.3, `(:approach :bq7)`, wait 9.4, `(:autopilot :rendezvous :skua-yard)`, coast, wait 9.5, `(:wait :autopilot-done)`, `(:autopilot :off)`, let-go, wait 9.6, `(:target :highport)`, `(:autopilot :rendezvous (:highport :plant))`, coast, `(:wait :autopilot-done)`, `(:approach (:highport :plant))`, wait 9.7 | `:burn :departure :between "2040-03-03" "2040-03-06"`, the meeting between 2040-03-10 20:00 and 2040-03-11 02:00, `:quest-line-done` before 2040-03-16, propellant > 5,308 |

- If the passing rock is dropped (`docs/story.md` "If a 'should' is dropped"), `quest-8.sexp` ends at wait 9.7 instead of wait 9.1 (the ending follows 8.5 at once), and `quest-9.sexp` is only `(:start :after "quest-8")` and `(:expect :quest-line-done)`; the departure and meeting expectations go with it. If the story ships do not move, the scripts stay as they are: no step waits on THRIFT or TANKER 3.
- The test: `tests/flights.lisp`, test `play-through`, runs `quest-1` … `quest-9` in one fresh image through `:start :after`, each end state round-tripped through `saves.lisp` as above, and passes when every script ends without a failure and the quest line is done; it asserts that no quest was skipped, no death happened and no stranded offer was sent. A command `scripts/play-through` (`milestone 5`; added to `AGENTS.md` "Commands" by its task) runs it, prints one line per quest (script, game date at its end, propellant, Δv used, steps, run time) and exits 1 on the first failure; `nix flake check` runs it after `scripts/test`. It is not part of `scripts/test`, whose budget is ~15 s ("Rules for tests").
- Expected run time, headless at warp: the line covers ~74 days of game time (2040-01-01 to ~2040-03-15) in ~5×10^6 fine steps (the ×1 and ×4 parts, ~21 h of game time: approaches, deliveries, terminal phases, drifting away from a berth within 1 km, quest 1's manual flying) and ~5×10^5 coarse steps (coasts and berthed waits); at ~0.4 µs per fine step and ~2 µs per coarse step with promoted objects, contact tests and the prediction (from "Integrator"'s 180 ns per ship step), that is ~3 s of stepping, plus ~20 autopilot plans at 0.1–2 s each (the lunar chains and the hyperbolic search the dearest): **~15 s** in all on the developer's machine (x86_64 i5-12500H), an average warp of ~×400,000. Per quest: 1 ~0.1 s; 2, 4, 5 ~0.5 s each; 3 ~0.6 s; 6 and 8 ~1.3 s each; 7 ~5.5 s; 9 ~4 s. The task that writes it measures and records the time in `AGENTS.md`; over 60 s, it profiles before going on.
- Every quest has been played once in the running game, with screenshots (`milestone 5`'s second Done when): `scripts/fly quest-<n> <out-dir>` gives a quest's screenshots from its `:screenshot` steps, and `./sly eval '(fly "quest-6")'` flies one in the window from the chain's end state.
