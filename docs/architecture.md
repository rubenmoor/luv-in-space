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

PLACEHOLDER(33): written by `task 33: Architecture: units, coordinate frames, simulation time and rendering precision`.

## Time and integration

PLACEHOLDER(34): written by `task 34: Architecture: integrator, gravity model, ephemerides, time warp, collisions and prediction`.

## Testing

PLACEHOLDER(35): written by `task 35: Architecture: testing section with drift bounds; read-through against the other documents`.
