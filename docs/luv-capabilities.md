# What luv can carry

This document records what [luv](https://github.com/mbrock/luv) offers Luv in Space, a solar-system flight game, and where it falls short. It is the first output of `plan 7: luv capabilities`; this draft comes from reading luv's docs and code only (`task 16: Read luv's docs and systems; draft docs/luv-capabilities.md`). Every claim carries how it was checked: `(read)` for reading; later tasks of the plan change claims to `(ran)` after running them, or mark them `(assumed)`. Paths into luv are written `../luv/<path>`.

- luv revision examined: `6abc9d10ec8991c2cfe92888b84946372d6030b9` (2026-10-13, "Rewrite the README for people; film a real terminal; Luvland overview").
- Later tasks keep this revision unless they say why; `flake.lock` of the walking skeleton pins the same one.
- Machine of this draft: x86-64 Linux with a desktop session (`DISPLAY` and `WAYLAND_DISPLAY` set); nothing was run yet.

## What luv is

luv is a Common Lisp GPU workshop, not a general engine: a small WebGPU-shaped hardware abstraction (HAL) with hand-written Vulkan and Metal 4 backends, an SDL3 window host, a typed shader language written in Lisp, GPU text, and a live SBCL image to develop in. Three programs live in it: Luvcraft (block world), Luft (bevelled block world with a McCLIM atelier) and Luvland (Wayland compositor).

- "Inspectable machinery, not a general-purpose engine" (`../luv/AGENTS.md`) (read).
- Games written so far build their own renderer on the HAL; luv core has no scene graph, material system, lighting model, camera, or mesh library (read: `../luv/luv.asd`, `../luv/hal/`).
- Reusable pieces live in base `luv` (HAL, canvas, shaders, Slug text, captures, live artifacts, lifecycle); much useful game code (sky, world text runs, terminal wall, physics, persistence) lives in `luvcraft/core` and is not a library (read).

## ASDF systems

luv defines many ASDF systems in `../luv/luv.asd`, `../luv/luvcraft.asd`, `../luv/luft.asd` and smaller `.asd` files. ★ marks the ones a game like ours likely loads; the rest are listed so a later reader knows what to skip.

- ★ `luv`: HAL (`hal/gpu.lisp`), canvas protocol and SDL3 host, Vulkan backend (Metal on Darwin), PNG writing, offscreen captures, live artifacts, lifecycle/release, Slug text, SDL3 audio sink, CPU trace zones. Depends on `luv/arithmetic`, `luv/shader`, `luv/slug-shader`, `luv/spir-v`, `luv/msl`, `luv/wgsl`, `cffi`, `closer-mop`, `ieee-floats`, `sdl3` (cl-sdl3), `zpng`, `zpb-ttf`, `sb-posix`; on Darwin `luv/objective-c`, `float-features`, `trivial-main-thread` (read).
- ★ `luv/shader`: the backend-neutral typed shader language and program linking; depends on `closer-mop`, `luv/arithmetic` (read).
- ★ `luv/arithmetic`: semantic quantities (dimensions, units, points vs. differences), columnar records, a checked arithmetic compiled to Lisp, `vec3`; depends on `closer-mop`, `luv/domains` (read).
- `luv/domains`: finite-domain protocols; no dependencies (read).
- `luv/spir-v`, `luv/msl`, `luv/wgsl`, `luv/hlsl`: lowerings of the shader language to SPIR-V, Metal, WGSL, HLSL; pulled in by `luv` except HLSL (read).
- `luv/slug-shader`: Slug glyph coverage as shader functions; in `luv` (read).
- `luv/shaderc`, `luv/shaderc/program`: the ahead-of-time `luv-shaderc` compiler (read).
- ★ `luv/test-support`: the Parachute test runner (`luv.test-support:test-package`); depends on `parachute` (read).
- `luv/production`: a bounded latest-value owner/worker boundary (background work feeding frames); depends on `luv`, `sb-concurrency`; candidate for orbit prediction off the render thread (read).
- ★? `luv/ghostty`: CFFI binding to libghostty-vt (terminal emulation); depends on `cffi`, `cffi-libffi`; needs the full dev shell, not `slim` (read).
- `luv/terminal`, `luv/terminal/canvas`: PTY ownership and canvas keys into a terminal; depend on `luv/ghostty` (read).
- `luv/libav`: FFmpeg binding (video/audio decode); depends on `cffi`, `cffi-grovel` (read).
- `luv/mcclim`, `luv/workbench`, `luv/lobby*`: McCLIM GPU backend, developer workbench (status line, M-x), MQTT lobby; depend on `mcclim-render`, `cl-dejavu`, `mqtt/net` (read). Not for the game itself; maybe for dev tools.
- `luv/tracy-capture`: Tracy profiler capture; depends on `luv` (read).
- `luv/build`, `luv/build/cli`: narrated ASDF builds (the console progress of `make`) (read).
- `luv/objective-c`, `luv/wayland`, `luv/mupdf`, `luv/application-agent`, `luv/showcase`, `luv/parinfer`: not relevant to the game (read).
- `luvcraft/world`, `luvcraft/frontier`, `luvcraft/core`, `luvcraft`, `luvcraft/agent`, `luvcraft/program`, …: the block-world game; `luvcraft/core` pulls in Ghostty, libav, sb-simd and the whole game. Read as reference code, not loaded (read).
- `luft`, `luft/renderer`, `luft/simulation`, `luft/render`, `luft/program`: the Luft atelier; not loaded (read).
- `luv-workbench`: the aggregate the `./sly` image loads by default (`luv`, `luvcraft`, agents, Luft) (read).
- `sly-client`: the `./sly` command-line client (read).

## Rendering

The HAL is a small explicit GPU API: devices, buffers, textures, samplers, bind groups, render and compute pipelines, command encoders, passes and explicit `destroy`. A game writes its own shaders in luv's Lisp shader language and its own pipelines; nothing higher-level is provided.

- HAL vocabulary in `../luv/hal/gpu.lisp`: `create`, `encode`, `finish`, `submit`, `submitted-work-done`, `write-buffer`, `write-texture`, `read-buffer`, `draw`, `draw-indexed` (instanced), compute dispatch, task/mesh pipelines (read).
- Explicit resource discipline: callers destroy what they create; destruction is deferred until the GPU is done; per-frame mutable buffers belong in `canvas-frame-resource-cache` slots (`../luv/wiki/gpu-architecture.org`) (read).
- Vertex formats: `:float32x2`, `:float32x3`, `:float32x4` only (`vertex-attribute-format-component-count`) (read).
- Texture formats: `:r16-float`, `:rg16-float`, `:rg16-uint`, `:rgba8-unorm(-srgb)`, `:bgra8-unorm(-srgb)`, `:rgba16-float`, `:depth32-float`; MSAA 1/2/4/8 (read).
- Primitive topology: `:triangle-list` and `:triangle-strip` only; the Vulkan and Metal backends reject anything else, so no lines or points (`../luv/hal/vulkan/gpu.lisp`, `../luv/hal/metal/gpu.lisp`) (read).
- Blending: opaque or `:premultiplied-alpha` per target; nothing else. Premultiplied output with alpha 0 adds colour, which covers additive glows (read; additive use assumed).
- Back-face culling: off in the Vulkan backend (`:cull-mode nil` in `../luv/hal/vulkan/native.lisp`); no option to enable it (read).
- Depth: only `:depth32-float`; every compare op (`:less`, `:greater`, `:greater-or-equal`, …) and a depth clear value in [0, 1] are accepted, so reversed-Z is expressible; depth clamp is not exposed. Luvcraft uses `:less`, clear 1.0, near 0.1 m, far 180 m (`../luv/luvcraft/simulation.lisp`, `render.lisp`) (read).
- Shader language (`../luv/hal/shader/language.lisp`, `../luv/wiki/mathematical-shaders.org`): vertex, fragment, compute, task and mesh stages; types `float`, `int`, `uint`, `uint64`, `bool`, `vec2..4`, `ivec`, `uvec`, `mat2..4` (column-major), structures; resources: uniform blocks (vec4 lanes, mat4), storage buffers, `texture-2d`, `texture-2d-array`, `texture-cube`, `texture-3d`, depth textures with comparison samplers, read-write textures; GLSL-style math (`mix`, `clamp`, `smoothstep`, `normalize`, `expt`, …), conditionals, bounded folds, no general loops (read).
- No 64-bit floats in shaders: all float types are 32-bit (read).
- Shaders are lowered to SPIR-V (assembled in Lisp; no external compiler), MSL, WGSL, HLSL; `luv-shaderc` does the same ahead of time (read).
- Live shaders: `define-shader-program` and shader methods are redefinable; `../luv/hal/live-artifact.lisp` rebuilds a pipeline on redefinition, keeps the last good one on error (read).
- Lighting, shadows, sky, sun, bloom, fog: all in Luvcraft (`../luv/luvcraft/shaders.lisp`, `render.lisp`, `sky.lisp`), not in `luv`: HDR `:rgba16-float` scene, ACES tone map, quarter-resolution bloom, sun shafts, directional shadow map, procedural sky with Henyey-Greenstein sun glow, stars as hashed gaussians, depth-of-field (`../luv/wiki/sky-and-light.org`) (read). A game copies and adapts these ideas; it cannot load them without all of `luvcraft/core`.
- TAA: Luft-only experiment (Halton jitter, motion vectors, Vulkan resolve shader, MetalFX on Metal); the HAL has a `temporal-scaler` command and multiple render targets (`../luv/wiki/temporal-antialiasing.org`) (read).
- Text in the world: base `luv` has Slug glyph rendering from TrueType outlines (`../luv/hal/shader/slug*.lisp`), HarfBuzz shaping (`shape-slug-text`) and a glyph outline cache; the instanced world-text run with atlas (`make-world-text-run`) lives in `../luv/luvcraft/text.lisp` (read).
- Fonts shipped in `../luv/fonts`: Iosevka Aile, Monaspace Neon (licences included); `cl-dejavu` provides DejaVu (read).
- Terminal screens: Ghostty (`luv/ghostty`) is a VT engine without a shell of its own; `write-terminal` feeds it bytes, so a game can drive a terminal with generated text and VT escape codes (instrument panels) without a PTY (`../luv/ghostty/terminal.lisp`) (read; not tried). The renderer that draws a terminal grid in the world (terminal wall) is Luvcraft code (`../luv/luvcraft/terminal-wall.lisp`) (read).
- McCLIM can draw panes onto GPU surfaces (`luv/mcclim`), used for Luvcraft/Luft developer UIs (read).
- Image loading: luv has no image decoder for textures; it only writes PNGs (`zpng`). Luvcraft's block atlas is procedural. SDL3_image is on luv's native library path and cl-sdl3 binds it; FFmpeg (`luv/libav`) also decodes images (read; texture loading unproven).

## Windowing and input

SDL3 opens windows and reads input; luv wraps it in a canvas protocol (`../luv/hal/canvas.lisp`, `../luv/hal/sdl/canvas.lisp`) with its own event classes and clocks.

- Canvas: `make-sdl-canvas`, `open-canvas`, `close-canvas`, `show`/`hide`, `resize-canvas`, `set-canvas-fullscreen`, `canvas-size`, `canvas-health` (read).
- Clocks drive frames: demand, cadence (fixed fps) and presentation clocks; `request-canvas-frame` runs a function on the canvas thread (read).
- Events: pointer motion/enter/exit/buttons/wheel, key press/release, window resize, pixel-size change, focus, close request; dispatched to `handle-canvas-event` (read).
- Relative mouse mode: `set-canvas-relative-pointer-mode` (read).
- Gamepad and joystick: not exposed by the canvas protocol; SDL3 has them, cl-sdl3 may bind them (read; binding not checked).
- Minimal example of opening a window and drawing every frame: `start-clear-color-demo` in `../luv/hal/examples.lisp` (read).
- Frame failures park drawing and keep the window responsive; a watchdog warns on stalls (read).

## Live image and dev loop

Development in luv happens in a long-lived SBCL image supervised by Swash, driven by `./sly`. You redefine functions, classes and shaders while the game runs.

- `./sly start`, `play [luvcraft|luft|luvland]`, `status`, `screenshot PNG`, `stop-playing`, `failures`, `resume`, `eval`, `inspect`, `describe`, `apropos`, `edit`, `xref`, `load`, `systems`, `stale`, `log`, `restart`, `stop`, `list` (`../luv/AGENTS.md`, `../luv/scripts/sly-client.lisp`) (read).
- `./sly` is bound to its checkout: it enters `../luv`'s Nix shell, starts `sly-server.lisp` from the checkout root, and loads the system in `LUV_SLY_SYSTEM` (default `luv-workbench`) (read).
- `play`, `screenshot` and `stop-playing` only know Luvcraft, Luft and Luvland; `eval`, `describe` and the rest are generic. Luv in Space needs its own launcher or a copy of the setup; to be settled by `task 18: Try luv's live image, screenshots and headless rendering; record the findings` (read).
- Swash, Slynk (without Emacs) and the `sly` Emacs package come from luv's flake (`swashPackage`, `slynkSource`) (read).
- Evaluation errors leave the image alive and wait for a restart choice on stdin (read).

## Screenshots, headless runs and tests

luv renders screenshots from the GPU, not from the desktop, and can render without a visible window. Tests are Parachute systems run by luv's own runner.

- `capture-application-screenshot`: renders one frame offscreen into a target texture, reads it back and writes a PNG; the game implements `capture-canvas`, `prepare-capture`, `encode-capture-frame`, `cleanup-capture` (`../luv/hal/application-capture.lisp`) (read).
- Films: `capture-application-film` writes frame sequences/video (read).
- Hidden canvases: Luvcraft's `capture-hidden-luvcraft-screenshot` opens a hidden SDL canvas (`:visible-p nil`), renders one frame and saves it (`../luv/luvcraft/capture.lisp`); smoke targets `make smoke` / `vulkan-smoke` do this for the built program (read).
- No display: with neither `DISPLAY` nor `WAYLAND_DISPLAY`, luv sets `SDL_VIDEODRIVER=offscreen` (`kmsdrm` on a Linux console), and the dev shell then points Vulkan at Mesa's lavapipe software driver (`LUV_LAVAPIPE_ICD`) (`../luv/flake.nix`, `../luv/hal/sdl/canvas.lisp`) (read). So agents without a display should get software-rendered screenshots (not yet run).
- Tests: Parachute via `luv/test-support`; `make test` runs a Parinfer check plus `scripts/test.lisp`, which discovers the `.asd` test systems and runs them in parallel workers; `make LUV_DEV_SHELL=slim test` runs only Luft's tests (`../luv/Makefile`) (read).
- Tests and builds write into `../luv/build/` (logs, timings, dependency core) and the FASL cache under `~/.cache/common-lisp/luv/<closure>` (read).

## Physics and SIMD

luv has no orbital mechanics. Its physics is Luvcraft's: a fixed-step player controller and a small rigid-body solver for spheres against a voxel grid. What carries over is the pattern and SBCL's SIMD, not the code.

- Player: scalar AABB vs. voxel cells at a fixed 120 Hz step, accumulator in `double-float`, decoupled from frame rate (`../luv/luvcraft/simulation.lisp`, `app.lisp`) (read).
- Rigid bodies: spheres, persistent contacts, graph colouring, Soft Step solver with 4 substeps, sleeping; 1000 awake bodies ≈ 1.5 ms scalar, 1.15 ms NEON on arm64 (`../luv/wiki/physics-and-simd.org`) (read).
- `sb-simd`: SBCL 2.6.7+ supports it on x86-64 and arm64 (NEON); luv's flake pins such an SBCL, so `(require :sb-simd)` is a project invariant (`../luv/wiki/sb-simd.org`) (read).
- `double-float`: ordinary in Lisp code (Luvcraft uses it for time and some physics); the GPU path is single-float only (`write-buffer` takes a `single-float` array, shaders have no f64) (read).
- Quantities (`luv/arithmetic`, `../luv/wiki/quantities-and-measurement.org`): dimensions with rational exponents over any base symbol, units as conversions, quantity kinds, point/absolute/difference character, tensor order, checked declarations on Lisp storage and shader types (read).
- Seeded vocabulary is small: base dimensions length and duration; units metre, kilometre, second, hertz, radian, steradian, percent. No mass, kilogram, force or energy; the protocol is open, so a game adds them with `define-quantity-kind`, `define-unit`, `define-quantity` (`../luv/arithmetic/semantics.lisp`) (read).
- Fit for SI orbital mechanics: the semantics fit (positions as points, displacements as differences, SI units); the cost and ergonomics in a hot integrator are unknown. Likely use: declare quantities at boundaries and keep the integrator on plain `double-float` arrays (assumed).

## Packaging

luv's flake builds each game as a dumped SBCL image with a launcher script, and serves prebuilt outputs from a binary cache. Its packaging function is not exported, so this repo cannot call it directly.

- Packages: `luvcraft` (default), `luft`, `luvland` (Linux), `luv-shaderc`, `sbcl`, `sbcl-2_6_8`, `lisp`, `environment`, `slim-environment`, `ffmpeg`, `libghostty-vt`, `swash`, `tracy`, `tracy-client`; apps `default`, `luvcraft`, `luft`, `luvland`, `luv-shaderc` (`../luv/flake.nix`) (read).
- `applicationPackage` is a `let` binding inside `outputs`, built from `src = self` (luv's own tree) with a fixed set of build scripts; it is not an output (read).
- `lib.environmentFor system` is exported: it returns `pkgs`, `lisp` (SBCL with the Lisp library closure), the development/slim/application package lists and environments, `developmentEnvironmentHook`, `clSdl3WithoutMixer`, `slynkSource`, `swashPackage`, `libghosttyVt`, and more. This repo can build its own dev shell and its own `applicationPackage`-like derivation from it, copying luv's ~120-line recipe (read; not tried).
- No overlays are exported (read).
- Lisp sources of luv itself are not a package: a dependent repo must put the luv flake input's source tree on `CL_SOURCE_REGISTRY` (read; approach assumed).
- Binary cache: `https://luv.swa.sh/nix-cache` with key `luv.swa.sh-1:PpD45iCBkJ38ZkvlyZcLiGdIz6yVehXn3fm1JvG18Bw=` in `nixConfig`; `--accept-flake-config` enables it (read). It caches luv's own outputs; whether derivations built from `lib.environmentFor` hit it depends on identical nixpkgs pins (assumed).
- Audio libraries: `sdl3-mixer` only in the `mixer` dev shell; plain SDL3 audio is in every shell (read).

## Platforms

The flake supports three systems; luv uses Vulkan on Linux and Metal 4 on macOS.

- `x86_64-linux`, `aarch64-linux`: Vulkan 1.4 minimum (timeline semaphores, synchronization2) (`../luv/wiki/gpu-architecture.org`) (read).
- `aarch64-darwin`: native Metal 4; Vulkan via MoltenVK for comparison (read).
- No Windows; HLSL and WGSL exist only as shader outputs (read).
- Developer's machine and main test platform appear to be Apple Silicon (wiki measurements on M2 Pro, arm64) (read).
- This machine: x86-64 Linux; to be tested by `task 17: Build and run luv's programs and test suite; record the runs` (not yet run).

## Performance

The wiki measures CPU frame time with luv's trace zones and Tracy; there are no GPU timestamps yet.

- Luvcraft on an Apple M2 Pro: 60 steady frames at 8.4 ms CPU per frame, of which 5.7 ms waiting for a drawable and 1.1 ms encoding (`../luv/wiki/frame-performance.org`) (read).
- A streaming run: 8.5 ms median, 43 ms p95, 10 frames over the 16.7 ms deadline (read).
- World text: 165 scene draws, 0.17 ms median scene encoding (M2 Pro) (`../luv/wiki/slug-bezier.org`) (read).
- Physics: 300–400 awake bodies ≈ 0.25 ms per step (read).
- Tools: `with-cpu-trace-zone`, Tracy (`nix develop .#tracy`) (read).
- What this means for us: a few hundred draws and an orbital integrator fit easily in a 16 ms frame on the developer's hardware (assumed); lavapipe on agents will be slow (assumed).

## Runs

Filled in by `task 17: Build and run luv's programs and test suite; record the runs`, `task 18: Try luv's live image, screenshots and headless rendering; record the findings` and `task 19: Probe program: a lit sphere in a luv window plus a screenshot`: each run with command, time and outcome.

- None yet.

## Gaps and workarounds

Filled in by `task 20: Assess luv's rendering gaps for solar-system scale, each with a workaround` and `task 21: Remaining gaps, luv wishlist, summary and decisions of the luv findings`. Gaps already visible from reading, for those tasks to assess:

- Depth range across 10^12 m: only `:depth32-float`, but `:greater` and clear 0.0 allow reversed-Z (read).
- Double-precision positions: no f64 on the GPU; camera-relative single-float positions needed (read).
- Spheres and planet surfaces: no mesh generators; no texture loading (read).
- Starfield: Luvcraft's stars are procedural hashes; luft's "stars" are cubical-topology neighbourhoods, not astronomy (`../luv/wiki/luft-star-figures.org`) (read).
- Orbit lines and markers: no line or point topology (read).
- HUD/2D overlay: Slug text and McCLIM exist; no simple screen-space 2D layer in core (read).
- Audio: `audio-sink` in base `luv` (`../luv/hal/sdl/audio.lisp`) plays pushed float frames through SDL3; no mixer or sound loading (read).
- Saves: Luvcraft writes s-expressions under `$XDG_DATA_HOME/luvcraft` (`../luv/luvcraft/play.lisp`, `persistence.lisp`) (read).
- Gamepad: not in the canvas protocol (read).
- Back-face culling not available (read).

## What this means for luv-in-space

Filled in by `task 21: Remaining gaps, luv wishlist, summary and decisions of the luv findings`, the summary that the walking skeleton, architecture and design build on.
