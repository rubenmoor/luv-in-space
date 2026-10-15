# What luv can carry

This document records what [luv](https://github.com/mbrock/luv) offers Luv in Space, a solar-system flight game, and where it falls short. It is the first output of `plan 7: luv capabilities`, written by its tasks 16 to 21 from reading luv's docs and code, building and running its programs and tests, and running probe programs against it. Every claim carries how it was checked: `(ran)` when it was run (the runs are listed under "Runs"), `(read)` when it comes from luv's docs or code, `(assumed)` when neither; recommendations are marked as such. Paths into luv are written `../luv/<path>`. The short version is "What this means for luv-in-space"; what would better live in luv is in `docs/luv-wishlist.md`.

- luv revision examined: `6abc9d10ec8991c2cfe92888b84946372d6030b9` (2026-10-13, "Rewrite the README for people; film a real terminal; Luvland overview").
- Later tasks keep this revision unless they say why; `flake.lock` of the walking skeleton pins the same one.
- Machine: x86-64 Linux (NixOS), Intel Core i5-12500H, with a desktop session (`DISPLAY=:1`, `WAYLAND_DISPLAY=wayland-1`); GPUs: Intel Iris Xe (ADL GT2, Mesa ANV) and NVIDIA GeForce RTX 4060 Laptop (proprietary 595.45), plus Mesa lavapipe (CPU). Runs are listed under "Runs".

## What luv is

luv is a Common Lisp GPU workshop, not a general engine: a small WebGPU-shaped hardware abstraction (HAL) with hand-written Vulkan and Metal 4 backends, an SDL3 window host, a typed shader language written in Lisp, GPU text, and a live SBCL image to develop in. Three programs live in it: Luvcraft (block world), Luft (bevelled block world with a McCLIM atelier) and Luvland (Wayland compositor).

- "Inspectable machinery, not a general-purpose engine" (`../luv/AGENTS.md`) (read).
- Games written so far build their own renderer on the HAL; luv core has no scene graph, material system, lighting model, camera, or mesh library (read: `../luv/luv.asd`, `../luv/hal/`).
- Reusable pieces live in base `luv` (HAL, canvas, shaders, Slug text, captures, live artifacts, lifecycle); much useful game code (sky, world text runs, terminal wall, physics, persistence) lives in `luvcraft/core` and is not a library (read).

## ASDF systems

luv defines many ASDF systems in `../luv/luv.asd`, `../luv/luvcraft.asd`, `../luv/luft.asd` and smaller `.asd` files. ★ marks the ones a game like ours likely loads; the rest are listed so a later reader knows what to skip.

- ★ `luv`: the only system the sphere probe needed (ran: run 30); HAL (`hal/gpu.lisp`), canvas protocol and SDL3 host, Vulkan backend (Metal on Darwin), PNG writing, offscreen captures, live artifacts, lifecycle/release, Slug text, SDL3 audio sink, CPU trace zones. Depends on `luv/arithmetic`, `luv/shader`, `luv/slug-shader`, `luv/spir-v`, `luv/msl`, `luv/wgsl`, `cffi`, `closer-mop`, `ieee-floats`, `sdl3` (cl-sdl3), `zpng`, `zpb-ttf`, `sb-posix`; on Darwin `luv/objective-c`, `float-features`, `trivial-main-thread` (read).
- ★ `luv/shader`: the backend-neutral typed shader language and program linking; depends on `closer-mop`, `luv/arithmetic` (read).
- ★ `luv/arithmetic`: semantic quantities (dimensions, units, points vs. differences), columnar records, a checked arithmetic compiled to Lisp, `vec3`; depends on `closer-mop`, `luv/domains` (read).
- `luv/domains`: finite-domain protocols; no dependencies (read).
- `luv/spir-v`, `luv/msl`, `luv/wgsl`, `luv/hlsl`: lowerings of the shader language to SPIR-V, Metal, WGSL, HLSL; pulled in by `luv` except HLSL (read).
- `luv/slug-shader`: Slug glyph coverage as shader functions; in `luv` (read).
- `luv/shaderc`, `luv/shaderc/program`: the ahead-of-time `luv-shaderc` compiler (read).
- ★ `luv/test-support`: the Parachute test runner (`luv.test-support:test-package`); depends on `parachute` (read).
- `luv/production`: a bounded latest-value owner/worker boundary (background work feeding frames); depends on `luv`, `sb-concurrency`; once a candidate for orbit prediction off the render thread (read); not used, `decision 20: Trajectory prediction on the frame thread`.
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
- Swapchain rebuilds: on a resize the Vulkan canvas rebuilds its swapchain and destroys the old textures (`rebuild-vulkan-canvas-swapchain`); nothing public signals it but a new `canvas-extent`, and a frame-resource cache keeps the old textures' entries until evicted. Here `note-swapchain-target` (`src/render.lisp`) evicts them on an extent change (ran: `tests/render.lisp`, three `resize-canvas` calls on a hidden canvas, 4 slots before and after; 16 without the eviction).
- Vertex formats: `:float32x2`, `:float32x3`, `:float32x4` only (`vertex-attribute-format-component-count`) (read).
- Texture formats: `:r16-float`, `:rg16-float`, `:rg16-uint`, `:rgba8-unorm(-srgb)`, `:bgra8-unorm(-srgb)`, `:rgba16-float`, `:depth32-float`; MSAA 1/2/4/8 (read).
- Primitive topology: `:triangle-list` and `:triangle-strip` only; the Vulkan and Metal backends reject anything else, so no lines or points (`../luv/hal/vulkan/gpu.lisp`, `../luv/hal/metal/gpu.lisp`) (read).
- Blending: opaque or `:premultiplied-alpha` per target; nothing else. Premultiplied output with alpha 0 adds colour, which covers additive glows (read; additive use assumed).
- Back-face culling: off in the Vulkan backend (`:cull-mode nil` in `../luv/hal/vulkan/native.lisp`); no option to enable it (read).
- Depth: only `:depth32-float`; every compare op (`:less`, `:greater`, `:greater-or-equal`, …) and a depth clear value in [0, 1] are accepted, so reversed-Z is expressible; depth clamp is not exposed. Luvcraft uses `:less`, clear 1.0, near 0.1 m, far 180 m (`../luv/luvcraft/simulation.lisp`, `render.lisp`) (read).
- A game owns its projection: luv has no camera or projection helper; the clip rows come from the game's uniforms (ran: "Appendix: probe program").
- Clip space on the Vulkan path: y points down, depth 0..1; the viewport is not flipped (`cmd-set-viewport-and-scissor` in `../luv/hal/vulkan/native.lisp`), so a game negates clip y, as Luvcraft's shaders do (ran: run 29, the first sphere came out upside down). Metal not checked.
- Reversed-Z works: compare `:greater`, clear 0.0 and an infinite projection (`depth = near/z`) drew Earth from 629 km altitude and the Moon from 384,400 km, both with near 0.1 m; the standard projection (`:less`, clear 1.0, far 10^7 / 10^9 m) showed the sphere's far side bleeding through the near side at Earth scale and drew nothing at the Moon's distance, where `1 - near/z` rounds to 1.0 in single float (ran: runs 29–31).
- Rendering to the window: the canvas's swapchain textures carry only `:copy-dst` unless the canvas configuration asks for more; a game that renders straight into them passes `:usage '(:render-attachment :copy-dst)` to `make-canvas-configuration`. Render passes take texture views, not textures (`:unsupported-render-pass` otherwise), and a view destroyed before `submit` fails (`gpu-object-destroyed-error`), so views live as long as their texture (ran: run 29).
- Shader language (`../luv/hal/shader/language.lisp`, `../luv/wiki/mathematical-shaders.org`): vertex, fragment, compute, task and mesh stages; types `float`, `int`, `uint`, `uint64`, `bool`, `vec2..4`, `ivec`, `uvec`, `mat2..4` (column-major), structures; resources: uniform blocks (vec4 lanes, mat4), storage buffers, `texture-2d`, `texture-2d-array`, `texture-cube`, `texture-3d`, depth textures with comparison samplers, read-write textures; GLSL-style math (`mix`, `clamp`, `smoothstep`, `normalize`, `expt`, …), conditionals, bounded folds, no general loops (read).
- No 64-bit floats in shaders: all float types are 32-bit (read).
- Shaders are lowered to SPIR-V (assembled in Lisp; no external compiler), MSL, WGSL, HLSL; `luv-shaderc` does the same ahead of time (ran: run 10, SPIR-V passes `spirv-val`; at run time Luvcraft logs each SPIR-V module it prepares, run 6).
- Live shaders: `define-shader-program` and shader methods are redefinable; `../luv/hal/live-artifact.lisp` rebuilds a pipeline on redefinition, keeps the last good one on error (ran: runs 20, 21).
- Lighting, shadows, sky, sun, bloom, fog: all in Luvcraft (`../luv/luvcraft/shaders.lisp`, `render.lisp`, `sky.lisp`), not in `luv`: HDR `:rgba16-float` scene, ACES tone map, quarter-resolution bloom, sun shafts, directional shadow map, procedural sky with Henyey-Greenstein sun glow, stars as hashed gaussians, depth-of-field (`../luv/wiki/sky-and-light.org`) (read). Luvcraft's smoke test drew its HDR target through bloom and tone map on lavapipe, so rendering into a texture and sampling it in a later pass works (ran: run 28); our post chain and the grapple camera's render target rely on it. A game copies and adapts these ideas; it cannot load them without all of `luvcraft/core`.
- TAA: Luft-only experiment (Halton jitter, motion vectors, Vulkan resolve shader, MetalFX on Metal); the HAL has a `temporal-scaler` command and multiple render targets (`../luv/wiki/temporal-antialiasing.org`) (read).
- Text in the world: base `luv` has Slug glyph rendering from TrueType outlines (`../luv/hal/shader/slug*.lisp`), HarfBuzz shaping (`shape-slug-text`) and a glyph outline cache; the instanced world-text run with atlas (`make-world-text-run`) lives in `../luv/luvcraft/text.lisp` (read).
- Fonts shipped in `../luv/fonts`: Iosevka Aile, Monaspace Neon (licences included); `cl-dejavu` provides DejaVu (read).
- Terminal screens: Ghostty (`luv/ghostty`) is a VT engine without a shell of its own; `write-terminal` feeds it bytes, so a game can drive a terminal with generated text and VT escape codes (instrument panels) without a PTY (`../luv/ghostty/terminal.lisp`) (read; not tried). The renderer that draws a terminal grid in the world (terminal wall) is Luvcraft code (`../luv/luvcraft/terminal-wall.lisp`) (read).
- McCLIM can draw panes onto GPU surfaces (`luv/mcclim`), used for Luvcraft/Luft developer UIs (read).
- Image loading: luv has no image decoder of its own; it only writes PNGs (`zpng`), and Luvcraft's block atlas is procedural. The `sdl3` system that `luv` loads includes cl-sdl3's SDL_image binding (`sdl3-image:img-load`), and SDL3_image is on luv's native library path: a 5400×2700 JPEG decoded, converted to `:abgr8888` and was uploaded as an `:rgba8-unorm-srgb` texture (ran: run 32). `write-texture` takes a rank-2 `(unsigned-byte 32)` array of rows × columns, not a flat vector (ran: run 32; `check-vulkan-texture-write`). FFmpeg (`luv/libav`) also decodes images (read).
- Instancing: `draw` and `draw-indexed` take an instance count, and a vertex buffer layout with `:step-mode :instance` advances per instance (Luvcraft's text, terminal wall, bodies) (ran: run 32, 4,000 star quads and 256 orbit segments in two draws).
- Fragment depth and discard: `:frag-depth` output and `(discard)` work, so a fragment shader can ray-trace a sphere and write its true depth (ran: run 32).
- Shader language limits met in run 32: only `let*` binds at statement level (`let` is rejected); no implicit scalar–vector broadcasting for `-` (`(- 0.0 ray)` fails; `*` and `/` with a scalar work); no `atan`, `asin`, `acos` or `tan` operators (the list in `../luv/hal/shader/language.lisp` has `sin`, `cos`, `exp`, `log`, `expt`, `sqrt`), so angles come from a polynomial `atan2` written as a `define-shader-function` (ran).
- Bind groups: a render pass needs `set-bind-group` after each `set-pipeline`; binding before the first pipeline fails with `:NO-PIPELINE` (ran: run 32).
- Render passes on Vulkan only clear: a colour attachment with `:load-op :load` is rejected with `:UNSUPPORTED-RENDER-PASS` (the check in `begin-render-pass`, `../luv/hal/vulkan/gpu.lisp`), so a pass cannot add into what an earlier pass drew; only bind group index 0 is accepted (ran: `task 111: post.lisp: HDR target, bloom, tone map, exposure`). Workaround: a pass that adds two images samples both and writes a third target (`src/post.lisp`'s way up); passes that draw on one target share one render pass.

## Windowing and input

SDL3 opens windows and reads input; luv wraps it in a canvas protocol (`../luv/hal/canvas.lisp`, `../luv/hal/sdl/canvas.lisp`) with its own event classes and clocks.

- Canvas: `make-sdl-canvas`, `open-canvas`, `close-canvas`, `show`/`hide`, `resize-canvas`, `set-canvas-fullscreen`, `canvas-size`, `canvas-health` (read).
- Clocks drive frames: demand, cadence (fixed fps) and presentation clocks; `request-canvas-frame` runs a function on the canvas thread (read).
- Events: pointer motion/enter/exit/buttons/wheel, key press/release, window resize, pixel-size change, focus, close request; dispatched to `handle-canvas-event` (read).
- Relative mouse mode: `set-canvas-relative-pointer-mode` (read).
- Gamepad and joystick: not exposed by the canvas protocol; luv initialises only SDL's video subsystem and its event loop ignores event types it does not know; cl-sdl3 binds SDL3's gamepad and joystick API (`src/gamepad/func.lisp` of luv's `cl-sdl3-without-mixer` source) (read). Workaround under "Gaps and workarounds".
- On this machine Luvcraft and Luft open an SDL3 window on Wayland with Vulkan and close cleanly on `SIGTERM` ("canvas closing … canvas closed" in the log) (ran: runs 6, 8, 9).
- Input as the programs announce it in their window titles: Luvcraft "1–9,0 select · e place · x mine · c pick · I inventory · F phone · shift sprint · arrows look · tab focus · ctrl-q quit"; Luft "click to play · WASD move · Space jump, twice to fly · L/R edit · 1–8 material · Tab terminal · F5 camera · Esc release" (ran; keys not pressed, the session had no human at the keyboard).
- Minimal example of opening a window and drawing every frame: `start-clear-color-demo` in `../luv/hal/examples.lisp` (read).
- Frame failures park drawing and keep the window responsive; a watchdog warns on stalls (read).

## Live image and dev loop

Development in luv happens in a long-lived SBCL image supervised by Swash, driven by `./sly`. You redefine functions, classes and shaders while the game runs.

- `./sly start`, `play [luvcraft|luft|luvland]`, `status`, `screenshot PNG`, `stop-playing`, `failures`, `resume`, `eval`, `inspect`, `describe`, `apropos`, `edit`, `xref`, `load`, `systems`, `stale`, `log`, `restart`, `stop`, `list` (`../luv/AGENTS.md`, `../luv/scripts/sly-client.lisp`) (read).
- The whole loop works for an agent from a terminal: `start` (26 s cold, 3–4 s with a cached dependency core), `play luft`, `status`, `screenshot`, `eval`, live redefinition, `failures`, `resume`, `stop-playing`, `stop` (ran: runs 15–25).
- Swash runs each image as a transient `systemd --user` unit; `./sly list --all` reads the instances from Swash's journal events, keyed by checkout root (`LUV_ROOT`). The image's PID is in `./sly list`; `./sly stop` ends it and the PID is gone afterwards (ran).
- State written inside the checkout: `build/` (`sly-client` 130 MB, `sly-dependencies.core` 365 MB, build logs; ignored by git), plus the FASL cache under `~/.cache/common-lisp/luv/` and Swash's journal entries. There is no option to move `build/`, so the runs used a `/tmp` clone of `../luv` (ran).
- The image keeps the environment it was started with: Environment changes in the client's shell do not reach it; `(sb-posix:setenv …)` inside the image does, e.g. `VK_DRIVER_FILES` set by `./sly eval` before `play luft` moved Luft to the NVIDIA GPU, because luv creates the Vulkan instance per canvas (ran: run 18).
- Live shader redefinition: loading a new `define-shader-function` for Luft's colour grade while it played changed the next frame (screenshot turned red) with no restart, in 2.7 s (ran: run 20).
- A broken shader redefinition (unknown operator) keeps the last good pipeline drawing; nothing reaches `./sly status`, `./sly failures` or the log; the error is only in `luv:live-artifact-diagnostic` of the application's live artifacts (status `:failed`) (ran: run 21). For us: our `./sly status` prints failed live artifacts with their diagnostic (`task 28: Live image: report failed live artifacts`). In this game's `define-shader` an unknown operator is already rejected by the eval; a build failure (e.g. a `shader-specification-for` method calling an undefined shader) reaches the artifact.
- Failure parking: an error in a Lisp frame function parks the canvas; `./sly status` says "frames are PARKED … ./sly resume", `./sly failures` prints the error and backtrace, the window stays responsive; after fixing the function `./sly resume` resumed drawing (ran: run 22). `./sly failures` prints a literal `~%` (cosmetic luv bug, ran).
- Errors at start (`play luft` on a GPU without mesh shaders) abort the evaluation; the image stays up and `./sly eval` keeps working (ran: run 17).
- Evaluation errors print the backtrace and "No input; aborting evaluation" when stdin is not a terminal, so an agent's command returns instead of hanging (ran).
- Swash, Slynk (without Emacs) and the `sly` Emacs package come from luv's flake (`swashPackage`, `slynkSource`) (read).

### Using the live image from this repo

`./sly` serves the checkout it lives in, and only its generic commands work for another game. An external ASDF system loads fine into a luv image, but `play`, `screenshot` and `stop-playing` are hard-wired to Luvcraft, Luft and Luvland.

- Loading another system into a running luv image works: `./sly eval '(asdf:load-asd "/path/x.asd")'`, then `(asdf:load-system "x")` in a second `eval` (one form is read whole, so package-qualified symbols of the new system fail in the same form) (ran: run 24).
- `LUV_SLY_SYSTEM` picks the image's system, but luv's shell hook unsets it, and `./sly` re-enters that shell when called from outside; set it (and `CL_SOURCE_REGISTRY`) inside the dev shell, after the hook. Then the image held only the probe system: no `LUVCRAFT` or `LUFT` packages (ran: run 25).
- In such an image `./sly screenshot` and `./sly stop-playing` fail with a reader package error (their forms name `LUVCRAFT:` symbols); `status`, `eval`, `failures`, `resume`, `stop` work (ran: run 25).
- `./sly` cannot run from the luv flake input: it writes `build/` into its own root, which would be the read-only store (read; follows from `sly`, `scripts/build-sly-client`).
- Built by `task 26: Live image dev loop for luv-in-space`, without copying luv's 2,400-line client: `scripts/sly-client.lisp` loads luv's `sly-client` system from the flake input and redefines `ensure-sly-dependency-core` (luv's `scripts/sly-dependency-core.lisp` run on this checkout), the game status, `play`, `screenshot`, `stop-playing` and the usage text; `scripts/build-sly-client` dumps it to `build/sly-client`; `sly-server.lisp` is luv's, adapted (root here, `luv/build/cli` and `sly-asdf-status.lisp` from luv's source, system `luv-in-space`). Swash keys the image by this checkout's root, so it does not clash with luv's images (ran: run 34).
- Run 34 (2026-10-14): `./sly start` 8.4 s cold (client and dependency core built, luv's FASLs cached); `play`, `status` "healthy (… 252 frames)", `screenshot` 1.3 s at 959×1010; `eval` of a redefined `uniform-data` (sun from the right) while playing changed the next screenshot's lit side; `stop-playing`, `stop`, image PID gone. Screenshots read, then deleted.
- Our scene's pipelines are luv live shader pipelines (`make-live-shader-pipeline`, roles `:earth` and `:overlay` as `shader-specification-for` methods returning the `define-shader` functions), returned by `application-live-artifacts` and refreshed at the top of each frame; a re-evaluated `earth-fragment` turned the oceans red on the next frame, and a source with an unknown operator was rejected by the eval while the last good pipelines kept drawing (ran: `task 30: Live shader pipelines for the scene`).
- Wishlist for luv (`docs/luv-wishlist.md`): `./sly` with a configurable root and build directory, and `play`/`screenshot`/`stop-playing` through a registered application protocol instead of hard-coded games.

## Screenshots, headless runs and tests

luv renders screenshots from the GPU, not from the desktop, and can render without a visible window. Tests are Parachute systems run by luv's own runner.

- `capture-application-screenshot`: renders one frame offscreen into a target texture, reads it back and writes a PNG; the game implements `capture-canvas` and `encode-capture-frame` (optionally `prepare-capture`, `cleanup-capture`) (`../luv/hal/application-capture.lisp`) (read). The canvas must be open with an RGBA8/BGRA8 context; the PNG has the canvas's size (ran: runs 24–27).
- A 40-line probe outside luv (an application class, the two methods, a clear colour; source in "Appendix: screenshot probe") got a correct PNG: linear (1.0, 0.5, 0.1) came out as sRGB (255, 188, 89) (ran: runs 24, 26, 27).
- `./sly screenshot` uses the same path for Luft and Luvcraft: 1.3 s for a 959×1010 frame of the running game, including the status line (ran: run 19).
- Hidden canvases (`make-sdl-canvas :visible-p nil`) render and capture at the requested size; visible windows get the compositor's size (959×1010 under this tiling compositor, not the 320×200 asked for), so screenshots for tests should use a hidden canvas (ran: run 24).
- Films: `capture-application-film` writes frame sequences/video (read).
- No display works: with neither `DISPLAY` nor `WAYLAND_DISPLAY` set when the dev shell is entered, its hook sets `SDL_VIDEODRIVER=offscreen` (`kmsdrm` on a Linux console) and `VK_DRIVER_FILES` to Mesa's lavapipe (`LUV_LAVAPIPE_ICD`); the probe rendered and saved its PNG that way, and Luvcraft's `--smoke-test` rendered a full 3D frame (HDR, Slug text, HUD) on lavapipe in 8 s (ran: runs 26, 28). The variables must be unset before `nix develop`, since the hook decides on entry (read: `../luv/flake.nix`).
- Inside the Nix build sandbox (no `/dev/dri`, no GPU, no display) the probe rendered on lavapipe and wrote the PNG into `$out`, in 21 s including compiling luv (ran: run 27). So the skeleton's GPU test can be a `nix flake check` (`checks.<system>`) built with `lib.environmentFor`'s `slimDevelopmentEnvironment`, `slimDevelopmentPackages` and `slimDevelopmentEnvironmentHook`, `HOME` set to `$TMPDIR` for the FASL cache. Recorded as `decision 2: Screenshots and GPU test: offscreen capture on a hidden canvas, headless on lavapipe`.
- Rendering outside the shell needs `LUV_PROJECT_ROOT` (or `CL_SOURCE_REGISTRY`) pointing at luv's source; `./sly` and luv's scripts set it, a bare `nix develop -c sbcl` does not, and luv then failed to load (ran: run 26).
- Tests: Parachute via `luv/test-support`; `make test` runs a Parinfer check plus `scripts/test.lisp`, which discovers the `.asd` test systems and runs them in parallel workers; `make LUV_DEV_SHELL=slim test` runs only Luft's tests (`../luv/Makefile`) (read).
- Test model for this repo: each `…/test` system has `:perform (test-op … (uiop:symbol-call '#:luv.test-support '#:test-package '#:<package>))`; `test-package` runs the package's Parachute tests and signals on failure, so `sbcl --non-interactive --eval '(asdf:test-system "luv-in-space/tests")'` exits non-zero on a failure. luv's own suites use fake GPU objects (`../luv/hal/application-capture-tests.lisp`); real-GPU checks are smoke runs (`luvcraft --smoke-test PNG`), not Parachute tests (read).
- `make test`, `make test-suite`, `make luft-test` pass at the examined revision on x86-64 Linux: 31 Parachute suites, 0 failures; Luft's 611,001 star checks pass (ran: runs 11–13).
- Tests and builds write into `<checkout>/build/` (sly client, dependency core, logs) and the FASL cache under `~/.cache/common-lisp/luv/<closure>`; the targets take no output-directory option, so the runs here used a throwaway `git clone` of `../luv` in `/tmp` to keep `../luv` clean (ran).

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

luv's flake builds each game as a dumped SBCL image with a launcher script, and a binary cache serves the dependency closure. Its packaging function is not exported, so this repo calls `lib.environmentFor` and copies the packaging recipe.

- Packages: `luvcraft` (default), `luft`, `luvland` (Linux), `luv-shaderc`, `sbcl` (2.6.7), `sbcl-2_6_8`, `lisp`, `environment`, `slim-environment`, `ffmpeg`, `libghostty-vt`, `swash`, `tracy`, `tracy-client`, `wpewebkit`; apps `default`, `luvcraft`, `luft`, `luvland` (Linux), `luv-shaderc`; dev shells `default`, `slim`, `mixer`, `tracy`, `annex` (ran: run 1).
- Top-level outputs: `apps`, `devShells`, `lib`, `packages`; no `overlays`, no `checks` (ran: run 1).
- `applicationPackage` is a `let` binding inside `outputs` (`../luv/flake.nix:637`), built from `src = self` (luv's own tree): copy the tree, run the dev hook, `sbcl --script <build script>`, ship the dumped image under `libexec/`, the FASL cache under `lib/fasl/`, `fonts/`, and a launcher in `bin/`; it is not an output (read).
- `lib` has one attribute, `environmentFor`; `lib.environmentFor "x86_64-linux"` returns `applicationClosure`, `applicationEnvironment`, `applicationPackages`, `clSdl3WithoutMixer`, `developmentClosure`, `developmentEnvironment`, `developmentEnvironmentHook`, `developmentPackages`, `ffmpeg`, `ffmpegLibraryDirectory`, `libghosttyVt`, `libghosttyVtLibrary`, `lisp`, `mesaLibraryPath`, `mupdf`, `mupdfLibraryDirectory`, `nativeLibraryPath`, `pkgs`, `sbcl`, `sbcl268`, `slimDevelopment*`, `slyRoot`, `slynkSource`, `swashPackage`, `tracy*`, `wpePkgs` (ran: run 14). Everything `applicationPackage` uses is among them, so a ~120-line copy here with `src` = this repo works (read; the copy is built by `task 22: Flake: luv input, dev shell, package and nix run opening an empty window`).
- After the dump the recipe deletes every top-level entry of its copied tree but `fonts/` (and on Darwin `build/` and `objective-c/`) (`../luv/flake.nix:720`); the image finds luv's tree through `LUV_PROJECT_ROOT`, so a game's own runtime data (textures, star table) must be installed outside that tree (read). Built here (`task 110: assets.lisp, the Blue Marble and its packaging`): the recipe moves `assets/` aside before the delete and reinstalls it as `$out/share/luv-in-space/assets`, and the launcher exports `LUV_IN_SPACE_ASSETS` to it; `nix build` installed the Blue Marble there and `cd /tmp && nix run <repo>` drew the textured Earth (ran).
- Lisp sources of luv itself are not a package: a dependent repo puts the luv flake input's source tree (`${luv}`) on `CL_SOURCE_REGISTRY` (read; approach assumed).
- Binary cache: `https://luv.swa.sh/nix-cache` (priority 30) with key `luv.swa.sh-1:PpD45iCBkJ38ZkvlyZcLiGdIz6yVehXn3fm1JvG18Bw=` in `nixConfig`; `--accept-flake-config` enables it (read). It serves the dependency closure (Mesa, Vulkan tools, ffmpeg, typst, deno, zig, clang, …: the dev shell's first entry copied them from it), but none of the luv outputs at this revision for x86_64-linux (`luvcraft`, `luft`, `luv-shaderc`, `lisp` narinfos: HTTP 404) (ran: runs 4, 11). So this repo's derivations get their dependencies from the cache as long as they use luv's `pkgs`, and build only the Lisp parts locally (about 40 s per game image).
- `nix build path:../luv#…` and `github:mbrock/luv/<rev>#…` give different store paths (the source hash differs), so builds from a local checkout never reuse the pinned ones (ran: runs 2, 3).
- Launcher pitfall: the launcher prepends luv's library path to an inherited `LD_LIBRARY_PATH`; this session's `LD_LIBRARY_PATH` held a host `alsa-lib` built against glibc 2.43, which `libSDL3.so` then loaded against luv's glibc 2.42 and the program died before its first frame (ran: run 5). Workaround: our launcher and scripts unset `LD_LIBRARY_PATH` before starting (`env -u LD_LIBRARY_PATH`), or replace it instead of prepending.
- Audio libraries: `sdl3-mixer` only in the `mixer` dev shell; plain SDL3 audio is in every shell (read).

## Platforms

The flake supports three systems; luv uses Vulkan on Linux and Metal 4 on macOS.

- `x86_64-linux`, `aarch64-linux`: Vulkan 1.4 minimum (timeline semaphores, synchronization2) (`../luv/wiki/gpu-architecture.org`) (read).
- `aarch64-darwin`: native Metal 4; Vulkan via MoltenVK for comparison (read). The Objective-C exception bridge is compiled at load time to `../build/objective-c-exception-bridge-<stamp>.dylib` relative to luv's `objective-c/` sources (`../luv/objective-c/exception-bridge.lisp`), so on Darwin luv cannot load from a read-only store path (read).
- No Windows; HLSL and WGSL exist only as shader outputs (read).
- luv's developer's machine and main test platform appear to be Apple Silicon (wiki measurements on M2 Pro, arm64) (read).
- This machine, x86-64 Linux: builds, test suite, Luvcraft, Luft and `luv-shaderc` work (ran: runs 2–13).
- GPU choice: with several Vulkan devices, luv took the first one, the Intel iGPU (`vulkaninfo`: the only device here without `VK_EXT_mesh_shader`; the RTX 4060 and llvmpipe have it); Luft then died at start with `GPU request failed during :CREATE-MESH-RENDER-PIPELINE: :VULKAN-MESH-SHADER-RUNTIME-UNAVAILABLE`; with `VK_DRIVER_FILES=/run/opengl-driver/share/vulkan/icd.d/nvidia_icd.json` it ran (ran: runs 7, 8). luv has no device-selection option; `VK_DRIVER_FILES` is the workaround (ran). For us: no mesh or task shaders, so the game runs on any Vulkan 1.4 iGPU (recommendation).
- `aarch64-linux`, `aarch64-darwin`: evaluated only (`nix flake show` lists their packages); not run here (read).

## Performance

The wiki measures CPU frame time with luv's trace zones and Tracy; there are no GPU timestamps yet.

- Luvcraft on an Apple M2 Pro: 60 steady frames at 8.4 ms CPU per frame, of which 5.7 ms waiting for a drawable and 1.1 ms encoding (`../luv/wiki/frame-performance.org`) (read).
- A streaming run: 8.5 ms median, 43 ms p95, 10 frames over the 16.7 ms deadline (read).
- World text: 165 scene draws, 0.17 ms median scene encoding (M2 Pro) (`../luv/wiki/slug-bezier.org`) (read).
- Physics: 300–400 awake bodies ≈ 0.25 ms per step (read).
- Luvcraft on this machine at 959×1010: its frame pacer measured 20–36 ms of GPU work per frame on the Intel Iris Xe and settled at 24–36 Hz; on the RTX 4060 16–22 ms, settling at 36–48 Hz (ran: runs 6, 9). Luvcraft's full pipeline (shadows, bloom, sun shafts, McCLIM overlay) is heavy on an iGPU; our frame budget should assume an iGPU (assumed).
- Tools: `with-cpu-trace-zone`, Tracy (`nix develop .#tracy`) (read).
- Integrator (measured for `docs/architecture.md` "Integrator" by a throwaway SBCL script on this machine's i5-12500H): ~180 ns per RK4 step of the ship with Earth, J2, Sun and Moon, ~50 ns for a point mass alone (ran). The CPU budgets rest on it: at most 4 ms of steps per frame and 4,096 steps (~1 ms) (`decision 16: Integrator and simulation clock`), trajectory prediction 1 ms per frame (`decision 20: Trajectory prediction on the frame thread`), the rest of the frame 6 ms (`docs/design.md` "Performance target"), the headless quest play-through ~15 s (`docs/architecture.md` "The quest play-through").
- What this means for us: the simulation fits a 16.7 ms frame with room to spare (ran, above); the GPU side is not measured for our pipeline. The 60 Hz target at 1280×800 holds on the Intel iGPU luv picks first only if our passes stay well below Luvcraft's (no shadow map, no sun shafts, no McCLIM overlay), which `milestone 6` measures (assumed); lavapipe on agents is slow (ran: run 33, 9.1 s for a first scene with a 58 MB texture).

## Gaps and workarounds

What a solar-system flight game needs that luv lacks, each gap with a workaround in this repo. The rendering gaps come from `task 20: Assess luv's rendering gaps for solar-system scale, each with a workaround`, checked against luv's code and the gap probe (runs 32–33, "Appendix: gap probe"); the rest are assessed by `task 21: Remaining gaps, luv wishlist, summary and decisions of the luv findings`. Costs are rough sizes of the code this repo writes.

### Depth range across 10^12 m

- luv has: one depth format, `:depth32-float`; every compare op; a depth clear value in [0, 1]; `:frag-depth` output; no depth clamp; no projection helper, so the game builds its own clip rows (read: `../luv/hal/gpu.lisp`, `../luv/hal/vulkan/native.lisp`; ran: runs 29–32).
- Gap: a standard projection loses the far side of Earth to depth precision from orbit and draws nothing at the Moon's distance (ran: run 30). Nothing in luv is wrong; a game must choose its depth mapping.
- Options weighed:
  - Reversed-Z with float depth and an infinite far plane (`depth = near / z`, compare `:greater`, clear 0.0): one pass, works from 0.1 m to 3.8×10^8 m (ran: runs 30–32). Float32's relative precision is about 10^-7 at every distance, so separation stays good from cockpit to Neptune (assumed from the format; not tried beyond the Moon).
  - Logarithmic depth written by `:frag-depth`: also works in luv, but disables early depth testing for every surface and needs care with interpolation; no gain over reversed float depth (read; not tried).
  - Layered passes (far bodies, near bodies, cockpit), each with its own near/far and a depth clear between: works with any mapping but costs a pass per layer and sorting of bodies into layers (assumed).
- Workaround here: reversed-Z with an infinite far plane for every pass. Things at infinity (stars) write depth 0 and draw first with compare `:always` and depth writes off. The cockpit is drawn in the same pass (near 0.1 m keeps a 0.5 m panel well resolved); a separate cockpit pass with its own depth clear is the fallback if the hull ever needs to clip into the world (assumed).
- Cost: small; a projection function and a pipeline convention, both already in the probes.
- Recorded as `decision 4: Depth strategy`.

### Double-precision positions

- luv has: no `double-float` on the GPU (shader floats are 32-bit; `write-buffer` takes `single-float` arrays); no camera or world-position type in base `luv`; `luv.arithmetic`'s `vec3` stores any `real`, so it is precision-neutral but generic (read: `../luv/arithmetic/lisp/vec3.lisp`). Luvcraft's camera and text placement coerce to `single-float` (read: `../luv/luvcraft/text.lisp`); nothing the game loads assumes single floats on the CPU.
- Gap: world positions span 10^12 m; single floats give about 0.5 m resolution at Earth's radius and 60 km at 1 AU, so positions cannot go to the GPU as they are (assumed from float32 arithmetic).
- Workaround here: physics state in `double-float` (heliocentric or per-body frames, see `docs/architecture.md`); each frame the CPU subtracts the camera's double position and converts the difference to `single-float`; the GPU sees only camera-relative positions and directions. Values that cancel badly in single float are computed on the CPU in double: the probe passes `|c|^2 - r^2` for the ray–sphere test as `h (2r + h)` with `h` the altitude, since `|c|^2` alone is ~5×10^13 and its float error ~3×10^6 m² (ran: runs 30–32; the probe's camera math is in double).
- Cost: small per object (one subtraction per draw); the discipline matters more than the code: no world-space matrices on the GPU.
- Recorded as `decision 5: Precision of positions`.

### Spheres and planet surfaces

- luv has: no mesh generators and no sphere; triangle lists and strips; textures, samplers, cube textures, `:frag-depth`, `discard`; image decoding through `sdl3-image` (ran: run 32).
- Gap: a UV sphere mesh shows a polygonal limb from low orbit (64 slices, ran: run 30) and needs view-dependent tessellation to stay round, and without back-face culling its far side relies on depth alone.
- Workaround here: draw planets and moons as ray-traced impostors: a full-screen triangle (or a screen-space quad around the body's disc), a fragment shader that intersects the view ray with the sphere, discards misses and writes the hit's reversed-Z depth. The limb is exactly round at every distance, there is no mesh, and other geometry (orbit lines, ships) depth-tests against it (ran: run 32, both scenes). Bodies below a few pixels become point sprites (assumed). Cost: about 60 lines of shader per body type.
- Earth's surface: NASA's Blue Marble Next Generation, an equirectangular colour texture, sampled by latitude and longitude from a polynomial `atan2` (the shader language has no `atan`/`asin`); the 5400×2700 JPEG (1.8 MB) looked good from 629 km and 34 Mm (ran: run 32). Larger versions exist for close views: 21600×10800 for the whole Earth (~2 km per pixel) and 8 tiles of 21600×21600 (~0.5 km per pixel) (read: the download page's links). A cube map built from it at load time would avoid the polynomial and the poles' pinch (assumed).
- Clouds, night lights, atmosphere: an additive rim term on the lit surface shows the idea (ran: run 32); a halo past the limb needs a second, slightly larger impostor with additive blending, and night lights a second texture (NASA "Black Marble") (assumed). Procedural continents are cut in favour of the texture: real Earth is recognisable, procedural would cost more and look less real (recommendation for `docs/design.md`).
- Moon and planets: the same impostor with their own textures (NASA/USGS maps; licences checked when chosen) or flat albedo colours where a body is only seen from afar (assumed).
- Licence of NASA imagery: "NASA content – images, audio, video, and media files used in the rendition of 3-dimensional models, such as texture maps and polygon data in any format – generally are not subject to copyright in the United States. You may use this material for educational or informational purposes, including photo collections, textbooks, public exhibits, computer graphical simulations and Internet Web pages." and "NASA should be acknowledged as the source of the material." (NASA, "Guidelines for using images and media", `https://www.nasa.gov/nasa-brand-center/images-and-media/`, fetched 2026-10-14). The NASA logo and any implied endorsement are off limits under the same guidelines. The Blue Marble pages credit "NASA Earth Observatory" (read). So: ship or download the texture, credit "NASA Earth Observatory (Blue Marble Next Generation)" in the credits and the repo, no NASA logos.
- Cost: impostor shader and texture loading ~150 lines; textures add a download or a Nix fetch of a few MB.

### Starfield

- luv has: Luvcraft's stars are hashed points in a procedural night sky (`star-light` in `../luv/luvcraft/shaders.lisp`), not real positions; Luft's "stars" are cubical-topology figures, not astronomy (`../luv/wiki/luft-star-figures.org`) (read). No catalogue, no point topology (read).
- Gap: real constellations matter for a navigation game (the player orients by them) and luv has none.
- Workaround here: a star catalogue converted at build time to a vector of (direction, brightness, colour) and drawn as instanced additive quads at infinity, sized in pixels, brightness from magnitude (ran: run 32 with 4,000 random stars standing in for the catalogue, one instanced draw; not timed).
- Catalogues: HYG (Hipparcos, Yale BSC, Gliese; ~120,000 stars) is under Creative Commons Attribution-ShareAlike 4.0 ("This work is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License", `LICENSE` of `github.com/astronexus/HYG-Database`, which now points to `codeberg.org/astronexus/hyg`; read 2026-10-14). ShareAlike applies to the derived star table we ship, not to the game's code (assumed; `docs/architecture.md` "Assets" ships it so). The Yale Bright Star Catalogue (~9,100 stars to magnitude 6.5) would suffice for naked-eye stars; its licence was not checked here.
- Recommendation: HYG filtered to magnitude ≤ 6.5 (~8,900 stars), with attribution and the derived table under CC BY-SA 4.0 in its own directory; the Yale BSC is the fallback if ShareAlike is unwanted.
- Cost: a converter script, one buffer, two small shaders (~80 lines).

### Sun glare and brightness

- luv has: nothing in base `luv`; Luvcraft has an HDR `:rgba16-float` scene, an exposure knob, ACES filmic tone mapping (`aces-filmic`), a quarter-resolution bloom chain with threshold and gain, sun shafts, and a sky shader with a sun disc and Henyey-Greenstein glow (read: `../luv/luvcraft/render.lisp`, `shaders.lisp`, `../luv/wiki/sky-and-light.org`). None of it loads without `luvcraft/core`.
- Gap: space has no sky; the Sun's disc is about 10^5 to 10^6 times brighter than sunlit Earth, sunlit Earth far brighter than stars; a game needs HDR, a tone curve, glare and an exposure rule.
- Workaround here: an HDR `:rgba16-float` scene target, a tone-map pass to the canvas (ACES or similar, written here after Luvcraft's), a small bloom chain (threshold, downsample, blur, add), and the Sun drawn as an impostor disc with a large radiance so bloom makes the glare. Exposure follows the scene: a fixed "sunlit" exposure when the Sun or a lit body fills part of the view, so stars fade out as they do in real photographs, and a deep-space exposure otherwise, blended over a second or two. Cost: ~250 lines (render targets, three or four passes).
- Built (`task 111: post.lisp: HDR target, bloom, tone map, exposure`, `decision 76: Frame passes, radiance scale and exposure`): `src/post.lisp` (~150 lines) and the post shaders in `src/shaders.lisp` (~90 lines). The scene pass draws into `:hdr` (`:rgba16-float`); 4 passes down (13-tap) to 1/16, 3 passes up (tent of the smaller level plus the downsampled level of the same size, into separate up levels because passes only clear), and one pass on the canvas or capture target: the scene mixed with the first up level at `*bloom-strength*`, exposure, ACES, sRGB encoding when the target is not `-srgb`, then the overlay in the same pass. Every pass a live pipeline; inputs clamped to [0, 6×10^4] with NaN to 0 (ran: `scripts/test` on lavapipe, `tests/render.lisp` `render-frames`; on the Intel iGPU in a 959×1010 window ~127 frames/s with the cadence clock lifted, the 60 frames/s cadence held).
- Measured: a 2×2 px quad of 4×10^4 at the deep exposure glows ~50 px wide at strength 0.04; `(setf *bloom-strength* 0.3)` widens it on the next frame; a redefined `tone-map-fragment` shows on the next frame (ran: `./sly play`).
- Finding for the Earth part: at `*exposure-sunlit*` 1.0 the Blue Marble's open ocean (sRGB about (2, 5, 20), linear ~0.007) falls into ACES's toe (slope ~0.2) and shows at a few of 255; the start view's Earth is dark navy. The texture is display-referred, not albedo; the Earth part retunes (an albedo gain, the atmosphere) as `decision 76` allows (ran: `scripts/screenshot`).
- The "too close to the Sun" danger is a game rule, not rendering; glare and a heat tint on the cockpit only show it (assumed).

### Orbit lines and markers

- luv has: triangle lists and strips only; no line or point topology in the Vulkan and Metal backends (read); premultiplied-alpha blending, which with alpha 0 adds colour (ran: run 32).
- Gap: orbit lines, trajectories and markers need lines of constant pixel width.
- Workaround here: screen-space ribbons: one instance per segment with both endpoints (camera-relative, from double on the CPU), a 6-vertex quad expanded in the vertex shader to a fixed pixel width, endpoints behind the camera clipped to a near `w`, additive blending, depth test against bodies without writing (ran: run 32, a 256-segment orbit 3 px wide, hidden behind Earth where it should be). Markers are the same quads around a point, with a shape in the fragment shader (assumed). Cost: ~100 lines; long orbits need adaptive segment counts near the camera (assumed).

### HUD and instruments

- luv has: Slug glyph rendering from TrueType outlines, HarfBuzz shaping and an outline cache in base `luv` (`../luv/hal/shader/slug*.lisp`, ~1,700 lines) (read); the instanced text run that turns them into quads with an atlas (`make-world-text-run`, ~390 lines) is Luvcraft code (`../luv/luvcraft/text.lisp`) (read). Ghostty (`luv/ghostty`) emulates a terminal fed with bytes, no shell needed (`make-terminal`, `write-terminal` in `../luv/ghostty/terminal.lisp`), but drawing its grid is Luvcraft's terminal wall (~2,150 lines) (read). McCLIM panes on GPU surfaces (`luv/mcclim`) are developer UI (read). No screen-space 2D layer in base `luv` (read).
- Gap: no ready text or 2D layer a game can load without `luvcraft/core`.
- Workaround here: instruments drawn on in-world cockpit screens (fits the DIY-cockpit design): each screen is a dark quad in the cockpit with its contents (text, a small orbit plot) drawn directly on its plane in the scene pass: Slug text through an adapted copy of Luvcraft's `text.lisp`, plot lines as ribbons; no screen textures, the grapple camera the only render target (`decision 36: Cockpit instruments: Slug screens, dot-matrix strip, gauges and lamps; Ghostty out`). The overlay's screen-space quads serve the outside-view readout (`decision 7: Placeholder labels: dot-matrix overlay quads`). Ghostty as a terminal screen is cut by the same decision: its renderer would be a second copy of Luvcraft code.
- Cost: the text copy ~400 lines plus the screens' layout and ribbons; this is the largest rendering workaround.

### Instancing many objects

- luv has: instanced `draw`/`draw-indexed` and per-instance vertex buffers (`:step-mode :instance`); storage buffers read by `:instance-index` (`../luv/hal/shaderc/examples/textured-instances.lisp`) (ran: run 32; read).
- Gap: none in the HAL; no ready instanced mesh renderer.
- Workaround here: asteroids as one or a few rock meshes (generated in Lisp, displaced icospheres) drawn with per-instance position, scale and rotation, camera-relative in single float; distant asteroids as sprites or not at all (the real belt is empty to the eye) (assumed). Markers and stars as instanced quads (ran). Cost: ~100 lines per instanced kind.

### Audio

- luv has: `audio-sink` in base `luv` (`../luv/hal/sdl/audio.lisp`, exported from `luv`): one SDL3 playback stream on the default device; the caller pushes mono single-float frames with a left/right pan (`push-audio-sink-mono`), sets a gain, and reads a played-frames clock; SDL resamples and paces it on its own thread (read). Luvcraft uses it for film sound, fed by FFmpeg (`../luv/luvcraft/sound.lisp`) (read). No mixer, no sound-file loading in `luv`; luv's flake patches SDL_mixer out of cl-sdl3 and offers `sdl3-mixer` only in the opt-in `mixer` dev shell (`../luv/flake.nix`) (read). cl-sdl3 binds `SDL_LoadWAV_IO` (read).
- Gap: no mixing of several sounds and no loading of sound assets in the default environment.
- Workaround here: one `audio-sink` fed by a small mixer in Lisp on its own thread, as Luvcraft's film pump does: a quarter second queued ahead, voices summed into one buffer per push. Ship sounds are synthesised, not sampled (engine rumble as filtered noise scaled by throttle, RCS puffs, valve clicks, alarm tones, radio static), which suits a DIY cockpit in vacuum (only what travels through the hull is heard) and needs no asset loading; radio voices, if ever wanted, as WAV through `SDL_LoadWAV_IO` (assumed). The sink itself works: opened, fed 12,000 frames and closed on SDL's `dummy` driver, its played-frames clock advancing (ran: run 35). `sdl3-mixer` is not used: it is outside luv's main shell and its application closure.
- Headless agents have no audio device; the game must run with sound off when `open-audio-sink` fails, and tests never open a real one. `open-audio-sink` signals an error when SDL's audio cannot start (an unknown `SDL_AUDIODRIVER`: "SDL audio initialization failed"), so a `handler-case` around it is the switch; `SDL_AUDIODRIVER=dummy` gives tests a working sink without sound (ran: run 35).
- Cost: ~150 lines for the mixer and a few voices; deferred to the design's scope (recommendation: after flight and cockpit work, before release).

### Save files

- luv has: nothing in base `luv`; Luvcraft writes one printable s-expression per world to `$XDG_DATA_HOME/luvcraft/worlds/default.sexp` (falling back to `~/.local/share/`), reads it with `*read-eval*` off and a fresh readtable, rejects files with more than one form, and replaces the file atomically through a temporary file and a rename (`../luv/luvcraft/play.lisp`, `persistence.lisp`) (read; run 6 loaded such a save).
- Gap: none that luv should fill; the pattern is a few dozen lines.
- Workaround here: the same pattern under `$XDG_DATA_HOME/luv-in-space/` (or `~/.local/share/luv-in-space/`): a versioned plist (format version, game time, ship state in SI `double-float`, quest progress), read safely, written atomically, at checkpoints rather than continuously (assumed). Tests point `XDG_DATA_HOME` at a temp directory. macOS gets the same path (no `~/Library/Application Support`) for now (recommendation).
- Cost: ~100 lines plus a round-trip test.

### Fixed physics step and time warp

- luv has: no simulation clock; the canvas's clocks drive frames (demand, cadence, presentation) and the frame function gets the elapsed seconds (read: `../luv/hal/canvas.lisp`). Luvcraft steps its player at a fixed 1/120 s with a `double-float` accumulator inside the frame function, with no cap on steps per frame and no interpolation between steps (`../luv/luvcraft/render.lisp`, `simulation.lisp`) (read). `luv/production` hands latest values from a worker thread to frames (read).
- Gap: orbital flight needs a fixed step decoupled from the frame rate and time warp up to ×10^5 or more; at warp, a fixed small step would need millions of steps per second.
- Workaround here: the frame function adds `elapsed × warp` to a `double-float` accumulator and runs fixed steps, capped per frame (the sim slows rather than stalls); planets and moons are always on rails from ephemerides. Decided since in `docs/architecture.md` "Time and integration": the ship is never on rails but integrated at every warp level with a step that grows with distance from bodies (`decision 16: Integrator and simulation clock`, `decision 17: Gravity model`, `decision 19: Time-warp rules`); rendering interpolates between the last two steps; trajectory prediction runs on the frame thread within a budget, so `luv/production` is not a dependency (`decision 20: Trajectory prediction on the frame thread`).
- Cost: ~200 lines for the clock, warp rules and Kepler propagation, excluding the integrator.

### Gamepad and joystick

- luv has: keyboard, mouse buttons, wheel and relative pointer mode through its canvas events; no gamepad or joystick; luv calls `SDL_Init` with video only and its event handler ignores gamepad events (`../luv/hal/sdl/canvas.lisp`); cl-sdl3, which `luv` loads, binds SDL3's gamepad API (`SDL_GetGamepads`, `SDL_OpenGamepad`, axis and button state) (read).
- Gap: a flight game benefits from analogue sticks for attitude and translation.
- Workaround here: keyboard and mouse first; a gamepad later by initialising SDL's gamepad subsystem on the canvas thread (`request-canvas-frame`) and polling axis and button state each frame through cl-sdl3, since luv's event loop drops the events but SDL updates the state while it pumps events (assumed; no gamepad attached here, not tried).
- Cost: ~80 lines; deferred (recommendation: milestone 3 or 6, per `docs/design.md`).

### Other findings

- Back-face culling: not available (read: `../luv/hal/vulkan/native.lisp`); with a broken depth range the far hemisphere of a sphere shows through (ran: run 30). Planet impostors and reversed-Z sidestep it for bodies (ran: run 32); closed ship meshes rely on depth alone, which costs overdraw only (assumed).
- Clip-space y: a game negates clip y on the Vulkan path (ran: run 29); keep it in one projection function so a Metal check later touches one place (recommendation).
- GPU choice: luv takes the first Vulkan device; `VK_DRIVER_FILES` picks another (ran: runs 7, 8, 18). We avoid mesh and task shaders so any Vulkan 1.4 device works (recommendation).
- `LD_LIBRARY_PATH` from the host breaks luv launchers (ran: run 5); our launcher, scripts and checks unset it (recommendation; see Packaging).
- Shader-language gaps met: no `atan`/`asin`/`acos`/`tan`, no scalar–vector broadcasting for `-`, only `let*` at statement level (ran: run 32). Workarounds: shader functions for `atan2` and friends, explicit `vec3` constants (ran).
- Live-artifact failures are silent: a broken shader redefinition keeps the last good pipeline and reports nowhere but `luv:live-artifact-diagnostic` (ran: run 21). Workaround: our `./sly status` prints failed live artifacts (`task 28: Live image: report failed live artifacts`).
- The luv binary cache holds luv's dependencies but not its game outputs at the pinned revision, so our image builds locally, ~40 s (ran: runs 2–4).

## What this means for luv-in-space

The game is its own renderer and simulation on luv's HAL, shader language, SDL3 canvas and capture path, with luv consumed as a pinned flake input. luv gives a sound base for a small, explicit renderer and a strong agent workflow (headless screenshots, live image); everything game-shaped (camera, depth mapping, planets, stars, lines, text panels, HDR, physics, audio mixing, saves) is written here. This summary is what `plan 8: walking skeleton` and the architecture, design and story parts of `plan 1: masterplan` build on.

- Systems to load: `luv` only; tests add `luv/test-support`; nothing from `luvcraft/*` or `luft/*`, whose ideas (sky, bloom, text runs, terminal wall, saves) are copied and adapted instead (`decision 3: luv ASDF systems the game depends on`).
- Consuming luv: flake input `github:mbrock/luv` at `6abc9d1`, dev shell and package from `luv.lib.environmentFor`, a copy of luv's `applicationPackage` recipe here, luv's sources on `CL_SOURCE_REGISTRY`, luv's binary cache, launchers without `LD_LIBRARY_PATH` (`decision 1: How the game consumes luv`).
- Build and run: `nix build` / `nix run` of this repo's package; first build ~40 s for our image on top of cached dependencies (ran for luv's games, runs 2–6).
- Test: Parachute via `luv/test-support`, `asdf:test-system` with a non-zero exit on failure; the GPU test renders on a hidden canvas with lavapipe, as a `nix flake check` in the sandbox (`decision 2: Screenshots and GPU test: offscreen capture on a hidden canvas, headless on lavapipe`).
- Screenshot: `capture-application-screenshot` on a hidden canvas into a PNG; works with no display (ran: runs 24–27, 31, 33).
- Live image: `./sly` here wraps luv's client from the flake input with our own `play`/`screenshot`/`stop-playing` (`task 26: Live image dev loop for luv-in-space`; run 34).
- Depth: reversed-Z, `depth32-float`, infinite far plane, near 0.1 m, in every pass; stars first at infinity (`decision 4: Depth strategy`).
- Precision: `double-float` world state on the CPU, camera-relative single floats to the GPU, cancelling quantities computed in double (`decision 5: Precision of positions`).
- Rendering approach: planets, moons and the Sun as ray-traced sphere impostors writing `:frag-depth`; Earth from NASA Blue Marble (credited); stars from HYG ≤ magnitude 6.5 as instanced additive quads (CC BY-SA 4.0 for the derived table); orbit lines and markers as screen-space ribbons; an HDR target, bloom and tone map after Luvcraft's; instruments on in-world cockpit screens with Slug text (a copy of Luvcraft's `text.lisp`) drawn on their planes, the grapple camera the only render target (`decision 36: Cockpit instruments: Slug screens, dot-matrix strip, gauges and lamps; Ghostty out`). Largest rendering cost: the text and the screens.
- Simulation: fixed RK4 steps with a `double-float` accumulator; the ship integrated at every warp level, bodies on rails from ephemerides; trajectory prediction on the frame thread; `luv/arithmetic` quantities at boundaries, plain `double-float` in the integrator (`docs/architecture.md` "Time and integration"; `decision 16: Integrator and simulation clock` to `decision 20: Trajectory prediction on the frame thread`).
- Input: keyboard and mouse through luv's canvas events; gamepad via SDL directly, deferred.
- Audio: synthesised ship sounds through one `audio-sink` and a Lisp mixer, off when no device; deferred.
- Saves: a versioned s-expression under `$XDG_DATA_HOME/luv-in-space/`, atomic writes.
- Frame budget: assume an Intel iGPU (Luvcraft's full pipeline ran at 24–36 Hz on one here); agents render on lavapipe, slowly (ran: runs 6, 28, 33).
- Platforms: `x86_64-linux` built and tested here; `aarch64-linux` and `aarch64-darwin` evaluated only; no Windows (`decision 6: Target platforms`).
- Cut or deferred: Windows (luv has no backend); mesh and task shaders (not on every iGPU); logarithmic depth and layered depth passes (reversed-Z suffices); procedural continents (the real Earth texture is cheaper and better); `sdl3-mixer` (outside luv's main closure); Ghostty terminal screens (a second copy of Luvcraft's terminal wall; cut by `decision 36: Cockpit instruments: Slug screens, dot-matrix strip, gauges and lamps; Ghostty out`); loading `luvcraft/core` for its sky or physics (pulls in the whole block game).

## Runs

Filled in by `task 17: Build and run luv's programs and test suite; record the runs`, `task 18: Try luv's live image, screenshots and headless rendering; record the findings` and `task 19: Probe program: a lit sphere in a luv window plus a screenshot`: each run with command, time and outcome. Runs 1–14 are from 2026-10-14 at luv `6abc9d1`; `R` stands for that revision. Program runs used `env -u LD_LIBRARY_PATH` from run 6 on (see Packaging); build links went to `/tmp/t17/`, deleted afterwards.

- Run 1: `nix flake show --accept-flake-config path:../luv`: 5 s; outputs `apps`, `devShells`, `lib`, `packages` for `x86_64-linux`, `aarch64-linux`, `aarch64-darwin`; no overlays or checks.
- Run 2: `nix build --accept-flake-config path:../luv#{luvcraft,luft,luv-shaderc} --out-link /tmp/…`: 47 s, 36 s, 11 s; all built locally (nothing substituted for the game outputs).
- Run 3: `nix build --accept-flake-config github:mbrock/luv/R#{luvcraft,luft,luv-shaderc}`: 16 s (already in the store from an earlier build), 36 s (built locally), 0.8 s (same path as run 2); store paths differ from run 2 except `luv-shaderc`.
- Run 4: `curl https://luv.swa.sh/nix-cache/<hash>.narinfo` for the `luvcraft`, `luft`, `luv-shaderc` and `lisp` paths of runs 2–3: all 404; `nix-cache-info` answers, so the cache is up but holds no x86_64-linux luv outputs at R.
- Run 5: `nix run --accept-flake-config github:mbrock/luv/R` with the session's `LD_LIBRARY_PATH` (a host `alsa-lib`): exits in under a second, `Error opening shared object "libSDL3.so": … version 'GLIBC_2.43' not found (required by …alsa-lib…/libasound.so.2)`; no window.
- Run 6: the same without `LD_LIBRARY_PATH`: Luvcraft opens "luvcraft — walk, jump, mine, and build" on Vulkan at 1382×864 within ~1 s after the store paths are present, compiles 47 shader modules in 1.5 s, loads the saved world from `~/.local/share/luvcraft/worlds/default.sexp`, opens Slynk on a local port; frame pacer settles at 24–36 Hz (Intel iGPU). Stopped after 30 s with `kill $!`; log "canvas closed".
- Run 7: `nix run … github:mbrock/luv/R#luft`: window opens, then exits after 3 loop iterations with `:VULKAN-MESH-SHADER-RUNTIME-UNAVAILABLE` (Intel iGPU picked).
- Run 8: the same with `VK_DRIVER_FILES=…/nvidia_icd.json`: Luft runs on the RTX 4060 (1533 loop iterations in 30 s, 1100×800 window resized to 959×1010 by the compositor); stopped by PID, "canvas closed".
- Run 9: Luvcraft's built launcher with `VK_DRIVER_FILES=…/nvidia_icd.json`, 25 s: frame pacer 16–22 ms GPU work, 36–48 Hz; stopped by PID.
- Run 10: `luv-shaderc --out /tmp/… ../luv/hal/shaderc/examples/sky.lisp`: 0.3 s; "1 program, 10 files": `sky.{vertex,fragment}.{spv,metal,hlsl,wgsl}`, `sky.json` (stages, entry points, resources), `sky.hh` (C++ reflection); `spirv-val --target-env vulkan1.3` accepts both SPIR-V modules. `--target msl|hlsl|spirv|wgsl` limits the outputs.
- Run 11: `make test` in a `/tmp` clone of `../luv` at R (cold: first `nix develop` of the default shell, dependencies from `luv.swa.sh` and `cache.nixos.org`): 284 s wall, exit 0; Python lock tests "Ran 4 tests … OK", "parinfer: strict check passed", 31 suites (34 parallel tasks, 16 SBCL workers) all PASS in 40.7 s; slowest `luvcraft/core/test [1/2]` 9.4 s.
- Run 12: `make test-suite` in the same clone (warm): 15.6 s wall, exit 0; 31 suites (37 tasks) all PASS in 14.0 s.
- Run 13: `make luft-test` in the same clone: 1.9 s wall, exit 0; "LUFT: 611001 star checks passed."
- Run 14: `nix eval github:mbrock/luv/R#lib --apply …`: `lib` = `{ environmentFor }`; its attributes listed under Packaging.
- Afterwards: `git -C ../luv status` shows only the pre-existing `result` symlink; no `sbcl`, `luvcraft` or `luft` process left.

Runs 15–28 are from 2026-10-14 at R, by `task 18: Try luv's live image, screenshots and headless rendering; record the findings`, in a `/tmp/t18` clone of `../luv` with `LD_LIBRARY_PATH` unset; screenshots were read, then deleted with the clone.

- Run 15: `./sly start`: 26 s wall; builds `build/sly-client` and `build/sly-dependencies.core`, compiles `luv-workbench` (287 files, 38 systems, 19 s), Slynk on a kernel-assigned loopback port, image PID shown by `./sly list`.
- Run 16: `./sly status` ("Nothing is playing"), `./sly eval` of `uiop:getenv`: the image has the dev shell's `LD_LIBRARY_PATH` and `WAYLAND_DISPLAY`, no `VK_DRIVER_FILES`.
- Run 17: `./sly play luft` on the default (Intel) GPU: window opens and closes after 3 loop iterations, `GPU-REQUEST-ERROR` during mesh pipeline creation, backtrace, "No input; aborting evaluation"; image still up.
- Run 18: `./sly eval '(sb-posix:setenv "VK_DRIVER_FILES" "…/nvidia_icd.json" 1)'`, then `./sly play luft`: 4.1 s to a running viewer; `./sly status` "LUFT is playing … canvas loop is healthy (frame, 516 iterations, 497 frames)".
- Run 19: `./sly screenshot /tmp/t18/luft-1.png`: 1.3 s, 959×1010 `:bgra8-unorm-srgb`; a grey blocky cliff face with grass terraces at its foot, a sandy path and grass in the foreground, blue sky with soft clouds, crosshair centred, status line "LUFT · mode 1P · terrain · ready · pid … · fps 60 · heap 500M".
- Run 20: `./sly eval '(load "/tmp/t18/redef.lisp")'` redefining Luft's `paper-grade` shader function (luminance mapped to red): 2.7 s; the next screenshot shows the same scene in reds and browns.
- Run 21: redefinition calling an unknown shader operator: the red frame stays; status healthy, `./sly failures` "No failures are retained"; `luv:live-artifact-diagnostic` reports "LUFT renderer cohort" `:failed`, "Cannot understand shader expression … UNKNOWN-OPERATOR".
- Run 22: `sb-int:encapsulate` of Luft's per-frame `prepare-viewer-frame-renderer` with an error: status "frames are PARKED: a frame failed at … in frame (loop iteration 5161)"; `./sly failures` shows the message and backtrace; `./sly screenshot` fails while parked; after `unencapsulate`, `./sly resume` → "healthy"; reloading `luft/render/shaders.lisp` restored the original colours (screenshot as run 19, clouds moved).
- Run 23: `./sly stop-playing`: 1.6 s; `(luv:open-canvases)` → `NIL`.
- Run 24: a probe system in `/tmp/t18/probe` (appendix) loaded into the running image by `./sly eval`; visible window captured at 959×1010, hidden canvas at 320×200; both PNGs uniform (255, 188, 89).
- Run 25: `./sly stop` (PID gone), then `LUV_SLY_SYSTEM=t18-probe ./sly start --name probe` from outside the shell: the variable was dropped, the image loaded `luv-workbench`; inside `nix develop` with `LUV_SLY_SYSTEM` and `CL_SOURCE_REGISTRY` exported: 4 s start, image with `T18-PROBE` and without `LUVCRAFT`/`LUFT`; `./sly screenshot` and `stop-playing` fail (reader package error), capture by `eval` works; `./sly stop`.
- Run 26: `env -u DISPLAY -u WAYLAND_DISPLAY LUV_PROJECT_ROOT=… nix develop … -c sbcl --script headless.lisp out.png`: `SDL_VIDEODRIVER=offscreen`, `VK_DRIVER_FILES=…/lvp_icd.x86_64.json`; hidden 320×200 canvas, PNG (255, 188, 89); ~2 s for canvas, frames and capture. Without `LUV_PROJECT_ROOT` the script failed while loading the probe (luv not on the source registry).
- Run 27: `nix build --impure -f sandbox.nix`: a `runCommand` with luv's slim development environment and hook, running the same script in the sandbox; "no /dev/dri", lavapipe, PNG in `$out` with the expected pixel; 21 s total.
- Run 28: `env -u DISPLAY -u WAYLAND_DISPLAY SDL_VIDEODRIVER=offscreen VK_DRIVER_FILES=…lvp… luvcraft --smoke-test out.png` (built `#luvcraft` at R): exit 0 in 8.2 s; 960×640 frame: floating dark-stone islands with glowing cyan blocks under a pale sky, the player's hands, the hotbar with ten block icons and "Stone", "Creative · F6", status line with "fps 0".
- Afterwards: no `sbcl` or Swash host process left (`ps`); `./sly list --all` shows all three images `exited`; `git -C ../luv status` unchanged; `/tmp/t18` deleted.

Runs 29–31 are from 2026-10-14 at R, by `task 19: Probe program: a lit sphere in a luv window plus a screenshot`, with the probe of "Appendix: probe program" in a `mktemp -d` directory, loaded from luv's default dev shell straight against `../luv` (`LUV_PROJECT_ROOT=../luv`; FASLs go to `~/.cache`, nothing was written into `../luv`).

- Run 29: first attempts, visible windows: `GPU request failed during :CREATE: :UNSUPPORTED-RENDER-PASS` (the swapchain texture passed as `:view`); then "requires usage :RENDER-ATTACHMENT … but was created with (:COPY-DST)" and "has already been destroyed during :SUBMIT" (a view destroyed right after `end-pass`); then a lit sphere, upside down (clip y). Each fixed as noted under Rendering.
- Run 30: `env -u LD_LIBRARY_PATH LUV_PROJECT_ROOT=../luv nix develop path:../luv -c sbcl --script run.lisp OUT t`: 11 s wall for five scenes (load, five windows, five captures); each scene 1.6–1.7 s for open, 1 s of frames, capture, close; visible windows captured at the compositor's 959×1010.
  - `small-standard`: a blue sphere on black, lit from the upper left, terminator on the right, dark side faintly visible (3 % ambient).
  - `earth-standard`: Earth's limb across the lower half, black space above; lighter and darker facet patches scattered over the surface, the far hemisphere showing through (no culling, depth precision gone).
  - `earth-reversed`: the same view, clean surface; the limb is visibly polygonal (64 slices at 7,000 km).
  - `moon-standard`: all black. `moon-reversed`: a lit disc a few pixels wide in the centre (1,737 km radius at 384,400 km).
- Run 31: the same with `env -u DISPLAY -u WAYLAND_DISPLAY` and hidden canvases (`OUT nil`): the hook chose `SDL_VIDEODRIVER=offscreen` and lavapipe; 10 s wall; five 640×400 PNGs matching run 30 (each scene 1.5–1.6 s).
- Afterwards: no `sbcl` process left; `git -C ../luv status` shows only the old `result`; the temp dir deleted.

Runs 32–33 are from 2026-10-14 at R, by `task 20: Assess luv's rendering gaps for solar-system scale, each with a workaround`, with the gap probe of "Appendix: gap probe" in a `mktemp -d` directory, loaded the same way as runs 29–31; the Earth texture was NASA's Blue Marble Next Generation, April, 5400×2700 JPEG (`world.200404.3x5400x2700.jpg` from `assets.science.nasa.gov`, 1.8 MB).

- Run 32: `env -u LD_LIBRARY_PATH LUV_PROJECT_ROOT=../luv nix develop path:../luv -c sbcl --script run.lisp OUT nil earth.jpg`, hidden canvases on the desktop's GPU: texture decoded and copied in 0.2 s; each of two scenes 1.6 s (open, 1 s of frames, capture, close). Three attempts failed first: a `let` in a shader, `(- 0.0 ray)` on a `vec3`, and a flat vector for `write-texture`; one more failed at `set-bind-group` before `set-pipeline`.
  - `leo` (629 km altitude, looking up at the limb): textured Earth with a smooth, round limb and a pale blue rim; stars above it; the 2,000 km orbit as a 3-pixel orange line across the top.
  - `far` (34 Mm from Earth): the whole Earth, North America and the Caribbean the right way round, snow on Canada, the night side dark; the inclined orbit ring passes in front of Earth and its far half is hidden behind it (depth test against the impostor's written depth); a field of stars of varying brightness.
- Run 33: the same with `env -u DISPLAY -u WAYLAND_DISPLAY` (hook chose `SDL_VIDEODRIVER=offscreen` and lavapipe): the same two pictures; first scene 9.1 s (lavapipe compiling pipelines and taking the 58 MB texture), second 1.5 s.
- Afterwards: no `sbcl` process left; `git -C ../luv status` shows only the old `result`; the temp dir with texture and screenshots deleted.

Run 34 is from 2026-10-14 at R, by `task 26: Live image dev loop for luv-in-space`, in this checkout with its own `./sly`.

- Run 34: `./sly start` 8.4 s cold (builds `build/sly-client` and `build/sly-dependencies.core`, loads `luv-in-space` from cached FASLs), PID in `./sly list`; `./sly play` → `:PLAYING`; `./sly status` "The canvas loop is healthy (waiting, 8440 iterations, 252 frames)"; `./sly screenshot` 1.3 s, 959×1010: the placeholder Earth lit from the left; `./sly eval` of `sun-direction` and a `uniform-data` using it (`--package LUV-IN-SPACE`): the next screenshot has the lit side on the right; `./sly stop-playing`, `./sly status` "Nothing is playing"; `./sly stop`, the PID gone, `./sly list` "No running Swash-managed Lisps". Screenshots deleted.

Run 35 is from 2026-10-14 at R, by `task 63: Check luv capabilities, wishlist and architecture against each other`, in this checkout's dev shell.

- Run 35: `env -u LD_LIBRARY_PATH SDL_AUDIODRIVER=dummy nix develop -c sbcl --non-interactive --load audio.lisp` (a throwaway script in `/tmp`, loading `luv`): `open-audio-sink` in 5 ms, `push-audio-sink-mono` of 12,000 zero frames at gain 0, 0.5 s later `audio-sink-played-frames` 12,000 and queued 0, `close-audio-sink`. The same with `SDL_AUDIODRIVER=nosuchdriver`: `open-audio-sink` signals "SDL audio initialization failed: Audio target 'nosuchdriver' not available". Script deleted.

## Appendix: screenshot probe

The probe of runs 24–27: an ASDF system `t18-probe` (`:depends-on ("luv")`, one file) that opens a canvas, clears it every frame and takes an offscreen screenshot. It seeds the skeleton's screenshot and GPU test; `task 19: Probe program: a lit sphere in a luv window plus a screenshot` extends it to a sphere.

```lisp
(defpackage #:t18-probe (:use #:cl) (:export #:start #:stop #:screenshot #:*app*))
(in-package #:t18-probe)

(defclass app () ((canvas :initarg :canvas :reader app-canvas)
                  (device :initarg :device :reader app-device)
                  (color :initform (vector 1.0 0.5 0.1 1.0) :accessor app-color)))
(defvar *app* nil)

(defmethod luv:capture-canvas ((app app)) (app-canvas app))
(defmethod luv:encode-capture-frame ((app app) capture encoder target extent)
  (declare (ignore capture extent))
  (luv:encode encoder (luv:make-gpu-clear-texture-command :texture target :color (app-color app))))

(defun draw (app)
  (let ((c (app-color app)))
    (luv:render-canvas-color (luv:canvas-context (app-canvas app)) (aref c 0) (aref c 1) (aref c 2))))

(defun start (&key (visible t))
  (let* ((canvas (luv:make-sdl-canvas :title "t18 probe" :width 320 :height 200 :visible-p visible))
         (_ (luv:open-canvas canvas))
         (device (luv:request-gpu-device luv:*gpu-provider* (luv:make-device-descriptor :label "t18")))
         (app (make-instance 'app :canvas canvas :device device)))
    (declare (ignore _))
    (luv:make-canvas-context canvas luv:*gpu-provider* (luv:make-canvas-configuration :device device))
    (setf (luv:canvas-clock canvas)
          (luv:make-cadence-clock (lambda (native ts) (declare (ignore native ts)) (draw app))
                                  :frames-per-second 30))
    (setf *app* app)))

(defun screenshot (path) (luv:capture-application-screenshot *app* path))

(defun stop (&optional (app *app*))
  (let ((canvas (app-canvas app)))
    (when (eq :open (luv:canvas-state canvas))
      (setf (luv:canvas-clock canvas) (luv:make-demand-clock)))
    (luv:close-canvas canvas)
    (luv:destroy (app-device app))
    (setf *app* nil)
    (values)))
```

- Headless script: `(asdf:load-system "t18-probe")`, then `(luv:call-with-sdl-main-thread (lambda () (t18-probe:start :visible nil) (sleep 1) (t18-probe:screenshot path) (t18-probe:stop)))`, `(uiop:quit 0)`.
- Sandbox derivation: `(builtins.getFlake …).lib.environmentFor "x86_64-linux"` as `e`; `e.pkgs.runCommand "…" (e.slimDevelopmentEnvironment // { nativeBuildInputs = e.slimDevelopmentPackages; })` with a script that sets `HOME=$TMPDIR`, `LUV_PROJECT_ROOT=<luv source>`, unsets `DISPLAY`/`WAYLAND_DISPLAY`, runs `e.slimDevelopmentEnvironmentHook` and `luv_activate_native_environment`, then `sbcl --script`.

## Appendix: probe program

The probe of runs 29–31, by `task 19: Probe program: a lit sphere in a luv window plus a screenshot`: an ASDF system `sphere-probe` (`:depends-on ("luv")`, two files) that draws a UV sphere (32 stacks, 64 slices, 12,288 vertices, unit positions as normals) with a vertex and a fragment shader in luv's shader language, Lambert diffuse light from a sun direction, a camera with camera-relative positions computed in `double-float`, a depth buffer, and an offscreen screenshot. It seeds `plan 8: walking skeleton`.

- Needed systems: only `luv`. Its dependencies bring the shader language (`luv/shader`), its SPIR-V lowering and `luv/arithmetic`; shader files use a package with `(:use #:cl #:luv.shader)` and `(:shadowing-import-from #:luv.shader #:step)`, like luv's `luv.shader-user`.
- What a game writes itself: the projection and camera, mesh generation, the depth target, texture views and their lifetime, pipeline creation (`make-render-pipeline-descriptor`), the frame function (`present-canvas-frame`) and the two capture methods.
- Shaders are compiled at `start` with `make-shader-module-descriptor :language :mathematical :code (sphere-vertex)`; a game that wants live shader edits uses `make-live-shader-pipeline` with `define-shader-method` roles instead (see Rendering) (not tried here).
- Leaks accepted in a probe: the per-frame uniform write while the previous frame may be in flight, depth targets after a resize, capture views; the game keeps per-frame resources in `canvas-frame-resource-cache` slots.
- Run: `env -u LD_LIBRARY_PATH LUV_PROJECT_ROOT=<luv checkout> nix develop --accept-flake-config path:<luv checkout> -c sbcl --script run.lisp OUT-DIR t` (visible windows; `nil` for hidden canvases, and also `-u DISPLAY -u WAYLAND_DISPLAY` for headless). Time: about 2 s per scene after the first load; luv's FASLs were cached from earlier runs.

`sphere-probe.asd`:

```lisp
(defsystem "sphere-probe"
  :depends-on ("luv")
  :serial t
  :components ((:file "shaders") (:file "sphere")))
```

`shaders.lisp`:

```lisp
(defpackage #:sphere-probe.shaders
  (:use #:cl #:luv.shader)
  (:shadowing-import-from #:luv.shader #:step))
(in-package #:sphere-probe.shaders)

;; Unit-sphere vertices; the uniform block places the sphere relative to the
;; camera and projects it through four clip rows.
(define-shader sphere-vertex
    (:stage :vertex
     :inputs ((unit :vec3 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)
               (normal :vec3 :location 0))
     :resources ((frame :uniform-block :binding 0
                  :members ((clip-x :vec4) (clip-y :vec4)
                            (clip-z :vec4) (clip-w :vec4)
                            (body :vec4) (sun :vec4)))))
  (let* ((point (vec4 (+ (swizzle body :xyz) (* unit (swizzle body :w))) 1.0)))
    (set-output clip-position
                (vec4 (dot clip-x point) (dot clip-y point)
                      (dot clip-z point) (dot clip-w point)))
    (set-output normal unit)))

(define-shader sphere-fragment
    (:stage :fragment
     :inputs ((normal :vec3 :location 0))
     :outputs ((color :vec4 :location 0))
     :resources ((frame :uniform-block :binding 0
                  :members ((clip-x :vec4) (clip-y :vec4)
                            (clip-z :vec4) (clip-w :vec4)
                            (body :vec4) (sun :vec4)))))
  (let* ((n (normalize normal))
         (diffuse (clamp (dot n (swizzle sun :xyz)) 0.0 1.0))
         (albedo (vec3 0.25 0.45 0.9)))
    (set-output color (vec4 (+ (* albedo diffuse) (* albedo 0.03)) 1.0))))
```

`sphere.lisp`:

```lisp
(defpackage #:sphere-probe
  (:use #:cl)
  (:export #:start #:stop #:screenshot #:*app*))
(in-package #:sphere-probe)

;;; Scenes: eye, target, body centre and radius in metres (double-float),
;;; near and far planes, and the depth mode (:standard or :reversed).
(defparameter *scenes*
  '(:small (:eye (0d0 0d0 3d0) :target (0d0 0d0 0d0)
            :center (0d0 0d0 0d0) :radius 1d0 :near 0.1d0 :far 100d0)
    ;; Earth from 629 km altitude, looking 70 degrees up from nadir so the
    ;; limb crosses the view.
    :earth (:eye (0d0 0d0 7d6) :target (0d0 0.9397d6 6.658d6)
            :center (0d0 0d0 0d0) :radius 6.371d6 :near 0.1d0 :far 1d7)
    ;; The Moon from Earth's distance.
    :moon (:eye (0d0 0d0 3.844d8) :target (0d0 0d0 0d0)
           :center (0d0 0d0 0d0) :radius 1.737d6 :near 0.1d0 :far 1d9)))

(defun v- (a b) (mapcar #'- a b))
(defun dot3 (a b) (reduce #'+ (mapcar #'* a b)))
(defun cross (a b)
  (destructuring-bind ((ax ay az) (bx by bz)) (list a b)
    (list (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))))
(defun unit (a) (let ((l (sqrt (dot3 a a)))) (mapcar (lambda (x) (/ x l)) a)))

(defun uniform-data (scene depth width height)
  "Clip rows for camera-relative points, the body relative to the eye, the sun."
  (destructuring-bind (&key eye target center radius near far) scene
    (let* ((forward (unit (v- target eye)))
           (right (unit (cross forward '(0d0 1d0 0d0))))
           (up (cross right forward))
           (focal (/ (tan (/ (* 50 (/ pi 180)) 2))))
           (aspect (/ width height))
           (rows
             (list (append (mapcar (lambda (x) (* x (/ focal aspect))) right) '(0))
                   ;; clip y points down on luv's Vulkan path
                   (append (mapcar (lambda (x) (* x (- focal))) up) '(0))
                   (ecase depth
                     ;; depth = far/(far-near) * (1 - near/z), compare :less
                     (:standard
                      (append (mapcar (lambda (x) (* x (/ far (- far near)))) forward)
                              (list (/ (* far near) (- near far)))))
                     ;; infinite reversed-Z: depth = near/z, compare :greater
                     (:reversed (list 0 0 0 near)))
                   (append forward '(0))
                   (append (v- center eye) (list radius))
                   (append (unit '(-1d0 0.6d0 0.8d0)) '(0)))))
      (map '(simple-array single-float (*))
           (lambda (x) (coerce x 'single-float))
           (apply #'append rows)))))

(defun uv-sphere (&key (stacks 32) (slices 64))
  "Triangle-list positions of a unit sphere."
  (let ((data (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0)))
    (flet ((corner (i j)
             (let ((theta (* pi (/ i stacks))) (phi (* 2 pi (/ j slices))))
               (dolist (x (list (* (sin theta) (cos phi)) (cos theta)
                                (* (sin theta) (sin phi))))
                 (vector-push-extend (coerce x 'single-float) data)))))
      (dotimes (i stacks)
        (dotimes (j slices)
          (loop for (a b) in '((0 0) (1 0) (1 1) (0 0) (1 1) (0 1))
                do (corner (+ i a) (+ j b))))))
    (coerce data '(simple-array single-float (*)))))

(defclass app ()
  ((canvas :initarg :canvas :reader app-canvas)
   (device :initarg :device :reader app-device)
   (context :initarg :context :reader app-context)
   (scene :initarg :scene :accessor app-scene)
   (depth :initarg :depth :reader app-depth)
   (vertices :initarg :vertices :reader app-vertices)
   (vertex-count :initarg :vertex-count :reader app-vertex-count)
   (uniforms :initarg :uniforms :reader app-uniforms)
   (layout :initarg :layout :reader app-layout)
   (bind-group :initarg :bind-group :reader app-bind-group)
   (modules :initarg :modules :reader app-modules)
   (pipeline :initarg :pipeline :reader app-pipeline)
   (depth-target :initform nil :accessor app-depth-target)
   (color-views :initform (make-hash-table :test #'eq) :reader app-color-views)))

(defvar *app* nil)

(defun depth-view (app extent)
  "A depth32-float texture and view of EXTENT, remade when the size changes."
  (let ((cached (app-depth-target app)))
    (if (and cached (equal (first cached) extent))
        (third cached)
        (let* ((device (app-device app))
               (texture (luv:create device (luv:make-texture-descriptor
                                            :label "sphere depth" :size extent
                                            :dimensions :2d :format :depth32-float
                                            :usage '(:render-attachment))))
               (view (luv:create device (luv:make-texture-view-descriptor
                                         :texture texture))))
          ;; The old target may still be in flight; a probe just leaks it.
          (setf (app-depth-target app) (list extent texture view))
          view))))

;; A view must outlive the submit; one per swapchain or capture texture.
(defun color-view (app texture)
  (or (gethash texture (app-color-views app))
      (setf (gethash texture (app-color-views app))
            (luv:create (app-device app)
                        (luv:make-texture-view-descriptor :texture texture)))))

(defun encode-scene (app encoder target extent)
  (luv:write-buffer (app-uniforms app)
                    (uniform-data (getf *scenes* (app-scene app)) (app-depth app)
                                  (first extent) (second extent)))
  (let* ((view (color-view app target))
         (pass (luv:begin-render-pass
               encoder
               (luv:make-render-pass-descriptor
                :color-attachments
                `((:view ,view :load-op :clear :store-op :store
                   :clear-value #(0.0 0.0 0.0 1.0)))
                :depth-stencil-attachment
                `(:view ,(depth-view app extent)
                  :depth-load-op :clear :depth-store-op :store
                  :depth-clear-value ,(if (eq (app-depth app) :reversed) 0.0 1.0))))))
    (luv:set-pipeline pass (app-pipeline app))
    (luv:set-bind-group pass 0 (app-bind-group app))
    (luv:set-vertex-buffer pass 0 (app-vertices app))
    (luv:draw pass (app-vertex-count app))
    (luv:end-pass pass)))

(defmethod luv:capture-canvas ((app app)) (app-canvas app))
(defmethod luv:encode-capture-frame ((app app) capture encoder target extent)
  (declare (ignore capture))
  (encode-scene app encoder target extent))

(defun draw (app)
  (let ((context (app-context app)))
    (luv:present-canvas-frame
     context
     (lambda (texture encoder time)
       (declare (ignore time))
       (encode-scene app encoder texture (luv:canvas-extent context))))))

(defun start (&key (visible t) (scene :small) (depth :standard)
                (width 640) (height 400))
  (let* ((canvas (luv:make-sdl-canvas :title "sphere probe" :width width :height height
                                      :visible-p visible))
         (_ (luv:open-canvas canvas))
         (device (luv:request-gpu-device luv:*gpu-provider*
                                         (luv:make-device-descriptor :label "sphere")))
         (context (luv:make-canvas-context canvas luv:*gpu-provider*
                                           (luv:make-canvas-configuration
                                            :device device
                                            :usage '(:render-attachment :copy-dst))))
         (mesh (uv-sphere))
         (vertices (luv:create device (luv:make-buffer-descriptor
                                       :label "sphere" :size (* 4 (length mesh))
                                       :usage '(:vertex :copy-dst))))
         (uniforms (luv:create device (luv:make-buffer-descriptor
                                       :label "frame" :size (* 4 24) :usage '(:uniform))))
         (layout (luv:create device (luv:make-bind-group-layout-descriptor
                                     :entries '((:binding 0 :type :uniform-buffer)))))
         (bind-group (luv:create device (luv:make-bind-group-descriptor
                                         :layout layout
                                         :entries `((:binding 0 :resource ,uniforms)))))
         (vertex-module (luv:create device (luv:make-shader-module-descriptor
                                            :language :mathematical
                                            :code (sphere-probe.shaders::sphere-vertex))))
         (fragment-module (luv:create device (luv:make-shader-module-descriptor
                                              :language :mathematical
                                              :code (sphere-probe.shaders::sphere-fragment))))
         (pipeline
           (luv:create device
                       (luv:make-render-pipeline-descriptor
                        :label "sphere" :layout layout
                        :vertex `(:module ,vertex-module
                                  :buffers ((:array-stride 12
                                             :attributes ((:shader-location 0 :offset 0
                                                           :format :float32x3)))))
                        :fragment `(:module ,fragment-module
                                    :targets ((:format ,(luv:canvas-format context))))
                        :primitive '(:topology :triangle-list)
                        :depth-stencil `(:format :depth32-float :depth-write-enabled t
                                         :depth-compare ,(if (eq depth :reversed)
                                                             :greater :less)))))
         (app (make-instance 'app :canvas canvas :device device :context context
                                  :scene scene :depth depth
                                  :vertices vertices :vertex-count (/ (length mesh) 3)
                                  :uniforms uniforms :layout layout :bind-group bind-group
                                  :modules (list vertex-module fragment-module)
                                  :pipeline pipeline)))
    (declare (ignore _))
    (luv:write-buffer vertices mesh)
    (setf (luv:canvas-clock canvas)
          (luv:make-cadence-clock (lambda (native ts) (declare (ignore native ts)) (draw app))
                                  :frames-per-second 30))
    (setf *app* app)))

(defun screenshot (path) (luv:capture-application-screenshot *app* path))

(defun stop (&optional (app *app*))
  (let ((canvas (app-canvas app)))
    (when (eq :open (luv:canvas-state canvas))
      (setf (luv:canvas-clock canvas) (luv:make-demand-clock)))
    (luv:close-canvas canvas)
    (luv:destroy (app-device app))
    (setf *app* nil)
    (values)))
```

`run.lisp`:

```lisp
;; sbcl --script run.lisp OUT-DIR VISIBLE(t|nil)
(require :asdf)
(asdf:load-asd (merge-pathnames "probe/sphere-probe.asd" *load-truename*))
(asdf:load-system "sphere-probe")
(let ((out (second sb-ext:*posix-argv*))
      (visible (string= "t" (third sb-ext:*posix-argv*))))
  (luv:call-with-sdl-main-thread
   (lambda ()
     (loop for (scene depth) in '((:small :standard) (:earth :standard) (:earth :reversed)
                                    (:moon :standard) (:moon :reversed))
           for name = (format nil "~A/~(~A-~A~).png" out scene depth)
           do (let ((start (get-internal-real-time)))
                (sphere-probe:start :visible visible :scene scene :depth depth)
                (sleep 1)
                (sphere-probe:screenshot name)
                (sphere-probe:stop)
                (format t "~&~A in ~,1F s~%" name
                        (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second)))))))
(uiop:quit 0)
```

## Appendix: gap probe

The probe of runs 32–33, by `task 20: Assess luv's rendering gaps for solar-system scale, each with a workaround`: an ASDF system `gap-probe` (`:depends-on ("luv")`, two files) that draws, in one reversed-Z pass, 4,000 instanced star quads at infinity, Earth as a ray-traced impostor textured with the Blue Marble (loaded through `sdl3-image`), and a 256-segment orbit as instanced screen-space ribbons, then takes an offscreen screenshot. It seeds the planet, star and orbit-line rendering of later milestones. Run as the sphere probe, with the JPEG path as a third argument.

`gap-probe.asd`:

```lisp
(defsystem "gap-probe"
  :depends-on ("luv")
  :serial t
  :components ((:file "shaders") (:file "probe")))
```

`shaders.lisp`:

```lisp
(defpackage #:gap-probe.shaders
  (:use #:cl #:luv.shader)
  (:shadowing-import-from #:luv.shader #:step))
(in-package #:gap-probe.shaders)

(defmacro frame-block ()
  ''(frame :uniform-block :binding 0
     :members ((clip-x :vec4) (clip-y :vec4) (clip-z :vec4) (clip-w :vec4)
               (right :vec4) (up :vec4) (forward :vec4)
               (body :vec4) (extra :vec4) (sun :vec4))))

;; Abramowitz-Stegun 4.4.49 on [0, 1]; the language has no atan or asin.
(define-shader-function atan-unit (x)
  (let* ((x2 (* x x)))
    (* x (+ 0.999866 (* x2 (+ -0.3302995 (* x2 (+ 0.180141 (* x2 (+ -0.085133 (* x2 0.0208351)))))))))))

(define-shader-function atan2 (y x)
  (let* ((ax (abs x)) (ay (abs y))
         (a (atan-unit (/ (min ax ay) (max (max ax ay) 1e-20))))
         (b (if (> ay ax) (- 1.5707964 a) a))
         (c (if (< x 0.0) (- 3.1415927 b) b)))
    (if (< y 0.0) (- 0.0 c) c)))

;;; Stars: one quad per instance, at infinity, additive.
(define-shader star-vertex
    (:stage :vertex
     :inputs ((corner :vec2 :location 0)
              (star :vec4 :location 1))
     :outputs ((clip-position :vec4 :built-in :position)
               (local :vec2 :location 0)
               (brightness :float :location 1))
     :resources (#.(frame-block)))
  (let* ((point (vec4 (swizzle star :xyz) 0.0))
         (clip (vec4 (dot clip-x point) (dot clip-y point)
                     (dot clip-z point) (dot clip-w point)))
         (offset (* corner (/ (vec2 6.0 6.0) (swizzle extra :yz)) (swizzle clip :w))))
    (set-output clip-position (vec4 (+ (swizzle clip :xy) offset) (swizzle clip :zw)))
    (set-output local corner)
    (set-output brightness (swizzle star :w))))

(define-shader star-fragment
    (:stage :fragment
     :inputs ((local :vec2 :location 0)
              (brightness :float :location 1))
     :outputs ((color :vec4 :location 0)))
  (let* ((glow (* brightness (exp (* -6.0 (dot local local))))))
    (set-output color (vec4 (* (vec3 0.9 0.95 1.0) glow) 0.0))))

;;; Earth: a ray-sphere impostor over a full-screen triangle.
(define-shader earth-vertex
    (:stage :vertex
     :inputs ((corner :vec2 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)))
  (set-output clip-position (vec4 corner 0.5 1.0)))

(define-shader earth-fragment
    (:stage :fragment
     :inputs ((pixel :vec4 :built-in :frag-coord))
     :outputs ((color :vec4 :location 0)
               (depth :float :built-in :frag-depth))
     :resources (#.(frame-block)
                 (earth :texture-2d :binding 1)
                 (linear :sampler :binding 2)))
  (let* ((ndc (vec2 (- (* 2.0 (/ (swizzle pixel :x) (swizzle extra :y))) 1.0)
                    (- 1.0 (* 2.0 (/ (swizzle pixel :y) (swizzle extra :z))))))
         (ray (normalize (+ (swizzle forward :xyz)
                            (* (swizzle right :xyz) (swizzle ndc :x))
                            (* (swizzle up :xyz) (swizzle ndc :y)))))
         (center (swizzle body :xyz))
         (b (dot ray center))
         ;; |c|^2 - r^2, computed in double on the CPU
         (disc (- (* b b) (swizzle extra :x)))
         (tt (- b (sqrt (max disc 0.0))))
         (n (/ (- (* ray tt) center) (swizzle body :w)))
         (lon (atan2 (- 0.0 (swizzle n :z)) (swizzle n :x)))
         (lat (atan2 (swizzle n :y) (sqrt (max (- 1.0 (* (swizzle n :y) (swizzle n :y))) 0.0))))
         (uv (vec2 (+ 0.5 (/ lon 6.2831855)) (- 0.5 (/ lat 3.1415927))))
         (albedo (swizzle (sample earth linear uv) :rgb))
         (light (clamp (dot n (swizzle sun :xyz)) 0.0 1.0))
         (rim (expt (- 1.0 (clamp (- 0.0 (dot n ray)) 0.0 1.0)) 4.0))
         (air (* (vec3 0.3 0.55 1.0) rim (clamp (+ (dot n (swizzle sun :xyz)) 0.2) 0.0 1.0))))
    (when (or (< disc 0.0) (< tt 0.0))
      (discard))
    (set-output color (vec4 (+ (* albedo (+ light 0.01)) air) 1.0))
    (set-output depth (/ (swizzle forward :w) (* tt (dot ray (swizzle forward :xyz)))))))

;;; Orbit: one screen-space ribbon segment per instance.
(define-shader orbit-vertex
    (:stage :vertex
     :inputs ((corner :vec2 :location 0)
              (from :vec4 :location 1)
              (to :vec4 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (across :float :location 0))
     :resources (#.(frame-block)))
  (let* ((a0 (vec4 (dot clip-x from) (dot clip-y from) (dot clip-z from) (dot clip-w from)))
         (b0 (vec4 (dot clip-x to) (dot clip-y to) (dot clip-z to) (dot clip-w to)))
         (limit 1.0)
         (wa (swizzle a0 :w)) (wb (swizzle b0 :w))
         (a (if (< wa limit) (mix a0 b0 (/ (- limit wa) (- wb wa))) a0))
         (b (if (< wb limit) (mix b0 a0 (/ (- limit wb) (- wa wb))) b0))
         (size (swizzle extra :yz))
         (sa (* (/ (swizzle a :xy) (swizzle a :w)) size))
         (sb (* (/ (swizzle b :xy) (swizzle b :w)) size))
         (along (normalize (+ (- sb sa) (vec2 1e-6 0.0))))
         (normal (vec2 (- 0.0 (swizzle along :y)) (swizzle along :x)))
         (p (mix a b (swizzle corner :x)))
         (offset (* normal (swizzle corner :y) (/ (vec2 3.0 3.0) size) (swizzle p :w)))
         (visible (if (and (< wa limit) (< wb limit)) 0.0 1.0)))
    (set-output clip-position (* (vec4 (+ (swizzle p :xy) offset) (swizzle p :zw)) visible))
    (set-output across (swizzle corner :y))))

(define-shader orbit-fragment
    (:stage :fragment
     :inputs ((across :float :location 0))
     :outputs ((color :vec4 :location 0)))
  (let* ((edge (- 1.0 (abs across))))
    (set-output color (vec4 (* (vec3 1.0 0.7 0.2) (clamp (* edge 2.0) 0.0 1.0)) 0.0))))
```

`probe.lisp`:

```lisp
(defpackage #:gap-probe
  (:use #:cl)
  (:export #:start #:stop #:screenshot #:*app* #:load-earth))
(in-package #:gap-probe)

(defparameter *scenes*
  '(:leo (:eye (0d0 0d0 7d6) :target (0d0 0.9397d6 6.658d6))
    :far (:eye (0d0 1.2d7 3.2d7) :target (0d0 0d0 0d0))))

(defun v- (a b) (mapcar #'- a b))
(defun v+ (a b) (mapcar #'+ a b))
(defun v* (a s) (mapcar (lambda (x) (* x s)) a))
(defun dot3 (a b) (reduce #'+ (mapcar #'* a b)))
(defun cross (a b)
  (destructuring-bind ((ax ay az) (bx by bz)) (list a b)
    (list (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))))
(defun unit (a) (v* a (/ (sqrt (dot3 a a)))))

(defconstant +radius+ 6.371d6)
(defconstant +near+ 0.1d0)

(defun floats (list)
  (map '(simple-array single-float (*)) (lambda (x) (coerce x 'single-float)) list))

(defun uniform-data (scene width height)
  (destructuring-bind (&key eye target) scene
    (let* ((forward (unit (v- target eye)))
           (right (unit (cross forward '(0d0 1d0 0d0))))
           (up (cross right forward))
           (focal (/ (tan (/ (* 50 (/ pi 180)) 2))))
           (aspect (/ width height))
           (center (v- '(0d0 0d0 0d0) eye)))
      (floats
       (append (append (v* right (/ focal aspect)) '(0))
               (append (v* up (- focal)) '(0))
               (list 0 0 0 +near+)
               (append forward '(0))
               (append (v* right (/ aspect focal)) '(0))
               (append (v* up (/ focal)) '(0))
               (append forward (list +near+))
               (append center (list +radius+))
               ;; |c|^2 - r^2 = h (2r + h), exact in double
               (let ((h (- (sqrt (dot3 center center)) +radius+)))
                 (list (* h (+ (* 2 +radius+) h)) width height 0))
               (append (unit '(-1d0 0.4d0 0.8d0)) '(0)))))))

(defun star-data (count)
  (let ((state (sb-ext:seed-random-state 7)))
    (floats
     (loop repeat count
           append (let* ((z (- (random 2d0 state) 1))
                         (phi (random (* 2 pi) state))
                         (s (sqrt (- 1 (* z z)))))
                    (list (* s (cos phi)) z (* s (sin phi))
                          (expt (random 1d0 state) 6)))))))

(defun orbit-data (eye segments)
  "Segment endpoints relative to EYE: a 2,000 km orbit inclined 30 degrees."
  (let ((r (+ +radius+ 2d6)) (inc (* 30 (/ pi 180))))
    (flet ((point (k)
             (let ((a (* 2 pi (/ k segments))))
               (v- (list (* r (cos a)) (* r (sin a) (sin inc)) (* r (sin a) (cos inc))) eye))))
      (floats
       (loop for k below segments
             append (append (point k) '(1) (point (1+ k)) '(1)))))))

(defun load-earth (path)
  "Pixels of PATH as RGBA8 words, through SDL3_image."
  (let* ((loaded (sdl3-image:img-load (namestring path)))
         (surface (sdl3:convert-surface loaded :abgr8888)))
    (sdl3:destroy-surface loaded)
    (let* ((w (cffi:mem-ref surface :int 8))
           (h (cffi:mem-ref surface :int 12))
           (pitch (cffi:mem-ref surface :int 16))
           (pixels (cffi:mem-ref surface :pointer 24))
           (words (make-array (list h w) :element-type '(unsigned-byte 32))))
      (dotimes (y h)
        (dotimes (x w)
          (setf (aref words y x) (cffi:mem-aref pixels :uint32 (+ x (* y (/ pitch 4)))))))
      (sdl3:destroy-surface surface)
      (values words w h))))

(defclass app ()
  ((canvas :initarg :canvas :reader app-canvas)
   (device :initarg :device :reader app-device)
   (context :initarg :context :reader app-context)
   (scene :initarg :scene :reader app-scene)
   (parts :initarg :parts :reader app-parts)
   (depth-target :initform nil :accessor app-depth-target)
   (color-views :initform (make-hash-table :test #'eq) :reader app-color-views)))

(defvar *app* nil)
(defun part (app name) (getf (app-parts app) name))

(defun depth-view (app extent)
  (let ((cached (app-depth-target app)))
    (if (and cached (equal (first cached) extent))
        (second cached)
        (let* ((device (app-device app))
               (texture (luv:create device (luv:make-texture-descriptor
                                            :label "depth" :size extent :dimensions :2d
                                            :format :depth32-float :usage '(:render-attachment))))
               (view (luv:create device (luv:make-texture-view-descriptor :texture texture))))
          (setf (app-depth-target app) (list extent view))
          view))))

(defun color-view (app texture)
  (or (gethash texture (app-color-views app))
      (setf (gethash texture (app-color-views app))
            (luv:create (app-device app) (luv:make-texture-view-descriptor :texture texture)))))

(defun encode-scene (app encoder target extent)
  (luv:write-buffer (part app :uniforms)
                    (uniform-data (getf *scenes* (app-scene app)) (first extent) (second extent)))
  (let ((pass (luv:begin-render-pass
               encoder
               (luv:make-render-pass-descriptor
                :color-attachments `((:view ,(color-view app target) :load-op :clear
                                      :store-op :store :clear-value #(0.0 0.0 0.0 1.0)))
                :depth-stencil-attachment
                `(:view ,(depth-view app extent) :depth-load-op :clear :depth-store-op :store
                  :depth-clear-value 0.0)))))
    (luv:set-pipeline pass (part app :star-pipeline))
    (luv:set-bind-group pass 0 (part app :bind-group))
    (luv:set-vertex-buffer pass 0 (part app :quad))
    (luv:set-vertex-buffer pass 1 (part app :stars))
    (luv:draw pass 6 (part app :star-count))
    (luv:set-pipeline pass (part app :earth-pipeline))
    (luv:set-bind-group pass 0 (part app :bind-group))
    (luv:set-vertex-buffer pass 0 (part app :triangle))
    (luv:draw pass 3)
    (luv:set-pipeline pass (part app :orbit-pipeline))
    (luv:set-bind-group pass 0 (part app :bind-group))
    (luv:set-vertex-buffer pass 0 (part app :ribbon))
    (luv:set-vertex-buffer pass 1 (part app :orbit))
    (luv:draw pass 6 (part app :orbit-count))
    (luv:end-pass pass)))

(defmethod luv:capture-canvas ((app app)) (app-canvas app))
(defmethod luv:encode-capture-frame ((app app) capture encoder target extent)
  (declare (ignore capture))
  (encode-scene app encoder target extent))

(defun draw (app)
  (let ((context (app-context app)))
    (luv:present-canvas-frame
     context
     (lambda (texture encoder time)
       (declare (ignore time))
       (encode-scene app encoder texture (luv:canvas-extent context))))))

(defun buffer (device data usage)
  (let ((b (luv:create device (luv:make-buffer-descriptor
                               :size (* 4 (length data)) :usage (list usage :copy-dst)))))
    (luv:write-buffer b data)
    b))

(defun module (device code)
  (luv:create device (luv:make-shader-module-descriptor :language :mathematical :code code)))

(defun start (earth-pixels &key (visible t) (scene :leo) (width 640) (height 400))
  (destructuring-bind (words tw th) earth-pixels
    (let* ((canvas (luv:make-sdl-canvas :title "gap probe" :width width :height height
                                        :visible-p visible))
           (_ (luv:open-canvas canvas))
           (device (luv:request-gpu-device luv:*gpu-provider*
                                           (luv:make-device-descriptor :label "gap")))
           (context (luv:make-canvas-context canvas luv:*gpu-provider*
                                             (luv:make-canvas-configuration
                                              :device device
                                              :usage '(:render-attachment :copy-dst))))
           (format (luv:canvas-format context))
           (texture (luv:create device (luv:make-texture-descriptor
                                        :label "earth" :size (list tw th) :dimensions :2d
                                        :format :rgba8-unorm-srgb
                                        :usage '(:copy-dst :texture-binding))))
           (sampler (luv:create device (luv:make-sampler-descriptor :address-mode-u :repeat)))
           (uniforms (luv:create device (luv:make-buffer-descriptor
                                         :size (* 4 40) :usage '(:uniform))))
           (layout (luv:create device (luv:make-bind-group-layout-descriptor
                                       :entries '((:binding 0 :type :uniform-buffer)
                                                  (:binding 1 :type :texture)
                                                  (:binding 2 :type :sampler)))))
           (bind-group (luv:create device (luv:make-bind-group-descriptor
                                           :layout layout
                                           :entries `((:binding 0 :resource ,uniforms)
                                                      (:binding 1 :resource
                                                       ,(luv:create device (luv:make-texture-view-descriptor :texture texture)))
                                                      (:binding 2 :resource ,sampler)))))
           (eye (getf (getf *scenes* scene) :eye))
           (stars 4000) (segments 256))
      (declare (ignore _))
      (luv:write-texture (luv:device-queue device) (luv:make-texture-copy :texture texture)
                         words (luv:make-texture-data-layout :bytes-per-row (* tw 4)
                                                             :rows-per-image th)
                         (list tw th))
      (flet ((pipeline (vs fs buffers compare write blend)
               (luv:create device
                           (luv:make-render-pipeline-descriptor
                            :layout layout
                            :vertex `(:module ,(module device vs) :buffers ,buffers)
                            :fragment `(:module ,(module device fs)
                                        :targets ((:format ,format ,@(when blend '(:blend :premultiplied-alpha)))))
                            :primitive '(:topology :triangle-list)
                            :depth-stencil `(:format :depth32-float :depth-write-enabled ,write
                                             :depth-compare ,compare)))))
        (let* ((corner '(:array-stride 8 :attributes ((:shader-location 0 :offset 0 :format :float32x2))))
               (parts
                 (list :uniforms uniforms :bind-group bind-group
                       :quad (buffer device (floats '(-1 -1 1 -1 1 1 -1 -1 1 1 -1 1)) :vertex)
                       :triangle (buffer device (floats '(-1 -1 3 -1 -1 3)) :vertex)
                       :ribbon (buffer device (floats '(0 -1 1 -1 1 1 0 -1 1 1 0 1)) :vertex)
                       :stars (buffer device (star-data stars) :vertex) :star-count stars
                       :orbit (buffer device (orbit-data eye segments) :vertex) :orbit-count segments
                       :star-pipeline
                       (pipeline (gap-probe.shaders::star-vertex) (gap-probe.shaders::star-fragment)
                                 `(,corner (:array-stride 16 :step-mode :instance
                                            :attributes ((:shader-location 1 :offset 0 :format :float32x4))))
                                 :always nil t)
                       :earth-pipeline
                       (pipeline (gap-probe.shaders::earth-vertex) (gap-probe.shaders::earth-fragment)
                                 (list corner) :greater t nil)
                       :orbit-pipeline
                       (pipeline (gap-probe.shaders::orbit-vertex) (gap-probe.shaders::orbit-fragment)
                                 `(,corner (:array-stride 32 :step-mode :instance
                                            :attributes ((:shader-location 1 :offset 0 :format :float32x4)
                                                         (:shader-location 2 :offset 16 :format :float32x4))))
                                 :greater nil t)))
               (app (make-instance 'app :canvas canvas :device device :context context
                                        :scene scene :parts parts)))
          (setf (luv:canvas-clock canvas)
                (luv:make-cadence-clock (lambda (native ts) (declare (ignore native ts)) (draw app))
                                        :frames-per-second 30))
          (setf *app* app))))))

(defun screenshot (path) (luv:capture-application-screenshot *app* path))

(defun stop (&optional (app *app*))
  (let ((canvas (app-canvas app)))
    (when (eq :open (luv:canvas-state canvas))
      (setf (luv:canvas-clock canvas) (luv:make-demand-clock)))
    (luv:close-canvas canvas)
    (luv:destroy (app-device app))
    (setf *app* nil)
    (values)))
```

`run.lisp`:

```lisp
;; sbcl --script run.lisp OUT-DIR VISIBLE(t|nil) EARTH-JPG
(require :asdf)
(asdf:load-asd (merge-pathnames "probe/gap-probe.asd" *load-truename*))
(asdf:load-system "gap-probe")
(let* ((out (second sb-ext:*posix-argv*))
       (visible (string= "t" (third sb-ext:*posix-argv*)))
       (start (get-internal-real-time))
       (earth (multiple-value-list (gap-probe:load-earth (fourth sb-ext:*posix-argv*)))))
  (format t "~&texture ~Ax~A loaded in ~,1F s~%" (second earth) (third earth)
          (/ (- (get-internal-real-time) start) internal-time-units-per-second))
  (luv:call-with-sdl-main-thread
   (lambda ()
     (dolist (scene '(:leo :far))
       (let ((start (get-internal-real-time))
             (name (format nil "~A/~(~A~).png" out scene)))
         (gap-probe:start earth :visible visible :scene scene)
         (sleep 1)
         (gap-probe:screenshot name)
         (gap-probe:stop)
         (format t "~&~A in ~,1F s~%" name
                 (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))))
(uiop:quit 0)
```
