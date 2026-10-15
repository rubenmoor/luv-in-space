# AGENTS.md

Luv in Space is a showcase for [luv](https://github.com/mbrock/luv), Mikael Brockman's Common Lisp GPU workshop: a hard sci-fi spaceflight game in near-future, realistic technology, centered on piloting a rugged DIY spaceship under Newtonian physics from low Earth orbit out into the solar system. It is the prelude to a space adventure with a linear quest line.

## Work: `pm`
- Milestones, plans, tasks, decisions: all in `tasks.sql`, only through `pm`. Read `pm instructions` first.
- Start: `pm status`, `pm show <task>`.
- Before a choice: `pm decision list --answered --grep <word>`; answers are binding.
- Human decisions are the exception: decide yourself and record it (`pm decision record`); the developer overrides by superseding.
  - `pm decision add` only for what no agent can decide or find out: accounts, money, licences, publishing, or a contradiction in a milestone.
  - Creative choices (story, names, look, features) and technical choices are the agents' own: record them.
- Follow-up work: `pm task add`.
- Cite milestones, plans, tasks, decisions by id and subject, in backticks: `decision 14: Units and time epoch`.

## Outside pm workers
- Skip this section if `PM_TASK` is set (you are a pm worker).
- Milestones are the developer's; edit them with pm when we answer decisions: `pm decision list`, `pm milestone set <id> --body - <<'EOF'`, then `pm decision answer <id> …`.
- Editing a milestone makes pm queue a revision of its plans and tasks; editing the milestone is usually enough.

## luv
- `../luv` (sibling of this repo): a read-only reference checkout of github.com/mbrock/luv. Read its `AGENTS.md`, `README.md`, `wiki/` and code; never change or commit there.
- This repo builds against luv pinned as a flake input, not against `../luv`.
- Something luv lacks: build it here, in this repo; list what would better live in luv in `docs/luv-wishlist.md`.

## Rules
- Push directly to `main`. No feature branches or pull requests.
- Before every commit: the project's build and tests pass (commands below, once they exist).
- Use comments sparingly and keep them short.
- Style of `.md` files: bullet points, concise. Exception: an intro paragraph in full sentences at the top of the document and under each top-level heading.
- Stop background processes by PID (`kill $!`), never `pkill -f`: the pattern can match your own `claude` process.
- Close every game window you open; a worker leaves no process running.
- Unfinished game parts: `PLACEHOLDER(<task id>)` in code and, where visible, on screen.

## Commands
- Dev shell: `direnv allow` or `nix develop`; SBCL with luv on `CL_SOURCE_REGISTRY`, Vulkan/SDL3, `pm`, `git`. The scripts below enter it themselves.
  - `scripts/test`, `scripts/screenshot` and `./sly` always re-enter it, so a stale direnv shell or a foreign `LD_LIBRARY_PATH` (`GLIBC_2.43 not found`, `Unable to load … libSDL3.so`) doesn't reach them; run `sbcl` by hand only in a fresh `nix develop` (or after `direnv reload`).
- Build: `nix build` (`result/bin/luv-in-space`); ~17 s from a fresh clone, luv and its dependencies from luv's binary cache; ~11 s when only the game's image builds.
- Run: `nix run` opens the window; Escape or closing the window quits. An agent stops it by PID.
- Test: `scripts/test` (Parachute, `luv-in-space/tests`; renders one frame on a hidden canvas, lavapipe without a display; exit 1 on failure; ~5 s). `nix flake check` runs it in the sandbox (~16 s).
- Screenshot: `scripts/screenshot out.png [--frames N]`: a 1280x800 PNG of the start state from the outside view (`capture-scene`), from a hidden canvas, headless too; ~3 s.
- Star table: `scripts/convert-stars [out]` downloads HYG (needs network), checks its pinned SHA-256 and writes the stars ≤ 6.5 mag; without an argument it rewrites the committed `assets/stars/hyg-6.5.sexp`; ~12 s.
- Live image: `./sly start` (a Slynk image with `luv-in-space` loaded; ~5 s cold), `./sly play`, `./sly status`, `./sly screenshot /tmp/x.png` (window size, after `./sly play`), `./sly eval '(defun …)' --package LUV-IN-SPACE` (redefine while playing), `./sly failures`, `./sly resume`, `./sly stop-playing`, `./sly stop`; `./sly --help` lists all. Enters the dev shell itself; options go after the code.
  - `./sly` is luv's client from the flake input with `play`, `screenshot`, `stop-playing` and status redefined for this game (`scripts/sly-client.lisp`); `sly-server.lisp` boots the image. State in `build/` (~410 MB, git-ignored).
  - Shaders are live: a `define-shader` re-evaluated with `./sly eval '(define-frame-shader earth-fragment …)' --package LUV-IN-SPACE.SHADERS` shows on the next frame (luv live shader pipelines, refreshed in `draw`). A source that fails to parse is rejected by the eval; one that fails to build keeps the last good pipeline, and `./sly status` names it with its error (`Live artifact earth FAILED …`) until a fix rebuilds it; in Lisp `(luv-in-space:report-failed-live-artifacts)`.

## Layout
- `flake.nix`: luv as a pinned input, the package (`nix build`), `apps.default` (`nix run`), the dev shell, the check (`nix flake check`).
- `luv-in-space.asd`: systems `luv-in-space` (`src/`), `luv-in-space/program` (the executable) and `luv-in-space/tests` (`tests/`).
- `src/`: the skeleton's `package.lisp`, `shaders.lisp`, `main.lisp` (the game: sim, clock, camera, the frame) and `capture.lisp` (`capture-state` of a given sim, `with-capture-game`); the simulation files built so far: `math.lisp`, `units.lisp`, `bodies.lisp`, `orbits.lisp`, `physics.lisp`, `ship.lisp`, `controls.lisp`, `clock.lisp`, `hazards.lisp`, `prediction.lisp`; `assets.lisp` (the assets root, image decoding, the star table); the rendering files built so far: `gpu.lisp` (HAL helpers, the renderer), `camera.lisp` (the camera, the one projection), `post.lisp` (HDR, bloom, tone map, exposure), `overlay.lisp` (dot-matrix labels), `render.lisp` (the render state, the frame's passes); `docs/architecture.md` "Files in load order" lists every file, milestone by milestone, and what each holds.
  - Placeholders on screen: a true-scale UV-sphere Earth sampling the Blue Marble and screen-space stars `PLACEHOLDER(76)`, no ship `PLACEHOLDER(77)`, no readout `PLACEHOLDER(80)`; the label `PLACEHOLDER(76 77 80)` top left.
- `assets/`: runtime data, one directory per kind with its `SOURCE.md` (`assets/earth/blue-marble-5400.jpg`; `assets/stars/hyg-6.5.sexp`, the HYG stars ≤ 6.5 mag, under CC BY-SA 4.0 with its `LICENSE`); `CREDITS.md` at the root; the package installs `assets/` and the launcher sets `LUV_IN_SPACE_ASSETS` (`docs/architecture.md` "Assets").
- `scripts/`: `test`, `screenshot`, `convert-stars` (with `convert-stars.lisp`), the build entry (`build.lisp`) and the `./sly` client; `./sly` and `sly-server.lisp` at the root.
- `docs/`: the source of truth for features after `milestone 1`; cite their sections when implementing.
  - `luv-capabilities.md`: what luv offers, measured, and the gaps with their workarounds here.
  - `luv-wishlist.md`: what this repo builds that would better live in luv.
  - `architecture.md`: the dependency on luv, module layout, units and frames, integrator and time warp, tests and flight scripts.
  - `design.md`: the game for implementers; "Scope" → "Feature list" assigns every feature to `milestone 2` to `milestone 6` or cuts it.
  - `story.md`: setting, characters, the quest line, every radio text; "Names and terms" is the one list of names.
