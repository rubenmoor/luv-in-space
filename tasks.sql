-- pm database. Change it with pm; after a hand edit run `pm validate`.
PRAGMA user_version = 15;

CREATE TABLE milestone (
    id             INTEGER PRIMARY KEY,
    subject        TEXT NOT NULL CHECK (trim(subject) <> ''),
    status         TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'on-hold', 'implemented')),
    body           TEXT NOT NULL DEFAULT '',               -- Markdown
    created_at     TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ', 'now')),
    implemented_at TEXT,
    queue_index    INTEGER NOT NULL DEFAULT 0,             -- its tasks are queued lowest index first, then by priority
    CONSTRAINT "implemented_at is set exactly when implemented" CHECK ((status = 'implemented') = (implemented_at IS NOT NULL))
) STRICT;

CREATE TABLE plan (
    id              INTEGER PRIMARY KEY,
    milestone_id    INTEGER NOT NULL REFERENCES milestone (id),
    subject         TEXT NOT NULL CHECK (trim(subject) <> ''),
    priority        INTEGER,                               -- NULL: 0
    body            TEXT NOT NULL CONSTRAINT "a plan needs a body" CHECK (trim(body) <> ''),
    created_at      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ', 'now')),
    created_by_task INTEGER REFERENCES task (id)
) STRICT;

CREATE TABLE task (
    id              INTEGER PRIMARY KEY,
    plan_id         INTEGER NOT NULL REFERENCES plan (id),
    subject         TEXT NOT NULL CHECK (trim(subject) <> ''),
    kind            TEXT NOT NULL DEFAULT 'work' CHECK (kind IN ('work', 'plan', 'masterplan', 'split', 'cleanup', 'revise')),
    status          TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'done', 'dropped')),
    priority        INTEGER,                               -- NULL: the plan's
    note            TEXT,                                  -- done: date, files; dropped: reason
    body            TEXT NOT NULL DEFAULT '',              -- Markdown
    created_at      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ', 'now')),
    retired_at      TEXT,                                  -- done or dropped
    created_by_task INTEGER REFERENCES task (id),
    CONSTRAINT "an open task needs a body" CHECK (status <> 'open' OR trim(body) <> ''),
    CONSTRAINT "retired_at is set exactly when done or dropped" CHECK ((status = 'open') = (retired_at IS NULL))
) STRICT;

CREATE TABLE decision (
    id              INTEGER PRIMARY KEY,
    subject         TEXT NOT NULL CHECK (trim(subject) <> ''),
    status          TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'answered', 'irrelevant')),
    options         TEXT NOT NULL DEFAULT '[]' CHECK (json_type(options) = 'array'),  -- recommended first
    body            TEXT NOT NULL DEFAULT '',
    answer          TEXT,
    reason          TEXT,
    created_at      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ', 'now')),
    closed_at       TEXT,
    created_by_task INTEGER REFERENCES task (id),
    signoff         TEXT CHECK (signoff IN ('masterplan', 'plan', 'task-queue')),  -- made by pm; NULL: a question
    kind            TEXT NOT NULL DEFAULT 'rule' CHECK (kind IN ('rule', 'once')),  -- once: a go-ahead, not in the log
    decided_by_task INTEGER REFERENCES task (id),          -- a worker's own choice (`pm decision record`); NULL: the human
    consequences    TEXT NOT NULL DEFAULT '',              -- what changed because of the answer; appended
    CONSTRAINT "a sign-off is only approved"
    CHECK (signoff IS NULL OR status <> 'irrelevant' AND coalesce(answer, 'approve') = 'approve'),
    CONSTRAINT "only an answered question has a decider and consequences"
    CHECK (status = 'answered' AND signoff IS NULL OR decided_by_task IS NULL AND consequences = ''),
    CONSTRAINT "pending: no answer, reason or closed_at; answered: answer and closed_at; irrelevant: reason and closed_at"
    CHECK (status = 'pending' AND answer IS NULL AND reason IS NULL AND closed_at IS NULL
           OR status = 'answered' AND answer IS NOT NULL AND closed_at IS NOT NULL
           OR status = 'irrelevant' AND reason IS NOT NULL AND closed_at IS NOT NULL)
) STRICT;

CREATE TABLE run (                                         -- one worker run of `pm run`
    id                    INTEGER PRIMARY KEY,
    task_id               INTEGER NOT NULL REFERENCES task (id),
    started_at            TEXT NOT NULL,
    ended_at              TEXT,                            -- NULL: running
    result                TEXT NOT NULL CHECK (result IN ('running', 'done', 'blocked', 'failed', 'limit')),
    reason                TEXT,                            -- failed: why; limit: the message
    exit_code             INTEGER,
    base_commit           TEXT,                            -- HEAD when the run started
    model                 TEXT,                            -- from here on: the worker's JSON output, if any
    session_id            TEXT,
    turns                 INTEGER,
    input_tokens          INTEGER,
    output_tokens         INTEGER,
    cache_read_tokens     INTEGER,
    cache_creation_tokens INTEGER,
    cost_usd              REAL,
    pid                   INTEGER,                         -- the `pm run` process
    CONSTRAINT "ended_at is set exactly when not running" CHECK ((result = 'running') = (ended_at IS NULL))
) STRICT;

CREATE TABLE task_decision (                               -- the task waits while the decision is pending
    task_id     INTEGER NOT NULL REFERENCES task (id),
    decision_id INTEGER NOT NULL REFERENCES decision (id),
    PRIMARY KEY (task_id, decision_id)
) STRICT;

CREATE TABLE decision_supersedes (                         -- decision_id's answer replaces superseded_id's
    decision_id   INTEGER NOT NULL REFERENCES decision (id),
    superseded_id INTEGER NOT NULL REFERENCES decision (id),
    note          TEXT NOT NULL DEFAULT '',                -- '': all of it; else partly, e.g. "scope only"
    PRIMARY KEY (decision_id, superseded_id),
    CHECK (decision_id <> superseded_id)
) STRICT;

CREATE INDEX plan_milestone ON plan (milestone_id);

CREATE INDEX task_plan ON task (plan_id);

CREATE INDEX task_decision_decision ON task_decision (decision_id);

CREATE INDEX decision_superseded ON decision_supersedes (superseded_id);

CREATE INDEX run_task ON run (task_id);

CREATE UNIQUE INDEX run_running ON run (result) WHERE result = 'running';

CREATE VIEW queue AS
SELECT coalesce(t.priority, p.priority, 0) AS priority, t.id AS task_id, t.subject, t.kind
FROM task t
JOIN plan p ON p.id = t.plan_id
JOIN milestone m ON m.id = p.milestone_id
WHERE m.status = 'open' AND t.status = 'open'
  AND NOT EXISTS (SELECT 1 FROM task_decision td JOIN decision d ON d.id = td.decision_id
                  WHERE td.task_id = t.id AND d.status = 'pending')
ORDER BY m.queue_index, priority DESC, m.id, p.id, t.id;

INSERT INTO milestone (id, subject, status, body, created_at, implemented_at, queue_index) VALUES (1, 'Scope and design: what luv can carry, what the game is', 'open', 'The first milestone finds out what luv can carry and turns the developer''s ideas into a concrete, scoped game design. Every later milestone builds what these documents specify, so they must be specific enough to implement from, and honest about luv''s limits.

## The developer''s ideas (input, not yet scope)
- Hard sci-fi. Humans have only just begun travelling in space; all technology is known and realistic.
- Ships are rugged: bare steel frames with a life-support module attached, their function obvious at a glance (e.g. tugs that move asteroids around).
- Weapons, if any, are low tech.
- The game starts in a near-Earth orbit, with Newtonian physics: you can crash into Earth.
- An autopilot sets you on a path into an orbit; presets reach points of interest in Earth orbit, and also the Moon, the asteroid belt, maybe some planets.
- Flying too close to the Sun burns and kills you.
- The cockpit and controls look like a DIY job.
- The game centres on piloting the ship.
- A story: the prelude to a space-adventure game with a linear quest line.

## Outcome
- `docs/luv-capabilities.md`: what luv offers this game, found by building luv and running its programs and tests, not only reading.
  - Rendering (meshes, shaders written in Lisp, lighting, text drawn in the world, its terminal screens), windowing and input, the live image (`./sly`), screenshots, physics and SIMD, packaging (`nix run`), platforms.
  - What a game at solar-system scale needs that luv lacks (e.g. depth range, double-precision positions, spheres and planet surfaces, a starfield, audio), each with a workaround in this repo.
- `docs/architecture.md`: how luv-in-space depends on luv (flake input; the ASDF systems it loads; the dev loop with a live image), the module layout, units and coordinates (precision across 10^12 m: e.g. double-precision world positions, camera-relative rendering), time and integration (time warp, the integrator), how agents build, run, test and screenshot it.
- `docs/design.md`: the game design, written for implementers.
  - The ship(s): frame, modules, thrusters, fuel and reaction mass, mass and delta-v, damage; how they look.
  - Flight: controls, views (cockpit, outside), instruments, the autopilot and its presets, time warp, dying (crash, Sun) and what follows.
  - The cockpit: its DIY look and how each instrument is drawn.
  - The places: Earth orbit points of interest, the Moon, the asteroid belt, planets, the Sun; real orbital data where it matters.
  - Scope: a prioritised feature list, each feature assigned to one of the later milestones (2: flight core, 3: cockpit and autopilot, 4: the solar system, 5: story and quests, 6: release) or cut, with the reason.
- `docs/story.md`: setting, characters, the prelude''s premise and a linear quest line of 5 to 10 quests that teaches piloting step by step and uses the places and the ship''s work; how quests are told (radio, messages, the cockpit''s screens).
- A walking skeleton: an ASDF system in this repo, built against luv pinned as a flake input, that opens a window and draws something, with a test and a screenshot command that work for an agent. `AGENTS.md` "Commands" filled in.

## Constraints
- Don''t change `../luv`; see `AGENTS.md`.
- No human decisions needed: make the creative and technical choices, record the lasting ones (`pm decision record`). The developer reads the documents and supersedes what they want changed.
- Realism over spectacle, but scope over realism: a feature luv can''t carry, or that costs far more than it shows, is cut in `docs/design.md` with the reason.

## Out of scope
- Game features beyond the walking skeleton.

## Done when
- The four documents exist, agree with each other, and the feature list assigns every feature to a later milestone or cuts it.
- The skeleton builds and runs from a clean checkout with the commands in `AGENTS.md`, and its test passes.', '2026-10-14T16:06:30Z', NULL, 0);
INSERT INTO milestone (id, subject, status, body, created_at, implemented_at, queue_index) VALUES (2, 'Flight core: Newtonian flight in near-Earth orbit', 'open', 'The flight core: a ship flying under Newtonian physics in near-Earth orbit, from which every other milestone builds. The design in `docs/` (from `milestone 1`) is the specification; this milestone implements what `docs/design.md` assigns to it.

## Outcome
- The game starts with the ship in a low Earth orbit.
- Orbital mechanics with the precision `docs/architecture.md` sets: Earth''s gravity, plus the Moon''s and the Sun''s where the design calls for them; time warp.
- The ship as a rigid body with thrusters for rotation and translation, mass, fuel or reaction mass; the player flies it by hand.
- Earth rendered so it reads as Earth from orbit; space, Sun and stars; the ship (bare steel frame, life-support module) seen from an outside view.
- Crashing into Earth kills you, as `docs/design.md` says, and the game goes on from there (restart or as designed).
- A minimal flight readout (altitude, speed, apoapsis and periapsis) until `milestone 3` brings the real cockpit.

## Constraints
- As in `AGENTS.md`: no changes to `../luv`; own choices recorded, not asked.
- Physics is tested headless: orbits stay stable over long time warps; energy and angular momentum drift stay within a bound the tests set.

## Done when
- From a clean checkout an agent can start the game, fly a burn that raises the orbit, and deorbit into Earth, with screenshots of each.
- Tests pass; `AGENTS.md` and `docs/` match what was built.', '2026-10-14T16:06:59Z', NULL, 1);
INSERT INTO milestone (id, subject, status, body, created_at, implemented_at, queue_index) VALUES (3, 'Cockpit and autopilot: the DIY cockpit, orbits and Earth-orbit presets', 'open', 'The cockpit and the autopilot: piloting becomes the heart of the game. The design in `docs/` is the specification; this milestone implements what `docs/design.md` assigns to it.

## Outcome
- The DIY cockpit as designed: instruments that look improvised but readable (e.g. luv''s in-world text and terminal screens), switches and readouts; the player can fly from the cockpit alone.
- The autopilot: from the current orbit into a target orbit (circularise, raise, lower, change plane), with burns the player can watch and interrupt; it must not fly into Earth.
- Points of interest in Earth orbit as designed (stations, depots, debris, asteroids being towed, …), each an autopilot preset: rendezvous and stop nearby.
- Close-range manoeuvring and docking or grappling, if `docs/design.md` assigns it here.

## Constraints
- As in `AGENTS.md`: no changes to `../luv`; own choices recorded, not asked.
- The autopilot uses the same physics and the same fuel as the player: no teleporting.

## Done when
- From a low Earth orbit, every Earth-orbit preset is reached by autopilot in a headless test, and once in the running game with screenshots.
- Tests pass; `AGENTS.md` and `docs/` match what was built.', '2026-10-14T16:07:00Z', NULL, 2);
INSERT INTO milestone (id, subject, status, body, created_at, implemented_at, queue_index) VALUES (4, 'The solar system: Moon, asteroid belt, planets, the deadly Sun', 'open', 'The solar system: beyond Earth orbit to the Moon, the asteroid belt and the planets the design includes. The design in `docs/` is the specification; this milestone implements what `docs/design.md` assigns to it.

## Outcome
- The Moon, the asteroid belt and the planets in scope, on realistic orbits, rendered at every distance from far away to close by as designed.
- Transfers by autopilot (e.g. trans-lunar injection, Hohmann transfers, capture), with the time warp they need.
- The Sun is deadly: too close, the ship heats up and the pilot dies, with warnings first.
- Asteroids as places of work for the ship (moving them around, or as designed).

## Constraints
- As in `AGENTS.md`: no changes to `../luv`; own choices recorded, not asked.
- Real orbital data where `docs/design.md` asks for it; delta-v and travel times plausible for the ship.

## Done when
- From Earth orbit, every destination in scope is reached by autopilot in a headless test, and once in the running game with screenshots.
- Flying towards the Sun kills the pilot, tested.
- Tests pass; `AGENTS.md` and `docs/` match what was built.', '2026-10-14T16:07:01Z', NULL, 3);
INSERT INTO milestone (id, subject, status, body, created_at, implemented_at, queue_index) VALUES (5, 'Story and quests: the playable prelude', 'open', 'Story and quests: the prelude in `docs/story.md` becomes playable, a linear quest line from the first flight to its end. The design in `docs/` is the specification; this milestone implements what `docs/design.md` and `docs/story.md` assign to it.

## Outcome
- The quest line as written in `docs/story.md`: each quest given, tracked, completed, followed by the next; the story told as designed (radio, messages, the cockpit''s screens).
- Characters, ships and places the story needs; other ships at their work, low-tech weapons if the story uses them.
- Progress saved and resumed.

## Constraints
- As in `AGENTS.md`: no changes to `../luv`; own choices recorded, not asked. The story may be refined while implementing it; `docs/story.md` is kept in step.
- Quests use the flight, the autopilot and the places of the earlier milestones; no shortcuts around piloting.

## Done when
- A headless test plays through the whole quest line (scripted inputs or autopilot) to its end.
- Every quest has been played once in the running game, with screenshots.
- Tests pass; `AGENTS.md` and `docs/` match what was built.', '2026-10-14T16:07:15Z', NULL, 4);
INSERT INTO milestone (id, subject, status, body, created_at, implemented_at, queue_index) VALUES (6, 'Release: one command to play, a README for people', 'open', 'The release: Luv in Space as a showcase anyone can start with one command and understand in a minute. The design in `docs/` is the specification; this milestone implements what `docs/design.md` assigns to it.

## Outcome
- `nix run github:rubenmoor/luv-in-space` starts the game on the platforms luv supports (at least x86-64 Linux).
- A title screen, a short introduction, the controls in game; a smooth first minute for a new player.
- Performance: a steady frame rate on the developer''s machine, measured and documented.
- `README.md` for people: what it is, what it shows of luv, screenshots and a short recording, how to run it, credits to luv.

## Constraints
- As in `AGENTS.md`: no changes to `../luv`; own choices recorded, not asked.
- Publishing beyond pushing to this repository (stores, announcements) is out of scope.

## Done when
- `nix run` from a clean machine state works, verified by an agent.
- The README''s screenshots show the current game.
- Tests pass; `AGENTS.md` and `docs/` match what was built.', '2026-10-14T16:07:32Z', NULL, 5);

INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (1, 1, 'masterplan', 1000, 'The masterplan of `milestone 1: Scope and design: what luv can carry, what the game is`. It divides the milestone into six parts, in the order they should run: first find out what luv really does (by building and running it), then prove the build path with a walking skeleton, then write the architecture, the game design and the story on that ground, and finally check that everything agrees.

**The tasks of this masterplan are not implementation work: each of them writes the plan for one part** (`pm plan add 1 "<part subject>" --priority N`), and that plan is then split into the tasks that do the work.

## Ground rules for every part
- `../luv` is read-only reference; the game builds against luv pinned as a flake input in `flake.nix` (`AGENTS.md`).
- Whatever luv lacks is built in this repo and listed in `docs/luv-wishlist.md` (create it in part 1).
- No human decisions: creative and technical choices are recorded with `pm decision record`; `pm decision add` only for accounts, money, licences, publishing or a contradiction in the milestone.
- Docs follow the `.md` style in `AGENTS.md`: intro paragraph, then concise bullets.
- Every game window opened is closed again; no process left running.
- Out of scope: game features beyond the skeleton.

## Part 1: luv capabilities (priority 900)
Find out what luv can carry for a solar-system flight game, by building luv and running its programs and tests, not only reading.
- Read `../luv/AGENTS.md`, `README.md`, `wiki/` (e.g. `gpu-architecture.org`, `physics-and-simd.org`, `sky-and-light.org`, `mathematical-shaders.org`, `world-terminal.org`, `quantities-and-measurement.org`, `sb-simd.org`, `frame-performance.org`, `technical-debt.org`) and the code.
- Build luv via its flake (`nix build`, `nix run github:mbrock/luv`, `#luft`), run Luvcraft and Luft, run its test suite, take screenshots the way luv does; try the live image (`./sly`).
- Cover: rendering (meshes, Lisp shaders, lighting, sky, in-world text, terminal screens), windowing and input (keyboard, mouse, gamepad?), live image, screenshots and headless runs, physics and SIMD, quantities/units, packaging (`nix run`), platforms (Linux x86-64/ARM64, macOS Metal).
- Gaps for this game, each with a workaround in this repo: depth range at 10^12 m (reversed-Z, logarithmic depth, or layered passes), double-precision positions, spheres and planet surfaces (Earth texture/shader), starfield, Sun glare, orbit lines, HUD/2D overlay, audio, save files.
- Deliverables: `docs/luv-capabilities.md`, `docs/luv-wishlist.md`; recorded decisions for anything chosen (e.g. which luv systems to depend on).

## Part 2: walking skeleton (priority 800)
An ASDF system in this repo, built against luv pinned as a flake input, that opens a window and draws something, with a test and a screenshot command that work for an agent. Depends on part 1''s findings on how a luv program is built, started and screenshotted.
- `flake.nix`: add luv as an input (pinned in `flake.lock`), a package and `apps.default` that start the game, a dev shell with SBCL/luv and `pm`.
- An ASDF system (e.g. `luv-in-space.asd`, sources under `src/`), a window drawing something recognisable (e.g. a lit sphere for Earth and a starfield or a placeholder, marked `PLACEHOLDER(<task id>)` where it shows).
- A test system run headless (e.g. `nix flake check` or a `make test`-like command), and a screenshot command that writes a PNG an agent can read.
- The dev loop with a live image, if luv''s `./sly` setup can be reused here.
- Fill in `AGENTS.md` "Commands": dev shell, build, run, test, screenshot, live image; verified from a clean checkout (`git clone` into a temp dir).

## Part 3: architecture (priority 700)
`docs/architecture.md`, grounded in parts 1 and 2.
- How luv-in-space depends on luv: flake input, the ASDF systems it loads, the dev loop with a live image.
- Module layout: physics/orbits, ship, bodies and ephemerides, rendering (camera-relative), cockpit/instruments, autopilot, story/quests, save/load, tests.
- Units and coordinates: SI units, double-precision world positions across 10^12 m, frames (heliocentric, planet-centred, ship-local), camera-relative single-precision rendering, depth strategy.
- Time and integration: fixed physics step, the integrator (e.g. symplectic for coasting, patched conics or n-body as decided), time warp levels and when they are allowed (no thrust under high warp?), on-rails bodies.
- Testing: headless physics tests (energy and angular-momentum drift bounds, as `milestone 2: Flight core: Newtonian flight in near-Earth orbit` requires), scripted runs, screenshots.
- Record the lasting technical choices as decisions (unit of length, coordinate frames, integrator, depth strategy).

## Part 4: game design (priority 600)
`docs/design.md`, written for implementers, within what parts 1 and 3 say luv and the architecture can carry.
- The ship(s): bare steel frame, life-support module, tanks, engines and RCS thrusters, realistic propulsion (chemical, maybe electric), masses, Isp, delta-v budget; damage; how they look (built from simple meshes).
- Flight: controls (keys/mouse/gamepad), views (cockpit, outside, map/orbit view), instruments, the autopilot and its presets, time warp, dying (crash into Earth or the Moon, Sun heat) and what follows (restart, checkpoint).
- The cockpit: its DIY look (welded panels, taped labels, mismatched screens) and how each instrument is drawn (luv terminal screens, in-world text, needles).
- The places: Earth-orbit points of interest (stations, depots, debris, towed asteroids), the Moon, near-Earth asteroids and the belt, planets in scope, the Sun; real orbital data where it matters (sources named).
- Scope: a prioritised feature list; every feature assigned to milestone 2 (flight core), 3 (cockpit and autopilot), 4 (the solar system), 5 (story and quests), 6 (release), or cut with the reason (luv can''t carry it, or it costs far more than it shows).
- Record lasting design choices as decisions (ship propulsion, how death works, which planets are in scope).

## Part 5: story (priority 500)
`docs/story.md`, using the ship, places and features of `docs/design.md`.
- Setting (near future, early commercial spaceflight, the ship''s owner/pilot''s situation), characters, the prelude''s premise.
- A linear quest line of 5 to 10 quests that teaches piloting step by step (orbit raising, rendezvous, docking or grappling, moving an asteroid, a lunar transfer, a trip out and a close call with the Sun, …) and uses the ship''s work; each quest with goal, place, skills taught, milestone that makes it playable, completion condition.
- How quests are told: radio, messages, the cockpit''s screens; low-tech weapons only if the story needs them.
- Record the lasting story choices (names, premise) as decisions.

## Part 6: consistency check and sign-off (priority 400)
Last: make the milestone''s "Done when" true.
- The four documents agree with each other and with the skeleton (units, names, features, milestone assignments); every feature in `docs/design.md` is assigned to milestones 2 to 6 or cut; the story uses only features in scope by milestone 5.
- The skeleton builds and runs from a clean checkout with the `AGENTS.md` commands, its test passes, the screenshot command works.
- Add follow-up tasks for anything the later milestones need from these documents that is still missing.', '2026-10-14T16:06:30Z', NULL);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (2, 2, 'masterplan', 1000, 'tbd.', '2026-10-14T16:06:59Z', NULL);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (3, 3, 'masterplan', 1000, 'tbd.', '2026-10-14T16:07:00Z', NULL);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (4, 4, 'masterplan', 1000, 'tbd.', '2026-10-14T16:07:01Z', NULL);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (5, 5, 'masterplan', 1000, 'tbd.', '2026-10-14T16:07:15Z', NULL);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (6, 6, 'masterplan', 1000, 'tbd.', '2026-10-14T16:07:32Z', NULL);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (7, 1, 'luv capabilities', 900, 'Part 1 of `plan 1: masterplan`: find out what luv can carry for a solar-system flight game, by building luv and running its programs, tests and screenshot paths, not only by reading. The findings ground `plan 1: masterplan` parts 2 to 5 (skeleton, architecture, design, story), so every claim in the deliverables says how it was checked (ran, read, or assumed).

## Ground rules
- `../luv` is read-only: build and run it from its flake (`nix build`/`nix run` on `github:mbrock/luv` or `path:../luv`), or with build outputs in a temp dir; never edit or commit there, and leave no new files that `git -C ../luv status` would show (its `result` symlink already exists; leave it).
- Note the luv revision examined (`git -C ../luv rev-parse HEAD`) at the top of `docs/luv-capabilities.md`; part 2 pins the same revision in `flake.lock`.
- Every window opened is closed (`./sly stop-playing`, or `kill $!` by PID); no image or game left running; never kill by name pattern.
- Docs style per `AGENTS.md`: intro paragraph, then concise bullets.
- Headless/no-display findings matter: record what works without a display (agents run in such sessions).

## Reading list (in `../luv`)
- `AGENTS.md`, `README.md`, `CLAUDE.md`, `Makefile`, `flake.nix` (packages, apps, devShells, `applicationPackage`, the luv nix cache), `luv.asd`, `luft.asd`, `luvcraft.asd` (system names and dependencies).
- `wiki/`: `index.org`, `gpu-architecture.org`, `metal-backend.org`, `mathematical-shaders.org`, `sky-and-light.org`, `temporal-antialiasing.org`, `frame-performance.org`, `physics-and-simd.org`, `sb-simd.org`, `quantities-and-measurement.org`, `field-notes-measures.org`, `world-terminal.org`, `slug-bezier.org` (text), `technical-debt.org`, `luft-star-figures.org` (stars), `application-workbench.org`.
- Code: `hal/` (`gpu.lisp`, `canvas.lisp`, `application-lifecycle.lisp`, `application-capture.lisp`, `capture-specification.lisp`, `png.lisp`, `sdl/`, `vulkan/`, `metal/`, `shader/`), `luv/`, `terminal/`, `arithmetic/`, `luft/star-*.lisp`, `luvcraft/` (game loop, input, physics, sky), `scripts/` (`test.lisp`, `captures.lisp`, `readme-screenshots.lisp`, `build-application`), `sly`, `sly-init.lisp`.

## Run (each run noted in the doc with command, time, outcome)
- `nix run --accept-flake-config github:mbrock/luv` (Luvcraft) and `#luft`: does a window open here, how fast from the cache, what input it takes; close it.
- `nix build` of `#luvcraft`, `#luft`, `#luv-shaderc`; `luv-shaderc` on `hal/shaderc/examples/sky.lisp` into a temp dir.
- The test suite: `make test` / `make test-suite` / `make luft-test` inside `nix develop ../luv`, outputs kept out of `../luv` where possible; if a target can only write into `../luv/build` (ignored build output), note it.
- The live image: `./sly start`, `./sly play luft`, `./sly status`, `./sly screenshot /tmp/…png` (read the PNG), `./sly eval`, redefining a shader or function while playing, `./sly stop-playing`, stopping the image by PID. Find out whether `./sly` can serve a different checkout (this repo) or how part 2 should copy the setup.
- Screenshots and headless: `scripts/captures`, `hal/application-capture.lisp`: can a frame be rendered offscreen to PNG without a window or display (for tests and the screenshot command)?
- A tiny probe program loaded from a temp dir against luv''s ASDF systems: open a window, draw a sphere mesh with a Lisp shader, take a screenshot, close. This answers which systems a game loads (`luv`, `luv/shader`, `luv/terminal`, `luv/arithmetic`, …) and seeds part 2 (walking skeleton); keep its source in the doc or an appendix, not as repo code.

## Capabilities to report
- Rendering: meshes and vertex formats, Lisp shaders (stages, uniforms, textures, compute), lighting, shadows, sky/sun, bloom, fog, TAA, in-world text (Slug glyphs), terminal screens (Ghostty) and whether they can show game-generated text (instrument panels) without a shell.
- Windowing and input: SDL3, keyboard, mouse (relative mode), gamepad/joystick (SDL3 has it; does luv expose it?), resizing, fullscreen.
- Live image and dev loop: `./sly`, Swash, failure parking and `./sly resume`.
- Screenshots and headless runs, tests (Parachute), how luv runs its own tests.
- Physics and SIMD: what luvcraft''s physics is (voxel collision, not orbital), `sb-simd`, use of `double-float`; the quantities/units library and whether it fits SI orbital mechanics.
- Packaging: `applicationPackage` in luv''s flake, whether this repo can reuse it through the luv flake input (`lib`, overlays) or must copy it; the binary cache (`luv.swa.sh`).
- Platforms: Linux x86-64/ARM64 (Vulkan), macOS Apple Silicon (Metal 4); what was tested on this machine vs. only read.
- Performance: frame budget numbers from `frame-performance.org` or measured.

## Gaps to assess, each with a workaround in this repo
- Depth range across 10^12 m: reversed-Z with float depth, logarithmic depth, or layered passes (far bodies, near bodies, cockpit); check what depth format and clear luv uses and whether a game can set them.
- Double-precision positions: physics in `double-float` in Lisp, camera-relative single-float positions sent to the GPU.
- Spheres and planet surfaces: UV/icosphere generation, an Earth shader (texture vs. procedural continents, clouds, atmosphere rim), texture loading (which image formats luv loads; licences of public Earth textures, e.g. NASA Blue Marble).
- Starfield: luft''s star tables, or a catalogue (e.g. HYG, Yale BSC; licence noted) drawn as points or sprites.
- Sun glare and brightness: bloom from luvcraft''s sky, a sun disc, exposure across Earth-lit and deep-space scenes.
- Orbit lines and markers: does luv draw lines? otherwise thin quads or ribbons.
- HUD/2D overlay: screen-space text and shapes over the 3D view, vs. everything drawn on in-world cockpit screens.
- Audio: luv''s main shell has none (`sdl3-mixer` only in a side shell); SDL3 audio via cl-sdl3, or cut.
- Save files: luvcraft''s save path; a small s-expression save under `$XDG_DATA_HOME`.
- Anything else found (e.g. fixed physics step decoupled from frame rate, instancing many objects).

## Deliverables
- `docs/luv-capabilities.md`: luv revision, one section per capability area (what it offers, how it was checked, limits), the gaps each with its workaround, and a short "what this means for luv-in-space" summary that parts 2 to 4 of `plan 1: masterplan` build on.
- `docs/luv-wishlist.md`: what would better live in luv than here (e.g. a reversed-Z option, double-precision camera helpers, sphere meshes, line rendering, audio), each with why.
- Decisions recorded with `pm decision record`, at least: which luv ASDF systems the game depends on; how the game consumes luv (flake input, which package or lib output); the depth strategy and the precision approach as findings-backed recommendations (part 3, architecture, may supersede them); which platforms are targeted and tested.
- Follow-up tasks (`pm task add`) for gaps needing work outside this part, in the plan they belong to.

## Suggested split
- Read and summarise luv''s docs and systems (no running).
- Build and run luv''s programs, tests, live image and screenshots; record outcomes.
- Probe program: a sphere in a window plus a screenshot, from a temp dir against luv.
- Assess the gaps with workarounds; write `docs/luv-capabilities.md` and `docs/luv-wishlist.md`; record the decisions.', '2026-10-14T16:13:00Z', 8);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (8, 1, 'walking skeleton', 800, 'Part 2 of `plan 1: masterplan`: an ASDF system in this repo, built against luv pinned as a flake input, that opens a window and draws something recognisable, with a headless test and a screenshot command an agent can use, and `AGENTS.md` "Commands" filled in. It runs after `plan 7: luv capabilities` (priority 900) and builds on its findings and decisions: which luv ASDF systems to load, how a luv program is built and started (`applicationPackage`, `scripts/build-application`), how screenshots and headless runs work (`hal/application-capture.lisp`, `./sly screenshot`), and its probe program (sphere in a window plus a screenshot), which seeds this skeleton.

## Ground rules
- Read `docs/luv-capabilities.md` and the decisions of `plan 7: luv capabilities` first (`pm decision list --answered --grep luv --grep depth --grep platform`); they are binding unless superseded.
- `../luv` is read-only reference; the build uses luv from the flake input only, at the revision noted in `docs/luv-capabilities.md`.
- Every window opened is closed (by PID or `./sly stop-playing`); no process left running.
- Out of scope: game features. The scene is a placeholder, marked `PLACEHOLDER(<task id>)` in code and on screen.
- Before each commit the build and the test pass.

## Flake
- `flake.nix`: add `luv` as an input (`github:mbrock/luv`, pinned in `flake.lock`; `nixpkgs.follows` only if luv tolerates it, else keep luv''s nixpkgs for its cache); `nixConfig` with luv''s binary cache (`luv.swa.sh`) as luv''s flake does.
- `packages.default` (`luv-in-space`): built with luv''s `applicationPackage` if the input exposes it (`lib` or similar), else a copy adapted here, noted in `docs/luv-wishlist.md`.
- `apps.default`: starts the game (`nix run` from the repo root).
- `devShells.default`: luv''s dev shell environment (SBCL with luv''s Lisp deps, Vulkan/SDL3 libraries) plus `pm` and `git`; `direnv` (`.envrc` with `use flake`) keeps working.
- `checks.<system>.default`: the headless test, if it can run inside the nix sandbox (no GPU); otherwise the test is a dev-shell command and the reason is noted.
- Platforms: those decided in `plan 7: luv capabilities`; tested on this machine (Linux x86-64), others only evaluated (`nix flake check --no-build` or `nix eval`).

## ASDF system
- `luv-in-space.asd` at the repo root: system `luv-in-space` (sources under `src/`), `luv-in-space/tests` (under `tests/`, Parachute as luv uses it).
- `src/package.lisp`, `src/main.lisp` (entry point: open a window, run the frame loop, quit on Escape or window close), `src/scene.lisp` (the placeholder scene).
- The scene: a lit sphere (Earth placeholder, procedural colour, no texture) against a dark background with a few points or a sun direction; a visible `PLACEHOLDER(<task id>)` label (in-world text or terminal screen, whatever `plan 7: luv capabilities` found cheapest), the task id being the task that builds the scene.
- Shaders written in Lisp the way luv''s programs do; nothing that pre-empts the depth and precision decisions of part 3 (architecture), beyond what the probe already did.

## Test
- A headless test command (e.g. `make test` or `scripts/test`, run inside the dev shell): loads `luv-in-space/tests` and runs it with a non-zero exit on failure.
- Tests: the system loads; a pure function (e.g. sphere mesh generation: vertex count, unit radius) is correct; if luv supports offscreen rendering without a display, one frame renders to a PNG with the expected size and non-background pixels at the sphere''s centre.

## Screenshot
- A screenshot command (e.g. `scripts/screenshot out.png`): starts the game, renders some frames, writes a PNG, exits; works for an agent (from a terminal, without a human). Offscreen if luv allows it, else in a window that it closes itself.
- Optional `--frames N` or a time argument; the default shows the scene.

## Live image
- If `./sly` (luv''s Slynk-based live image) can load this repo''s system (from `../luv` or via the flake''s dev shell), a `./sly`-like wrapper here or documented steps: start the image, load `luv-in-space`, play, screenshot, redefine a function while playing, stop. If it can''t be reused, say why in `AGENTS.md` and add a follow-up task.

## AGENTS.md "Commands"
- Fill in: dev shell, build (`nix build`), run (`nix run`), test, screenshot, live image; each command one bullet.
- Verify from a clean checkout: `git clone` into a temp dir, run each command there, delete the clone; note the time a cold build takes.
- Record a decision for the lasting choices (`pm decision record`): the layout of systems and directories, the test and screenshot commands.

## Suggested split
- Flake: luv input, dev shell, package and `apps.default` building an empty system that opens a window.
- The placeholder scene with its `PLACEHOLDER` label.
- Headless test command and the screenshot command.
- Live-image dev loop (or the reason it''s skipped).
- `AGENTS.md` "Commands", verified from a clean clone; decisions recorded.', '2026-10-14T16:13:35Z', 9);

INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (1, 1, 'write the masterplan of milestone 1: Scope and design: what luv can carry, what the game is', 'masterplan', 'done', NULL, '2026-10-14, plan 1 body (tasks.sql)', 'Write `plan 1: masterplan`, the masterplan of `milestone 1: Scope and design: what luv can carry, what the game is`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:06:30Z', '2026-10-14T16:11:05Z', NULL);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (2, 2, 'write the masterplan of milestone 2: Flight core: Newtonian flight in near-Earth orbit', 'masterplan', 'open', NULL, NULL, 'Write `plan 2: masterplan`, the masterplan of `milestone 2: Flight core: Newtonian flight in near-Earth orbit`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:06:59Z', NULL, NULL);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (3, 3, 'write the masterplan of milestone 3: Cockpit and autopilot: the DIY cockpit, orbits and Earth-orbit presets', 'masterplan', 'open', NULL, NULL, 'Write `plan 3: masterplan`, the masterplan of `milestone 3: Cockpit and autopilot: the DIY cockpit, orbits and Earth-orbit presets`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:07:00Z', NULL, NULL);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (4, 4, 'write the masterplan of milestone 4: The solar system: Moon, asteroid belt, planets, the deadly Sun', 'masterplan', 'open', NULL, NULL, 'Write `plan 4: masterplan`, the masterplan of `milestone 4: The solar system: Moon, asteroid belt, planets, the deadly Sun`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:07:01Z', NULL, NULL);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (5, 5, 'write the masterplan of milestone 5: Story and quests: the playable prelude', 'masterplan', 'open', NULL, NULL, 'Write `plan 5: masterplan`, the masterplan of `milestone 5: Story and quests: the playable prelude`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:07:15Z', NULL, NULL);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (6, 6, 'write the masterplan of milestone 6: Release: one command to play, a README for people', 'masterplan', 'open', NULL, NULL, 'Write `plan 6: masterplan`, the masterplan of `milestone 6: Release: one command to play, a README for people`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:07:32Z', NULL, NULL);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (7, 1, 'create tasks for plan 1: masterplan', 'split', 'done', 1000, '2026-10-14, tasks 8-13', 'Split `plan 1: masterplan` into tasks and identify all decisions that need to be made. If the plan already has tasks, its
done and dropped ones stay as they are (pm refuses to change them): drop the open ones that no longer fit and add
tasks for what the plan asks beyond them. A task that writes a plan gets `--kind plan`.', '2026-10-14T16:11:02Z', '2026-10-14T16:11:47Z', 1);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (8, 1, 'write plan: luv capabilities', 'plan', 'done', 1000, '2026-10-14, plan 7: luv capabilities', 'Write the plan for part 1 of `plan 1: masterplan`: "luv capabilities".

- `pm plan list` first, then `pm plan add 1 "luv capabilities" --priority 900` with the full body.
- Body: the part''s bullets in `plan 1: masterplan`, made concrete: which `../luv` docs and code to read, which luv programs, tests and screenshot paths to build and run (via luv''s flake), which gaps to assess, each with a workaround in this repo.
- Deliverables of the plan: `docs/luv-capabilities.md`, `docs/luv-wishlist.md`, recorded decisions (e.g. which luv systems to depend on).
- Respect the ground rules of `plan 1: masterplan` (read-only `../luv`, docs style from `AGENTS.md`, close every window).

Done when: the plan exists with priority 900 and its "create tasks" task is queued.', '2026-10-14T16:11:45Z', '2026-10-14T16:13:02Z', 7);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (9, 1, 'write plan: walking skeleton', 'plan', 'done', 1000, '2026-10-14, plan 8: walking skeleton in tasks.sql', 'Write the plan for part 2 of `plan 1: masterplan`: "walking skeleton".

- `pm plan list` first, then `pm plan add 1 "walking skeleton" --priority 800`; it runs after the plan "luv capabilities" (priority 900) and depends on its findings on how a luv program is built, started and screenshotted; refer to it by id.
- Body: the part''s bullets made concrete: `flake.nix` with luv as a pinned input, package and `apps.default`, dev shell with SBCL/luv and `pm`; an ASDF system under `src/` drawing something recognisable with `PLACEHOLDER(<task id>)` where it shows; a headless test command; a screenshot command writing a PNG; the live-image dev loop if luv''s `./sly` can be reused; `AGENTS.md` "Commands" filled in and verified from a clean `git clone`.

Done when: the plan exists with priority 800 and its "create tasks" task is queued.', '2026-10-14T16:11:45Z', '2026-10-14T16:13:37Z', 7);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (10, 1, 'write plan: architecture', 'plan', 'open', 790, NULL, 'Write the plan for part 3 of `plan 1: masterplan`: "architecture" (`docs/architecture.md`).

- Priority 790 on this task: it runs after the work of the plans "luv capabilities" (900) and "walking skeleton" (800), so the plan is grounded in `docs/luv-capabilities.md` and the skeleton. Read them first.
- `pm plan list`, then `pm plan add 1 "architecture" --priority 700` with the full body.
- Body: the part''s bullets in `plan 1: masterplan` (dependency on luv, module layout, units and coordinates, depth strategy, time and integration, time warp, testing incl. the energy/angular-momentum drift tests that `milestone 2: Flight core: Newtonian flight in near-Earth orbit` requires), and the decisions to record (unit of length, coordinate frames, integrator, depth strategy).
- Check `pm decision list --answered` for decisions the earlier parts recorded and follow them.

Done when: the plan exists with priority 700 and its "create tasks" task is queued.', '2026-10-14T16:11:45Z', NULL, 7);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (11, 1, 'write plan: game design', 'plan', 'open', 690, NULL, 'Write the plan for part 4 of `plan 1: masterplan`: "game design" (`docs/design.md`).

- Priority 690 on this task: it runs after the work of the plan "architecture" (700), so the plan is grounded in `docs/luv-capabilities.md` and `docs/architecture.md`. Read them first.
- `pm plan list`, then `pm plan add 1 "game design" --priority 600` with the full body.
- Body: the part''s bullets in `plan 1: masterplan` (ships, flight, cockpit, places with real orbital data and sources, the prioritised scope list assigning every feature to milestones 2 to 6 or cutting it with the reason; read those milestones with `pm milestone show`), and the decisions to record (propulsion, how death works, planets in scope).
- Follow the answered decisions of the earlier parts (`pm decision list --answered`).

Done when: the plan exists with priority 600 and its "create tasks" task is queued.', '2026-10-14T16:11:45Z', NULL, 7);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (12, 1, 'write plan: story', 'plan', 'open', 590, NULL, 'Write the plan for part 5 of `plan 1: masterplan`: "story" (`docs/story.md`).

- Priority 590 on this task: it runs after the work of the plan "game design" (600), so the plan uses the ship, places and features of `docs/design.md`. Read it first.
- `pm plan list`, then `pm plan add 1 "story" --priority 500` with the full body.
- Body: the part''s bullets in `plan 1: masterplan` (setting, characters, premise; a linear quest line of 5 to 10 quests, each with goal, place, skills taught, the milestone that makes it playable, completion condition; how quests are told), and the decisions to record (names, premise).
- The story uses only features `docs/design.md` puts in milestones 2 to 5.

Done when: the plan exists with priority 500 and its "create tasks" task is queued.', '2026-10-14T16:11:45Z', NULL, 7);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (13, 1, 'write plan: consistency check and sign-off', 'plan', 'open', 490, NULL, 'Write the plan for part 6 of `plan 1: masterplan`: "consistency check and sign-off".

- Priority 490 on this task: it runs after the work of the plan "story" (500), when all four documents and the skeleton exist. Read them first.
- `pm plan list`, then `pm plan add 1 "consistency check and sign-off" --priority 400` with the full body.
- Body: the part''s bullets in `plan 1: masterplan`: the four documents agree with each other and with the skeleton (units, names, features, milestone assignments); every feature in `docs/design.md` is assigned to milestones 2 to 6 or cut; the story uses only features in scope by milestone 5; the skeleton builds, runs, tests and screenshots from a clean checkout with the `AGENTS.md` commands; follow-up tasks for what later milestones still need. Name the concrete checks you find needed after reading the documents.
- The milestone''s "Done when" in `milestone 1: Scope and design: what luv can carry, what the game is` is the target.

Done when: the plan exists with priority 400 and its "create tasks" task is queued.', '2026-10-14T16:11:45Z', NULL, 7);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (14, 7, 'create tasks for plan 7: luv capabilities', 'split', 'done', 1000, '2026-10-14, tasks 16-21 in plan 7; decisions identified (consumption of luv, ASDF systems, depth strategy, precision, target platforms) are recorded by tasks 17, 19, 20, 21; none need the human', 'Split `plan 7: luv capabilities` into tasks and identify all decisions that need to be made. If the plan already has tasks, its
done and dropped ones stay as they are (pm refuses to change them): drop the open ones that no longer fit and add
tasks for what the plan asks beyond them. A task that writes a plan gets `--kind plan`.', '2026-10-14T16:13:00Z', '2026-10-14T16:15:17Z', 8);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (15, 8, 'create tasks for plan 8: walking skeleton', 'split', 'done', 1000, '2026-10-14, tasks 22-27 in plan 8; no human decisions needed, lasting choices recorded by tasks 22-27', 'Split `plan 8: walking skeleton` into tasks and identify all decisions that need to be made. If the plan already has tasks, its
done and dropped ones stay as they are (pm refuses to change them): drop the open ones that no longer fit and add
tasks for what the plan asks beyond them. A task that writes a plan gets `--kind plan`.', '2026-10-14T16:13:35Z', '2026-10-14T16:16:18Z', 9);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (16, 7, 'Read luv''s docs and systems; draft docs/luv-capabilities.md', 'work', 'done', NULL, '2026-10-14, docs/luv-capabilities.md', 'First task of `plan 7: luv capabilities`: read luv and write the first draft of `docs/luv-capabilities.md`, from reading only (no running yet). Later tasks of the plan run things and fill in or correct the draft.

## Do
- Read the plan''s "Reading list" in `../luv` (read-only; never edit or create files there): `AGENTS.md`, `README.md`, `Makefile`, `flake.nix`, the `.asd` files, the listed `wiki/*.org` pages and code under `hal/`, `luv/`, `terminal/`, `arithmetic/`, `luft/star-*.lisp`, `luvcraft/`, `scripts/`, `sly`, `sly-init.lisp`.
- Note the luv revision examined (`git -C ../luv rev-parse HEAD`) at the top of the doc; later tasks keep the same revision unless they say why.
- Write `docs/luv-capabilities.md` (create `docs/`): intro paragraph, then one section per capability area of the plan ("Capabilities to report"): rendering, windowing and input, live image and dev loop, screenshots/headless/tests, physics and SIMD (incl. the quantities library vs. SI orbital mechanics), packaging (`applicationPackage`, flake outputs such as `lib`/overlays, the `luv.swa.sh` cache), platforms, performance.
- Also list luv''s ASDF systems with one line each (name, what it gives, its dependencies), flagging the ones a game likely loads.
- Every claim is tagged with how it was checked: `(read)` here; later tasks change it to `(ran)` or add `(assumed)`.
- Add headed sections "Runs" (filled by later tasks), "Gaps and workarounds" and "What this means for luv-in-space" (both filled by later tasks).
- Style per `AGENTS.md`: intro paragraph per top-level heading, then concise bullets; references into luv as `../luv/<path>`.

## Done when
- `docs/luv-capabilities.md` covers every area of "Capabilities to report" from reading, with the revision at the top; staged.', '2026-10-14T16:15:14Z', '2026-10-14T16:20:04Z', 14);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (17, 7, 'Build and run luv''s programs and test suite; record the runs', 'work', 'done', NULL, '2026-10-14, docs/luv-capabilities.md (Runs 1–14, Packaging, Platforms, Performance); decision 1: How the game consumes luv', 'Part of `plan 7: luv capabilities`: build and run luv from its flake and record what happens in `docs/luv-capabilities.md` (drafted by the plan''s first task; read it first).

## Ground rules
- `../luv` is read-only: use `nix run`/`nix build` on `path:../luv` (at the revision in the doc) or `github:mbrock/luv/<rev>`; build outputs into a temp dir (`--out-link /tmp/…` or `--no-link`); `git -C ../luv status` must show nothing new afterwards (its `result` symlink already exists; leave it).
- Every window opened is closed by PID (`kill $!`), never by name pattern. Run long commands in the foreground with a timeout.

## Do
- `nix run --accept-flake-config` of the default app (Luvcraft) and `#luft`: does a window open in this session (is there a display: `$WAYLAND_DISPLAY`/`$DISPLAY`), time to start from the cache, what input it takes; close it.
- `nix build` of `#luvcraft`, `#luft`, `#luv-shaderc`; run `luv-shaderc` on `hal/shaderc/examples/sky.lisp` into a temp dir; note what it outputs (SPIR-V, MSL).
- The test suite: `make test`, `make test-suite`, `make luft-test` inside `nix develop ../luv`; keep outputs out of `../luv` where possible, and if a target can only write to `../luv/build` (ignored), note that. Record pass/fail counts and duration; failures as found, not fixed.
- Can this repo consume luv''s packaging through the flake (`nix flake show path:../luv`: `lib`, `overlays`, `applicationPackage`) or must it copy it?
- Record each run in the doc''s "Runs" section: command, wall time, outcome; update the claims it confirms or contradicts from `(read)` to `(ran)`.
- Record a decision: `pm decision record "How the game consumes luv" --answer "…" --body "…"` (flake input `github:mbrock/luv` pinned to the examined revision; which package/lib output or a copied `applicationPackage`; the binary cache).

## Done when
- The "Runs" section lists every run above with its outcome; the decision is recorded; nothing left running; doc staged.', '2026-10-14T16:15:14Z', '2026-10-14T16:33:44Z', 14);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (18, 7, 'Try luv''s live image, screenshots and headless rendering; record the findings', 'work', 'done', NULL, '2026-10-14, docs/luv-capabilities.md (live image, screenshots/headless, runs 15–28, probe appendix); decision 2', 'Part of `plan 7: luv capabilities`: find out how an agent develops and checks a luv program: the live image (`./sly`), screenshots, and whether a frame renders without a window or display. Record findings in `docs/luv-capabilities.md` (read it first; its "Runs" section gets each run).

## Ground rules
- `../luv` is read-only. If `./sly` writes state (sockets, logs) inside `../luv`, use an option or environment variable that puts it elsewhere, or record which ignored paths it writes; `git -C ../luv status` stays clean.
- Stop everything you start: `./sly stop-playing`, then the image by PID; never by name pattern.

## Do
- Live image: `./sly start`, `./sly play luft`, `./sly status`, `./sly screenshot /tmp/<name>.png` (read the PNG and describe it), `./sly eval`, redefine a function or shader while playing and see the change, `./sly stop-playing`, stop the image by PID. Note Swash, failure parking and `./sly resume` as found.
- Can `./sly` serve another checkout (load this repo''s ASDF system), or how should `plan 8: walking skeleton` copy or wrap it (which files, which environment)?
- Screenshots and headless: `scripts/captures`, `hal/application-capture.lisp`, `hal/capture-specification.lisp`, `hal/png.lisp`: can a frame be rendered offscreen to PNG with no window and no display (e.g. `env -u WAYLAND_DISPLAY -u DISPLAY`)? Inside the nix sandbox (no GPU; lavapipe/llvmpipe)? This decides whether the skeleton''s test can be a `nix flake check`.
- How luv runs its own tests (Parachute, `scripts/test.lisp`), as a model for this repo''s test command.
- Update the doc''s live-image and screenshots/headless sections, tagging claims `(ran)`; screenshots are described in the doc, not committed (delete them from `/tmp` afterwards).

## Done when
- The doc says, with evidence, how an agent screenshots a luv program, whether that works headless, and how the live image can be used from this repo; nothing left running; doc staged.', '2026-10-14T16:15:14Z', '2026-10-14T16:43:04Z', 14);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (19, 7, 'Probe program: a lit sphere in a luv window plus a screenshot', 'work', 'done', NULL, '2026-10-14, docs/luv-capabilities.md (Rendering findings, runs 29–31, Appendix: probe program, gap notes); decision 3: luv ASDF systems the game depends on', 'Part of `plan 7: luv capabilities`: write a tiny luv program in a temp dir (not in this repo, not in `../luv`) that opens a window, draws a sphere mesh with a shader written in Lisp, takes a screenshot, and closes. It answers which ASDF systems a game loads and seeds `plan 8: walking skeleton`.

## Read first
- `docs/luv-capabilities.md` (runs, live image, screenshot findings so far); luvcraft and luft as examples of a program (window, frame loop, mesh upload, shaders, input).

## Do
- In `$(mktemp -d)`: an `.asd` and one or two `.lisp` files, loaded against luv''s ASDF systems from luv''s dev shell (`nix develop path:../luv`) or the live image; nothing written into `../luv`.
- Generate a UV sphere or icosphere mesh in Lisp; vertex and fragment shaders in luv''s Lisp shader language with simple diffuse lighting from a sun direction; a camera looking at it; dark background.
- Take a screenshot to `/tmp` (by the path the earlier findings recommend), read the PNG and describe it; close the window by PID or by the program quitting.
- If time allows: draw the sphere at Earth scale (6.4e6 m radius seen from 7e6 m) to see near/far plane and precision behaviour; note the depth format and projection luv uses.
- Add an appendix "Probe program" to `docs/luv-capabilities.md` with the full source, the commands to run it, how long it took, and what it showed; delete the temp dir.
- Record a decision: `pm decision record "luv ASDF systems the game depends on" --answer "…" --body "…"` (the systems the probe needed and why).

## Done when
- The appendix has a source that ran and produced a screenshot with a visible lit sphere (or the precise blocker, with a follow-up task in `plan 7: luv capabilities`); the decision is recorded; nothing left running; doc staged.', '2026-10-14T16:15:15Z', '2026-10-14T16:48:20Z', 14);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (20, 7, 'Assess luv''s rendering gaps for solar-system scale, each with a workaround', 'work', 'done', NULL, '2026-10-14, docs/luv-capabilities.md (Gaps and workarounds, runs 32-33, Appendix: gap probe); decisions 4 and 5', 'Part of `plan 7: luv capabilities`: assess what a solar-system flight game needs in rendering that luv lacks; check each against luv''s code (and the probe program in the appendix of `docs/luv-capabilities.md`, which can be re-run from a temp dir for small experiments); write each gap with a workaround in this repo into the doc''s "Gaps and workarounds" section.

## Gaps (each: what luv has, how checked (ran/read/assumed), the gap, the workaround here, its cost)
- Depth range across 10^12 m: luv''s depth format, clear value and projection; whether a program can set them; reversed-Z with float depth vs. logarithmic depth vs. layered passes (far bodies, near bodies, cockpit).
- Double-precision positions: `double-float` physics state in Lisp, camera-relative single-float positions sent to the GPU; where luv''s math/camera code assumes single floats.
- Spheres and planet surfaces: sphere generation; an Earth shader (texture vs. procedural continents, clouds, atmosphere rim); which image formats luv loads; licences of public Earth textures (NASA Blue Marble etc.; cite the licence text, don''t guess).
- Starfield: luft''s star tables vs. a catalogue (HYG, Yale BSC; licence noted), drawn as points or sprites.
- Sun glare and brightness: luvcraft''s sky/bloom, a sun disc, exposure across Earth-lit and deep-space scenes.
- Orbit lines and markers: does luv draw lines; else thin quads or ribbons.
- HUD/2D overlay: screen-space text and shapes over the 3D view vs. in-world cockpit screens (Slug text; terminal screens showing game-generated text without a shell).
- Instancing many objects (asteroids, markers).
- Record decisions as findings-backed recommendations (`docs/architecture.md`, written later in `plan 1: masterplan`, may supersede them): `pm decision record "Depth strategy" …` and `pm decision record "Precision of positions" …`.

## Done when
- Every gap above has an entry with a workaround; the two decisions are recorded; temp files deleted, nothing left running; doc staged.', '2026-10-14T16:15:15Z', '2026-10-14T16:56:43Z', 14);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (21, 7, 'Remaining gaps, luv wishlist, summary and decisions of the luv findings', 'work', 'done', NULL, '2026-10-14, docs/luv-capabilities.md (remaining gaps: audio, saves, fixed step and time warp, gamepad, other findings; summary), docs/luv-wishlist.md; decision 6: Target platforms; follow-up task 28', 'Last task of `plan 7: luv capabilities`: finish `docs/luv-capabilities.md`, write `docs/luv-wishlist.md`, record the remaining decisions and add follow-up tasks.

## Do
- Remaining gaps in "Gaps and workarounds" (each: what luv has, how checked, the workaround here): audio (luv''s main dev shell has none, `sdl3-mixer` only in a side shell; SDL3 audio via cl-sdl3, or cut); save files (luvcraft''s save path vs. a small s-expression save under `$XDG_DATA_HOME`); a fixed physics step decoupled from the frame rate and time warp; gamepad/joystick if luv doesn''t expose it; anything else the earlier tasks of the plan found.
- "What this means for luv-in-space": a short summary that `plan 8: walking skeleton` and the architecture, design and story parts of `plan 1: masterplan` build on: systems to load, how to build/run/test/screenshot, the depth and precision approach, what to cut or defer.
- `docs/luv-wishlist.md`: intro paragraph, then what would better live in luv than here (e.g. a reversed-Z option, double-precision camera helpers, sphere meshes, line rendering, audio, packaging as a flake `lib`), each with why; only items backed by the findings.
- Check the doc against the plan''s "Capabilities to report" and "Gaps to assess": every item present, every claim tagged (ran/read/assumed); the luv revision at the top matches what the runs used.
- Record a decision: `pm decision record "Target platforms" --answer "…" --body "…"` (targeted platforms; tested on this machine vs. only evaluated). Check the plan''s other decisions exist (`pm decision list --answered --grep luv --grep depth --grep precision`); record any that''s missing.
- Follow-up tasks (`pm task add`) for gaps needing work outside this plan, in the plan they belong to (e.g. `plan 8: walking skeleton`); don''t duplicate what plans already list.

## Done when
- Both docs complete per the plan''s "Deliverables"; decisions recorded; docs staged.', '2026-10-14T16:15:15Z', '2026-10-14T16:59:30Z', 14);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (22, 8, 'Flake: luv input, dev shell, package and nix run opening an empty window', 'work', 'done', NULL, '2026-10-14, flake.nix, flake.lock, luv-in-space.asd, src/package.lisp, src/main.lisp, scripts/build.lisp, .gitignore, docs/luv-wishlist.md; nix build 17 s warm; nix run opens the window, Escape quits (sent with xdotool under SDL_VIDEODRIVER=x11); dev shell loads luv-in-space; flake check --no-build passes for x86_64-linux, aarch64-linux, aarch64-darwin; follow-up task 29 (Darwin Objective-C bridge)', 'Part of `plan 8: walking skeleton`: make this repo build against luv pinned as a flake input, with a minimal ASDF system `luv-in-space` that opens an empty window and quits on Escape or window close.

## Read first
- `docs/luv-capabilities.md` (luv revision, packaging, `applicationPackage`, the binary cache, platforms, the probe program appendix) and the answered decisions: `pm decision list --answered --grep luv --grep platform --grep ASDF`. They are binding.
- `../luv/flake.nix` (read-only), `../luv/scripts/build-application`.

## Do
- `flake.nix`:
  - input `luv` = `github:mbrock/luv`, locked in `flake.lock` at the revision `docs/luv-capabilities.md` names; `inputs.nixpkgs.follows` only if luv builds with it and still hits its cache, else keep luv''s nixpkgs (note which in a short comment).
  - `nixConfig` with luv''s binary cache (`luv.swa.sh` and its public key) as luv''s flake has it.
  - `packages.default` (`luv-in-space`) with luv''s `applicationPackage` if the input exposes it; else a copy adapted here, and an entry in `docs/luv-wishlist.md` asking luv to export it.
  - `apps.default`: starts the game; `nix run` from the repo root opens the window.
  - `devShells.default`: luv''s dev shell environment (SBCL with luv''s Lisp deps, Vulkan/SDL3 libraries; reuse `luv.devShells.<system>.default` via `inputsFrom` or equivalent) plus `pm` and `git`; `.envrc` (`use flake`) keeps working.
  - Systems: the platforms decided in `plan 7: luv capabilities`; evaluate the non-Linux-x86-64 ones with `nix flake check --no-build` or `nix eval`.
- `luv-in-space.asd` at the repo root: system `luv-in-space`, sources under `src/`: `src/package.lisp`, `src/main.lisp` (entry point: window, frame loop clearing to a dark colour, Escape/close quits). Depends on the luv systems decided in `plan 7: luv capabilities`. Seed from the probe program in `docs/luv-capabilities.md`.
- Add `result` and other build outputs to `.gitignore`.

## Done when
- `nix build` succeeds; `nix run` opens a window that closes on Escape (close it yourself: by PID if Escape can''t be sent); `nix develop -c sbcl` can `(asdf:load-system "luv-in-space")`; `nix flake check --no-build` passes for all listed systems; `flake.lock` and the files staged; no process left running.', '2026-10-14T16:16:09Z', '2026-10-14T17:10:20Z', 15);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (23, 8, 'Placeholder scene: lit Earth sphere and a PLACEHOLDER label', 'work', 'done', NULL, '2026-10-14, src/shaders.lisp, src/scene.lisp, src/main.lisp, src/package.lisp, luv-in-space.asd', 'Part of `plan 8: walking skeleton`: draw a recognisable placeholder scene in the window the flake task made (`src/main.lisp`).

## Read first
- `docs/luv-capabilities.md`: the probe program appendix (sphere mesh, Lisp shaders, lighting), in-world text and terminal screens (which is cheapest for a label), depth and precision findings; answered decisions `pm decision list --answered --grep depth --grep precision --grep luv`.

## Do
- `src/scene.lisp` (add it to `luv-in-space.asd`):
  - a pure function generating a unit sphere mesh (UV sphere or icosphere; positions and normals), testable without a GPU;
  - an Earth placeholder: the sphere with a procedural colour (e.g. blue oceans, a few green/brown bands; no texture), diffuse light from a fixed sun direction, dark background; a few star points optional;
  - vertex and fragment shaders written in luv''s Lisp shader language, the way luv''s programs do;
  - a visible on-screen label `PLACEHOLDER(23)` with luv''s cheapest text path (in-world text or a terminal screen, per `plan 7: luv capabilities`).
- Mark the scene code with `PLACEHOLDER(23)` comments.
- Don''t pre-empt the architecture (part 3 of `plan 1: masterplan`): no depth or double-precision scheme beyond what the probe used; camera at a plain distance from a unit-ish sphere is fine.

## Done when
- `nix run` shows the lit sphere and the label; you took a screenshot (by the path `docs/luv-capabilities.md` recommends), read it and confirmed both are visible; window closed; `nix build` passes; files staged.', '2026-10-14T16:16:09Z', '2026-10-14T17:15:05Z', 15);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (24, 8, 'Screenshot command: scripts/screenshot writes a PNG of the scene and exits', 'work', 'done', NULL, '2026-10-14, scripts/screenshot, src/capture.lisp (screenshot, capture-scene, screenshot-main), src/main.lisp (frame count), src/package.lisp, luv-in-space.asd; ~5 s warm on GPU and headless lavapipe; nix run .#screenshot skipped (optional)', 'Part of `plan 8: walking skeleton`: a command an agent can run from a terminal, without a human, that starts the game, renders some frames, writes a PNG and exits.

## Read first
- `docs/luv-capabilities.md`: screenshots and headless runs (`hal/application-capture.lisp`, `capture-specification.lisp`, `./sly screenshot`, offscreen rendering without a display).

## Do
- `scripts/screenshot <out.png> [--frames N]` (executable, runs inside the dev shell; or `nix run .#screenshot -- out.png` as well if cheap): renders the placeholder scene, writes the PNG, exits with non-zero status on failure. Offscreen if luv allows it; else a window it closes itself (by the program quitting or by PID, never by name pattern).
- Default frame count shows the scene fully (label included).
- Reuse luv''s capture code through its ASDF systems; any capture glue here goes into `src/` (e.g. `src/capture.lisp`) so the test can reuse it.

## Done when
- `scripts/screenshot /tmp/lis.png` produces a PNG of the window''s size showing the sphere and the `PLACEHOLDER` label (read it and check); no process left running; the time it takes noted in the commit-ready files (e.g. a comment at the top of the script); files staged.', '2026-10-14T16:16:09Z', '2026-10-14T17:17:13Z', 15);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (25, 8, 'Headless test: luv-in-space/tests and a test command', 'work', 'done', NULL, '2026-10-14, luv-in-space.asd (luv-in-space/tests), tests/{package,scene,capture}.lisp, scripts/test, flake.nix (checks.<system>.default: scripts/test in the sandbox on lavapipe), src/capture.lisp (screenshot also returns pixels and format). scripts/test: 18 checks pass with display and headless on lavapipe (~5 s warm); a broken test exits 1; nix flake check passes (sandbox run ~16 s), --all-systems --no-build evaluates.', 'Part of `plan 8: walking skeleton`: a test system and a command that runs it with a non-zero exit on failure.

## Read first
- `docs/luv-capabilities.md`: how luv runs its tests (Parachute, `scripts/test.lisp`, `make test`), what works without a display or GPU, whether tests run in the nix sandbox.

## Do
- `luv-in-space/tests` in `luv-in-space.asd`, sources under `tests/` (Parachute, as luv uses it). Tests:
  - the system loads;
  - the sphere mesh function of `src/scene.lisp`: expected vertex/index counts for given subdivisions, every position at unit radius, normals unit length;
  - if luv renders offscreen without a display: one frame to a PNG (reuse the screenshot command''s capture code) with the expected size and a non-background pixel at the sphere''s centre; otherwise skip this test with the reason printed.
- `scripts/test` (executable, inside the dev shell): loads `luv-in-space/tests`, runs it, exits non-zero on failure.
- `checks.<system>.default` in `flake.nix` running the GPU-free tests, if they can run in the nix sandbox; if not, a short comment in `flake.nix` saying why, and the test stays a dev-shell command.

## Done when
- `scripts/test` passes; breaking a test (temporarily) makes it exit non-zero; `nix flake check` passes (or the reason it can''t run the test is noted); files staged.', '2026-10-14T16:16:09Z', '2026-10-14T17:19:20Z', 15);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (26, 8, 'Live image dev loop for luv-in-space', 'work', 'done', NULL, '2026-10-14, sly, sly-server.lisp, scripts/sly-client.lisp, scripts/build-sly-client, AGENTS.md, docs/luv-capabilities.md, docs/luv-wishlist.md; decision 8; follow-up task 30', 'Part of `plan 8: walking skeleton`: the live-image dev loop (luv''s Slynk-based `./sly`) for this repo''s system.

## Read first
- `docs/luv-capabilities.md`: the live image section (`./sly start`, `play`, `screenshot`, `eval`, `stop-playing`, failure parking, `resume`) and whether `./sly` can serve another checkout. `../luv/sly`, `../luv/sly-init.lisp` (read-only).

## Do
- If `./sly` can load this repo''s system: a `./sly`-like wrapper here (e.g. `./sly` at the repo root, adapted from luv''s, using the dev shell''s luv, never `../luv`) or documented steps, covering: start the image, load `luv-in-space`, play, screenshot, redefine a function (e.g. the sphere colour) while playing and see it change in a second screenshot, stop playing, stop the image by PID.
- If it can''t be reused: say why in `AGENTS.md` ("Commands") and add a follow-up task (`pm task add 8 …`) for what it would take; note in `docs/luv-wishlist.md` what luv could export to make it reusable.

## Done when
- The loop was run end to end once (two screenshots read, showing the change), or the reason it can''t be is written down with a follow-up task; no image or window left running; files staged.', '2026-10-14T16:16:09Z', '2026-10-14T17:23:48Z', 15);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (27, 8, 'AGENTS.md Commands, verified from a clean clone; skeleton decisions recorded', 'work', 'done', NULL, '2026-10-14, AGENTS.md (Commands, Layout); verified from a clean clone: nix build 17 s (luv from cache), nix run opens the window, scripts/test 18/18 ~5 s, scripts/screenshot with and without display ~3 s, ./sly start/play/screenshot/eval/stop ~5 s start; decision 9, decision 10, consequence on decision 8. Seen: luv''s sly-client prints a literal ~% after ''No failures are retained'' (luv bug, scripts/sly-client.lisp:2293 upstream; relevant to task 28)', 'Part of `plan 8: walking skeleton`, its last task: fill in the "Commands" section of `AGENTS.md` and prove each command works from a clean checkout.

## Do
- `AGENTS.md` "Commands": replace "to be filled in by `milestone 1`" with one bullet each for: dev shell, build (`nix build`), run (`nix run`), test (`scripts/test` or what the test task made), screenshot, live image (or why there is none). Keep the existing style (concise bullets).
- Verify: `git clone` the repo (HEAD holds the earlier tasks'' work; your uncommitted `AGENTS.md` edit isn''t needed there) into `$(mktemp -d)`, run each command there (close every window), note the time of a cold `nix build`/first run in `AGENTS.md`, delete the clone.
- Record the lasting choices with `pm decision record`, citing what they rely on:
  - layout of systems and directories (`luv-in-space.asd`, `src/`, `tests/`, `scripts/`);
  - the test command and the screenshot command (paths, arguments, headless or windowed);
  - the dev loop (live image wrapper, or none and why).
- Fix small breakages found in the clean clone; larger ones become follow-up tasks in `plan 8: walking skeleton`.

## Done when
- Every command in "Commands" ran from the clean clone; decisions recorded; `AGENTS.md` staged; clone deleted; no process left running.', '2026-10-14T16:16:09Z', '2026-10-14T17:25:52Z', 15);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (28, 8, 'Live image: report failed live artifacts', 'work', 'open', 790, NULL, 'Part of `plan 8: walking skeleton`, after `task 26: Live image dev loop for luv-in-space`.

luv''s live image keeps the last good pipeline when a shader redefinition fails and reports the failure nowhere but `luv:live-artifact-diagnostic` of the application''s live artifacts (status `:failed`); `./sly status` and `./sly failures` stay silent (`docs/luv-capabilities.md`, "Live image and dev loop", run 21; listed in `docs/luv-wishlist.md`).

## Do
- In this repo''s live-image setup from `task 26: Live image dev loop for luv-in-space`: make `status` (or `failures`) also print every failed live artifact of the running game with its diagnostic; if task 26 found the live image can''t be reused, a Lisp function the agent calls with `eval` instead.
- Check it: while the game plays, redefine a shader function with an unknown operator; the command names the shader and the error; fix it and the report is gone.
- Document the command in `AGENTS.md` "Commands".

## Done when
- The broken-shader check above was run; no image or window left running; files staged.', '2026-10-14T16:59:23Z', NULL, 21);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (29, 8, 'Darwin build: Objective-C bridge outside luv''s read-only source', 'work', 'done', NULL, '2026-10-14, flake.nix, docs/luv-wishlist.md: on aarch64-darwin luv loads from a writable copy (package $out/share/luv pruned to build/ bridge dylib and objective-c/ as luv''s applicationPackage does; dev shell ${XDG_CACHE_HOME:-~/.cache}/luv-in-space/luv-<shortRev>; check $TMPDIR/luv). Linux derivations byte-identical (same drv paths); nix flake check --no-build --all-systems passes; nix build and scripts/test pass. Darwin build untested: no Apple Silicon machine (decision 6: Target platforms).', 'Part of `plan 8: walking skeleton`. `task 22: Flake: luv input, dev shell, package and nix run opening an empty window` builds the game against luv''s sources straight from the flake input (`${luv}/` on `CL_SOURCE_REGISTRY` in `flake.nix`). On `aarch64-darwin`, luv''s `objective-c/exception-bridge.lisp` compiles a dylib to `<luv source>/../build/objective-c-exception-bridge-<stamp>.dylib`, i.e. into the read-only Nix store path, so `nix build` and loading `luv-in-space` in the dev shell will fail there. luv''s own `applicationPackage` avoids this by copying its tree into `$out/share/luv` and keeping `build/` and `objective-c/` on Darwin.

## Do
- In `flake.nix`, on Darwin only (`lib.optionalString pkgs.stdenv.hostPlatform.isDarwin`): copy `${luv}` into a writable directory (package: `$out/share/luv`, kept with its `build/` and `objective-c/` as luv''s recipe does; dev shell: a cache directory, e.g. `${XDG_CACHE_HOME:-$HOME/.cache}/luv-in-space/luv-<rev>`) and put that on `CL_SOURCE_REGISTRY` instead of `${luv}/`.
- Keep Linux unchanged.
- Verify with `nix flake check --no-build --all-systems`; a real build needs an Apple Silicon machine (`decision 6: Target platforms`: Darwin is evaluated only), so note it as untested if none is available.
- The wishlist entry already exists in `docs/luv-wishlist.md` (Objective-C bridge output directory).', '2026-10-14T17:08:42Z', '2026-10-14T17:28:42Z', 22);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (30, 8, 'Live shader pipelines for the scene', 'work', 'open', NULL, NULL, 'Part of `plan 8: walking skeleton`, follow-up of `task 26: Live image dev loop for luv-in-space`: shader redefinitions while playing in `./sly` should show on the next frame.

## Now
- `src/scene.lisp` `make-scene` calls the shader functions (`luv-in-space.shaders::earth-fragment` …) once and builds plain pipelines; a `define-shader` re-evaluated through `./sly eval` changes nothing on screen until `./sly stop-playing` and `./sly play` (`docs/luv-capabilities.md`, live image, run 34).
- luv''s Luft refreshes its pipelines live: a redefined shader function changed the next frame (run 20). luv exports `make-live-shader-pipeline`, `refresh-live-shader-pipeline`, `live-shader-pipeline-native-pipeline`, `application-live-artifacts` (`../luv/hal/package.lisp`, read-only reference; see how Luft uses them).

## Do
- Build the Earth and overlay pipelines as luv live shader pipelines (or the equivalent luv uses), registered as the game''s live artifacts, so a redefined shader is picked up while playing.
- Verify with `./sly`: play, screenshot, redefine `earth-fragment` (e.g. red oceans) by `./sly eval`, screenshot again, read both; stop playing and stop the image by `./sly stop`.
- A broken redefinition keeps the last good pipeline; reporting it is `task 28: Live image: report failed live artifacts`.
- `scripts/test` and `scripts/screenshot` still pass; update the live image bullet in `AGENTS.md` "Commands".', '2026-10-14T17:23:16Z', NULL, 26);

INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (1, 'How the game consumes luv', 'answered', '[]', 'Findings of `task 17: Build and run luv''s programs and test suite; record the runs` (docs/luv-capabilities.md, Packaging and Runs).
- luv''s flake exports apps, devShells, packages and lib; lib has only environmentFor, which returns everything the packaging needs (pkgs, lisp, applicationEnvironment/Packages, developmentEnvironmentHook, slynkSource, swashPackage, …). No overlays.
- applicationPackage (../luv/flake.nix:637) is not an output and is hard-wired to src = self, so it is copied (~120 lines) with src = this repo; noted for docs/luv-wishlist.md (export a lib.applicationPackage taking src).
- Do not set nixpkgs.follows: luv''s own pkgs keep the dependency closure identical to what luv.swa.sh/nix-cache serves (the dev shell''s Mesa, Vulkan tools, ffmpeg etc. came from it); the cache holds no x86_64-linux luv outputs at this revision, so the Lisp images are built locally (~40 s each).
- Pin the same revision as docs/luv-capabilities.md; bump deliberately, rerunning luv''s make test.
- Inherited LD_LIBRARY_PATH from the host broke Luvcraft (host alsa-lib needing glibc 2.43); our launcher and scripts unset it.
Part 3 (architecture) may supersede this.', 'Flake input github:mbrock/luv pinned to 6abc9d10ec8991c2cfe92888b84946372d6030b9; dev shell and app built from luv.lib.environmentFor (pkgs, lisp, developmentEnvironmentHook, applicationEnvironment) with a copy of luv''s applicationPackage recipe adapted here; luv''s sources on CL_SOURCE_REGISTRY from the input; luv''s binary cache in nixConfig; launchers unset LD_LIBRARY_PATH.', NULL, '2026-10-14T16:33:38Z', '2026-10-14T16:33:38Z', 17, NULL, 'rule', 17, '');
INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (2, 'Screenshots and GPU test: offscreen capture on a hidden canvas, headless on lavapipe', 'answered', '[]', 'Found by task 18 (docs/luv-capabilities.md, runs 24–28): a probe outside luv captured correct PNGs in a window, on a hidden canvas, with no DISPLAY/WAYLAND_DISPLAY (offscreen + lavapipe), and inside the nix build sandbox (no /dev/dri) in 21 s including compiling luv; Luvcraft''s full 3D frame also renders headless on lavapipe. Hidden canvases keep the requested size; visible windows get the compositor''s size, so tests should not use them. Binds task 24 (screenshot command) and task 25 (headless test); the architecture may supersede it.', 'Screenshots and the GPU test use luv''s capture-application-screenshot on a hidden SDL canvas (:visible-p nil); without a display the luv shell hook picks SDL''s offscreen driver and Mesa lavapipe; the GPU test runs as a nix flake check in the sandbox, built with lib.environmentFor''s slim development environment and hook.', NULL, '2026-10-14T16:42:58Z', '2026-10-14T16:42:58Z', 18, NULL, 'rule', 18, '');
INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (3, 'luv ASDF systems the game depends on', 'answered', '[]', 'The sphere probe of `task 19: Probe program: a lit sphere in a luv window plus a screenshot` (docs/luv-capabilities.md, "Appendix: probe program", runs 30–31) opened a window, drew a lit UV sphere with shaders in luv''s shader language, used a depth buffer and took an offscreen screenshot, visible and headless, with `:depends-on ("luv")` alone. Shader files use a package with (:use #:cl #:luv.shader) and (:shadowing-import-from #:luv.shader #:step). luvcraft/core is not loadable as a library (it pulls in Ghostty, libav and the whole game), so its sky, bloom and world-text code is copied and adapted here. luv/ghostty needs the full dev shell, not slim, which the GPU test (`decision 2: Screenshots and GPU test: offscreen capture on a hidden canvas, headless on lavapipe`) uses; adding it later is a separate choice.', 'The game system depends on "luv" only (it brings luv/shader, the SPIR-V/MSL lowerings and luv/arithmetic); tests add "luv/test-support". luv/ghostty, luv/terminal, luv/production and luvcraft/* are not dependencies until a feature needs one, and then each is recorded.', NULL, '2026-10-14T16:48:14Z', '2026-10-14T16:48:14Z', 19, NULL, 'rule', 19, '');
INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (4, 'Depth strategy', 'answered', '[]', 'Findings of task 20: Assess luv''s rendering gaps for solar-system scale, each with a workaround (docs/luv-capabilities.md, "Gaps and workarounds"). luv offers only depth32-float but any compare op and clear value. The standard projection lost Earth''s far side from orbit and drew nothing at the Moon''s distance; infinite reversed-Z with near 0.1 m drew both, and a textured Earth impostor, an orbit ribbon and stars in one pass (runs 30-33). Logarithmic depth via frag-depth costs early-Z for no gain; layered passes cost a pass per layer. A separate cockpit pass with its own depth clear stays the fallback. docs/architecture.md may supersede this.', 'Reversed-Z: depth32-float, an infinite far plane (depth = near/z, near 0.1 m), compare :greater, clear 0.0, in every pass; stars at infinity drawn first with compare :always and no depth write; planets as ray-traced impostors writing their depth through :frag-depth.', NULL, '2026-10-14T16:56:15Z', '2026-10-14T16:56:15Z', 20, NULL, 'rule', 20, '');
INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (5, 'Precision of positions', 'answered', '[]', 'Findings of task 20: Assess luv''s rendering gaps for solar-system scale, each with a workaround (docs/luv-capabilities.md, "Gaps and workarounds"). luv''s shaders have no 64-bit floats and write-buffer takes single-float arrays; base luv has no camera, and luv.arithmetic''s vec3 holds any real. Single floats give ~0.5 m at Earth''s radius and ~60 km at 1 AU. The sphere and gap probes computed the camera in double and sent camera-relative singles; Earth from 629 km and 34 Mm and the Moon from 384,400 km rendered correctly (runs 30-33). docs/architecture.md may supersede this, e.g. with per-body frames for the integrator.', 'World state in double-float on the CPU; each frame positions are made camera-relative in double and only then converted to single-float for the GPU; no world-space positions or matrices reach shaders; quantities that cancel in single float (e.g. |c|^2 - r^2 of a ray-sphere test) are computed in double on the CPU.', NULL, '2026-10-14T16:56:15Z', '2026-10-14T16:56:15Z', 20, NULL, 'rule', 20, '');
INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (6, 'Target platforms', 'answered', '[]', 'luv builds for exactly these three systems and has no Windows backend (docs/luv-capabilities.md, Platforms). Only x86_64-linux was available to run luv''s builds, tests, programs and probes (runs 1-33); the developer''s luv wiki measures on Apple Silicon, so aarch64-darwin is a target but untested here. Luft''s mesh-shader requirement failed on the Intel iGPU (runs 7, 17), so the game avoids mesh and task shaders. Clip-space y and other backend differences are kept in one projection function so a Metal check later touches one place.', 'Targets: x86_64-linux and aarch64-linux (Vulkan 1.4) and aarch64-darwin (Metal 4), the systems luv''s flake supports; no Windows. Tested: x86_64-linux only (this machine, Intel iGPU, NVIDIA and lavapipe). The others are evaluated (nix flake show/eval) until a machine runs them. No mesh or task shaders, so any Vulkan 1.4 iGPU works.', NULL, '2026-10-14T16:58:45Z', '2026-10-14T16:58:45Z', 21, NULL, 'rule', 21, '');
INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (7, 'Placeholder labels: dot-matrix overlay quads', 'answered', '[]', 'luv has no text layer a game can load without luvcraft/core (docs/luv-capabilities.md, HUD and instruments); the Slug copy costs ~400 lines, a dot-matrix overlay ~40 and reuses the triangle-list pipeline that also draws the placeholder stars. Added by task 23: Placeholder scene: lit Earth sphere and a PLACEHOLDER label.', 'On-screen PLACEHOLDER labels are 5x7 dot-matrix glyphs drawn as screen-space quads by the overlay pipeline in src/scene.lisp (glyph table *glyphs*), not Slug text; real text (instruments, HUD) waits for the Slug text copy the design plans.', NULL, '2026-10-14T17:15:05Z', '2026-10-14T17:15:05Z', 23, NULL, 'rule', 23, '');
INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (8, 'Live image: ./sly wraps luv''s client from the flake input', 'answered', '[]', 'luv''s ./sly serves only its own checkout and games (docs/luv-capabilities.md, live image). Redefining five functions on top of luv''s 2,400-line client keeps its other commands (eval, failures, resume, describe, xref, …) current with the luv pin, instead of a copy that drifts. Verified end to end in run 34 by task 26: Live image dev loop for luv-in-space.', './sly at the repo root builds build/sly-client from luv''s sly-client system (flake input) with play, screenshot, stop-playing, status and the dependency core redefined in scripts/sly-client.lisp; sly-server.lisp is luv''s, adapted to this root and the luv-in-space system.', NULL, '2026-10-14T17:23:33Z', '2026-10-14T17:23:33Z', 26, NULL, 'rule', 26, '2026-10-14, task 27: Verified from a clean clone by `task 27: AGENTS.md Commands, verified from a clean clone; skeleton decisions recorded`: ./sly start (~5 s), play, status, screenshot, eval, failures, stop-playing, stop all work; AGENTS.md Commands documents them.');
INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (9, 'Layout of systems and directories', 'answered', '[]', 'Follows `decision 3: luv ASDF systems the game depends on` (luv only; tests add luv/test-support) and `decision 1: How the game consumes luv` (luv''s sources from the flake input on CL_SOURCE_REGISTRY, so the root .asd is found from the repo root). One .asd keeps the game and its tests together as luv''s own programs do; new modules go under src/ and are listed in the .asd. Verified from a clean clone by `task 27: AGENTS.md Commands, verified from a clean clone; skeleton decisions recorded`.', 'luv-in-space.asd at the root with systems luv-in-space (src/) and luv-in-space/tests (tests/, Parachute); scripts/ holds the agent commands (test, screenshot, build.lisp, the sly client); ./sly and sly-server.lisp at the root; docs/ the design documents.', NULL, '2026-10-14T17:25:48Z', '2026-10-14T17:25:48Z', 27, NULL, 'rule', 27, '');
INSERT INTO decision (id, subject, status, options, body, answer, reason, created_at, closed_at, created_by_task, signoff, kind, decided_by_task, consequences) VALUES (10, 'Test and screenshot commands', 'answered', '[]', 'Relies on `decision 2: Screenshots and GPU test: offscreen capture on a hidden canvas, headless on lavapipe`. No window opens, so an agent needs no display and leaves no process running. Ran from a clean clone (`task 27: AGENTS.md Commands, verified from a clean clone; skeleton decisions recorded`): tests 18/18 in ~5 s, screenshot ~3 s, with and without DISPLAY. The live image (`./sly`) is the interactive dev loop, `decision 8: Live image: ./sly wraps luv''s client from the flake input`.', 'scripts/test runs luv-in-space/tests (exit 1 on failure); scripts/screenshot <out.png> [--frames N] writes a 1280x800 PNG and exits; both offscreen on a hidden canvas, headless on lavapipe without a display, and enter the dev shell themselves; nix flake check runs scripts/test in the sandbox.', NULL, '2026-10-14T17:25:48Z', '2026-10-14T17:25:48Z', 27, NULL, 'rule', 27, '');

INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (1, 1, '2026-10-14T16:10:25Z', '2026-10-14T16:11:12Z', 'done', NULL, 0, 'dbb0732a624bc26e8981cc36772e27859e6c5ea6', 'claude-opus-5-5[1m]', '332e5758-5e41-45e9-975c-f9674f9ae310', 5, 10, 4524, 144844, 45557, 0.4839448, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (2, 7, '2026-10-14T16:11:12Z', '2026-10-14T16:11:52Z', 'done', NULL, 0, '413343ae3d6bf69e5b19c1f2946215ec59767499', 'claude-opus-5-5[1m]', 'ef09344d-70e2-4295-94e9-29228b85f2ab', 5, 10, 4111, 147758, 29261, 0.3458996, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (3, 8, '2026-10-14T16:11:52Z', '2026-10-14T16:13:08Z', 'done', NULL, 0, '42cf81d600dbdeff190872b1fa14165d477205fd', 'claude-opus-5-5[1m]', '2f2a21a0-a00e-459d-bc9e-552a88c42cd3', 9, 18, 8020, 310398, 36150, 0.5117516, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (4, 9, '2026-10-14T16:13:08Z', '2026-10-14T16:13:46Z', 'done', NULL, 0, 'e98c78fecc390ad4cffcbee0ac1e76d680a398d5', 'claude-opus-5-5[1m]', '6bcb2306-9a2d-4200-8e9b-16a13d12798b', 6, 12, 3433, 176342, 25464, 0.30768840000000003, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (5, 14, '2026-10-14T16:13:46Z', '2026-10-14T16:15:23Z', 'done', NULL, 0, '2fcb3e5fd5091e1dc8c5d2f0260d407319f40a1c', 'claude-opus-5-5[1m]', 'c572bc8a-2e53-40b9-a246-e4bc234e996a', 7, 14, 11269, 241880, 38126, 0.57881999999999989, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (6, 15, '2026-10-14T16:15:23Z', '2026-10-14T16:16:26Z', 'done', NULL, 0, 'e91356694c977e6c7466802e01e8ace2a48297d3', 'claude-opus-5-5[1m]', '96ca9c6c-bac2-4d87-bdf0-70a7ebb81d26', 6, 12, 5980, 201764, 33006, 0.4240488, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (7, 16, '2026-10-14T16:16:26Z', '2026-10-14T16:20:13Z', 'done', NULL, 0, '9f803af1df3ad6323fd8a4a5a71bb22fb64deab3', 'claude-opus-5-5[1m]', '6a292265-a720-438a-a216-00807c1e244d', 52, 98, 21842, 6055094, 174051, 3.0406587999999997, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (8, 17, '2026-10-14T16:20:13Z', '2026-10-14T16:33:56Z', 'done', NULL, 0, 'ff970882d4690922ffcf424af4fb522fe9403b27', 'claude-opus-5-5[1m]', 'd6632261-8431-4fca-9dbc-8fe4c0ea1bbf', 48, 84, 22207, 2947294, 90082, 1.7545908, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (9, 18, '2026-10-14T16:33:56Z', '2026-10-14T16:43:15Z', 'done', NULL, 0, 'cbc15e05e0a0d485f3b5fda68c3cb215f6bdceb4', 'claude-opus-5-5[1m]', 'db8baacc-95f5-4da3-a464-74df82d7c729', 77, 146, 36764, 7701874, 148728, 3.4660627999999996, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (10, 19, '2026-10-14T16:43:15Z', '2026-10-14T16:48:29Z', 'done', NULL, 0, '952eda58e3898424d7d61a3e2f22c77a89551a49', 'claude-opus-5-5[1m]', '126d1b3b-e893-487b-ac6d-d8cf4773d5cf', 58, 94, 26832, 4387995, 127896, 2.4377829999999992, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (11, 20, '2026-10-14T16:48:29Z', '2026-10-14T16:56:54Z', 'done', NULL, 0, '8f9afba58ca5c38a40bd14a6599b7d594fea5045', 'claude-opus-5-5[1m]', '26a6c198-3e98-4fa4-a474-e0fe415bf74d', 83, 148, 44889, 7043054, 133413, 3.3742868, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (12, 21, '2026-10-14T16:56:54Z', '2026-10-14T16:59:42Z', 'done', NULL, 0, '20730acd44bcef0d6ee5d9d76713c87057554cf4', 'claude-opus-5-5[1m]', '39aa35c2-f34b-42a8-9ec3-dafcb3cececc', 27, 44, 16644, 1494666, 78224, 1.2577812000000002, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (13, 22, '2026-10-14T16:59:42Z', '2026-10-14T17:10:35Z', 'done', NULL, 0, '26482a088745fabef58945d49ff88cc9b4927b13', 'claude-opus-5-5[1m]', '22c28e5c-a217-41d0-bb2e-08976893162c', 42, 72, 19563, 2813863, 89273, 1.6685046000000001, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (14, 23, '2026-10-14T17:10:35Z', '2026-10-14T17:15:18Z', 'done', NULL, 0, 'bdef61bb679b0ee36f300bd5cae0abc0284929f2', 'claude-opus-5-5[1m]', '55f7a9d8-382b-48ff-9c2e-8443003d772f', 31, 58, 18903, 1910326, 81364, 1.4112692000000002, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (15, 24, '2026-10-14T17:15:18Z', '2026-10-14T17:17:19Z', 'done', NULL, 0, '26af09bf5050c98bfe3e028b09de120ed3dcac90', 'claude-opus-5-5[1m]', '37eb22f5-5b09-41fb-a4d0-2f2b76ad6a9c', 18, 36, 8615, 910029, 56344, 0.8052018, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (16, 25, '2026-10-14T17:17:19Z', '2026-10-14T17:19:27Z', 'done', NULL, 0, 'e7643cf988e7e4faded4e1accd9ac00e1350c1f0', 'claude-opus-5-5[1m]', '65140ef3-ccaf-4d7d-8346-2b5b5f9f36dc', 20, 38, 8969, 858436, 47821, 0.7337872, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (17, 26, '2026-10-14T17:19:27Z', '2026-10-14T17:23:57Z', 'done', NULL, 0, '939408b92a12a631e1683b3930671231a1d36c55', 'claude-opus-5-5[1m]', '8c582e0e-e67d-4d5e-8e6d-23520223cdc9', 34, 64, 21396, 2730166, 104263, 1.8083132, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (18, 27, '2026-10-14T17:23:57Z', '2026-10-14T17:26:00Z', 'done', NULL, 0, '43b13518ef638aac5c877d90d8ede25210e92c7a', 'claude-opus-5-5[1m]', 'f0b93c62-ae59-44aa-8bd9-854e9b8da5c4', 18, 32, 6225, 564502, 31931, 0.49297640000000009, 2177276);
INSERT INTO run (id, task_id, started_at, ended_at, result, reason, exit_code, base_commit, model, session_id, turns, input_tokens, output_tokens, cache_read_tokens, cache_creation_tokens, cost_usd, pid) VALUES (19, 29, '2026-10-14T17:26:00Z', '2026-10-14T17:28:49Z', 'done', NULL, 0, '09d185e3c020955ab7b76277e05daa1d14ee9e39', 'claude-opus-5-5[1m]', '3217bcb8-4b51-4d7d-8b83-a0dec8dbc916', 27, 44, 12904, 1049787, 49897, 0.86738939999999987, 2177276);
