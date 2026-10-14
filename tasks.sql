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

INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (1, 1, 'masterplan', 1000, 'tbd.', '2026-10-14T16:06:30Z', NULL);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (2, 2, 'masterplan', 1000, 'tbd.', '2026-10-14T16:06:59Z', NULL);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (3, 3, 'masterplan', 1000, 'tbd.', '2026-10-14T16:07:00Z', NULL);
INSERT INTO plan (id, milestone_id, subject, priority, body, created_at, created_by_task) VALUES (4, 4, 'masterplan', 1000, 'tbd.', '2026-10-14T16:07:01Z', NULL);

INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (1, 1, 'write the masterplan of milestone 1: Scope and design: what luv can carry, what the game is', 'masterplan', 'open', NULL, NULL, 'Write `plan 1: masterplan`, the masterplan of `milestone 1: Scope and design: what luv can carry, what the game is`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:06:30Z', NULL, NULL);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (2, 2, 'write the masterplan of milestone 2: Flight core: Newtonian flight in near-Earth orbit', 'masterplan', 'open', NULL, NULL, 'Write `plan 2: masterplan`, the masterplan of `milestone 2: Flight core: Newtonian flight in near-Earth orbit`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:06:59Z', NULL, NULL);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (3, 3, 'write the masterplan of milestone 3: Cockpit and autopilot: the DIY cockpit, orbits and Earth-orbit presets', 'masterplan', 'open', NULL, NULL, 'Write `plan 3: masterplan`, the masterplan of `milestone 3: Cockpit and autopilot: the DIY cockpit, orbits and Earth-orbit presets`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:07:00Z', NULL, NULL);
INSERT INTO task (id, plan_id, subject, kind, status, priority, note, body, created_at, retired_at, created_by_task) VALUES (4, 4, 'write the masterplan of milestone 4: The solar system: Moon, asteroid belt, planets, the deadly Sun', 'masterplan', 'open', NULL, NULL, 'Write `plan 4: masterplan`, the masterplan of `milestone 4: The solar system: Moon, asteroid belt, planets, the deadly Sun`: expand on everything that needs to be done to achieve it,
divided into parts. State in the masterplan that its own tasks are not implementation work: each of them writes the
plan for one part.', '2026-10-14T16:07:01Z', NULL, NULL);
