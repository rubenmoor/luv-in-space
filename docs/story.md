# Story

This document is the story of Luv in Space's prelude, written for the people and agents who implement it: the setting, the characters, the premise, the quest line and how it is told. `milestone 5: Story and quests: the playable prelude` builds it (`quests.lisp`, RADIO and JOB, static texts, saves, a headless play-through), and `milestone 6: Release: one command to play, a README for people` takes its introduction from it. It uses only what `docs/design.md` "Scope", "What the story may use" assigns to milestones 2 to 5, and the places, ships and figures of `docs/design.md`; it cites the pm decisions it relies on by id and subject. Its own lasting choices are `decision 48: Story: setting and premise`, `decision 49: Story: names, call signs and ships`, `decision 50: Story: start date and the passing rock's date`, `decision 52: Story: the quest line` and `decision 57: Story: how it is told, its texts and the story ships' schedule`. "Names and terms" at the end is the one list of names that code and the other documents use.

## Setting

Near-Earth space in January 2040 is a small working place, not a frontier of wonders: about twenty people off Earth (Tern Station's six, Highport's four, Peary Camp's four on the Moon, five tug pilots), most of them in one polar orbit, living on propellant made from water. The technology is what was flown or tested by the late 2020s, scaled up a little; nothing is secret and nothing is alien. The danger is the trade's own: a spinning derelict, a heavy load, a long transfer, a periapsis too low, the Sun for whoever flies too far in (`docs/design.md` "Weapons"). There are no weapons (`decision 41: Weapons: cut`) and no money in the game (`decision 26: Refuelling and economy`): the companies below pay their pilots, and the story never names a price.

### The polar plane
- Everything in Earth orbit shares one polar plane, i 90°, Ω 270° (`decision 38: Start orbit: polar`, `docs/design.md` "One plane for near-Earth space"): J2 leaves a polar plane where it is, so places at different heights stay together and a tug pays only for height and phasing. Pilots call it "the plane".
- Crews launch from high-latitude sites (Andøya in Norway is the one named) into the plane; the crew capsules are Boreal Orbital's.
- What keeps the plane alive is water: small carbonaceous rocks, netted by uncrewed bagger probes and pushed in by tugs, are cooked at Highport for their water, which is split into hydrogen and oxygen. Every tank in the plane, and every tank going to the Moon, is filled from it. Earth-launched propellant still exists but costs a launch; rock water is the cheaper way up, and the plane's people know they live on it.
- The Moon is three to five days away: Peary Depot in lunar orbit is fed with ice water from Peary Camp on the surface near the north pole, which the game never shows (landing is cut, `docs/design.md` "The Moon").

### Who owns what
- **Cairn Water**: the water company and the pilot's employer. Owns Highport and its water plant, Skua Yard and its rocks, the rock mover GRINDSTONE, the bagger probes, the tankers, Peary Depot and Peary Camp, and four Mule tugs: PATIENCE (the player's), STUBBORN, THISTLE and THRIFT. Run from Highport by a dispatcher, CAIRN OPS.
- **Boreal Orbital**: the plane's landlord in low orbit. Owns Tern Station (the crew station, home port of anyone working low) and Fulmar Depot (the low propellant depot, filled by Cairn tankers), and flies the AURORA crew capsules from Andøya. Boreal buys its propellant from Cairn and does not like depending on it.
- **Lindgren Debris**: a debris-removal firm of a dozen people in Kiruna, Sweden. Its uncrewed servicers bolt grapple fixtures onto dead stages; it hires tugs for the push, since its servicers are too small to move 2.5 t. K-17 is the seventeenth object on its contract list.
- Nobody else works in the plane. Satellites and other nations' stations are elsewhere and not shown (`docs/design.md` "Not places").

### A tug pilot's life
- One person in a cab 3 m across and 4.5 m long, for days at a time; 180 person-days of stores (`decision 22: The player's ship and its propulsion`), restocked at Tern Station, Highport or Peary Depot. A jump seat that is seldom used.
- The work is pushing: propellant pods, rocks, a dead stage now and then; a pilot is paid for loads delivered in one piece, and a load is moved by thinking first and burning later. Most of a job is coasting, with the time warp; the minutes that matter are a burn and the last 100 m to a fixture.
- Radiation is the career's clock: a pilot flies until the career dose limit, then goes down for good. Six to eight years in the plane is a long career.
- Pilots know each other by their ships' names. The radio is terse, numbers first; jokes are short and dry.
- Tugs are old, patched and personal: every cab has its tape, its notes and its habits, left by whoever flew it before.

### Start date
- `*start-date*` is **2040-01-01 00:00 UTC**, the default kept (`decision 14: Units and time epoch`; game time before 2050-01-01, `decision 18: Ephemerides`). The start orbit's phase (argument of latitude 0° at the start), and so its lighting, are unchanged (`decision 38`): sunlit, 25° from the Sun, β −10°; the January Blue Marble (`decision 40: Earth's look and drag`) stays the right month.
- Why this date: Highport's first lunar window comes about 20 days after the start, enough for the Earth-orbit quests, and it is the best window of the year.
  - The window: a TLI from Highport's perigee raises the apogee along Highport's line of apsides, so the Moon must stand at Highport's apogee direction when the tug arrives there (`docs/design.md` "Highport", "The Moon": RA 90°, Dec ~+24°, half a TLI ellipse of a = (7,378 + 384,400) / 2 km, π √(a³/μ⊕) = 4.99 days later).
  - Method: the Moon's geocentric position from Meeus, "Astronomical Algorithms", ch. 47 (the 32 largest longitude and distance terms and 20 latitude terms of tables 47.A and 47.B, with the A1–A3 corrections), precessed to the J2000 ecliptic (−1.397° per century in longitude) and rotated by the obliquity 23.4392911° to ICRF; checked against Meeus's example 47.a (1992-04-12 0h TD): within 0.10° in RA and 0.03° in Dec, the remainder nutation and aberration. A throwaway script, not committed.
  - Highport's apogee direction drifts: at i = 90° J2 turns ω by −(3/4) n J2 (R⊕/p)² = −0.0665°/day (n = 7.367×10⁻⁵ rad/s, p = 13,456 km), so the apogee climbs north from Dec +24.0° at the start by 2° a month.
  - The Moon's crossings of the polar plane on Highport's apogee side (RA 90°), with the angle between Moon and apogee direction:

| Moon at RA 90° (UTC) | Game day | Moon Dec | Distance (km) | Apogee Dec | Angle |
|---|---|---|---|---|---|
| 2040-01-26 02:36 | 25.1 | +25.1° | 405,900 | +25.7° | 0.6° |
| 2040-02-22 09:44 | 52.4 | +25.3° | 405,000 | +27.5° | 2.2° |
| 2040-03-20 17:37 | 79.7 | +25.6° | 403,900 | +29.3° | 3.8° |
| 2040-04-17 01:47 | 107.1 | +25.8° | 403,400 | +31.1° | 5.4° |

  - So the TLI from Highport's perigee falls near **2040-01-21** (game day 20) for arrival on 2040-01-26; the autopilot's choice is the 2040-01-20 06:01 perigee (`decision 55`, `docs/design.md` "Into another sphere of influence: Peary Depot"); the February window (TLI near 2040-02-17) is the fallback, 2.2° off, a few m/s of mid-course correction more. The autopilot's TLI search looks 30 days ahead (`docs/design.md` "The autopilot"), so it always finds one of them.
  - The time the earlier quests take, roughly ("The quest line", `decision 52: Story: the quest line`, fixes it): Tern Station and Fulmar Depot 1–2 days, K-17 2 days, the transfer to Highport 1–2 days, a Skua Yard tow ~2 days (one 23.7 h phasing orbit): about 8 days, leaving 12 days of slack before the January TLI.
- The passing rock (`milestone 4` "should", `docs/design.md` "Asteroids"): **2040 BQ7** (Minor Planet Center style: B for a discovery between 16 and 31 January 2040, Q7 the 191st of that half-month), found by a survey on 2040-01-19, perigee **2040-03-12 12:00 UTC** at 7,378 km in the polar plane.
  - The tug meets it about 1.5 days before perigee near 325,000 km on Highport's apogee side (~2040-03-10 23:00), leaving Highport's orbit shortly after a perigee pass ~5.3 days earlier, near 2040-03-05: the autopilot's cheapest intercept (`decision 55: Meeting 2040 BQ7 and the lunar arrival from the polar plane`, `docs/design.md` "Rendezvous: search, phasing and the approach"; at 400,000 km, 2.08 days out, the rock is 17.6° off the transfer's apogee and the meeting costs ~400 m/s more).
  - The meeting must keep clear of the Moon, which stands at that point at its RA 90° crossings: on 2040-03-10 12:00 the Moon is at RA 320°, Dec −21°, 134° from the apogee direction and over 600,000 km from the meeting point; at perigee (2040-03-12) still 106° away.
  - Slack: a pilot back from the January lunar window (at Highport ~2040-02-01) waits about five weeks, which the story fills (the find, the bagger probe's trip out by RADIO, and quest 8, "The quest line"); one back from the February window (~2040-03-01) has four days.

## Characters

Few people, each with a job, a place and a way of talking; nobody has a face or a voice beyond text (`decision 37: Message channels the story may use`: RADIO is text only, no reply choices). The pilot is the player and is never described; the others are known by their call signs. Every speaker has a location, from which the light time to the ship is computed (`docs/design.md` "Messages and radio").

### The pilot
- The player: newly certified, up from Andøya on an AURORA capsule to Tern Station to take over PATIENCE. No name, gender, face or past is shown; messages address the pilot as "Patience" (the ship's call sign) or "you".
- What the pilot wants, as far as the story says: to keep the seat, to be trusted with bigger loads, and to go farther than the plane.

### The ship
- **PATIENCE**, Cairn Water's Mule-class tug, registration **M-07** (`decision 22`), call sign "Patience". Six years in service, rebuilt cab rack, mismatched screens (`docs/design.md` "The DIY look").
- Its previous pilot: **Mirela Doru**, who flew PATIENCE for six years and reached the career dose limit in December 2039. Mirela went down to Tromsø and left the cab as it was: tanks full, three tape notes on the console.

### Radio speakers
| Call sign | Name | Role | Where (for light time) | Voice in text | Sender colour |
|---|---|---|---|---|---|
| CAIRN OPS | Rafael Quiroga | Cairn Water's dispatcher; gives most jobs | Highport | short, numbers first, dry; "Patience, Cairn Ops." | cyan |
| TERN | Ana Folau | Boreal Orbital's commander of Tern Station; controls Tern's and Fulmar's berths | Tern Station | formal, safety first, procedural callouts ("cleared to approach, 100 m hold") | white |
| LINDGREN | Sami Lindgren | owner of Lindgren Debris; hires the K-17 push | Kiruna, Earth's surface | eager, a few words too many, grateful | white |
| HIGHPORT | Esther Okafor | chief of Highport's water plant; takes delivered rocks | Highport | precise, mass and water fractions in kg and % | cyan |
| GRINDSTONE | Ruth Abara | pilot of the rock mover GRINDSTONE, the plane's oldest hand | Skua Yard | laconic, one line at a time, tips without saying they are tips | cyan |
| THRIFT | Dag Eliassen | pilot of the Mule THRIFT, the pilot's nearest colleague | THRIFT's slot: Highport's orbit, from 2040-01-26 Peary Depot's orbit (the same if THRIFT does not move, "Other ships") | friendly, teasing, talks about Mirela and the old days | cyan |
| PEARY | Tomasz Wierzbicki | lead of Peary Camp; runs Peary Depot from the surface | the Moon (Peary Camp near the north pole; 1.3 s from Earth orbit) | slow, warm, mentions the delay | cyan |
| MIRELA | Mirela Doru | PATIENCE's previous pilot, retired | Tromsø, Earth's surface | rare, short, personal; never gives orders | white |

- Cyan for Cairn Water's senders (the pilot's employer, `docs/design.md` "Messages and radio"), white for others.
- A speaker on a surface (Kiruna, Tromsø, Peary Camp) is placed at the body's centre for the light time; the error is under 0.02 s.

### Other ships
Built from the tug's parts or a capsule frustum and on Kepler rails only (`decision 27: Other ships`, `docs/design.md` "Other ships"). Ships that are part of a place's shapes and mesh (`docs/design.md` "Rigid bodies: attitude, mass, shapes and fixtures") never move; the moving ones are `milestone 5`'s "should" ("Story ships moving between places"), departing and arriving out of view (elements changed beyond 200 km). If moving ships are dropped, those ships are not drawn and the story only tells of their trips by RADIO.

| Ship | Kind | Owner | Where | Moves |
|---|---|---|---|---|
| STUBBORN (M-04) | Mule tug, Cairn livery | Cairn Water | berthed at a Highport arm | no (part of Highport) |
| THISTLE (M-05) | Mule tug, Cairn livery | Cairn Water | berthed at a Highport arm | no (part of Highport) |
| THRIFT (M-09) | Mule tug, Cairn livery, pilot Dag Eliassen | Cairn Water | parked in Highport's orbit, 5 km ahead of Highport (`docs/design.md` "Earth-orbit points of interest"), until 2040-01-21 06:00; from 2040-01-26 06:00 parked in Peary Depot's orbit, half a turn ahead of the depot, for the rest of the line (Peary's tug for the winter) | should: Highport → Peary Depot once; if dropped, told only, never drawn |
| TANKER 2 | uncrewed tanker | Cairn Water | berthed at Fulmar Depot's fixture B | no (part of Fulmar) |
| TANKER 3 | uncrewed tanker | Cairn Water | parked in Highport's orbit, 2 km ahead of Highport, until 2040-01-21 06:00; 2 km ahead of Fulmar Depot in its orbit from 2040-01-22 06:00 (the winter fill) until 2040-03-06 06:00; back in its slot ahead of Highport from 2040-03-07 06:00 | should: Highport → Fulmar Depot → Highport; if dropped, told only, never drawn |
| AURORA 11 | crew capsule | Boreal Orbital | berthed at Tern Station's node | no (part of Tern) |
| GRINDSTONE | rock mover, pilot Ruth Abara | Cairn Water | parked in Skua Yard with its 10 m rock | no (fixed, `decision 47: Rigid bodies of the places`) |
| BAGGER 3 | uncrewed bagger probe: a box 1.5 m with two small solar wings | Cairn Water | nets 2040 BQ7 out beyond the Moon, then rides 1 km ahead of it on its rails, on the far side from the pilot's approach; after the grapple it flies on past perigee and out of Earth's sphere, out of view | with the rock (`milestone 4` "should", scenery on rails as `docs/design.md` "Asteroids" has it); if the passing rock is dropped, told only |

- Cairn Water's livery: white cab, a grey cairn of three stacked stones and the registration stencilled in black on both cab sides. Boreal Orbital's: white with a pale green band.

### Static texts
Fixed for the whole game (`decision 37`; `docs/design.md` "The DIY look", "Messages and radio"); exact wording, in capitals as stencilled or hand-lettered.
- Above the window, a stencilled steel plate: `M-07  PATIENCE`
- Tape notes (gaffer tape, white marker, Mirela's hand, drawn like the tape labels of `docs/design.md` "The DIY look", one 50 mm strip per line), each with its place on the console (`docs/design.md` "Layout") and its line breaks (`/`), so each fits its tape at the readability rule's cap height (`docs/design.md` "Readability at 1280 × 800": ≤ 24 characters on 0.30 m, ≤ 16 on 0.20 m):
  1. Under the TARGET screen (strips up to 0.30 m): `CLOSE AT 0.1. / THE PLATE WON'T MIND. -M`
  2. Under the SHIP screen (up to 0.26 m): `LOADED SHE TURNS / LIKE A BARGE. / START EARLY.`
  3. On the rack beside the hull temperature gauge, towards the window (up to 0.20 m; the gauge's Ø 0.12 m bezel is too small for it): `NOTHING INSIDE / 0.55 AU IS / WORTH IT.`

## Premise

The pilot starts in PATIENCE in the start orbit, 400 km up, tanks full, the day after Mirela's last entry in the log. Cairn Water has kept the seat warm because the plane is short of pilots this winter: the January lunar window is coming, Highport's plant wants rock, Lindgren Debris has paid for a push nobody had time to fly, and survey telescopes keep finding small rocks that pass close. The prelude is the pilot's first two and a half months: each job a little further out and a little heavier, from a 20 km hop to Tern Station to the Moon and to a rock that will not come back.

### What goes on around the pilot
- Cairn Water is proving that rock water can fill every tank in the plane and at the Moon. Its rocks have so far been fetched by GRINDSTONE on long trips; catching a small rock as it passes Earth, with a bagger probe and one tug, would be cheaper, and 2040 BQ7 is the first chance.
- Boreal Orbital depends on Cairn's tankers for Fulmar Depot and is uneasy about rocks: Ana Folau objects by RADIO to a 50 t rock passing 1,000 km above the plane's crew station, and wants the capture flown by the book. The quarrel is told, never fought.
- Lindgren Debris needs K-17 down before its contract runs out; Sami Lindgren is a small operator with a big promise to keep.
- Dag Eliassen flew with Mirela for years and watches the new pilot with some doubt and more goodwill.

### The ending
- The prelude ends at Highport, berthed, after 2040 BQ7 is in Highport's orbit and delivered to Skua Yard (or, if the passing rock is dropped, after quest 8, the 100 t tow; the ending's messages stay the same apart from the rock's assay, "The quest line").
- Esther Okafor's assay: BQ7 is 21 % water by mass, wetter than anything fetched so far. Cairn Water's answer comes by RADIO from CAIRN OPS: the company will build a long-haul tug, a Mule cab with two years of stores and twice the tanks, to fetch rock from farther out than any tug has flown, and it wants the pilot, who has just caught BQ7, to fly it. Mirela's last message: PATIENCE is the pilot's now, and Mirela would take the long-haul seat if the dose limit allowed it.
- The hook, not a resolution: the long-haul trip is the longer adventure. Nothing of it is flown here: its destinations are cut from this game (`docs/design.md` "Scope": destinations beyond the Moon and the passing rock), and it is told only by RADIO and the last job card.

### Introduction
The text shown before the first flight (`milestone 6`, `docs/design.md` "Feature list": "Short introduction before the first flight"); also the README's short introduction:

> January 2040. About twenty people live and work off Earth, most of them in one polar orbit, and most of what they need is pushed there by tugs: steel frames with a hydrogen engine, a small cab and a claw on the nose.
>
> You are the new pilot of PATIENCE, a Mule-class tug of Cairn Water. Its last pilot, Mirela Doru, flew it for six years, until the radiation dose limit. Mirela left the tanks full and three notes taped to the console.
>
> You are 400 km up, on an orbit over both poles. Cairn Ops has your first job.

## The quest line

Nine quests in a fixed order, each a job a little farther out and a little heavier, teaching one piloting skill after another: attitude and a small burn, rendezvous and berthing, a grapple and a push by hand, refuelling, the long climb to Highport, towing, the Moon, a 100 t load and finally a rock that passes Earth once. `milestone 5: Story and quests: the playable prelude` implements them in `quests.lisp`; every completion condition reads the simulation state of `docs/architecture.md` "Checkpoints", so the headless play-through can test it. The quest line is `decision 52: Story: the quest line`. Δv figures are cited from `docs/design.md` ("Δv for the standard legs", "Earth-orbit points of interest", "The Moon", "Asteroids", "Δv checked against the ship"); a leg not listed there gives its formula and result (a throwaway script, not committed; μ⊕ and R⊕ of `decision 14: Units and time epoch`, v_e = 4,410 m/s).

| # | Title | Giver | Place(s) | Skills | Milestone | Δv (m/s) | Time (game) |
|---|---|---|---|---|---|---|---|
| 1 | Shakedown | CAIRN OPS | start orbit, 400 km | manual turns, rate damping, holds, throttle, Ap/Pe, warp | 2 | 6 | 2 h |
| 2 | Home port | TERN | Tern Station | targets, autopilot rendezvous, target and relative holds, grapple camera, berthing | 3 | 31 | ≤ 1 day |
| 3 | Seventeen | LINDGREN | Derelict K-17, 800 km | grappling a towable load, a burn below the autopilot's floor by hand, PERI alarm, release | 3 | 622 | ≤ 2 days |
| 4 | Topping off | CAIRN OPS | Fulmar Depot | rendezvous alone, berthing beside a tanker, propellant transfer (B) | 3 | 126 | ≤ 2 days |
| 5 | The long climb | CAIRN OPS | Highport | a multi-burn plan, the Burn instrument, a long coast at warp, an eccentric orbit | 3 | 2,799 | ≤ 3 days |
| 6 | Rock three | HIGHPORT | Skua Yard, Highport | towing 50 t, a heavy stack's turns and trim, delivering a load, a refused load | 3 | 50 | ≤ 2.5 days |
| 7 | The January window | CAIRN OPS | Skua Yard, the Moon, Peary Depot, Highport | TLI, SOI change, capture, light time, life support, a delivery in lunar orbit, return | 4 | 25 + 1,353 out, 1,353 back | ~23 days, ≤ 12 away from stores out, ≤ 7 back |
| 8 | The big one | HIGHPORT | Skua Yard, Highport | the rated 100 t load | 3 | 50 | ≤ 2.5 days |
| 9 | BQ7 | CAIRN OPS | Highport, 325,000 km out, Skua Yard | an intercept far out, grappling a spinning rock, a loaded capture at perigee | 4 ("should") | 2,065 empty + 565 loaded | ~11 days, ~10 away from stores |

- Order of the lessons: quest 1 needs only `milestone 2` (manual flight); quests 2–6 and 8 need `milestone 3` (autopilot in Earth orbit, grapple, berthing, towing); 7 and 9 need `milestone 4` (the Moon, SOI changes, life support, the passing rock). Quest 8 is flyable from `milestone 3` but stands after the lunar trip to fill the weeks before BQ7 passes.
- The "Milestone" column names the milestone whose features make the quest flyable; the quests themselves, RADIO and JOB are `milestone 5`'s.

### Rules for every quest
How `quests.lisp` runs the line; per-quest sections only add what differs.
- Quest state (part of the simulation state, `docs/architecture.md` "Checkpoints"): the current quest and step, the step's start time, the acknowledged RADIO messages, the sent messages, skipped quests. Conditions are evaluated after each simulation step, after `hazards.lisp`, on the 1/64 s grid, so a run is deterministic at any warp level ("Determinism"). A completed step gives the next one at once; its RADIO message arrives after the light time from the sender (`docs/design.md` "Messages and radio").
- Step kinds: a JOB card (title and up to three lines; the COMMS grid is 26 characters wide, so every line below is at most 26, and the title with a space and the step id fits it too; "How the story is told") and a completion condition. A step marked **Y** counts as given only when its RADIO message is acknowledged with Y (or a click on COMMS); its condition is not evaluated before.
- Terms used in the conditions, each read from the simulation state:
  - **On station at F**: the grapple cone's tip (16.6 m from R along the ship's +x) within 150 m of fixture F's pin, and the ship's velocity relative to F's object at the pin at most 0.10 m/s (ship state; the object's promoted state or rails; the fixture from `places.lisp`). The autopilot's rendezvous stops at 100 m on the fixture's axis (`docs/design.md` "The autopilot"), inside it; a pilot flying by hand gets there too.
  - **Berthed at F**: the berth of the simulation state is F (`decision 33: Grapple tolerances, contact and berthing`).
  - **Grappled X**: the grapple state is latched and the load is object X.
  - **Settled in hold H**: controls' hold mode is H, the nose within 0.1° of H's direction and the angular rate under 0.02°/s (`docs/design.md` "Control modes").
  - **Ap, Pe**: apoapsis and periapsis altitude above the dominant body's sphere (R, not the crash radius) of the osculating orbit of the ship or stack (`orbits.lisp`), as the instruments show them.
  - **Removed X by C**: X is in the removed objects with cause C (`docs/architecture.md` "Removal of objects"): `:burned-up` (Earth's crash sphere) or `:delivered` (below).
  - **Delivered**: a load released with its CoM within 100 m of the receiving fixture's pin and moving at most 0.05 m/s relative to the receiving object, still within 150 m 600 s later, is taken in by the place's handling gear (told by RADIO): removed with cause `:delivered` at that moment (`docs/design.md` "Towing", the removal of `docs/architecture.md` "Removal of objects"). Receiving fixtures: `(:highport :plant)`, `(:peary-depot :berth)`.
  - **Propellant full**: at least 27,720 kg (99 % of 28 t) of propellant, berthed at a depot (`decision 26: Refuelling and economy`; 10 kg/s, 47 min for all 28 t).
  - **Time**: `t` against a UTC date, or the step's start time plus a duration.
- Proposed ids for `places.lisp` and `quests.lisp`: places `:tern-station`, `:fulmar-depot`, `:k-17`, `:highport`, `:skua-yard`, `:peary-depot`, `:bq7`; fixtures `(:tern-station :berth)`, `(:fulmar-depot :a)`, `(:highport :plant)`, `(:peary-depot :berth)`; loads `:k-17`, `:skua-yard-3`, `:skua-yard-5`, `:skua-yard-6`, `:pod-14`, `:bq7` (each with its one fixture).
- Checkpoints (`decision 35: Saving and audio`): at the start of every step, when the state is safe (coasting, no impact or contact predicted, hull below the warning; `docs/design.md` "Saving") and the ship's Δv remaining with its load is at least the step's **floor** below; a step whose start is unsafe keeps the previous checkpoint. The floor (`docs/design.md` "Saving") also vetoes the berthing and plan-complete checkpoints during the quest, so a checkpoint never holds a ship that cannot finish its job. Floors not named are 0.
- Death never fails a quest (`decision 34: How death works`): Return on the LOST panel restores the last checkpoint, with the quest state and every object as they were; the step is flown again.
- The stranded offer: if, outside a berth, the ship's Δv remaining with its load falls below the current step's floor, or the ship is outbound on an escape path (osculating e ≥ 1 around Earth past perigee, or the Sun as its dominant body), or the step's own object can no longer be reached (named per quest), then after 10 min of game time CAIRN OPS sends the offer (message S1 below); **Y** restores the last checkpoint exactly as the LOST panel's Return does. Until Y the pilot may fly on; the offer stays in the log. No tanker flies in view; it is told (`docs/design.md` "Dying and what follows").
- Running out of propellant does not kill (`docs/design.md` "Dying and what follows"); with the offer, no state of the line is a dead end.
- Messages: each quest has a table of its RADIO messages with an id, a trigger, the sender, **Y** if the message must be acknowledged, and the exact text, which `quests.lisp` copies. Triggers: "n.m given" (the step becomes current), "n.m done" (its condition met), a named condition, or a date. A message is sent once per quest line (the quest state's sent messages) unless its trigger says "each time"; messages with the same trigger are sent in table order; a restored checkpoint restores the sent messages too, so a step flown again sends its messages again.
- Messages by date, sent whatever quest is running (the D table under quest 9; the BQ7 ones only if the passing rock is built), and the stranded offer:

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| S1 | stranded for 10 min, once per checkpoint | CAIRN OPS | Y | Patience, Cairn Ops. You're not making it. TANKER 3 will fetch you, and we fly it again from the last good point. Y when ready. |

### 1. Shakedown
- Giver: CAIRN OPS. Goal: get the feel of PATIENCE: turn it by hand, hold an attitude, raise the apoapsis with a small burn and bring it back, warp one orbit. Place: the start orbit, 400 km circular (`decision 38: Start orbit: polar`).
- Skills: W/S, A/D, Q/E; rate damping (T) and fine control (F); holds prograde and retrograde (1, 2); throttle (Shift, X); the readout's Ap, Pe, attitude mode and warp; warp (. , slash); the map (M).
- Milestone: 2. Propellant: 2 × 2.83 m/s, from √(μ/r₁)(√(2r₂/(r₁ + r₂)) − 1) for 400 → 410 km and back (the retrograde burn at the same point undoes it), ~5.7 m/s, 45 kg, plus RCS for the turns: budget 10 m/s, 81 kg. Time: ~2 h. Expected state at start: the start ("The start"): 28,000 kg.
- Why the orbit is given back: Tern Station is 20° ahead at 420 km (`docs/design.md` "Earth-orbit points of interest"), and from 400 km PATIENCE gains 1.59° per orbit on it (n ∝ a^(−3/2): 1.5 × 20 / 6,788 × 360°), so the cheap Hohmann of quest 2 opens ~19 h after the start; leaving the ship at 400 km keeps that.

| Step | JOB card | Y | Completion |
|---|---|---|---|
| 1.1 | FIRST JOB / Read Cairn Ops: Y | Y | the RADIO acknowledged |
| 1.2 | TURN BY HAND / Nose to orbit normal / W S A D; T damps / No hold, within 10° | | the nose within 10° of the orbit normal (r × v), angular rate under 0.1°/s, no hold engaged |
| 1.3 | HOLD PROGRADE / Press 1, wait SETTLED | | settled in hold prograde |
| 1.4 | RAISE APOAPSIS / Shift once: 10 % / Cut (X) at AP 410 km | | Ap ≥ 408 km and Pe ≥ 395 km |
| 1.5 | AND BACK DOWN / Hold retrograde: 2 / Burn to AP under 405 km | | Pe ≥ 395 km and Ap ≤ 405 km |
| 1.6 | ONE ORBIT / Warp up with . / Watch the night side | | t ≥ step start + 5,556 s (one period, 92.6 min) |

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| 1a | t = 10 s (1.1 given at t = 0) | CAIRN OPS | Y | Patience, Cairn Ops. Morning. Mirela left her full, so don't empty her. Turn her by hand first, then a small burn. Y to copy. |
| 1b | 1.4 done | CAIRN OPS | | AP 410. That's a burn. Now take it back. |
| 1c | 1.6 done | CAIRN OPS | | Tern has a berth for you. Ana Folau runs it. Do what Tern says. |

- Checkpoints: the start state, then each step's start.
- Failure: an overshoot is fixed by the next burn; only a retrograde burn of more than 87 m/s (`docs/design.md` "Δv for the standard legs", deorbit to 100 km) can kill, and death restarts the step.

### 2. Home port
- Giver: TERN (the job assigned by CAIRN OPS at 1.6). Goal: rendezvous with Tern Station and berth. Place: Tern Station, 420 km, u₀ 20°.
- Skills: targets (] [), the autopilot's "Rendezvous: Tern Station" (O, Up/Down, Return to plan, Return to fly), the AUTOPILOT screen's plan and the Burn instrument; warp through the coast and its drop-outs; the TARGET screen; target and relative holds (7, 0); grapple armed (G) with the grapple camera; fine translation (F, H/N, I/K, J/L); berthing; an R-bar approach to the zenith fixture.
- Milestone: 3. Propellant: 11 m/s (+ phasing) from the start (`docs/design.md` "Earth-orbit points of interest": Hohmann 400 → 420 km, 5.65 + 5.64 m/s), phasing by waiting ~19 h inside the autopilot's 48 h search; terminal phase and braking budget 20 m/s: 31 m/s, 254 kg. Time: ≤ 1 day. Expected state at start: 27,919 kg, 400 km circular. A late start (Tern Station behind) makes the plan dearer; the tanks hold 6.6 km/s and quest 4 refuels.

| Step | JOB card | Y | Completion |
|---|---|---|---|
| 2.1 | TARGET TERN / ] until TERN STATION | | the target is `:tern-station` |
| 2.2 | RENDEZVOUS / O: Rendezvous: Tern / Return plans, Return flies / Warp the coast | | on station at `(:tern-station :berth)` |
| 2.3 | CLEARED TO BERTH / Arm the grapple: G / Close at 0.1 m/s, F on / Zenith face, from above | Y | berthed at `(:tern-station :berth)` |

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| 2a | 2.1 given | TERN | | Patience, Tern. Berth is the zenith fixture, aft module. Autopilot to 100 m, the rest is yours. |
| 2b | 2.3 given | TERN | Y | Patience, Tern. Cleared to approach from the 100 m hold. 0.1 m/s or less. Y to copy. |
| 2c | 2.3 done | TERN | | Patience, Tern. Berthed. Welcome to the plane. |
| 2d | 2.3 done | MIRELA | | Heard you got her. Close at 0.1. She doesn't mind. -M |

- Checkpoints: each step's start; berthing.
- Failure: a contact above 1.0 m/s kills, and 2.3 restarts on station at 100 m; a missed capture bounces (`decision 33`) and the pilot tries again.

### 3. Seventeen
- Giver: LINDGREN (Sami Lindgren, Kiruna; CAIRN OPS lends PATIENCE). Goal: grapple Derelict K-17, push it into an orbit that meets Earth's crash sphere, release it, save the tug, see it burn up (`docs/design.md` "The derelict's re-entry"). Place: K-17, 800 km, u₀ 270°, spinning 0.3°/s about its axis.
- Skills: a rendezvous to a towable fixture on a turning object; grappling a load; a stack's mass and turns; a burn below the autopilot's 150 km floor by hand (it refuses, `decision 32: Autopilot scope and safety rules`); the PERI lamp and the impact marker; release (G); a fast recovery burn; the SHIP message "K-17 BURNED UP".
- Milestone: 3. Propellant from Tern Station's 420 km: Hohmann 420 → 800 km 103.4 + 102.0 = 205 m/s (√(μ/r) and vis-viva as "Δv for the standard legs"; the design's 217 is from 400 km), 208 m/s with a transfer that passes K-17 clear (`docs/design.md` "The terminal path and crowded targets", `decision 59: Rendezvous transfers pass the target clear`), and terminal 20 m/s, 1,798 kg; the push 196 m/s with the 36 t stack, 1,580 kg (`docs/design.md`: 1.6 t); the tug's perigee raised back to 400 km and circularised, 89 + 109 m/s, 1,417 kg. Total 622 m/s, 4,795 kg. Time: ≤ 2 days (K-17 gains 28° per orbit on a ship at 420 km, so the autopilot's Hohmann opens within 13 orbits); the push to the burn-up, 1 h. Expected state at start: 27,665 kg, berthed at Tern.

| Step | JOB card | Y | Completion |
|---|---|---|---|
| 3.1 | DERELICT K-17 / G to unberth / Rendezvous: Derelict K-17 / 800 km, slowly spinning | | on station at K-17's fixture |
| 3.2 | GRAPPLE IT / Fixture on the nose ring / It turns. The pin won't. | | grappled `:k-17` |
| 3.3 | PUSH IT DOWN / Hold retrograde: 2 / Burn to PE 90 km / Autopilot won't. By hand. | | grappled `:k-17` and Pe < 100 km |
| 3.4 | LET GO / Release: G, back off | | not grappled, and K-17's Pe < 100 km |
| 3.5 | SAVE YOURSELF / Hold prograde: 1 / Raise PE above 150 km / 43 min to the air | | the ship's Pe ≥ 150 km |
| 3.6 | WATCH IT GO / K-17 burns up in 43 min / Then 400 km circular | | removed `:k-17` by `:burned-up` |

- If K-17 is released with its Pe at or above 100 km (steps 3.3–3.5), the quest returns to 3.2 with message 3e. K-17 drifts at the release speed, metres per second at most, and stays a target; a rendezvous by autopilot works while its Pe is ≥ 150 km, below that the pilot closes by hand.

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| 3a | 3.1 given | CAIRN OPS | | Lindgren Debris paid for a push nobody had time to fly. You're lent to them. Sami will brief you. |
| 3b | 3.1 given | LINDGREN | | Patience, Lindgren Debris. Seventeen is yours. Fixture is new, the stage is not. Perigee 90, release at apogee, and please don't follow it. |
| 3c | 3.3 given | CAIRN OPS | | The autopilot won't go under 150. Your hands, your PERI lamp. |
| 3d | 3.4 done | LINDGREN | | Released! Now you, Patience. Up. |
| 3e | K-17 released with Pe ≥ 100 km, each time | LINDGREN | | It's still up there. Please grab it again. |
| 3f | 3.6 done | LINDGREN | | It's gone. Seventeen of forty. Thank you. Really. |

- Checkpoints: 3.1 (berthed), 3.2 (on station), 3.3 (latched, 800 km circular). 3.4 and 3.5 begin with an impact predicted, so death there restarts at 3.3 with K-17 latched. 3.6 begins safe: a checkpoint with K-17 on its rails to its burn-up, which a restore replays bit for bit (`docs/architecture.md` "Removal of objects").
- Floor: 3.3 and later: 300 m/s with the load (the push's 196 m/s and margin; ~4,000 m/s on hand).

### 4. Topping off
- Giver: CAIRN OPS (TERN controls Fulmar Depot's berths). Goal: berth at Fulmar Depot's free fixture A and fill the tanks. Place: Fulmar Depot, 500 km, u₀ 180°; TANKER 2 berthed at fixture B.
- Skills: a rendezvous chosen and flown without help; berthing beside a berthed tanker (fixture A faces +z, the tanker's B −z); propellant transfer (B), the XFER line and the PROP gauge; warp while berthed.
- Milestone: 3. Propellant: 56 m/s (`docs/design.md` "Earth-orbit points of interest", 400 → 500 km), phasing ≤ 50 m/s (the ship gains 7.8° per orbit on Fulmar from 400 km, a full turn in 2.96 days; inside 48 h by waiting, else a drift orbit at ≤ 33 m/s and ≤ 1 m/s for a final transfer that passes the depot clear, `docs/design.md` "Rendezvous: search, phasing and the approach"), terminal 20: 126 m/s, 869 kg. Time: ≤ 2 days and 47 min of transfer. Expected state at start: 22,870 kg, 400 km circular.

| Step | JOB card | Y | Completion |
|---|---|---|---|
| 4.1 | FULMAR DEPOT / Rendezvous: Fulmar Depot / Fixture A, the free arm | | on station at `(:fulmar-depot :a)` |
| 4.2 | BERTH AT A / Mind TANKER 2 at B | Y | berthed at `(:fulmar-depot :a)` |
| 4.3 | FILL HER UP / Transfer on: B / 10 kg/s; warp is fine | | propellant full |

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| 4a | 4.1 given | CAIRN OPS | | Fill up at Fulmar. Boreal's tanks, our water. Then you go up. |
| 4b | 4.2 given | TERN | Y | Patience, Tern. Fulmar A is yours. TANKER 2 is on B, keep off it. Y to copy. |
| 4c | 4.3 done | CAIRN OPS | | Full? Good. Highport next. Plan it, check it, fly it. |

- Checkpoints: each step's start; berthing. Failure: a hard contact with the tanker kills (restart at 4.2 on station).

### 5. The long climb
- Giver: CAIRN OPS. Goal: climb from the low plane to Highport (1,000 × 70,000 km), berth at the water plant's fixture, fill up. Place: Highport; THRIFT and TANKER 3 parked 5 and 2 km ahead of it (`decision 51: Story ships flying free: THRIFT, TANKER 3, BAGGER 3`), STUBBORN and THISTLE on its arms.
- Skills: a multi-burn plan (perigee burn, apogee burn, rendezvous), reading the Burn instrument (total Δv, propellant left, arrival); a 12 h coast at ×10,000 and warp drop-outs before burns; an eccentric orbit's Ap and Pe and their times; the approach in the plant's shadow under the floodlight.
- Milestone: 3. Propellant: from Fulmar's 500 km circular to Highport's orbit, 2,699 m/s at 500 km (apogee to 70,000 km, vis-viva) + 30 m/s at apogee (perigee to 1,000 km) = 2,729 m/s (the design's 2,762 is from 400 km), transfer 11.7 h; phasing ≤ 50, terminal 20: 2,799 m/s, 16,899 kg; 11,101 kg left (`docs/design.md` "Δv checked against the ship": 11,265 kg from 400 km). Time: ≤ 3 days. Expected state at start: full (28,000 kg), berthed at Fulmar A.

| Step | JOB card | Y | Completion |
|---|---|---|---|
| 5.1 | UP TO HIGHPORT / Rendezvous: Highport / Two burns, 12 h coast / Read the plan first | | on station at `(:highport :plant)` |
| 5.2 | BERTH AT THE PLANT / Plant face, in its shadow / Floodlight on with G | | berthed at `(:highport :plant)` |
| 5.3 | FILL HER UP / Transfer on: B | | propellant full |

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| 5a | 5.1 given | CAIRN OPS | | Highport, 1,000 by 70,000. 2.8 km/s from Fulmar, you have 6.6. The autopilot does it in three burns. |
| 5b | the ship's Ap first above 60,000 km during 5.1 | THRIFT | | Patience, THRIFT. Dag. Saw you go up. Mirela always waved at Fulmar. Welcome to the high end. |
| 5c | 5.2 given | HIGHPORT | | Patience, Highport. Esther here. Plant fixture is free. Mind the radiator fins. |
| 5d | 5.3 done | CAIRN OPS | | Rest. Esther has rocks for you. |

- Checkpoints: each step's start; a completed plan; berthing. Floor: 5.1: 2,900 m/s (the leg and its margin); with a full tank at the start it is always met.

### 6. Rock three
- Giver: HIGHPORT (Esther Okafor); GRINDSTONE (Ruth Abara) gives the tips. Goal: bring Skua Yard 3 (50 t) from the yard to the plant and deliver it. Place: Skua Yard (30 km behind Highport, a string of rocks 667 m apart at apogee, GRINDSTONE 2 km behind the last; `docs/design.md` "Earth-orbit points of interest"), Highport's plant.
- Skills: towing a 50 t load (`docs/design.md` "Towing"): turns at 0.36°/s² (32 s for 90°), 14 s to the 5°/s rate, translation at 0.013 m/s²; the autopilot planning with the stack's mass; stopping a heavy stack; delivering (release at standstill); the refused load (Skua Yard 6, 150 t: "LOAD 150 T OVER 100 T RATING"); Mirela's second tape note.
- Milestone: 3. Propellant: Highport → Skua Yard < 5 m/s (`docs/design.md` "Earth-orbit points of interest") + terminal 20, 203 kg; back with 50 t < 5 + 20, 485 kg: 50 m/s, 688 kg. Time: ≤ 2.5 days (one 23.7 h phasing orbit each way). Expected state at start: full, berthed at the plant.

| Step | JOB card | Y | Completion |
|---|---|---|---|
| 6.1 | SKUA YARD / Target SKUA YARD 3, 50 t / Rendezvous, 30 km back | | on station at Skua Yard 3's fixture |
| 6.2 | GRAPPLE THREE / Not six: 150 t, refused | | grappled `:skua-yard-3` |
| 6.3 | TOW IT IN / Rendezvous: Highport / Loaded: start turns early / Stop 100 m off the plant | | grappled `:skua-yard-3`, its CoM within 100 m of the plant's pin, at most 0.05 m/s relative to Highport |
| 6.4 | LET IT GO / Release: G, back off 50 m / The plant takes it | | removed `:skua-yard-3` by `:delivered` |
| 6.5 | BERTH AND FILL / Plant fixture, then B | | berthed at `(:highport :plant)` and propellant full |

- If Skua Yard 3 is released without being delivered (outside 100 m, faster than 0.05 m/s, or drifting out of 150 m within the 600 s), the quest returns to 6.2 with message 6d. On rails it stays a target the autopilot can reach.
- A grapple attempt on Skua Yard 6 is a missed capture (`docs/design.md` "Grappling and berthing"); GRINDSTONE then sends 6c.

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| 6a | 6.1 given | HIGHPORT | | Patience, Highport. Three, please. Fifty tonnes, about twenty percent water. Bring it to the plant face and stop it there. |
| 6b | 6.1 given | GRINDSTONE | | Loaded, she turns like a barge. |
| 6c | a missed capture on Skua Yard 6 | GRINDSTONE | | Six is mine. 150 tonnes. Your claws won't. |
| 6d | Skua Yard 3 released and not delivered, each time | GRINDSTONE | | It's walking. Go get it. |
| 6e | 6.4 done | HIGHPORT | | We have it. Fifty point two tonnes on the arm. Thank you, Patience. |

- Checkpoints: each step's start; 6.3's with the rock latched. Floor: 6.3: 100 m/s with 50 t.

### 7. The January window
- Giver: CAIRN OPS; PEARY (Tomasz Wierzbicki) at the Moon. Goal: carry POD 14 (a standard 12 t cargo pod of electrolyser spares for Peary Camp) to Peary Depot in the January window, deliver it, fill up and come back to Highport. Place: Skua Yard (where POD 14 waits), the Moon, Peary Depot (100 km polar lunar orbit), Highport.
- Skills: the TLI preset (departure searched over 30 days), the SOI change and its drop-out, the Moon as the dominant body, lunar capture, light time (1.3 s to Highport), the LIFE line and lamp, a delivery in lunar orbit, refuelling at Peary, the return to an Earth periapsis of 1,000 km and capture into Highport's orbit.
- Milestone: 4. Propellant: to POD 14 25 m/s empty, 203 kg. Out with 12 t: TLI from Highport's perigee 376 m/s and LOI into 100 km 897 m/s with a 4.9° plane change (`docs/design.md` "The Moon": 1,273 m/s in the best window; v∞ 0.99 km/s, the Moon crossing the polar plane), mid-course 10, phasing at the Moon 0 (merged into the LOI, `docs/design.md` "Rendezvous: search, phasing and the approach"; ≤ 50 kept as margin), terminal 20: 1,353 m/s, 12,618 kg with the pod (4,410 × ln(m₀/m₁), m₀ 47,758 kg); 15,179 kg left at Peary. Refuel full. Back: TEI and capture into Highport's orbit ~1,273 m/s (`docs/design.md` "The Moon"), mid-course 10, phasing 0 (merged into the capture; 50 kept as margin), terminal 20: 1,353 m/s, 9,501 kg; 18,499 kg left. Time: from Highport ~2040-01-10: POD 14 ≤ 1 day, then the coast to the TLI at 2040-01-20 06:01 (`docs/story.md` "Start date"), ~6 days out, ≤ 1 day at Peary, ~6 days back, ≤ 2 days to berth: at Highport ~2040-02-01. Away from stores: Highport → Peary ≤ 12 days with the pod (no berth with a load), Peary → Highport ≤ 7.
- Expected state at start: full, berthed at Highport's plant.

| Step | JOB card | Y | Completion |
|---|---|---|---|
| 7.1 | POD 14 / Grapple POD 14, 12 t / Spares for Peary Camp | | grappled `:pod-14` |
| 7.2 | TRANS-LUNAR / O: Rendezvous: Peary Depot / Leaves at a perigee / Five days out | | the dominant body is the Moon |
| 7.3 | PEARY DEPOT / Autopilot flies the LOI / Pod off 100 m out, still | | removed `:pod-14` by `:delivered` (at `(:peary-depot :berth)`) |
| 7.4 | BERTH AND FILL / Peary's one arm, then B | | berthed at `(:peary-depot :berth)` and propellant full |
| 7.5 | HOME / O: Rendezvous: Highport / TEI and capture, one plan / Then berth at the plant | | berthed at `(:highport :plant)` |

- The windows: the Moon stands at Highport's apogee direction on 2040-01-26 and 2040-02-22 (`docs/story.md` "Start date"); the TLI preset finds the next one. The return falls from the Moon at Highport's apogee direction to its perigee, so it leaves Peary within ~1 day of arrival; a pilot who misses it waits berthed at Peary for the next month (restocked there) or returns to 400 km (3,983 m/s, `docs/design.md` "The Moon") and climbs again via Fulmar. The autopilot's TLI aims at Peary Depot's plane (i and Ω) and takes the phasing into the LOI: "TLI: Peary Depot" and "Rendezvous: Peary Depot" plan the same chain (`docs/design.md` "Rendezvous: search, phasing and the approach").
| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| 7a | 7.1 given | CAIRN OPS | | POD 14 goes to Peary in the next window. Twelve tonnes of electrolyser spares: handle it like eggs. |
| 7b | 7.2 given | MIRELA | | First time out? Look back once. Then fly. |
| 7c | 7.2 done | PEARY | | Patience, Peary. Tomasz. You're one point three seconds from home now. Take your time, we have plenty. |
| 7d | 7.3 done | PEARY | | Pod's in. The camp says thank you, in four languages. |
| 7e | POD 14 released at Peary and not delivered, each time | PEARY | | Pod's drifting, Patience. Catch it before it walks off. |
| 7f | 7.4 given, or THRIFT's arrival at the Moon (2040-01-26 06:00 or later) if later, while the ship is in the Moon's sphere (else not sent) | THRIFT | | Patience, THRIFT. Parked half a turn ahead of Peary, so don't come looking. Nice flying. |
| 7g | 7.5 given | CAIRN OPS | | The window home opens within a day. Don't stay for the view. |
| 7h | 7.5 done | CAIRN OPS | | Welcome back. Esther wants the big one next. |

- Checkpoints: each step's start; 7.2's with POD 14 latched, 7.3's inside the Moon's sphere before the capture.
- Floors: 7.2: 1,400 m/s with 12 t; 7.3: 1,000 m/s with 12 t; 7.5: 1,400 m/s.
- Failure: a missed lunar capture leaves the Moon on a flyby: the stranded offer if it ends on an escape path or below the floor; a missed delivery (POD 14 released outside the tolerance) returns to 7.1 with 7e, as in quest 6; 7.2 is then met at once (the ship is in the Moon's sphere) and 7.3 follows.

### 8. The big one
- Giver: HIGHPORT. Goal: Skua Yard 5 (100 t, the grapple's rating) to the plant. Place: Skua Yard, Highport. Fills the weeks between the lunar trip and BQ7.
- Skills: the rated load: 1.0 km/s of Δv, 0.08 g₀, a 90° turn in over 40 s (`docs/design.md` "Grapple and towing"); the same procedure as quest 6, slower.
- Milestone: 3 (placed after quest 7). Propellant: < 5 + 20 m/s empty, 203 kg; < 5 + 20 m/s with 100 t, 767 kg: 50 m/s, 970 kg; Δv on hand with 100 t after the fetch 1,010 m/s. Time: ≤ 2.5 days. Expected state at start: full, berthed at the plant, ~2040-02-01.
- Steps as quest 6's with Skua Yard 5 (`:skua-yard-5`) and the same conditions; the same recovery (back to 8.2 with 8c).

| Step | JOB card | Y | Completion |
|---|---|---|---|
| 8.1 | SKUA YARD / Target SKUA YARD 5, 100 t / Rendezvous, 30 km back | | on station at Skua Yard 5's fixture |
| 8.2 | GRAPPLE FIVE / Rated load. No more. | | grappled `:skua-yard-5` |
| 8.3 | TOW IT IN / Rendezvous: Highport / 100 t: turn 40 s early / Stop 100 m off the plant | | grappled `:skua-yard-5`, its CoM within 100 m of the plant's pin, at most 0.05 m/s relative to Highport |
| 8.4 | LET IT GO / Release: G, back off 50 m / The plant takes it | | removed `:skua-yard-5` by `:delivered` |
| 8.5 | BERTH AND FILL / Plant fixture, then B | | berthed at `(:highport :plant)` and propellant full |

- Skipped if 8.1 would be given after 2040-03-02 12:00 (a late return from the February window), so BQ7 is not missed: 8f is sent and 9.1 is given.
- Floor: 8.3: 100 m/s with 100 t. Checkpoints as quest 6.

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| 8a | 8.1 given | HIGHPORT | | Five, please. A hundred tonnes, the most your claws take. |
| 8b | 8.1 given | GRINDSTONE | | Start early. Then earlier. |
| 8c | Skua Yard 5 released and not delivered, each time | GRINDSTONE | | It's walking. Go get it. |
| 8d | 8.4 done | HIGHPORT | | One hundred point four tonnes. Nineteen percent water. |
| 8e | 8.5 done | THRIFT | | Patience, THRIFT. Heard you brought Five in. Mirela's tape by the hull gauge is meant: inside 0.55 the paint cooks. Nothing in there to fetch. |
| 8f | quest 8 skipped | CAIRN OPS | | Five can wait. BQ7 can't. |

### 9. BQ7
- Giver: CAIRN OPS; BAGGER 3 is told, never heard; TERN objects; HIGHPORT assays; the ending. Goal: meet 2040 BQ7 ~325,000 km out, 1.5 days before its perigee, grapple it, fall with it and capture the stack into Highport's orbit at perigee (2040-03-12 12:00 UTC, `decision 50: Story: start date and the passing rock's date`), leave it in Skua Yard. Place: Highport, BQ7's inbound hyperbola (v∞ 0.75 km/s, perigee 7,378 km), Skua Yard.
- Skills: an intercept far out (a TLI-like burn, then matching 1.6 km/s mostly radial), a rock spinning 0.4°/s about its fixture axis, BAGGER 3 1 km beyond it, a loaded capture burn at perigee timed to the minute, the stack in a 23.7 h orbit.
- Milestone: 4, "should" (the passing rock, `docs/design.md` "Asteroids"). Propellant: empty, the intercept leaving Highport's orbit after the 2040-03-05 15:56 perigee and matching the rock's velocity near 325,000 km at v∞ 0.75 km/s: 606 + 1,405 = 2,010 m/s (`docs/design.md` "Rendezvous: search, phasing and the approach"; 1,683 leaving 03-03, 2,289 leaving 03-06), mid-course 10, terminal 20: 2,040 m/s, 13,318 kg; 14,682 kg left, 996 m/s with 50 t. Loaded: capture at perigee √(v∞² + 2μ/q) − 9,926 = 495 m/s (design 480–516), split around one 25.5 h phasing revolution so the stack reaches Skua Yard's drop point (phasing 0; ≤ 50 kept as margin), terminal 20: 565 m/s, 8,735 kg. Empty back to Highport 25 m/s, 79 kg. 5,868 kg left at the end. Time: berthed from ~2040-02-04 to 2040-03-03; depart ~2040-03-05 (5.3 days out), meet ~2040-03-10 23:00, perigee 2040-03-12 12:00, the yard 2040-03-13 ~13:30, Highport ~2040-03-14: ~10 days away from stores.
- Expected state at start: full, berthed at the plant.

| Step | JOB card | Y | Completion |
|---|---|---|---|
| 9.1 | STAND BY / BQ7 perigee 12 Mar 12:00 / Stay berthed, keep full | | t ≥ 2040-03-03 00:00 UTC, berthed at `(:highport :plant)`, propellant full |
| 9.2 | INTERCEPT / O: Rendezvous: 2040 BQ7 / Leave by 6 Mar / Meet it 325,000 km out | Y | on station at BQ7's fixture |
| 9.3 | GRAPPLE BQ7 / 50 t, turning 0.4°/s / BAGGER 3 is 1 km past it | | grappled `:bq7` |
| 9.4 | CAPTURE / O: Rendezvous: Skua Yard / Burn at perigee, 12 Mar / It captures and phases | | grappled `:bq7`, around Earth with e < 1, Ap ≤ 80,000 km, Pe ≥ 150 km |
| 9.5 | INTO THE YARD / Stops at Three's old slot / Release it still: G | | not grappled, BQ7's CoM within 1 km of Skua Yard's drop point (Skua Yard 3's slot, `docs/design.md` "Rendezvous: search, phasing and the approach"), at most 0.05 m/s relative to it |
| 9.6 | HOME / O: Rendezvous: Highport / Then berth at the plant | | berthed at `(:highport :plant)` |
| 9.7 | THE LONG HAUL / Cairn builds a long tug. / Two years of stores. / The seat is yours. | | none: the prelude is over, free flight goes on |

- The autopilot's intercept of a target on a hyperbola: departures anywhere on the ship's orbit, transfers of 2–10 days, the meeting ≥ 36 h before perigee (`docs/design.md` "Rendezvous: search, phasing and the approach"). The rock stays in the yard on rails (no removal: it is the yard's newest rock).
- The step's object out of reach: BQ7 past its perigee and not grappled (steps 9.2–9.3), or released outside Earth orbit, brings the stranded offer.
- Skipped if 9.2 would be given after 2040-03-06 00:00 UTC: 9g is sent, then the ending (9h, 9i) and 9.7's job card at once.
- Messages by date (D1 and D4–D7 only if the passing rock is built; D2 and D3 always, THRIFT's from its scheduled position, "Other ships"):

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| D1 | 2040-01-19 12:00 | CAIRN OPS | | Survey found a small one. 2040 BQ7, about fifty tonnes, perigee 12 March, right in the plane. We want it. |
| D2 | THRIFT leaves Highport's orbit (2040-01-21 06:00 or later, "Other ships") | THRIFT | | Patience, THRIFT. Off to the Moon for the winter. Peary needs a tug more than we do. |
| D3 | TANKER 3 leaves Highport's orbit (2040-01-21 06:00 or later) | CAIRN OPS | | TANKER 3 goes down to Fulmar for the winter fill. |
| D4 | 2040-02-10 09:00 | CAIRN OPS | | BAGGER 3 is off to BQ7. |
| D5 | 2040-03-01 09:00 | CAIRN OPS | | BAGGER 3 has it netted. It's yours, Patience. |
| D6 | 2040-03-03 00:00 | TERN | | Cairn Ops, Tern. A fifty-tonne rock a thousand km over my crew. I want the capture plan on my desk. |
| D7 | 2040-03-03 00:00, after D6 | CAIRN OPS | | Tern, Cairn Ops. You'll have it. Perigee is 1,000 km, Tern is at 420, and Patience flies by the book. |

- Quest messages:

| Id | Trigger | Sender | Y | Text |
|---|---|---|---|---|
| 9a | 9.1 given | CAIRN OPS | | Stay berthed, stay full. BQ7 comes to us. |
| 9b | 9.2 given | CAIRN OPS | Y | Go. Leave at a perigee, by the sixth at the latest. Y to copy. |
| 9c | 9.3 given | GRINDSTONE | | Spinning rock: watch the pin, not the rock. |
| 9d | 9.4 done | TERN | | Tern copies, captured. Thank you, Patience. |
| 9e | 9.5 done | HIGHPORT | | Twenty-one percent water. The wettest thing we've ever had. |
| 9f | BQ7 out of reach (the stranded offer follows) | GRINDSTONE | | It's past perigee. Nobody catches it now. |
| 9g | quest 9 skipped | CAIRN OPS | | BQ7's gone by. There'll be others. |
| 9h | 9.6 done, or after 9g | CAIRN OPS | | Patience, Cairn Ops. The board met. Cairn builds a long-haul tug: a Mule cab, two years of stores, twice the tanks. Farther out than anyone has pushed. We want you in that seat. |
| 9i | after 9h | MIRELA | | She's yours now. If they'd let me fly again, I'd take that seat. Since they won't: take it. -M |

- The ending is 9h, 9i and then 9.7's job card.
- Checkpoints: 9.1 (berthed), 9.2 (berthed, 2040-03-03), 9.3 (on station at BQ7), 9.4 (latched, falling), 9.5 (captured), 9.6.
- Floors: 9.3 and 9.4: 650 m/s with 50 t (the capture and the yard, 565, and margin); 9.5: 100 m/s with 50 t.

### Running checks
Propellant and stores carried from quest to quest, on the nominal path (Δv budgets above; refuelling only berthed at Fulmar Depot, Highport, Peary Depot; restocking at Tern Station, Fulmar Depot, Highport, Peary Depot, `docs/design.md` "Refuelling and resupply"). The ship's limits: 6,650 m/s empty, 1,738 m/s with 50 t, 1,017 m/s with 100 t, 180 person-days (`docs/design.md` "Mass budget and Δv", "Parts").

| Quest | Propellant at start (kg) | Used (kg) | At end (kg) | Refuel | Largest need vs on hand (m/s) | Longest away from stores |
|---|---|---|---|---|---|---|
| 1 | 28,000 | 81 | 27,919 | — | 10 vs 6,650 | 2 h |
| 2 | 27,919 | 254 | 27,665 | — (Tern restocks) | 31 vs 6,640 | 1 day |
| 3 | 27,665 | 4,795 | 22,870 | — | push 196 with 2.5 t vs 5,490 | 2 days |
| 4 | 22,870 | 869 | 22,001 → 28,000 | Fulmar Depot | 126 vs 5,971 | 2 days |
| 5 | 28,000 | 16,899 | 11,101 → 28,000 | Highport | 2,799 vs 6,650 | 3 days |
| 6 | 28,000 | 688 | 27,312 → 28,000 | Highport | 25 with 50 t vs 1,738 | 2.5 days |
| 7 | 28,000 | 12,821 out; 9,501 back | 15,179 → 28,000 at Peary; 18,499 → 28,000 | Peary Depot, Highport | 1,353 with 12 t vs 3,866 | 12 days |
| 8 | 28,000 | 970 | 27,029 → 28,000 | Highport | 25 with 100 t vs 1,010 | 2.5 days |
| 9 | 28,000 | 22,132 | 5,868 | (Highport after the end) | 565 with 50 t vs 996 | 10 days |

- Every leg fits with margin; the tightest is quest 9's loaded part, 565 of 996 m/s (911 leaving on the last day, 2040-03-06).
- Time: quests 1–6 take at most ~11 days by the bounds above (to ~2040-01-12), before the January TLI at 2040-01-20 06:01; a slower pilot takes the February window (TLI ~2040-02-17, back ~2040-03-01), skips quest 8 and still leaves for BQ7 by 2040-03-05.
- Stores: no leg is away from a restock for more than 12 days of 180; life support never decides a quest, it teaches the LIFE line.

### The Sun
- Not a quest. The warning zone (0.551 AU) is 4,143 m/s from LEO and 1,473 m/s from Highport's perigee, 125 days to perihelion (`docs/design.md` "The Sun"); the way back to 1 AU takes as long again, ~250 days against 180 person-days, and no depot waits there. A close call with the Sun does not fit, so the Sun stays a danger told about: the third tape note and THRIFT's message 8e.
- A pilot who flies there by hand meets the hull-temperature rule and the stranded offer (escape path).

### If a "should" is dropped
- The passing rock (`milestone 4` "should"): quest 9 is replaced by its ending alone. After quest 8's 8.5 (and 8e), the ending follows at once: 9h, 9i and 9.7's job card; Skua Yard 5's assay is 8d. The date messages about BQ7 (D1, D4–D7) are not sent. The line is complete with eight quests.
- Story ships moving (`milestone 5` "should"): no quest depends on them. THRIFT and TANKER 3 are then not drawn ("Other ships"); THRIFT's messages are sent from its scheduled position for the light time (Highport's until 2040-01-26 06:00, then Peary Depot's); TANKER 3 in the stranded offer was only ever told.
- Night lights, clouds, the sunlit patch, the moving ships, sound: none is a completion condition.

### Follow-ups
- `task 55: Design: deliveries, the stranded offer, checkpoint floors and POD 14 for the quest line` (`plan 10: game design`): delivered loads removed with cause `:delivered`; the stranded offer restoring the last checkpoint on Y; quest floors vetoing checkpoints; POD 14 placed in Skua Yard. Done.
- `task 56: Design: autopilot rendezvous for the quest line: phasing, Peary Depot's plane, a hyperbolic target` (`plan 10`): phasing orbits for co-altitude targets; TLI aimed at Peary Depot's plane and phase; transfer times for a target on a hyperbola. Done: `docs/design.md` "Rendezvous: search, phasing and the approach"; it moved BQ7's meeting to ~1.5 days and ~325,000 km and raised the lunar legs to 1,273 m/s (`decision 55: Meeting 2040 BQ7 and the lunar arrival from the polar plane`).
- `task 57: Architecture: flight-script actions for the quest play-through` (`plan 10`): the actions `milestone 5`'s headless play-through needs (target, grapple, approach to a fixture, transfer, release, acknowledge) and one `flights/quest-<n>.sexp` per quest. Done: `docs/architecture.md` "Scripted runs", "The quest play-through" (`decision 56: Flight-script actions, start states and the quest play-through`).
- `task 58: Design: parking slots and moves of the story ships THRIFT and TANKER 3` (`plan 11: story`): the slots' elements at Peary Depot and Fulmar Depot with their clearances, and the rails for a ship in transit with the 200 km deferral (`decision 57: Story: how it is told, its texts and the story ships' schedule`). Done: `docs/design.md` "Earth-orbit points of interest", "The Moon", the plan check of "The terminal path and crowded targets"; `docs/architecture.md` "Story ships' schedules" (`decision 58: Story ships' slots at Fulmar Depot and Peary Depot, schedules and clear plans`); TANKER 3 leaves Fulmar 2040-03-06 06:00.

## How the story is told

The story is told inside the cockpit and nowhere else: people speak by RADIO, the current order stands on the job card, and the cab itself carries a few fixed words. There are no cutscenes, voices, faces or reply choices (`decision 37: Message channels the story may use`); the only story text outside the cockpit is the introduction before the first flight. The channels are those of `docs/design.md` "Messages and radio", drawn on the COMMS screen of `docs/design.md` "Layout" (26 × 11 characters); this section fixes how the story uses them, and the quest tables above hold every text.

### Channels
- **RADIO**: everything people say: the job's reason, tips, news of the plane, the quarrel with Boreal, the ending. CAIRN OPS gives the jobs and the stranded offer; the others speak of their own places and work (the speaker table in "Characters"). Every message is in a table above (quests 1–9, the D table, S1); none is generated.
- **JOB**: what to do now, one card per quest step: a title in capitals and up to three lines of at most 26 characters, imperative, with the keys and preset names as the cockpit's tape labels and AUTOPILOT menu show them ("G", "O: Rendezvous: Highport") and the numbers to fly ("Burn to PE 90 km"). RADIO tells why, the card tells what; a card never holds news or a name the radio has not given first.
- **SHIP**: the flight computer's messages are the game's, not the story's (`docs/design.md` "Messages and radio"); the quest line only relies on some that the game sends anyway: "<LOAD> DELIVERED", "K-17 BURNED UP" (the removal message), "NO CAPTURE: …", "LOAD 150 T OVER 100 T RATING", "ENGINE LOCKED: BERTHED". The story never repeats a SHIP message by RADIO; the people answer it ("We have it.").
- **Static texts**: the plate above the window and Mirela's three tape notes ("Static texts"); the notes are echoed once by radio each (2d, 6b, 8e), so a pilot who never looks down still hears them.
- **The introduction** (`milestone 6`, "Introduction"): shown once before the first flight of a new game; the pilot's first COMMS message, 1a, follows 10 s into the flight.

### The radio's voice
- Terse working talk. The first message of a contact opens with the addressee and the sender's call sign ("Patience, Tern."); later messages in the same job drop it. Speakers sign off by doing, not by formula: no "over", no "out".
- Numbers in digits when they are values the pilot sets or reads off an instrument, with the instrument's units and abbreviations: "AP 410", "0.1 m/s", "1,000 by 70,000", "Perigee 90", "under 150". Numbers in words when they are talk: "fifty tonnes", "one point three seconds", "Twenty-one percent".
- The pilot never answers in text. A message that needs an answer ends "Y to copy" or "Y when ready", and Y (or a click on COMMS) is the pilot's whole reply; the steps marked **Y** wait for it.
- Length: at most 160 characters, six COMMS lines once wrapped, so a message fits the log without scrolling; the ending's 9h is the one exception (177).
- Each voice as "Characters" gives it: CAIRN OPS short and dry; TERN procedural; LINDGREN a few words too many; HIGHPORT in tonnes and percent; GRINDSTONE one line, tips that are not called tips; THRIFT friendly; PEARY slow and warm; MIRELA rare, signed "-M".
- No message mentions a feature the game does not have: no money, no weapons, no landing, no reply, no docking; the tug "berths" and "grapples".

### The COMMS screen
The 26 × 11 grid of `docs/design.md` "Layout", in Monaspace Neon with the colours of "Readability at 1280 × 800"; all text is typed out at 60 characters per second with a block cursor, one entry after another.

| Row | Content |
|---|---|
| 1 | the job card's title, left, and the step id, right ("BERTH AND FILL         5.3"), inverted: near-black on paper white |
| 2–4 | the card's lines, paper white; empty lines stay empty |
| 5 | a rule of `─`; while the current step waits for a Y, "Y TO COPY" in amber, centred in it, flashing at 1 Hz |
| 6–11 | the log, newest at the bottom, scrolled to the bottom whenever an entry arrives; the wheel scrolls back through the last 200 lines |

- A RADIO entry: a header line, "HH:MM SENDER" (the arrival time, UTC) in the sender's colour (cyan for Cairn Water's, white for others, "Characters"), then "+1.3s" when the light time is 0.5 s or more, then "[Y]" in amber while a Y message is unacknowledged; then the text, word-wrapped at 26 columns, paper white.
- A SHIP entry: one line or more, "HH:MM " and the text, amber.
- A new UTC day starts with a grey line "── 2040-01-21 ──" before its first entry.
- The card changes the moment a step is given; the steps follow each other without a gap, so between quests the next quest's first card replaces the last one's at once, and its RADIO arrives after the light time. The waits of the line are steps with their own card (9.1 STAND BY).
- No quest running (`milestone 3` and `4`, or a game before `milestone 5`'s line is built): the title "NO JOB", the lines empty, the log with SHIP messages only. After the ending, 9.7's card THE LONG HAUL stays for good; free flight goes on with SHIP messages.
- After a checkpoint is restored or a save loaded, the log is rebuilt from the quest state's sent messages (sender, arrival time, text) up to that moment; SHIP entries before it are not kept. The MSG lamp then flashes for any message not yet acknowledged.
- The outside and map views show the job as one overlay line, "JOB 5.3 BERTH AND FILL", and the newest RADIO's first wrapped line after "SENDER: ", cut with "…" at 60 characters.

### Light time
- A RADIO leaves its sender's position at its trigger and arrives at the ship after the distance between the two at that moment divided by c, rounded up to the 1/64 s grid, so runs stay deterministic (`docs/architecture.md` "Determinism"). Positions: CAIRN OPS, HIGHPORT at Highport; TERN at Tern Station; GRINDSTONE at the rock mover; THRIFT at THRIFT's slot ("Other ships"); LINDGREN, MIRELA at Earth's centre, PEARY at the Moon's (the error under 0.02 s, "Radio speakers").
- What it amounts to: in Earth orbit up to 0.3 s from Highport (its apogee 76,378 km from Earth's centre), never shown; at the BQ7 meeting about 1.1 s; in lunar orbit 1.3 s from Earth and Highport, shown as "+1.3s" in the header. PEARY and THRIFT, at the Moon, then arrive at once and Cairn's messages late.
- The pilot's Y travels nowhere (there are no replies), so the delay "each way" is only told, by PEARY's 7c.

### Other ships at their work
- At the places, as scenery from `milestone 3` (part of their place's shapes; `decision 47: Rigid bodies of the places`): AURORA 11 at Tern Station's node, TANKER 2 on Fulmar Depot's fixture B (quest 4 berths beside it), STUBBORN and THISTLE on two of Highport's arms, GRINDSTONE with its 10 m rock behind Skua Yard. Their lights burn and strobes blink; they never move.
- Moving (`milestone 5` "should", "Story ships moving between places"; `decision 27: Other ships`): THRIFT and TANKER 3, as the "Other ships" table schedules them. A ship sits in a parking slot as an immovable object; a move is a departure and an arrival: at the departure it leaves its slot (no longer drawn, on the map or in contact tests), in transit it exists only in the story, and at the arrival it appears in its next slot. Each switch waits while the player's ship is within 200 km of the slot concerned, so nothing appears or vanishes in view; D2, D3 and 7f follow the actual switch. No ship is ever seen thrusting (cut, `docs/design.md` "Scope").
  - What the pilot sees of it: back from the Moon, the two slots ahead of Highport are empty; THRIFT is a marker half a turn ahead in Peary Depot's orbit; TANKER 3 waits 2 km ahead of Fulmar Depot, seen only by a pilot who goes back there, and is back ahead of Highport after the BQ7 trip.
- With the rock (`milestone 4` "should"): BAGGER 3 rides 1 km beyond 2040 BQ7 and flies on past perigee, out of view.
- Told only: AURORA crew rotations, the landers that bring ice to Peary Depot, GRINDSTONE's long fetches, BAGGER 3's launch (D4) and its netting (D5), TANKER 3 fetching a stranded pilot (S1).

## Names and terms

Every name the story fixes, for `places.lisp`, `quests.lisp`, `meshes.lisp` and the other documents. Place names are the working names of `decision 39: Places in scope`, kept unchanged. Call signs are written in capitals in RADIO headers and as words in text ("Patience, Cairn Ops.").

### Ship and companies
- PATIENCE: the player's Mule-class tug, registration M-07, call sign "Patience".
- Mule: the tug class (`decision 22`).
- Cairn Water: the pilot's employer; the water and propellant company.
- Boreal Orbital: owner of Tern Station, Fulmar Depot and the AURORA capsules.
- Lindgren Debris: the debris company of K-17 (`docs/design.md` "the debris company").

### People and call signs
- Mirela Doru (MIRELA): PATIENCE's previous pilot, Tromsø.
- Rafael Quiroga (CAIRN OPS): Cairn Water's dispatcher, Highport.
- Ana Folau (TERN): commander of Tern Station.
- Sami Lindgren (LINDGREN): owner of Lindgren Debris, Kiruna.
- Esther Okafor (HIGHPORT): chief of Highport's water plant.
- Ruth Abara (GRINDSTONE): pilot of the rock mover.
- Dag Eliassen (THRIFT): pilot of the Mule THRIFT.
- Tomasz Wierzbicki (PEARY): lead of Peary Camp and Peary Depot.

### Places (kept)
- Tern Station, Fulmar Depot, Derelict K-17 (short: K-17), Highport, Skua Yard, Peary Depot (`decision 39`, `docs/design.md` "The places").
- Peary Camp: Cairn's ice camp on the Moon near the north pole (Peary crater); never shown, a RADIO location only.
- Andøya, Kiruna, Tromsø: places on Earth, told only.
- The plane: pilots' word for the shared polar plane (i 90°, Ω 270°).

### Other ships
- STUBBORN (M-04), THISTLE (M-05), THRIFT (M-09): Cairn Mule tugs.
- TANKER 2, TANKER 3: Cairn's uncrewed tankers.
- AURORA 11: Boreal's crew capsule.
- GRINDSTONE: Cairn's rock mover.
- BAGGER 3: Cairn's bagger probe, with 2040 BQ7.
- POD 14: a standard 12 t cargo pod of electrolyser spares, parked in Skua Yard between Skua Yard 3 and 5 (`docs/design.md` "Earth-orbit points of interest"), carried to Peary Depot in quest 7.

### Quests
- 1 Shakedown, 2 Home port, 3 Seventeen, 4 Topping off, 5 The long climb, 6 Rock three, 7 The January window, 8 The big one, 9 BQ7; the final job card: THE LONG HAUL.

### Rocks and dates
- 2040 BQ7: the passing rock (found 2040-01-19, perigee 2040-03-12 12:00 UTC); pilots say "BQ7".
- Skua Yard 1–6: the yard's bagged rocks by number (`docs/design.md` "Rigid bodies: attitude, mass, shapes and fixtures").
- Start: 2040-01-01 00:00 UTC (`*start-date*`); best lunar window: Moon at Highport's apogee 2040-01-26 02:36 UTC, TLI from Highport's perigee of 2040-01-20 06:01 UTC.
