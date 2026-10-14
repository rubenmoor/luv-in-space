# Story

This document is the story of Luv in Space's prelude, written for the people and agents who implement it: the setting, the characters, the premise, the quest line and how it is told. `milestone 5: Story and quests: the playable prelude` builds it (`quests.lisp`, RADIO and JOB, static texts, saves, a headless play-through), and `milestone 6: Release: one command to play, a README for people` takes its introduction from it. It uses only what `docs/design.md` "Scope", "What the story may use" assigns to milestones 2 to 5, and the places, ships and figures of `docs/design.md`; it cites the pm decisions it relies on by id and subject. Its own lasting choices are `decision 48: Story: setting and premise`, `decision 49: Story: names, call signs and ships` and `decision 50: Story: start date and the passing rock's date`. "Names and terms" at the end is the one list of names that code and the other documents use.

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
- `*start-date*` stays the default: **2040-01-01 00:00 UTC** (`decision 14: Units and time epoch`; game time before 2050-01-01, `decision 18: Ephemerides`). The start orbit's phase (argument of latitude 0° at the start), and so its lighting, are unchanged (`decision 38`): sunlit, 25° from the Sun, β −10°; the January Blue Marble (`decision 40: Earth's look and drag`) stays the right month.
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

  - So the TLI from Highport's perigee falls near **2040-01-21** (game day 20) for arrival on 2040-01-26; the February window (TLI near 2040-02-17) is the fallback, 2.2° off, a few m/s of mid-course correction more. The autopilot's TLI search looks 30 days ahead (`docs/design.md` "The autopilot"), so it always finds one of them.
  - The time the earlier quests take, roughly (the quest line, `task 52: Story: the quest line, its steps, completion conditions and checks`, fixes it): Tern Station and Fulmar Depot 1–2 days, K-17 2 days, the transfer to Highport 1–2 days, a Skua Yard tow ~2 days (one 23.7 h phasing orbit): about 8 days, leaving 12 days of slack before the January TLI.
- The passing rock (`milestone 4` "should", `docs/design.md` "Asteroids"): **2040 BQ7** (Minor Planet Center style: B for a discovery between 16 and 31 January 2040, Q7 the 191st of that half-month), found by a survey on 2040-01-19, perigee **2040-03-12 12:00 UTC** at 7,378 km in the polar plane.
  - The tug meets it about 2 days before perigee near 400,000 km on Highport's apogee side (an inbound hyperbola of v∞ 0.75 km/s falls from 400,000 km to perigee in 1.99 days by the hyperbolic Kepler equation), leaving Highport's perigee ~5.3 days earlier, near 2040-03-05.
  - The meeting must keep clear of the Moon, which stands at that point at its RA 90° crossings: on 2040-03-10 12:00 the Moon is at RA 320°, Dec −21°, 134° from the apogee direction and 708,000 km from the meeting point; at perigee (2040-03-12) still 106° away.
  - Slack: a pilot back from the January lunar window (at Highport ~2040-02-01) waits about five weeks, which the story fills (the find, the bagger probe's trip out; quests in `task 52`); one back from the February window (~2040-03-01) has four days.

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
| THRIFT | Dag Eliassen | pilot of the Mule THRIFT, the pilot's nearest colleague | wherever THRIFT is (Highport when THRIFT does not move) | friendly, teasing, talks about Mirela and the old days | cyan |
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
| THRIFT (M-09) | Mule tug, Cairn livery, pilot Dag Eliassen | Cairn Water | between trips in Highport's orbit, 5 km ahead of Highport (`docs/design.md` "Earth-orbit points of interest") | should: Highport ↔ Peary Depot, crossing the pilot's lunar trip; if dropped, told only, never drawn |
| TANKER 2 | uncrewed tanker | Cairn Water | berthed at Fulmar Depot's fixture B | no (part of Fulmar) |
| TANKER 3 | uncrewed tanker | Cairn Water | between trips in Highport's orbit, 2 km ahead of Highport | should: Highport ↔ Fulmar Depot; if dropped, told only, never drawn |
| AURORA 11 | crew capsule | Boreal Orbital | berthed at Tern Station's node | no (part of Tern) |
| GRINDSTONE | rock mover, pilot Ruth Abara | Cairn Water | parked in Skua Yard with its 10 m rock | no (fixed, `decision 47: Rigid bodies of the places`) |
| BAGGER 3 | uncrewed bagger probe: a box 1.5 m with two small solar wings | Cairn Water | nets 2040 BQ7 out beyond the Moon, then rides 1 km ahead of it on its rails, on the far side from the pilot's approach; after the grapple it flies on past perigee and out of Earth's sphere, out of view | with the rock (`milestone 4` "should", scenery on rails as `docs/design.md` "Asteroids" has it); if the passing rock is dropped, told only |

- Cairn Water's livery: white cab, a grey cairn of three stacked stones and the registration stencilled in black on both cab sides. Boreal Orbital's: white with a pale green band.

### Static texts
Fixed for the whole game (`decision 37`; `docs/design.md` "The DIY look", "Messages and radio"); exact wording, in capitals as stencilled or hand-lettered.
- Above the window, a stencilled steel plate: `M-07  PATIENCE`
- Tape notes (gaffer tape, white marker, Mirela's hand), each with its place on the console (`docs/design.md` "Layout"):
  1. Under the TARGET screen: `CLOSE AT 0.1. THE PLATE WON'T MIND. -M`
  2. Under the SHIP screen: `LOADED SHE TURNS LIKE A BARGE. START EARLY.`
  3. On the hull temperature gauge's bezel: `NOTHING INSIDE 0.55 AU IS WORTH IT.`

## Premise

The pilot starts in PATIENCE in the start orbit, 400 km up, tanks full, the day after Mirela's last entry in the log. Cairn Water has kept the seat warm because the plane is short of pilots this winter: the January lunar window is coming, Highport's plant wants rock, Lindgren Debris has paid for a push nobody had time to fly, and survey telescopes keep finding small rocks that pass close. The prelude is the pilot's first two and a half months: each job a little further out and a little heavier, from a 20 km hop to Tern Station to the Moon and to a rock that will not come back.

### What goes on around the pilot
- Cairn Water is proving that rock water can fill every tank in the plane and at the Moon. Its rocks have so far been fetched by GRINDSTONE on long trips; catching a small rock as it passes Earth, with a bagger probe and one tug, would be cheaper, and 2040 BQ7 is the first chance.
- Boreal Orbital depends on Cairn's tankers for Fulmar Depot and is uneasy about rocks: Ana Folau objects by RADIO to a 50 t rock passing 1,000 km above the plane's crew station, and wants the capture flown by the book. The quarrel is told, never fought.
- Lindgren Debris needs K-17 down before its contract runs out; Sami Lindgren is a small operator with a big promise to keep.
- Dag Eliassen flew with Mirela for years and watches the new pilot with some doubt and more goodwill.

### The ending
- The prelude ends at Highport, berthed, after 2040 BQ7 is in Highport's orbit and delivered to Skua Yard (or, if the passing rock is dropped, after the return from Peary Depot; the ending's messages stay the same apart from the rock's assay).
- Esther Okafor's assay: BQ7 is 21 % water by mass, wetter than anything fetched so far. Cairn Water's answer comes by RADIO from CAIRN OPS: the company will build a long-haul tug, a Mule cab with two years of stores and twice the tanks, to fetch rock from farther out than any tug has flown, and it wants the pilot who caught BQ7 to fly it. Mirela's last message: PATIENCE is the pilot's now, and Mirela would take the long-haul seat if the dose limit allowed it.
- The hook, not a resolution: the long-haul trip is the longer adventure. Nothing of it is flown here: its destinations are cut from this game (`docs/design.md` "Scope": destinations beyond the Moon and the passing rock), and it is told only by RADIO and the last job card.

### Introduction
The text shown before the first flight (`milestone 6`, `docs/design.md` "Feature list": "Short introduction before the first flight"); also the README's short introduction:

> January 2040. About twenty people live and work in one polar orbit around Earth, and most of what they need is pushed there by tugs: steel frames with a hydrogen engine, a small cab and a claw on the nose.
>
> You are the new pilot of PATIENCE, a Mule-class tug of Cairn Water. Its last pilot, Mirela Doru, flew it for six years, until the radiation dose limit. Mirela left the tanks full and three notes taped to the console.
>
> You are 400 km up, over the pole. Cairn Ops has your first job.

## The quest line

PLACEHOLDER(52): written by `task 52: Story: the quest line, its steps, completion conditions and checks`.

## How the story is told

PLACEHOLDER(53): written by `task 53: Story: how it is told, every RADIO and JOB text, read-through`.

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

### Rocks and dates
- 2040 BQ7: the passing rock (found 2040-01-19, perigee 2040-03-12 12:00 UTC); pilots say "BQ7".
- Skua Yard 1–6: the yard's bagged rocks by number (`docs/design.md` "Rigid bodies: attitude, mass, shapes and fixtures").
- Start: 2040-01-01 00:00 UTC (`*start-date*`); best lunar window: Moon at Highport's apogee 2040-01-26 02:36 UTC, TLI from Highport's perigee ~2040-01-21.
