# Design

This document is the game design of Luv in Space, written for the people and agents who implement it. It says what the player does, what the ship is and how it flies, what the cockpit shows, which places exist, and which milestone builds each feature; `milestone 2: Flight core: Newtonian flight in near-Earth orbit` to `milestone 6: Release: one command to play, a README for people` implement what its "Scope" section assigns them. It stays inside what `docs/luv-capabilities.md` says luv can carry and what `docs/architecture.md` fixes (units, frames, integrator, time warp), and cites the pm decisions it relies on by id and subject. Every physical figure states its formula or source; constants (μ, radii, g₀) are those of `docs/architecture.md` "Units" (`decision 14: Units and time epoch`). The story itself is in `docs/story.md`.

## The game in one page

Luv in Space is a hard sci-fi flight game about one pilot and one rugged tug in the early 2040s, when people have only just begun to work in space. The player flies the tug under real Newtonian physics from a low Earth orbit to the stations, depots and rocks of near-Earth space and out to the Moon and beyond, moving loads with a grapple and a hydrogen engine. A linear quest line, the prelude to a longer space adventure, gives the work a reason and teaches piloting one step at a time.

### What the player does
- Minute to minute: turns the ship with the RCS, holds an attitude (prograde, retrograde, target …), fires the main engine for a planned Δv, watches apoapsis and periapsis move on home-made instruments, and closes on a target at metres per second to latch the grapple onto its fixture.
- Hour to hour (game time, compressed by time warp): takes a job by radio, plans or presets a transfer with the autopilot, coasts at warp, refuels at a depot, pushes a load into a new orbit, travels to the Moon or an asteroid and back.
- What is never done: no walking, no landing, no trading, no combat as a core activity; the ship and its orbit are the game.

### The start
- The ship: the player's Mule-class tug (see "The ship(s)"), tanks full, no load, attitude held prograde.
- The orbit (`decision 38: Start orbit: polar`): 400 km circular (a = R⊕ + 400 km = 6,778,136.6 m), inclination 90° (polar), right ascension of the ascending node 270°, argument of latitude 0° at the start date; period 2π √(a³/μ⊕) = 92.6 min, speed √(μ⊕/a) = 7,669 m/s. Polar so that every Earth-orbit place stays in its plane under J2 (superseding the 51.6° of `decision 23: Start orbit`; "The places"). RAAN and phase are invented; `docs/story.md` keeps them (`decision 50: Story: start date and the passing rock's date`).
- The date: `*start-date*` in `units.lisp`, 2040-01-01 00:00 UTC (`decision 14: Units and time epoch`), kept by `docs/story.md` "Start date" for its lunar window (`decision 50: Story: start date and the passing rock's date`). Game time stays before 2050-01-01 (`decision 18: Ephemerides`).
- Sunlit at the start: on 1 January the Sun is near right ascension 281°, declination −23°, so the ship at the ascending node (ICRF −y) is 25° from the Sun and the Sun is 10° from the orbit plane (β = −10°); Earth's shadow at 400 km begins 180° − asin(R⊕/a) = 110° from the Sun, 22 min after the start.
- The ship's orbit is the reference for every Δv in this document ("Δv for the standard legs").

### What makes it a luv showcase
- All of it is Lisp: simulation, renderer and shaders (`define-shader` in `luv-in-space.shaders`); the live image (`./sly`) redefines a function or a shader while the ship flies, and the next frame shows it (`decision 11: Render pipelines are luv live shader pipelines`).
- A real Earth from orbit (Blue Marble impostor, `decision 13: Asset storage and attribution`), real stars (HYG), real ephemerides, and one renderer that draws from a cockpit panel at 0.5 m to Neptune (reversed-Z, `decision 4: Depth strategy`; camera-relative doubles, `decision 5: Precision of positions`).
- A home-made cockpit whose screens are luv's GPU text (Slug) on in-world panels.
- An agent can build, fly, test and screenshot it headless (`AGENTS.md` "Commands", flight scripts in `docs/architecture.md` "Scripted runs").

### Pillars
Later sections cite these to keep or cut a feature.
- **The ship is a tool.** Every part has a visible job; nothing is on the ship that a working tug would not carry, and its function reads from its silhouette.
- **Newton is the game.** All motion of the ship is integrated (`decision 17: Gravity model`); no teleporting, no autopilot shortcuts, no rails for the player; propellant is the limit.
- **The cockpit is home-made.** Instruments are bolted-on screens, gauges and lamps in a welded frame; the player can fly from it alone.
- **Real sky, real places.** Real Earth, Moon, planets and stars where they are known, with sources; invented places are plausible and say so.
- **Small and finished.** A feature that costs far more than it shows is cut (`milestone 1`: scope over realism); one ship done well beats many done thinly.

## The ship(s)

The player flies one ship, a Mule-class tug: a long welded steel truss carrying hydrogen and oxygen tanks, one hydrolox engine at the back, a crew cab on top at the front and a grapple on the nose. Its numbers below are computed from its parts (a throwaway script, not committed); the lasting choices are `decision 22: The player's ship and its propulsion`, `decision 24: Grapple, not docking; towing in`, `decision 25: Damage model`, `decision 26: Refuelling and economy` and `decision 27: Other ships`. The code lives in `ship.lisp` (definition, mass properties), `controls.lisp` (throttle, RCS mixer) and `meshes.lisp` (looks), per `docs/architecture.md` "Files in load order". The player's ship is PATIENCE, Cairn Water's Mule-class tug M-07 (`docs/story.md`, `decision 49: Story: names, call signs and ships`).

### Body axes and reference point
- Ship body axes FRD (`decision 15: Coordinate frames`): x forward (the main engine's thrust direction), y right, z down; "on top" means −z.
- Positions below are from a fixed reference point R: the truss's centre line at its aft end (the engine mount plane). The centre of mass (CoM) is computed from the parts and moves with propellant and load; thrusters' lever arms are taken from the current CoM.
- Layout along x: engine −2.2 to 0 m; truss 0 to 15.0 m with the LH2 tank (aft) and LOX tank (forward) inside it; cab on top from 10.5 to 15.0 m; grapple 15.0 to 16.6 m; a load beyond 16.6 m.

### Parts
| Part | Mass (kg) | Dimensions (m) | Centre in FRD from R (m) | Mass from |
|---|---|---|---|---|
| Truss frame (steel, welded) | 2,131 | 15.0 × 3.6 × 3.6, 10 bays of 1.5 | (7.5, 0, 0) | 4 longerons 76.1×4 mm tube (7.51 kg/m) × 15 m; 11 square frames and 40 face diagonals of 60.3×3 mm tube (4.46 kg/m), 158 m and 156 m; steel 7,850 kg/m³, linear mass π d t ρ; +15 % nodes, welds, brackets |
| Crew cab (life-support module) | 2,466 | cylinder Ø 3.0 × 4.5, axis along x | (12.75, 0, −3.5) | below |
| Main engine (RL10C-1 class) | 190 | Ø 1.45 nozzle exit × 2.2 long | (−0.7, 0, −0.5); gimbal at (0, 0, −0.5) | engine data ("Propulsion") |
| LH2 tank | 1,060 | Ø 3.2 × 8.86 (hemispherical domes), x 0.50–9.36 | (4.93, 0, 0) | Al 2219, 3 mm wall (hoop t = p r / σ = 0.2 MPa × 1.6 m / 150 MPa = 2.1 mm, rounded up), 2,840 kg/m³; MLI 1.0 kg/m²; +25 % rings, baffles, mounts |
| LOX tank | 579 | Ø 3.2 × 3.73, x 9.76–13.48 | (11.62, 0, 0) | as LH2, 4 mm wall (0.35 MPa: 3.7 mm) |
| Grapple | 450 | capture cone Ø 1.6 × 1.6, three claws | (15.8, 0, 0) | estimate: cone, claws and drives, latches, range finder and camera |
| RCS blocks (8) | 136 | 0.4 × 0.4 × 0.3 each, at the truss corners | x 0.5 and 14.5, y ±1.8, z ±1.8 | 24 thrusters × 3 kg + 8 brackets × 8 kg |
| Gas conditioning | 220 | 1.0 × 1.2 × 0.6, inside the truss bottom | (9.5, 0, 1.4) | estimate: heat exchanger, pumps and accumulators that gasify LH2/LOX for the RCS |
| Radiators (2) | 120 | 4.0 × 1.5 fins under the truss | (8.0, ±1.2, 2.55) | 10 kg/m² |
| Solar wings (2) | 140 | 2.0 × 6.0 each on a 0.5 m boom | (5.0, ±5.3, 0) | 4 kg/m² blanket + 22 kg boom and drive each |
| Equipment bay | 270 | 1.0 × 1.6 × 0.8, under the truss | (10.0, 0, 2.3) | battery 20 kWh at 150 Wh/kg (133 kg) + power electronics and avionics |
| Cabling and plumbing | 200 | along the truss top | (7.5, 0, −1.8) | estimate |
| **Dry** | **7,961** | | CoM (9.46, 0, −0.99) | sum |
| Propellant (LH2 + LOX) | 28,000 | 4,308 kg LH2 (62.7 m³), 23,692 kg LOX (21.4 m³) | in the tanks | mixture ratio 5.5; volumes with 3 % ullage |
| **Wet** | **35,961** | | CoM (10.34, 0, −0.22) | |

- Crew cab, 2,466 kg: pressure shell Al-Li 4 mm over 56.5 m² (cylinder π·3.0·4.5 + two ends) at 2,700 kg/m³, ×1.5 for frames, window and hatch (916 kg); micrometeoroid and thermal blanket 3 kg/m² (170); ECLSS (CO₂ removal, water recovery, fans, cabin O₂ drawn from the LOX tank) 450; cockpit, screens and avionics 250; interior, seats, stowage 250; consumables 250; crew 1 in a suit 180.
- Crew: one pilot and one jump seat; consumables for 180 person-days: food 1.0 kg/day and water make-up 0.3 kg/day (the ECLSS recovers the rest), 180 × 1.3 = 234 kg, plus 16 kg nitrogen for leaks. Breathing oxygen (~0.84 kg/day) comes from the LOX tank and is negligible against propellant. Whether endurance is simulated is "Flight"'s choice.
- Tank sizes: volume V = m / ρ × 1.03 with ρ(LH2) = 70.8 kg/m³ and ρ(LOX) = 1,141 kg/m³ (normal boiling points, NIST Chemistry WebBook); a tank with hemispherical domes of radius r = 1.6 m has V = π r² (L − 2r) + (4/3) π r³, solved for L. The two tanks fill 13.0 m of the 15 m truss; 28 t is close to what fits.
- Not simulated (pillar "small and finished"), the parts are there for their looks and mass: electrical power (the wings give 1,361 W/m² × 24 m² × 30 % = 9.8 kW at 1 AU against a few kW for the cab; never short in the inner solar system), heat rejection, hydrogen boil-off (MLI and the radiators' cryocooler are taken to hold it), ullage settling before ignition.

### Propulsion
- Main engine: one hydrolox expander-cycle engine of the RL10C-1 class, as flown on Centaur and Delta upper stages: vacuum thrust F = 101.8 kN, Isp = 449.7 s, oxidiser-to-fuel mixture ratio 5.5, mass 190 kg, gimbal ±4° (Aerojet Rocketdyne / L3Harris, "RL10 Propulsion System" data sheet). Exhaust velocity v_e = Isp g₀ = 4,410 m/s; mass flow ṁ = F / v_e = 23.08 kg/s; the whole load of 28 t burns in 1,213 s.
- Throttle: 10 % to 100 % or off; RL10s fly at full thrust, but NASA's Common Extensible Cryogenic Engine (CECE, an RL10 derivative, tests 2006–2009) throttled deeply, to 10 % in 2006 and 8 % in 2009. Ignition and cut-off are instantaneous; restarts unlimited.
- Thrust line: the gimbal sits 0.5 m above the truss axis, between the wet and dry CoM heights; the flight computer trims the gimbal through the current CoM, so the engine gives no torque. Trim angle atan((z_cm − z_g)/(x_cm − x_g)): wet 1.6°, half 0.9°, dry −3.0°, with a 50 t load 1.6° (wet) to 1.2° (dry): inside ±4°. The simulation applies thrust along +x at the CoM; the cosine loss (< 0.14 %) is not modelled.
- Why hydrolox: it gives the Δv for low lunar orbit or a planetary departure on one tank (below), the RL10 is the best documented engine of its size, and water turned into hydrogen and oxygen at depots is the reason tugs move water-bearing rock (the story's economy). Storable hypergolics (Isp ~316 s, e.g. the AJ10-190) would give 4.6 km/s at the same masses; electric drives (~1 N per 100 kW class, Δv of 1 km/s in months at 36 t) cannot be flown even at ×1,000 (`decision 19: Time-warp rules` limits thrust to ×1,000): cut.
- RCS: 24 thrusters of 445 N, Isp 360 s, burning gaseous oxygen and hydrogen gasified from the main tanks (the gas conditioning part), in the spirit of the Integrated Vehicle Fluids concept for ULA's ACES upper stage. Each draws ṁ = 445 / (360 g₀) = 0.126 kg/s at the main mixture ratio, so the ship has one propellant pool, as `decision 16: Integrator and simulation clock` integrates it (ṁ_p = −Σ F_i / (Isp_i g₀)). 1,000 kg of propellant spent through the RCS alone gives 360 g₀ ln(35,961 / 34,961) = 100 m/s at wet mass.
- Sources, each with the URL and the date it was read (`task 44: Verify the sourced figures of docs/design.md "The ship(s)"`); every figure above agrees with them:
  - RL10C-1: L3Harris (formerly Aerojet Rocketdyne), "RL10" spec sheet, `https://www.l3harris.com/sites/default/files/2024-07/l3harris-ar-rl10-spec-sheet.pdf` (the 2024-01 issue `…/2024-01/AJRD-LHX_RL10_SpecSheet_CMYK_APPROVED_011024.pdf` has the same table), read 2026-10-14 through the search engine's copy, both URLs answering 404 that day: thrust 22,890 lbf (101.8 kN), Isp 449.7 s, mixture ratio 5.5, weight 420 lb (190 kg), length 86 in (2.18 m), nozzle Ø 57 in (1.45 m). Wikipedia, "RL10", `https://en.wikipedia.org/wiki/RL10`, read 2026-10-14, agrees except thrust 101.5 kN (22,820 lbf); the manufacturer's figure is kept. NASA HAT, "Cryogenic Propulsion Stage" (2011), `https://ntrs.nasa.gov/api/citations/20110015783/downloads/20110015783.pdf`, read 2026-10-14, lists a pre-flight RL10C-1 (23,900 lbf, 448.5 s, MR 5.88): superseded by the flown engine's sheet.
  - Gimbal ±4°: NASA TM-87335, "Centaur Engine Gimbal Friction Characteristics Under Simulated Thrust Load" (Lewis Research Center, September 1986), `https://ntrs.nasa.gov/api/citations/19860022149/downloads/19860022149.pdf`, read 2026-10-14: "gimbal movement of ±4° in a square pattern" for Centaur's RL10; the C-1's own range is not published on its sheet.
  - CECE throttling: V. J. Giuliano, T. G. Leonard, W. M. Adamski, T. S. Kim, "CECE: A Deep Throttling Demonstrator Cryogenic Engine for NASA's Lunar Lander" (AIAA, 2007-07-09), `https://ntrs.nasa.gov/citations/20090028814`, read 2026-10-14: over 10:1 in 2006; NASA release 09-005, "NASA Tests Engine Technology for Landing Astronauts on the Moon" (January 2009), `https://www.nasa.gov/home/hqnews/2009/jan/HQ_09-005_Cryo_engine_test.html`, read 2026-10-14: 104 % down to 8 %, designed for 100 % to 10 %.
  - GO₂/GH₂ RCS Isp 360 s: D. K. Berkman, L. Schoenman (Aerojet TechSystems), "Oxygen/Hydrogen Thrusters for Space Station Auxiliary Propulsion Systems", JPL contract 956457 final report (1984), `https://ntrs.nasa.gov/api/citations/19850017072/downloads/19850017072.pdf`, read 2026-10-14: 30–100 lbf class, Isp 360–438 s at mixture ratio ~2.5 and 30–40 psia; R. J. Rollbuhler, "Experimental Performance of a Water-Electrolysis Rocket", NASA TM X-1737 (Lewis, February 1969), `https://ntrs.nasa.gov/api/citations/19690009679/downloads/19690009679.pdf`, read 2026-10-14: 338 s at the stoichiometric ratio 7.94; F. Zegler (ULA), "An Integrated Vehicle Propulsion and Power System for Long Duration Cryogenic Spaceflight" (AIAA, 2011), `https://www.ulalaunch.com/docs/default-source/extended-duration/integrated-vehicle-propulsion-and-power-system-2011.pdf`, read 2026-10-14: IVF lateral thrusters, mixture ratio 1–4, nominal Isp 400 s at 3.0. At the main mixture ratio of 5.5, between these points, 360 s is the conservative choice.
  - Densities: NIST Chemistry WebBook, "Thermophysical Properties of Fluid Systems", `https://webbook.nist.gov/chemistry/fluid/`, saturation properties at 0.101325 MPa for hydrogen (normal) and oxygen, read 2026-10-14: LH2 70.85 kg/m³ at 20.37 K, LOX 1,141 kg/m³ at 90.19 K.
  - Al 2219: Aubert & Duval, "2219" data sheet, `https://aubertduval.com/wp-content/uploads/2024/12/2219_GB.pdf`, read 2026-10-14: density 2.84 (2,840 kg/m³). The 150 MPa allowable: R. G. Forman, S. V. Glorioso, J. D. Medlock, "Flaw Growth Behavior in Thick Welded Plates of 2219-T87 Aluminum at Room and Cryogenic Temperatures", NASA TN D-7377 (JSC, August 1973), `https://ntrs.nasa.gov/api/citations/19730021214/downloads/19730021214.pdf`, read 2026-10-14: as-welded weld yield 145 MPa at room temperature and 193 MPa in liquid nitrogen; the tanks are pressurised cold, so 150 MPa is the cold weld yield over a factor 1.29.

### Mass budget and Δv
- Rocket equation: Δv = Isp g₀ ln(m₀ / m₁).
- Empty (no load), full tanks: 4,410 × ln(35,961 / 7,961) = **6,650 m/s**.
- With a load L: 4,410 × ln((35,961 + L) / (7,961 + L)): L = 50 t gives **1,738 m/s**, L = 100 t gives **1,017 m/s**.
- Thrust-to-weight (in g₀): wet 0.29 (2.83 m/s²), half 0.47, dry 1.30 (12.8 m/s²), wet with 50 t 0.12 (1.18 m/s²), wet with 100 t 0.08 (0.75 m/s²).
- Burn time for a Δv from mass m₀: t = m₀ (1 − e^(−Δv / v_e)) / ṁ. With 50 t: 100 m/s takes 83 s, 500 m/s 399 s, 1,000 m/s 756 s; with 100 t, 500 m/s takes 631 s.

### Δv for the standard legs
Computed from the start orbit (400 km circular) with μ and radii of `decision 14`: circular speed √(μ/r), Hohmann transfers, vis-viva v = √(μ (2/r − 1/a)); departures from LEO as Δv = √(v∞² + 2μ⊕/r₀) − √(μ⊕/r₀); captures as Δv = √(v∞² + 2μ/r_p) − √(μ/r_p); heliocentric legs between circular coplanar orbits (Earth at 1 AU, Venus a = 0.72334 AU, Mars a = 1.52371 AU from Standish Table 1, `decision 18: Ephemerides`; Ceres' a = 2.77 AU for the belt); the Moon at 384,400 km circular, patched conics. These are impulsive first guesses: real burns of 10–17 min from LEO add gravity losses of a few per cent, which the autopilot's shooting covers ("Flight"); "The places" checks each destination against this table with real elements. Prop and burn columns start from full tanks and no load.

| Leg | Δv (m/s) | Propellant (kg) | Burn | Left (kg) |
|---|---|---|---|---|
| Raise 400 → 500 km circular | 56 | 453 | 20 s | 27,547 |
| Raise 400 → 2,000 km circular | 769 | 5,753 | 4.2 min | 22,247 |
| Deorbit: periapsis 400 → 100 km | 87 | 705 | 31 s | 27,295 |
| GEO from the polar start (90° plane change at apogee) | 5,872 | 26,464 | 19.1 min | 1,536 |
| GEO from an equatorial LEO | 3,854 | 20,954 | 15.1 min | 7,046 |
| TLI (apogee at 384,400 km) | 3,082 | 18,083 | 13.1 min | 9,917 |
| LOI into 100 km circular lunar orbit (coplanar arrival, v∞ 0.83 km/s; from the polar plane 901, "The Moon") | 823 | — | 4.4 min (full) | — |
| TLI + LOI | 3,905 | 21,126 | 15.3 min | 6,874 |
| Return: TEI + propulsive capture into 400 km LEO | 3,905 | 21,126 | 15.3 min | 6,874 |
| Departure C3 = 0 (escape) | 3,176 | 18,462 | 13.3 min | 9,538 |
| Departure C3 = 10 km²/s² | 3,628 | 20,165 | 14.6 min | 7,835 |
| NEA example: circular coplanar 1.1 AU, departure (196 d) | 3,199 | 18,551 | 13.4 min | 9,449 |
| … rendezvous at arrival | 684 | — | 3.7 min (full) | — |
| Venus Hohmann departure (v∞ 2.50 km/s, 146 d) | 3,460 | 19,551 | 14.1 min | 8,449 |
| Mars Hohmann departure (v∞ 2.94 km/s, 259 d) | 3,569 | 19,953 | 14.4 min | 8,047 |
| Mars capture into 500 km circular (v∞ 2.65 km/s; μ♂ DE440) | 2,070 | — | — | — |
| Main belt (2.77 AU) departure (v∞ 6.32 km/s, 472 d) | 4,882 | 24,074 | 17.4 min | 3,926 |
| … circularise at 2.77 AU (v∞ 4.86 km/s) | 4,859 | — | — | — |

What one tank reaches from the start orbit (6,650 m/s):
- Anywhere in LEO's polar plane and high Earth orbit; GEO with 0.8 km/s left.
- Low lunar orbit (3.9 km/s), with 2.75 km/s left: not enough to return to LEO (3.9 km/s); a round trip needs a refuelling, e.g. at a depot in high Earth or lunar orbit ("The places").
- A planetary departure (escape, NEA, Venus, Mars) with 3.1–3.5 km/s left: a NEA rendezvous of the example kind, or a Mars capture (2.07 km/s), but no return without a depot; transfer times of 146–472 days exceed the cab's 180 person-days for anything but Venus. "The places" decides which of these are in scope (flyby, map only, or cut).
- The main belt: no (9.7 km/s for departure and circularisation); only with refuelling far from Earth or not at all.
- With a 50 t load (1.74 km/s): moves between Earth orbits of the "raise" kind, not interplanetary.

### Inertia
- From the parts: each as a point mass with its own moment (solid or thin-walled cylinder, box, rods for the truss) and the parallel-axis rule I = Σ (I_i + m_i (|d|² 𝟙 − d dᵀ)), d from the CoM.
- Propellant: a solid cylinder of 0.95 × the tank radius filling each tank from its aft end in proportion to the load, as under thrust; both tanks drain at the mixture ratio. Slosh is not modelled.
- The cab on top makes the tensor non-diagonal in body axes: I_xz up to 2.7×10⁴ kg m², tilting the principal axes up to 11° in the xz-plane (dry). `ship.lisp` hands the integrator the full symmetric tensor about the current CoM, with its inverse; Euler's equations I ω̇ = τ − ω × I ω (`decision 16: Integrator and simulation clock`) hold unchanged with it. The wording in `docs/architecture.md` "Integrator" ("principal axes") is adjusted by `task 45: Architecture: the integrator takes the full inertia tensor`.
- `ship.lisp` recomputes CoM and tensor from the current propellant mass (and load) once per step, held over the step as the architecture says.

| State | Mass (kg) | CoM (m) | I_xx | I_yy | I_zz | I_xz (kg m²) | Principal tilt |
|---|---|---|---|---|---|---|---|
| Wet | 35,961 | (10.34, 0, −0.22) | 8.79×10⁴ | 4.48×10⁵ | 4.21×10⁵ | 2.0×10⁴ | 3.4° |
| Half | 21,961 | (9.46, 0, −0.36) | 7.06×10⁴ | 3.35×10⁵ | 3.09×10⁵ | 2.7×10⁴ | 6.3° |
| Dry | 7,961 | (9.46, 0, −0.99) | 4.95×10⁴ | 1.99×10⁵ | 1.78×10⁵ | 2.7×10⁴ | 11.3° |
| Wet + 50 t | 85,961 | (15.21, 0, −0.09) | 1.55×10⁵ | 1.98×10⁶ | 1.95×10⁶ | −1.9×10⁴ | 0.6° |
| Dry + 50 t | 57,961 | (17.44, 0, −0.14) | 1.22×10⁵ | 8.60×10⁵ | 8.32×10⁵ | −3.6×10⁴ | 2.9° |
| Wet + 100 t | 135,961 | (16.85, 0, −0.06) | 2.98×10⁵ | 2.73×10⁶ | 2.70×10⁶ | −3.1×10⁴ | 0.8° |

- The load in these rows: a sphere of rock at 2,000 kg/m³ (radius (3m / 4πρ)^(1/3): 1.81 m for 50 t, 2.29 m for 100 t) with its fixture plate at the grapple's tip, x = 16.6 m, standing 0.3 m proud of the rock ("The places", "Rigid bodies: attitude, mass, shapes and fixtures"): centre at 16.9 m + r.

### RCS forces, torques and accelerations
- Layout: eight blocks at the truss corners, x = 0.5 m (aft) and 14.5 m (front), y = ±1.8 m, z = ±1.8 m; each block has three nozzles pointing outward: one along x (aft blocks push +x, front blocks push −x), one along y and one along z (each pushing towards the axis).
- The mixer (`controls.lisp`): the player, the autopilot and flight scripts command a body force and torque; the mixer picks duty cycles in [0, 1] for the 24 thrusters (a small bounded least-squares problem, solved per frame) that give the commanded torque exactly when it can, and as much of the force as fits without unwanted torque. Thrusters are throttled by pulse-width over the step, so the integrator sees their mean force.
- Capabilities (wet; dry; wet with 50 t):
  - Translation along ±x: 4 × 445 = 1,780 N: 0.050; 0.224; 0.021 m/s².
  - Translation along ±y or ±z without torque: front and rear forces balanced about the CoM, F_rear = F_front × arm_front / arm_rear, plus the axial nozzles' ±1.8 m arms when the CoM lies ahead of the front blocks: 1,266 N (0.035 m/s²); 1,391 N (0.175 m/s²); ~1,077 N (0.013 m/s²).
  - Pitch and yaw: a couple of two front and two rear lateral nozzles, τ = 2 × 445 × (14.5 − 0.5) = 12,460 N m, independent of the CoM: pitch 0.028 rad/s² (1.6°/s²); 0.063 (3.6°/s²); 0.0063 (0.36°/s²); yaw about the same (I_zz ≈ I_yy).
  - Roll: four lateral nozzles at 1.8 m, τ = 4 × 445 × 1.8 = 3,204 N m: 0.036 rad/s² (2.1°/s²); 0.065 (3.7°/s²); 0.021 (1.2°/s²).
- What it feels like: a 90° turn, accelerating half way and braking half way, takes 2 √(θ/α) = 15 s wet, 10 s dry, 32 s with 50 t; a 1 m/s change of closing speed takes 20–30 s wet, 5 s dry, 77 s with 50 t. Stopping a rotation of 0.01 rad/s (the limit for warp above ×4, `decision 19: Time-warp rules`) takes under 0.4 s wet. The ship feels heavy, more so loaded, which is the point of a tug; key map, rate damping and holds are "Flight"'s.

### Grapple and towing
- One mechanism (`decision 24: Grapple, not docking; towing in`): the nose grapple, a capture cone Ø 1.6 m with three claws, latches onto a standard grapple fixture, a plate with a central pin and three alignment guides. Fixtures are on depots, stations, cargo pods and the nets of bagged asteroids, always facing outward at a known point of the object.
- No docking port and no crew transfer: the pilot never leaves the cab.
- Grappled, the object is part of one rigid stack: combined mass, CoM and inertia tensor (as in the table above), the main engine's trim through the new CoM, the RCS mixer with the new lever arms. Releasing gives both bodies the stack's velocity and rotation at the release point.
- Rated load: 100 t; the grapple refuses heavier fixtures (with 100 t the stack has 1.0 km/s and turns 90° in over 40 s; heavier loads would give the player little to fly).
- How it is flown, the tolerances (closing speed, alignment) and what happens on contact: "Flight", "Grappling and berthing".

### Refuelling and resupply
- Where: depots only: Fulmar Depot (LEO), Highport (high Earth orbit) and Peary Depot (lunar orbit), "The places"; Tern Station restocks consumables only.
- How (`decision 26: Refuelling and economy`): grappled to the depot's fixture, the pilot sets a cockpit switch; LH2 and LOX flow at 10 kg/s total in the mixture ratio until full or switched off (28 t in ~47 min game time; time warp allowed while grappled and coasting); consumables are topped up at the same time.
- No economy: no money, no prices; propellant is free and the story's employer pays. A linear quest line needs no market, and an economy costs a UI and balancing (pillar "small and finished").

### Damage
- None (`decision 25: Damage model`): the ship is intact or destroyed. No part is damaged, leaks or is repaired; there are no hit points.
- Destroyed means death: a contact harder than the crash rule, a crash into a body, or the Sun's heat past its threshold; the rules and what follows are "Flight", "Dying and what follows".
- Cut, with the reason (pillars "Newton is the game" and "small and finished"): per-part damage (lost thrusters, leaks, dead screens), over-stress while pushing a load, repair at stations. Each needs effects, cockpit warnings and repairs for an event a careful pilot rarely meets.

### Other ships
Built from the tug's parts so they read as the same technology, and all on Kepler rails (`decision 27: Other ships`; `decision 19: Time-warp rules`): promoted to integration within 100 km of the player and demoted beyond 200 km, but never thrusting in view. A story ship that leaves changes its elements while beyond 200 km. Which ships exist where is for "The places" and `docs/story.md`.

| Kind | Built from | Dimensions (m) | Job |
|---|---|---|---|
| Other Mule tugs | the player's parts, other livery and markings | as the player's | pushing loads, waiting at depots |
| Tanker (uncrewed) | the truss lengthened to 24 m with two LH2/LOX tank pairs, no cab, grapple fixture at the nose, one engine | 24 × 3.6 × 3.6 | carries propellant between depots |
| Crew capsule | a frustum Ø 3.9 → 1.5 m, 2.5 m tall, on a cylindrical service section Ø 3.9 × 2.0 m with two solar wings | Ø 3.9 × 4.5 | crew rotation to stations |
| Rock mover | two trusses side by side (7.2 × 3.6 section, 20 m), two engines, the cab of a Mule, pushing a bagged rock of Ø 8–12 m | 20 + rock | moving large asteroids to high Earth orbit |
| Bagger probe (uncrewed) | a box bus 1.5 m with four of the Mule's RCS blocks on its corners, a small LH2/LOX tank feeding them as gas, a net canister, two solar wings 1.0 × 2.0 m on short booms; no main engine; 0.5 t | 1.5 × 6.0 × 1.5 (across the wings) | flies out to a small rock, nets it, despins it to a slow spin about the net's fixture axis, then rides beside it as a beacon until a tug takes the rock |

- The bagger probe's Δv: 160 kg of its 504 kg are propellant, burnt in the RCS at 360 s: 360 g₀ ln(504 / 344) = 1.35 km/s. Enough to net a passing rock far beyond the Moon: escape from Highport's perigee, √(2μ⊕ / r_p) − v_p = 10,395 − 9,926 = 469 m/s, then match the rock's v∞ (0.75 km/s in `docs/story.md`), 1.22 km/s in all; its trips are told, never flown in view.
- Free-flying story ships (`docs/story.md` "Other ships": THRIFT, TANKER 3, BAGGER 3) are objects of their own, immovable like the rock mover; their attitude, mass and shapes are in "The places", "Rigid bodies: attitude, mass, shapes and fixtures".

### How they look
- Generated meshes in `meshes.lisp` (`docs/architecture.md` "Files in load order"; luv has no mesh library, `docs/luv-capabilities.md` "Spheres and planet surfaces"): tubes as square-section beams or 8-sided cylinders, boxes, cylinders, spheres and hemispheres, frustums; drawn instanced where repeated (truss members, RCS blocks; "Instancing many objects").
- Materials, shaded in Lisp shaders: bare steel (grey, rough, darker weld nodes), crinkled gold or silver MLI foil on the tanks (a normal perturbation by noise), white-painted cab with stencilled registration and hand-painted markings, dark blue-black solar cells, white radiators, a dark nozzle with a lighter throat band.
- Lights: red at −y and green at +y on the front corners, a white strobe on the cab top (blink phase reduced on the CPU, `docs/architecture.md` "Floating-origin pitfalls"), a floodlight and range-finder light on the grapple; the main engine's hydrogen flame is nearly invisible in vacuum: a faint blue glow at the nozzle exit while firing, no long plume.
- The player's Mule as a parts list (the parts table gives positions):
  - Truss: 4 longerons Ø 0.076 × 15.0; 11 square frames of 4 tubes Ø 0.060 × 3.6; 40 face diagonals Ø 0.060 × 3.9.
  - LH2 tank Ø 3.2 × 8.86 and LOX tank Ø 3.2 × 3.73, hemispherical domes, foil-wrapped, visible through the open truss.
  - Cab: cylinder Ø 3.0 × 4.5 on four short struts on the truss top, a flat front with a rectangular window ~1.2 × 0.6 m looking forward along +x over the grapple, a side hatch at +y, two portholes Ø 0.35 m low in the side walls ("The cockpit").
  - Engine: bell (frustum) Ø 1.45 at the exit, Ø 0.3 at the throat, 1.6 long; chamber and turbopumps as a cylinder Ø 0.5 × 0.6 and two boxes, on a 4-strut thrust frame.
  - Grapple: cone Ø 1.6 × 0.8 on a cylinder Ø 0.6 × 0.8; three claws as boxes 0.1 × 0.1 × 0.6.
  - RCS blocks: boxes 0.4 × 0.4 × 0.3 with three small cones (nozzles) each.
  - Solar wings: two flat boxes 2.0 × 6.0 × 0.05 on booms Ø 0.08 × 0.5 at the truss sides.
  - Radiators: two fins 4.0 × 1.5 × 0.05 under the truss; equipment bay and gas conditioning as boxes under it.
- Silhouette: a long open frame with a white box on top at the front and a claw on the nose; from any side it reads "this pushes things".

## Flight

How the ship is flown: controls, control modes, views, instruments, time warp, the autopilot, grappling and towing, dying, saving and sound.

### Controls
Keyboard and mouse through luv's canvas events (`docs/luv-capabilities.md` "Windowing and input"); the lasting choice is `decision 29: Control scheme: key map, control modes, gamepad`. Code: `input.lisp` turns events into `controls` (`controls.lisp`), which the autopilot and flight scripts also write.
- Keys are luv's physical key names (SDL scancodes: `:w`, `:shift-left`, `:|.|` …), so the map keeps its positions on any keyboard layout; help texts show the layout's character (`canvas-key-event-character`). Letters below are QWERTY positions.
- Held keys act from press to release; key-repeat events are ignored; losing window focus releases all keys.
- Rates of held keys (throttle ramp, head look by keys) run in real time, not game time, so they feel the same at every warp level.

| Function | Keys | From milestone |
|---|---|---|
| Pitch nose down / up (−/+ pitch) | W / S | 2 |
| Yaw left / right (−/+ yaw) | A / D | 2 |
| Roll left / right (−/+ roll) | Q / E | 2 |
| Translate forward / back (+x / −x) | H / N | 2 |
| Translate up / down (−z / +z) | I / K | 2 |
| Translate left / right (−y / +y) | J / L | 2 |
| Fine control on/off (10 % torque and force, 0.5°/s rate limit) | F | 2 |
| Throttle up / down (50 %/s; from off, the first press gives 10 %; below 10 % is off) | Shift-left / Control-left | 2 |
| Throttle full / cut | Z / X | 2 |
| Rate damping on/off; releases an active hold to rate damping | T | 2 |
| Inertial hold: keep the current attitude | R | 2 |
| Hold prograde / retrograde | 1 / 2 | 2 |
| Hold normal / anti-normal | 3 / 4 | 2 |
| Hold radial out / radial in | 5 / 6 | 2 |
| Hold target / anti-target (nose at or away from the target) | 7 / 8 | 3 |
| Hold relative prograde / relative retrograde (along ± the velocity relative to the target) | 9 / 0 | 3 |
| Warp up / down / back to ×1 | . / , / slash | 2 |
| Pause on/off | P | 2 |
| View: outside ↔ cockpit | V | 3 (outside only in 2) |
| Map on/off | M | 2 |
| Outside camera: orbital frame ↔ locked to the ship | C | 2 |
| Reset camera or head look | Home | 2 |
| Map focus: next (ship, current body, target, other bodies) | Tab | 2 (ship, Earth); 3 targets; 4 bodies |
| Map zoom ×10 in / out | Page-up / Page-down | 2 |
| Target next / previous / clear | ] / [ / \ | 3 |
| Autopilot: open the preset list; choose; engage; disengage | O; Up/Down; Return; Backspace | 3 |
| Grapple (arm, release; "Grappling and berthing") | G | 3 |
| Propellant transfer on/off, berthed at a depot (`decision 26: Refuelling and economy`) | B | 3 |
| Acknowledge: the master alarm; radio messages read ("The cockpit") | Y | 3 |
| Controls help | F1 | 6 (before: the window title lists the main keys, as luv's games do) |
| Quit | Escape, Control+Q | 2; from 6 Escape pauses and opens the menu (resume, controls, credits, quit) |

- Pressing the key of the active hold again releases it to rate damping; pitch or yaw keys release any hold; roll keys during a hold turn the ship about the hold's axis and the hold keeps the new roll.
- Free keys kept for later parts: Space, F2–F12; B and Y are the cockpit's (`decision 36: Cockpit instruments: Slug screens, dot-matrix strip, gauges and lamps; Ghostty out`); U is sound on/off ("Sound"); there is no save key ("Saving").
- Mouse:
  - Right button held: look (cockpit: head look; outside and map: orbit the camera); luv's relative pointer mode while held, so the pointer stays put.
  - Wheel: cockpit field of view; outside camera distance; map zoom (×1.2 per notch).
  - Left click: cockpit switches and screen buttons (`milestone 3`, "The cockpit"); in the map, a marker becomes the target, a double click makes it the focus.
- Gamepad: `milestone 6` as a "could": SDL's gamepad subsystem polled on the canvas thread through cl-sdl3, left stick pitch/yaw, triggers roll, right stick translation (`docs/luv-capabilities.md` "Gamepad and joystick", ~80 lines). Not earlier: the keyboard covers every function, and there is no gamepad here to test with.

### Control modes
The flight computer in `controls.lisp` turns keys, holds and autopilot commands into a commanded body torque and force; the RCS mixer ("RCS forces, torques and accelerations") turns those into thruster duty cycles. Translation is always direct; rotation has three modes.
- Direct RCS (rate damping off): a held rotation key commands the full couple about its axis (12,460 N m pitch and yaw, 3,204 N m roll); release leaves the ship turning. For the expert and for showing Newton.
- Rate damping (the default; the start holds prograde, "The start"): a held key commands an angular rate of 5°/s (0.087 rad/s) about its axis, 0.5°/s in fine control; no key commands zero. The loop fires the RCS with the torque I (ω_c − ω) / 0.5 s, clamped to the available couple, so the ship stops turning when keys are released.
- Holds: the computer turns the ship to the hold's attitude by a rate-limited slew (bang-bang at 80 % of the available torque, at most 5°/s), then holds it within 0.1° and 0.02°/s, and reports "settled". The nose (+x) points along the hold direction; the roll is the one at engagement, or by default the cab (−z) away from the body (radial out) for prograde, retrograde, normal and anti-normal holds, towards normal for the radial holds.
  - Directions from the orbital frame (`docs/architecture.md` "Frames"): prograde along v, normal along r × v, radial out completing the triple, in the dominant body's frame. Target holds point at the target's position; relative holds along ± the velocity relative to the target (braking on approach).
  - Inertial hold keeps the attitude of the moment it is set; it is also what the attitude becomes when warp rises above ×4 without another hold.
- Translation: H/N, I/K, J/L command the full torque-free force on their axis while held, 10 % in fine control; the mixer keeps rotation commands exact and fits as much force as it can.
- What the player feels, from the ship's accelerations ("RCS forces, torques and accelerations"); one 60 Hz frame is the shortest key tap:

| | Wet | Dry | Wet + 50 t |
|---|---|---|---|
| Pitch acceleration | 1.6°/s² | 3.6°/s² | 0.36°/s² |
| Time to the 5°/s rate (rate damping) | 3.1 s | 1.4 s | 14 s |
| 90° turn at 5°/s, full torque | 21 s | 19 s | 32 s |
| Shortest direct tap, ×1 / ×4 | 0.027 / 0.11°/s | 0.060 / 0.24°/s | 0.006 / 0.024°/s |
| Shortest translation tap along x, ×1 / ×4 | 0.8 / 3.3 mm/s | 3.7 / 15 mm/s | 0.35 / 1.4 mm/s |

- ×1: everything at the speed it happens; the tug is slow to turn, more so loaded, and a 1 m/s closing speed takes 20–30 s of RCS to change. Close work (grappling, holding station) is flown here, in fine control.
- ×4: the same fine 1/64 s step and full rotational dynamics (`decision 19: Time-warp rules`), so all controls work; a 90° turn wet takes 5 s of real time and a direct tap is four times coarser, so ×4 is for turns, settling holds and short burns, with rate damping on.
- Above ×4: attitude is kinematic and manual RCS is off (`decision 19`); rotation and translation keys are ignored with the message "RCS OFF ABOVE ×4"; holds still work, so burns under warp fly in a hold ("Time warp").

### Views
Three views of one scene, drawn by one renderer (reversed-Z, `decision 4: Depth strategy`; camera-relative doubles, `decision 5: Precision of positions`); the lasting choice is `decision 30: Views and cameras`. Code: `camera.lisp`; the camera position is kept relative to the ship or the focus body, so re-centring (`decision 15: Coordinate frames`) never moves the picture. All views are available at every warp level, paused and during the autopilot; switching changes nothing in the simulation.

- Outside (`milestone 2`): the default view until the cockpit exists.
  - An orbit camera around the stack's CoM: distance from 1.2 × the stack's bounding radius (12 m for the empty Mule, more with a load) to 5 km, wheel ×1.2 per notch, default 40 m; vertical field of view 50° (73° horizontal at 1280×800).
  - Its axes in the orbital frame, so Earth stays below as the ship goes round (default), or locked to the ship's body axes (a chase camera; C toggles).
  - Shows the ship, its engine glow and lights, the target and other objects in range, Earth, Moon, Sun and stars, and the minimal readout ("Instruments").
- Cockpit (`milestone 3`): the pilot's seat in the cab.
  - Eye point (14.1, 0, −3.6) m from R in FRD: in the cab (cylinder Ø 3.0 × 4.5, x 10.5–15.0, axis at z = −3.5), 0.9 m behind the flat front wall, 0.1 m above the cab's axis, facing +x. The 1.2 × 0.6 m window centred at (15.0, 0, −3.6) spans ±34° horizontally and ±18° vertically from it. "The cockpit" keeps the eye point and lays its panels out around it; Home recentres the head to 16° below the nose line.
  - Field of view 70° vertical (96° horizontal) by default, so the window and the panels around it are both in view; the wheel zooms between 20° and 90°.
  - Head look by mouse: yaw ±150°, pitch −60° to +80°, no translation (the pilot is strapped in); Home recentres.
  - The grapple's tip sits 55° below the line of sight (atan(3.6 / 2.5)), outside the window: close work uses the grapple camera on a cockpit screen ("Grappling and berthing", "The cockpit").
- Map (`milestone 2` basic, completed by 3 and 4): the same scene from a far camera at true scale, not a separate chart.
  - Focus: the ship (default when the map opens), the dominant body, the target, or another body; Tab cycles. The camera orbits the focus (right drag) at a distance from 50 m (ship focus) or 1.2 × the body's radius to 1.5×10^13 m (100 AU), ×1.2 per wheel notch and ×10 per Page key, about 80 notches from Earth's surface to the edge; the zoom eases exponentially over 0.15 s.
  - Opening the map frames the ship's orbit: focus the dominant body, distance 2.5 × the apoapsis radius (or the body's radius, if larger), viewed 30° above the orbit plane from the normal side.
  - Bodies are drawn as their impostors at true size; below 6 px they become a marker with a label, so the Moon and the planets stay findable. The ship and targets are markers at every zoom.
  - Lines, as screen-space ribbons (`docs/luv-capabilities.md` "Orbit lines and markers"; `ribbons.lisp`): `milestone 2` the ship's osculating orbit (2 px) with apoapsis and periapsis markers and an impact marker where the orbit meets the crash sphere; `milestone 3` the predicted trajectory (`decision 20: Trajectory prediction on the frame thread`, two periods in the map) in place of the osculating orbit, the target's orbit, closest approach, relative ascending and descending nodes, and the planned burns ("The autopilot"); `milestone 4` the Moon's and planets' orbits (1 px), SOI changes on the trajectory, focus on the Moon, the Sun and the planets.
  - Colours: the ship's line cyan, the target's magenta (the colour rules of "The cockpit", "Readability"), bodies' orbits grey, the part of a trajectory after a planned burn dashed, impacts red; labels as the readout's text.
  - The exposure is fixed in the map, so stars stay visible next to a lit Earth.

### Instruments
What the pilot reads, and from which milestone; how each is drawn and where it sits is "The cockpit". In `milestone 2` the minimal readout is a screen-space panel of the overlay pass (`overlay.lisp`, `docs/architecture.md` "Files in load order") in the outside and map views; from `milestone 3` the cockpit screens carry the full set and the outside view keeps the minimal readout. Values come in display units through `units.lisp` (`decision 14: Units and time epoch`); orbital values are those of the dominant body.

| Instrument | Shows | Milestone |
|---|---|---|
| Altitude | above the dominant body's sphere (km) | 2 |
| Speed | orbital speed in the dominant body's inertial frame (m/s, km/s) | 2 |
| Apoapsis and periapsis | altitudes (km), with time to each | 2 (altitudes); 3 (times) |
| Throttle | lever position and engine on/off | 2 |
| Propellant | kg and % of the 28 t load | 2 |
| Attitude mode | direct, rate damping, the hold's name and "settled", fine control | 2 |
| Warp | nominal level and, below 90 %, the actual rate; "PAUSED" | 2 |
| Clock and date | game time as UTC date and time | 2 |
| Alarms and messages | periapsis below the crash radius, impact time; warp refused or dropped and why | 2 |
| Inclination, eccentricity, period | of the osculating orbit (degrees against the body's equator, minutes or days) | 3 |
| Vertical speed | rate of altitude change (m/s) | 3 |
| Attitude indicator | the nose against the orbital frame (prograde, normal, radial markers), the hold's target direction, angular rates per axis | 3 |
| Δv remaining | by the rocket equation at the current mass, load included (m/s) | 3 |
| Mass and thrust-to-weight | stack mass (t) and acceleration at full throttle (m/s²) | 3 |
| Burn | planned Δv, Δv left, time to start, burn duration ("The autopilot") | 3 |
| Target | name, distance, closing speed, relative velocity in body axes, time and distance of closest approach | 3 |
| Grapple range and alignment | from the grapple's range finder ("Grappling and berthing") | 3 |
| More alarms | low propellant (below the Δv of the planned burns, or 5 %), rotation above the warp limit, autopilot aborts | 3 |
| Body and SOI | the dominant body's name, time to the next SOI change | 3 (body); 4 (SOI) |
| Sun | distance (AU), flux, hull temperature with warning and danger marks ("Dying and what follows") | 4 |
| Life support | consumables in days left at the current crew ("Dying and what follows") | 4 |
| Radio and messages | the quest line's messages: RADIO and JOB on the COMMS screen ("The cockpit", "Messages and radio") | 5 |

### Time warp
Within `decision 19: Time-warp rules` and `docs/architecture.md` "Time warp"; the design's choices are `decision 31: Warp choices within the time-warp rules`. Code: `clock.lisp`, events from `prediction.lisp`.
- Levels: all eight, ×1, ×4, ×10, ×100, ×1,000, ×10,000, ×100,000, ×1,000,000; none greyed out. Where the 4,096-step cap holds the sim back (×1,000,000 runs at about ×490,000 in LEO), the readout shows the actual rate.
- Thrust under warp, below the ×1,000 limit of `decision 19`:
  - Manual throttle: up to ×10. One 60 Hz frame at ×100 is 1.67 s of thrust, 4.7 m/s wet and 21 m/s dry, too coarse for a cut by key; at ×10 it is 0.5–2 m/s.
  - Burns with a scheduled cut-off (the autopilot's burns, flight scripts' `:after-dv`): up to ×100; the cut lands on the 1/64 s grid at any warp, and the longest burn (all 28 t, 1,213 s) takes 12 s of real time.
  - Above ×4 a burn always flies in a hold (`decision 19`); throttling up at a higher warp lowers the warp to the limit at once instead of refusing the throttle.
- Drop-outs, from the prediction's events (warp lowered so the event is ≥ 3 s of real time away, ×1 at the event):
  - impact, or a periapsis below the crash radius (radii in "Dying and what follows");
  - SOI change: stop (×1 at the change), the architecture's default; the pilot checks the new body's periapsis before warping on;
  - a scheduled burn, at its start minus the lead time: 60 s or the estimated slew to the burn attitude plus 10 s, whichever is longer. The slew rule ("Control modes") turns 180° in 40 s wet and 59 s with 100 t, so 60 s covers almost every case;
  - entering the Sun's warning zone, inside 0.551 AU ("Dying and what follows"); a predicted contact with an object ("Grappling and berthing"); and the life-support alarms at 30 and 7 days (these added to `decision 19`'s list by `decision 43: Warp drop-outs for contact and life support`).
- Refusals (`decision 19`): warp up is refused while thrusting above the limit, while rotating faster than 0.01 rad/s without a hold, or with an event under 3 s of real time away at the new level.
- Shown: the readout's warp field (nominal level; actual rate in amber when below 90 % of nominal; "PAUSED"); a refusal or drop-out writes a message with its reason for 3 s, e.g. "WARP REFUSED: ENGINE ON, LIMIT ×10", "WARP REFUSED: ROTATING, SET A HOLD", "WARP ×1: PERIAPSIS BELOW 100 KM IN 3 S". From `milestone 3` the cockpit's WARP lamp flashes with it ("The cockpit"); from `milestone 6` a short tone ("Sound").
- Pause (P) is apart from warp: no steps run, every view, the camera and the map still work, flight inputs are ignored; resuming restores the warp level.
- Warp is allowed while grappled and coasting, e.g. while refuelling (`decision 26: Refuelling and economy`), under the same rules.

### The autopilot
The flight computer's planner: it plans burns from the current state to a goal, shows them, and flies them by writing `controls` as the pilot does (same throttle, holds, RCS, propellant; `milestone 3`'s constraint, pillar "Newton is the game"). The lasting choice is `decision 32: Autopilot scope and safety rules`. Code: `autopilot.lisp` (planning, execution), `prediction.lisp` (shooting), `places.lisp` (destinations).
- Planning: a first guess from Kepler elements and patched conics (Hohmann, plane change at a node, Lambert for intercepts; `decision 17: Gravity model`), then refined by shooting with the flight's own integrator and force model (`decision 20: Trajectory prediction on the frame thread`): differential correction of each burn's start time, Δv and direction until the predicted result meets the goal (apsides within 0.1 % or 1 km, plane within 0.01°, rendezvous point within 100 m). Iterations share the 1 ms prediction budget, so a plan takes a few frames to a second; "PLANNING…" shows meanwhile.
- Burns: finite, centred on the impulsive node (start at t_node − t_burn / 2, `t_burn` from "Mass budget and Δv"), at a fixed inertial attitude found by the shooting; the cut-off is by Δv delivered (as flight scripts' `:after-dv`), on the 1/64 s grid.
  - Throttle: full, unless the burn would last under 10 s, then lowered to make it 10 s, down to the 10 % minimum.
  - Burns under 0.5 m/s (below 1.8 s even at 10 % throttle wet, 10,180 N / 35,961 kg = 0.283 m/s²) are flown with the RCS along ±x (1,780 N: 0.050 m/s² wet).
  - After each burn the computer measures the result; an error over 0.2 m/s or 1 % adds a trim burn before the next leg.
- Manoeuvres, `milestone 3` (around the dominant body, Earth in practice):
  - Circularise at the next apoapsis or periapsis.
  - Set apoapsis or set periapsis to an altitude, burning at the opposite apsis.
  - Change orbit: a Hohmann transfer to a circular altitude (two burns).
  - Change plane: to an inclination, or to a target's plane, at the ascending or descending node, whichever is cheaper (combined with an apsis change when both are asked, at apoapsis).
  - Rendezvous with a point of interest (`places.lisp`, "The places") and stop at a range: match the plane; a Lambert intercept found by searching departure times over the next 48 h and transfer times of 0.4 to 1.5 periods for the least Δv, or a phasing orbit held for whole revolutions before it ("Rendezvous: search, phasing and the approach"), aiming 1 km behind the target on its orbit, or at another aim point when that path is not clear; a braking burn there; then the terminal phase on RCS and low throttle along the relative-retrograde hold, closing at most at range / 100 s (10 m/s at 1 km, 1 m/s at 100 m), down to the stop range (default 100 m, on the target's grapple-fixture axis if it has one), where it nulls the relative velocity below 0.02 m/s and keeps station (holds the range within ±5 m, at ×1 and ×4) until disengaged.
- Manoeuvres, `milestone 4`, across spheres of influence (patched-conic first guess, the whole chain shot through the SOI change):
  - TLI: from Earth orbit to a lunar periapsis altitude and inclination (default 100 km), the departure time searched over the next 30 days; with a lunar place as its goal ("TLI: Peary Depot"), aimed at the place's plane and continued as "Rendezvous: Peary Depot" ("Rendezvous: search, phasing and the approach").
  - Capture: at the periapsis of the current body, into a circular orbit or one with a set apoapsis (LOI; Earth capture on return); captures planned as part of a rendezvous take the phasing in them at no extra Δv.
  - Rendezvous with a place in another sphere of influence (Peary Depot from Earth orbit, Highport or Skua Yard from the Moon) as one chain, and with a target on a hyperbola ("Rendezvous: 2040 BQ7"), by the searches of "Rendezvous: search, phasing and the approach".
  - Return: TEI from lunar orbit to an Earth periapsis of 400 km, then capture.
  - Transfer to an asteroid or planet in scope ("The places"): a Lambert search over departure dates in the next 2 years and flight times up to 1.5 × the Hohmann time, for the least total Δv; departure burn from Earth orbit; mid-course corrections at 10 %, 50 % and 90 % of the coast (each typically under 10 m/s, from the ephemeris and integration difference); arrival as a rendezvous (asteroid, stop at 1 km then as in `milestone 3`) or a capture (planet).
- Not planned: landings (none exist), aerobraking (no atmosphere model, `decision 17`), gravity assists and low-thrust spirals (cost far more than they show; pillar "small and finished").
- Picking a preset: O opens the autopilot screen (a cockpit screen from `milestone 3`, "The cockpit"; in the outside and map views a panel of the overlay): the list of manoeuvres, then their goals (altitudes, inclinations, the selected target or a place). Up/Down move, Left/Right change a value (altitude in 10 km steps, inclination in 0.5° steps, held keys accelerate ×10 after 1 s; a place from the list), Return plans. Places of `places.lisp` appear as one-line presets ("Rendezvous: <place>").
- Showing the plan, before and while it runs:
  - Map: each burn as a marker on the predicted trajectory with its Δv; the trajectory after the first burn dashed (as "Views"); the goal orbit thin in the ship's colour.
  - Burn instrument ("Instruments"): number of burns, total Δv, propellant needed and left after, time to the next burn, its Δv and duration, the arrival time.
  - A second Return engages; Backspace drops the plan.
- Flying it: the autopilot sets the burn attitude hold (slewing at the rules of "Control modes"), throttles at the burn start, cuts by Δv, sets rate damping with a prograde hold between burns. It never changes warp: the player warps the coasts; the clock drops warp before each burn by its lead time (`decision 31: Warp choices within the time-warp rules`) and caps warp during a burn at ×100. "Warp to the next burn" as an automatic function is cut: the player's own warp keys and the drop-outs do it.
- Interrupting: Backspace disengages at once (throttle off, rate damping, the plan kept on the map in grey; Return re-plans from the new state and engages). Any rotation, translation or throttle key while engaged also disengages ("AUTOPILOT OFF: PILOT INPUT").
- Safety rules (planning refuses with the reason on the screen; execution aborts: engine cut, rate damping, alarm, "AUTOPILOT ABORT: <reason>", plan kept for re-planning):
  - No planned trajectory, including every intermediate orbit up to the prediction's horizon, comes below a body's minimum periapsis: its crash radius ("Dying and what follows") plus a margin of 50 km (Earth, the planets) or 10 km (the Moon). The Earth floor is thus 150 km altitude.
  - Propellant: the plan's propellant (by the rocket equation with the stack's current mass) plus a reserve of max(2 % of the plan's Δv, 20 m/s) must be on board; during execution, an abort when the propellant falls below what the remaining burns need plus half the reserve.
  - During a burn, each frame: if the predicted orbit after cutting now is safe but the one after the full burn is not (the burn has gone wrong), cut now; an attitude error over 5° cuts the burn, re-settles, and resumes once, then aborts.
  - No plan passes inside the Sun's warning distance (0.551 AU, "Dying and what follows"), or arrives after the consumables run out.
  - A contact or a latch during the plan aborts it.
- Flight scripts engage presets with `(:autopilot <preset> …)` and wait with `(:wait :autopilot-done)`; `milestone 3`'s headless test runs one script per Earth-orbit preset from the start orbit, `milestone 4`'s one per destination ("Scripted runs" in `docs/architecture.md`); the rendezvous cases below add their own.

### Rendezvous: search, phasing and the approach
How "Rendezvous: <place>" finds its burns when the target is far ahead or behind on an orbit of about its own period, when it lies in another sphere of influence, or when it moves on a hyperbola, and how the last kilometre stays clear of the target's neighbours. The quest line (`docs/story.md` "The quest line") budgets ≤ 50 m/s of phasing per rendezvous; every case it flies is checked below. The lasting choices are `decision 54: Autopilot rendezvous: phasing, lunar chain, hyperbolic targets, clear approaches` (refining `decision 32: Autopilot scope and safety rules`, whose safety rules apply unchanged) and `decision 55: Meeting 2040 BQ7 and the lunar arrival from the polar plane`. Figures by patched conics and 2D Lambert transfers in the common plane, a throwaway script, not committed; the shooting refines them. Code: `autopilot.lisp`.

#### The search
- Goal: the aim point (below) reached with the target's velocity, then the terminal phase. Time limit: the aim point within 48 h of planning; a goal on the AUTOPILOT screen, 12 h to 10 days in 12 h steps, so the pilot can trade time for Δv. A chain across spheres of influence counts the limit from its arrival burn (the LOI).
- Candidates of two families; the least total Δv within the limit wins, and within 1 m/s the earlier arrival:
  - A, direct: a single-revolution Lambert transfer, departures over the limit on a grid of 1/64 of the ship's period, transfer times 0.4–1.5 target periods in 32 steps.
  - B, phasing orbit: a tangential burn into an orbit of period T_ph, held N whole revolutions, then an A transfer departing within one T_ph of its end. The burn is at one of 64 points of the ship's next revolution, or merged into an apsis burn the plan already has (a capture, the LOI, a Hohmann's second burn), changing its size. First guess: if the target leads by Δt in time along its orbit (0 ≤ Δt < T_t, its period) at the phasing burn, catch up with T_ph = T_t − Δt / N or fall back with T_ph = T_t + (T_t − Δt) / N; N = 1 to 32 while N T_ph fits the limit. For a target at another height the phasing orbit is a drift orbit and the A transfer closes the rest.
  - Bounds: every phasing orbit obeys the floor of "Safety rules" and stays within 150,000 km of Earth or 5,000 km of the Moon, where the patched-conic first guess holds; others are dropped. Multi-revolution Lambert arcs are not used: a phasing orbit gives the same plans with a single-revolution solver.
  - Cost: two burns of δv = (ΔT / T) μ / (3 a v) each for a period change ΔT, from T ∝ a^(3/2) and vis-viva (2 v δv = μ δa / a²), v the speed at the burn: on a circular orbit δv = v (ΔT / T) / 3; at Highport's perigee 3 a v_p / μ = 3.13 s/km. Catching up half a period in N revolutions costs 2,557 / N m/s at 400 km, 320 / N at Highport's perigee, 545 / N in Peary Depot's orbit.
  - Falling back merged into a retrograde apsis burn costs nothing: the capture's Δv is split between this periapsis pass and the one N revolutions later, the impulsive sum unchanged (the two shorter burns lose less to gravity). So after any capture the planner has a phasing plan at no extra Δv within 2 T_t (N = 1).
  - Cost of the search: about 20,000 Lambert solves (a coarse grid per family and N, then golden-section refinement of the best three), ~40 ms in SBCL, spread over the 1 ms prediction budget per frame with the shooting (`decision 20: Trajectory prediction on the frame thread`); "PLANNING…" shows meanwhile.
- The quest line's rendezvous:

| Case (`docs/story.md`) | Gap | Plan | Phasing Δv | Time |
|---|---|---|---|---|
| Tern Station from the start (2) | 20° ahead at 420 km; the ship gains 1.59° per orbit | A, waiting ~19 h | 0 | ≤ 1 day |
| Fulmar Depot after K-17 (4) | any; from 400 km the ship gains 7.8° per orbit on it, a full turn in 2.96 days | worst case, the ship 296° short of the Hohmann's phase (waiting gains 245° in 48 h): B, a drift orbit at 370 km (catching up 296° in 45 h) or 530 km (falling back 64° in 43 h) | ≤ 33 (89 in all against the Hohmann's 56) | ≤ 48 h |
| Highport from Fulmar Depot (5) | any | A: departures over 48 h at 500 km, transfers 9.5–35.5 h; worst over Highport's phase 2,757 against the Hohmann's 2,729 | ≤ 28 | ≤ 48 h |
| Skua Yard from Highport, and back (6, 7's POD 14, 8) | 29–31 s behind Highport | B, one revolution, T_ph = T_t ± 31 s, burns at perigee | 2 × 0.11 | 23.7 h |
| Peary Depot after the LOI (7) | any | B merged into the LOI: falling back, T_ph ≤ 2 T_t, apoapsis ≤ 2,259 km | 0 (catching up instead: 45 m/s in 12 revolutions, 23.6 h) | ≤ 3.9 h |
| Highport after the lunar return (7) | any | B merged into the Earth capture at perigee: T_ph ≤ 2 T_t, apogee ≤ 119,200 km | 0 (catching up: 320 / N m/s) | ≤ 47.4 h |
| Skua Yard after BQ7's capture (9) | the drop point 1.81 h behind at perigee (below) | B merged into the capture: T_ph = 25.5 h, apogee 74,200 km (inside 9.4's Ap ≤ 80,000 km) | 0 | 25.5 h |

- Skua Yard after BQ7: BQ7 passes perigee at 2040-03-12 12:00 UTC (`decision 50: Story: start date and the passing rock's date`); the yard's reference passes it at 13:48 (Highport's elements with J2's secular mean motion n̄ = n (1 − ¾ J2 (R⊕/p)² √(1 − e²)), a 1.03×10⁻⁴ change, 23.6938 h; Kepler alone gives 13:37), and again 2040-03-13 13:30, when the stack arrives there.

#### Into another sphere of influence: Peary Depot (`milestone 4`)
- "Rendezvous: Peary Depot" from Earth orbit is one chain: TLI, mid-course corrections at 10 %, 50 % and 90 % of the coast, the LOI merged with phasing (above), the burn at the aim point, the terminal phase. "TLI: Peary Depot" plans the same chain. The plan shows every burn; the SOI change drops warp as always (`decision 31: Warp choices within the time-warp rules`), and the autopilot stays engaged through it.
- TLI aiming: the B-plane goal is the place's orbit, not a periapsis altitude and inclination alone: periapsis 100 km in Peary Depot's plane (i 90°, Ω 0° against the Moon's equator, "The Moon"), passing the pole that gives Peary Depot's sense of motion. Departures over 30 days at the ship's periapsis passes (from Highport's orbit one per 23.7 h), flight times 3–7 days; whatever plane error remains is paid in the LOI.
- Why the plane fits: the Moon crosses Earth's polar plane (Ω 270°) at right angles, moving along ∓x (ICRF) at its RA 90° and 270° crossings; the Moon's equator has its ascending node at RA ≈ 0° (IAU pole α₀ ≈ 270°), so its x axis is ICRF x. A ship rising in the polar plane meets the Moon with v∞ = v_ship − v_Moon along ±x plus the ship's own transverse and radial speed: in Peary Depot's plane except for the radial speed at arrival.
- The crossing costs speed: v∞ = √(v_Moon² + v_t² + v_r²), not the coplanar v_Moon − v_t of "Δv for the standard legs": 0.99–1.04 km/s, LOI into 100 km 880–925 m/s, not 823 (`decision 55`).
- The January window (`docs/story.md` "Start date"): the Moon at RA 90° on 2040-01-26 02:36 at 405,900 km (0.969 km/s, near its apogee, so its radial speed is ~0). The half ellipse from Highport's perigee to 405,900 km takes 129.8 h (TLI 375 m/s), which falls between Highport's perigee passes of 2040-01-20 06:01 and 2040-01-21 05:43:
  - leaving 2040-01-20 06:01: 140.6 h, apogee 407,400 km, arriving 0.085 km/s inbound, v∞ 0.991 km/s, 4.9° off the plane: TLI 376 + LOI 897 (880 in plane) = **1,273 m/s**, the search's choice;
  - leaving 2040-01-21 05:43: 116.9 h, arriving 0.120 km/s outbound, 6.9° off: 376 + 915 = 1,291 m/s.
  - Checked against "The Moon" (corrected there from 1,193): with POD 14 aboard (m₀ 47,758 kg), 1,273 + mid-course 10 + terminal 20 + 50 of margin = 1,353 m/s, 12,618 kg, 15,179 kg left at Peary Depot; the return is the mirror image, 1,273 m/s, and its phasing at Highport is free (table above).

#### A target on a hyperbola: 2040 BQ7 (`milestone 4`, "should")
- A hyperbola has no period, so the search uses absolute times: departures from any point of the ship's orbit (64 per revolution, not only periapsis) over up to 7 days, transfer times 2–10 days; the meeting while the target is inbound, inside Earth's sphere, at least 36 h before its periapsis (time to grapple and to plan the capture; past periapsis the target is out of reach, `docs/story.md` quest 9); the least sum of departure and matching Δv. The aim point is 1 km from the fixture's pin on its axis (the rock's axis is held against its inbound velocity, "Asteroids").
- 2040 BQ7 (v∞ 0.75 km/s, perigee 7,378 km along Highport's at 2040-03-12 12:00 UTC, `docs/story.md`): the cheapest plans leave 20–60 min after a Highport perigee (true anomaly 84–111°: a burn there turns the transfer's line of apsides towards the rock) and meet ~2040-03-10 23:00 near 325,000 km, 1.53–1.56 days before perigee:

| Leaving (after the perigee of) | Departure + matching (m/s) | Transfer | With 50 t after it (from full, + 30 m/s mid-course and terminal) |
|---|---|---|---|
| 2040-03-03 16:33 | 554 + 1,129 = 1,683 | 174 h | 1,100 m/s |
| 2040-03-04 16:14 | 571 + 1,249 = 1,819 | 150 h | 1,056 m/s |
| 2040-03-05 15:56 | 606 + 1,405 = **2,010** | 126 h | 996 m/s |
| 2040-03-06 15:38 | 662 + 1,627 = 2,289 | 102 h | 911 m/s |
| 2040-03-07 15:19 | 764 + 1,960 = 2,724 | 78 h | 785 m/s |

  - Why not ~2 days out near 400,000 km, as first drafted: there the rock is 17.6° past the apse line (true anomaly −162.4°), not at a transfer's apogee; meeting at least 48 h before perigee costs 2,404 m/s leaving 03-05. A meeting 24 h out would cost less (1,650 leaving 03-05) but leaves one day to grapple. `decision 55` moves the meeting; the dates of `decision 50` stand.
  - Against the ship: leaving 03-05, 2,040 m/s in all, 13,318 kg, 14,682 kg left, 996 m/s with the 50 t rock against the 565 m/s the loaded legs need and quest 9's floor of 650; even leaving a day after the story's deadline (03-07) 785 m/s remain.
  - At the meeting the rock's path has turned 0.6° since 400,000 km, so its fixture axis still points back along its velocity; BAGGER 3 rides 1.09 km ahead (1.735 km/s × 0.626 s), beyond the rock, ≥ 1.19 km from the stop point.

#### The terminal path and crowded targets
- Clearance: the terminal path, straight segments from the aim point to the stop point flown at most at range / 100 s, keeps every point ≥ 100 m from the target's CoM until the stop point and ≥ 200 m beyond the bounding radius of every other object within 100 km; so does the aim point. Every place's bound is under 73 m (Highport) and every stop point lies ≥ 100 m from its target's CoM (the pin plus 100 m along an outward axis).
- Aim point: the first candidate whose segment is clear: (1) 1 km behind the target along its orbit; (2) 1 km from the stop point along the local radius, outward unless the fixture axis points inward (an R-bar approach); (3) 1 km from the stop point out of the plane, either side. None clear: the plan is refused, "NO CLEAR APPROACH" (no place in scope comes to it).
- Holding a straight path off the target's orbit needs thrust against the relative motion: at most 3 n² × 1 km = 3.8 mm/s² in LEO (n = 1.13×10⁻³ rad/s at 400 km), well inside the RCS's lateral 13 mm/s² with 50 t ("RCS forces, torques and accelerations"); near Highport's apogee ~0.
- Skua Yard: a string 666.7 m apart at apogee, 6.9 km at perigee ("Earth-orbit points of interest", `decision 53: Deliveries, the stranded offer, checkpoint floors and Skua Yard's layout`). Its rocks hold their fixture axes as POD 14 does: inertial, aft along the orbit at apogee, so forward at perigee.
  - At apogee: the stop point is 100 m behind the target, in the line. (1) runs through the neighbour 667 m behind: refused. (2), 1 km above the stop point, comes straight down 567 m from that neighbour (bounds ≤ 3.7 m: ≥ 563 m clear) and 767 m from the one ahead; the aim point is 1,150 m from the nearest object.
  - At perigee: the stop point is 100 m ahead of the target, so (1) would pass through the target; (2) comes down 100 m ahead of it, the neighbours 6.9 km away. The phasing plans above arrive at perigee, so this is the usual case.
  - "Rendezvous: Skua Yard" (the yard, no fixture of its own) stops the stack's CoM at the drop point, Skua Yard 3's slot (+2,000 m at apogee, M₀ = 179.87880°, the front; empty after quest 6), and keeps station there for the release (`docs/story.md` quest 9.5); behind it the nearest object is ≥ 1,333 m away at apogee (Skua Yard 5, or POD 14's slot, empty after quest 7).

#### Scripted runs
Headless checks in the terms of `docs/architecture.md` "Scripted runs": each script starts from a stated state (`(:start :state …)`, "Start states" there, `decision 56: Flight-script actions, start states and the quest play-through`), engages a preset, waits with `(:wait :autopilot-done)` and expects; Δv bounds are the figures above plus 3 % for finite burns. `docs/architecture.md` "Scripts by milestone" gives each one's start state and expectations in script terms.
- `milestone 3`:
  - `rendezvous-fulmar-phasing`: 400 km circular, 296° short of the Hohmann's phase on Fulmar Depot (the worst case); "Rendezvous: Fulmar Depot": on station at fixture A within 48 h 47 min, Δv ≤ 113 m/s (89 + terminal 20, + 3 %).
  - `rendezvous-skua-yard`: unberthed at Highport; "Rendezvous: Skua Yard 5" (POD 14 and Skua Yard 3 in place): one phasing revolution, Δv ≤ 21 m/s with the terminal phase, on station on the fixture axis; along the whole path ≥ 500 m from every object but the target.
  - `skua-yard-apogee-approach`: the ship set 1 km behind Skua Yard 5 near apogee at its velocity; the same preset picks aim point (2) and stays ≥ 560 m from Skua Yard 1 and POD 14.
- `milestone 4`:
  - `rendezvous-peary`: free in Highport's orbit at POD 14's place on 2040-01-19 00:00, full tanks, POD 14 grappled; "Rendezvous: Peary Depot": TLI at the 2040-01-20 06:01 perigee (± 1 h), LOI phasing ≤ 2 Peary periods, on station within 6.5 days of the TLI, Δv ≤ 1,342 m/s (1,273 + 30 + 3 %).
  - `return-highport`: unberthed at Peary Depot after the January delivery; "Rendezvous: Highport": the Earth capture's phasing adds ≤ 2 m/s, on station within 47.4 h of the capture.
  - `intercept-bq7` ("should"): free in Highport's orbit 1 km behind Highport on 2040-03-05 00:00, full tanks; "Rendezvous: 2040 BQ7": departure within 1 h after the 15:56 perigee, meeting between 2040-03-10 20:00 and 2040-03-11 02:00, Δv ≤ 2,101 m/s (2,040 + 3 %), on station on the fixture axis; then grappled and "Rendezvous: Skua Yard": the capture split around one 25.5 h revolution, ≤ 5 m/s over the capture, the stack's CoM at the drop point by 2040-03-13 15:00.

### Grappling and berthing
The nose grapple is the ship's only way to hold on to something (`decision 24: Grapple, not docking; towing in`); the last 100 m to a fixture are always flown by the pilot, which is the game's finest piloting. The lasting choice is `decision 33: Grapple tolerances, contact and berthing`. From `milestone 3`. Code: `ship.lisp` (the grapple's state, the stack), `hazards.lisp` (contact), `cockpit.lisp` (the grapple camera).
- Fixtures: a plate with a central pin and three guides, an outward axis, and a kind: towable (cargo pods, bagged rocks; a mass up to the 100 t rating) or berth (depots, stations; any mass).
- G cycles the grapple: stowed → armed (claws open, floodlight and range finder on, the grapple camera on its cockpit screen) → stowed; when latched, G releases.
- Range finder and alignment (the "Grapple range and alignment" instrument), for the nearest fixture within 500 m in front of the nose: range from the cone's tip (16.6 m from R) to the plate, closing speed, the pin's lateral offset in body y and z, the angle between the ship's +x and the fixture's inward axis, the relative rotation rate; each value green inside its capture tolerance, amber outside.
- Flying it: the autopilot or the pilot stops on the fixture axis at ~100 m; the pilot turns to the target hold (7), then flies in with RCS translation in fine control at ×1 (taps of 0.8 mm/s wet, "Control modes"), braking with the relative-retrograde hold (0) or the translation keys. The grapple camera (a view from the cone along +x with a crosshair and the range values) is what the pilot looks at; the outside view helps.
- Capture tolerances, at the moment the tip touches the plate: closing speed 0.01 to 0.30 m/s; lateral offset ≤ 0.30 m (the cone's 0.8 m radius less the guides' reach); angle ≤ 10°; relative rotation ≤ 0.5°/s. Real berthing and docking runs at a few cm/s; the game's window is wider so a careful first try can work.
- Capture: the claws close over 2 s and the bodies become one stack in a perfectly inelastic coupling: linear and angular momentum conserved, the stack's state from the two bodies' masses, velocities, rotations and inertia ("Inertia"). The impulse at 0.3 m/s between a wet Mule and 50 t is 6.4 kN s, harmless.
- Missed capture (contact outside the tolerances, but at ≤ 1.0 m/s): a bounce with restitution 0.3 along the contact normal, no damage (`decision 25: Damage model`), message "NO CAPTURE: <TOO FAST|OFF AXIS|MISALIGNED|ROTATING>".
- Release: G while latched opens the claws; both bodies keep the stack's motion at release (as "Grapple and towing"), the pilot backs off with the RCS. Refused while the stack rotates faster than 0.5°/s or the engine fires.
- Towable fixtures over 100 t: the claws do not close ("LOAD 150 T OVER 100 T RATING"), and the contact counts as a missed capture.
- Berthing (a berth fixture): the ship becomes a passenger of the station: its state is held at the station's fixture (pose relative to it fixed), the station keeps moving on its rails (or integrated, when promoted), and engine and RCS are locked ("ENGINE LOCKED: BERTHED"). Refuelling and restocking work only berthed (`decision 26: Refuelling and economy`); warp is allowed. Release returns the ship to integration with the station's velocity.
- Contact with anything else (the hull against a station, a rock, a pod): the ship's collision shape is three boxes (truss with tanks 17.0 × 3.6 × 3.6 m including the engine, cab 4.5 × 3.0 × 3.0, grapple cone as a box 1.6 × 1.6 × 1.6); objects are spheres (rocks, bagged rocks) or a few boxes (stations, depots, pods), from their definitions in `places.lisp`. Tested each step against promoted objects (within 100 km, `decision 19: Time-warp rules`) by a swept bounding-sphere test first, then the boxes; a predicted contact is a warp drop-out like an impact. At ≤ 1.0 m/s relative normal speed: the bounce above; above 1.0 m/s: the ship is destroyed ("Dying and what follows"). 1 m/s against 36 t is 18 kJ, a car at 30 km/h: enough to buckle a truss tube or split the cab's 4 mm shell. Objects never break.

### Towing
- Grappled to a towable load, the ship and the load are one rigid stack (`decision 24`): its mass, CoM and full inertia tensor recomputed per step from the propellant and the load's mass properties (a sphere, box or the fixture's stated tensor), as in "Inertia"; the load's collision shape joins the ship's.
- Thrust: the main engine's gimbal trims through the stack's CoM (1.2° to 1.6° with 50 t, inside ±4°, "Propulsion"), so a burn pushes the stack without torque; the RCS mixer uses lever arms from the stack's CoM. A load is pushed, never pulled: there is no cable.
- What the pilot gets: 1.74 km/s and 0.12 g₀ wet with 50 t, a 90° turn in 32 s (rate damping), "Mass budget and Δv" and "Control modes"; the autopilot plans with the stack's mass, and its Δv and burn times show it.
- Delivery (`milestone 3`; the rule of `docs/story.md` "Rules for every quest", **Delivered**): a place takes a load in with its own handling gear (arms and nets, not shown), so the tug only parks the load and lets go; the last metres and the ship's own berthing stay the pilot's.
  - Receiving fixtures: Highport's plant fixture (`milestone 3`) and Peary Depot's berth (`milestone 4`); a flag of the fixture in `places.lisp` ("Rigid bodies: attitude, mass, shapes and fixtures"). They stay berth fixtures for the ship.
  - The rule: at the release of a towable load (G, "Grappling and berthing"), if the load's CoM is within 100 m of a receiving fixture's pin and its velocity relative to the receiving object (the object's velocity at the pin) is at most 0.05 m/s, a delivery is pending: load, fixture, release time. While pending, the load must stay within 150 m of the pin; 600 s after the release (on the 1/64 s grid: 38,400 fine steps, a scheduled time for the coarse step like a burn's), it is taken in: removed with cause `:delivered` (`docs/architecture.md` "Removal of objects"), the SHIP message "<LOAD> DELIVERED" ("Messages and radio"; the quest's RADIO tells the rest).
  - The pending delivery ends without removal if the load leaves 150 m, is grappled again, or the receiving object is removed; the load then drifts on as any released object (a quest sends the pilot back to it, `docs/story.md`).
  - Any towable load counts, not only a quest's: a rock delivered by mistake is gone (a later quest that needs it gets the stranded offer, "Dying and what follows", and its checkpoint brings it back).
  - Why 600 s: long enough that a load released with a drift is seen to walk away (0.05 m/s covers 30 m), short enough to watch at ×1 or ×4; the 100 m keeps the ship's own approach to the fixture free.
- Not modelled (pillar "small and finished"): flexing of the stack, the claws' load limits under thrust (1.18 m/s² on 50 t is 59 kN, inside any grapple worth its name), slosh in the load.

### Dying and what follows
Death ends the flight, never the game: the player goes on from the last checkpoint. The lasting choice is `decision 34: How death works`. Code: `hazards.lisp` (`milestone 2`: crash; `milestone 3`: contact; `milestone 4`: Sun, life support).
- Crash spheres (`docs/architecture.md` "Collisions and the Sun", radius R + h_crash, bodies as spheres per `decision 14: Units and time epoch`):

| Body | h_crash | Why |
|---|---|---|
| Earth | 100 km | entry interface: the tug has no heat shield and breaks up in entry (the architecture's default) |
| Moon | 0 | no atmosphere; the sphere at the mean radius 1,737.4 km; the highest terrain (~10.8 km) is ignored, since bodies are spheres |
| Venus | 200 km | the top of its dense atmosphere (Venus probes' entry interface) |
| Mars | 125 km | Mars entry interface as used for landers |
| Gas giants and the Sun | 1,000 km above the 1-bar radius; the Sun at R☉ | never approached by a reachable trajectory; only the swept test's completeness needs them |

  - Whether Venus and Mars are in scope is "The places"'s choice; the radii stand if they are.
  - Impacts are found by the swept test exactly (time and body-fixed point; `docs/architecture.md`); an impact on the prediction is an alarm and a warp drop-out ("Time warp").
- Contact: above 1.0 m/s relative normal speed with any object ("Grappling and berthing").
- The Sun: a hull temperature T that relaxes towards an equilibrium, as offered by `docs/architecture.md` "Collisions and the Sun":
  - Equilibrium of a sunlit, white-painted plate radiating from both sides with the cab's internal heat added: T_eq = ((α S + q) / (2 ε σ))^(1/4), with S = 1,361 W/m² / d² (d in AU; zero in a shadow), α = 0.30 and ε = 0.85 (white paint, aged), q = 100 W/m² for the cab's dissipation and planetary infrared, σ = 5.670374×10⁻⁸ W m⁻² K⁻⁴. This gives 269 K at 1 AU, 179 K in shadow, 309 K at Venus's orbit (0.723 AU), 231 K at Mars's (1.524 AU).
  - Relaxation: dT/dt = (T_eq − T) / τ, τ = 900 s, updated exactly per step for constant flux, T ← T_eq + (T − T_eq) e^(−h/τ). τ is the plate's linearised time constant C / (8 ε σ T³) at 350 K, with C = 2,700 kg/m³ × 4 mm × 1.5 (frames) × 900 J/(kg K) = 14,580 J/(m² K): 882 s.
  - Thresholds, with the distances where T_eq reaches them, d = √(α × 1,361 / (2 ε σ T⁴ − q)) AU:

| Hull temperature | Mark | T_eq reached at | What happens |
|---|---|---|---|
| 350 K (77 °C) | warning | 0.551 AU | amber "HULL TEMP" alarm; the cab's coolers cannot hold the cabin below 30 °C; warp drop-out on entering (the Sun hazard zone of `decision 19: Time-warp rules`); the autopilot plans nothing inside |
| 400 K (127 °C) | danger | 0.415 AU | red alarm, flashing; the instrument shows the time to 450 K at the current flux |
| 450 K (177 °C) | death | 0.326 AU | the pilot dies of heat ("LOST: HEAT") |

  - So Mercury's orbit (0.307 to 0.467 AU, Standish Table 1; T_eq 463 to 378 K) lies in the warning and danger zones and partly past death; Venus (0.718 to 0.728 AU) is safe. A ship on a transfer ellipse from 1 AU to a perihelion of 0.326 AU crosses 0.551 AU 24 days before perihelion (Kepler's equation on the ellipse, a = 0.663 AU), so the warning leaves weeks to turn back; against such changes of d, τ is short and T follows T_eq closely.
  - The hull temperature is shown from `milestone 4` ("Instruments"); in Earth orbit it moves between ~180 and ~270 K with the shadow, which teaches the gauge harmlessly.
- Life support (`milestone 4`): consumables in person-days, 180 when full ("Parts"), spent at one per day per person aboard (the jump seat doubles it, if the story uses it); restocked when berthed at a depot or station ("Refuelling and resupply"). Alarms at 30 and 7 days left (each a warp drop-out, an event of the same kind as those of `decision 19`), death at zero ("LOST: LIFE SUPPORT"); the autopilot refuses plans that arrive after the consumables run out. In Earth orbit it never matters; for a transfer it does ("Δv for the standard legs": 146 to 472 days).
- Not causes of death: running out of propellant (the ship drifts; the pilot may be stranded, and the stranded offer below ends it), high rotation, high acceleration (at most 1.3 g₀ dry).
- The stranded offer (`milestone 5`, `quests.lisp`; `docs/story.md` "Rules for every quest"): the quest line has no dead end, since the employer offers to fetch a ship that cannot finish its job.
  - Stranded, outside a berth, is any of: the ship's Δv remaining with its load below the current quest step's floor ("Saving"); outbound on an escape path, the osculating orbit around Earth with e ≥ 1 and past perigee (radial velocity > 0), or the Sun the dominant body; the step's own object out of reach, as the quest names it (removed, e.g. delivered or burned up by mistake, or a rock past its perigee, `docs/story.md`).
  - Evaluated after each step with the quest conditions; when stranded has held for 10 min of game time without a break (the time on the 1/64 s grid, a scheduled time like a burn's), CAIRN OPS sends the offer as a RADIO marked **Y**, with the light-time delay (`docs/story.md` drafts the text: TANKER 3 will fetch the ship, and the step is flown again from the last good point). A break in the condition resets the 10 min; one offer per checkpoint.
  - Y (or a click on COMMS) on the offer restores the last checkpoint exactly as the LOST panel's Return does: the quest state, the ship and every object as they were. Until Y the pilot flies on; the offer stays in the COMMS log and Y works on it until a new checkpoint is written (which a floor allows only once the ship is fit again, "Saving").
  - Nothing is shown of the rescue: no tanker flies in view, it is told. Not a warp drop-out: the MSG lamp and the overlay's line show it (`decision 43: Warp drop-outs for contact and life support` stands).
- What the player sees:
  - The simulation stops at the moment of death (the impact time on the Hermite, the contact, the threshold crossing); warp falls to ×1.
  - The view switches to outside, the camera fixed at its last position, looking at the ship; for an Earth impact a 3 s orange glow on the ship (entry heating), for a contact a white flash, for heat the ship's lights going out, for life support the cab's lights going out.
  - A panel: "LOST: <cause>" with the facts (body, speed, latitude and longitude; or hull temperature and distance in AU; or days), and the choices: Return restarts from the last checkpoint, N starts a new game.
  - Nothing else: no debris simulation, no explosion particles (pillar "small and finished").
- What follows: a restart from the last checkpoint ("Saving"); there is no permanent death. Before `milestone 5` the checkpoints live in memory: `milestone 2` has only the start state (Return and N are the same); `milestone 3` adds one when an autopilot plan completes and when the ship berths.
- Quests (`milestone 5`): death never fails or skips a quest; the checkpoint is at or after the start of the current quest step, so the step is flown again. A quest may set its own checkpoint (e.g. before a dangerous burn).

### Saving
From `milestone 5` (its outcome "progress saved and resumed"); the lasting choice is `decision 35: Saving and audio`. Code: `saves.lisp`, as `docs/architecture.md` sets it: one versioned s-expression per file under `$XDG_DATA_HOME/luv-in-space/`, read safely, written atomically.
- Two files, no save slots and no save key:
  - `checkpoint.sexp`: written at the start of each quest step, when a quest asks, when the ship berths, when an autopilot plan completes, and only in a safe state (no impact or contact predicted, coasting, hull below the warning) with the step's floor met (below); death and the stranded offer restart from it.
  - `resume.sexp`: written when the player quits (Escape menu or closing the window) and every 5 min of real time; Continue on the title screen loads it. It is deleted on death, so a doomed state is never resumed.
- Checkpoint floors (`milestone 5`; `docs/story.md` "Rules for every quest"): each quest step has a Δv floor, a speed v_f (m/s) with a load mass m_L (the load it carries or will carry; 0 and no load when not named). While a quest runs, a checkpoint (the step's start, berthing, a completed plan, a quest's own) is not written while the ship's Δv remaining is below the floor: Δv = v_e ln((m_dry + m_L + m_p) / (m_dry + m_L)), v_e = 4,410 m/s, m_dry = 7,961 kg ("Mass budget and Δv"), m_p the propellant; m_L the step's load mass, or the latched load's if the step names none. So a checkpoint never holds a ship that cannot finish its job, and death or the stranded offer always leads back to a flyable state. The step's floor is part of the quest state; a step whose start is vetoed keeps the previous checkpoint, and the next berthing or completed plan that passes takes one.
- What is saved: format version; `t`; the dominant body; the ship's state (r, v, q, ω, propellant) as exact doubles; controls (throttle, hold mode, fine control, rate damping; the throttle is off in a checkpoint, which is only taken while coasting); the grapple state and the latched object with its pose relative to the ship; the hull temperature and consumables; the target; the states of promoted objects and the elements of objects whose rails a quest changed; quest state (`quests.lisp`); the warp level is not saved (loads run at ×1, paused). Autopilot plans are not saved: re-planning is cheap.
- New game (N, or the title screen from `milestone 6`): the start state ("The start"); it overwrites both files after a confirmation.
- Tests: `docs/architecture.md` "Saves" (round trip bit for bit, continuation, safety).

### Sound
Only what travels through the hull and the cab's air: there is no sound in vacuum, so the ship sounds like a machine around the pilot. Synthesised in Lisp, no sound files, through one `audio-sink` and a small mixer (`docs/luv-capabilities.md` "Audio"; `audio.lisp`). `milestone 6`, priority "should" (`decision 35: Saving and audio`): flight, cockpit and story come first, and an agent cannot hear it; the game is complete without it.
- Voices:
  - Cab ambience: fans and pumps, a constant low hum with slow noise; always on while alive.
  - Main engine: low-passed noise and a 40–60 Hz rumble, gain by throttle, a thump at ignition and cut-off.
  - RCS: a short noise burst per thruster pulse (gain by duty cycle), panned by the thruster's y relative to the eye point; the front blocks louder (nearer the cab).
  - Valves and mechanisms: clicks on throttle steps and hold changes, the grapple's motor whine and the claws' clunk, the propellant transfer's hiss while refuelling.
  - Contact: a dull thud by impact speed.
  - Alarms: a two-tone chime for warnings (amber), a fast beep for danger (red), a short tone for warp refused or dropped; each alarm sounds once per onset, the danger beep repeats until acknowledged (Y or a click on the MSTR lamp).
  - Radio: static and a chirp when a message arrives (`milestone 5`'s messages); voices are not recorded (text only, "The cockpit").
- Under warp above ×4 the RCS is off and so is its sound; the engine's sound plays at any warp it runs under (up to ×100); ambience plays always, paused included.
- Key: U toggles sound on and off (from the free keys of "Controls"); the state is kept in the resume file.
- No audio device (headless agents, `nix flake check`): the game runs silent; tests never open the sink.

### Not in "Flight"
- Cut, with the reason (pillar "small and finished" unless named): docking ports and crew transfer (`decision 24`), EVA, landing on any body (the tug has no legs; pillar "the ship is a tool"), aerobraking and drag (`decision 17: Gravity model`), automatic warp by the autopilot, a debris or explosion simulation, per-part damage (`decision 25: Damage model`), recorded radio voices, manual save slots.

## The cockpit

The home-made cockpit inside the cab: its layout, each instrument and how it is drawn, and how messages reach the pilot. Mismatched screens, a dot-matrix strip, needle gauges and lamps are bolted to a welded rack below and beside the window; together they carry every instrument of "Flight", "Instruments", so the pilot can fly from the cockpit alone (`milestone 3`). The lasting choices are `decision 36: Cockpit instruments: Slug screens, dot-matrix strip, gauges and lamps; Ghostty out` and `decision 37: Message channels the story may use`. Code: `cockpit.lisp` (geometry, panels, picking, instrument logic), `text.lisp` (Slug text), `ribbons.lisp` (plot lines), the dot-matrix glyphs of `overlay.lisp` (`docs/architecture.md` "Files in load order"); all drawn in the scene's pass with reversed-Z, near 0.1 m (`decision 4: Depth strategy`), camera-relative from the ship's double state (`decision 5: Precision of positions`), through live pipelines (`decision 11: Render pipelines are luv live shader pipelines`).

### The DIY look
- The cab's inside: a cylinder of 1.40 m inner radius (the Ø 3.0 m shell less frames and lining), painted off-white, with ring frames of steel angle every 0.5 m and stringers between; a flat deck of tread plate; the flat front wall with the window; the aft wall with the ECLSS rack (boxes with fan grilles), a stowage net with bags, a red extinguisher; the side hatch (Ø 0.8 m, a handwheel) in the +y wall; a folded jump seat aft at −y.
- The console: a rack of slotted steel channel (41 × 41 mm, bolted and welded) from the deck to the front wall; each screen hangs on L-brackets in its own bezel: black plastic, grey ruggedised with four bolts, green military surplus, beige office monitor, a bare panel with taped edges; a cardboard or sheet-metal sun hood taped over two of them. Nothing is flush, nothing matches.
- Cables: one to three per panel, tubes Ø 15–25 mm sagging in a few segments to a cable tray on the rack and on to a conduit along the ceiling, aft; zip ties every 0.2 m; black and grey, one red and one yellow run; amber Kapton tape on the joints.
- Labels: strips of grey gaffer tape (50 mm) under each panel and switch, hand-lettered in white marker (Iosevka Aile, rotated ±2° at random, ragged ends cut in the fragment shader), each with its key in brackets ("PRO [1]"), so the cockpit teaches the key map; duct-tape patches on the lining.
- The window: the 1.2 × 0.6 m opening in the front wall (`decision 30: Views and cameras`), a welded frame of steel angle 50 × 50 × 5 mm, outer 1.30 × 0.70 m, with bolts Ø 12 mm every 0.1 m; glass as one transparent quad (95 % transmission, a faint green tint, darker at the edges); a boresight cross of white tape (two strips 0.10 × 0.012 m) on the glass at its centre, which marks the ship's +x from the eye point, since eye and window share y and z. The ship's name, stencilled on a steel plate above the window: "M-07  PATIENCE" (`docs/story.md` "Static texts").
- Two portholes, Ø 0.35 m, low in the side walls: at the start's prograde hold (cab radial out) Earth's horizon lies 19.8° below the nose line (acos(R⊕ / (R⊕ + 400 km))), just under the window's lower edge at 18.4°, so the window shows space and the portholes show Earth (head look about 90° right or left, 40° down). The outside meshes ("How they look") get the same portholes.
- Light: two LED strips on the ceiling (warm white, `dz` −1.25, `dy` ±0.4, the cab's length); the screens' and lamps' own glow; sunlight through the window and portholes as a sharp patch, found per fragment by intersecting the ray towards the Sun with the openings' rectangles (an analytic test, no shadow map), so the patch wanders through the cab as the ship turns. Earthlight is cut (a second light for little).
- Seen from outside, the window is dark glass with the screens' faint glow behind it.

### Layout
Positions are panel centres relative to the eye point, (14.1, 0, −3.6) m from R (`decision 30`), in ship body axes FRD (`decision 15: Coordinate frames`): `dx` forward, `dy` right, `dz` down; the absolute position is the eye point plus these. Every panel faces the eye (its normal points at the eye point) unless the table says otherwise; directions are azimuth (right +) and elevation (down +) from the nose line. The front wall is at `dx` = 0.90, the cab's axis at `dz` = +0.10, the deck at `dz` = +1.15; every panel lies inside the 1.40 m radius. The numbers come from a throwaway layout script (panel rectangles checked for overlap as seen from the eye; not committed).

| Panel | Centre (dx, dy, dz) m | Direction | Size w × h (m) | Text grid | Contents |
|---|---|---|---|---|---|
| Window | (0.90, 0, 0) | 0°, 0° | 1.20 × 0.60 | — | the view forward; spans ±33.7° by ±18.4° |
| Caution lamps | (0.90, −0.72, 0) | −38.7°, 0° | 0.08 × 0.58, five lamps 0.08 × 0.05 at `dz` −0.22, −0.11, 0, 0.11, 0.22 | 4 characters each | MSTR, PERI, PROP, TEMP, LIFE; on the front wall, turned 30° towards the eye |
| Status lamps | (0.90, +0.72, 0) | +38.7°, 0° | as the caution lamps | 4 characters each | WARP, RCS, AUTO, GRPL, MSG; as the caution lamps |
| ORBIT screen | (0.60, −0.28, +0.35) | −25°, 27.5° | 0.30 × 0.20 | 24 × 8 | orbital readout |
| ATTITUDE screen | (0.67, 0, +0.35) | 0°, 27.5° | 0.24 × 0.20 | 19 × 8 | attitude plot, mode, rates |
| TARGET screen | (0.60, +0.28, +0.35) | +25°, 27.5° | 0.30 × 0.20 | 24 × 8 | target data, or the grapple camera when armed |
| Dot-matrix strip | (0.55, 0, +0.44) | 0°, 38.5° | 0.30 × 0.08 | 19 × 3 | clock, warp, ship messages |
| Button block | (0.46, 0, +0.52) | 0°, 48.5° | 0.26 × 0.13, 18 buttons 0.036 m square at 0.042 m pitch | 4 characters per button | holds, modes, grapple, transfer, autopilot, warp |
| AUTOPILOT screen | (0.42, −0.26, +0.49) | −32°, 45° | 0.26 × 0.19 | 22 × 8 | orbit plot, preset menu, plan and burns |
| SHIP screen | (0.42, +0.26, +0.49) | +32°, 45° | 0.26 × 0.19 | 22 × 8 | propellant, Δv, mass, throttle, load, consumables, Sun |
| COMMS screen | (0.34, −0.60, +0.12) | −60°, 10° | 0.30 × 0.24 | 26 × 11 | job card, radio and ship message log |
| Throttle quadrant | (0.17, −0.24, +0.52) | −55°, 61° | 0.14 × 0.16 | — | throttle lever, ENG lamp, PROP gauge (Ø 0.10) |
| Hull temperature gauge | (0.17, +0.24, +0.52) | +55°, 61° | Ø 0.12 | — | needle gauge, 150–500 K |
| Portholes | (−0.30, ±1.07, +1.00) | ±90°, 42° | Ø 0.35 | — | Earth below; in the wall, facing outward |

- The default head direction (Home) is 16° below the nose line: with the 70° field of view (`decision 30`) it shows the whole window, the lamp columns and the console down to 51° (the button block's lowest row and the side screens' bottom edge are cut by 2°); the throttle quadrant, the hull gauge and the COMMS screen need a glance (head look by right drag).
- The pilot's seat is not drawn beyond its headrest edges; the eye point stays where `decision 30` puts it.

### Instruments and how each is drawn
Five kinds of device, drawn in the scene's pass (the grapple camera's image in its own small pass before it):
- **Screens**: a dark quad (the panel's face) with Slug text drawn on its plane 1 mm in front, as Luvcraft's world-text runs do (`text.lisp`), and plot lines and markers as ribbons on the same plane (`ribbons.lisp`). Text is resolution-independent, so a screen stays sharp at the 20° zoom and needs no render target or mipmaps; this replaces the screen textures that `docs/luv-capabilities.md` "HUD and instruments" assumed. Monospace (Monaspace Neon) on a fixed character grid: values right-aligned in fixed columns, so digits do not jump. Text and plots update at 10 Hz of real time (values sampled from the interpolated state), mode changes at once; the ATTITUDE plot every frame.
- **Grapple camera**: the only render target: the scene (without the cockpit) from a camera on the grapple's tip, (16.6, 0, 0) m from R, looking along +x, 50° field of view, 480 × 320 pixels, every frame while the grapple is armed, greyscale with a little noise; shown on the TARGET screen's quad with the grapple values as Slug text over it.
- **Dot-matrix strip**: three lines of 19 characters of 5 × 7 amber LED dots (the glyph table of `decision 7: Placeholder labels: dot-matrix overlay quads`, moved to `overlay.lisp`, extended with `×`, `°` and the punctuation messages use), drawn as instanced dot quads in the world at a 2.45 mm pitch; unlit dots show faintly. Every frame.
- **Needle gauges**: a disc with a printed scale (Slug text and arcs on its face), a needle as a thin box rotated about the centre; the needle follows its value with a first-order lag of 0.3 s, so it swings like a real one. Every frame.
- **Lamps and buttons**: a lens quad with an engraved label (Slug text), unlit a dark coloured lens, lit an emissive colour; flashing at 2 Hz (warning, red) or 1 Hz (caution, amber), in real time, so they flash while paused. The 18 buttons are illuminated push-buttons: lit while their function is on. The throttle lever is a box on a pivot, turned 0–60° with 0–100 % throttle, −10° at off.
- Fonts: Monaspace Neon (screens) and Iosevka Aile (tape labels, lamp engravings), both shipped in luv's `fonts/` under the SIL Open Font License 1.1; copied with their licence files to `assets/fonts/` with a `SOURCE.md` and credited in `CREDITS.md` (`decision 13: Asset storage and attribution`). luv's `fonts/equity/` is private and never used.

| Instrument ("Flight") | Where | Drawn as | Milestone |
|---|---|---|---|
| Altitude, speed, vertical speed | ORBIT lines 2–4 | screen text: "ALT  400.00 km", "SPD 7.669 km/s", "VS  +0.0 m/s" | 3 |
| Apoapsis and periapsis, times | ORBIT lines 5–6 | "AP 400.0 km T-45:12"; red, inverted, when below the crash radius | 3 |
| Inclination, eccentricity, period | ORBIT lines 7–8 | "INC 90.00° ECC 0.0001", "PER 92.6 min" | 3 |
| Body and SOI | ORBIT line 1 | "EARTH  SOI --:--" (the time to the next SOI change) | 3 (body); 4 (SOI) |
| Attitude indicator | ATTITUDE | a plot of the whole sky in body axes, centred on the nose (+x), −z up, azimuthal equidistant (the rim is the tail, 180°): prograde ○ with a tick, retrograde ⊗, normal and anti-normal ▲ ▼, radial out and in ■ □, the target ◆, the hold's direction as a cyan cross; the dominant body as a filled disc of angular radius asin(R / r) about the nadir, so its limb is a curve; 10° rings faint | 3 |
| Angular rates | ATTITUDE right edge | three bars (pitch, yaw, roll), full scale ±5°/s, green within the warp limit (0.01 rad/s) | 3 |
| Attitude mode | ATTITUDE bottom line; buttons | "HOLD PRO  SETTLED", "DAMP", "DIRECT", "FINE"; the active hold's button lit amber while slewing, green settled | 3 |
| Throttle | quadrant | lever, ENG lamp (white while firing); SHIP line 4 "THR 100 %  ENG ON" | 3 |
| Propellant | quadrant gauge; SHIP line 1 | needle 0–100 % with a red arc below 5 %; "PROP 27,547 kg 98 %" | 3 |
| Δv remaining | SHIP line 2 | "DV 6.61 km/s" (rocket equation at the stack's mass) | 3 |
| Mass and thrust-to-weight | SHIP line 3 | "MASS 35.5 t  ACC 2.87 m/s²" | 3 |
| Load or berth | SHIP line 5 | "LOAD 50.0 t", "BERTHED <place>", "XFER 10 kg/s" while refuelling | 3 |
| Warp | dot-matrix line 2; WARP lamp | "WARP ×1000 ACT ×490", "PAUSED"; the lamp steady white above ×1 | 3 |
| Clock and date | dot-matrix line 1 | "2040-01-01 00:00:00" (UTC) | 3 |
| Alarms and messages | lamps; dot-matrix line 3; COMMS log | the lamp of the alarm and MSTR; the message's text scrolling on line 3 (marquee, 12 characters per second) and logged on COMMS | 3 |
| Burn | AUTOPILOT, plan mode | "BURN 1/2 T-12:30", "DV 56.2 m/s  20 s", "TOTAL 112 m/s 905 kg", "LEFT 27.1 t  ARR 02:14" | 3 |
| Target | TARGET, grapple stowed | name; "RNG 12.345 km"; "CLS +10.2 m/s" (closing positive); relative velocity in body axes "F +1.2 R −0.3 D +0.0"; "CA T-03:12 0.84 km" | 3 |
| Grapple range and alignment | TARGET, grapple armed | the camera image with a crosshair and the fixture's pin marked; over it "RNG 12.4 m CLS 0.12 m/s", "OFF Y+0.05 Z−0.10", "ANG 2.1° ROT 0.1°/s", each green inside its capture tolerance, amber outside (`decision 33: Grapple tolerances, contact and berthing`); "LATCHED" | 3 |
| Orbit plot | AUTOPILOT, plot mode | the orbit in its plane seen from its normal side: the body's disc, the predicted trajectory cyan with Ap/Pe marks, the ship's mark, the target's orbit projected into the plane in magenta, burns as marks and the path after them dashed; scaled to the apoapsis | 3 |
| Autopilot presets | AUTOPILOT, menu mode | the list and goal values of "The autopilot" ("Picking a preset"), the selection inverted | 3 |
| More alarms | lamps PROP, WARP, RCS, AUTO | low propellant amber; warp refused or dropped: WARP flashes 3 s; rotation over the warp limit and RCS off above ×4: RCS amber; autopilot abort: AUTO flashes red | 3 |
| Sun | SHIP lines 7–8; hull gauge; TEMP lamp | "SUN 0.98 AU 1410 W/m²", "HULL 268 K"; the gauge with an amber arc 350–400 K, red 400–450 K, hatched beyond; TEMP amber at 350 K, red at 400 K, the time to 450 K on line 8 then | 4 |
| Life support | SHIP line 6; LIFE lamp | "LIFE 176 d"; amber at 30 days, red at 7 | 4 |
| Radio and messages | COMMS; MSG lamp | the channels of "Messages and radio" | 5 (`milestone 3` shows the job card empty and ship messages only) |

- Lamps in detail: MSTR flashes red with any red alarm and lights amber with any amber one, until acknowledged (a click on it or Y); PERI red while a periapsis below the crash radius or an impact is predicted (the alarms of "Dying and what follows"); PROP amber below 5 % or below the planned burns' need; TEMP and LIFE from `milestone 4` (unlit before); WARP, RCS and AUTO as in the table; GRPL white armed, green latched, flashing amber 3 s on a missed capture; MSG flashing white while a radio message is unread.
- In `milestone 3` the TEMP and LIFE lamps and the hull gauge are fitted but dark (the gauge's needle at its stop), so the cockpit mesh does not change in `milestone 4`.
- Cost, roughly: the cockpit's meshes and layout ~400 lines; the screens' contents ~500; picking and buttons ~150; the grapple camera ~100 (a second, small scene pass); text and ribbons are shared with the overlay and the map.

### Ghostty
- Cut (`decision 36`). The terminal look stays: the COMMS screen is a monospace grid with a block cursor, lines typed out at 60 characters per second and scrolling, drawn with the same Slug text as every screen.
- Why: no screen runs a program or a shell, so a VT engine has nothing to emulate; the game generates every line itself. The cost is real: `luv/ghostty` needs libghostty-vt and the full dev shell, not the `slim` one the sandbox test is built with (`decision 2: Screenshots and GPU test: offscreen capture on a hidden canvas, headless on lavapipe`); drawing its grid in the world means copying Luvcraft's terminal wall (~2,150 lines, `docs/luv-capabilities.md` "HUD and instruments"); and `decision 3: luv ASDF systems the game depends on` would need extending. `milestone 3`'s "terminal screens" are an example of the look, which the COMMS screen gives.

### Interaction
- Every function is a key ("Controls", `decision 29: Control scheme: key map, control modes, gamepad`); the cockpit mirrors the ones a pilot reaches for as buttons, pressed with the left mouse button. The pointer's ray from the eye is intersected on the CPU (double) with every clickable rectangle in eye coordinates; the nearest hit wins; the button under the pointer gets a thin outline. While the right button is held (head look), clicks are ignored.
- The button block, three rows of six:
  - PRO [1], RETRO [2], NML [3], ANML [4], RAD+ [5], RAD− [6];
  - TGT [7], ATGT [8], RPRO [9], RRET [0], INRT [R], DAMP [T];
  - FINE [F], GRPL [G], XFER [B], AP [Return / Backspace: engages a shown plan, disengages a running one], WRP− [,], WRP+ [.].
- Two keys join the map in "Controls" (from its free keys): B, propellant transfer on/off while berthed at a depot (the switch of `decision 26: Refuelling and economy`); Y, acknowledge (the master alarm, and marks radio messages read).
- Screens: a click on a line of the AUTOPILOT menu selects it (as Up/Down); the wheel over the COMMS screen scrolls its log (the last 200 lines) instead of zooming; a click on the COMMS screen marks messages read.
- Not clickable: the throttle lever (Shift/Control, Z, X; dragging it is cut), the warp ×1 (slash), the targets (] [ \), view keys.
- Flying from the cockpit alone (`milestone 3`): every instrument of "Flight", "Instruments" has a place in the table above; the ATTITUDE plot replaces seeing the ship from outside, the AUTOPILOT plot replaces the map for orbits around one body, the grapple camera replaces the outside view for the last 100 m (the grapple's tip is 55° below the window's line of sight). `tests/cockpit.lisp` checks that every instrument of the "Flight" table maps to a cockpit field; `milestone 3`'s screenshots include a rendezvous and capture flown in the cockpit view.
- What the outside view adds: the stack and its load seen whole, the approach's geometry (the fixture against the station's other parts), the Earth below; it keeps the minimal readout of `milestone 2` and adds, from `milestone 3`, the latest message line and the autopilot panel in the overlay. The map adds the predicted trajectory at true scale, targets far away, and other bodies.

### Readability at 1280 × 800
- At the default 70° vertical field of view a unit of size at 1 m covers 400 / tan 35° = 571 pixels; the rules are set there (at 90° everything is 30 % smaller; zooming in makes it larger).
- Text: cap height ≥ 10 px, so ≥ 0.0175 × d metres at distance d (13 mm at 0.75 m, 20 mm on the lamps at 1.15 m); cells 0.86 × cap wide and 1.6 × cap high; the grids of the layout table follow from this and the panel sizes with 10 mm margins. Text never shrinks to fit: a long value is cut with "…".
- Lines and plot strokes ≥ 2 px, markers ≥ 8 px; dot-matrix dot pitch ≥ 2 px (2.45 mm at 0.70 m).
- Contrast, measured on the final tone-mapped image: screen text ≥ 7:1 against its background, lamp lit against unlit ≥ 5:1, tape labels ≥ 4.5:1 in Earth's shadow (lit by the ceiling strips). Screens, lamps and LEDs are emissive with their output divided by the current exposure ("Sun glare and brightness" in `docs/luv-capabilities.md`), like screens with a light sensor, so they read the same in sunlight and shadow; sunlight on a screen adds a wash of at most 1/7 of the text's luminance (the hoods). The sunlit patch on the lining may blow out; it never covers a screen fully, thanks to the hoods and the angles.
- Colour, with the same meaning on screens, lamps, the overlay and the map: normal text paper white (#E8E4D8) on near-black (#0A0C0C), labels grey (#8A8A80); green (#40D040) in tolerance, engaged, latched; amber (#FFB000) caution; red (#FF3030) danger; cyan (#40D0FF) the own ship; magenta (#E040E0) the target. The dot-matrix strip is one colour (amber LEDs) and carries no colour meaning.
- Never colour alone: a caution value carries "!", a danger value is inverted (red block, dark text) and flashes at 2 Hz, every lamp has its label, every marker its shape.

### Messages and radio
How the world talks to the pilot; `docs/story.md` uses these channels by name (`decision 37: Message channels the story may use`). Code: `cockpit.lisp` draws them, `quests.lisp` (`milestone 5`) writes RADIO and JOB.
- **RADIO**: a message from a person or station: sender (call sign), game time, text of any length. It appears in the COMMS log (newest at the bottom, typed out at 60 characters per second, the sender in cyan for the pilot's own employer or white for others), MSG flashes until read (Y, a click on COMMS, or COMMS in the middle of the view for 1 s), dot-matrix line 3 shows "MSG <SENDER>", the overlay of the outside and map views shows the first line; from `milestone 6` static and a chirp ("Sound"). Arrival is delayed by the light time from the sender's position, so Earth's messages reach the Moon 1.3 s and Mars minutes late; text only, no reply choices (a linear quest line).
- **JOB**: the current quest step's orders, the job card: a title and up to three lines (goal, place, deadline if any) at the top of COMMS, kept until the step is done or replaced; a one-line form in the overlay. A quest step may require Y to acknowledge its RADIO message before it counts as given.
- **SHIP**: the flight computer's own messages (alarms, warp refusals and drop-outs, autopilot notices, "NO CAPTURE …") on dot-matrix line 3 and in the COMMS log in amber; written by the game, not by the story.
- **Static texts**: the ship's name above the window, and up to three tape notes on the console (e.g. a previous owner's warnings), chosen in `docs/story.md`, fixed for the game.
- Cut, with the reason: a printer strip for orders (a 58 mm till roll at 0.6 m holds about six readable characters per line at 1280 × 800, so it could be read only zoomed in); recorded voices ("Sound"); faces on video calls (portrait art costs far more than it shows, pillar "small and finished"); an inbox with folders (the log and the job card are enough for a linear quest line).

## The places

Where the ship can go and what it sees on the way: Earth, five points of interest in Earth orbit, the Moon with a propellant depot, small asteroids near Earth, the planets and the Sun. Each place has its orbit, its purpose, its look and the Δv to reach it, checked against the ship's 6.65 km/s ("Δv for the standard legs"); what the ship cannot reach, or not come back from, is shown in the sky and on the map but is not a destination. The near-Earth economy of the early 2040s is invented but plausible: crew and propellant in a polar low orbit, a high depot that turns captured rock into hydrogen and oxygen, and a depot in lunar orbit fed by polar ice. The lasting choices are `decision 38: Start orbit: polar` (superseding `decision 23: Start orbit`), `decision 39: Places in scope` (its asteroid-marker priority superseded by `decision 42: Scope: what each milestone builds`) and `decision 40: Earth's look and drag`. Code: `places.lisp` (definitions, rails elements, fixtures, collision shapes), `bodies.lisp` (bodies), `impostors.lisp` (Earth, Moon, planets, Sun), `meshes.lisp` (stations, depots, rocks), per `docs/architecture.md` "Files in load order".

### One plane for near-Earth space
- Every Earth-orbit place and the start orbit are polar (i = 90°) with the same right ascension of the ascending node, Ω = 270° (`decision 38: Start orbit: polar`).
- Why: Earth's J2 (`decision 17: Gravity model`) turns orbit planes at Ω̇ = −(3/2) n J2 (R⊕/p)² cos i. At 51.6° that is −5.00°/day at 400 km, −4.75°/day at 500 km and −4.09°/day at 800 km, so places at different altitudes drift apart by up to 0.9°/day, and a rendezvous a week later would cost a plane change of hundreds of m/s. At i = 90° cos i = 0: every polar orbit keeps its plane at every altitude, so the places stay coplanar for the whole game and their presets cost only altitude and phasing. In the game's world this is the reason the economy settled in one polar plane; launches come from high-latitude sites, as polar launches do today.
- What the player sees: a polar orbit passes over both poles and every latitude; with the Sun near the plane (β = −10° at the start) each orbit has a day and a night side. The Sun's angle to the plane (β) changes by up to 1° per day through the year, so in some weeks the orbit stays sunlit (|β| above ~70° at 400 km) and in others it crosses the shadow every orbit: the seasons of a polar orbit.
- Rails (`decision 19: Time-warp rules`): the places move on Kepler elements with J2's secular rates; at i = 90° only ω and M drift. The Moon's and Sun's pull is ignored on rails (it tilts Highport's high orbit by a fraction of a degree per month); a promoted place is integrated with the ship's force model, as the architecture says.
- Epoch of every element below: the start date (2040-01-01 00:00 UTC, `decision 14: Units and time epoch`, `decision 50: Story: start date and the passing rock's date`); Earth orbits against Earth's equator (= the ICRF xy-plane), lunar orbits against the Moon's equator (`docs/architecture.md` "Frames"). u₀ is the argument of latitude at the epoch, M₀ the mean anomaly.

### Earth
- Surface: the Blue Marble Next Generation 5400×2700 (`decision 13: Asset storage and attribution`) as a ray-traced impostor (`docs/luv-capabilities.md` "Spheres and planet surfaces"); the month of the start date (January: `world.200401.3x5400x2700.jpg`, 1.8 MB, snow and ice of a northern winter; not the `world.topo.bathy.200401` variant with shaded relief and sea floor). One month for the whole game; seasonal swaps are cut (the game spans months, the textures would cost 1.8 MB each for a change few notice). `milestone 2`.
- Atmosphere: an additive rim on the lit limb (as in `docs/luv-capabilities.md` run 32) and a halo past the limb from a second impostor of radius R⊕ + 100 km (the crash sphere's radius, so the visible air and the deadly height agree), blue on the day side, reddened along the terminator by the light's path length; no scattering model. `milestone 2`.
- Night side: city lights from NASA's Black Marble 2016 (NASA Earth Observatory, "Earth at Night"): the 3 km colour map `BlackMarble_2016_3km.jpg` (13500×6750, 7.7 MB) resampled to 5400×2700 like the day map, a JPEG of ~1.0 MB (`decision 13`: ≤ 5 MB; the derivation goes in `assets/earth/SOURCE.md`), credited "NASA Earth Observatory images by Joshua Stevens, using Suomi NPP VIIRS data from Miguel Román, NASA GSFC"; added on the night side only, faded across the terminator; in, `milestone 3`, "should". Why: half of every orbit is night, and a black disc tells the pilot nothing; the lights show where the ground is and how fast it moves.
- Clouds: a static cloud layer, NASA Visible Earth "Blue Marble: Clouds" (published 2002-02-11): `cloud_combined_8192.tif` (8192×4096, 34 MB) resampled to a 5400×2700 greyscale JPEG of ~3.5 MB, credited "NASA Goddard Space Flight Center Image by Reto Stöckli"; mixed over the surface in the same impostor shader (white, a slight offset shadow on the ground); fixed to the ground, not animated; in, `milestone 3`, "should". Why: Earth without clouds looks like a map from orbit; one texture and a few shader lines make it look like a planet. Animated weather is cut (no data, no model).
- Cut, with the reason (pillar "small and finished"): ocean glint (needs a land/water mask and a specular term; a "could" for `milestone 6`), aurorae, lightning, terrain relief (a 400 km view shows none worth its data).
- Drag: cut, as `decision 17` has it (`decision 40: Earth's look and drag`). At 400 km the air's density is 2.80×10⁻¹² kg/m³ (U.S. Standard Atmosphere 1976; it varies several-fold with solar activity); with C_d = 2.2, the Mule broadside (54 m²) and 36 t, drag is ½ ρ v² C_d A / m = 2.7×10⁻⁷ m/s², 0.023 m/s per day, a 42 m drop in altitude per day (Δa = 2 a Δv / v): invisible in the game's weeks. Drag matters only below ~200 km, where Earth's crash sphere at 100 km already ends the flight and the autopilot's floor is 150 km ("The autopilot"). Without drag no orbit decays on its own; objects released below the crash radius re-enter by the rule in "The derelict's re-entry" below.

### Earth-orbit points of interest
Five places, all invented (no real object of the early 2040s is known: the ISS partners have committed to operating the station through 2030, after which NASA's US Deorbit Vehicle, contracted to SpaceX in June 2024, brings it down; and no real station or depot sits in this plane), built from the tug's parts and the other ships' (`decision 27: Other ships`). Each is a `milestone 3` autopilot preset, "Rendezvous: <place>" (`decision 32: Autopilot scope and safety rules`); `docs/story.md` keeps their names (`decision 49: Story: names, call signs and ships`). Δv from the start orbit by Hohmann transfers (no plane change: one plane); phasing (the target's position along the orbit) adds a few m/s in the autopilot's 48 h search, or waiting.

| Place | Purpose | Orbit (i 90°, Ω 270°, epoch the start) | Fixtures | Mass | Δv from the start |
|---|---|---|---|---|---|
| Tern Station | crew station, the pilot's home port: berth, restock consumables, crew capsules arrive | 420 km circular, u₀ = 20° (2,370 km ahead) | 1 berth | 85 t | 11 m/s (+ phasing) |
| Fulmar Depot | LEO propellant depot, filled by tankers from Highport: berth, refuel | 500 km circular, u₀ = 180° | 2 berth | 60 t dry | 56 m/s |
| Derelict K-17 | a dead upper stage, the debris company's (Lindgren Debris, `docs/story.md`) job: grapple, push to re-entry | 800 km circular, u₀ = 270°; spinning 0.3°/s about its long axis | 1 towable, on the axis | 2.5 t | 217 m/s |
| Highport | high depot and water plant: rocks become LH2/LOX; berth, refuel, restock | 1,000 × 70,000 km (a = 41,878.1 km, e = 0.82382), ω = 336°, M₀ = 180° (at apogee); period 23.7 h | 3 berth | 300 t dry | 2,762 m/s |
| Skua Yard | rock yard beside Highport: bagged rocks waiting for processing, the rock mover parked | Highport's orbit, M₀ = 179.87° (30 km behind it) | 1 towable per rock | rocks 20–100 t, one 150 t | 2,762 m/s (+ < 5 m/s from Highport) |

- Tern Station: three pressurised modules Ø 4.2 × 8 m in a line along the orbit (white, stencilled), a node at the front with a crew capsule berthed (`decision 27`), a truss across with two solar wings 8 × 25 m and two radiators 4 × 10 m; the berth fixture on the zenith face of the last module. Collision shape: four boxes (the module line with node and capsule, the truss with the radiators, the two wings), "Rigid bodies: attitude, mass, shapes and fixtures". Lights: red/green at the wing tips, a strobe, the berth fixture lit white. Its 420 km is 20 km above the start, so the first rendezvous is the smallest lesson.
- Fulmar Depot: four tanks on a 30 m truss (two LH2 Ø 6 × 14 m, two LOX Ø 4.5 × 6 m, foil-wrapped), a gold-foil sunshade disc Ø 16 m at the sunward end (Sun-pointing: it turns once a year), cryocooler radiators, two berth fixtures on short arms (one for the player, one with an uncrewed tanker berthed, `decision 27`). Its propellant is unlimited (no economy, `decision 26: Refuelling and economy`).
- Derelict K-17: a Centaur-class stage, cylinder Ø 3.05 × 12.7 m with one engine bell, grey and scorched, its insulation peeling (normal noise); on the payload adapter ring at the front, a grapple fixture bolted on by an earlier uncrewed servicer of the debris company (the fixture is new and bright on an old stage). The 0.3°/s spin about the axis keeps the fixture's axis still, so a capture needs no matched rotation, but the stage visibly turns. Collision shape: one box.
- The derelict's re-entry: the job is to push the stage from 800 km to a perigee of 90 km, release it at apogee, and raise the tug's own perigee. A released object on rails whose orbit meets Earth's crash sphere is removed at that point ("burned up"), integrated or on rails (`docs/architecture.md` "Removal of objects", `decision 46: Rails across spheres, hyperbolas and re-entry`): 2,603 s after the release at apogee. The autopilot refuses the push (its floor is 150 km, "The autopilot"): this job is flown by hand with the PERI alarm on, the game's lesson in reading the periapsis. Δv: push 196 m/s with the 36.7 t stack (1.6 t of propellant), then 89 m/s to raise the tug's perigee back to 400 km and 109 m/s to circularise there.
- Highport: a 60 m truss frame carrying the water plant (electrolysis, a box 6 × 6 × 10 m with radiators 6 × 20 m), four large tanks, two solar wings 20 × 40 m, three berth fixtures on arms (other Mule tugs berthed at two of them). It is high so that rocks arrive cheaply: a rock captured from a passing orbit needs ~0.5 km/s to stay here, but 2.8 km/s more to reach LEO, which no loaded tug has ("Mass budget and Δv": 1.74 km/s with 50 t). Its apogee points to right ascension ~90°, declination ~+24°, where the Moon crosses the polar plane once a month, so a TLI from its perigee costs 370 m/s in the best month's window ("The Moon"; the best of 2040 has the Moon there on 2040-01-26, the TLI near 2040-01-21, `docs/story.md` "Start date").
- Skua Yard: five bagged rocks in nets (spheres at 2,000 kg/m³: 20 t Ø 2.7 m, 35 t Ø 3.2 m, 50 t Ø 3.6 m, 80 t Ø 4.2 m, 100 t Ø 4.6 m, and one of 150 t, Ø 5.2 m, which the grapple refuses: `decision 33: Grapple tolerances, contact and berthing`), spaced 300–800 m apart, each with a towable fixture on its net (plain spheres in `milestone 3`, the asteroid meshes below from `milestone 4`); the rock mover (`decision 27`) parked 2 km away with a bagged rock Ø 10 m. Moving a rock to Highport's water plant fixture is a hop of 30 km in a 23.7 h orbit: one phasing orbit at < 0.1 m/s, then the approach on RCS, a few m/s in total.
  - Layout: the yard is a string along Highport's orbit, every object on Highport's a, e, i, Ω, ω with its own M₀, so the gaps keep their times like THRIFT's and TANKER 3's (below). Spacing 666.7 m at apogee, so that the autopilot's aim point 1 km behind any object ("The autopilot") falls in the middle of a gap, 333 m from both neighbours (the terminal path from there would pass the neighbour behind, so near apogee the autopilot comes in radially instead, "Rendezvous: search, phasing and the approach"); the gaps scale with speed like the ships' (6.9 km at perigee). Offsets at apogee from the yard's reference point (M₀ = 179.87°), + towards Highport; ΔM = n Δt, Δt = offset / 958.9 m/s (the apogee speed, vis-viva), n = 4.2210×10⁻³ °/s (period 23.69 h):
    - Skua Yard 3 (50 t) +2,000 m, M₀ = 179.87880°; at the front, so the first tow (`docs/story.md` quest 6) leaves forwards into the empty lane to Highport.
    - POD 14 (12 t) +1,333 m, M₀ = 179.87587° (below).
    - Skua Yard 5 (100 t) +667 m, M₀ = 179.87293°.
    - Skua Yard 1 (20 t) 0 m, M₀ = 179.87000°.
    - Skua Yard 2 (35 t) −667 m, M₀ = 179.86707°.
    - Skua Yard 4 (80 t) −1,333 m, M₀ = 179.86413°.
    - Skua Yard 6 (150 t, refused) −2,000 m, M₀ = 179.86120°; beside the rock mover, whose rock it is (`docs/story.md`).
    - The rock mover −4,000 m (2 km behind the last rock), M₀ = 179.85239°.
    - In the order the quests take them (3, POD 14, 5), each job's object is the front one when its quest starts, and every tow leaves forwards; the passing rock (`docs/story.md` quest 9) is released in the front slots, empty by then.
  - POD 14 (`milestone 5`, with the quests; `docs/story.md` "Names and terms"): a standard 12 t cargo pod ("Rigid bodies: attitude, mass, shapes and fixtures") of electrolyser spares, waiting in the yard for Peary Depot (`docs/story.md` quest 7). Elements: Highport's a = 41,878.1 km, e = 0.82382, i 90°, Ω 270°, ω 336°, M₀ = 179.87587° (1.39 s, 1,333 m at apogee, ahead of the yard's reference; 28.2 km behind Highport at apogee). Attitude inertial, no spin: the LVLH attitude at the epoch turned 180° about z, so its fixture (+x) faces back along the orbit at every apogee, towards the autopilot's aim point 1 km behind it; its +z (nadir at the epoch) points at Earth near each apogee.
    - Clearances: 667 m from Skua Yard 3 and Skua Yard 5 (bounds 2.9 and 3.4 m; the pod's 3.1 m), inside the rocks' 300–800 m spacing; its aim point (+333 m) 333 m from Skua Yard 5 and Skua Yard 1; the aim point of Skua Yard 3 (+1,000 m) 333 m behind the pod. The lane to Highport starts at Skua Yard 3, ahead of the pod; Highport's own approach, 1 km behind it, is 27 km ahead of the pod at apogee.
- Ships parked in Highport's orbit between trips (`docs/story.md` "Other ships"; `milestone 5`, "should", with "Story ships moving between places"; not drawn if that is dropped): THRIFT, a Mule, at M₀ = 180.0220° (5 km ahead of Highport at apogee, 5.21 s ahead in time), and TANKER 3, an uncrewed tanker, at M₀ = 180.0088° (2 km ahead, 2.09 s); same a, e, i, Ω, ω as Highport (`decision 51: Story ships flying free: THRIFT, TANKER 3, BAGGER 3`). Their trips (Highport ↔ Peary Depot, Highport ↔ Fulmar Depot) are `milestone 5`'s, with elements changed beyond 200 km (`decision 27: Other ships`).
  - Sharing Highport's orbit, they keep their time gap; the gap in metres scales with the speed: smallest at apogee (959 m/s: 2 and 5 km), 6.4 and 16.1 km at r = a, 20.7 and 51.8 km at perigee (9,926 m/s). Skua Yard, 31.3 s behind, is 29.5 km away at apogee.
  - Ahead, not behind, so they stay clear of every approach: the autopilot aims 1 km behind its target on the target's orbit ("The autopilot"), and its terminal phase then stays within 1 km of Highport down to the free fixture's axis; TANKER 3, at least 2 km from Highport at every point of the orbit, is at least 1 km from that path and 3 km from the aim point. Skua Yard's aim point is 31 km behind Highport, and a rock towed from Skua Yard closes on Highport from behind. The free fixture's axis (−x, anti-sun) is near the orbit's apse line in January (β −10°) and points along the velocity only where it is 6.4 km (TANKER 3) or 16.1 km (THRIFT) from Highport.
- Not places: GEO (5,872 m/s from the polar start orbit with the 90° plane change at apogee, 1.5 t left; no work there in the story), Earth–Moon Lagrange points and a near-rectilinear halo orbit (they need three-body motion, and places move on Kepler rails, `decision 19`), satellite constellations as traffic (thousands of objects for dots nobody can work with).

### The Moon
- Body: the Moon from Meeus's ELP-2000/82 series (`decision 18: Ephemerides`), μ and mean radius 1,737.4 km per `decision 14: Units and time epoch`, a sphere with its IAU rotation (the near side faces Earth, `decision 15: Coordinate frames`); crash sphere at R + 0 (`decision 34: How death works`). `milestone 2` puts it in the sky (a grey Lambert sphere, untextured) and in the force model; `milestone 4` textures it and makes it a destination.
- Surface: the 2019 colour map of NASA's Scientific Visualization Studio "CGI Moon Kit" (SVS ID 4720, from the LRO Camera's Hapke-normalised Wide Angle Camera mosaic): `lroc_color_poles_4k.tif` (4096×2048, 24-bit, 12.5 MB) committed as a JPEG of ~2.0 MB at high quality (`decision 13`: ≤ 5 MB); credited "NASA's Scientific Visualization Studio (CGI Moon Kit; LRO LROC WAC)", as the page asks ("Please give credit for this item to: NASA's Scientific Visualization Studio"), no NASA logo. Its LOLA elevation map `ldem_4.tif` (1440×720, float, 4.0 MB, committed as is; or `ldem_4_uint.tif`, 2.0 MB) for a normal map, so the terminator shows craters' relief. NASA's media guidelines cover it by name: "texture maps and polygon data in any format – generally are not subject to copyright", use in "computer graphical simulations" allowed, NASA acknowledged as the source. The kit also has a 2025 colour map (16-bit TIFFs, the 4k one 59 MB, better poles); the 2019 map is chosen because it is 8-bit and verified here, the 2025 one may replace it in `milestone 4` with the same credit. The 16k colour (`lroc_color_poles_16k.tif`, 178 MB) and `ldem_16.tif` (5760×2880, 63 MB) through a pinned Nix fetch, the committed versions as fallback (`decision 13`), are a "could". `milestone 4`.
- What is seen: from LEO the Moon is 0.52° wide, 6 pixels at the 70° field of view (21 at 20°); from 100 km above it the surface fills the view and the 4k map's 2.7 km per pixel looks soft at nadir, sharp towards the horizon.
- Peary Depot (`milestone 4`, invented): a propellant depot fed by water from the polar craters' ice (the landers that bring it are not shown); berth, refuel, restock. Orbit: Moon-centred, 100 km circular (r = 1,837.4 km), i = 90° against the Moon's equator, Ω = 0°, u₀ = 0°; period 117.8 min, speed 1,634 m/s. Looks like Fulmar Depot with two tanks and one berth fixture. A real 100 km lunar orbit needs station-keeping against the Moon's mascons; the game's Moon is a point mass (`decision 17: Gravity model`), so it needs none.
- Δv ("Δv for the standard legs"): TLI 3,082 m/s, and LOI into 100 km 901 m/s from the polar plane: the Moon crosses that plane at right angles, so v∞ = √(v_Moon² + v_t²) = √(1.0245² + 0.1976²) = 1.04 km/s at 384,400 km (0.99–1.10 over the Moon's distances, LOI 880–925 m/s), not the coplanar 0.83 km/s and 823 m/s of the standard legs (`decision 55: Meeting 2040 BQ7 and the lunar arrival from the polar plane`, "Rendezvous: search, phasing and the approach"). So LEO → Peary Depot 3,983 m/s (6,613 kg left); from Highport's perigee in the January window TLI 376 + LOI 897 (a 4.9° plane change included) = 1,273 m/s (9,017 kg; 18,983 kg left), corrected from the coplanar 370 + 823 = 1,193. Return: TEI and capture into 400 km 3,983 m/s, or into Highport's orbit ~1,273 m/s, its phasing at Highport free. Transfer time ~5–6 days (the half ellipse to 384,400 km takes 4.99 days, to 405,900 km 5.41).
- Round trips: LEO → Peary Depot (refuel) → LEO, each leg 4.0 km/s; or LEO → Highport (refuel) → Peary Depot (refuel) → Highport. A round trip without a lunar refuel fails: 3.98 + 3.98 = 7.97 km/s > 6.65.
- Landing: cut (`decision 34`, "Not in 'Flight'"): the Mule has no legs, the RL10 at TWR 0.3–1.3 against 1.62 m/s² would need a lander's guidance and the surface a terrain model; pillar "the ship is a tool".
- Other lunar orbits: none as places; the autopilot captures into any circular lunar orbit or one with a set apoapsis ("The autopilot").

### Asteroids
The ship's work with asteroids is near Earth, where its Δv and 180 person-days reach: rocks already brought in (Skua Yard) and one small rock captured as it passes Earth. Real asteroids farther out are on the map, not destinations.
- The passing rock (`milestone 4`, "should"; its quest is `docs/story.md`'s): 2040 BQ7, an invented small near-Earth asteroid, Ø 4.0 m, 1,500 kg/m³ (a carbonaceous, water-bearing body), 50 t; designation in the Minor Planet Center's style, found 2040-01-19, perigee 2040-03-12 12:00 UTC (`decision 50: Story: start date and the passing rock's date`). Invented because rocks this small are mostly undiscovered today: a real one with a known 2040s flyby cannot be named. Bagged by the company's uncrewed bagger probe (BAGGER 3, scenery on rails), so it has a towable fixture. It passes Earth on a hyperbola with v∞ 0.5–1.0 km/s and perigee at 7,378 km (Highport's perigee radius), in the polar plane.
  - The job: from Highport's orbit, a Lambert intercept leaving shortly after a perigee pass, meeting the rock ~1.5 days before perigee near 325,000 km and matching its inbound velocity (2,010 m/s in all leaving 2040-03-05, 1,683–2,289 for 03-03 to 03-06 at v∞ 0.75 km/s; `decision 55: Meeting 2040 BQ7 and the lunar arrival from the polar plane`, "Rendezvous: search, phasing and the approach"), grapple, fall with it to perigee and capture the stack into Highport's orbit (480–516 m/s with 50 t over v∞ 0.5–1.0, 495 at 0.75), the capture split around one phasing revolution so the stack arrives at Skua Yard's drop point. Check: after 2,040 m/s empty from full tanks, 14.7 t of propellant are left, which gives the 58 t stack 996 m/s: twice the capture's need. The autopilot does each leg (`milestone 4`: the intercept, the capture with its phasing); the grapple is the pilot's.
  - Rails: the rock is on heliocentric elements outside Earth's sphere of influence and on a geocentric hyperbola inside it; a rails object switches bodies at the sphere's boundary (`docs/architecture.md` "Hyperbolic rails" and "Rails across spheres of influence", `decision 46: Rails across spheres, hyperbolas and re-entry`).
  - BAGGER 3, the probe that netted it (`docs/story.md` "Other ships"): on the rock's own rails, shifted in time: its state at t is the rock's at t + Δt, Δt = 1 km / v(400,000 km), v = √(v∞² + 2μ⊕/r): 0.668 s at v∞ 0.5 km/s, 0.626 s at 0.75, 0.578 s at 1.0. So it rides 1 km ahead of the rock at 400,000 km, 1.09 km where the tug meets it (325,700 km at v∞ 0.75), 0.3–0.6 km ahead far out, ~6–7 km ahead at perigee. It stays on those rails when the rock is grappled: it passes perigee and leaves Earth on the rock's outbound hyperbola, out of view.
  - The rock's fixture axis (+x) is held inertially against the rock's velocity at 400,000 km inbound (the bagger left it so): the autopilot's aim point, 1 km behind the rock, lies on the fixture axis, and BAGGER 3 is ~1 km on the far side of the rock, ~2 km from the aim point and ~1 km from the approach; by the meeting near 325,000 km the path has turned only 0.6°. Over the last 1.5 days' fall to perigee the path turns, the axis does not; the grapple comes early in it.
- Real asteroids on the map (`milestone 4`, "should", raised from "could" by `decision 42: Scope: what each milestone builds` so the belt is in the sky): markers with labels and their orbits as 1 px lines, on Kepler rails around the Sun from JPL Small-Body Database elements; no meshes (none is reached):
  - (469219) Kamoʻoalewa, Earth's quasi-satellite, 40–100 m (H 24.3), possibly lunar ejecta (Sharkey et al. 2021, Communications Earth & Environment), China's Tianwen-2 target (launched 2025-05-28 UTC); by the elements below it stays 0.04–0.45 AU from Earth over 2040–2049.
  - The belt's four largest, (1) Ceres, (4) Vesta, (2) Pallas, (10) Hygiea.
  - Elements from the JPL Small-Body Database API (read 2026-10-14), heliocentric, ecliptic and equinox J2000 (rotated to ICRF by the obliquity, as Standish's, `decision 18`), osculating at epoch JD 2461200.5 TDB (2026-06-09); `places.lisp` takes them to the nearest 10⁻⁶ AU and 10⁻⁴°. Propagated as two-body Kepler orbits from 2026 into the 2040s they drift from the truth by planetary perturbations (largest for Kamoʻoalewa near Earth), invisible on a map line; no object is reached. 2008 EV5 is listed for the Δv scan only, not drawn.

| Object | a (AU) | e | i (°) | Ω (°) | ω (°) | M (°) |
|---|---|---|---|---|---|---|
| (469219) Kamoʻoalewa | 1.000810 | 0.102239 | 7.8026 | 65.5932 | 304.3632 | 243.3871 |
| (1) Ceres | 2.765553 | 0.079692 | 10.5880 | 80.2486 | 73.2942 | 274.4193 |
| (2) Pallas | 2.769559 | 0.230700 | 34.9328 | 172.8866 | 310.9699 | 254.2497 |
| (4) Vesta | 2.361366 | 0.090204 | 7.1439 | 103.7013 | 151.4686 | 81.1902 |
| (10) Hygiea | 3.150974 | 0.106709 | 3.8295 | 283.1199 | 312.4242 | 252.0344 |
| (341843) 2008 EV5 | 0.959767 | 0.082891 | 7.4479 | 93.1879 | 235.9306 | 332.5012 |

- The belt itself is not drawn as a field: its objects are kilometres to hundreds of kilometres apart, and the real belt is empty to the eye (`docs/luv-capabilities.md` "Instancing many objects"); the map may draw its extent (2.1–3.3 AU) as a faint band ("could").
- Cut as destinations, with the reason (the Δv by single-revolution Lambert transfers between Standish's Earth and the SBDB elements above, departures 2040-01-01 to 2048-12-31 on a 4-day grid, transfers 20–400 days, at least 12 days at the object; impulsive, departing from and captured into Highport's orbit at its perigee with the apse assumed aligned, so optimistic; a throwaway script, re-run 2026-10-14):
  - Kamoʻoalewa: out from Highport ≥ 1.9 km/s (1.90, of it 0.48 to match its velocity), but a round trip within 360 days costs 4.9 km/s via Highport (departing late 2044: 112 days out, 12 there, the rest home), within 180 days 8.2 km/s; the cheapest takes ~2 years (3.9 km/s). The cab carries 180 person-days ("Parts"); a trip that kills the pilot halfway home is no destination.
  - The main belt: 9.7 km/s for departure and circularisation at 2.77 AU and 472 days one way ("Δv for the standard legs"): beyond both limits.
  - Other real near-Earth asteroids, e.g. 2008 EV5 (~400 m), the Asteroid Redirect Mission's boulder target: its synodic period with Earth is 15.7 years (period 343.4 days), and the decade's one window opens at the game's start: a 180-day round trip leaving Highport on 2040-01-01 costs 6.43 km/s, from 2040-01-17 already 7.0, from February 2040 on ≥ 8.4; the cheapest within 360 days is 6.35 km/s (leaving June 2040). The ship starts in LEO, 2.76 km/s and a refuelling away from Highport, and an impulsive 6.4 km/s leaves nothing for gravity losses (a few per cent) against 6.65: beyond one tank with a return.
- The asteroid meshes (`meshes.lisp`, `milestone 4`): displaced icospheres, a few shapes at random scales and rotations, instanced (`docs/luv-capabilities.md` "Instancing many objects"); dark grey (albedo 0.05 for C-type rocks), the nets as a darker lattice, the fixture plate bright.

### Planets
- All eight planets on Standish's elements (`decision 18: Ephemerides`), with their system μ in the Sun frame's force model from `milestone 4` (`decision 17`). None is a destination.
- Cut as destinations, with the reason: Venus needs 3,460 m/s to depart and 146 days one way, with no propellant to come back; Mars 3,569 m/s plus 2,070 m/s to capture (5.6 km/s of 6.65) and 259 days one way, beyond the 180 person-days; the outer planets more. A tug's planets are a sequel's (pillar "the ship is a tool"). The autopilot offers no planetary transfer; the crash radii of Venus and Mars (`decision 34`) stay for completeness, as a manual flight can still head there.
- How they look: from anywhere the ship can be, each is below a pixel (Venus at its closest, 0.277 AU, is 60″ wide: 0.19 px at the 70° field of view; Jupiter 49″, Mars 25″), so in the views they are point sprites like the stars, with brightness from distance and phase (a Lambert sphere with each planet's geometric albedo) and a flat colour. In the map, zoomed in, they are impostors with a flat albedo colour and Lambert shading; Saturn's rings as a flat annulus are a "could" (`milestone 6`). Textures are cut: nobody sees them outside a map zoom, and each would be another asset to license.
- `milestone 4`: rails, gravity, sprites, map focus, markers and orbits as 1 px lines ("Views").

### The Sun
- Body: fixed at the heliocentric origin (`decision 18`), radius 6.957×10⁵ km (IAU 2015 B3, `decision 14`), crash sphere R☉.
- Look: an impostor disc with linear limb darkening (I/I₀ = 1 − 0.6 (1 − cos θ)), a radiance far above the scene's, so the bloom chain makes its glare (`docs/luv-capabilities.md` "Sun glare and brightness"); 0.53° wide at 1 AU, growing as 1/d. The exposure follows it: looking near the Sun the stars fade. `milestone 2` (it lights everything).
- The danger zone, from the hull-temperature rule (`decision 34: How death works`; "Dying and what follows"): warning 350 K at 0.551 AU, danger 400 K at 0.415 AU, death 450 K at 0.326 AU. Mercury's orbit (0.307–0.467 AU) lies inside it; Venus's (0.718–0.728 AU) does not. On the map, from `milestone 4`, three circles around the Sun in the ecliptic: amber at 0.551 AU, red at 0.415 AU and 0.326 AU (hatched inside), labelled "350 K", "400 K", "450 K"; the cockpit's TEMP lamp, hull gauge and SUN line ("The cockpit") give the warnings in flight; inside 0.551 AU a warm tint grows on the cab's window glass with the hull temperature.
- Reachable, on purpose: a Hohmann-like dive from Earth to a perihelion of 0.551 AU needs v∞ 4.68 km/s (4,143 m/s from LEO, 1,473 from Highport's perigee; 125 days to perihelion); to the deadly 0.326 AU v∞ 8.90 km/s (6,360 m/s from LEO on full tanks, 3,758 from Highport; 99 days). So a determined pilot can fly into the Sun by hand, and `milestone 4`'s test of death by heat is a flight script with that burn. The autopilot plans nothing inside 0.551 AU (`decision 32`).

### Rigid bodies: attitude, mass, shapes and fixtures
What `places.lisp` gives each object for `physics.lisp`'s rigid-body type and `hazards.lisp`'s contact test (`docs/architecture.md` "Rails" and "Collisions and the Sun", `decision 44: Contact, capture and berthing in the simulation`). Figures from the parts below by a throwaway script, not committed; the lasting choices are `decision 47: Rigid bodies of the places` and, for the story's free-flying ships, `decision 51: Story ships flying free: THRIFT, TANKER 3, BAGGER 3`.
- Body axes per object: origin at its centre of mass (CoM), x forward, y right, z down as for the ship (`decision 15: Coordinate frames`); positions in m from the CoM; a box is centre and size (x × y × z), a sphere centre and radius.
- Inertia: the full symmetric tensor about the CoM in body axes, from the parts by the parallel-axis rule as in "Inertia": solid cylinder ½ m r² (axis), m (3r² + L²)/12 (across); thin-walled cylinder m r², m (6r² + L²)/12; thin ring m r², ½ m r²; box m (b² + c²)/12 per axis; sphere (2/5) m r². Off-diagonal entries are tensor elements (I_xy = −Σ m x y). Berth objects never use theirs (immovable, kinematic attitude); it is listed for completeness and a later change of rule.
- Attitude modes of berth objects (kinematic, `docs/architecture.md` "Rails"), with ω from the mode's closed form for a berthed ship's velocity:
  - LVLH: x along the local horizontal in the direction of motion, z to the body's centre (nadir), y = z × x (orbit anti-normal); ω = h / r² about −y (0.065°/s at 420 km).
  - Sun-pointing: x towards the Sun, z the ecliptic south pole's direction made perpendicular to x, y = z × x; turns ~1° a day.
  - Inertial: a fixed quaternion in `places.lisp`, the object's LVLH attitude at the epoch.
- Towable objects spin about their fixture axis (+x, a principal axis), at most 0.4°/s: the pin and the fixture's axis stand still, so a capture needs no matched motion, and the spin stays inside the 0.5°/s of `decision 33: Grapple tolerances, contact and berthing` without the pilot rolling with it. The bagger probe that netted a rock leaves it so; K-17's spin happens to be about its axis.
- Fixtures: a point (the pin, on the plate) and an outward axis in body axes. Every plate stands 0.3 m proud of the object's shapes: at the 10° capture limit the front corners of the ship's grapple box (half-diagonal 1.13 m) reach 1.13 sin 10° = 0.20 m ahead of the tip, and would touch a flush face before the tip touched the plate; on a sphere the 0.3 m also covers the 0.30 m lateral offset ((r + 0.1)² + 0.83² > r² for every r).
- Shapes: one to five per object (cost in `docs/architecture.md` "Collisions and the Sun"); thin parts the pilot never flies near (solar wings of berthed or scenery ships, booms, antennas) are left out, as the ship's own wings are. Ships berthed at a place as scenery (`decision 27: Other ships`) are part of its shapes and mesh, not objects of their own. The bounding radius covers the shapes and every pin plus 0.8 m.

| Object | Kind, attitude | Mass (t) | Inertia about the CoM (kg m²) | Shapes (centre; size or radius, m) | Fixtures (pin; outward axis) | Bound (m) |
|---|---|---|---|---|---|---|
| Tern Station | berth, LVLH | 85 | diag(3.59, 6.46, 9.88)×10⁶ | module line (1.33, 0, 0) 32.5 × 4.2 × 4.2; truss and radiators (−2.92, 0, 0) 4.0 × 28.0 × 1.5; wings (−2.92, ±26.5, 0) 8.0 × 25.0 × 0.5 | berth (−10.92, 0, −2.40), −z (zenith) | 39.8 |
| Fulmar Depot | berth, Sun-pointing | 60 (dry) | diag(0.793, 4.65, 5.12)×10⁶ | sunshade (14.22, 0, 0) 0.3 × 16.0 × 16.0; tanks (2.77, 0, 0) 21.5 × 13.8 × 6.5; aft spine and arms (−11.98, 0, 0) 8.0 × 1.5 × 8.0; berthed tanker (−13.98, 0, −16.30) 3.6 × 3.6 × 24.0 | A (−13.98, 0, 4.30), +z, free; B (−13.98, 0, −4.30), −z, the tanker's | 33.7 |
| Highport | berth, Sun-pointing | 300 (dry) | I_xx 1.21×10⁸, I_yy 1.60×10⁷, I_zz 1.07×10⁸, I_xz 2.4×10⁴ | truss and tanks (0.02, 0, −4.84) 6.5 × 60.0 × 6.5; water plant and radiators (0.02, 0, 13.41) 6.0 × 6.0 × 30.0; wings (0.02, ±50.0, −4.84) 0.5 × 40.0 × 20.0; arms and two berthed Mules (0.02, 0, −20.84) 10.0 × 44.0 × 19.5 | plant (−3.28, 0, 3.41), −x, free (refuelling, rock delivery); arms (0.02, ±20.0, −11.39), −z, Mules' | 72.6 |
| Peary Depot | berth, Sun-pointing | 42 (dry) | I_xx 4.43×10⁵, I_yy 3.53×10⁶, I_zz 3.74×10⁶, I_xy −4.28×10⁵, I_xz 3.1×10⁴, I_yz 1.6×10³ | sunshade (15.16, −0.65, −0.06) 0.3 × 16.0 × 16.0; tanks (3.71, −0.65, −0.06) 21.5 × 13.8 × 6.5; aft spine and arm (−11.04, −0.65, 1.94) 8.0 × 1.5 × 4.0 | berth (−13.04, −0.65, 4.24), +z | 26.5 |
| Rock mover | fixed (as berth, immovable), inertial | 1,087 | I_xx 1.07×10⁷, I_yy 2.18×10⁷, I_zz 2.19×10⁷, I_xz −7.9×10⁴ | trusses, engines, grapples (−16.61, 0, 0.01) 23.8 × 7.2 × 3.6; cab (−8.56, 0, −3.49) 4.5 × 3.0 × 3.0; rock sphere (0.59, 0, 0.01) r 5.0 | none (its rock's fixture holds its own grapples) | 29.2 |
| Derelict K-17 | towable, 0.3°/s about +x | 2.5 | diag(4.89×10³, 5.65×10⁴, 5.65×10⁴) | stage and bell (−1.05, 0, 0) 14.2 × 3.05 × 3.05 | (6.35, 0, 0), +x | 8.5 |
| Cargo pod (standard) | towable, no spin | 12 | diag(1.25, 2.22, 2.22)×10⁴ | (0, 0, 0) 4.0 × 2.5 × 2.5 | (2.30, 0, 0), +x | 3.1 |
| POD 14 (a standard cargo pod, Skua Yard) | towable, inertial, no spin (fixture aft along the orbit at apogee) | 12 | as the standard pod | as the standard pod | as the standard pod | 3.1 |
| THRIFT (free-flying Mule) | fixed (as berth, immovable), inertial | 36.0 (wet) | I_xx 8.79×10⁴, I_yy 4.48×10⁵, I_zz 4.21×10⁵, I_xz 2.0×10⁴ | truss, tanks and engine (−3.84, 0, 0.22) 17.0 × 3.6 × 3.6; cab (2.41, 0, −3.28) 4.5 × 3.0 × 3.0; grapple (5.46, 0, 0.22) 1.6 × 1.6 × 1.6 | none | 12.6 |
| TANKER 3 (free-flying tanker) | fixed (as berth, immovable), inertial | 54.6 (full) | I_xx 7.80×10⁴, I_yy 2.36×10⁶, I_zz 2.36×10⁶ (I_xz 5×10², negligible) | truss, tanks and engine (−3.75, 0, −0.01) 26.2 × 3.6 × 3.6 | none (its nose fixture is for depot arms) | 17.0 |
| BAGGER 3 (bagger probe) | fixed (as berth, immovable), Sun-pointing | 0.5 | diag(294, 181, 292) | bus (0, 0, 0) 1.5 × 1.5 × 1.5 | none | 1.3 |

- Tern Station, parts (reference: the middle module's centre; the CoM is 2.92 m ahead of it): three modules 20 t each, solid cylinders Ø 4.2 × 8 at x = −8, 0, 8; node 6 t, Ø 4.2 × 4 at x 14; crew capsule 9 t, Ø 3.9 × 4.5 at x 18.25; truss 4 t, 1.5 × 28 × 1.5 across at the middle module; radiators 2 × 1 t, 4 × 10 m on the truss at y ±8; wings 2 × 2 t, 8 × 25 m at y ±26.5. The berth fixture sits on the zenith face of the aft module: the approach comes down the radius vector (an R-bar approach, as to the ISS), and a berthed Mule stands 19 m tall above the station.
- Fulmar Depot, parts (reference: the 30 m spine's centre, x to the Sun): spine 8 t, 30 × 1.5 × 1.5; LH2 tanks 2 × 12 t, Ø 6 × 14 from x 0.5 to 14.5 at y ±3.9; LOX tanks 2 × 5 t, Ø 4.5 × 6 at x −4, y ±3.9; sunshade 3 t, a disc Ø 16 at x 15.2, wide enough to shade the tanks' outer edges at 6.9 m; cryocooler radiators 2 × 3 t, fins 6 × 2.5 at x −4, z ±2; berth arms 2 × 1 t along ±z at x −13; avionics and power 7 t at the aft end. The tanker berthed at B (`decision 27`) is drawn and collides as part of the depot; its mass is not counted.
- Peary Depot: Fulmar Depot with one LH2 tank (+y), one LOX tank (−y) and one berth arm (+z), 42 t; built for four tanks, the tanks' box covers the two empty cradles.
- Highport, parts (reference: the truss centre; x to the Sun, the wings face it): truss frame 40 t, 6.5 × 60 × 6.5 along y; four tanks 4 × 12.5 t, Ø 6 × 12 along y inside the frame at y ±7 and ±21; water plant 120 t, 6 × 6 × 10 below the truss (+z); its radiators 2 × 10 t, 6 × 20 m fins hanging below the plant, edge-on to the Sun; wings 2 × 15 t, 40 × 20 m at y ±50; berth arms 3 × 2 t; power, avionics and rock handling 34 t at the centre. The free fixture is on the water plant's anti-sun face: tugs berth there to refuel and bring rocks, in the plant's shadow, lit by their own floodlight. The two arms on the −z side hold the berthed Mules.
- Rock mover, parts (reference: the trusses' aft end): two trusses 2 × 2,842 kg (20 m of the Mule's truss each, side by side at y ±1.8); two engines 2 × 190 kg; a Mule cab 2,466 kg on top at x 15.5–20; tanks with half propellant 2 × 15 t, solid cylinders Ø 3.04 × 17; two grapples 900 kg; the bagged rock Ø 10 m at 2,000 kg/m³, 1,047 t, its centre at 21.6 + 0.3 + 5.0 m. Parked in Skua Yard, holding its attitude: the pilot can bump it but never grapple it.
- Story ships flying free (THRIFT, TANKER 3, BAGGER 3; `docs/story.md` "Other ships"): immovable like the rock mover, with no fixture the player can use, so a bump never moves them off the rails the story's trips depend on (`decision 27: Other ships`). Inertial attitude as the rock mover's (the LVLH attitude at the epoch); the bagger probe Sun-pointing, its wings facing the Sun. Thin parts (wings, booms) left out of the shapes, as elsewhere.
- THRIFT, parts: the player's Mule ("Parts") with full tanks, ready for its next trip; tensor from the "Inertia" table's wet row, shapes the ship's three boxes ("Grappling and berthing"), the truss box from x = −2.0 to 15.0 m from R, about the wet CoM (10.34, 0, −0.22).
- TANKER 3, parts (reference R: the truss's aft end on its axis): truss 24 m, 16 bays of 1.5 (4 longerons × 24 m, 17 frames, 64 diagonals, as the Mule's +15 %), 3,365 kg; the Mule's engine 190 kg on the axis at x −0.7; two tank pairs Ø 3.2 with hemispherical domes, LH2 7.60 m (910 kg) at x 0.50–8.10 and 12.20–19.80, LOX 3.30 m (512 kg) at x 8.50–11.80 and 20.20–23.50, walls and masses as the Mule's tanks scaled by area; the Mule's RCS blocks at x 0.5 and 23.5, gas conditioning, radiators, solar wings and equipment bay; cabling 320 kg; nose fixture 50 kg at x 24.15. Dry 7,654 kg; full 46,973 kg (3,613 kg LH2, 19,873 kg LOX per pair at MR 5.5, 3 % ullage), wet 54,626 kg, CoM (14.65, 0, 0.01); Δv full 4,410 ln(54,626 / 7,654) = 8.67 km/s, so a Highport → Fulmar Depot round trip (2 × 2.76 km/s) delivers ~15 t.
- BAGGER 3, parts: bus 478 kg (structure 120, avionics, battery and radio 80, four RCS blocks 68, tank 20, propellant 160, net canister 30) as a uniform box 1.5 m; two wings 13 kg each, plates 0.03 × 2.0 × 1.0 centred at y ±2.0. Mass 504 kg at launch, listed as such (immovable, its propellant left does not matter).
- Derelict K-17, parts (reference: the tank cylinder's centre): tank shell 1,750 kg, thin-walled Ø 3.05 × 12.7; engine and thrust structure 300 kg, Ø 1.0 × 2.2 at x −6.75 (the bell 1.5 m out of the aft end); payload adapter ring and avionics 400 kg, ring r 1.4 at x 6.1; fixture and stand-off 50 kg. Its spin axis is the axis of least inertia, steady in torque-free motion. Captured by a wet Mule, its angular momentum (26 N m s) gives the stack 0.02°/s of roll.
- Cargo pod: a standard 12 t box (food, parts, equipment) with one fixture on an end; no place carries one in `milestone 3` or `4`; `docs/story.md` places one, POD 14 in Skua Yard (`milestone 5`, "Earth-orbit points of interest"), with the same definition.
- Rocks: spheres of radius r = (3m / 4πρ)^(1/3) (ρ 2,000 kg/m³; the passing rock 1,500), tensor (2/5) m r² 𝟙, one fixture at (r + 0.3, 0, 0), +x, on the net's frame; net and fixture are inside the stated mass and left out of the tensor (< 0.5 % of it). Spin about +x:

| Rock | Mass (t) | r (m) | I (kg m²) | Fixture pin (m) | Spin (°/s) | Bound (m) |
|---|---|---|---|---|---|---|
| Skua Yard 1 | 20 | 1.34 | 1.43×10⁴ | 1.64 | 0.10 | 2.44 |
| Skua Yard 2 | 35 | 1.61 | 3.63×10⁴ | 1.91 | 0.20 | 2.71 |
| Skua Yard 3 | 50 | 1.81 | 6.58×10⁴ | 2.11 | 0.15 | 2.91 |
| Skua Yard 4 | 80 | 2.12 | 1.44×10⁵ | 2.42 | 0.25 | 3.22 |
| Skua Yard 5 | 100 | 2.29 | 2.09×10⁵ | 2.59 | 0.05 | 3.39 |
| Skua Yard 6 (refused) | 150 | 2.62 | 4.11×10⁵ | 2.92 | 0.30 | 3.72 |
| The passing rock | 50 | 2.00 | 7.97×10⁴ | 2.30 | 0.40 | 3.10 |

- Checked against `decision 33`: every spin is about the fixture axis and ≤ 0.4°/s (≤ 0.5°/s); every free fixture's axis leads out into open space, clear of the place's other shapes for the Mule's 18.8 m from grapple tip to nozzle; berth objects turn at most 0.065°/s (Tern's LVLH). Against `decision 39: Places in scope`: the same places and objects, nothing added but the standard cargo pod's definition, the story's free-flying ships (`decision 49: Story: names, call signs and ships`), which are scenery, not places, and POD 14, a load in Skua Yard, not a place.
- The Mule's loaded rows in "Inertia" put the load's centre at 16.6 + 0.3 + r, its fixture plate at the grapple's tip.

### Δv checked against the ship
Every destination in scope against the 6,650 m/s of full tanks (and 1,738 m/s with 50 t), from "Δv for the standard legs" and the sections above; "left" is propellant after the leg from full tanks.

| Trip | Δv (m/s) | Left (kg) | Reachable |
|---|---|---|---|
| Start → Tern Station | 11 (+ phasing) | ~27,900 | yes |
| Start → Fulmar Depot | 56 | 27,547 | yes |
| Start → Derelict K-17, push to re-entry (2.5 t), back to 400 km | 217 + 196 + 89 + 109 | ~23,200 | yes |
| Start → Highport or Skua Yard | 2,762 | 11,265 | yes; refuel at Highport |
| Highport → LEO | 2,762 | 11,265 | yes; a loaded tug cannot (1,738 m/s with 50 t), so rocks stay at Highport |
| Skua Yard → Highport with 50–100 t | < 5 | — | yes |
| LEO → Peary Depot (TLI + LOI) | 3,983 | 6,613 | yes; refuel there to return |
| Highport → Peary Depot, best window | 1,273 | 18,983 | yes |
| Peary Depot → LEO, or → Highport | 3,983, ~1,273 | 6,613, 18,983 | yes, from a full depot refuel |
| Highport → passing rock, capture with 50 t into Highport's orbit | 1,683–2,289 empty (2,010 leaving 2040-03-05; + 30 mid-course and terminal), 495 loaded (v∞ 0.75 km/s) | 13,294–16,425 after the empty legs (14,682 leaving 03-05) | yes: 911–1,100 m/s with 50 t against 565 |
| LEO → Sun warning zone (0.551 AU) / death (0.326 AU) | 4,143 / 6,360 | — | yes by hand, never by the autopilot |
| GEO from the start | 5,872 | 1,536 | yes, but not a place |
| Venus, Mars, Kamoʻoalewa, the main belt | see above | — | no (no return, or longer than 180 person-days): map only |

### Sources
Real data and assets of this section, each with the URL and the date it was read; the asset files' sizes were checked by downloading them, the JPEG sizes by converting them (`task 47: Verify the sourced figures and asset licences of docs/design.md "The places"`). NASA's Earth Observatory and Visible Earth pages moved to `science.nasa.gov` in 2025–2026; the file URLs below are the ones that answered on the date read.
- Body constants: `docs/architecture.md` "Units" (`decision 14`): JPL DE440 (Park et al. 2021), IAU 2015 B3, IAU WGCCRE 2015.
- Planet and Earth–Moon barycentre elements: E. M. Standish, "Keplerian Elements for Approximate Positions of the Major Planets", JPL Solar System Dynamics, Table 1 (`decision 18`; as in `docs/architecture.md` "Ephemerides").
- The Moon's position: J. Meeus, "Astronomical Algorithms", 2nd ed., Willmann-Bell 1998, ch. 47 (`decision 18`).
- NASA media licence: "Guidelines for using images and media", `https://www.nasa.gov/nasa-brand-center/images-and-media/`, read 2026-10-14 (quoted in `docs/luv-capabilities.md`; names "texture maps and polygon data in any format" and "computer graphical simulations").
- Earth, day: Blue Marble Next Generation, NASA Earth Observatory, `https://science.nasa.gov/earth/earth-observatory/blue-marble-next-generation/base-map/`; January file `https://assets.science.nasa.gov/content/dam/science/esd/eo/images/bmng/bmng-base/january/world.200401.3x5400x2700.jpg` (1,884,678 bytes), read 2026-10-14 (`decision 13`).
- Earth, night: "Black Marble: Flat Maps", NASA Earth Observatory, `https://science.nasa.gov/earth/earth-observatory/earth-at-night/maps/`; `https://assets.science.nasa.gov/content/dam/science/esd/eo/images/imagerecords/144000/144898/BlackMarble_2016_3km.jpg` (13500×6750, 8,106,233 bytes; the 3600×1800 `BlackMarble_2016_01deg.jpg`, 779,638 bytes, is the unresampled alternative); credit from "Night Light Maps Open Up New Applications", `https://science.nasa.gov/earth/earth-observatory/night-light-maps-open-up-new-applications-90008/`; read 2026-10-14.
- Earth, clouds: "Blue Marble: Clouds", NASA Visible Earth, `https://visibleearth.nasa.gov/images/57747/blue-marble-clouds` (now redirecting; read in the Internet Archive's copy of 2024-01-20), file `https://eoimages.gsfc.nasa.gov/images/imagerecords/57000/57747/cloud_combined_8192.tif` (35,870,468 bytes; `cloud_combined_2048.jpg`, 829,367 bytes, the small alternative), read 2026-10-14.
- Atmospheric density at 400 km: "U.S. Standard Atmosphere, 1976", NOAA/NASA/USAF, NASA-TM-X-74335, `https://ntrs.nasa.gov/citations/19770009539`; 2.803×10⁻¹² kg/m³ computed with its model (the `pyatmos` package's `coesa76`, 2026-10-14), its tables being scanned.
- The Moon's surface: "CGI Moon Kit", NASA Scientific Visualization Studio (visualizer Ernie Wright, USRA; scientist Noah Petro, NASA GSFC), released 2019-09-06, updated 2026-01-09, `https://svs.gsfc.nasa.gov/4720`; files under `https://svs.gsfc.nasa.gov/vis/a000000/a004700/a004720/`: `lroc_color_poles_4k.tif` (13,095,388 bytes), `ldem_4.tif` (4,153,352 bytes), `lroc_color_poles_16k.tif` (187,004,094 bytes); read 2026-10-14.
- Asteroids: JPL Small-Body Database API, `https://ssd-api.jpl.nasa.gov/sbdb.api?sstr=<number>&full-prec=1` for 469219, 1, 2, 4, 10 and 341843, read 2026-10-14 (orbit solutions dated 2021-04-13 to 2026-09-03); B. N. L. Sharkey, V. Reddy, R. Malhotra et al., "Lunar-like silicate material forms the Earth quasi-satellite (469219) 2016 HO3 Kamoʻoalewa", Communications Earth & Environment 2, 231 (2021-11-11), doi:10.1038/s43247-021-00303-7, checked at Crossref 2026-10-14.
- Tianwen-2's launch, 2025-05-28 (UTC) on a Long March 3B from Xichang: Wikipedia, "Tianwen-2", `https://en.wikipedia.org/wiki/Tianwen-2`, read 2026-10-14.
- The ISS's end: NASA, "NASA Selects International Space Station US Deorbit Vehicle" (2024-06-26), `https://www.nasa.gov/news-release/nasa-selects-international-space-station-us-deorbit-vehicle/`, read 2026-10-14: operations through 2030, then a controlled deorbit; the reason no real station is a place.

## Scope

The prioritised feature list is the specification of `milestone 2: Flight core: Newtonian flight in near-Earth orbit` to `milestone 6: Release: one command to play, a README for people`: each milestone builds the rows assigned to it, "must" first, then "should", then "could" as time allows; a row assigned to a milestone may be extended by later ones, as the sections above say. Cut rows give the reason, usually a pillar ("The game in one page"). The lasting choices are `decision 41: Weapons: cut` and `decision 42: Scope: what each milestone builds`. The section ends with each milestone checked against its Outcome and Done when, and with what `docs/story.md` may use.

### Feature list
Priority: **must** (the milestone's Outcome or Done when needs it, or the game breaks without it), **should** (plainly worth its cost; dropped only with a recorded reason), **could** (only if time is left; a later milestone may pick it up).

| Feature | Priority | Milestone | Reason, section |
|---|---|---|---|
| Start in a 400 km polar LEO, tanks full, prograde hold, sunlit | must | 2 | the developer's idea; "The start", `decision 38` |
| Integrated flight: RK4, fine and coarse steps, n-body against Earth (J2), Moon, Sun | must | 2 | pillar "Newton is the game"; `decision 16`, `decision 17` |
| Ephemerides: Sun, Earth, Moon (planets 4) | must | 2 | the Moon and Sun pull and are seen; `decision 18` |
| The Mule as a rigid body: parts' mass, CoM, full inertia tensor, one propellant pool | must | 2 | "Parts", "Inertia"; `decision 22` |
| Main engine: throttle 10–100 %, gimbal trimmed through the CoM | must | 2 | "Propulsion" |
| RCS: 24 thrusters, the mixer (torque exact, force as fits) | must | 2 | "RCS forces, torques and accelerations" |
| Key map of `milestone 2` (attitude, translation, throttle, holds 1–6, R, T, F, warp, P, M, C, Home, Tab, Page keys, quit) | must | 2 | "Controls", `decision 29` |
| Mouse: right-drag look, wheel zoom | must | 2 | "Controls" |
| Control modes: direct, rate damping, orbital and inertial holds, fine control | must | 2 | "Control modes" |
| Time warp: eight levels, refusals, drop-outs, thrust limits ×10 / ×100, pause | must | 2 | "Time warp", `decision 19`, `decision 31` |
| Outside view: orbit camera, orbital frame or locked | must | 2 | `milestone 2` Outcome; "Views", `decision 30` |
| Map, basic: Earth, the osculating orbit with Ap/Pe and impact markers, zoom to 100 AU | must | 2 | "Views"; extended in 3 and 4 |
| Minimal readout (overlay): the `milestone 2` rows of "Instruments" | must | 2 | `milestone 2` Outcome |
| Earth: Blue Marble impostor (January), atmosphere rim and halo | must | 2 | `milestone 2` Outcome; "Earth", `decision 40` |
| Stars (HYG ≤ 6.5 mag), the Sun's disc and glare, bloom and exposure | must | 2 | `milestone 2` Outcome; "The Sun", `decision 13` |
| The Moon as a body in the sky, untextured (flat grey Lambert sphere) | must | 2 | it pulls and is seen; the texture is 4's |
| The Mule's meshes, materials and lights | must | 2 | `milestone 2` Outcome; "How they look" |
| Engine glow; navigation lights and strobe | should | 2 | "How they look" |
| Crash into Earth (100 km sphere), exact impact time | must | 2 | the developer's idea; "Dying and what follows", `decision 34` |
| LOST panel, restart from the start state (in memory) | must | 2 | `milestone 2` Outcome; "What follows" |
| Flight scripts `burn`, `raise-orbit`, `deorbit` with screenshots | must | 2 | `milestone 2` Done when; `decision 21` |
| Physics drift tests | must | 2 | `milestone 2` Constraints; `decision 21` |
| Window title listing the main keys | must | 2 | the controls help comes in 6 |
| The DIY cockpit: cab interior, rack, cables, tape labels, window, portholes | must | 3 | the developer's idea; `milestone 3` Outcome; "The DIY look", `decision 36` |
| Cockpit view: eye point, head look, zoom; V toggles views | must | 3 | "Views", `decision 30` |
| Cockpit screens (ORBIT, ATTITUDE, TARGET, AUTOPILOT, SHIP, COMMS) in Slug text and ribbons | must | 3 | flying from the cockpit alone; "Instruments and how each is drawn" |
| Dot-matrix strip, lamps, needle gauges, throttle lever | must | 3 | "Instruments and how each is drawn" |
| 18 cockpit buttons, mouse picking | must | 3 | "Interaction" |
| Grapple camera (render target) on the TARGET screen | must | 3 | the last 100 m from the cockpit; "Grappling and berthing" |
| Sunlit patch through window and portholes | should | 3 | the cab lives with the ship's attitude; "The DIY look" |
| Readability rules and colours | must | 3 | "Readability at 1280 × 800" |
| Full instrument set (`milestone 3` rows of "Instruments") | must | 3 | "Instruments" |
| Trajectory prediction ribbons, targets, closest approach, nodes, planned burns in the map | must | 3 | "Views", `decision 20` |
| Targets: ] [ \, map click, target and relative holds 7–0 | must | 3 | "Controls" |
| Autopilot, Earth orbit: circularise, set Ap/Pe, Hohmann, plane change, rendezvous with phasing orbits and clear approaches, station-keeping, safety rules | must | 3 | the developer's idea; `milestone 3` Outcome; "The autopilot", "Rendezvous: search, phasing and the approach", `decision 32`, `decision 54` |
| Earth-orbit places: Tern Station, Fulmar Depot, Derelict K-17, Highport, Skua Yard, each a rendezvous preset | must | 3 | the developer's idea; `milestone 3` Outcome; "Earth-orbit points of interest", `decision 39` |
| Places on Kepler rails with J2 rates, promotion and demotion | must | 3 | `decision 19` |
| Grapple, capture tolerances, missed capture bounce, release | must | 3 | `milestone 3` Outcome; "Grappling and berthing", `decision 24`, `decision 33` |
| Berthing, refuelling and restocking (B), no economy | must | 3 | "Refuelling and resupply", `decision 26` |
| Towing loads up to 100 t as one stack | must | 3 | the developer's idea (tugs move asteroids); "Towing" |
| Contact with objects: bounce ≤ 1.0 m/s, death above | must | 3 | "Grappling and berthing", `decision 33` |
| Released objects re-enter (the derelict burns up) | must | 3 | the derelict's job; `decision 46` |
| Deliveries: a load released still within 100 m of Highport's plant fixture (Peary Depot's berth in 4) is taken in after 600 s | must | 3 | the yard's rocks go somewhere; "Towing", `docs/story.md` |
| Other ships as scenery at the places (capsule at Tern, tanker at Fulmar, Mules at Highport, rock mover at Skua Yard), on rails | should | 3 | the places' life; "Other ships", `decision 27` |
| SHIP messages on the strip and COMMS; Y acknowledges | must | 3 | "Messages and radio", `decision 37` |
| Checkpoints in memory after a completed plan and on berthing | must | 3 | "What follows", `decision 34` |
| Earth's night lights (Black Marble 2016) | should | 3 | half of every orbit is night; "Earth" |
| Earth's static cloud layer | should | 3 | Earth reads as a planet; "Earth" |
| Autopilot headless test per Earth-orbit preset; a cockpit rendezvous and capture with screenshots | must | 3 | `milestone 3` Done when |
| Moon: LROC colour mosaic and LOLA normal map, Peary Depot (100 km polar) | must | 4 | the developer's idea; `milestone 4` Outcome; "The Moon" |
| Autopilot across SOIs: TLI, capture, TEI and Earth capture; "Rendezvous: Peary Depot" as one chain; the intercept of a target on a hyperbola (with the passing rock, "should") | must | 4 | `milestone 4` Outcome; "The autopilot", "Rendezvous: search, phasing and the approach", `decision 54` |
| Re-centring between Earth, Moon and Sun frames; SOI on the map and ORBIT screen | must | 4 | `decision 15`; "Views" |
| Planets: all eight on rails, gravity in the Sun frame, point sprites, map impostors and orbits | must | 4 | `milestone 4` Outcome (planets shown, none a destination); "Planets" |
| The Sun's heat: hull temperature, TEMP lamp, hull gauge, danger circles on the map, window tint | must | 4 | the developer's idea; `milestone 4` Outcome; "The Sun", `decision 34` |
| Sun death flight script | must | 4 | `milestone 4` Done when |
| Life support: consumables, alarms, death at zero | must | 4 | transfers last weeks; "Dying and what follows" |
| Asteroid meshes (displaced icospheres, nets, fixtures) | must | 4 | Skua Yard's rocks use them; drawn as spheres in 3 until then; "Asteroids" |
| The passing rock: an invented 50 t NEA captured into Highport's orbit, with its bagger probe on its rails | should | 4 | asteroid work beyond Earth orbit; needs hyperbolic and SOI-crossing rails (`decision 46`); "Asteroids" |
| Real asteroids on the map: Kamoʻoalewa, Ceres, Vesta, Pallas, Hygiea | should | 4 | the belt in the sky as `milestone 4` asks, for five markers and lines (raised from "could", `decision 42`); "Asteroids" |
| The belt's extent as a faint band in the map | could | 4 | "Asteroids" |
| Moon textures at 16k through a pinned Nix fetch | could | 4 | the committed 4k is enough; `decision 13` |
| Autopilot headless test per destination (Peary Depot, the passing rock) | must | 4 | `milestone 4` Done when |
| Quest line (`quests.lisp`): steps given, tracked, completed, its checkpoints | must | 5 | the developer's idea; `milestone 5` Outcome; `docs/story.md` |
| RADIO and JOB channels on COMMS, MSG lamp, light-time delay | must | 5 | "Messages and radio", `decision 37` |
| Static texts: the ship's name above the window, tape notes | must | 5 | "Messages and radio" |
| The story's start date, ship name and place names | must | 5 | `docs/story.md`; "The start" |
| Story ships moving between places (element changes beyond 200 km): THRIFT and TANKER 3, parked ahead of Highport between trips | should | 5 | "Other ships at their work"; `decision 27`; "Earth-orbit points of interest" |
| Saves: `checkpoint.sexp`, `resume.sexp` | must | 5 | `milestone 5` Outcome; "Saving", `decision 35` |
| Checkpoint floors per quest step; the stranded offer (CAIRN OPS, Y restores the last checkpoint) | must | 5 | no dead end in the quest line; "Saving", "Dying and what follows", `docs/story.md` |
| POD 14 in Skua Yard, the yard's layout | must | 5 | `docs/story.md` quest 7; "Earth-orbit points of interest" |
| Headless play-through of the whole quest line | must | 5 | `milestone 5` Done when |
| Title screen (Continue, New game, Controls, Credits, Quit) | must | 6 | `milestone 6` Outcome; `title.lisp` |
| Short introduction before the first flight | must | 6 | `milestone 6` Outcome; text from `docs/story.md` |
| Controls help (F1), Escape menu | must | 6 | `milestone 6` Outcome; "Controls" |
| Credits screen from `CREDITS.md` | must | 6 | `decision 13` |
| `README.md`: what it is, what it shows of luv, screenshots, a short recording, how to run, credits | must | 6 | `milestone 6` Outcome |
| `nix run github:rubenmoor/luv-in-space` on x86-64 Linux | must | 6 | `milestone 6` Done when; `decision 6` |
| Performance: 60 Hz steady at 1280 × 800 on the developer's machine, measured and documented | must | 6 | `milestone 6` Outcome; "Performance target" below |
| Sound: hull-borne voices synthesised in Lisp, U toggles | should | 6 | "Sound", `decision 35` |
| Gamepad | could | 6 | "Controls", `decision 29` |
| Ocean glint | could | 6 | "Earth" |
| Saturn's rings in the map | could | 6 | "Planets" |
| Weapons, low-tech or any | — | cut | `decision 41`, "Weapons" below |
| Docking ports, crew transfer, EVA | — | cut | the grapple does the job; `decision 24` |
| Landing on any body | — | cut | no legs, no lander guidance, no terrain; pillar "the ship is a tool" |
| Drag, aerobraking | — | cut | invisible above 200 km, the crash sphere ends lower flight; `decision 17`, `decision 40` |
| Per-part damage, leaks, repairs, hit points | — | cut | `decision 25` |
| Economy, money, trading | — | cut | a linear quest line needs none; `decision 26` |
| Electric drive; storable hypergolic engine | — | cut | not flyable under the ×1,000 thrust limit; less Δv; "Propulsion" |
| Power, boil-off, ullage, slosh, stack flexing simulated | — | cut | pillar "small and finished"; "Parts", "Towing" |
| Other playable ships; other ships thrusting in view | — | cut | one ship done well; rails only, `decision 27` |
| Automatic warp by the autopilot; gravity assists; low-thrust spirals | — | cut | "The autopilot" |
| Destinations: GEO, Lagrange points and NRHO, Venus, Mars, outer planets, Kamoʻoalewa, the main belt | — | cut | no work there, three-body motion, or beyond Δv and 180 person-days; "The places", "Δv checked against the ship" |
| Dense asteroid field, constellations as traffic | — | cut | the real belt is empty to the eye; dots nobody works with |
| Planet textures; Earth's seasonal textures, aurorae, weather, relief; earthlight in the cab | — | cut | below a pixel or seen by few, for an asset or a light each |
| Ghostty terminal screen | — | cut | `decision 36`; "Ghostty" |
| Printer strip, recorded voices, video faces, inbox | — | cut | `decision 37`; "Messages and radio" |
| Save slots, a save key | — | cut | `decision 35` |
| Debris and explosion simulation | — | cut | "Dying and what follows" |
| Dragging the throttle lever with the mouse | — | cut | keys do it; "Interaction" |

### Weapons
- Cut (`decision 41: Weapons: cut`). No ship in the game carries a weapon, and the quest line has no fight.
- Why: pillar "the ship is a tool": a tug's tools are its grapple and its engine; the developer's ideas allow low-tech weapons "if any", and the game centres on piloting. A weapon needs targets that react, which other ships on rails (`decision 27: Other ships`) cannot do, and damage, which the ship does not have (`decision 25: Damage model`); both are large systems for one scene.
- What the story may use instead: danger from the trade itself (a spinning derelict, a heavy load, a long transfer, the Sun), people's conflicts told by RADIO, and the ship's own mass: a careless push or a hard contact is the only violence a tug does.

### Performance target
- 60 frames per second steady at 1280 × 800 on the developer's machine (x86-64 Linux, `decision 6: Target platforms`), in every view and at every warp level, measured over a scripted flight that visits the cockpit, the map and ×1,000,000.
- CPU budget per frame: simulation steps ≤ 4 ms (the cap of `decision 16: Integrator and simulation clock`), trajectory prediction ≤ 1 ms (`decision 20: Trajectory prediction on the frame thread`), the rest of the frame ≤ 6 ms; the GPU within the 16.7 ms frame.
- Every milestone keeps it in mind; `milestone 6` measures and documents it (frame times, the machine, the commands) in `docs/` and the README. lavapipe is a test renderer, not a target.

### Each milestone against its Outcome and Done when
- `milestone 2`: start in LEO, integrated orbits with Earth, Moon and Sun, the rigid ship with RCS and propellant flown by hand, Earth, space, Sun and stars, the outside view, crash into Earth and a restart, the minimal readout; the Done when's raise and deorbit are the flight scripts. The Moon is drawn untextured until 4.
- `milestone 3`: the cockpit, the autopilot within Earth's sphere, five Earth-orbit presets, grappling. Its "terminal screens" are an example, not a requirement: the COMMS screen gives the look without Ghostty (`decision 36`). "Asteroids being towed" are Skua Yard's rocks. The derelict's push to re-entry is flown by hand, since the autopilot's 150 km floor forbids it; its rendezvous is a preset like the others, so every preset is reached by autopilot.
- `milestone 4`: the design wins inside the milestone's intent: the Moon is a destination (Peary Depot); the planets are "in scope" as bodies on real orbits, rendered from point sprites to map impostors, but none is a destination; the asteroid belt is in the sky and on the map (five real asteroids, "should"), not a destination, since its Δv and travel time are beyond the ship ("Δv checked against the ship"). Asteroid work is Skua Yard's rocks (from 3) and the passing rock ("should"); the Sun kills, tested by a flight script. "Every destination in scope" is Peary Depot and, if built, the passing rock.
- `milestone 5`: quests, the story's characters and ships, other ships at their work on rails, saves. Its "low-tech weapons if the story uses them" is settled: none (`decision 41`).
- `milestone 6`: `nix run`, title screen, introduction, controls in game, performance, README; credits and sound join it here.
- No real contradiction with a milestone remains, so no human decision is needed.

### What the story may use
`docs/story.md` (`task 12: write plan: story`) may use these, by the milestone that makes them playable; anything cut above is out.
- `milestone 2`:
  - The Mule tug (`decision 22`): one pilot, a jump seat, 28 t of LH2/LOX, 6.65 km/s empty, 1.74 km/s with 50 t; named PATIENCE by the story.
  - The 400 km polar start orbit and its date (2040-01-01, game time before 2050; `decision 14`, `decision 38`, `decision 50`).
  - Manual flight: burns, attitude holds, time warp; death by crashing into Earth, followed by a restart.
- `milestone 3`:
  - Places: Tern Station (home port, crew), Fulmar Depot (LEO propellant), Derelict K-17 (push to re-entry by hand), Highport (high depot and water plant), Skua Yard (bagged rocks 20–100 t, one 150 t too heavy); names kept by the story (`decision 39`, `decision 49`).
  - Autopilot presets in Earth orbit; rendezvous, grapple, berth, refuel, tow a load up to 100 t, deliver it at Highport's plant (removed as `:delivered`); contact deaths above 1 m/s.
  - Other ships as scenery at the places: Mule tugs, a tanker, a crew capsule, the rock mover.
  - SHIP messages (the flight computer's), not the story's.
- `milestone 4`:
  - The Moon and Peary Depot; TLI, capture, return; a lunar round trip needs a refuel there or at Highport; deliveries at Peary Depot's berth.
  - The passing rock (if built, "should"): 2040 BQ7, a 50 t NEA captured into Highport's orbit, perigee 2040-03-12 (`decision 50`).
  - The planets and the belt's large asteroids in the sky and on the map, never visited; the Sun's heat and death (warning at 0.551 AU); life support of 180 person-days.
- `milestone 5`:
  - Messages: RADIO (sender, text, light-time delay, no reply choices), JOB (the job card), static texts (the ship's name, up to three tape notes) (`decision 37`).
  - Story ships that depart and arrive on rails, out of view (`decision 27`); checkpoints at quest steps, vetoed below a step's Δv floor; the stranded offer; POD 14 in Skua Yard; quest steps may require an acknowledgement (Y).
  - No weapons, no fights, no landing, no money, no EVA.
