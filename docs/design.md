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
- The orbit (`decision 23: Start orbit`): 400 km circular (a = R⊕ + 400 km = 6,778,136.6 m), inclination 51.6°, right ascension of the ascending node 0°, argument of latitude 0° at the start date; period 2π √(a³/μ⊕) = 92.6 min, speed √(μ⊕/a) = 7,669 m/s. RAAN and phase are invented; `docs/story.md` or "The places" may change them.
- The date: `*start-date*` in `units.lisp`, chosen by `docs/story.md`; default 2040-01-01 00:00 UTC (`decision 14: Units and time epoch`). Game time stays before 2050-01-01 (`decision 18: Ephemerides`).
- Sunlit at the start: on 1 January the Sun is near right ascension 281°, declination −23°, so the ship at the ascending node (ICRF +x) is ~80° from the Sun; Earth's shadow at 400 km begins 180° − asin(R⊕/a) = 110° from the Sun.
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

The player flies one ship, a Mule-class tug: a long welded steel truss carrying hydrogen and oxygen tanks, one hydrolox engine at the back, a crew cab on top at the front and a grapple on the nose. Its numbers below are computed from its parts (a throwaway script, not committed); the lasting choices are `decision 22: The player's ship and its propulsion`, `decision 24: Grapple, not docking; towing in`, `decision 25: Damage model`, `decision 26: Refuelling and economy` and `decision 27: Other ships`. The code lives in `ship.lisp` (definition, mass properties), `controls.lisp` (throttle, RCS mixer) and `meshes.lisp` (looks), per `docs/architecture.md` "Files in load order". The class name "Mule" is a working name; `docs/story.md` names the player's ship.

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
- Throttle: 10 % to 100 % or off; RL10s fly at full thrust, but NASA's Common Extensible Cryogenic Engine (CECE, an RL10 derivative, tests 2006–2009) throttled deeply to ~10 %. Ignition and cut-off are instantaneous; restarts unlimited.
- Thrust line: the gimbal sits 0.5 m above the truss axis, between the wet and dry CoM heights; the flight computer trims the gimbal through the current CoM, so the engine gives no torque. Trim angle atan((z_cm − z_g)/(x_cm − x_g)): wet 1.6°, half 0.9°, dry −3.0°, with a 50 t load 1.6° (wet) to 1.2° (dry): inside ±4°. The simulation applies thrust along +x at the CoM; the cosine loss (< 0.14 %) is not modelled.
- Why hydrolox: it gives the Δv for low lunar orbit or a planetary departure on one tank (below), the RL10 is the best documented engine of its size, and water turned into hydrogen and oxygen at depots is the reason tugs move water-bearing rock (the story's economy). Storable hypergolics (Isp ~316 s, e.g. the AJ10-190) would give 4.6 km/s at the same masses; electric drives (~1 N per 100 kW class, Δv of 1 km/s in months at 36 t) cannot be flown even at ×1,000 (`decision 19: Time-warp rules` limits thrust to ×1,000): cut.
- RCS: 24 thrusters of 445 N, Isp 360 s, burning gaseous oxygen and hydrogen gasified from the main tanks (the gas conditioning part), in the spirit of the Integrated Vehicle Fluids concept for ULA's ACES upper stage. Each draws ṁ = 445 / (360 g₀) = 0.126 kg/s at the main mixture ratio, so the ship has one propellant pool, as `decision 16: Integrator and simulation clock` integrates it (ṁ_p = −Σ F_i / (Isp_i g₀)). 1,000 kg of propellant spent through the RCS alone gives 360 g₀ ln(35,961 / 34,961) = 100 m/s at wet mass.
- Sources, named but not yet re-read online (no web access in this task; `task 44: Verify the sourced figures of docs/design.md "The ship(s)"` adds URLs and dates): RL10C-1 data sheet (Aerojet Rocketdyne, now L3Harris); NASA CECE test reports (Glenn Research Center, 2006–2010); GO₂/GH₂ thruster Isp from NASA Lewis/Glenn low-thrust hydrogen-oxygen thruster tests and ULA's IVF papers (Zegler and Kutter); densities from the NIST Chemistry WebBook.

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
| GEO from 51.6° (plane change at apogee) | 4,825 | 23,919 | 17.3 min | 4,081 |
| GEO from an equatorial LEO | 3,854 | 20,954 | 15.1 min | 7,046 |
| TLI (apogee at 384,400 km) | 3,082 | 18,083 | 13.1 min | 9,917 |
| LOI into 100 km circular lunar orbit (v∞ 0.83 km/s) | 823 | — | 4.4 min (full) | — |
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
- Anywhere in LEO, high Earth orbit, GEO (with 1.8 km/s left from 51.6°).
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
| Wet + 50 t | 85,961 | (15.04, 0, −0.09) | 1.55×10⁵ | 1.88×10⁶ | 1.85×10⁶ | −1.7×10⁴ | 0.6° |
| Dry + 50 t | 57,961 | (17.18, 0, −0.14) | 1.22×10⁵ | 8.22×10⁵ | 7.95×10⁵ | −3.4×10⁴ | 2.9° |
| Wet + 100 t | 135,961 | (16.63, 0, −0.06) | 2.98×10⁵ | 2.59×10⁶ | 2.56×10⁶ | −2.9×10⁴ | 0.8° |

- The load in these rows: a sphere of rock at 2,000 kg/m³ (radius (3m / 4πρ)^(1/3): 1.81 m for 50 t, 2.29 m for 100 t) touching the grapple at x = 16.6 m.

### RCS forces, torques and accelerations
- Layout: eight blocks at the truss corners, x = 0.5 m (aft) and 14.5 m (front), y = ±1.8 m, z = ±1.8 m; each block has three nozzles pointing outward: one along x (aft blocks push +x, front blocks push −x), one along y and one along z (each pushing towards the axis).
- The mixer (`controls.lisp`): the player, the autopilot and flight scripts command a body force and torque; the mixer picks duty cycles in [0, 1] for the 24 thrusters (a small bounded least-squares problem, solved per frame) that give the commanded torque exactly when it can, and as much of the force as fits without unwanted torque. Thrusters are throttled by pulse-width over the step, so the integrator sees their mean force.
- Capabilities (wet; dry; wet with 50 t):
  - Translation along ±x: 4 × 445 = 1,780 N: 0.050; 0.224; 0.021 m/s².
  - Translation along ±y or ±z without torque: front and rear forces balanced about the CoM, F_rear = F_front × arm_front / arm_rear, plus the axial nozzles' ±1.8 m arms when the CoM lies ahead of the front blocks: 1,266 N (0.035 m/s²); 1,391 N (0.175 m/s²); ~1,077 N (0.013 m/s²).
  - Pitch and yaw: a couple of two front and two rear lateral nozzles, τ = 2 × 445 × (14.5 − 0.5) = 12,460 N m, independent of the CoM: pitch 0.028 rad/s² (1.6°/s²); 0.063 (3.6°/s²); 0.0066 (0.38°/s²); yaw about the same (I_zz ≈ I_yy).
  - Roll: four lateral nozzles at 1.8 m, τ = 4 × 445 × 1.8 = 3,204 N m: 0.036 rad/s² (2.1°/s²); 0.065 (3.7°/s²); 0.021 (1.2°/s²).
- What it feels like: a 90° turn, accelerating half way and braking half way, takes 2 √(θ/α) = 15 s wet, 10 s dry, 31 s with 50 t; a 1 m/s change of closing speed takes 20–30 s wet, 5 s dry, 77 s with 50 t. Stopping a rotation of 0.01 rad/s (the limit for warp above ×4, `decision 19: Time-warp rules`) takes under 0.4 s wet. The ship feels heavy, more so loaded, which is the point of a tug; key map, rate damping and holds are "Flight"'s.

### Grapple and towing
- One mechanism (`decision 24: Grapple, not docking; towing in`): the nose grapple, a capture cone Ø 1.6 m with three claws, latches onto a standard grapple fixture, a plate with a central pin and three alignment guides. Fixtures are on depots, stations, cargo pods and the nets of bagged asteroids, always facing outward at a known point of the object.
- No docking port and no crew transfer: the pilot never leaves the cab.
- Grappled, the object is part of one rigid stack: combined mass, CoM and inertia tensor (as in the table above), the main engine's trim through the new CoM, the RCS mixer with the new lever arms. Releasing gives both bodies the stack's velocity and rotation at the release point.
- Rated load: 100 t; the grapple refuses heavier fixtures (with 100 t the stack has 1.0 km/s and turns 90° in over 40 s; heavier loads would give the player little to fly).
- How it is flown, the tolerances (closing speed, alignment) and what happens on contact: "Flight" (task 40).

### Refuelling and resupply
- Where: depots only; which depots and their orbits are "The places"' (task 42).
- How (`decision 26: Refuelling and economy`): grappled to the depot's fixture, the pilot sets a cockpit switch; LH2 and LOX flow at 10 kg/s total in the mixture ratio until full or switched off (28 t in ~47 min game time; time warp allowed while grappled and coasting); consumables are topped up at the same time.
- No economy: no money, no prices; propellant is free and the story's employer pays. A linear quest line needs no market, and an economy costs a UI and balancing (pillar "small and finished").

### Damage
- None (`decision 25: Damage model`): the ship is intact or destroyed. No part is damaged, leaks or is repaired; there are no hit points.
- Destroyed means death: a contact harder than the crash rule, a crash into a body, or the Sun's heat past its threshold; the rules and what follows are "Flight"'s (task 40).
- Cut, with the reason (pillars "Newton is the game" and "small and finished"): per-part damage (lost thrusters, leaks, dead screens), over-stress while pushing a load, repair at stations. Each needs effects, cockpit warnings and repairs for an event a careful pilot rarely meets.

### Other ships
Built from the tug's parts so they read as the same technology, and all on Kepler rails (`decision 27: Other ships`; `decision 19: Time-warp rules`): promoted to integration within 100 km of the player and demoted beyond 200 km, but never thrusting in view. A story ship that leaves changes its elements while beyond 200 km. Which ships exist where is for "The places" and `docs/story.md`.

| Kind | Built from | Dimensions (m) | Job |
|---|---|---|---|
| Other Mule tugs | the player's parts, other livery and markings | as the player's | pushing loads, waiting at depots |
| Tanker (uncrewed) | the truss lengthened to 24 m with two LH2/LOX tank pairs, no cab, grapple fixture at the nose, one engine | 24 × 3.6 × 3.6 | carries propellant between depots |
| Crew capsule | a frustum Ø 3.9 → 1.5 m, 2.5 m tall, on a cylindrical service section Ø 3.9 × 2.0 m with two solar wings | Ø 3.9 × 4.5 | crew rotation to stations |
| Rock mover | two trusses side by side (7.2 × 3.6 section, 20 m), two engines, the cab of a Mule, pushing a bagged rock of Ø 8–12 m | 20 + rock | moving large asteroids to high Earth orbit |

### How they look
- Generated meshes in `meshes.lisp` (`docs/architecture.md` "Files in load order"; luv has no mesh library, `docs/luv-capabilities.md` "Spheres and planet surfaces"): tubes as square-section beams or 8-sided cylinders, boxes, cylinders, spheres and hemispheres, frustums; drawn instanced where repeated (truss members, RCS blocks; "Instancing many objects").
- Materials, shaded in Lisp shaders: bare steel (grey, rough, darker weld nodes), crinkled gold or silver MLI foil on the tanks (a normal perturbation by noise), white-painted cab with stencilled registration and hand-painted markings, dark blue-black solar cells, white radiators, a dark nozzle with a lighter throat band.
- Lights: red at −y and green at +y on the front corners, a white strobe on the cab top (blink phase reduced on the CPU, `docs/architecture.md` "Floating-origin pitfalls"), a floodlight and range-finder light on the grapple; the main engine's hydrogen flame is nearly invisible in vacuum: a faint blue glow at the nozzle exit while firing, no long plume.
- The player's Mule as a parts list (the parts table gives positions):
  - Truss: 4 longerons Ø 0.076 × 15.0; 11 square frames of 4 tubes Ø 0.060 × 3.6; 40 face diagonals Ø 0.060 × 3.9.
  - LH2 tank Ø 3.2 × 8.86 and LOX tank Ø 3.2 × 3.73, hemispherical domes, foil-wrapped, visible through the open truss.
  - Cab: cylinder Ø 3.0 × 4.5 on four short struts on the truss top, a flat front with a rectangular window ~1.2 × 0.6 m looking forward along +x over the grapple, a side hatch at +y.
  - Engine: bell (frustum) Ø 1.45 at the exit, Ø 0.3 at the throat, 1.6 long; chamber and turbopumps as a cylinder Ø 0.5 × 0.6 and two boxes, on a 4-strut thrust frame.
  - Grapple: cone Ø 1.6 × 0.8 on a cylinder Ø 0.6 × 0.8; three claws as boxes 0.1 × 0.1 × 0.6.
  - RCS blocks: boxes 0.4 × 0.4 × 0.3 with three small cones (nozzles) each.
  - Solar wings: two flat boxes 2.0 × 6.0 × 0.05 on booms Ø 0.08 × 0.5 at the truss sides.
  - Radiators: two fins 4.0 × 1.5 × 0.05 under the truss; equipment bay and gas conditioning as boxes under it.
- Silhouette: a long open frame with a white box on top at the front and a claw on the nose; from any side it reads "this pushes things".

## Flight

How the ship is flown: controls, control modes, views, instruments, time warp, the autopilot, grappling and towing, dying, saving and sound.

PLACEHOLDER(39): controls, control modes, views, instruments and time warp: `task 39: Design: flight controls, control modes, views, instruments and time warp`.

PLACEHOLDER(40): the autopilot, grappling and towing, dying, saving and sound: `task 40: Design: autopilot, docking and towing, dying, saving and sound`.

## The cockpit

The home-made cockpit inside the cab: its layout, each instrument and how it is drawn, and how messages reach the pilot.

PLACEHOLDER(41): `task 41: Design: the cockpit`.

## The places

Earth, the points of interest in Earth orbit, the Moon, asteroids, planets and the Sun, with real data and Δv checked against "Δv for the standard legs".

PLACEHOLDER(42): `task 42: Design: the places, with real data and Δv checked against the ship`.

## Scope

The prioritised feature list, each feature assigned to a milestone (2 to 6) or cut with the reason, and what `docs/story.md` may use.

PLACEHOLDER(43): `task 43: Design: scope table, read-through and what the story may use`.
