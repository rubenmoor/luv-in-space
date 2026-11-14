# Luv in Space

Luv in Space is a hard sci-fi spaceflight game built on [luv](https://github.com/mbrock/luv), Mikael Brockman's Common Lisp GPU workshop. You fly the Mule, a rugged DIY spaceship, under Newtonian physics from a 400 km polar orbit around the Earth. So far you can fly it by hand or with the autopilot, rendezvous with the five places in Earth orbit, and grapple, berth and tow. The solar system comes next.

## Requirements

You need Nix with flakes. Nix fetches everything else: SBCL, luv, Vulkan (Metal on macOS) and SDL3.
- Platforms: `x86_64-linux`, `aarch64-linux`, `aarch64-darwin`.
- Nix with flakes enabled (`experimental-features = nix-command flakes`).
- A GPU with a working Vulkan driver on Linux.
- luv's binary cache (`https://luv.swa.sh/nix-cache`), declared in `flake.nix`. Accept it when Nix asks, or pass `--accept-flake-config`. Without the cache, luv and its dependencies build from source.

## Run

Run the game straight from the repository, or clone it first.
- From GitHub, without cloning:
  ```sh
  nix run github:rubenmoor/luv-in-space --accept-flake-config
  ```
- From a clone:
  ```sh
  git clone https://github.com/rubenmoor/luv-in-space
  cd luv-in-space
  nix run --accept-flake-config
  ```
- Build only: `nix build`, then start `result/bin/luv-in-space`. The first build takes ~17 s with the binary cache.
- The window opens in the cockpit. Its title lists the main keys.
- Quit with Escape, Control+Q or by closing the window.

## Controls

Keys are given by their position on a QWERTY keyboard; the game uses the same positions on any layout. `docs/design.md` "Controls" has the complete key map.

| Function | Keys |
|---|---|
| Pitch / yaw / roll | W S / A D / Q E |
| Translate forward/back, up/down, left/right | H N, I K, J L |
| Throttle up / down | Shift-left / Control-left |
| Throttle full / cut | Z / X |
| Fine control | F |
| Rate damping | T |
| Hold the current attitude | R |
| Hold prograde, retrograde, normal, anti-normal, radial out, radial in | 1 – 6 |
| Hold target, anti-target, relative prograde, relative retrograde | 7 – 0 |
| Time warp up / down / ×1 | . / , / slash |
| Pause | P |
| Cockpit ↔ outside view | V |
| Map | M |
| Camera frame / reset | C / Home |
| Map focus, zoom | Tab, Page Up / Page Down, mouse wheel |
| Target next / previous / clear | ] / [ / \ |
| Autopilot presets: open, choose, change, plan and engage, cancel | O, Up/Down, Left/Right, Return, Backspace |
| Grapple: arm, stow, release | G |
| Refuel while berthed at a depot | B |
| Acknowledge | Y |
| Quit | Escape, Control+Q |

- Mouse:
  - Hold the right button and drag to look around.
  - The wheel zooms.
  - A left click presses cockpit buttons. In the map, a click targets a marker and a double click focuses it.
- After a crash: Return restarts from the latest checkpoint and N starts a new game.

## Getting started

These steps fly the first rendezvous without touching the throttle.
- Press O to open the autopilot presets.
- Pick a row "RDV <PLACE> <h> H" with Up/Down. Left/Right change the time limit.
- Return plans the trip and a second Return engages it.
- Raise the time warp with `.` while the autopilot flies. Any flight key takes control back.

## Development

Development uses the dev shell and the scripts in `scripts/`. `AGENTS.md` lists every command, and `docs/` holds the design and the architecture.
- Dev shell: `nix develop` (or `direnv allow`).
- Tests: `scripts/test` and `scripts/test-slow`.
- Headless screenshot: `scripts/screenshot out.png`.
- Fly a scripted flight headless: `scripts/fly raise-orbit /tmp/out`.
- Live image for interactive development: `./sly start`, then `./sly play`.

## Credits

The game uses NASA's Blue Marble and Black Marble imagery, the HYG star catalogue and the Monaspace and Iosevka fonts. See `CREDITS.md` for sources and licences.
