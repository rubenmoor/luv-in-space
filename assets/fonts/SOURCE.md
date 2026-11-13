# assets/fonts

The fonts of the cockpit's text: Monaspace Neon for the screens and the name plate, Iosevka Aile Bold for the tape labels and engravings (`decision 13: Asset storage and attribution`, `decision 126: Cockpit structure and view in milestone 3: frames, draw order, Slug text, head look, exposure, cab light, portholes, bezels, tape labels`).

## Files

Copied unchanged from luv's `fonts/` at the revision pinned in `flake.lock` (`6abc9d10ec8991c2cfe92888b84946372d6030b9`, github.com/mbrock/luv); luv's `fonts/equity/` is private and never copied.

- `MonaspaceNeon-Regular.ttf` (494,400 bytes, SHA-256 `1f24f8d194d3ede159942e1f85c55b3399678b47f504579bb82d5fc05400aec7`): screens.
- `MonaspaceNeon-Bold.ttf` (495,956 bytes, SHA-256 `9c880eb53b4dcfc25494fa3c872c8be44a1ca36d09b26bf5d6cf5d2f1ee6f40b`): the name plate, emphasis.
- `IosevkaAile-Bold.ttf` (1,487,684 bytes, SHA-256 `5e1d89693b43bf56560b3c61f658504eeb21a26361e4e277556135640cb9a880`): tape labels, engravings.
- `LICENSE-Monaspace.txt`, `LICENSE-Iosevka.md`: the licence texts as luv ships them.

## Upstream

- Monaspace: github.com/githubnext/monaspace, GitHub Next; version 1.400 by the name table (`Version 1.400 (Monaspace Neon Var)`; the static Regular and Bold instances carry the variable font's name strings; the Bold's outlines are bold: its H stem is 295 units of 2,000 against the Regular's 193). Copyright (c) 2023 GitHub, Reserved Font Name "Monaspace".
- Iosevka: github.com/be5invis/Iosevka, Renzhi Li (Belleve Invis); Iosevka Aile 34.8.0 by the name table. Copyright (c) 2015-2026 Renzhi Li.
- Licence of both: SIL Open Font License 1.1 (https://openfontlicense.org); the fonts are used as they are, not renamed or modified.

## Use

- `font-path` (`src/assets.lisp`): `:mono`, `:mono-bold`, `:label` to these files.
- `src/text.lisp` packs the glyphs of a fixed repertoire (printable ASCII and `° × … ± ² ³ µ Δ ← → ↑ ↓ ○ ⊗ ▲ ▼ ■ □ ◆ ·`) of the three fonts into one Slug atlas; the font files are read at start-up, so they are installed with the other assets.
