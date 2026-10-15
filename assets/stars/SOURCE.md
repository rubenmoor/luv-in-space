# assets/stars

The naked-eye sky: every star of the HYG database with visual magnitude ≤ 6.5 except the Sun, as a table the game reads at start (`decision 13: Asset storage and attribution`, `decision 78: Sky in numbers: stars, Sun, Moon and Earth's look`). The table is a derived work of HYG and shares its licence, CC BY-SA 4.0, given in full in `LICENSE`; the game's code is not affected.

## hyg-6.5.sexp

- Source: the HYG database v4.4, https://codeberg.org/astronexus/hyg (README: "Current version: HYG v4.4").
- File: `data/hyg/CURRENT/hyg_v44.csv.gz` at commit `53e3df311869e813ace5f1ad2ec4ce909f13256c` (2026-07-12), a Git LFS file, fetched from https://codeberg.org/astronexus/hyg/media/commit/53e3df311869e813ace5f1ad2ec4ce909f13256c/data/hyg/CURRENT/hyg_v44.csv.gz
- Fetched: 2026-10-14.
- SHA-256 of `hyg_v44.csv.gz` (13,636,362 bytes): `00b349893b9a53106dd488d8371e8d2fa586043e500bb3cdb8bff3931682197d`; of the CSV inside (119,614 stars): `ea0e1699a1e7d48daa197b5fe567fcf447420e367f37619862ee1d6f077599cd`.
- Licence: Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0), per the repository's README and `data/hyg/CURRENT/LICENSE`; `LICENSE` here is the legal code from https://creativecommons.org/licenses/by-sa/4.0/legalcode.txt.
- Credit: "HYG database by David Nash (astronexus)".
- Derivation: `scripts/convert-stars` (needs network) downloads the file into a temporary directory, checks the SHA-256 above, unpacks it and runs `scripts/convert-stars.lisp`:
  - keeps rows with `mag` ≤ 6.5 but the Sun (`id` 0, "Sol"): 8,920 stars;
  - sorts them by magnitude, then HYG `id`;
  - writes one line per star, `(hip ra-deg dec-deg mag ci)`: Hipparcos number (`nil` for the 50 stars without one), J2000 RA in degrees (HYG's hours × 15) and Dec, six decimals; visual magnitude and B−V colour index, three decimals (`ci` `nil` when HYG has none: 40 stars); exact decimal arithmetic, so runs give identical files.
- Size: 369,131 bytes; SHA-256 `4dc4a11bd91481ddeb44be4b7f1cdaf59548a1fa414beffbaf173de84eb7116c`.
- Changes from HYG: the columns and rows above only; positions are J2000 without proper motion.
