# assets/earth

Earth's surface textures: NASA's Blue Marble Next Generation for January, an equirectangular colour map without shaded relief or bathymetry, and the night lights and cloud cover derived from two more NASA maps (`decision 13: Asset storage and attribution`, `decision 40: Earth's look and drag`, `decision 130: Earth's night lights and clouds in numbers: conversion, packing, radiance, fade, cloud albedo and shadow`).

## blue-marble-5400.jpg

The file as NASA publishes it, renamed; not modified.

- Source page: https://science.nasa.gov/earth/earth-observatory/blue-marble-next-generation/base-map/
- File: https://assets.science.nasa.gov/content/dam/science/esd/eo/images/bmng/bmng-base/january/world.200401.3x5400x2700.jpg
- Fetched: 2026-10-14, with `curl -sSfL -o blue-marble-5400.jpg <file URL>`.
- Size: 1,884,678 bytes; JPEG, 5400×2700, 3 components.
- SHA-256: `99f5faad74efe985fbf1714c8be7296ca9999759a1215b65f99b7f1df278dde5`
- Layout: 180° W at the left edge, Greenwich in the centre column, 180° E at the right edge; 90° N in the top row.
- Credit: "NASA Earth Observatory (Blue Marble Next Generation)".
- Licence, from NASA's "Guidelines for using images and media" (https://www.nasa.gov/nasa-brand-center/images-and-media/, fetched 2026-10-14, quoted in `docs/luv-capabilities.md`):
  - "NASA content – images, audio, video, and media files used in the rendition of 3-dimensional models, such as texture maps and polygon data in any format – generally are not subject to copyright in the United States. You may use this material for educational or informational purposes, including photo collections, textbooks, public exhibits, computer graphical simulations and Internet Web pages."
  - "NASA should be acknowledged as the source of the material."
- No NASA logo and no implied endorsement by NASA, under the same guidelines.

## black-marble-5400.jpg

City lights from NASA's Black Marble 2016, resampled and with its dim background removed; made by `scripts/convert-earth`.

- Source page: https://science.nasa.gov/earth/earth-observatory/earth-at-night/maps/
- Source file: https://assets.science.nasa.gov/content/dam/science/esd/eo/images/imagerecords/144000/144898/BlackMarble_2016_3km.jpg
- Fetched: 2026-11-13; 8,106,233 bytes; JPEG, 13500×6750; SHA-256 `230aac448ae68c358be433dd518888cccb3a85ccf66f7b44326441c324ad6725`.
- Result: 1,289,995 bytes; JPEG, 5400×2700, 4:4:4; SHA-256 `fa866eb35acfda8db3ea3c54bf177d34432efb2d7b8d0adedf4ac6f5297fed30`.
- Layout: as the Blue Marble (180° W at the left edge, Greenwich in the centre column, 90° N in the top row); checked by the Nile delta and the Po valley lit at their texels (`tests/assets.lisp`).
- Derivation (ImageMagick, one thread, in linear light): `-strip -colorspace RGB`, per channel `-evaluate subtract F` and `divide 1−F` with F = 2.5 % for R and G and 6.5 % for B (the image's land background is dim and blue: over the Sahara R 36–39, G 33–36, B 60–70 in sRGB; measured from 60×60 texel windows), `-evaluate max 0`, `-filter Lanczos -resize 5400x2700!`, `-clamp -colorspace sRGB`, `-sampling-factor 4:4:4 -quality 92`. Sahara and open Pacific read 0.
- Credit: "NASA Earth Observatory images by Joshua Stevens, using Suomi NPP VIIRS data from Miguel Román, NASA GSFC".
- Licence: NASA's guidelines, as for the day map; no NASA logo, no implied endorsement.

## clouds-5400.jpg

Cloud cover from NASA's Blue Marble clouds, resampled to grey; made by `scripts/convert-earth`.

- Source page: https://visibleearth.nasa.gov/images/57747/blue-marble-clouds (now redirects; read in the Internet Archive).
- Source file: https://eoimages.gsfc.nasa.gov/images/imagerecords/57000/57747/cloud_combined_8192.tif
- Fetched: 2026-11-13 from the Internet Archive's capture of 2023-12-30 (`https://web.archive.org/web/20231230035002id_/<file URL>`), because eoimages.gsfc.nasa.gov did not answer; the script tries the original URL first and checks the hash either way. 35,870,468 bytes; TIFF, 8192×4096, one grey channel; SHA-256 `d137775d8966ab8d443fd5126dc6e7ad72072bc1ed50555c5818d221735daf0f`.
- Result: 4,458,563 bytes; JPEG, 5400×2700, greyscale; SHA-256 `e0a02bc27efa4ae6aea4579c062720f84a6b2dbd221276dc01f63c8e0b1fdfc5`.
- Layout: as the day map.
- Derivation (ImageMagick, one thread): `-strip -channel R -separate`, `-filter Lanczos -resize 5400x2700!`, `-clamp -colorspace Gray -quality 90`; the file's own values, 0 clear to 255 overcast.
- Credit: "NASA Goddard Space Flight Center Image by Reto Stöckli".
- Licence: NASA's guidelines, as for the day map; no NASA logo, no implied endorsement.
