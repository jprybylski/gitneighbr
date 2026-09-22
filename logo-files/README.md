# Logo assets

The artwork was generated with the built-in image-generation tool. The
initial prompt is saved in `prompt.txt`; the final simplification and
androgynous character refinement is saved in `refinement-prompt.txt`.

These exports follow deckifyr and quartifyr's logo conventions:

- `gitneighbr-hex-master.png`: 1200 × 1390 transparent PNG, with artwork
  bounds of 1160 × 1340 centered at (20, 25), at 72 dpi.
- `logo.png` and `../man/figures/logo.png`: identical 240 × 278 PNGs.
  The README displays the logo at 139 pixels high; pkgdown discovers it
  in `man/figures/`.
- `../pkgdown/favicon/`: 96-pixel PNG, SVG with an embedded raster logo,
  ICO containing 16/32/48-pixel icons, 180-pixel Apple touch icon, and
  192/512-pixel manifest icons. Square exports preserve aspect ratio
  with transparent padding.
- `../frontend/public/`: browser icons copied into `inst/www/` by Vite.

To regenerate the small images and icons from the saved master, run
`bash logo-files/build-assets.sh` from the repository root. To replace
the master with new artwork, pass an absolute source PNG path. This
requires ImageMagick. Then run `bun install && bun run build` inside
`frontend/` to refresh the bundled app.

The large artwork and pkgdown source assets are excluded from the R
package build. The small documentation logo and bundled app icons ship
with the package.
