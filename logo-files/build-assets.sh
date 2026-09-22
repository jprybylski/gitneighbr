#!/usr/bin/env bash
# Re-export logo sizes and browser icons. Requires ImageMagick.
set -euo pipefail
cd "$(dirname "$0")/.."

master=logo-files/gitneighbr-hex-master.png
mkdir -p man/figures pkgdown/favicon frontend/public

# Optional input is a new generated image. Match deckifyr/quartifyr's
# 1160 x 1340 artwork bounds on a transparent 1200 x 1390 canvas.
if [[ $# -gt 0 ]]; then
  magick "$1" -trim +repage -resize '1160x1340!' \
    -background none -gravity center -extent 1200x1390 \
    -units PixelsPerInch -density 72 "$master"
fi
magick "$master" -resize 240x278 logo-files/logo.png
cp logo-files/logo.png man/figures/logo.png

square_icon() {
  magick "$master" -resize "${1}x${1}" -background none \
    -gravity center -extent "${1}x${1}" "$2"
}
square_icon 96 pkgdown/favicon/favicon-96x96.png
square_icon 180 pkgdown/favicon/apple-touch-icon.png
square_icon 192 pkgdown/favicon/web-app-manifest-192x192.png
square_icon 512 pkgdown/favicon/web-app-manifest-512x512.png
magick pkgdown/favicon/web-app-manifest-512x512.png \
  -define icon:auto-resize=48,32,16 pkgdown/favicon/favicon.ico

# Same raster-in-SVG convention as deckifyr/quartifyr (not vector art).
{
  printf '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="240" height="278" viewBox="0 0 240 278"><image width="240" height="278" xlink:href="data:image/png;base64,'
  base64 < man/figures/logo.png | tr -d '\n'
  printf '"/></svg>\n'
} > pkgdown/favicon/favicon.svg

for asset in favicon-96x96.png favicon.svg favicon.ico apple-touch-icon.png; do
  cp "pkgdown/favicon/$asset" "frontend/public/$asset"
done
