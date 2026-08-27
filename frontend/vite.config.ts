import { defineConfig } from "vite";
import { svelte } from "@sveltejs/vite-plugin-svelte";

// Build output is committed to inst/www/ and shipped inside the CRAN
// package (see ../.Rbuildignore, which excludes this frontend/ source
// directory from the R build). Neither `R CMD build` nor `R CMD INSTALL`
// ever runs this step; run `bun run build` here and commit the result.
export default defineConfig({
  plugins: [svelte()],
  build: {
    outDir: "../inst/www",
    emptyOutDir: true,
  },
});
