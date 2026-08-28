# Third-party JavaScript/CSS licenses

`inst/www/` ships a precompiled build of the `frontend/` Svelte
application (spec §17.1). Vite, `@sveltejs/vite-plugin-svelte`,
TypeScript, and `svelte-check` are build-time only and contribute no
code to that output. Svelte's own runtime helpers, which every compiled
`.svelte` component calls into, are compiled directly into
`inst/www/assets/*.js` and so are recorded here.

| Library | Version (at last build) | License | Notice |
| --- | --- | --- | --- |
| [Svelte](https://svelte.dev) | 5.56.10 | MIT | `svelte.LICENSE.md` |

Regenerate this table (and re-copy the license text below) whenever
`frontend/bun.lock` bumps `svelte` to a new major/minor version.
