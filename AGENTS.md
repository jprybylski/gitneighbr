# gitneighbr

"Be a good neighbor to your repository."

## What this is

`gitneighbr` is a local, browser-based R package/app that helps people who
are unfamiliar with Git understand, save, and publish changes in an
*existing* Git repository — while staying a fast, scriptable convenience
tool for experienced users too. It deliberately exposes only a small, safe
subset of Git (status, commit, push, one annotated tag, single-file
restore, `.gitignore` help) and refuses to automate anything that requires
human judgment (diverged history, conflicts, force pushes, history rewrites
are detected and explained, never offered as one-click actions).

The full product spec lives in
[`gitneighbor-package-specification.md`](./gitneighbor-package-specification.md)
(historical filename — see **Naming**, below).

## Naming

The original spec document was written under the name `gitneighbor` (US
spelling). The package is actually named **`gitneighbr`** (matching this
repo/directory name — vowel-dropped, less US-centric). Every function,
option, and class name in the codebase uses `gitneighbr`
(`gitneighbr::open_repo()`, `getOption("gitneighbr.git")`,
`gitneighbr_session`, etc.) — mentally substitute `gitneighbr` for
`gitneighbor` when reading the spec doc.

## Architecture

- **Backend**: an R package that launches a local [`plumber2`](https://plumber2.data-imaginist.com/)
  HTTP server, bound to `127.0.0.1` only, protected by a random per-session
  bearer token.
- **Git engine**: the user's own system `git`, invoked only via
  `processx` argv arrays (never a shell string) — this preserves the
  user's credential helpers, SSH agent, hooks, signing config, and LFS
  setup, and is a hard non-negotiable (see spec §10.3, §13).
- **Frontend**: a Svelte + TypeScript SPA, built with **bun** (`bun install`,
  `bun run build`) instead of npm — source lives in `frontend/`, and the
  compiled, committed output ships in `inst/www/` so installing/loading the
  R package never needs Node, bun, or network access. Rebuild after
  changing `frontend/src/*`:
  ```sh
  cd frontend && bun install && bun run build
  ```
  then commit both `frontend/` and the regenerated `inst/www/`.
- **Non-blocking launch / clean shutdown**: `open_repo()` spawns the server
  as an independent OS process via `processx::process$new(cleanup_tree =
  TRUE)` (see `R/serve.R`), the same pattern `deck_serve()` uses in
  `~/deckifyr/R/serve.R` — control returns to the R console immediately.
  Stop it via the returned session's `$stop()`, or recover and stop it by
  port alone (`stop_session(port = ...)`) if the R session that started it
  was restarted; the port-based path verifies a process's command line
  actually looks like a `gitneighbr` server before killing it.

## Development phases

- **Phase 0 — Scaffold (done)**: package skeleton; non-blocking
  serve/stop; a real `/api/v1/status` endpoint backed by actual `git
  status` parsing; a minimal bun/Svelte page rendering it; local `R CMD
  check` clean.
- **Phase 1 — 0.1.0 "Safe core workflow"**: the rest of what the spec
  scopes into 0.1.0 — see the repo's issues labeled `phase-1`. Covers
  doctor() diagnostics, changes list + diff viewer, commit, fetch-first
  push, combined save-and-send, optional annotated tag, fast-forward-only
  update, single-file restore, untracked-file removal via trash,
  `.gitignore` assistance, the full state/notice model, the error
  taxonomy + Advanced Details drawer, security hardening (Host header/DNS
  rebinding checks, Origin validation, single-mutation locking),
  accessibility (WCAG 2.2 AA), CRAN packaging readiness, and a usability
  test with ≥5 target users.
- **Phase 2 — 0.2.0 "Onboarding and recovery"**: see issues labeled
  `phase-2`. Guided Git identity config + credential diagnostics, cloning
  an existing GitHub repo, init + publish a new repo, improved
  conflict-handoff/diagnostic export.
- **Phase 3 — 0.3.0 "Collaboration"**: see issues labeled `phase-3`.
  Optional GitHub API integration, protected-branch/PR workflow, release
  creation from a pushed tag, repo policy config.
- **Deferred indefinitely** (per the spec, pending user research): general
  branch management, history rewriting, a full merge-conflict editor,
  arbitrary remote-hosting admin, IDE embedding as the primary experience.

## Conventions

- Roxygen2 (markdown enabled) for all documentation; run
  `devtools::document()` after changing any `@export`ed function or R6
  class.
- `testthat` edition 3; tests that spin up a real server or shell out to
  `git` should `skip_on_cran()` and `skip_if_not_installed()` their
  optional deps (`httr2`, `withr`).
- Never shell-construct Git commands — always an explicit argv character
  vector passed to `processx`.
- Any new `pkg::fun()` usage that only appears inside an R6 method body
  needs an explicit `@importFrom` tag, or `R CMD check`'s "dependencies in
  R code" check won't see it (R6 generators aren't themselves functions,
  so the check's static scan skips into them).
