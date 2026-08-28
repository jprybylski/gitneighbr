# gitneighbr

<!-- badges: start -->
[![R-CMD-check](https://github.com/jprybylski/gitneighbr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jprybylski/gitneighbr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> Be a good neighbor to your repository.

`gitneighbr` is a local, browser-based tool that helps people who are
unfamiliar with Git understand, save, and publish changes in an existing
Git repository — while staying a fast, scriptable convenience tool for
people who already know Git well. It exposes a small, safe subset of Git
(status, commit, push, one annotated tag, single-file restore, `.gitignore`
help) and never automates anything that needs human judgment.

## Status

Early scaffold. A minimal vertical slice works end to end (launch, real
repository status, browser UI); most of the product described in
[`gitneighbor-package-specification.md`](./gitneighbor-package-specification.md)
is not yet built — see [`CLAUDE.md`](./CLAUDE.md) for the roadmap and this
repo's issues for the current task list.

## Installation (development)

```r
# install.packages("pak")
pak::pak("jprybylski/gitneighbr")
```

## Usage

```r
gitneighbr::open_repo()
```

Opens the app for the Git repository in your current working directory in
your default browser. The server runs in the background; your R console is
free immediately. Stop it with:

```r
session <- gitneighbr::open_repo()
session$stop()
```

## Development

```r
devtools::load_all()
devtools::test()
```

The frontend (`frontend/`, a Svelte + TypeScript app built with
[bun](https://bun.sh)) is precompiled and committed to `inst/www/`; see
[`CLAUDE.md`](./CLAUDE.md) for the rebuild command.
