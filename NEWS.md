# gitneighbr (development version)

Initial release of `gitneighbr`, a local, browser-based R package and application that helps people who are unfamiliar with Git understand, save, and publish changes in an existing Git repository — while remaining a fast, scriptable convenience tool for experienced users too.

## Core Features (Phase 1)

* **Repository Status & Primary State Model**: Plain-language classification of repository states (`READY`, `CHANGES_ONLY`, `LOCAL_ONLY`, `REMOTE_ONLY_CLEAN`, `DIVERGED`, `CONFLICTED`, `AUTH_REQUIRED`, `NO_UPSTREAM`, `NOT_REPOSITORY`, `GIT_UNAVAILABLE`).
* **Offline Diagnostics (`doctor()`)**: Diagnostic checks verifying Git executable availability, version, user identity, and credential helper / SSH agent status without starting a web server.
* **Changes List & Diff Viewer**: File-by-file status list with additions/deletions counts and an accessible unified diff viewer.
* **Selective Snapshot (Commit)**: Interactive file staging and commit creation with human-friendly validation.
* **Fetch-First Safe Push**: Safe push workflow verifying remote state before publishing snapshots.
* **Combined Save & Send**: One-step commit, optional version tagging, and push workflow.
* **Annotated Version Labels (Tags)**: Create and push annotated Git tags during snapshot or standalone.
* **Fast-Forward Update**: Clean updates from upstream when no diverged history exists.
* **Safe Single-File Restore**: Discard unsaved changes in single tracked files with modal confirmation.
* **Untracked File Removal via OS Trash**: Safe deletion of untracked files sending them to the operating system trash rather than calling `git clean`.
* **.gitignore Assistance**: Quickly ignore files or file patterns directly from the changes list.
* **Security Hardening**: Bearer token authentication, Host header DNS-rebinding protection, Origin verification, and single-mutation concurrency locking.
* **WCAG 2.2 AA Accessibility & Theme Support**: Full keyboard navigability, high-contrast dark/light mode toggle with system preference detection.

## Onboarding & Recovery (Phase 2)

* **Guided Git Identity Setup**: In-app setup for missing `user.name` and `user.email` with global vs. repository-local scoping.
* **Transport-Aware Credential Diagnostics**: Clear diagnostic messages and guidance for missing HTTPS helpers, SSH keys, or expired GitHub tokens.
* **Repository Onboarding**: Interactive Git initialization (`git init`) and GitHub repository cloning (`git clone`) when opened in a non-repository directory.
* **Remote Management**: Connect local repositories to GitHub remotes (`git remote add origin`) and publish initial branches.
* **Exportable Diagnostic Report**: Downloadable diagnostic text reports for handing off diverged or conflicted states to technical colleagues.

## Collaboration & Policy (Phase 3)

* **GitHub REST API Integration**: Multi-host PAT token resolution supporting both public GitHub (`github.com`) and GitHub Enterprise Server / Cloud (GHES / GHEC).
* **Protected-Branch & Pull Request Workflow**: Automatic branch generation (`user/patch-...`) and GitHub PR creation when pushing directly to protected branches is blocked.
* **GitHub Release Publishing**: One-click release publishing directly from pushed version tags with automated changelog generation options.
* **Repository Policy Configuration (`.gitneighbr.json`)**: Configurable team safety rules, including branch restrictions, PR branch prefixes, release workflows, and trash deletion disabling.
* **RStudio / Positron Addin**: Launch `gitneighbr` directly from RStudio's Addins menu via `gitneighbr_addin()`.
