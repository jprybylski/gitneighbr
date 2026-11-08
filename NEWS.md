# gitneighbr (development version)

* Managed mode: the status summary no longer mentions GitHub. With sending on, it counts snapshots not yet sent and those waiting on "the shared copy"; with sending off, it shows only what isn't in a snapshot yet.

# gitneighbr 0.2.0

## Managed (launcher) mode

A launcher can now run gitneighbr for someone, behind its own local web address. Standalone use with `open_repo()` is unchanged. See `vignette("launcher")`.

* **Path prefix**: the page and API work under a path such as `/apps/gitneighbr/`, through proxies that forward or strip the prefix.
* **Foreground entry point**: `gitneighbr_serve()` and `scripts/serve.R` print a `GITNEIGHBR_READY` line once the socket is bound. Health reports `version`, `busy` and `managed`. A shutdown route that needs a second token stops the server in order.
* **Launcher token**: the token arrives in the `X-Gitneighbr-Token` header that the proxy adds, so the page holds none. Every route now refuses cross-site browser requests (`Sec-Fetch-Site`).
* **Exact folder**: a managed session works only on the folder it is given. A folder inside another repository, a `.git` file or link, `core.worktree`, alternates and submodules are explained and refused.
* **Locked-down Git**: Git runs with a launcher-trusted executable, no inherited `GIT_*`/`SSH_*` variables, and none of the computer's or the user's own configuration. On Windows, gitneighbr's own system configuration keeps Git for Windows' defaults for line endings (`core.autocrlf`) and certificate checks (`schannel`), so folders checked out by other Windows Git tools aren't shown as changed and HTTPS works with a company's own certificates. Hooks, fsmonitor, pagers, signing, credential helpers, external diff and text conversion are off, and HTTPS is the only transport. A repository whose settings or attributes need anything else (an include, a filter such as Git LFS, an unreviewed setting) is refused, naming the setting but not its value.
* **Feature profiles**: `--features=` chooses what a session offers, and the server refuses the rest (`FEATURE_DISABLED`). Without `github`, nothing starts `gh` or calls the GitHub API.
* **Launcher policy file**:
  * groups changed files under labels;
  * protects paths from every working-tree action;
  * marks files that need extra review before sending, and blocks some from sending;
  * can offer `.gitignore` rules when the folder is first set up, never written over an existing `.gitignore`.
  * With a policy, nothing is pre-selected.
* **Send and Update with a full preview**: a user-invoked check fetches the destination branch only. The preview lists every outgoing or incoming snapshot with each changed path, size and binary flag. Send pushes, and Update fast-forwards to, exactly the previewed commit, and refuses if anything changed. Access tokens are held in memory and reach Git only through environment variables.

## Changes in every mode

* A selected-file snapshot no longer changes what else was staged. It is built on a temporary index, and each selected path is re-checked when saving.
* Repository policy (`require_pull_request`, `disallow_untracked_trash`) is now enforced by the server, not only by disabled buttons.
* Untracked folders are listed file by file. This fixes the changes list when a new folder appears.
* Standalone pages remove `#token=` from the address bar and history; reloading the tab still works.
* `git clone` no longer reads an address starting with `-` as an option, and one remaining error detail is now sanitised.
* Releases include `SHA256SUMS`.

# gitneighbr 0.1.0

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
