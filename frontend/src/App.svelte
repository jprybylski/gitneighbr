<script lang="ts">
  import { tick } from "svelte";
  import ConfirmDialog from "./ConfirmDialog.svelte";

  type Notice = { code: string; message: string };

  type StatusData = {
    repository: { root_display: string };
    primary_state: string;
    upstream: string | null;
    branch: string | null;
    ahead: number;
    behind: number;
    staged_count: number;
    unstaged_count: number;
    untracked_count: number;
    conflicted_count: number;
    notices: Notice[];
  };

  type ChangeState = "NEW" | "CHANGED" | "RENAMED" | "DELETED" | "CONFLICTED";

  type ChangeEntry = {
    path: string;
    old_path: string | null;
    state: ChangeState;
    untracked: boolean;
    added: number | null;
    deleted: number | null;
    binary: boolean;
    large: boolean;
  };

  type DiffData = {
    path: string;
    old_path: string | null;
    state: ChangeState;
    binary: boolean;
    lines: string[];
    offset_lines: number;
    total_lines: number;
    truncated: boolean;
  };

  type AdvancedDetails = { command: string; exit_status: number; stderr: string };

  // `title` and `advanced` are always present on a real API error (spec
  // Sec 14/15), but this type also covers the frontend's own
  // network/parse-failure messages, which have neither.
  type ApiError = {
    code?: string;
    title?: string;
    message: string;
    recoverable?: boolean;
    advanced?: AdvancedDetails | null;
    diagnosis?: CredentialDiagnosis | null;
  };

  type Envelope<T> = {
    ok: boolean;
    data: T | null;
    error: (ApiError & { code: string; title: string; recoverable: boolean }) | null;
    status_version: number | null;
  };

  type PushResult = {
    remote: string;
    remote_branch: string;
    branch: string;
    sha: string;
    pushed_count: number;
  };

  type UpdateResult = {
    remote: string;
    remote_branch: string;
    branch: string;
    sha: string;
    updated_count: number;
  };

  type IdentityData = {
    name: string | null;
    name_scope: string | null;
    email: string | null;
    email_scope: string | null;
    complete: boolean;
  };

  type CredentialCheck = { id: string; status: "ok" | "fail" | "advisory" | "skipped"; message: string };

  type CredentialDiagnosis = {
    transport: string | null;
    platform: string;
    checks: Record<string, CredentialCheck>;
    guidance: string[];
  };

  type DiagnosticCommand = { command: string; exit_status: number | null; stdout: string; stderr: string };

  type DiagnosticReport = {
    generated_at: string;
    primary_state: string;
    branch: string | null;
    upstream: string | null;
    ahead: number;
    behind: number;
    conflicted_files: string[];
    commands: DiagnosticCommand[];
  };

  const STATE_COPY: Record<string, string> = {
    READY: "Everything is saved and up to date.",
    CHANGES_ONLY: "You have unsaved changes.",
    LOCAL_ONLY: "You have saved snapshots waiting to be sent to GitHub.",
    CHANGES_AND_LOCAL: "You have unsaved changes and saved snapshots waiting to be sent.",
    REMOTE_ONLY_CLEAN: "GitHub has newer updates you don't have yet.",
    REMOTE_ONLY_DIRTY: "GitHub has newer updates, and you also have unsaved changes.",
    DIVERGED: "Local and GitHub work need help to combine.",
    CONFLICTED: "Some files contain changes that need help to combine.",
    AUTH_REQUIRED: "GitHub could not verify your access.",
    NO_UPSTREAM: "This branch isn't connected to GitHub yet.",
    DETACHED_HEAD: "You're not currently on a branch.",
    NOT_REPOSITORY: "This folder isn't a Git project.",
    GIT_UNAVAILABLE: "Git isn't available on this computer.",
  };

  const CHANGE_STATE_LABEL: Record<ChangeState, string> = {
    NEW: "New",
    CHANGED: "Changed",
    RENAMED: "Renamed",
    DELETED: "Deleted",
    CONFLICTED: "Conflicted",
  };

  function getToken(): string | null {
    const match = window.location.hash.match(/token=([^&]+)/);
    return match ? decodeURIComponent(match[1]) : null;
  }

  const token = getToken();

  // Dark mode (issue: simple theme toggle). "system" follows the OS/browser
  // `prefers-color-scheme` (the CSS handles that case with no JS help at
  // all - this only needs to track an explicit override). There's no
  // reliable way to read RStudio/Positron's editor theme from a page
  // rendered in their Viewer pane, so "system" is the closest we can get to
  // "match my IDE" - most webviews already forward the OS-level dark mode
  // setting, so this still lines up for many users.
  type Theme = "light" | "dark" | "system";
  const THEME_KEY = "gitneighbr-theme";
  const THEME_CYCLE: Record<Theme, Theme> = { system: "light", light: "dark", dark: "system" };
  const THEME_LABEL: Record<Theme, string> = { system: "Auto", light: "Light", dark: "Dark" };

  function loadTheme(): Theme {
    try {
      const saved = localStorage.getItem(THEME_KEY);
      return saved === "light" || saved === "dark" ? saved : "system";
    } catch {
      return "system";
    }
  }

  let theme = $state<Theme>(loadTheme());

  $effect(() => {
    if (theme === "system") {
      document.documentElement.removeAttribute("data-theme");
    } else {
      document.documentElement.dataset.theme = theme;
    }
    try {
      localStorage.setItem(THEME_KEY, theme);
    } catch {
      // Storage unavailable (private browsing, restricted webview) - the
      // toggle still works for the rest of this session.
    }
  });

  function cycleTheme() {
    theme = THEME_CYCLE[theme];
  }

  let status = $state<StatusData | null>(null);
  let statusVersion = $state<number | null>(null);
  let changes = $state<ChangeEntry[]>([]);
  let selected = $state<Set<string>>(new Set());
  let errorMessage = $state<ApiError | null>(null);
  let loading = $state(true);
  let statusRequestInFlight = false;

  let activePath = $state<string | null>(null);
  let diff = $state<DiffData | null>(null);
  let diffError = $state<ApiError | null>(null);
  let diffLoading = $state(false);

  let summary = $state("");
  let details = $state("");
  let committing = $state(false);
  let commitError = $state<ApiError | null>(null);
  let commitSuccess = $state<string | null>(null);

  let sendToGithub = $state(true);
  let sending = $state(false);
  let sendError = $state<ApiError | null>(null);
  let sendSuccess = $state<string | null>(null);
  let canRetrySend = $state(false);

  let markAsVersion = $state(false);
  let tagName = $state("");
  let tagAnnotation = $state("");
  let taggedName = $state<string | null>(null);
  let tagError = $state<ApiError | null>(null);
  let tagPushError = $state<ApiError | null>(null);
  let canRetryTagPush = $state(false);

  let confirmDialog: ConfirmDialog;
  let rowBusyPath = $state<string | null>(null);
  let rowError = $state<ApiError | null>(null);
  let rowSuccess = $state<string | null>(null);

  let updating = $state(false);
  let updateError = $state<ApiError | null>(null);
  let updateSuccess = $state<string | null>(null);

  // Onboarding (issues #19/#20): a fresh session for a folder that isn't a
  // Git repository yet offers two ways in, and either one hands off to the
  // normal dashboard once `loadStatus()` sees the resulting state change.
  let initializing = $state(false);
  let initError = $state<ApiError | null>(null);
  let cloneUrl = $state("");
  let cloning = $state(false);
  let cloneError = $state<ApiError | null>(null);

  // "Connect to GitHub" (also issue #20): the same action serves a
  // from-scratch repository right after Initialize, and an existing
  // repository that simply never had a remote configured.
  let publishUrl = $state("");
  let publishing = $state(false);
  let publishError = $state<ApiError | null>(null);
  let publishSuccess = $state<string | null>(null);

  let identityLoaded = false;
  let identityName = $state("");
  let identityEmail = $state("");
  let identityLocalOnly = $state(false);
  let identitySaving = $state(false);
  let identityError = $state<ApiError | null>(null);
  let identitySuccess = $state<string | null>(null);

  // Credential diagnostics (issue #18): loaded once per AUTH_REQUIRED
  // episode so a user who reloads the page while still locked out still
  // sees platform-appropriate guidance and a way to retry, not just the
  // bare state label - `credentialDiagnosisLoaded` resets whenever the
  // state moves away from AUTH_REQUIRED so the next episode reloads it.
  let credentialDiagnosisLoaded = false;
  let credentialDiagnosis = $state<CredentialDiagnosis | null>(null);
  let authRetrying = $state(false);
  let authRetryError = $state<ApiError | null>(null);

  // Conflict/divergence diagnostic export (issue #21): user-triggered, not
  // preloaded like credential diagnostics above, since generating it isn't
  // needed until someone actually wants to hand the report to someone else.
  let diagnosticLoading = $state(false);
  let diagnosticError = $state<ApiError | null>(null);

  const summaryLength = $derived(summary.trim().length);
  const summaryValid = $derived(summaryLength >= 3 && summaryLength <= 72);
  const tagNameValid = $derived(!markAsVersion || tagName.trim().length > 0);
  const canSend = $derived(status?.upstream != null);
  const willSend = $derived(canSend && sendToGithub);
  const actionLabel = $derived(willSend ? "Save and send" : "Save snapshot");
  const canSave = $derived(selected.size > 0 && summaryValid && tagNameValid && !committing && !sending);
  const canUpdate = $derived(status?.primary_state === "REMOTE_ONLY_CLEAN" && !updating);
  // spec Sec 7.1/8.3: LOCAL_ONLY has no working-tree changes to select, so
  // the commit form (and its embedded Send button) never renders - this is
  // the only way to trigger "Send to GitHub" when there's nothing to save.
  const canSendStandalone = $derived(canSend && changes.length === 0 && status?.ahead != null && status.ahead > 0 && !sending);
  const identityNeeded = $derived(status?.notices.some((n) => n.code === "IDENTITY_INCOMPLETE") ?? false);
  const needsHandoff = $derived(
    status?.primary_state === "CONFLICTED" || status?.primary_state === "DIVERGED",
  );
  const otherNotices = $derived(status?.notices.filter((n) => n.code !== "IDENTITY_INCOMPLETE") ?? []);
  const identityValid = $derived(
    identityName.trim().length > 0 && /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(identityEmail.trim()),
  );
  const cloneUrlValid = $derived(cloneUrl.trim().length > 0);
  const publishUrlValid = $derived(publishUrl.trim().length > 0);

  // spec Sec 9.6: "Focus moves to the result or error summary after an
  // operation." Each result region below has a stable `id` and
  // `tabindex="-1"` so it can receive focus programmatically without
  // joining the normal tab order; `tick()` waits for the just-set state to
  // actually reach the DOM before the focus call runs.
  async function focusRegion(id: string) {
    await tick();
    document.getElementById(id)?.focus();
  }

  // A rejected fetch() (as opposed to a resolved response with an error
  // envelope) always throws TypeError - that's the browser reporting it
  // never got a response at all, which in this app almost always means the
  // gitneighbr server process has exited. The raw browser message for that
  // ("Failed to fetch", "NetworkError when attempting to fetch resource.",
  // "Load failed", ...) is meaningless to this app's non-technical users.
  function describeFetchError(err: unknown): ApiError {
    if (err instanceof TypeError) {
      return { message: "Server shut down." };
    }
    return { message: err instanceof Error ? err.message : "Could not reach the gitneighbr server." };
  }

  async function api<T>(path: string): Promise<Envelope<T>> {
    const res = await fetch(path, {
      headers: { Authorization: `Bearer ${token}` },
    });
    const envelope = (await res.json()) as Envelope<T>;
    if (envelope.status_version != null) statusVersion = envelope.status_version;
    return envelope;
  }

  // Every mutating call carries the last status_version this client
  // observed (spec Sec 7.3); the server rejects a stale one with
  // 409 STATE_CHANGED rather than acting on a display the user no longer
  // sees, which still parses as an ordinary envelope here.
  async function postApi<T>(path: string, body: Record<string, unknown>): Promise<Envelope<T>> {
    const res = await fetch(path, {
      method: "POST",
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ ...body, status_version: statusVersion }),
    });
    const envelope = (await res.json()) as Envelope<T>;
    if (envelope.status_version != null) statusVersion = envelope.status_version;
    return envelope;
  }

  // Keeps each still-present file's checkbox as the user left it, and
  // defaults newly appeared files to selected - a naive replace would
  // otherwise reset the whole selection on every background poll.
  function reconcileSelection(previous: ChangeEntry[], next: ChangeEntry[]): Set<string> {
    const previousPaths = new Set(previous.map((c) => c.path));
    const result = new Set<string>();
    for (const path of selected) {
      if (previousPaths.has(path) && next.some((c) => c.path === path)) {
        result.add(path);
      }
    }
    for (const change of next) {
      if (!previousPaths.has(change.path)) {
        result.add(change.path);
      }
    }
    return result;
  }

  async function refresh() {
    if (!token || statusRequestInFlight) return;
    statusRequestInFlight = true;
    try {
      const [statusEnvelope, changesEnvelope, identityEnvelope] = await Promise.all([
        api<StatusData>("/api/v1/status"),
        api<{ changes: ChangeEntry[] }>("/api/v1/changes"),
        api<IdentityData>("/api/v1/identity"),
      ]);

      if (!statusEnvelope.ok || !statusEnvelope.data) {
        errorMessage = statusEnvelope.error ?? { message: "Request failed." };
      } else {
        status = statusEnvelope.data;
        errorMessage = null;
      }

      if (status?.primary_state === "AUTH_REQUIRED") {
        if (!credentialDiagnosisLoaded) {
          credentialDiagnosisLoaded = true;
          const diagEnvelope = await api<CredentialDiagnosis>("/api/v1/credential-diagnosis");
          if (diagEnvelope.ok && diagEnvelope.data) credentialDiagnosis = diagEnvelope.data;
        }
      } else {
        credentialDiagnosisLoaded = false;
        credentialDiagnosis = null;
        authRetryError = null;
      }

      if (changesEnvelope.ok && changesEnvelope.data) {
        selected = reconcileSelection(changes, changesEnvelope.data.changes);
        changes = changesEnvelope.data.changes;
      }

      // Prefill only once: the user may be mid-edit on a later poll, and a
      // background refresh must never clobber what they're typing.
      if (identityEnvelope.ok && identityEnvelope.data && !identityLoaded) {
        identityName = identityEnvelope.data.name ?? "";
        identityEmail = identityEnvelope.data.email ?? "";
        identityLoaded = true;
      }
    } catch (err) {
      errorMessage = describeFetchError(err);
    } finally {
      statusRequestInFlight = false;
    }
  }

  async function loadStatus() {
    if (!token) {
      errorMessage = { message: "Missing session token. Open this page via gitneighbr::open_repo()." };
      loading = false;
      return;
    }
    await refresh();
    loading = false;
  }

  // Spec Sec 7.3: poll every 2 seconds while the page is visible and
  // coalesce so only one status calculation is in flight at a time
  // (the `statusRequestInFlight` guard in `refresh()`); pause entirely
  // while the document is hidden.
  function startPolling() {
    const POLL_MS = 2000;
    setInterval(() => {
      if (document.visibilityState === "visible" && !committing && !sending) {
        refresh();
      }
    }, POLL_MS);
    document.addEventListener("visibilitychange", () => {
      if (document.visibilityState === "visible") refresh();
    });
  }

  function toggleSelected(path: string) {
    const next = new Set(selected);
    if (next.has(path)) {
      next.delete(path);
    } else {
      next.add(path);
    }
    selected = next;
  }

  // spec Sec 8.5: a tag is only ever created after the triggering commit
  // succeeds, using the fresh status_version the post-commit status refresh
  // observed (the commit response's own status_version is already stale by
  // definition - the commit itself is the change that bumped it).
  async function createVersionTag() {
    tagError = null;
    const name = tagName.trim();
    const envelope = await postApi<{ name: string; sha: string }>("/api/v1/tag", {
      name,
      annotation: tagAnnotation.trim() || undefined,
    });
    if (!envelope.ok || !envelope.data) {
      tagError = envelope.error ?? { message: "Could not create that version label." };
      taggedName = null;
      return;
    }
    taggedName = envelope.data.name;
    tagName = "";
    tagAnnotation = "";
    markAsVersion = false;
  }

  async function saveSnapshot() {
    if (!canSave) return;
    commitError = null;
    commitSuccess = null;
    sendError = null;
    sendSuccess = null;
    canRetrySend = false;
    tagError = null;
    tagPushError = null;
    canRetryTagPush = false;
    taggedName = null;
    const shouldSend = willSend;
    const shouldTag = markAsVersion && tagName.trim().length > 0;
    committing = true;
    try {
      const envelope = await postApi<{ sha: string; summary: string }>("/api/v1/commit", {
        paths: Array.from(selected),
        summary: summary.trim(),
        details: details.trim() || undefined,
      });
      if (!envelope.ok || !envelope.data) {
        commitError = envelope.error ?? { message: "Could not save this snapshot." };
        return;
      }
      commitSuccess = `Saved snapshot ${envelope.data.sha}.`;
      summary = "";
      details = "";
      activePath = null;
      diff = null;
      await loadStatus();
      if (shouldTag) {
        await createVersionTag();
      }
      if (shouldSend) {
        await sendSavedSnapshots();
      }
    } catch (err) {
      commitError = describeFetchError(err);
    } finally {
      committing = false;
      await focusRegion("commit-result");
    }
  }

  // Frontend orchestration over separate `/refresh-remote` and `/push`
  // calls (spec §14.1: "not one opaque backend transaction"). A snapshot
  // that already saved locally stays saved even if this fails, so on
  // failure the interface must say so precisely rather than implying the
  // whole "Save and send" action was lost - see `canRetrySend`.
  // spec Sec 8.5.6-8: the branch is always pushed before the tag, and a
  // tag-push failure after a successful branch push is reported precisely
  // as a partial result (the branch push already succeeded) with its own
  // retry, never folded into `sendError`.
  async function pushVersionTag(name: string) {
    tagPushError = null;
    canRetryTagPush = false;
    const envelope = await postApi<{ remote: string; name: string }>("/api/v1/push-tag", { name });
    if (!envelope.ok) {
      tagPushError = envelope.error ?? { message: "Could not send this version label to GitHub." };
      canRetryTagPush = true;
    }
  }

  async function sendSavedSnapshots() {
    sendError = null;
    sendSuccess = null;
    canRetrySend = false;
    sending = true;
    try {
      const refreshEnvelope = await postApi<Omit<StatusData, "repository">>("/api/v1/refresh-remote", {});
      if (!refreshEnvelope.ok) {
        sendError = refreshEnvelope.error ?? { message: "Could not check GitHub for updates before sending." };
        canRetrySend = true;
        return;
      }
      const pushEnvelope = await postApi<PushResult>("/api/v1/push", {});
      if (!pushEnvelope.ok || !pushEnvelope.data) {
        sendError = pushEnvelope.error ?? { message: "Could not send this snapshot to GitHub." };
        canRetrySend = true;
        return;
      }
      sendSuccess = `Sent to ${pushEnvelope.data.remote}/${pushEnvelope.data.remote_branch}.`;
      if (taggedName) {
        await pushVersionTag(taggedName);
      }
    } catch (err) {
      sendError = describeFetchError(err);
      canRetrySend = true;
    } finally {
      sending = false;
      await loadStatus();
      await focusRegion(changes.length > 0 || commitError || commitSuccess ? "commit-result" : "send-result");
    }
  }

  async function retryTagPush() {
    if (!taggedName) return;
    sending = true;
    try {
      await pushVersionTag(taggedName);
    } finally {
      sending = false;
      await loadStatus();
      await focusRegion("commit-result");
    }
  }

  // Scrolls all the way to the top of the diff pane (not just "nearest",
  // which after a long change list can leave the pane's head off-screen)
  // and focuses it - the outline plus the jump itself is the confirmation
  // that a click on a file actually did something.
  async function focusDiffPane() {
    await tick();
    const el = document.getElementById("diff-pane");
    if (!el) return;
    el.scrollIntoView({ behavior: "smooth", block: "start" });
    el.focus({ preventScroll: true });
  }

  async function openDiff(path: string) {
    activePath = path;
    diff = null;
    diffError = null;
    diffLoading = true;
    await focusDiffPane();
    try {
      const envelope = await api<DiffData>(`/api/v1/diff?path=${encodeURIComponent(path)}`);
      if (!envelope.ok || !envelope.data) {
        diffError = envelope.error ?? { message: "Could not load this diff." };
      } else {
        diff = envelope.data;
      }
    } catch (err) {
      diffError = describeFetchError(err);
    } finally {
      diffLoading = false;
      if (diffError) await focusRegion("diff-result");
    }
  }

  function closeDiff() {
    activePath = null;
    diff = null;
    diffError = null;
  }

  // spec Sec 8.7: restoring discards the file's current unsaved contents,
  // so this always confirms first, naming the file and stating the
  // consequence (point 3) via the shared focus-trapped ConfirmDialog.
  async function restoreFile(path: string) {
    const ok = await confirmDialog.confirm({
      title: "Restore last saved version?",
      message: `"${path}"'s current unsaved contents will be lost. This cannot be undone.`,
      confirmLabel: "Restore",
      danger: true,
    });
    if (!ok) return;
    rowError = null;
    rowSuccess = null;
    rowBusyPath = path;
    try {
      const envelope = await postApi<{ path: string }>("/api/v1/restore", { path });
      if (!envelope.ok) {
        rowError = envelope.error ?? { message: "Could not restore this file." };
      } else {
        rowSuccess = `Restored "${path}" to its last saved version.`;
        if (activePath === path) {
          activePath = null;
          diff = null;
        }
      }
      await refresh();
    } catch (err) {
      rowError = describeFetchError(err);
    } finally {
      rowBusyPath = null;
      await focusRegion("row-result");
    }
  }

  // spec Sec 8.8: never git clean, always one named file, always via the
  // OS trash/recycle bin - confirmed first since it removes the file from
  // the working tree even though it stays recoverable in the OS trash.
  async function trashFile(path: string) {
    const ok = await confirmDialog.confirm({
      title: "Move to trash?",
      message: `"${path}" will be moved to this computer's trash or recycle bin.`,
      confirmLabel: "Move to trash",
      danger: true,
    });
    if (!ok) return;
    rowError = null;
    rowSuccess = null;
    rowBusyPath = path;
    try {
      const envelope = await postApi<{ path: string }>("/api/v1/trash", { path });
      if (!envelope.ok) {
        rowError = envelope.error ?? { message: "Could not move this file to the trash." };
      } else {
        rowSuccess = `Moved "${path}" to the trash.`;
        selected = new Set([...selected].filter((p) => p !== path));
        if (activePath === path) {
          activePath = null;
          diff = null;
        }
      }
      await refresh();
    } catch (err) {
      rowError = describeFetchError(err);
    } finally {
      rowBusyPath = null;
      await focusRegion("row-result");
    }
  }

  // spec Sec 8.9: the exact proposed rule is shown before writing (point
  // 3) - the confirm dialog's message doubles as that preview.
  async function ignoreFile(path: string) {
    const ok = await confirmDialog.confirm({
      title: "Stop showing this file?",
      message: `A rule for "${path}" will be added to .gitignore. It will no longer appear as an untracked change.`,
      confirmLabel: "Add rule",
    });
    if (!ok) return;
    rowError = null;
    rowSuccess = null;
    rowBusyPath = path;
    try {
      const envelope = await postApi<{ path: string; rule: string; added: boolean }>("/api/v1/ignore", { path });
      if (!envelope.ok || !envelope.data) {
        rowError = envelope.error ?? { message: "Could not update .gitignore." };
      } else {
        rowSuccess = envelope.data.added
          ? `Added "${envelope.data.rule}" to .gitignore.`
          : `"${path}" was already ignored.`;
      }
      await refresh();
    } catch (err) {
      rowError = describeFetchError(err);
    } finally {
      rowBusyPath = null;
      await focusRegion("row-result");
    }
  }

  // spec Sec 8.6: offered only when behind, not ahead, not diverged, and
  // the working tree is clean (mirrored server-side); never git pull,
  // always a verified fast-forward that stops without changing anything
  // local if it isn't possible, so this needs no confirmation.
  async function getUpdates() {
    if (!canUpdate) return;
    updateError = null;
    updateSuccess = null;
    updating = true;
    try {
      const envelope = await postApi<UpdateResult>("/api/v1/update", {});
      if (!envelope.ok || !envelope.data) {
        updateError = envelope.error ?? { message: "Could not get updates from GitHub." };
      } else {
        updateSuccess = `Updated to ${envelope.data.sha} from ${envelope.data.remote}/${envelope.data.remote_branch}.`;
      }
      await loadStatus();
    } catch (err) {
      updateError = describeFetchError(err);
    } finally {
      updating = false;
      await focusRegion("update-result");
    }
  }

  // Conflict/divergence diagnostic export (issue #21): renders the report
  // as plain text and hands it to the browser as a download rather than
  // just displaying it, since the point is to give the user something to
  // paste into an email or chat message to someone more experienced.
  function formatDiagnosticReport(report: DiagnosticReport): string {
    const lines = [
      "gitneighbr diagnostic report",
      `Generated: ${report.generated_at}`,
      `State: ${STATE_COPY[report.primary_state] ?? report.primary_state}`,
      `Branch: ${report.branch ?? "(none)"}`,
      `Upstream: ${report.upstream ?? "(none)"}`,
      `Ahead: ${report.ahead}  Behind: ${report.behind}`,
    ];
    if (report.conflicted_files.length > 0) {
      lines.push("", "Conflicted files:", ...report.conflicted_files.map((f) => `  ${f}`));
    }
    lines.push("", "Commands gitneighbr checked:");
    for (const cmd of report.commands) {
      lines.push("", `$ ${cmd.command}`, `(exit status: ${cmd.exit_status ?? "unknown"})`);
      if (cmd.stdout.trim()) lines.push(cmd.stdout.trimEnd());
      if (cmd.stderr.trim()) lines.push(cmd.stderr.trimEnd());
    }
    return lines.join("\n") + "\n";
  }

  function downloadDiagnosticReport(report: DiagnosticReport) {
    const text = formatDiagnosticReport(report);
    const blob = new Blob([text], { type: "text/plain" });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = `gitneighbr-diagnostic-${report.generated_at.replace(/[^0-9a-z]/gi, "-")}.txt`;
    document.body.appendChild(link);
    link.click();
    link.remove();
    URL.revokeObjectURL(url);
  }

  async function exportDiagnosticReport() {
    diagnosticLoading = true;
    diagnosticError = null;
    try {
      const envelope = await api<{ report: DiagnosticReport }>("/api/v1/diagnostic-report");
      if (!envelope.ok || !envelope.data) {
        diagnosticError = envelope.error ?? { message: "Could not generate a diagnostic report." };
        return;
      }
      downloadDiagnosticReport(envelope.data.report);
    } catch (err) {
      diagnosticError = describeFetchError(err);
    } finally {
      diagnosticLoading = false;
      await focusRegion("diagnostic-result");
    }
  }

  // Credential diagnostics (issue #18): the AUTH_REQUIRED state is sticky
  // server-side until a fetch/push succeeds again, so this is the one
  // retry path that always stays available for it regardless of which
  // action originally failed - a read-only fetch is enough to clear the
  // flag (spec Sec 16 step 4: "Offer Retry after the user completes
  // authentication externally").
  async function retryConnection() {
    authRetrying = true;
    authRetryError = null;
    try {
      const envelope = await postApi<Omit<StatusData, "repository">>("/api/v1/refresh-remote", {});
      if (!envelope.ok) {
        authRetryError = envelope.error ?? { message: "Still could not connect to GitHub." };
        if (envelope.error?.diagnosis) credentialDiagnosis = envelope.error.diagnosis;
      }
    } catch (err) {
      authRetryError = describeFetchError(err);
    } finally {
      authRetrying = false;
      await loadStatus();
      await focusRegion("auth-result");
    }
  }

  // Guided identity setup (issue #17): a new user is never told to run
  // `git config` - this writes it for them, defaulting to their global
  // identity since who someone is isn't really a per-project fact, with an
  // explicit opt-out for someone who deliberately wants a different
  // identity in just this repository.
  async function saveIdentity() {
    if (!identityValid) return;
    identityError = null;
    identitySuccess = null;
    identitySaving = true;
    try {
      const envelope = await postApi<{ name: string; email: string; scope: string }>("/api/v1/identity", {
        name: identityName.trim(),
        email: identityEmail.trim(),
        scope: identityLocalOnly ? "local" : "global",
      });
      if (!envelope.ok || !envelope.data) {
        identityError = envelope.error ?? { message: "Could not save your Git identity." };
      } else {
        identitySuccess = `Saved as ${envelope.data.name} <${envelope.data.email}>.`;
        await refresh();
      }
    } catch (err) {
      identityError = describeFetchError(err);
    } finally {
      identitySaving = false;
      await focusRegion("identity-result");
    }
  }

  // Onboarding, path 1 of 2 (issue #20): initializes the folder gitneighbr
  // is already pointed at. `loadStatus()` afterward is what actually moves
  // the UI off the onboarding screen, by picking up the new primary_state.
  async function initializeHere() {
    initError = null;
    initializing = true;
    try {
      const envelope = await postApi<Record<string, never>>("/api/v1/init", {});
      if (!envelope.ok) {
        initError = envelope.error ?? { message: "Could not initialize a Git repository here." };
        return;
      }
      await loadStatus();
    } catch (err) {
      initError = describeFetchError(err);
    } finally {
      initializing = false;
      await focusRegion("init-result");
    }
  }

  // Onboarding, path 2 of 2 (issue #19): clones into the folder gitneighbr
  // is already pointed at.
  async function cloneHere() {
    if (!cloneUrlValid) return;
    cloneError = null;
    cloning = true;
    try {
      const envelope = await postApi<Record<string, never>>("/api/v1/clone", { url: cloneUrl.trim() });
      if (!envelope.ok) {
        cloneError = envelope.error ?? { message: "Could not clone that repository." };
        return;
      }
      await loadStatus();
    } catch (err) {
      cloneError = describeFetchError(err);
    } finally {
      cloning = false;
      await focusRegion("clone-result");
    }
  }

  // "Connect to GitHub" (issue #20, and the existing-repo-with-no-remote
  // case): a repository whose `origin` already points elsewhere is refused
  // as REMOTE_ALREADY_SET unless `force` is set, so that case is confirmed
  // first, the same shape as `restoreFile()`/`trashFile()`.
  async function publishRepo(force = false) {
    if (!publishUrlValid) return;
    if (!force) {
      publishError = null;
      publishSuccess = null;
    }
    publishing = true;
    try {
      const envelope = await postApi<PushResult>("/api/v1/publish", { url: publishUrl.trim(), force });
      if (!envelope.ok || !envelope.data) {
        if (envelope.error?.code === "REMOTE_ALREADY_SET") {
          const existingUrl = (envelope.data as unknown as { existing_url?: string } | null)?.existing_url;
          publishing = false;
          const ok = await confirmDialog.confirm({
            title: "Replace the connected GitHub repository?",
            message: `This project is already connected to "${existingUrl ?? "another address"}". Connecting to a different address will replace that connection.`,
            confirmLabel: "Replace",
            danger: true,
          });
          if (ok) {
            await publishRepo(true);
          }
          return;
        }
        publishError = envelope.error ?? { message: "Could not publish to GitHub." };
        return;
      }
      publishSuccess = `Connected to ${envelope.data.remote} and sent ${envelope.data.pushed_count} snapshot${envelope.data.pushed_count === 1 ? "" : "s"}.`;
      publishUrl = "";
      await loadStatus();
    } catch (err) {
      publishError = describeFetchError(err);
    } finally {
      publishing = false;
      await focusRegion("publish-result");
    }
  }

  async function loadMoreDiff() {
    if (!diff || !activePath) return;
    const path = activePath;
    const offset = diff.offset_lines + diff.lines.length;
    diffLoading = true;
    try {
      const envelope = await api<DiffData>(
        `/api/v1/diff?path=${encodeURIComponent(path)}&offset_lines=${offset}`,
      );
      if (envelope.ok && envelope.data && diff) {
        diff = { ...envelope.data, lines: [...diff.lines, ...envelope.data.lines] };
      }
    } finally {
      diffLoading = false;
    }
  }

  function diffLineClass(line: string): string {
    if (line.startsWith("+++") || line.startsWith("---")) return "diff-header";
    if (line.startsWith("@@")) return "diff-hunk";
    if (line.startsWith("+")) return "diff-add";
    if (line.startsWith("-")) return "diff-del";
    return "diff-context";
  }

  function diffLineSign(line: string): string {
    if (line.startsWith("+++") || line.startsWith("---") || line.startsWith("@@")) return "";
    if (line.startsWith("+")) return "+";
    if (line.startsWith("-")) return "-";
    return " ";
  }

  // spec Sec 9.4/9.6: the visible "+"/"-" characters already make
  // additions/deletions readable without color, but they're marked
  // aria-hidden since a bare glyph reads poorly aloud - this supplies the
  // screen-reader-only equivalent instead of leaving the line unannounced.
  function diffLineSrLabel(line: string): string {
    if (line.startsWith("+++") || line.startsWith("---") || line.startsWith("@@")) return "";
    if (line.startsWith("+")) return "Added: ";
    if (line.startsWith("-")) return "Removed: ";
    return "";
  }

  function diffLineText(line: string): string {
    if (line.startsWith("+++") || line.startsWith("---") || line.startsWith("@@")) return line;
    if (line.startsWith("+") || line.startsWith("-") || line.startsWith(" ")) return line.slice(1);
    return line;
  }

  loadStatus();
  startPolling();
</script>

{#snippet advancedDetails(advanced: AdvancedDetails)}
  <details class="advanced-details">
    <summary>Advanced details</summary>
    <dl>
      <dt>Command</dt>
      <dd><code>{advanced.command}</code></dd>
      <dt>Exit status</dt>
      <dd>{advanced.exit_status}</dd>
    </dl>
    {#if advanced.stderr}
      <pre class="advanced-stderr">{advanced.stderr}</pre>
    {/if}
  </details>
{/snippet}

{#snippet errorCard(err: ApiError)}
  <div class="card error" role="alert">
    {#if err.title}<p class="error-title">{err.title}</p>{/if}
    <p>{err.message}</p>
    {#if err.diagnosis && err.diagnosis.guidance.length > 0}
      <ul class="notices">
        {#each err.diagnosis.guidance as tip}
          <li>{tip}</li>
        {/each}
      </ul>
    {/if}
    {#if err.advanced}{@render advancedDetails(err.advanced)}{/if}
  </div>
{/snippet}

<ConfirmDialog bind:this={confirmDialog} />

<main>
  <div class="header-row">
    <div>
      <h1>gitneighbr</h1>
      <p class="tagline">Be a good neighbor to your repository.</p>
    </div>
    <button
      type="button"
      class="theme-toggle"
      onclick={cycleTheme}
      aria-label={`Color theme: ${THEME_LABEL[theme]}. Click to change.`}
    >
      {THEME_LABEL[theme]}
    </button>
  </div>

  {#if loading}
    <p>Checking repository status&hellip;</p>
  {:else if errorMessage}
    {@render errorCard(errorMessage)}
  {:else if status}
    <div class="card" aria-live="polite">
      <h2>{status.repository.root_display}</h2>
      {#if status.branch}
        <p class="branch">Branch: {status.branch}</p>
      {/if}
      <p class="state">{STATE_COPY[status.primary_state] ?? status.primary_state}</p>
      {#if status.primary_state === "NOT_REPOSITORY"}
        <section class="identity-section" aria-label="Set up this folder">
          <h3>Initialize a Git repository here</h3>
          <p>Start tracking changes in this folder. Any files already here are kept.</p>
          <div id="init-result" tabindex="-1">
            {#if initError}
              {@render errorCard(initError)}
            {/if}
          </div>
          <button type="button" class="update-button" disabled={initializing} onclick={initializeHere}>
            {initializing ? "Initializing…" : "Initialize a Git repository here"}
          </button>
        </section>
        <section class="identity-section" aria-label="Clone an existing GitHub repository">
          <h3>Or clone an existing GitHub repository</h3>
          <p>Copy a repository from GitHub into this folder.</p>
          <label class="field" for="clone-url">GitHub repository address</label>
          <input
            id="clone-url"
            type="text"
            bind:value={cloneUrl}
            placeholder="https://github.com/you/your-repo.git"
            disabled={cloning}
          />
          <div id="clone-result" tabindex="-1">
            {#if cloneError}
              {@render errorCard(cloneError)}
            {/if}
          </div>
          <button type="button" class="update-button" disabled={!cloneUrlValid || cloning} onclick={cloneHere}>
            {cloning ? "Cloning…" : "Clone into this folder"}
          </button>
        </section>
      {:else}
      <dl>
        <dt>Unsaved changes</dt>
        <dd>{status.staged_count + status.unstaged_count + status.untracked_count}</dd>
        <dt>Saved, not yet sent</dt>
        <dd>{status.ahead}</dd>
        <dt>Waiting on GitHub</dt>
        <dd>{status.behind}</dd>
        {#if status.conflicted_count > 0}
          <dt>Conflicted</dt>
          <dd>{status.conflicted_count}</dd>
        {/if}
      </dl>
      {#if otherNotices.length > 0}
        <ul class="notices">
          {#each otherNotices as notice (notice.code)}
            <li>{notice.message}</li>
          {/each}
        </ul>
      {/if}
      {#if needsHandoff}
        <section class="identity-section" aria-label="Get help with this repository">
          <h3>Get help from someone experienced with Git</h3>
          <p>
            gitneighbr won't combine these changes on its own. Download a diagnostic report to share with someone
            who can help - it includes this repository's state and the exact Git commands gitneighbr checked, with
            passwords and tokens removed.
          </p>
          <div id="diagnostic-result" tabindex="-1">
            {#if diagnosticError}
              {@render errorCard(diagnosticError)}
            {/if}
          </div>
          <button type="button" class="update-button" disabled={diagnosticLoading} onclick={exportDiagnosticReport}>
            {diagnosticLoading ? "Preparing report…" : "Download diagnostic report"}
          </button>
        </section>
      {/if}
      {#if status.primary_state === "AUTH_REQUIRED"}
        <section class="identity-section" aria-label="Reconnect to GitHub">
          <h3>Reconnecting to GitHub</h3>
          <p>gitneighbr could not verify your access the last time it checked GitHub. Your saved work is safe.</p>
          {#if credentialDiagnosis && credentialDiagnosis.guidance.length > 0}
            <ul class="notices">
              {#each credentialDiagnosis.guidance as tip}
                <li>{tip}</li>
              {/each}
            </ul>
          {/if}
          <div id="auth-result" tabindex="-1">
            {#if authRetryError}
              {@render errorCard(authRetryError)}
            {/if}
          </div>
          <button type="button" class="update-button" disabled={authRetrying} onclick={retryConnection}>
            {authRetrying ? "Checking…" : "Retry connecting to GitHub"}
          </button>
        </section>
      {/if}
      {#if identityNeeded}
        <section class="identity-section" aria-label="Set up your Git identity">
          <h3>Before your first snapshot</h3>
          <p>Git needs to know who's making changes. This is saved once and reused automatically.</p>
          <label class="field" for="identity-name">Your name</label>
          <input
            id="identity-name"
            type="text"
            bind:value={identityName}
            placeholder="Ada Lovelace"
            disabled={identitySaving}
          />
          <label class="field" for="identity-email">Your email</label>
          <input
            id="identity-email"
            type="email"
            bind:value={identityEmail}
            placeholder="ada@example.com"
            disabled={identitySaving}
          />
          <label class="field-inline">
            <input type="checkbox" bind:checked={identityLocalOnly} disabled={identitySaving} />
            Just for this project
          </label>
          <div id="identity-result" tabindex="-1">
            {#if identityError}
              {@render errorCard(identityError)}
            {:else if identitySuccess}
              <p class="update-success" role="status">{identitySuccess}</p>
            {/if}
          </div>
          <button
            type="button"
            class="update-button"
            disabled={!identityValid || identitySaving}
            onclick={saveIdentity}
          >
            {identitySaving ? "Saving…" : "Save my name and email"}
          </button>
        </section>
      {/if}
      {#if status.primary_state === "NO_UPSTREAM" || publishError || publishSuccess}
        <section class="identity-section" aria-label="Connect to GitHub">
          <h3>Connect to GitHub</h3>
          {#if status.primary_state === "NO_UPSTREAM"}
            <p>
              Create an empty repository on GitHub, then paste its address here to connect this project to it and
              send what's been saved so far.
            </p>
            <label class="field" for="publish-url">GitHub repository address</label>
            <input
              id="publish-url"
              type="text"
              bind:value={publishUrl}
              placeholder="https://github.com/you/your-repo.git"
              disabled={publishing}
            />
          {/if}
          <div id="publish-result" tabindex="-1">
            {#if publishError}
              {@render errorCard(publishError)}
            {:else if publishSuccess}
              <p class="update-success" role="status">{publishSuccess}</p>
            {/if}
          </div>
          {#if status.primary_state === "NO_UPSTREAM"}
            <button
              type="button"
              class="update-button"
              disabled={!publishUrlValid || publishing}
              onclick={() => publishRepo(false)}
            >
              {publishing ? "Connecting…" : "Connect and send to GitHub"}
            </button>
          {/if}
        </section>
      {/if}
      {#if canUpdate || updateError || updateSuccess}
        <div class="update-section">
          {#if canUpdate}
            <button type="button" class="update-button" disabled={updating} onclick={getUpdates}>
              {updating ? "Getting updates…" : "Get updates from GitHub"}
            </button>
          {/if}
          <div id="update-result" tabindex="-1">
            {#if updateError}
              {@render errorCard(updateError)}
            {:else if updateSuccess}
              <p class="update-success" role="status">{updateSuccess}</p>
            {/if}
          </div>
        </div>
      {/if}
      {#if canSendStandalone || (changes.length === 0 && (sendError || sendSuccess || canRetrySend))}
        <div class="update-section">
          {#if canSendStandalone}
            <button type="button" class="update-button" disabled={sending} onclick={sendSavedSnapshots}>
              {sending ? "Sending…" : "Send to GitHub"}
            </button>
          {/if}
          <div id="send-result" tabindex="-1">
            {#if sendError}
              {@render errorCard(sendError)}
            {:else if sendSuccess}
              <p class="update-success" role="status">{sendSuccess}</p>
            {/if}
          </div>
          {#if canRetrySend}
            <button type="button" class="retry-button" disabled={sending} onclick={sendSavedSnapshots}>
              {sending ? "Sending…" : "Retry sending to GitHub"}
            </button>
          {/if}
        </div>
      {/if}
      {/if}
    </div>

    {#if changes.length > 0}
      <section class="changes" aria-label="Changed files">
        <h2>Changes</h2>
        <ul class="change-list">
          {#each changes as change (change.path)}
            <li class="change-row" class:active={activePath === change.path}>
              <div class="change-row-main">
                <label class="change-select">
                  <input
                    type="checkbox"
                    checked={selected.has(change.path)}
                    onchange={() => toggleSelected(change.path)}
                    aria-label={`Include ${change.path} in the next snapshot`}
                  />
                </label>
                <button type="button" class="change-path" onclick={() => openDiff(change.path)}>
                  {#if change.state === "RENAMED" && change.old_path}
                    <span class="path-text">{change.old_path} &rarr; {change.path}</span>
                  {:else}
                    <span class="path-text">{change.path}</span>
                  {/if}
                </button>
                <span class="change-tag tag-{change.state.toLowerCase()}">{CHANGE_STATE_LABEL[change.state]}</span>
                {#if change.binary}
                  <span class="change-flag">Binary</span>
                {:else if change.added !== null || change.deleted !== null}
                  <span class="change-stats">
                    <span class="stat-add">+{change.added ?? 0}</span>
                    <span class="stat-del">-{change.deleted ?? 0}</span>
                  </span>
                {/if}
                {#if change.large}
                  <span class="change-flag">Large</span>
                {/if}
              </div>
              {#if change.state === "NEW"}
                <div class="change-row-actions">
                  <button
                    type="button"
                    class="row-action-button danger"
                    disabled={rowBusyPath === change.path}
                    aria-label={`Remove ${change.path}`}
                    onclick={() => trashFile(change.path)}
                  >
                    {rowBusyPath === change.path ? "Removing…" : "Remove"}
                  </button>
                  <button
                    type="button"
                    class="row-action-button"
                    disabled={rowBusyPath === change.path}
                    aria-label={`Stop showing this file: ${change.path}`}
                    onclick={() => ignoreFile(change.path)}
                  >
                    {rowBusyPath === change.path ? "Updating…" : "Stop showing this file"}
                  </button>
                </div>
              {:else if change.state !== "CONFLICTED"}
                <div class="change-row-actions">
                  <button
                    type="button"
                    class="row-action-button danger"
                    disabled={rowBusyPath === change.path}
                    aria-label={`Restore last saved version: ${change.path}`}
                    onclick={() => restoreFile(change.path)}
                  >
                    {rowBusyPath === change.path ? "Restoring…" : "Restore last saved version"}
                  </button>
                </div>
              {/if}
            </li>
          {/each}
        </ul>
      </section>
    {/if}

    {#if activePath}
      <section
        id="diff-pane"
        tabindex="-1"
        class="diff-pane"
        aria-label={`Diff for ${activePath}`}
        aria-live="polite"
      >
        <div class="diff-pane-header">
          <h2>{activePath}</h2>
          <button type="button" class="diff-close" aria-label="Close diff" onclick={closeDiff}>
            Close
          </button>
        </div>
        {#if diffLoading && !diff}
          <p>Loading diff&hellip;</p>
        {:else if diffError}
          <div id="diff-result" tabindex="-1">
            {@render errorCard(diffError)}
          </div>
        {:else if diff}
          {#if diff.binary}
            <p class="diff-binary">This is a binary file. No text diff is available.</p>
          {:else if diff.lines.length === 0}
            <p>No differences to show.</p>
          {:else}
            <pre class="diff-body"><code
              >{#each diff.lines as line}<span class="diff-line {diffLineClass(line)}"
                  ><span class="diff-sign" aria-hidden="true">{diffLineSign(line)}</span
                  ><span class="sr-only">{diffLineSrLabel(line)}</span
                  >{diffLineText(line)}
</span>{/each}</code
            ></pre>
            {#if diff.truncated}
              <button type="button" class="load-more" onclick={loadMoreDiff} disabled={diffLoading}>
                {diffLoading ? "Loading…" : `Load more (${diff.offset_lines + diff.lines.length} of ${diff.total_lines} lines)`}
              </button>
            {/if}
          {/if}
        {/if}
      </section>
    {/if}

    {#if rowError || rowSuccess}
      <div id="row-result" tabindex="-1">
        {#if rowError}
          {@render errorCard(rowError)}
        {:else if rowSuccess}
          <p class="row-success" role="status">{rowSuccess}</p>
        {/if}
      </div>
    {/if}

    {#if changes.length > 0 || commitError || commitSuccess}
      <section class="commit-form" aria-label="Save a snapshot">
        <h2>Save snapshot</h2>

        {#if changes.length > 0}
          <p class="selection-count">
            {selected.size} of {changes.length} file{changes.length === 1 ? "" : "s"} selected
          </p>
          <label class="field" for="commit-summary">
            Summary <span class="required">(required, 3-72 characters)</span>
          </label>
          <input
            id="commit-summary"
            type="text"
            maxlength="72"
            bind:value={summary}
            placeholder="What changed?"
            disabled={committing}
          />
          <label class="field" for="commit-details">Details <span class="optional">(optional)</span></label>
          <textarea
            id="commit-details"
            rows="3"
            bind:value={details}
            placeholder="Add more explanation if it helps"
            disabled={committing}
          ></textarea>

          {#if canSend}
            <label class="field-inline">
              <input
                type="checkbox"
                bind:checked={sendToGithub}
                disabled={committing || sending}
              />
              Also send to GitHub
            </label>
          {/if}

          <label class="field-inline">
            <input
              type="checkbox"
              bind:checked={markAsVersion}
              disabled={committing || sending}
            />
            Mark this snapshot as a version
          </label>
          {#if markAsVersion}
            <div class="tag-fields">
              <label class="field" for="tag-name">
                Version label <span class="required">(required)</span>
              </label>
              <input
                id="tag-name"
                type="text"
                bind:value={tagName}
                placeholder="e.g. v1.2.0"
                disabled={committing}
              />
              <label class="field" for="tag-annotation">Note <span class="optional">(optional)</span></label>
              <input
                id="tag-annotation"
                type="text"
                bind:value={tagAnnotation}
                placeholder="What's notable about this version?"
                disabled={committing}
              />
            </div>
          {/if}
        {/if}

        <div id="commit-result" tabindex="-1">
          {#if commitError}
            {@render errorCard(commitError)}
          {/if}
          {#if tagError}
            {@render errorCard(tagError)}
          {/if}
          {#if commitSuccess}
            <div class="card success" role="status">
              <p>{commitSuccess}</p>
              {#if taggedName}
                <p>Marked as version {taggedName}.</p>
              {/if}
              {#if sending}
                <p>Checking GitHub for updates and sending&hellip;</p>
              {:else if sendSuccess}
                <p>{sendSuccess}</p>
                {#if tagPushError}
                  <p class="partial-failure">
                    Sent, but the version label failed to send: {tagPushError.message}
                  </p>
                  {#if tagPushError.advanced}{@render advancedDetails(tagPushError.advanced)}{/if}
                {/if}
              {:else if sendError}
                <p class="partial-failure">Not yet sent to GitHub: {sendError.message}</p>
                {#if sendError.advanced}{@render advancedDetails(sendError.advanced)}{/if}
              {/if}
            </div>
          {/if}
        </div>
        {#if canRetrySend}
          <button type="button" class="retry-button" disabled={sending} onclick={sendSavedSnapshots}>
            {sending ? "Sending…" : "Retry sending to GitHub"}
          </button>
        {/if}
        {#if canRetryTagPush}
          <button type="button" class="retry-button" disabled={sending} onclick={retryTagPush}>
            {sending ? "Sending…" : "Retry sending version label"}
          </button>
        {/if}

        {#if changes.length > 0}
          <button type="button" class="save-button" disabled={!canSave} onclick={saveSnapshot}>
            {committing ? "Saving…" : sending ? "Sending…" : actionLabel}
          </button>
        {/if}
      </section>
    {/if}
  {/if}
</main>

<style>
  /* Theme tokens: light values on :root, overridden either automatically by
     `prefers-color-scheme` (the "Auto" setting - see the `theme` state in
     <script>) or explicitly once the user picks Light/Dark, via
     [data-theme] on <html>. Declaring everything as a variable here is what
     lets the rest of this stylesheet stay theme-agnostic. */
  :global(:root) {
    color-scheme: light dark;
    --bg: #ffffff;
    --surface: #ffffff;
    --surface-muted: #fafafa;
    --text: #1a1a1a;
    --text-muted: #555555;
    --border: #dddddd;
    --border-subtle: #eeeeee;
    --input-border: #cccccc;
    --accent: #0056b3;
    --accent-bg: #eaf1fb;
    --success: #1e7e34;
    --success-bg: #eaf7ec;
    --danger: #c0392b;
    --danger-text: #922b21;
    --danger-bg: #fdecea;
    --warning: #856404;
    --warning-bg: #fff8e1;
    --purple: #6f42c1;
    --diff-add-bg: #eaffea;
    --diff-del-bg: #ffecec;
    --active-row-bg: #f3f6fb;
    --diff-pane-bg: #f3f6fb;
  }

  @media (prefers-color-scheme: dark) {
    :global(:root:not([data-theme="light"])) {
      --bg: #16181d;
      --surface: #1e2128;
      --surface-muted: #23262d;
      --text: #e8e8e8;
      --text-muted: #a0a5ad;
      --border: #3a3e46;
      --border-subtle: #2c2f36;
      --input-border: #4a4f59;
      --accent: #7ab0f7;
      --accent-bg: #1c2c40;
      --success: #55b979;
      --success-bg: #1b3324;
      --danger: #e8897d;
      --danger-text: #f3a99e;
      --danger-bg: #3a201d;
      --warning: #e0b04a;
      --warning-bg: #3a2f13;
      --purple: #b696f2;
      --diff-add-bg: #16321c;
      --diff-del-bg: #3a1c1c;
      --active-row-bg: #232a35;
      --diff-pane-bg: #1b222c;
    }
  }

  :global(:root[data-theme="dark"]) {
    --bg: #16181d;
    --surface: #1e2128;
    --surface-muted: #23262d;
    --text: #e8e8e8;
    --text-muted: #a0a5ad;
    --border: #3a3e46;
    --border-subtle: #2c2f36;
    --input-border: #4a4f59;
    --accent: #7ab0f7;
    --accent-bg: #1c2c40;
    --success: #55b979;
    --success-bg: #1b3324;
    --danger: #e8897d;
    --danger-text: #f3a99e;
    --danger-bg: #3a201d;
    --warning: #e0b04a;
    --warning-bg: #3a2f13;
    --purple: #b696f2;
    --diff-add-bg: #16321c;
    --diff-del-bg: #3a1c1c;
    --active-row-bg: #232a35;
    --diff-pane-bg: #1b222c;
  }

  :global(body) {
    background: var(--bg);
  }

  .sr-only {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border: 0;
  }

  /* Result regions receive focus programmatically (spec Sec 9.6: "Focus
     moves to the result or error summary after an operation") even though
     they aren't in the normal tab order - always show where focus landed. */
  [id$="-result"]:focus,
  #diff-pane:focus {
    outline: 2px solid var(--accent);
    outline-offset: 2px;
    border-radius: 0.2rem;
  }

  @media (prefers-reduced-motion: reduce) {
    * {
      animation-duration: 0.001ms !important;
      animation-iteration-count: 1 !important;
      transition-duration: 0.001ms !important;
      scroll-behavior: auto !important;
    }
  }

  main {
    max-width: 48rem;
    margin: 3rem auto;
    padding: 0 1.5rem;
    font-family: system-ui, sans-serif;
    color: var(--text);
    background: var(--bg);
  }
  .header-row {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 1rem;
  }
  h1 {
    margin-bottom: 0;
  }
  .tagline {
    margin-top: 0.25rem;
    color: var(--text-muted);
  }
  .theme-toggle {
    flex-shrink: 0;
    font: inherit;
    font-size: 0.85rem;
    padding: 0.35rem 0.8rem;
    border: 1px solid var(--border);
    border-radius: 999px;
    background: var(--surface);
    color: var(--text);
    cursor: pointer;
  }
  .theme-toggle:hover {
    border-color: var(--accent);
  }
  .card {
    border: 1px solid var(--border);
    border-radius: 0.5rem;
    padding: 1.25rem;
    margin-top: 1.5rem;
    background: var(--surface);
  }
  .card.error {
    border-color: var(--danger);
    background: var(--danger-bg);
    color: var(--danger-text);
  }
  .error-title {
    font-weight: 600;
    margin: 0 0 0.25rem;
  }
  .advanced-details {
    margin-top: 0.75rem;
    font-size: 0.85rem;
  }
  .advanced-details summary {
    cursor: pointer;
    color: inherit;
  }
  .advanced-details dl {
    display: grid;
    grid-template-columns: auto 1fr;
    gap: 0.25rem 0.75rem;
    margin: 0.5rem 0 0;
  }
  .advanced-details dt {
    color: inherit;
    opacity: 0.75;
  }
  .advanced-details dd {
    margin: 0;
    text-align: left;
    font-family: ui-monospace, monospace;
    overflow-wrap: anywhere;
  }
  .advanced-stderr {
    margin: 0.5rem 0 0;
    padding: 0.5rem;
    background: rgba(128, 128, 128, 0.15);
    border-radius: 0.35rem;
    font-family: ui-monospace, monospace;
    font-size: 0.8rem;
    white-space: pre-wrap;
    overflow-wrap: anywhere;
    max-height: 12rem;
    overflow-y: auto;
  }
  .card.success {
    border-color: var(--success);
    background: var(--success-bg);
    color: var(--success);
  }
  .branch {
    font-family: ui-monospace, monospace;
    color: var(--text-muted);
  }
  .state {
    font-weight: 600;
  }
  .notices {
    margin: 1rem 0 0;
    padding-left: 1.25rem;
    color: var(--text-muted);
    font-size: 0.9rem;
  }
  .notices li {
    margin-top: 0.25rem;
  }
  .update-section {
    margin-top: 1rem;
  }
  .identity-section {
    margin-top: 1rem;
    padding: 0.75rem;
    border: 1px solid var(--border-subtle);
    border-radius: 0.4rem;
    background: var(--surface-muted);
  }
  .identity-section h3 {
    margin: 0 0 0.35rem;
    font-size: 1rem;
  }
  .identity-section p {
    margin: 0 0 0.5rem;
    color: var(--text-muted);
    font-size: 0.9rem;
  }
  .identity-section .field {
    display: block;
    font-weight: 600;
    margin-top: 0.75rem;
    margin-bottom: 0.35rem;
  }
  .identity-section input[type="text"],
  .identity-section input[type="email"] {
    width: 100%;
    box-sizing: border-box;
    font: inherit;
    padding: 0.5rem 0.6rem;
    border: 1px solid var(--input-border);
    border-radius: 0.4rem;
    background: var(--surface);
    color: var(--text);
  }
  .identity-section .update-button {
    margin-top: 1rem;
  }
  .update-button {
    font: inherit;
    font-weight: 600;
    padding: 0.45rem 1rem;
    border: 1px solid var(--accent);
    border-radius: 0.4rem;
    background: var(--accent-bg);
    color: var(--accent);
    cursor: pointer;
  }
  .update-button:disabled {
    cursor: default;
    opacity: 0.6;
  }
  .update-success {
    color: var(--success);
    font-weight: 600;
    margin-top: 0.5rem;
  }
  dl {
    display: grid;
    grid-template-columns: 1fr auto;
    row-gap: 0.35rem;
    margin-top: 1rem;
  }
  dt {
    color: var(--text-muted);
  }
  dd {
    margin: 0;
    text-align: right;
    font-variant-numeric: tabular-nums;
  }

  .changes {
    margin-top: 2rem;
  }
  .change-list {
    list-style: none;
    margin: 0.75rem 0 0;
    padding: 0;
    border: 1px solid var(--border);
    border-radius: 0.5rem;
    overflow: hidden;
  }
  .change-row {
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
    padding: 0.5rem 0.75rem;
    border-bottom: 1px solid var(--border-subtle);
  }
  .change-row:last-child {
    border-bottom: none;
  }
  .change-row.active {
    background: var(--active-row-bg);
  }
  .change-row-main {
    display: flex;
    align-items: center;
    flex-wrap: wrap;
    gap: 0.6rem;
  }
  .change-row-actions {
    display: flex;
    flex-wrap: wrap;
    gap: 0.5rem;
  }
  .row-action-button {
    font: inherit;
    font-size: 0.8rem;
    padding: 0.3rem 0.7rem;
    border-radius: 0.4rem;
    border: 1px solid var(--input-border);
    background: var(--surface);
    color: var(--text);
    cursor: pointer;
  }
  .row-action-button.danger {
    border-color: var(--danger);
    color: var(--danger-text);
  }
  .row-action-button:disabled {
    cursor: default;
    opacity: 0.6;
  }
  .row-success {
    color: var(--success);
    font-weight: 600;
  }
  .change-select {
    display: flex;
  }
  .change-path {
    flex: 1;
    text-align: left;
    background: none;
    border: none;
    padding: 0;
    font: inherit;
    font-family: ui-monospace, monospace;
    font-size: 0.9rem;
    cursor: pointer;
    color: var(--text);
    min-width: 0;
    overflow-wrap: anywhere;
  }
  .change-path:hover,
  .change-path:focus-visible {
    text-decoration: underline;
  }
  .change-tag {
    font-size: 0.75rem;
    font-weight: 600;
    padding: 0.15rem 0.5rem;
    border-radius: 999px;
    border: 1px solid currentColor;
    white-space: nowrap;
  }
  .tag-new {
    color: var(--success);
  }
  .tag-changed {
    color: var(--warning);
  }
  .tag-renamed {
    color: var(--accent);
  }
  .tag-deleted {
    color: var(--danger);
  }
  .tag-conflicted {
    color: var(--danger-text);
  }
  .change-flag {
    font-size: 0.75rem;
    color: var(--text-muted);
    border: 1px solid var(--input-border);
    border-radius: 999px;
    padding: 0.15rem 0.5rem;
    white-space: nowrap;
  }
  .change-stats {
    font-family: ui-monospace, monospace;
    font-size: 0.8rem;
    white-space: nowrap;
  }
  .stat-add {
    color: var(--success);
  }
  .stat-del {
    color: var(--danger);
    margin-left: 0.35rem;
  }

  .diff-pane {
    margin-top: 1.5rem;
    border: 1px solid var(--accent);
    border-radius: 0.5rem;
    padding: 1.25rem;
    background: var(--diff-pane-bg);
    scroll-margin-top: 1rem;
  }
  .diff-pane-header {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 0.75rem;
  }
  .diff-pane h2 {
    margin: 0;
    font-family: ui-monospace, monospace;
    font-size: 1rem;
    overflow-wrap: anywhere;
  }
  .diff-close {
    flex-shrink: 0;
    font: inherit;
    font-size: 0.85rem;
    padding: 0.3rem 0.8rem;
    border: 1px solid var(--accent);
    border-radius: 0.4rem;
    background: var(--surface);
    color: var(--accent);
    cursor: pointer;
  }
  .diff-binary {
    color: var(--text-muted);
  }
  .diff-body {
    border: 1px solid var(--border);
    border-radius: 0.5rem;
    padding: 0.75rem;
    overflow-x: auto;
    font-family: ui-monospace, monospace;
    font-size: 0.85rem;
    line-height: 1.4;
    white-space: pre;
    tab-size: 4;
    background: var(--surface);
    color: var(--text);
  }
  .diff-line {
    display: block;
  }
  .diff-sign {
    display: inline-block;
    width: 1.25em;
    user-select: none;
  }
  .diff-add {
    background: var(--diff-add-bg);
    color: var(--success);
  }
  .diff-del {
    background: var(--diff-del-bg);
    color: var(--danger);
  }
  .diff-hunk {
    color: var(--purple);
    font-weight: 600;
  }
  .diff-header {
    color: var(--text-muted);
    font-weight: 600;
  }
  .load-more {
    margin-top: 0.75rem;
    font: inherit;
    padding: 0.4rem 0.9rem;
    border: 1px solid var(--input-border);
    border-radius: 0.4rem;
    background: var(--surface);
    color: var(--text);
    cursor: pointer;
  }
  .load-more:disabled {
    cursor: default;
    opacity: 0.6;
  }

  .commit-form {
    margin-top: 2rem;
    border: 1px solid var(--border);
    border-radius: 0.5rem;
    padding: 1.25rem;
    background: var(--surface);
  }
  .commit-form h2 {
    margin-top: 0;
  }
  .selection-count {
    color: var(--text-muted);
    margin-top: -0.5rem;
  }
  .commit-form .field {
    display: block;
    font-weight: 600;
    margin-top: 1rem;
    margin-bottom: 0.35rem;
  }
  .commit-form .required,
  .commit-form .optional {
    font-weight: 400;
    color: var(--text-muted);
  }
  .commit-form input[type="text"],
  .commit-form textarea {
    width: 100%;
    box-sizing: border-box;
    font: inherit;
    padding: 0.5rem 0.6rem;
    border: 1px solid var(--input-border);
    border-radius: 0.4rem;
    background: var(--surface);
    color: var(--text);
  }
  .commit-form textarea {
    resize: vertical;
  }
  .field-inline {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    margin-top: 1rem;
    font-weight: 600;
  }
  .tag-fields {
    margin-top: 0.5rem;
    padding: 0.75rem;
    border: 1px solid var(--border-subtle);
    border-radius: 0.4rem;
    background: var(--surface-muted);
  }
  .tag-fields .field:first-child {
    margin-top: 0;
  }
  .partial-failure {
    color: var(--warning);
  }
  .retry-button {
    margin-top: 0.75rem;
    font: inherit;
    padding: 0.4rem 0.9rem;
    border: 1px solid var(--warning);
    border-radius: 0.4rem;
    background: var(--warning-bg);
    color: var(--warning);
    cursor: pointer;
  }
  .retry-button:disabled {
    cursor: default;
    opacity: 0.6;
  }
  .save-button {
    margin-top: 1.25rem;
    font: inherit;
    font-weight: 600;
    padding: 0.5rem 1.1rem;
    border: 1px solid var(--success);
    border-radius: 0.4rem;
    background: var(--success-bg);
    color: var(--success);
    cursor: pointer;
  }
  .save-button:disabled {
    cursor: default;
    opacity: 0.5;
  }
</style>
