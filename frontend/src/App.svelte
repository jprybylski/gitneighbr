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

  // spec Sec 9.6: "Focus moves to the result or error summary after an
  // operation." Each result region below has a stable `id` and
  // `tabindex="-1"` so it can receive focus programmatically without
  // joining the normal tab order; `tick()` waits for the just-set state to
  // actually reach the DOM before the focus call runs.
  async function focusRegion(id: string) {
    await tick();
    document.getElementById(id)?.focus();
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
      const [statusEnvelope, changesEnvelope] = await Promise.all([
        api<StatusData>("/api/v1/status"),
        api<{ changes: ChangeEntry[] }>("/api/v1/changes"),
      ]);

      if (!statusEnvelope.ok || !statusEnvelope.data) {
        errorMessage = statusEnvelope.error ?? { message: "Request failed." };
      } else {
        status = statusEnvelope.data;
        errorMessage = null;
      }

      if (changesEnvelope.ok && changesEnvelope.data) {
        selected = reconcileSelection(changes, changesEnvelope.data.changes);
        changes = changesEnvelope.data.changes;
      }
    } catch (err) {
      errorMessage = { message: err instanceof Error ? err.message : "Could not reach the gitneighbr server." };
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
      commitError = { message: err instanceof Error ? err.message : "Could not reach the gitneighbr server." };
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
      sendError = { message: err instanceof Error ? err.message : "Could not reach the gitneighbr server." };
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
      diffError = { message: err instanceof Error ? err.message : "Could not reach the gitneighbr server." };
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
      rowError = { message: err instanceof Error ? err.message : "Could not reach the gitneighbr server." };
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
      rowError = { message: err instanceof Error ? err.message : "Could not reach the gitneighbr server." };
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
      rowError = { message: err instanceof Error ? err.message : "Could not reach the gitneighbr server." };
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
      updateError = { message: err instanceof Error ? err.message : "Could not reach the gitneighbr server." };
    } finally {
      updating = false;
      await focusRegion("update-result");
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
    {#if err.advanced}{@render advancedDetails(err.advanced)}{/if}
  </div>
{/snippet}

<ConfirmDialog bind:this={confirmDialog} />

<main>
  <h1>gitneighbr</h1>
  <p class="tagline">Be a good neighbor to your repository.</p>

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
      {#if status.notices.length > 0}
        <ul class="notices">
          {#each status.notices as notice (notice.code)}
            <li>{notice.message}</li>
          {/each}
        </ul>
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
    outline: 2px solid #0056b3;
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
    color: #1a1a1a;
  }
  h1 {
    margin-bottom: 0;
  }
  .tagline {
    margin-top: 0.25rem;
    color: #555;
  }
  .card {
    border: 1px solid #ddd;
    border-radius: 0.5rem;
    padding: 1.25rem;
    margin-top: 1.5rem;
  }
  .card.error {
    border-color: #c0392b;
    background: #fdecea;
    color: #922b21;
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
    background: rgba(0, 0, 0, 0.05);
    border-radius: 0.35rem;
    font-family: ui-monospace, monospace;
    font-size: 0.8rem;
    white-space: pre-wrap;
    overflow-wrap: anywhere;
    max-height: 12rem;
    overflow-y: auto;
  }
  .card.success {
    border-color: #1e7e34;
    background: #eaf7ec;
    color: #1e7e34;
  }
  .branch {
    font-family: ui-monospace, monospace;
    color: #555;
  }
  .state {
    font-weight: 600;
  }
  .notices {
    margin: 1rem 0 0;
    padding-left: 1.25rem;
    color: #555;
    font-size: 0.9rem;
  }
  .notices li {
    margin-top: 0.25rem;
  }
  .update-section {
    margin-top: 1rem;
  }
  .update-button {
    font: inherit;
    font-weight: 600;
    padding: 0.45rem 1rem;
    border: 1px solid #0056b3;
    border-radius: 0.4rem;
    background: #eaf1fb;
    color: #0056b3;
    cursor: pointer;
  }
  .update-button:disabled {
    cursor: default;
    opacity: 0.6;
  }
  .update-success {
    color: #1e7e34;
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
    color: #555;
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
    border: 1px solid #ddd;
    border-radius: 0.5rem;
    overflow: hidden;
  }
  .change-row {
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
    padding: 0.5rem 0.75rem;
    border-bottom: 1px solid #eee;
  }
  .change-row:last-child {
    border-bottom: none;
  }
  .change-row.active {
    background: #f3f6fb;
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
    border: 1px solid #ccc;
    background: white;
    cursor: pointer;
  }
  .row-action-button.danger {
    border-color: #c0392b;
    color: #922b21;
  }
  .row-action-button:disabled {
    cursor: default;
    opacity: 0.6;
  }
  .row-success {
    color: #1e7e34;
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
    color: #1a1a1a;
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
    color: #1e7e34;
  }
  .tag-changed {
    color: #856404;
  }
  .tag-renamed {
    color: #0056b3;
  }
  .tag-deleted {
    color: #c0392b;
  }
  .tag-conflicted {
    color: #922b21;
  }
  .change-flag {
    font-size: 0.75rem;
    color: #555;
    border: 1px solid #ccc;
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
    color: #1e7e34;
  }
  .stat-del {
    color: #c0392b;
    margin-left: 0.35rem;
  }

  .diff-pane {
    margin-top: 1.5rem;
    border: 1px solid #0056b3;
    border-radius: 0.5rem;
    padding: 1.25rem;
    background: #f3f6fb;
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
    border: 1px solid #0056b3;
    border-radius: 0.4rem;
    background: white;
    color: #0056b3;
    cursor: pointer;
  }
  .diff-binary {
    color: #555;
  }
  .diff-body {
    border: 1px solid #ddd;
    border-radius: 0.5rem;
    padding: 0.75rem;
    overflow-x: auto;
    font-family: ui-monospace, monospace;
    font-size: 0.85rem;
    line-height: 1.4;
    white-space: pre;
    tab-size: 4;
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
    background: #eaffea;
    color: #1e7e34;
  }
  .diff-del {
    background: #ffecec;
    color: #c0392b;
  }
  .diff-hunk {
    color: #6f42c1;
    font-weight: 600;
  }
  .diff-header {
    color: #555;
    font-weight: 600;
  }
  .load-more {
    margin-top: 0.75rem;
    font: inherit;
    padding: 0.4rem 0.9rem;
    border: 1px solid #ccc;
    border-radius: 0.4rem;
    background: white;
    cursor: pointer;
  }
  .load-more:disabled {
    cursor: default;
    opacity: 0.6;
  }

  .commit-form {
    margin-top: 2rem;
    border: 1px solid #ddd;
    border-radius: 0.5rem;
    padding: 1.25rem;
  }
  .commit-form h2 {
    margin-top: 0;
  }
  .selection-count {
    color: #555;
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
    color: #555;
  }
  .commit-form input[type="text"],
  .commit-form textarea {
    width: 100%;
    box-sizing: border-box;
    font: inherit;
    padding: 0.5rem 0.6rem;
    border: 1px solid #ccc;
    border-radius: 0.4rem;
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
    border: 1px solid #eee;
    border-radius: 0.4rem;
    background: #fafafa;
  }
  .tag-fields .field:first-child {
    margin-top: 0;
  }
  .partial-failure {
    color: #856404;
  }
  .retry-button {
    margin-top: 0.75rem;
    font: inherit;
    padding: 0.4rem 0.9rem;
    border: 1px solid #856404;
    border-radius: 0.4rem;
    background: #fff8e1;
    color: #856404;
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
    border: 1px solid #1e7e34;
    border-radius: 0.4rem;
    background: #eaf7ec;
    color: #1e7e34;
    cursor: pointer;
  }
  .save-button:disabled {
    cursor: default;
    opacity: 0.5;
  }
</style>
