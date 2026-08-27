<script lang="ts">
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

  type Envelope<T> = {
    ok: boolean;
    data: T | null;
    error: { code: string; message: string; recoverable: boolean } | null;
  };

  const STATE_COPY: Record<string, string> = {
    READY: "Everything is saved and up to date.",
    CHANGES_ONLY: "You have unsaved changes.",
    LOCAL_ONLY: "You have saved snapshots waiting to be sent to GitHub.",
    CHANGES_AND_LOCAL: "You have unsaved changes and saved snapshots waiting to be sent.",
    REMOTE_ONLY_CLEAN: "GitHub has newer updates you don't have yet.",
    REMOTE_ONLY_DIRTY: "GitHub has newer updates, and you also have unsaved changes.",
    DIVERGED: "Local and GitHub work need help to combine.",
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
  let changes = $state<ChangeEntry[]>([]);
  let selected = $state<Set<string>>(new Set());
  let errorMessage = $state<string | null>(null);
  let loading = $state(true);

  let activePath = $state<string | null>(null);
  let diff = $state<DiffData | null>(null);
  let diffError = $state<string | null>(null);
  let diffLoading = $state(false);

  let summary = $state("");
  let details = $state("");
  let committing = $state(false);
  let commitError = $state<string | null>(null);
  let commitSuccess = $state<string | null>(null);

  const summaryLength = $derived(summary.trim().length);
  const summaryValid = $derived(summaryLength >= 3 && summaryLength <= 72);
  const canSave = $derived(selected.size > 0 && summaryValid && !committing);

  async function api<T>(path: string): Promise<Envelope<T>> {
    const res = await fetch(path, {
      headers: { Authorization: `Bearer ${token}` },
    });
    return (await res.json()) as Envelope<T>;
  }

  async function postApi<T>(path: string, body: unknown): Promise<Envelope<T>> {
    const res = await fetch(path, {
      method: "POST",
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify(body),
    });
    return (await res.json()) as Envelope<T>;
  }

  async function loadStatus() {
    if (!token) {
      errorMessage = "Missing session token. Open this page via gitneighbr::open_repo().";
      loading = false;
      return;
    }
    try {
      const [statusEnvelope, changesEnvelope] = await Promise.all([
        api<StatusData>("/api/v1/status"),
        api<{ changes: ChangeEntry[] }>("/api/v1/changes"),
      ]);

      if (!statusEnvelope.ok || !statusEnvelope.data) {
        errorMessage = statusEnvelope.error?.message ?? "Request failed.";
      } else {
        status = statusEnvelope.data;
        errorMessage = null;
      }

      if (changesEnvelope.ok && changesEnvelope.data) {
        changes = changesEnvelope.data.changes;
        selected = new Set(changes.map((c) => c.path));
      }
    } catch (err) {
      errorMessage = err instanceof Error ? err.message : "Could not reach the gitneighbr server.";
    } finally {
      loading = false;
    }
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

  async function saveSnapshot() {
    if (!canSave) return;
    commitError = null;
    commitSuccess = null;
    committing = true;
    try {
      const envelope = await postApi<{ sha: string; summary: string }>("/api/v1/commit", {
        paths: Array.from(selected),
        summary: summary.trim(),
        details: details.trim() || undefined,
      });
      if (!envelope.ok || !envelope.data) {
        commitError = envelope.error?.message ?? "Could not save this snapshot.";
      } else {
        commitSuccess = `Saved snapshot ${envelope.data.sha}.`;
        summary = "";
        details = "";
        activePath = null;
        diff = null;
        await loadStatus();
      }
    } catch (err) {
      commitError = err instanceof Error ? err.message : "Could not reach the gitneighbr server.";
    } finally {
      committing = false;
    }
  }

  async function openDiff(path: string) {
    activePath = path;
    diff = null;
    diffError = null;
    diffLoading = true;
    try {
      const envelope = await api<DiffData>(`/api/v1/diff?path=${encodeURIComponent(path)}`);
      if (!envelope.ok || !envelope.data) {
        diffError = envelope.error?.message ?? "Could not load this diff.";
      } else {
        diff = envelope.data;
      }
    } catch (err) {
      diffError = err instanceof Error ? err.message : "Could not reach the gitneighbr server.";
    } finally {
      diffLoading = false;
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

  function diffLineText(line: string): string {
    if (line.startsWith("+++") || line.startsWith("---") || line.startsWith("@@")) return line;
    if (line.startsWith("+") || line.startsWith("-") || line.startsWith(" ")) return line.slice(1);
    return line;
  }

  loadStatus();
</script>

<main>
  <h1>gitneighbr</h1>
  <p class="tagline">Be a good neighbor to your repository.</p>

  {#if loading}
    <p>Checking repository status&hellip;</p>
  {:else if errorMessage}
    <div class="card error" role="alert">
      <p>{errorMessage}</p>
    </div>
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
      </dl>
    </div>

    {#if changes.length > 0}
      <section class="changes" aria-label="Changed files">
        <h2>Changes</h2>
        <ul class="change-list">
          {#each changes as change (change.path)}
            <li class="change-row" class:active={activePath === change.path}>
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
            </li>
          {/each}
        </ul>
      </section>

      <section class="commit-form" aria-label="Save a snapshot">
        <h2>Save snapshot</h2>
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

        {#if commitError}
          <div class="card error" role="alert"><p>{commitError}</p></div>
        {/if}
        {#if commitSuccess}
          <div class="card success" role="status"><p>{commitSuccess}</p></div>
        {/if}

        <button type="button" class="save-button" disabled={!canSave} onclick={saveSnapshot}>
          {committing ? "Saving…" : "Save snapshot"}
        </button>
      </section>

      {#if activePath}
        <section class="diff-pane" aria-label={`Diff for ${activePath}`} aria-live="polite">
          <h2>{activePath}</h2>
          {#if diffLoading && !diff}
            <p>Loading diff&hellip;</p>
          {:else if diffError}
            <div class="card error" role="alert"><p>{diffError}</p></div>
          {:else if diff}
            {#if diff.binary}
              <p class="diff-binary">This is a binary file. No text diff is available.</p>
            {:else if diff.lines.length === 0}
              <p>No differences to show.</p>
            {:else}
              <pre class="diff-body"><code
                >{#each diff.lines as line}<span class="diff-line {diffLineClass(line)}"
                    ><span class="diff-sign" aria-hidden="true">{diffLineSign(line)}</span
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
    {/if}
  {/if}
</main>

<style>
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
    align-items: center;
    gap: 0.6rem;
    padding: 0.5rem 0.75rem;
    border-bottom: 1px solid #eee;
  }
  .change-row:last-child {
    border-bottom: none;
  }
  .change-row.active {
    background: #f3f6fb;
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
  }
  .diff-pane h2 {
    font-family: ui-monospace, monospace;
    font-size: 1rem;
    overflow-wrap: anywhere;
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
