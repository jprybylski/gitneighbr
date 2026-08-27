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

  function getToken(): string | null {
    const match = window.location.hash.match(/token=([^&]+)/);
    return match ? decodeURIComponent(match[1]) : null;
  }

  const token = getToken();

  let status = $state<StatusData | null>(null);
  let errorMessage = $state<string | null>(null);
  let loading = $state(true);

  async function loadStatus() {
    if (!token) {
      errorMessage = "Missing session token. Open this page via gitneighbr::open_repo().";
      loading = false;
      return;
    }
    try {
      const res = await fetch("/api/v1/status", {
        headers: { Authorization: `Bearer ${token}` },
      });
      const envelope = (await res.json()) as Envelope<StatusData>;
      if (!envelope.ok || !envelope.data) {
        errorMessage = envelope.error?.message ?? `Request failed (${res.status}).`;
      } else {
        status = envelope.data;
        errorMessage = null;
      }
    } catch (err) {
      errorMessage = err instanceof Error ? err.message : "Could not reach the gitneighbr server.";
    } finally {
      loading = false;
    }
  }

  loadStatus();
</script>

<main>
  <h1>gitneighbr</h1>
  <p class="tagline">Be a good neighbor to your repository.</p>

  {#if loading}
    <p>Checking repository status&hellip;</p>
  {:else if errorMessage}
    <div class="card error">
      <p>{errorMessage}</p>
    </div>
  {:else if status}
    <div class="card">
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
  {/if}
</main>

<style>
  main {
    max-width: 32rem;
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
</style>
