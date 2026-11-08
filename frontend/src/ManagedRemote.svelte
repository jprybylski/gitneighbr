<script lang="ts">
  // Send and Update in a managed session: nothing reaches the network until
  // the user asks, every outgoing or incoming snapshot and file is listed,
  // and the action is bound to exactly what was listed.

  type ApiError = { code?: string; title?: string; message: string };
  type Envelope<T> = { ok: boolean; data: T | null; error: ApiError | null; status_version?: number | null };

  type RemoteStatus = {
    configured: boolean;
    usable: boolean;
    problem: { code: string; message: string } | null;
    host: string | null;
    address: string | null;
    branch: string | null;
    has_token: boolean;
    checked: boolean;
  };

  type ReviewLabel = { id: string; label: string; message: string };

  type PreviewFile = {
    path: string;
    status: string;
    binary: boolean;
    size: number;
    symlink: boolean;
    review: string[];
    blocked: boolean;
  };

  type PreviewCommit = {
    sha: string;
    short: string;
    author: string;
    date: string;
    summary: string;
    merge: boolean;
    files: PreviewFile[];
  };

  type SendPreview = {
    preview_id: string;
    destination: { host: string; address: string; branch: string; remote: string; exists: boolean };
    commits: PreviewCommit[];
    totals: { commits: number; paths: number; bytes: number };
    blocked: string[];
    review: string[];
    can_send: boolean;
  };

  type ReceivePreview = {
    preview_id: string | null;
    commits: PreviewCommit[];
    touched: string[];
    blocked: { path: string; reason: "protected" | "local_changes" }[];
    can_update: boolean;
    up_to_date: boolean;
  };

  let {
    remote,
    reviewLabels = [],
    disabled = false,
    api,
    postApi,
    refresh,
  }: {
    remote: RemoteStatus;
    reviewLabels?: ReviewLabel[];
    disabled?: boolean;
    api: <T>(path: string) => Promise<Envelope<T>>;
    postApi: <T>(path: string, body: Record<string, unknown>) => Promise<Envelope<T>>;
    refresh: () => Promise<void>;
  } = $props();

  let address = $state("");
  let token = $state("");
  let username = $state("");
  let working = $state<string | null>(null);
  let error = $state<ApiError | null>(null);
  let success = $state<string | null>(null);
  let sendPreview = $state<SendPreview | null>(null);
  let receivePreview = $state<ReceivePreview | null>(null);
  let confirmed = $state(false);
  let confirmedReview = $state(false);

  const STATUS_LABEL: Record<string, string> = { A: "Added", M: "Changed", D: "Deleted", T: "Type changed" };
  const BLOCK_REASON: Record<string, string> = {
    protected: "is protected here and would be changed",
    local_changes: "has changes on this computer that aren't in a snapshot",
  };

  function sizeLabel(bytes: number): string {
    if (bytes < 1024) return `${bytes} B`;
    if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
    return `${(bytes / 1024 / 1024).toFixed(1)} MB`;
  }

  function labelFor(id: string): ReviewLabel | undefined {
    return reviewLabels.find((r) => r.id === id);
  }

  function reset() {
    error = null;
    success = null;
    confirmed = false;
    confirmedReview = false;
  }

  async function run<T>(name: string, action: () => Promise<Envelope<T>>): Promise<T | null> {
    working = name;
    error = null;
    try {
      const envelope = await action();
      if (!envelope.ok) {
        error = envelope.error ?? { message: "That didn't work." };
        return null;
      }
      return envelope.data;
    } catch {
      error = { message: "The version history service stopped responding." };
      return null;
    } finally {
      working = null;
      await refresh();
    }
  }

  async function connect() {
    reset();
    const done = await run("connect", () => postApi("api/v1/remote/connect", { url: address.trim() }));
    if (done) address = "";
  }

  async function saveToken() {
    reset();
    const done = await run("token", () => postApi("api/v1/remote/token", { token: token.trim(), username: username.trim() }));
    token = "";
    if (done) success = "Access token saved for this session only.";
  }

  async function forgetToken() {
    reset();
    await run("token", () => postApi("api/v1/remote/token", { token: "" }));
  }

  async function checkSend() {
    reset();
    sendPreview = null;
    receivePreview = null;
    const checked = await run("check-send", () => postApi("api/v1/remote/check", {}));
    if (!checked) return;
    sendPreview = await run("check-send", () => api<SendPreview>("api/v1/send/preview"));
  }

  async function send() {
    if (!sendPreview) return;
    const preview = sendPreview;
    success = null;
    const done = await run("send", () =>
      postApi("api/v1/send", { preview_id: preview.preview_id, confirm: confirmed, confirm_review: confirmedReview }),
    );
    if (done) {
      success = `Sent ${preview.totals.commits} snapshot${preview.totals.commits === 1 ? "" : "s"} to ${preview.destination.host}.`;
      sendPreview = null;
      confirmed = false;
      confirmedReview = false;
    }
  }

  async function checkReceive() {
    reset();
    sendPreview = null;
    receivePreview = null;
    const checked = await run("check-receive", () => postApi("api/v1/remote/check", {}));
    if (!checked) return;
    receivePreview = await run("check-receive", () => api<ReceivePreview>("api/v1/receive/preview"));
  }

  async function receive() {
    if (!receivePreview?.preview_id) return;
    const preview = receivePreview;
    const done = await run("receive", () => postApi("api/v1/receive", { preview_id: preview.preview_id, confirm: confirmed }));
    if (done) {
      success = `Got ${preview.commits.length} update${preview.commits.length === 1 ? "" : "s"}.`;
      receivePreview = null;
      confirmed = false;
    }
  }
</script>

{#snippet commitList(commits: PreviewCommit[])}
  <ol class="preview-commits">
    {#each commits as commit (commit.sha)}
      <li>
        <details>
          <summary>
            <span class="preview-summary">{commit.summary || "(no summary)"}</span>
            <span class="preview-meta">{commit.short} · {commit.author} · {commit.files.length} file{commit.files.length === 1 ? "" : "s"}</span>
          </summary>
          <ul class="preview-files">
            {#each commit.files as file (file.path)}
              <li class:blocked={file.blocked}>
                <span class="path-text">{file.path}</span>
                <span class="change-flag">{STATUS_LABEL[file.status] ?? file.status}</span>
                {#if file.status !== "D"}<span class="change-flag">{file.binary ? "Binary, " : ""}{sizeLabel(file.size)}</span>{/if}
                {#if file.symlink}<span class="change-flag">Link</span>{/if}
                {#each file.review as id (id)}<span class="change-flag review-flag">{labelFor(id)?.label ?? id}</span>{/each}
                {#if file.blocked}<span class="change-flag blocked-flag">Can't be sent</span>{/if}
              </li>
            {/each}
          </ul>
        </details>
      </li>
    {/each}
  </ol>
{/snippet}

<section class="identity-section managed-remote" aria-label="Shared copy">
  <h3>Shared copy</h3>
  <p class="changes-hint">
    Sending copies snapshots, and every file in them, to another computer. Nothing is sent or fetched until
    you choose one of the actions below.
  </p>

  {#if !remote.configured}
    <label class="field" for="remote-address">HTTPS address of the shared repository</label>
    <input id="remote-address" type="text" bind:value={address} placeholder="https://host/owner/project.git" disabled={disabled || working !== null} />
    <button type="button" class="update-button" disabled={disabled || working !== null || !address.trim()} onclick={connect}>
      {working === "connect" ? "Saving…" : "Connect"}
    </button>
  {:else if remote.problem}
    <p class="policy-error">{remote.problem.message}</p>
  {:else}
    <p>Destination: <strong>{remote.host}</strong>, branch <strong>{remote.branch}</strong></p>
    {#if remote.has_token}
      <p class="changes-hint">
        An access token for {remote.host} is set for this session.
        <button type="button" class="secondary-button" disabled={working !== null} onclick={forgetToken}>Forget it</button>
      </p>
    {:else}
      <label class="field" for="remote-token">Access token for {remote.host} <span class="optional">(kept only until this window closes)</span></label>
      <input id="remote-token" type="password" autocomplete="off" bind:value={token} disabled={working !== null} />
      <details>
        <summary>User name (only if your server needs one)</summary>
        <input id="remote-username" type="text" autocomplete="off" bind:value={username} placeholder="x-access-token" disabled={working !== null} />
      </details>
      <button type="button" class="secondary-button" disabled={working !== null || !token.trim()} onclick={saveToken}>
        {working === "token" ? "Saving…" : "Use this token"}
      </button>
    {/if}

    <div class="update-section">
      <button type="button" class="update-button" disabled={disabled || working !== null} onclick={checkSend}>
        {working === "check-send" ? "Checking…" : "Check what would be sent"}
      </button>
      <button type="button" class="secondary-button" disabled={disabled || working !== null} onclick={checkReceive}>
        {working === "check-receive" ? "Checking…" : "Check for updates"}
      </button>
    </div>
  {/if}

  {#if disabled}
    <p class="changes-hint" role="status">Sending and updates are paused while other work in this folder is running or unsaved.</p>
  {/if}

  <div id="remote-result" tabindex="-1" aria-live="polite">
    {#if error}
      <div class="card error" role="alert">
        {#if error.title}<p class="error-title">{error.title}</p>{/if}
        <p>{error.message}</p>
      </div>
    {:else if success}
      <p class="update-success" role="status">{success}</p>
    {/if}
  </div>

  {#if sendPreview}
    <div class="card preview">
      <h4>What would be sent</h4>
      <p>
        To <strong>{sendPreview.destination.host}</strong> ({sendPreview.destination.address}), branch
        <strong>{sendPreview.destination.branch}</strong>{sendPreview.destination.exists ? "" : " (new)"}:
        {sendPreview.totals.commits} snapshot{sendPreview.totals.commits === 1 ? "" : "s"},
        {sendPreview.totals.paths} file{sendPreview.totals.paths === 1 ? "" : "s"}, {sizeLabel(sendPreview.totals.bytes)}.
      </p>
      {#if sendPreview.commits.length === 0}
        <p>Nothing new to send.</p>
      {:else}
        <p class="changes-hint">Every snapshot that would be sent is listed, not just the latest. Open each one to see its files.</p>
        {@render commitList(sendPreview.commits)}
      {/if}
      {#if sendPreview.blocked.length > 0}
        <p class="policy-error" role="alert">
          These files can't be sent from here: {sendPreview.blocked.join(", ")}. Leaving them out of a new snapshot doesn't
          remove them from earlier snapshots.
        </p>
      {/if}
      {#if sendPreview.can_send}
        <p class="sensitivity-warning">
          Study files, results, reports and their history can contain identifiable or sensitive information. Send only
          what you're allowed to share with {sendPreview.destination.host}.
        </p>
        {#each sendPreview.review as id (id)}
          <p class="review-note"><strong>{labelFor(id)?.label ?? id}:</strong> {labelFor(id)?.message ?? ""}</p>
        {/each}
        <label class="field-inline">
          <input type="checkbox" bind:checked={confirmed} />
          I've reviewed every snapshot and file listed above and may send them to {sendPreview.destination.host}.
        </label>
        {#if sendPreview.review.length > 0}
          <label class="field-inline">
            <input type="checkbox" bind:checked={confirmedReview} />
            I've checked the files marked for extra review.
          </label>
        {/if}
        <button
          type="button"
          class="save-button"
          disabled={disabled || working !== null || !confirmed || (sendPreview.review.length > 0 && !confirmedReview)}
          onclick={send}
        >
          {working === "send" ? "Sending…" : "Send these snapshots"}
        </button>
      {/if}
    </div>
  {/if}

  {#if receivePreview}
    <div class="card preview">
      <h4>Updates from {remote.host}</h4>
      {#if receivePreview.up_to_date}
        <p>This project already has everything from the shared copy.</p>
      {:else}
        <p>
          {receivePreview.commits.length} snapshot{receivePreview.commits.length === 1 ? "" : "s"} would arrive, changing
          {receivePreview.touched.length} file{receivePreview.touched.length === 1 ? "" : "s"} in this folder.
        </p>
        {@render commitList(receivePreview.commits)}
        {#each receivePreview.blocked as block (block.path + block.reason)}
          <p class="policy-error">'{block.path}' {BLOCK_REASON[block.reason]}, so these updates can't be applied.</p>
        {/each}
        {#if receivePreview.can_update}
          <label class="field-inline">
            <input type="checkbox" bind:checked={confirmed} />
            I've reviewed these updates; replace the listed files in this folder with the shared versions.
          </label>
          <button type="button" class="save-button" disabled={disabled || working !== null || !confirmed} onclick={receive}>
            {working === "receive" ? "Getting updates…" : "Get these updates"}
          </button>
        {/if}
      {/if}
    </div>
  {/if}
</section>

<style>
  .preview-commits {
    padding-left: 1.25rem;
  }
  .preview-summary {
    font-weight: 600;
  }
  .preview-meta {
    margin-left: 0.5rem;
    color: var(--muted, #555);
    font-size: 0.9em;
  }
  .preview-files {
    list-style: none;
    padding-left: 0.5rem;
  }
  .preview-files li {
    display: flex;
    flex-wrap: wrap;
    gap: 0.4rem;
    align-items: baseline;
    padding: 0.15rem 0;
  }
  .preview-files li.blocked .path-text {
    text-decoration: underline wavy;
  }
  .sensitivity-warning,
  .review-note {
    margin: 0.5rem 0;
  }
</style>
