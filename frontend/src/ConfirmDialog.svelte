<script lang="ts">
  // A single reusable confirmation dialog (spec Sec 9.6: destructive
  // confirmation dialogs trap focus and support Escape to cancel). Built on
  // the native <dialog>/showModal(), which gives focus trapping, an
  // Escape-triggered "cancel" event, and focus-return-to-trigger for free in
  // every current browser -- no custom focus-trap JS to get wrong.
  //
  // Usage: mount once (`<ConfirmDialog bind:this={confirmDialog} />`), then
  // `const confirmed = await confirmDialog.confirm({ title, message, ... })`
  // from anywhere in the app. Only one confirmation can be in flight at a
  // time, which matches the single-mutation-at-a-time nature of every
  // action this dialog guards.
  let dialogEl: HTMLDialogElement | undefined = $state();
  let titleText = $state("");
  let message = $state("");
  let confirmLabel = $state("Confirm");
  let cancelLabel = $state("Cancel");
  let danger = $state(false);
  let resolveFn: ((confirmed: boolean) => void) | null = null;

  export function confirm(options: {
    title: string;
    message: string;
    confirmLabel?: string;
    cancelLabel?: string;
    danger?: boolean;
  }): Promise<boolean> {
    titleText = options.title;
    message = options.message;
    confirmLabel = options.confirmLabel ?? "Confirm";
    cancelLabel = options.cancelLabel ?? "Cancel";
    danger = options.danger ?? false;
    dialogEl?.showModal();
    return new Promise((resolve) => {
      resolveFn = resolve;
    });
  }

  function settle(result: boolean) {
    const resolve = resolveFn;
    resolveFn = null;
    dialogEl?.close();
    resolve?.(result);
  }

  // Fires for every close, including native Escape/cancel handling -
  // `resolveFn` is already null by then for a button-driven close, so this
  // only settles the promise as "cancelled" when nothing else has.
  function handleClose() {
    if (resolveFn) settle(false);
  }
</script>

<dialog bind:this={dialogEl} class="confirm-dialog" onclose={handleClose} aria-labelledby="confirm-dialog-title">
  <h2 id="confirm-dialog-title">{titleText}</h2>
  <p>{message}</p>
  <div class="dialog-actions">
    <button type="button" onclick={() => settle(false)}>{cancelLabel}</button>
    <button type="button" class="confirm-action" class:danger onclick={() => settle(true)}>{confirmLabel}</button>
  </div>
</dialog>

<style>
  .confirm-dialog {
    max-width: 26rem;
    width: calc(100vw - 2.5rem);
    box-sizing: border-box;
    border: 1px solid #ccc;
    border-radius: 0.6rem;
    padding: 1.25rem;
    font-family: system-ui, sans-serif;
    color: #1a1a1a;
  }
  .confirm-dialog::backdrop {
    background: rgba(0, 0, 0, 0.4);
  }
  .confirm-dialog h2 {
    margin: 0 0 0.5rem;
    font-size: 1.1rem;
  }
  .confirm-dialog p {
    margin: 0;
    color: #333;
  }
  .dialog-actions {
    display: flex;
    justify-content: flex-end;
    gap: 0.6rem;
    margin-top: 1.25rem;
  }
  .dialog-actions button {
    font: inherit;
    padding: 0.45rem 1rem;
    border-radius: 0.4rem;
    border: 1px solid #ccc;
    background: white;
    cursor: pointer;
  }
  .confirm-action {
    border-color: #0056b3;
    background: #eaf1fb;
    color: #0056b3;
    font-weight: 600;
  }
  .confirm-action.danger {
    border-color: #c0392b;
    background: #fdecea;
    color: #922b21;
  }

  @media (prefers-reduced-motion: reduce) {
    .confirm-dialog,
    .confirm-dialog::backdrop {
      transition: none !important;
      animation: none !important;
    }
  }
</style>
