import { test, expect } from "@playwright/test";
import { startFixtureServer, type FixtureServer } from "./helpers";

test.describe("DIVERGED: human-judgment handoff", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("diverged");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("shows the handoff card instead of a one-click action", async ({ page }) => {
    await page.goto(server.url);

    await expect(page.getByText("Local and GitHub work need help to combine.")).toBeVisible();
    await expect(page.getByRole("heading", { name: "Get help from someone experienced with Git" })).toBeVisible();
  });

  test("Download diagnostic report downloads a text file", async ({ page }) => {
    await page.goto(server.url);

    const downloadPromise = page.waitForEvent("download");
    await page.getByRole("button", { name: "Download diagnostic report" }).click();
    const download = await downloadPromise;

    expect(download.suggestedFilename()).toMatch(/^gitneighbr-diagnostic-.*\.txt$/);
  });

  test("offers copyable merge and rebase recovery commands, run by the user themselves", async ({ page }) => {
    await page.goto(server.url);

    await expect(page.getByText("You'll need to run these yourself in a terminal")).toBeVisible();
    await expect(page.getByRole("group", { name: "Combine with a merge" })).toBeVisible();
    await expect(page.getByText("git merge origin/main", { exact: true })).toBeVisible();
    await expect(page.getByRole("group", { name: "Combine with a rebase" })).toBeVisible();
    await expect(page.getByText("git rebase origin/main", { exact: true })).toBeVisible();
  });
});

test.describe("CONFLICTED: in-progress merge recovery guidance", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("conflicted");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("names the in-progress merge and offers finish/cancel commands", async ({ page }) => {
    await page.goto(server.url);

    await expect(page.getByText("Some files contain changes that need help to combine.")).toBeVisible();
    await expect(page.getByText("You're in the middle of a merge that was started outside gitneighbr.")).toBeVisible();
    await expect(page.getByRole("group", { name: "Finish the merge" })).toBeVisible();
    await expect(page.getByRole("group", { name: "Cancel the merge" })).toBeVisible();
    await expect(page.getByText("git merge --abort", { exact: true })).toBeVisible();
  });

  test("Copy commands copies the recovery commands to the clipboard", async ({ page, context }) => {
    await context.grantPermissions(["clipboard-read", "clipboard-write"]);
    await page.goto(server.url);

    const cancelBlock = page.getByRole("group", { name: "Cancel the merge" });
    await cancelBlock.getByRole("button", { name: "Copy commands" }).click();
    await expect(cancelBlock.getByRole("button", { name: "Copied" })).toBeVisible();

    const clipboardText = await page.evaluate(() => navigator.clipboard.readText());
    expect(clipboardText).toBe("git merge --abort");
  });
});
