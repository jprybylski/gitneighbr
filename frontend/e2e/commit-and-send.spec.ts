import { test, expect } from "@playwright/test";
import { startFixtureServer, type FixtureServer } from "./helpers";

// One evolving repo across both tests below (workers: 1, tests run in
// file order): the first commits just the tracked change, the second
// commits the rest with a version tag and sends everything to GitHub.
test.describe("commit and send", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("dirty-with-remote");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("Save snapshot commits only the selected file, leaving the rest unsaved", async ({ page }) => {
    await page.goto(server.url);

    // Every change starts pre-selected; deselect the two this test isn't
    // saving yet so only README.md is committed.
    await page.getByLabel("Include notes.txt in the next snapshot").uncheck();
    await page.getByLabel("Include scratch.log in the next snapshot").uncheck();
    await page.getByLabel(/Summary/).fill("Update README");
    // "Also send to GitHub" defaults to checked; uncheck it so this test
    // exercises the commit-only path and "Save snapshot" is the label.
    await page.getByLabel("Also send to GitHub").uncheck();
    await page.getByRole("button", { name: "Save snapshot" }).click();

    await expect(page.getByText(/^Saved snapshot .+\.$/)).toBeVisible();
    await expect(page.getByText("You have unsaved changes and saved snapshots waiting to be sent.")).toBeVisible();
  });

  test("Save and send commits the rest, tags the version, and pushes everything", async ({ page }) => {
    await page.goto(server.url);

    await page.getByLabel("Include notes.txt in the next snapshot").check();
    await page.getByLabel("Include scratch.log in the next snapshot").check();
    await page.getByLabel(/Summary/).fill("Add scratch files");
    await page.getByLabel("Also send to GitHub").check();
    await page.getByLabel("Mark this snapshot as a version").check();
    await page.getByLabel(/Version label/).fill("v1.0.0");
    await page.getByRole("button", { name: "Save and send" }).click();

    await expect(page.getByText(/^Saved snapshot .+\.$/)).toBeVisible();
    await expect(page.getByText("Marked as version v1.0.0.")).toBeVisible();
    // Once nothing is left to select, the standalone send section and the
    // commit-form's own success card both show the same sendSuccess text.
    await expect(page.getByText("Sent to origin/main.").first()).toBeVisible();
    await expect(page.getByText("Everything is saved and up to date.")).toBeVisible();
  });
});
