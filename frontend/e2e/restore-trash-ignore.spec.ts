import { test, expect } from "@playwright/test";
import { startFixtureServer, type FixtureServer } from "./helpers";

// One repo, three independent target files (README.md modified, notes.txt
// and scratch.log untracked) so restore/trash/ignore never touch each
// other's target -- test order within the file doesn't matter here.
test.describe("restore, trash, and ignore via ConfirmDialog", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("dirty-with-remote");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("Cancel leaves the file untouched, then Restore discards its unsaved changes", async ({ page }) => {
    await page.goto(server.url);
    const restoreButton = page.getByRole("button", { name: "Restore last saved version: README.md" });

    await restoreButton.click();
    await expect(page.getByRole("heading", { name: "Restore last saved version?" })).toBeVisible();
    await page.getByRole("button", { name: "Cancel" }).click();
    await expect(page.getByRole("heading", { name: "Restore last saved version?" })).not.toBeVisible();
    await expect(page.getByLabel("Include README.md in the next snapshot")).toBeVisible();

    await restoreButton.click();
    await page.getByRole("button", { name: "Restore", exact: true }).click();

    await expect(page.getByText('Restored "README.md" to its last saved version.')).toBeVisible();
    await expect(page.getByLabel("Include README.md in the next snapshot")).not.toBeVisible();
  });

  test("Remove moves an untracked file to the trash", async ({ page }) => {
    await page.goto(server.url);

    await page.getByRole("button", { name: "Remove notes.txt" }).click();
    await expect(page.getByRole("heading", { name: "Move to trash?" })).toBeVisible();
    await page.getByRole("button", { name: "Move to trash", exact: true }).click();

    await expect(page.getByText('Moved "notes.txt" to the trash.')).toBeVisible();
    await expect(page.getByLabel("Include notes.txt in the next snapshot")).not.toBeVisible();
  });

  test("Escape cancels the dialog, then Add rule stops showing the file", async ({ page }) => {
    await page.goto(server.url);
    const ignoreButton = page.getByRole("button", { name: "Stop showing this file: scratch.log" });

    await ignoreButton.click();
    await expect(page.getByRole("heading", { name: "Stop showing this file?" })).toBeVisible();
    await page.keyboard.press("Escape");
    await expect(page.getByRole("heading", { name: "Stop showing this file?" })).not.toBeVisible();
    await expect(page.getByLabel("Include scratch.log in the next snapshot")).toBeVisible();

    await ignoreButton.click();
    await page.getByRole("button", { name: "Add rule" }).click();

    await expect(page.getByText(/^Added ".*" to \.gitignore\.$/)).toBeVisible();
    await expect(page.getByLabel("Include scratch.log in the next snapshot")).not.toBeVisible();
  });
});
