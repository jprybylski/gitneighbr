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
});
