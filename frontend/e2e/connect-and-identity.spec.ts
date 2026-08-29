import { test, expect } from "@playwright/test";
import { startFixtureServer, type FixtureServer } from "./helpers";

test.describe("NO_UPSTREAM: connect to GitHub", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("no-upstream-with-publish-target");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("Connect and send to GitHub attaches a remote and pushes what's saved", async ({ page }) => {
    await page.goto(server.url);
    await expect(page.getByText("This branch isn't connected to GitHub yet.")).toBeVisible();

    await page.getByLabel("GitHub repository address").fill(server.remoteDir!);
    await page.getByRole("button", { name: "Connect and send to GitHub" }).click();

    await expect(page.getByText(/Connected to .* and sent 1 snapshot\./)).toBeVisible();
    await expect(page.getByText("Everything is saved and up to date.")).toBeVisible();
  });
});

test.describe("IDENTITY_INCOMPLETE: guided identity setup", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("missing-identity");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("Before your first snapshot prompts for and saves a project-only identity", async ({ page }) => {
    await page.goto(server.url);

    await expect(page.getByRole("heading", { name: "Before your first snapshot" })).toBeVisible();

    await page.getByLabel("Your name").fill("Ada Lovelace");
    await page.getByLabel("Your email").fill("ada@example.com");
    await page.getByLabel("Just for this project").check();
    await page.getByRole("button", { name: "Save my name and email" }).click();

    // The section (including its own success message) is gated on the
    // IDENTITY_INCOMPLETE notice, which the same save clears -- both
    // updates land in the same render, so the section just disappears
    // rather than the success text ever being independently observable.
    await expect(page.getByRole("heading", { name: "Before your first snapshot" })).not.toBeVisible();

    // Persisted server-side, not just a client-only dismissal.
    await page.reload();
    await expect(page.getByRole("heading", { name: "Before your first snapshot" })).not.toBeVisible();
  });
});
