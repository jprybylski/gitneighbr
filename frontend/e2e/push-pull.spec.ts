import { test, expect } from "@playwright/test";
import { startFixtureServer, type FixtureServer } from "./helpers";

test.describe("LOCAL_ONLY: standalone send", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("local-only-with-remote");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("Send to GitHub pushes a local-only commit with nothing to select", async ({ page }) => {
    await page.goto(server.url);
    await expect(page.getByText("You have saved snapshots waiting to be sent to GitHub.")).toBeVisible();

    await page.getByRole("button", { name: "Send to GitHub" }).click();

    await expect(page.getByText("Sent to origin/main.")).toBeVisible();
    await expect(page.getByText("Everything is saved and up to date.")).toBeVisible();
  });
});

test.describe("REMOTE_ONLY_CLEAN: fast-forward update", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("remote-ahead");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("Get updates from GitHub fast-forwards the local branch", async ({ page }) => {
    await page.goto(server.url);
    await expect(page.getByText("GitHub has newer updates you don't have yet.")).toBeVisible();

    await page.getByRole("button", { name: "Get updates from GitHub" }).click();

    await expect(page.getByText(/^Updated to .+ from origin\/main\.$/)).toBeVisible();
    await expect(page.getByText("Everything is saved and up to date.")).toBeVisible();
  });
});
