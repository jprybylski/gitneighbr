import { test, expect } from "@playwright/test";
import { rmSync } from "node:fs";
import { startFixtureServer, type FixtureServer } from "./helpers";

test.describe("theme toggle", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("clean-with-remote");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("cycles Auto -> Light -> Dark -> Auto, setting [data-theme] on <html>", async ({ page }) => {
    await page.goto(server.url);
    const html = page.locator("html");
    const toggle = page.getByRole("button", { name: /Color theme:/ });

    await expect(toggle).toHaveText("Auto");
    await expect(html).not.toHaveAttribute("data-theme");

    await toggle.click();
    await expect(toggle).toHaveText("Light");
    await expect(html).toHaveAttribute("data-theme", "light");

    await toggle.click();
    await expect(toggle).toHaveText("Dark");
    await expect(html).toHaveAttribute("data-theme", "dark");

    await toggle.click();
    await expect(toggle).toHaveText("Auto");
    await expect(html).not.toHaveAttribute("data-theme");
  });
});

test.describe("missing session token", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("clean-with-remote");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("shows the missing-token error when the URL has no #token fragment", async ({ page }) => {
    const url = new URL(server.url);
    url.hash = "";

    await page.goto(url.toString());

    await expect(
      page.getByRole("alert").getByText("Missing session token. Open this page via gitneighbr::open_repo()."),
    ).toBeVisible();
  });
});

test.describe("Advanced details on a real git failure", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("dirty-with-remote");
    // Break the "GitHub" stand-in after the server is already bound to
    // it, so sending (not committing, which is purely local) fails with
    // a genuine git stderr to show in the drawer.
    rmSync(server.remoteDir!, { recursive: true, force: true });
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("expands to show the failing git command and its stderr", async ({ page }) => {
    await page.goto(server.url);

    await page.getByLabel("Include README.md in the next snapshot").check();
    await page.getByLabel("Include notes.txt in the next snapshot").check();
    await page.getByLabel("Include scratch.log in the next snapshot").check();
    await page.getByLabel(/Summary/).fill("Update everything");
    await page.getByLabel("Also send to GitHub").check();
    await page.getByRole("button", { name: "Save and send" }).click();

    await expect(page.getByText(/^Not yet sent to GitHub: /)).toBeVisible();

    // Two "Advanced details" drawers render simultaneously here: the
    // standalone send section (changes.length is now 0) and the
    // commit-form success card both show the same sendError.
    await page.getByText("Advanced details").first().click();
    await expect(page.getByText("Command").first()).toBeVisible();
    await expect(page.getByText("Exit status").first()).toBeVisible();
  });
});
