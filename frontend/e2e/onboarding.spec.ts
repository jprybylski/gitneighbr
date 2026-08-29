import { test, expect } from "@playwright/test";
import { startFixtureServer, type FixtureServer } from "./helpers";

test.describe("NOT_REPOSITORY onboarding: initialize", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("not-a-repo");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("shows both onboarding paths for a folder that isn't a Git project", async ({ page }) => {
    await page.goto(server.url);
    await expect(page.getByText("This folder isn't a Git project.")).toBeVisible();
    await expect(page.getByRole("heading", { name: "Initialize a Git repository here" })).toBeVisible();
    await expect(page.getByRole("heading", { name: "Or clone an existing GitHub repository" })).toBeVisible();
  });

  test("Initialize a Git repository here moves the app to NO_UPSTREAM", async ({ page }) => {
    await page.goto(server.url);
    await page.getByRole("button", { name: "Initialize a Git repository here" }).click();

    await expect(page.getByText("This branch isn't connected to GitHub yet.")).toBeVisible();
    await expect(page.getByText("Branch: main")).toBeVisible();
  });
});

test.describe("NOT_REPOSITORY onboarding: clone", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("not-a-repo-with-clone-source");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("Or clone an existing GitHub repository clones into the folder", async ({ page }) => {
    await page.goto(server.url);

    await page.getByLabel("GitHub repository address").fill(server.remoteDir!);
    await page.getByRole("button", { name: "Clone into this folder" }).click();

    await expect(page.getByText("Everything is saved and up to date.")).toBeVisible();
    await expect(page.getByText("Branch: main")).toBeVisible();
  });
});
