import { defineConfig, devices } from "@playwright/test";

/**
 * Real-browser coverage for the gitneighbr Svelte app, modeled on
 * ~/deckifyr/web/playwright.config.ts (issue #29 asks for Playwright
 * "as per what is done with deckifyr"). One important difference: this
 * app has no generic reset/discard endpoint (spec: no automation for
 * anything requiring human judgment), so there is no single shared
 * `webServer` here. Each spec file starts its own scratch repo and its
 * own `gitneighbr::open_repo()` session via `e2e/helpers.ts`'s
 * `startFixtureServer()` in a `test.beforeAll`, and tears it down in
 * `test.afterAll` -- see that file's own comment for the full reasoning.
 *
 * `workers: 1` / `fullyParallel: false` for the same reason deckifyr's
 * config gives: predictable, one-thing-at-a-time execution against real
 * background server processes rather than fighting to make N of them
 * behave under parallel load.
 */
export default defineConfig({
  testDir: "./e2e",
  fullyParallel: false,
  workers: 1,
  retries: 0,
  reporter: "list",
  use: {
    trace: "retain-on-failure",
  },
  projects: [{ name: "chromium", use: { ...devices["Desktop Chrome"] } }],
});
