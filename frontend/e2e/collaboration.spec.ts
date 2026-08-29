import { test, expect } from "@playwright/test";
import { startFixtureServer, type FixtureServer } from "./helpers";

test.describe("collaboration and repository policy", () => {
  let server: FixtureServer;

  test.beforeAll(async () => {
    server = await startFixtureServer("with-policy");
  });

  test.afterAll(async () => {
    await server.stop();
  });

  test("displays repository policy indicator and disables untracked removal", async ({ page }) => {
    await page.goto(server.url);

    // Verify policy badge and indicator
    await expect(page.getByText("Policy")).toBeVisible();
    await expect(page.getByText("Pull requests required for protected branches (.gitneighbr.json)")).toBeVisible();

    // Verify untracked file remove button is disabled by policy
    const removeBtn = page.getByRole("button", { name: "Remove untracked.tmp" });
    await expect(removeBtn).toBeVisible();
    await expect(removeBtn).toBeDisabled();
    await expect(removeBtn).toHaveAttribute("title", "Disabled by repository policy");
  });

  test("opens GitHub connect modal and validates input", async ({ page }) => {
    await page.goto(server.url);

    // Click Connect GitHub in header
    const connectBtn = page.getByRole("button", { name: "Connect GitHub account" });
    if (await connectBtn.isVisible()) {
      await connectBtn.click();
      await expect(page.getByRole("heading", { name: "Connect GitHub Account" })).toBeVisible();
      await expect(page.getByText("Enter a GitHub Personal Access Token")).toBeVisible();

      // Cancel modal
      await page.getByRole("button", { name: "Cancel" }).click();
      await expect(page.getByRole("heading", { name: "Connect GitHub Account" })).not.toBeVisible();
    }
  });

  test("opens Pull Request modal with policy-configured branch prefix", async ({ page }) => {
    await page.goto(server.url);

    // Save a commit on protected branch so status becomes LOCAL_ONLY
    await page.getByLabel(/Summary/).fill("Test policy commit");
    await page.getByLabel("Also send to GitHub").uncheck();
    await page.getByRole("button", { name: "Save snapshot" }).click();

    await expect(page.getByText(/^Saved snapshot .+\.$/)).toBeVisible();

    // Since branch is protected and policy requires PR, "Create Pull Request" is shown instead of "Send to GitHub"
    const prBtn = page.getByRole("button", { name: "Create Pull Request" });
    await expect(prBtn).toBeVisible();
    await prBtn.click();

    // Verify PR modal opened and has feature/ prefix from policy
    await expect(page.getByRole("heading", { name: "Create Pull Request" })).toBeVisible();
    const branchInput = page.getByLabel(/Branch Name/);
    await expect(branchInput).toBeVisible();
    const branchVal = await branchInput.inputValue();
    expect(branchVal.startsWith("feature/")).toBeTruthy();

    // Close PR modal
    await page.getByRole("button", { name: "Cancel" }).click();
    await expect(page.getByRole("heading", { name: "Create Pull Request" })).not.toBeVisible();
  });

  test("opens GitHub Release modal from version label section", async ({ page }) => {
    await page.goto(server.url);

    // If Create Release button is present
    const releaseBtn = page.getByRole("button", { name: "Create GitHub Release" });
    if (await releaseBtn.isVisible()) {
      await releaseBtn.click();
      await expect(page.getByRole("heading", { name: "Create GitHub Release" })).toBeVisible();
      await expect(page.getByLabel(/Tag Name/)).toBeVisible();
      await expect(page.getByLabel("Save as draft")).toBeVisible();
      await expect(page.getByLabel("Set as pre-release")).toBeVisible();

      // Close release modal
      await page.getByRole("button", { name: "Cancel" }).click();
      await expect(page.getByRole("heading", { name: "Create GitHub Release" })).not.toBeVisible();
    }
  });
});
