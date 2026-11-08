import { test, expect } from "@playwright/test";
import { spawn, execFileSync, type ChildProcess } from "node:child_process";
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, rmSync } from "node:fs";
import { createServer, request as httpRequest, type Server } from "node:http";
import { createServer as netServer } from "node:net";
import { tmpdir } from "node:os";
import path from "node:path";

/**
 * Managed (launcher) mode end to end: gitneighbr's `scripts/serve.R` behind
 * a full-path reverse proxy at /apps/gitneighbr/ that injects the session
 * token, the way a launcher such as opncafyr runs it. Every request the page
 * makes must stay under the prefix, and no token may appear in any URL.
 */

const repoRoot = path.resolve(import.meta.dirname, "..", "..");
const PREFIX = "/apps/gitneighbr/";
const TOKEN = "e2e-managed-token-0123456789abcdef";
const SHUTDOWN = "e2e-managed-shutdown-0123456789ab";

function freePort(): Promise<number> {
  return new Promise((resolve) => {
    const srv = netServer();
    srv.listen(0, "127.0.0.1", () => {
      const port = (srv.address() as { port: number }).port;
      srv.close(() => resolve(port));
    });
  });
}

let study: string;
let outside: string;
let child: ChildProcess;
let proxy: Server;
let proxyPort: number;
const escaped: string[] = [];

test.beforeAll(async () => {
  study = mkdtempSync(path.join(tmpdir(), "gitneighbr-managed-study-"));
  outside = mkdtempSync(path.join(tmpdir(), "gitneighbr-managed-launcher-"));
  for (const [file, text] of [
    ["recipe/clean.json", '{"steps": []}\n'],
    ["data/subjects.csv", "id,value\n1,2\n"],
    ["OUTPUTS/table.csv", "a,b\n"],
    ["notes.txt", "hello\n"],
  ]) {
    mkdirSync(path.dirname(path.join(study, file)), { recursive: true });
    writeFileSync(path.join(study, file), text);
  }
  writeFileSync(
    path.join(outside, "policy.json"),
    JSON.stringify({
      schema_version: 1,
      groups: [
        { id: "sources", label: "Editable sources", description: "Recipes you edit.", globs: ["recipe/**"] },
        { id: "generated", label: "Generated outputs", globs: ["OUTPUTS/"] },
      ],
      default_group: { id: "other", label: "Other files" },
      review: [{ id: "data", label: "Study data", message: "May identify people.", globs: ["data/**"] }],
      ignore_template: [
        { rule: "OUTPUTS/", reason: "Rebuilt from a run." },
        { rule: "*.log", reason: "Logs." },
      ],
    }),
  );
  writeFileSync(path.join(outside, "git.config"), "[user]\n\tname = E2E\n\temail = e2e@example.com\n");

  const childPort = await freePort();
  const expr = `pkgload::load_all(${JSON.stringify(repoRoot)}, quiet = TRUE, export_all = FALSE); source(${JSON.stringify(path.join(repoRoot, "inst", "scripts", "serve.R"))})`;
  const git = execFileSync("sh", ["-c", "command -v git"]).toString().trim();
  child = spawn(
    "Rscript",
    [
      "--vanilla", "-e", expr,
      `--repo=${study}`, `--port=${childPort}`, `--git=${git}`, `--base-path=${PREFIX}`,
      "--api-token-env=GN_E2E_TOKEN", "--shutdown-token-env=GN_E2E_SHUTDOWN",
      `--git-config=${path.join(outside, "git.config")}`, `--policy-file=${path.join(outside, "policy.json")}`,
    ],
    { env: { ...process.env, GN_E2E_TOKEN: TOKEN, GN_E2E_SHUTDOWN: SHUTDOWN }, stdio: ["ignore", "pipe", "pipe"] },
  );
  await new Promise<void>((resolve, reject) => {
    let err = "";
    child.stderr?.on("data", (c: Buffer) => (err += c.toString()));
    child.stdout?.on("data", (c: Buffer) => {
      if (c.toString().includes("GITNEIGHBR_READY ")) resolve();
    });
    child.on("exit", (code) => reject(new Error(`serve.R exited (${code}): ${err}`)));
    setTimeout(() => reject(new Error(`serve.R not ready: ${err}`)), 60000);
  });

  // A launcher-style proxy: forwards the full path, rewrites Host, drops
  // Origin/cookies, injects the token. Anything outside the prefix 404s.
  proxyPort = await freePort();
  proxy = createServer((req, res) => {
    const url = req.url ?? "/";
    if (!url.startsWith(PREFIX)) {
      escaped.push(url);
      res.writeHead(404).end();
      return;
    }
    const headers = { ...req.headers };
    delete headers.origin;
    delete headers.cookie;
    delete headers.referer;
    headers.host = `127.0.0.1:${childPort}`;
    headers["x-gitneighbr-token"] = TOKEN;
    const upstream = httpRequest(
      { host: "127.0.0.1", port: childPort, path: url, method: req.method, headers },
      (up) => {
        res.writeHead(up.statusCode ?? 502, up.headers);
        up.pipe(res);
      },
    );
    upstream.on("error", () => res.destroy());
    req.pipe(upstream);
  });
  await new Promise<void>((resolve) => proxy.listen(proxyPort, "127.0.0.1", () => resolve()));
});

test.afterAll(async () => {
  proxy?.close();
  child?.kill();
  rmSync(study, { recursive: true, force: true });
  rmSync(outside, { recursive: true, force: true });
});

test.describe("managed mode behind a path-prefix proxy", () => {
  test("set up with suggested ignore rules, then save one deliberately chosen file", async ({ page }) => {
    const requested: string[] = [];
    page.on("request", (r) => requested.push(r.url()));

    await page.goto(`http://127.0.0.1:${proxyPort}${PREFIX}`);
    await expect(page.getByRole("heading", { name: "Start version history here" })).toBeVisible();
    await expect(page.getByText("Rebuilt from a run.")).toBeVisible();
    await page.getByRole("checkbox", { name: /\*\.log/ }).uncheck();
    await expect(page.getByRole("button", { name: /clone/i })).toHaveCount(0);
    await page.getByRole("button", { name: "Start version history here" }).click();

    await expect(page.getByRole("heading", { name: /Editable sources/ })).toBeVisible();
    await expect(page.getByText("Study data", { exact: true })).toBeVisible();
    // Nothing is pre-selected, and there is no Remove/Restore (features off).
    for (const box of await page.getByRole("checkbox", { name: /Include .* in the next snapshot/ }).all()) {
      await expect(box).not.toBeChecked();
    }
    await expect(page.getByRole("button", { name: /^Remove / })).toHaveCount(0);
    await expect(page.getByText("Also send to GitHub")).toHaveCount(0);
    // A managed page never talks about GitHub, and with sending off it has
    // nothing "not yet sent".
    await expect(page.getByText(/GitHub/)).toHaveCount(0);
    await expect(page.getByText("Saved, not yet sent")).toHaveCount(0);
    await expect(page.getByText("Mark this snapshot as a version")).toHaveCount(0);

    await page.getByRole("checkbox", { name: "Include recipe/clean.json in the next snapshot" }).check();
    await page.getByLabel(/Summary/).fill("Add cleaning recipe");
    await page.getByRole("button", { name: /Save/ }).click();
    await expect(page.getByText(/Saved snapshot [0-9a-f]+/)).toBeVisible();
    await expect(page.getByText(/GitHub/)).toHaveCount(0);
    await expect.poll(() => execFileSync("git", ["-C", study, "show", "--name-only", "--format=", "HEAD"]).toString().trim()).toBe("recipe/clean.json");

    const ignore = readFileSync(path.join(study, ".gitignore"), "utf-8");
    expect(ignore).toContain("OUTPUTS/");
    expect(ignore).not.toContain("*.log");

    expect(escaped).toEqual([]);
    for (const url of requested) {
      expect(new URL(url).pathname.startsWith(PREFIX)).toBe(true);
      expect(url).not.toContain(TOKEN);
    }
  });
});
