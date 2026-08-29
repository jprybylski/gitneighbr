import { spawn } from "node:child_process";
import { existsSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";

const repoRoot = path.resolve(import.meta.dirname, "..", "..");

export interface FixtureServer {
  /** The session's own authenticated URL, `#token=...` included. */
  url: string;
  /** Present only for fixtures that also hand back a clone/remote source path. */
  remoteDir?: string;
  stop: () => Promise<void>;
}

async function waitForInfoFile(infoPath: string, timeoutMs = 15000): Promise<{ url: string; remoteDir?: string }> {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (existsSync(infoPath)) {
      try {
        return JSON.parse(readFileSync(infoPath, "utf-8"));
      } catch {
        // serve.R may still be mid-write; retry.
      }
    }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  throw new Error(`gitneighbr e2e: server did not become ready within ${timeoutMs}ms (info file: ${infoPath})`);
}

/**
 * Builds the named scratch repo (see fixtures/repo.R) and starts a real
 * `gitneighbr::open_repo()` session against it in a child `Rscript`
 * process. gitneighbr has no generic "discard/reset" endpoint by design
 * (spec: no automation for anything requiring human judgment), so unlike
 * a shared-server + reset-before-each e2e setup, each spec file calls
 * this once in `test.beforeAll` to get its own repo and its own server.
 */
export async function startFixtureServer(fixture: string): Promise<FixtureServer> {
  const infoPath = path.join(tmpdir(), `gitneighbr-e2e-${fixture}-${process.pid}-${Date.now()}.json`);
  const scriptPath = path.join(import.meta.dirname, "fixtures", "serve.R");

  const proc = spawn("Rscript", [scriptPath, fixture, infoPath], {
    cwd: repoRoot,
    stdio: ["ignore", "pipe", "pipe"],
  });

  let stderrOutput = "";
  proc.stderr?.on("data", (chunk: Buffer) => {
    stderrOutput += chunk.toString();
  });

  const exited = new Promise<never>((_resolve, reject) => {
    proc.on("exit", (code) => {
      reject(new Error(`gitneighbr e2e: fixture server for "${fixture}" exited early (code ${code}).\n${stderrOutput}`));
    });
  });

  const info = await Promise.race([waitForInfoFile(infoPath), exited]);

  return {
    url: info.url,
    remoteDir: info.remoteDir,
    stop: async () => {
      proc.removeAllListeners("exit");
      const closed = new Promise<void>((resolve) => proc.once("exit", () => resolve()));
      proc.kill();
      await closed;
      try {
        rmSync(infoPath, { force: true });
      } catch {
        // best-effort cleanup
      }
    },
  };
}
