import { test, expect, BrowserContext, Page } from "@playwright/test";
import { spawn, ChildProcess } from "child_process";
import {
  mkdtempSync,
  rmSync,
  existsSync,
  readdirSync,
} from "fs";
import { join } from "path";
import { tmpdir } from "os";

// ----------------------------------------------------------
// Server lifecycle
// ----------------------------------------------------------

const BOOTSTRAP_PASS = "playwright-e2e-pass";
const USER_PASS = "user-secret";
const REPO_ROOT = join(__dirname, "../..");
let serverProcess: ChildProcess;
let serverPort: number;
let tmpDir: string;

async function findFreePort(): Promise<number> {
  const { createServer } = await import("net");
  return new Promise((resolve, reject) => {
    const srv = createServer();
    srv.listen(0, "127.0.0.1", () => {
      const addr = srv.address();
      if (addr && typeof addr !== "string") {
        const port = addr.port;
        srv.close(() => resolve(port));
      } else {
        reject(new Error("Could not get port"));
      }
    });
  });
}

function findServerBin(distBase: string): string {
  if (!existsSync(distBase)) {
    throw new Error(
      `dist-newstyle not found at ${distBase}. Run 'cabal build kel-circle-server -O0' first.`,
    );
  }
  for (const ghcDir of readdirSync(distBase)) {
    const pkgBase = join(distBase, ghcDir);
    if (!existsSync(pkgBase)) continue;
    let pkgDirs: string[];
    try {
      pkgDirs = readdirSync(pkgBase).filter((d) =>
        d.startsWith("kel-circle-"),
      );
    } catch {
      continue;
    }
    for (const pkgDir of pkgDirs) {
      const exeBase = join(pkgBase, pkgDir, "x/kel-circle-server");
      for (const sub of ["noopt/build", "build"]) {
        const bin = join(exeBase, sub, "kel-circle-server/kel-circle-server");
        if (existsSync(bin)) return bin;
      }
    }
  }
  throw new Error(`kel-circle-server binary not found under ${distBase}`);
}

async function startServer(): Promise<void> {
  serverPort = await findFreePort();
  tmpDir = mkdtempSync(join(tmpdir(), "kel-pw-"));
  const dbPath = join(tmpDir, "test.db");
  const keyFile = join(tmpDir, "seq.key");

  const distBase = join(REPO_ROOT, "dist-newstyle/build/x86_64-linux");
  const serverBin = findServerBin(distBase);

  serverProcess = spawn(
    serverBin,
    [String(serverPort), dbPath, BOOTSTRAP_PASS, keyFile],
    {
      cwd: REPO_ROOT,
      stdio: ["ignore", "pipe", "pipe"],
    },
  );
  serverProcess.stderr?.on("data", (data: Buffer) => {
    console.log(`[server] ${data.toString().trim()}`);
  });

  const deadline = Date.now() + 30_000;
  while (Date.now() < deadline) {
    try {
      const resp = await fetch(`http://127.0.0.1:${serverPort}/info`);
      if (resp.ok) return;
    } catch {
      // Server not ready yet
    }
    await new Promise((r) => setTimeout(r, 200));
  }
  throw new Error("Server did not start within 30s");
}

function stopServer(): void {
  if (serverProcess) {
    serverProcess.kill("SIGTERM");
  }
  try {
    if (tmpDir && existsSync(tmpDir)) {
      rmSync(tmpDir, { recursive: true });
    }
  } catch {
    // Ignore cleanup errors
  }
}

function baseUrl(): string {
  return `http://127.0.0.1:${serverPort}`;
}

// ----------------------------------------------------------
// Helpers
// ----------------------------------------------------------

/**
 * Generate a new identity. Enters passphrase, clicks Generate,
 * waits for the screen to transition. Returns the CESR prefix.
 */
async function generateIdentity(page: Page): Promise<string> {
  await page.goto(baseUrl(), { waitUntil: "networkidle" });

  await page.locator('input[placeholder="Passphrase"]').fill(USER_PASS);
  await page.locator("button", { hasText: "Generate Identity" }).click();

  // Wait for any screen transition away from "Welcome to kel-circle"
  await page.waitForFunction(
    () => {
      const h2 = document.querySelector("h2");
      return h2 && h2.textContent !== "Welcome to kel-circle";
    },
    { timeout: 30_000 },
  );
  await page.waitForTimeout(500);

  const prefix = await page.evaluate(() =>
    localStorage.getItem("kel-circle-prefix"),
  );
  expect(prefix).toBeTruthy();
  return prefix!;
}

/**
 * Unlock an existing identity via passphrase.
 * Navigates to the app, expects "Welcome back", enters passphrase.
 */
async function unlockIdentity(page: Page): Promise<void> {
  await page.goto(baseUrl(), { waitUntil: "networkidle" });

  await expect(
    page.locator("h2", { hasText: "Welcome back" }),
  ).toBeVisible({ timeout: 15_000 });

  await page.locator('input[placeholder="Passphrase"]').fill(USER_PASS);
  await page.locator("button", { hasText: "Unlock" }).click();

  // Wait for transition away from unlock screen
  await page.waitForFunction(
    () => {
      const h2 = document.querySelector("h2");
      return h2 && h2.textContent !== "Welcome back";
    },
    { timeout: 30_000 },
  );
  await page.waitForTimeout(500);
}

/** Get inception JSON from localStorage. */
async function getInceptionJson(page: Page): Promise<string> {
  const icp = await page.evaluate(() =>
    localStorage.getItem("kel-circle-inception"),
  );
  expect(icp).toBeTruthy();
  return icp!;
}

/**
 * Introduce a member via Alice's introduce form.
 * Assumes Alice is on the normal screen with the form visible.
 */
async function introduceMember(
  adminPage: Page,
  memberPrefix: string,
  memberName: string,
  memberInception: string,
): Promise<void> {
  // Wait for the Introduce button to be enabled
  const introduceBtn = adminPage.locator(".introduce-form button", {
    hasText: "Introduce",
  });
  await expect(introduceBtn).toBeEnabled({ timeout: 15_000 });

  await adminPage
    .locator('input[placeholder="CESR public key"]')
    .fill(memberPrefix);
  await adminPage
    .locator('input[placeholder="Display name"]')
    .fill(memberName);
  await adminPage
    .locator('textarea[placeholder="Paste member\'s inception JSON here"]')
    .fill(memberInception);

  await introduceBtn.click();

  // Wait for the new member to appear in the table
  await expect(
    adminPage.locator("td.name", { hasText: memberName }),
  ).toBeVisible({ timeout: 15_000 });

  // Wait for submitting to reset (button re-enabled)
  await expect(introduceBtn).toBeEnabled({ timeout: 15_000 });
}

// ----------------------------------------------------------
// Test suite: basic UI (fresh server)
// ----------------------------------------------------------

test.describe("kel-circle UI basics", () => {
  test.beforeAll(async () => {
    await startServer();
  });

  test.afterAll(() => {
    stopServer();
  });

  test("page loads with identity screen", async ({ page }) => {
    await page.goto(baseUrl());
    const btn = page.locator("button", {
      hasText: "Generate Identity",
    });
    await expect(btn).toBeVisible();
    await expect(btn).toBeDisabled();
  });

  test("entering passphrase enables Generate Identity", async ({ page }) => {
    await page.goto(baseUrl());
    const btn = page.locator("button", {
      hasText: "Generate Identity",
    });
    await expect(btn).toBeDisabled();

    await page.locator('input[placeholder="Passphrase"]').fill("mypass");
    await expect(btn).toBeEnabled();
  });

  test("generate identity shows bootstrap screen", async ({ page }) => {
    await page.goto(baseUrl());
    await page.locator('input[placeholder="Passphrase"]').fill(USER_PASS);
    await page.locator("button", { hasText: "Generate Identity" }).click();

    await expect(
      page.locator("h2", { hasText: "Bootstrap Mode" }),
    ).toBeVisible({ timeout: 15_000 });

    await expect(
      page.locator('input[placeholder="Bootstrap passphrase"]'),
    ).toBeVisible();
    await expect(
      page.locator('input[placeholder="Your display name"]'),
    ).toBeVisible();

    const keyInput = page.locator(
      'input[placeholder="Your CESR public key"]',
    );
    await expect(keyInput).toBeVisible();
    const keyValue = await keyInput.inputValue();
    expect(keyValue.length).toBeGreaterThan(0);
  });

  test("header shows user ID after identity generation", async ({ page }) => {
    await page.goto(baseUrl());
    await page.locator('input[placeholder="Passphrase"]').fill(USER_PASS);
    await page.locator("button", { hasText: "Generate Identity" }).click();

    await expect(
      page.locator("h2", { hasText: "Bootstrap Mode" }),
    ).toBeVisible({ timeout: 15_000 });

    await expect(page.locator(".user-id")).toBeVisible();
    await expect(
      page.locator("button", { hasText: "Copy" }),
    ).toBeVisible();
    await expect(
      page.locator("button", { hasText: "Reset" }),
    ).toBeVisible();
  });

  test("reset identity returns to initial screen", async ({ page }) => {
    await page.goto(baseUrl());
    await page.locator('input[placeholder="Passphrase"]').fill(USER_PASS);
    await page.locator("button", { hasText: "Generate Identity" }).click();

    await expect(
      page.locator("h2", { hasText: "Bootstrap Mode" }),
    ).toBeVisible({ timeout: 15_000 });

    page.on("dialog", (dialog) => dialog.accept());
    await page.locator("button.btn-reset").click();

    await expect(
      page.locator("button", { hasText: "Generate Identity" }),
    ).toBeVisible();
    await expect(page.locator(".user-id")).not.toBeVisible();
  });

  test("server /info endpoint is accessible", async ({ request }) => {
    const resp = await request.get(`${baseUrl()}/info`);
    expect(resp.ok()).toBeTruthy();
    const body = await resp.json();
    expect(body).toHaveProperty("authMode", "bootstrap");
    expect(body).toHaveProperty("sequencerId");
  });
});

// ----------------------------------------------------------
// Test suite: 3-client bootstrap and introduction
// ----------------------------------------------------------

test.describe("3-client bootstrap and introduction", () => {
  let aliceCtx: BrowserContext;
  let bobCtx: BrowserContext;
  let carolCtx: BrowserContext;
  let alicePage: Page;
  let bobPage: Page;
  let carolPage: Page;

  test.beforeAll(async ({ browser }) => {
    await startServer();
    aliceCtx = await browser.newContext();
    bobCtx = await browser.newContext();
    carolCtx = await browser.newContext();
    alicePage = await aliceCtx.newPage();
    bobPage = await bobCtx.newPage();
    carolPage = await carolCtx.newPage();
  });

  test.afterAll(async () => {
    await aliceCtx?.close();
    await bobCtx?.close();
    await carolCtx?.close();
    stopServer();
  });

  test("full 3-client lifecycle", async () => {
    // ---- Step 1: Alice generates identity and bootstraps ----
    const alicePrefix = await generateIdentity(alicePage);

    await expect(
      alicePage.locator("h2", { hasText: "Bootstrap Mode" }),
    ).toBeVisible({ timeout: 5_000 });

    await alicePage
      .locator('input[placeholder="Bootstrap passphrase"]')
      .fill(BOOTSTRAP_PASS);
    await alicePage
      .locator('input[placeholder="Your display name"]')
      .fill("Alice");
    await alicePage
      .locator("button", { hasText: "Introduce First Admin" })
      .click();

    // Wait for member table
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    await expect(
      alicePage.locator("td.name", { hasText: "Alice" }),
    ).toBeVisible();

    // Reload to get a fresh state (resets the `submitting` flag
    // which gets stuck after bootstrap due to SSE race condition)
    await unlockIdentity(alicePage);

    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    await expect(
      alicePage.locator("td.name", { hasText: "Alice" }),
    ).toBeVisible();

    // ---- Step 2: Bob generates identity ----
    const bobPrefix = await generateIdentity(bobPage);

    // Bob sees Members screen (events exist), as non-member
    await expect(
      bobPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    await expect(bobPage.locator(".not-member-hint")).toBeVisible();

    // Bob has inception data in localStorage
    const bobInception = await getInceptionJson(bobPage);
    expect(JSON.parse(bobInception)).toHaveProperty("event");

    // ---- Step 3: Alice introduces Bob ----
    await introduceMember(
      alicePage,
      bobPrefix,
      "Bob",
      bobInception,
    );

    // ---- Step 4: Carol generates identity ----
    const carolPrefix = await generateIdentity(carolPage);

    await expect(
      carolPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    await expect(carolPage.locator(".not-member-hint")).toBeVisible();

    const carolInception = await getInceptionJson(carolPage);

    // ---- Step 5: Alice introduces Carol ----
    await introduceMember(
      alicePage,
      carolPrefix,
      "Carol",
      carolInception,
    );

    // ---- Step 6: Bob unlocks and sees all members ----
    await unlockIdentity(bobPage);

    await expect(
      bobPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    await expect(
      bobPage.locator("td.name", { hasText: "Alice" }),
    ).toBeVisible({ timeout: 10_000 });
    await expect(
      bobPage.locator("td.name", { hasText: "Bob" }),
    ).toBeVisible();
    await expect(
      bobPage.locator("td.name", { hasText: "Carol" }),
    ).toBeVisible();
    // Bob is now a member (no hint)
    await expect(
      bobPage.locator(".not-member-hint"),
    ).not.toBeVisible();

    // ---- Step 7: Carol unlocks and sees all members ----
    await unlockIdentity(carolPage);

    await expect(
      carolPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    await expect(
      carolPage.locator("td.name", { hasText: "Alice" }),
    ).toBeVisible({ timeout: 10_000 });
    await expect(
      carolPage.locator("td.name", { hasText: "Bob" }),
    ).toBeVisible();
    await expect(
      carolPage.locator("td.name", { hasText: "Carol" }),
    ).toBeVisible();
    await expect(
      carolPage.locator(".not-member-hint"),
    ).not.toBeVisible();

    // ---- Step 8: Verify no errors on any client ----
    await expect(alicePage.locator(".error-bar")).not.toBeVisible();
    await expect(bobPage.locator(".error-bar")).not.toBeVisible();
    await expect(carolPage.locator(".error-bar")).not.toBeVisible();

    // ---- Step 9: Alice removes Carol ----
    const carolRow = alicePage.locator("tr", {
      has: alicePage.locator("td.name", { hasText: "Carol" }),
    });
    const removeBtn = carolRow.locator(".btn-danger");
    await expect(removeBtn).toBeEnabled({ timeout: 5_000 });
    await removeBtn.click();

    // Carol should disappear from Alice's table
    await expect(
      alicePage.locator("td.name", { hasText: "Carol" }),
    ).not.toBeVisible({ timeout: 15_000 });

    // Alice still sees sequencer, Alice, Bob
    await expect(
      alicePage.locator("td.name", { hasText: "Alice" }),
    ).toBeVisible();
    await expect(
      alicePage.locator("td.name", { hasText: "Bob" }),
    ).toBeVisible();

    // ---- Step 10: Bob reloads and sees Carol is gone ----
    await unlockIdentity(bobPage);

    await expect(
      bobPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    await expect(
      bobPage.locator("td.name", { hasText: "Alice" }),
    ).toBeVisible({ timeout: 10_000 });
    await expect(
      bobPage.locator("td.name", { hasText: "Bob" }),
    ).toBeVisible();
    await expect(
      bobPage.locator("td.name", { hasText: "Carol" }),
    ).not.toBeVisible();

    // ---- Step 11: Carol reloads and is no longer a member ----
    await unlockIdentity(carolPage);

    await expect(
      carolPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    // Carol sees the not-member hint
    await expect(
      carolPage.locator(".not-member-hint"),
    ).toBeVisible({ timeout: 10_000 });

    // ---- Step 12: Alice removes Bob ----
    const bobRow = alicePage.locator("tr", {
      has: alicePage.locator("td.name", { hasText: "Bob" }),
    });
    const removeBobBtn = bobRow.locator(".btn-danger");
    await expect(removeBobBtn).toBeEnabled({ timeout: 15_000 });
    await removeBobBtn.click();

    // Bob should disappear from Alice's table
    await expect(
      alicePage.locator("td.name", { hasText: "Bob" }),
    ).not.toBeVisible({ timeout: 15_000 });

    // Only sequencer and Alice remain
    const memberNames = await alicePage.evaluate(() => {
      const tds = document.querySelectorAll("td.name");
      return Array.from(tds).map((td) => td.textContent);
    });
    expect(memberNames).toEqual(["sequencer", "Alice"]);

    // ---- Step 13: Verify no errors ----
    await expect(alicePage.locator(".error-bar")).not.toBeVisible();
  });
});
