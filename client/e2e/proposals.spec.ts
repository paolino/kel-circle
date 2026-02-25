import { test, expect, BrowserContext, Page } from "@playwright/test";
import { spawn, ChildProcess } from "child_process";
import { mkdtempSync, rmSync, existsSync, readdirSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import {
  extractIdentity,
  submitRoleChangeProposal,
  submitDirectRoleChange,
  ExtractedIdentity,
} from "./keri-helpers";

// ----------------------------------------------------------
// Server lifecycle (shared with three-clients.spec.ts)
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
// UI Helpers
// ----------------------------------------------------------

async function generateIdentity(page: Page): Promise<string> {
  await page.goto(baseUrl(), { waitUntil: "networkidle" });
  await page.locator('input[placeholder="Passphrase"]').fill(USER_PASS);
  await page.locator("button", { hasText: "Generate Identity" }).click();

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

async function unlockIdentity(page: Page): Promise<void> {
  await page.goto(baseUrl(), { waitUntil: "networkidle" });
  await expect(
    page.locator("h2", { hasText: "Welcome back" }),
  ).toBeVisible({ timeout: 15_000 });
  await page.locator('input[placeholder="Passphrase"]').fill(USER_PASS);
  await page.locator("button", { hasText: "Unlock" }).click();

  await page.waitForFunction(
    () => {
      const h2 = document.querySelector("h2");
      return h2 && h2.textContent !== "Welcome back";
    },
    { timeout: 30_000 },
  );
  await page.waitForTimeout(500);
}

async function getInceptionJson(page: Page): Promise<string> {
  const icp = await page.evaluate(() =>
    localStorage.getItem("kel-circle-inception"),
  );
  expect(icp).toBeTruthy();
  return icp!;
}

async function introduceMember(
  adminPage: Page,
  memberPrefix: string,
  memberName: string,
  memberInception: string,
): Promise<void> {
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

  await expect(
    adminPage.locator("td.name", { hasText: memberName }),
  ).toBeVisible({ timeout: 15_000 });
  await expect(introduceBtn).toBeEnabled({ timeout: 15_000 });
}

// ----------------------------------------------------------
// Proposal flow tests
// ----------------------------------------------------------

test.describe("proposal-based role changes", () => {
  let aliceCtx: BrowserContext;
  let bobCtx: BrowserContext;
  let alicePage: Page;
  let bobPage: Page;

  test.beforeAll(async ({ browser }) => {
    await startServer();
    aliceCtx = await browser.newContext();
    bobCtx = await browser.newContext();
    alicePage = await aliceCtx.newPage();
    bobPage = await bobCtx.newPage();
  });

  test.afterAll(async () => {
    await aliceCtx?.close();
    await bobCtx?.close();
    stopServer();
  });

  test("promote member to admin via proposal", async () => {
    // ---- Step 1: Alice bootstraps ----
    await generateIdentity(alicePage);

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

    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    await expect(
      alicePage.locator("td.name", { hasText: "Alice" }),
    ).toBeVisible();

    // Reload to reset submitting flag
    await unlockIdentity(alicePage);
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    // ---- Step 2: Bob generates identity ----
    const bobPrefix = await generateIdentity(bobPage);
    await expect(
      bobPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    const bobInception = await getInceptionJson(bobPage);

    // ---- Step 3: Alice introduces Bob as Member ----
    await introduceMember(alicePage, bobPrefix, "Bob", bobInception);

    // Verify Bob is a Member
    const bobRoleCell = alicePage.locator("tr", {
      has: alicePage.locator("td.name", { hasText: "Bob" }),
    }).locator("td").nth(1);
    await expect(bobRoleCell).toHaveText("Member", { timeout: 5_000 });

    // ---- Step 4: Alice creates a proposal to promote Bob ----
    // Extract Alice's identity for API signing
    // Reload first so key states are up to date
    await unlockIdentity(alicePage);
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    const aliceIdentity: ExtractedIdentity = await extractIdentity(
      alicePage,
      USER_PASS,
    );

    // ---- Negative: direct role change blocked by baseGate ----
    // ChangeRole requires majority — direct submission must fail
    const directPromote = await submitDirectRoleChange(
      baseUrl(),
      aliceIdentity,
      bobPrefix,
      "admin",
    );
    expect(directPromote.status).not.toBe(200);

    // Extract Bob's identity for negative tests
    const bobIdentity: ExtractedIdentity = await extractIdentity(
      bobPage,
      USER_PASS,
    );

    // Non-admin Member cannot directly change roles either
    const memberDirect = await submitDirectRoleChange(
      baseUrl(),
      bobIdentity,
      aliceIdentity.prefix,
      "member",
    );
    expect(memberDirect.status).not.toBe(200);

    // ---- Step 4 continued: Alice creates proposal ----
    // Reload Alice (key state advanced from failed direct attempt? No —
    // the server rejected it, so no state change. But reload anyway
    // since Bob's identity extraction may have changed Bob's state.)
    await unlockIdentity(alicePage);
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    // Re-extract Alice's identity (key states refreshed after reload)
    const aliceIdentity2: ExtractedIdentity = await extractIdentity(
      alicePage,
      USER_PASS,
    );

    // Submit proposal via API
    const proposalResult = await submitRoleChangeProposal(
      baseUrl(),
      aliceIdentity2,
      bobPrefix,
      "admin",
    );
    expect(proposalResult.status).toBe(200);

    // ---- Step 5: Verify proposal appears in Alice's UI ----
    // Reload to pick up the new event (our API call
    // bypassed the client's key state tracking)
    await unlockIdentity(alicePage);
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    // The Proposals section should show the proposal
    await expect(
      alicePage.locator("h2", { hasText: "Proposals" }),
    ).toBeVisible({ timeout: 10_000 });
    const proposalCard = alicePage.locator(".proposal-card").first();
    await expect(proposalCard).toBeVisible({ timeout: 10_000 });
    await expect(proposalCard).toContainText("Respond");

    // ---- Step 6: Verify proposal appears in Bob's UI ----
    await unlockIdentity(bobPage);
    await expect(
      bobPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });
    await expect(
      bobPage.locator(".proposal-card").first(),
    ).toBeVisible({ timeout: 10_000 });

    // ---- Step 7: Alice clicks "Respond" ----
    const respondBtn = alicePage.locator(".proposal-card .btn-primary", {
      hasText: "Respond",
    });
    await expect(respondBtn).toBeVisible({ timeout: 5_000 });
    await respondBtn.click();

    // Wait for the proposal to be resolved (auto-resolve
    // fires because Alice is the only admin → majority of 1)
    await expect(
      alicePage.locator(".proposal-card.resolved"),
    ).toBeVisible({ timeout: 15_000 });

    // ---- Step 8: Verify Bob's role changed to admin ----
    // Reload Alice's page to get fresh state
    await unlockIdentity(alicePage);
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    // Bob should now be admin
    const bobRow = alicePage.locator("tr", {
      has: alicePage.locator("td.name", { hasText: "Bob" }),
    });
    const roleCell = bobRow.locator("td").nth(1);
    await expect(roleCell).toHaveText("Admin", { timeout: 10_000 });

    // ---- Step 9: Verify Bob sees himself as admin ----
    await unlockIdentity(bobPage);
    await expect(
      bobPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    const bobRowInBobPage = bobPage.locator("tr", {
      has: bobPage.locator("td.name", { hasText: "Bob" }),
    });
    await expect(bobRowInBobPage.locator("td").nth(1)).toHaveText(
      "Admin",
      { timeout: 10_000 },
    );

    // Bob should see himself as Admin on his own page
    const bobAdminOnOwnPage = bobPage.locator("tr", {
      has: bobPage.locator("td.name", { hasText: "Bob" }),
    }).locator("td").nth(1);
    await expect(bobAdminOnOwnPage).toHaveText(
      "Admin",
      { timeout: 10_000 },
    );

    // ---- Step 10: No errors ----
    await expect(alicePage.locator(".error-bar")).not.toBeVisible();
    await expect(bobPage.locator(".error-bar")).not.toBeVisible();
  });

  test("demote admin requires two-admin majority", async () => {
    // State from previous test: Alice=Admin, Bob=Admin

    // ---- Step 1: Alice cannot directly demote Bob ----
    await unlockIdentity(alicePage);
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    const aliceId: ExtractedIdentity = await extractIdentity(
      alicePage,
      USER_PASS,
    );
    const bobId: ExtractedIdentity = await extractIdentity(
      bobPage,
      USER_PASS,
    );

    // Direct demotion must be rejected (requiresMajority)
    const directDemote = await submitDirectRoleChange(
      baseUrl(),
      aliceId,
      bobId.prefix,
      "member",
    );
    expect(directDemote.status).not.toBe(200);

    // ---- Step 2: Alice proposes to demote Bob ----
    const demotionProposal = await submitRoleChangeProposal(
      baseUrl(),
      aliceId,
      bobId.prefix,
      "member",
    );
    expect(demotionProposal.status).toBe(200);

    // ---- Step 3: Alice responds — not enough for majority ----
    // Reload Alice to see the new proposal
    await unlockIdentity(alicePage);
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    // Click Respond on the OPEN proposal (not the resolved one from test 1)
    const openRespondBtn = alicePage
      .locator(".proposal-card:not(.resolved) .btn-primary", {
        hasText: "Respond",
      });
    await expect(openRespondBtn).toBeVisible({ timeout: 10_000 });
    await openRespondBtn.click();

    // Wait for Alice's response to be processed
    await alicePage.waitForTimeout(2000);

    // Proposal should NOT be auto-resolved yet (1/2 admins)
    // The open proposal card should still exist
    await unlockIdentity(alicePage);
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    // Bob should still be Admin (proposal not resolved)
    const bobRoleBefore = alicePage.locator("tr", {
      has: alicePage.locator("td.name", { hasText: "Bob" }),
    }).locator("td").nth(1);
    await expect(bobRoleBefore).toHaveText("Admin", { timeout: 5_000 });

    // ---- Step 4: Bob responds — majority reached ----
    await unlockIdentity(bobPage);
    await expect(
      bobPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    const bobRespondBtn = bobPage
      .locator(".proposal-card:not(.resolved) .btn-primary", {
        hasText: "Respond",
      });
    await expect(bobRespondBtn).toBeVisible({ timeout: 10_000 });
    await bobRespondBtn.click();

    // Wait for auto-resolution (2/2 admins = majority)
    await expect(
      bobPage.locator(".proposal-card:not(.resolved)"),
    ).not.toBeVisible({ timeout: 15_000 });

    // ---- Step 5: Verify Bob demoted to Member ----
    await unlockIdentity(alicePage);
    await expect(
      alicePage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    const bobRoleAfter = alicePage.locator("tr", {
      has: alicePage.locator("td.name", { hasText: "Bob" }),
    }).locator("td").nth(1);
    await expect(bobRoleAfter).toHaveText("Member", { timeout: 10_000 });

    // Bob should see himself as Member
    await unlockIdentity(bobPage);
    await expect(
      bobPage.locator("h2", { hasText: "Members" }),
    ).toBeVisible({ timeout: 15_000 });

    const bobRoleOnBobPage = bobPage.locator("tr", {
      has: bobPage.locator("td.name", { hasText: "Bob" }),
    }).locator("td").nth(1);
    await expect(bobRoleOnBobPage).toHaveText(
      "Member",
      { timeout: 10_000 },
    );

    // ---- Step 6: No errors ----
    await expect(alicePage.locator(".error-bar")).not.toBeVisible();
    await expect(bobPage.locator(".error-bar")).not.toBeVisible();
  });
});
