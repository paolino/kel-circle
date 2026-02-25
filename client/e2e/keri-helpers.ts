/**
 * KERI/CESR signing helpers for Playwright E2E tests.
 *
 * Reimplements just enough KERI to build signed submissions
 * from Node.js, matching the PureScript client's format.
 */

import { Page } from "@playwright/test";
import nacl from "tweetnacl";
import blakejs from "blakejs";

// --------------------------------------------------------
// CESR encoding
// --------------------------------------------------------

function base64UrlEncode(bytes: Uint8Array): string {
  return Buffer.from(bytes)
    .toString("base64")
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/g, "");
}

/**
 * CESR-encode a primitive: prepend pad bytes, base64url encode,
 * replace leading chars with the derivation code text.
 */
function cesrEncode(codeText: string, padLen: number, raw: Uint8Array): string {
  const padded = new Uint8Array(padLen + raw.length);
  padded.set(raw, padLen); // first padLen bytes are 0
  const b64 = base64UrlEncode(padded);
  return codeText + b64.slice(padLen);
}

export function cesrPubKey(publicKey: Uint8Array): string {
  return cesrEncode("D", 1, publicKey);
}

function cesrSignature(sigBytes: Uint8Array): string {
  return cesrEncode("0B", 2, sigBytes);
}

function cesrDigest(digestBytes: Uint8Array): string {
  return cesrEncode("F", 1, digestBytes);
}

// --------------------------------------------------------
// SAID computation (Blake2b-256 → CESR)
// --------------------------------------------------------

function computeSaid(jsonStr: string): string {
  const bytes = new TextEncoder().encode(jsonStr);
  const digest = blakejs.blake2b(bytes, undefined, 32);
  return cesrDigest(new Uint8Array(digest));
}

// --------------------------------------------------------
// Canonical JSON (matches keri-purs Json.Canonical)
// --------------------------------------------------------

/** Escape and quote a JSON string value. */
function jsonStr(s: string): string {
  return JSON.stringify(s);
}

// --------------------------------------------------------
// KERI Interaction Event
// --------------------------------------------------------

function serializeInteraction(
  version: string,
  digest: string,
  prefix: string,
  seqNo: number,
  priorDigest: string,
  anchorJson: string,
): string {
  return (
    `{"v":${jsonStr(version)},` +
    `"t":"ixn",` +
    `"d":${jsonStr(digest)},` +
    `"i":${jsonStr(prefix)},` +
    `"s":${jsonStr(seqNo.toString(16))},` +
    `"p":${jsonStr(priorDigest)},` +
    `"a":[${anchorJson}]}`
  );
}

/**
 * Build a complete KERI interaction event with correct
 * version string and SAID.
 */
function buildInteraction(
  keriPrefix: string,
  seqNo: number,
  priorDigest: string,
  anchorJson: string,
): string {
  // Blake2bDigest totalLength = (1 + 32) * 4 / 3 = 44
  const saidPlaceholder = "#".repeat(44);
  const versionPlaceholder = "KERI10JSON000000_";

  // Step 1: placeholder to measure size
  const placeholder = serializeInteraction(
    versionPlaceholder,
    saidPlaceholder,
    keriPrefix,
    seqNo,
    priorDigest,
    anchorJson,
  );
  const size = new TextEncoder().encode(placeholder).length;

  // Step 2: real version
  const version = "KERI10JSON" + size.toString(16).padStart(6, "0") + "_";
  const withVersion = serializeInteraction(
    version,
    saidPlaceholder,
    keriPrefix,
    seqNo,
    priorDigest,
    anchorJson,
  );

  // Step 3: compute SAID
  const said = computeSaid(withVersion);

  // Step 4: final
  return serializeInteraction(
    version,
    said,
    keriPrefix,
    seqNo,
    priorDigest,
    anchorJson,
  );
}

// --------------------------------------------------------
// Circle event JSON builders
// --------------------------------------------------------

/** Build a sorted JSON object string from key-value pairs. */
function sortedObj(pairs: [string, string][]): string {
  const sorted = [...pairs].sort((a, b) => a[0].localeCompare(b[0]));
  return "{" + sorted.map(([k, v]) => `${jsonStr(k)}:${v}`).join(",") + "}";
}

/** Encode a ChangeRole BaseDecision as JSON string. */
function encodeChangeRole(memberId: string, role: string): string {
  return sortedObj([
    ["memberId", jsonStr(memberId)],
    ["role", jsonStr(role)],
    ["tag", jsonStr("changeRole")],
  ]);
}

/** Encode a CEBaseDecision circle event as JSON string. */
function encodeBaseDecisionEvent(decision: string): string {
  return sortedObj([
    ["decision", decision],
    ["tag", jsonStr("baseDecision")],
  ]);
}

/** Encode a CEProposal circle event as JSON string. */
function encodeProposalEvent(content: string, deadline: number): string {
  return sortedObj([
    ["content", content],
    ["deadline", deadline.toString()],
    ["tag", jsonStr("proposal")],
  ]);
}

/** Encode a CEResponse circle event as JSON string. */
function encodeResponseEvent(proposalId: number): string {
  return sortedObj([
    ["content", "[]"],
    ["proposalId", proposalId.toString()],
    ["tag", jsonStr("response")],
  ]);
}

/** Build the full submission JSON string. */
function encodeSubmission(
  signer: string,
  signature: string,
  eventJson: string,
  passphrase: string | null = null,
): string {
  return sortedObj([
    ["event", eventJson],
    ["inception", "null"],
    ["passphrase", passphrase ? jsonStr(passphrase) : "null"],
    ["signature", jsonStr(signature)],
    ["signer", jsonStr(signer)],
  ]);
}

// --------------------------------------------------------
// Identity extraction (from browser localStorage)
// --------------------------------------------------------

export interface ExtractedIdentity {
  secretKey: number[];
  prefix: string;
  keriPrefix: string;
  keyStates: Record<
    string,
    {
      prefix: string;
      sequenceNumber: number;
      lastDigest: string;
    }
  >;
}

/**
 * Extract identity data from the browser's localStorage.
 * Decrypts the secret key using WebCrypto in the browser context.
 */
export async function extractIdentity(
  page: Page,
  passphrase: string,
): Promise<ExtractedIdentity> {
  return page.evaluate(async (pass: string) => {
    const encrypted = localStorage.getItem("kel-circle-encrypted");
    const prefix = localStorage.getItem("kel-circle-prefix");
    const keriPrefix = localStorage.getItem("kel-circle-keri-prefix");
    const keyStatesStr = localStorage.getItem("kel-circle-key-states");

    if (!encrypted || !prefix || !keriPrefix || !keyStatesStr) {
      throw new Error("Missing identity data in localStorage");
    }

    // Decrypt secret key
    const enc = new TextEncoder();
    const raw = Uint8Array.from(atob(encrypted), (c) => c.charCodeAt(0));
    const salt = raw.slice(0, 16);
    const iv = raw.slice(16, 28);
    const ciphertext = raw.slice(28);
    const keyMaterial = await crypto.subtle.importKey(
      "raw",
      enc.encode(pass),
      "PBKDF2",
      false,
      ["deriveKey"],
    );
    const key = await crypto.subtle.deriveKey(
      {
        name: "PBKDF2",
        salt,
        iterations: 100000,
        hash: "SHA-256",
      },
      keyMaterial,
      { name: "AES-GCM", length: 256 },
      false,
      ["decrypt"],
    );
    const plaintext = await crypto.subtle.decrypt(
      { name: "AES-GCM", iv },
      key,
      ciphertext,
    );

    return {
      secretKey: Array.from(new Uint8Array(plaintext)),
      prefix,
      keriPrefix,
      keyStates: JSON.parse(keyStatesStr),
    };
  }, passphrase);
}

// --------------------------------------------------------
// Sign + submit helpers
// --------------------------------------------------------

/**
 * Sign a circle event and POST it to the server.
 * Returns the response status and body.
 */
export async function signAndSubmit(
  serverUrl: string,
  identity: ExtractedIdentity,
  eventJson: string,
  passphrase: string | null = null,
): Promise<{ status: number; body: string }> {
  const secretKey = new Uint8Array(identity.secretKey);
  const ks = identity.keyStates[identity.prefix];
  if (!ks) {
    throw new Error(`No key state for ${identity.prefix}`);
  }

  // Build KERI interaction event
  const ixnStr = buildInteraction(
    identity.keriPrefix,
    ks.sequenceNumber + 1,
    ks.lastDigest,
    eventJson,
  );

  // Sign
  const msgBytes = new TextEncoder().encode(ixnStr);
  const sigBytes = nacl.sign.detached(msgBytes, secretKey);
  const signature = cesrSignature(sigBytes);

  // Build submission
  const submissionJson = encodeSubmission(
    identity.prefix,
    signature,
    eventJson,
    passphrase,
  );

  // POST
  const resp = await fetch(`${serverUrl}/events`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: submissionJson,
  });
  const body = await resp.text();
  return { status: resp.status, body };
}

/**
 * Submit a proposal to change a member's role.
 */
export async function submitRoleChangeProposal(
  serverUrl: string,
  identity: ExtractedIdentity,
  targetMemberId: string,
  role: "admin" | "member",
  deadline: number = 9999,
): Promise<{ status: number; body: string }> {
  const content = encodeChangeRole(targetMemberId, role);
  const eventJson = encodeProposalEvent(content, deadline);
  return signAndSubmit(serverUrl, identity, eventJson);
}

/**
 * Submit a response to a proposal.
 */
export async function submitProposalResponse(
  serverUrl: string,
  identity: ExtractedIdentity,
  proposalId: number,
): Promise<{ status: number; body: string }> {
  const eventJson = encodeResponseEvent(proposalId);
  return signAndSubmit(serverUrl, identity, eventJson);
}

/**
 * Submit a direct role change (CEBaseDecision, not via proposal).
 * Should be rejected by baseGate for majority-gated decisions.
 */
export async function submitDirectRoleChange(
  serverUrl: string,
  identity: ExtractedIdentity,
  targetMemberId: string,
  role: "admin" | "member",
): Promise<{ status: number; body: string }> {
  const decision = encodeChangeRole(targetMemberId, role);
  const eventJson = encodeBaseDecisionEvent(decision);
  return signAndSubmit(serverUrl, identity, eventJson);
}
