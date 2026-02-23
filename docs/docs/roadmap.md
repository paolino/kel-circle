# Roadmap

---

## Phase 1 — Foundation ✅

Core protocol semantics, Lean formalisation, and a running server.

- Global sequence invariants (contiguity, monotone timestamps, no duplicates)
- Two-level gate: fixed base gate + pluggable application gate
- Full membership lifecycle: introduce, remove, change role
- Proposal machinery: open, respond, resolve (manual)
- Sequencer protection and rotation
- Unique member-name invariant
- SQLite persistence with full-replay on restart
- HTTP + SSE server (WAI)
- KERI AID for the sequencer (Ed25519 keypair, CESR prefix, storage invariant)
- PureScript client library mirroring Haskell semantics
- 50+ QuickCheck properties, 15+ E2E scenarios
- Lean formalisation of invariants and gate theorems

---

## Phase 2 — Cryptographic integrity

Signatures are currently persisted but **never verified**. Every submitted
event carries an Ed25519 signature over the payload, but the gate layer
ignores it. This phase closes that gap and brings members' identities in line
with KERI.

### Current state

| Component | Status |
|-----------|--------|
| `Submission` carries `subSigner :: Text` and `subSignature :: Text` | ✅ stored |
| SQLite `events` table has `signer` and `signature` columns | ✅ stored |
| `MemberId` is a `Text` newtype — no format validation | ❌ unvalidated |
| `signSubmission` in E2E helpers produces dummy `"sig:<key>"` | ❌ not real crypto |
| No `keri-hs` dependency in `kel-circle.cabal` | ❌ missing |
| No signature verification anywhere in gate or server | ❌ missing |
| No challenge-response endpoints | ❌ missing |
| PureScript client has no Ed25519 keygen or signing | ❌ missing |

### Sub-roadmap

Phase 2 is split into six incremental steps. Each step is independently
buildable and testable; later steps depend on earlier ones.

#### Step 2.1 — Add `keri-hs` dependency and CESR validation

Wire the `keri-hs` library into kel-circle. Use it to validate that
`MemberId` values are well-formed CESR-encoded Ed25519 public key prefixes.

**Changes:**

- Add `keri-hs` to `kel-circle.cabal` `build-depends` (and to the Nix
  flake input if needed)
- New module `KelCircle.Crypto` that re-exports the subset of `keri-hs`
  needed: `Keri.Cesr.decode`, `Keri.Cesr.Primitive`,
  `Keri.Cesr.DerivationCode(Ed25519PubKey, Ed25519Sig)`,
  `Keri.Crypto.Ed25519.verify`, `Keri.Crypto.Ed25519.publicKeyFromBytes`
- Validation function `validateCesrPrefix :: Text -> Either Text PublicKey`
  that decodes a CESR text, checks the derivation code is `Ed25519PubKey`,
  and returns the parsed public key
- `IntroduceMember mid name role` — the gate rejects if `mid` is not a
  valid CESR Ed25519 prefix (call `validateCesrPrefix`)
- Add `InvalidMemberId` constructor to `ValidationError`
- E2E test: scenario 6 (non-CESR member ID rejected)

**Done when:** existing tests still pass, and introducing a member with a
garbage `MemberId` returns 422.

#### Step 2.2 — Signature verification at the gate layer

Every `POST /events` must carry a valid Ed25519 signature over the
JSON-serialized event body, verifiable against the signer's public key.

**Changes:**

- New function `verifySubmissionSignature :: Text -> Text -> LBS.ByteString
  -> Either Text ()` — decodes the CESR signer prefix to a public key,
  decodes the CESR signature, and calls `Keri.Crypto.Ed25519.verify`
- **What is signed:** the canonical JSON encoding of `subEvent` (the same
  bytes the server stores in `event_json`). The server re-encodes the event
  to JSON and verifies the signature over those bytes.
- Call `verifySubmissionSignature` in `handlePostEvent` (and
  `handleBootstrapPost`) **before** gate validation. Return 422 with a new
  `SignatureInvalid` error on failure.
- Add `SignatureInvalid` constructor to `ValidationError` (or a separate
  `CryptoError` type)
- Update E2E `TestId` to hold a real `Keri.Crypto.Ed25519.KeyPair`;
  `newTestId` calls `generateKeyPair`, `tidKey` becomes the CESR-encoded
  public key prefix
- Update `signSubmission` to produce a real Ed25519 signature over the
  JSON-encoded event
- E2E tests: scenarios 1–4 (valid accepted, forged rejected, wrong-key
  rejected, tampered payload rejected)

**Done when:** all 4 signature E2E scenarios pass, existing E2E tests
still pass (they now use real signatures).

#### Step 2.3 — Anchored key verification for members

After introduction, all subsequent events from a member must be signed
with the key that was anchored at introduction time.

**Changes:**

- `Member` gains a `memberKey :: Text` field (the CESR-encoded public key,
  already stored as the second argument to `IntroduceMember`)
- On `POST /events` in normal mode, look up the signer's `memberKey` from
  `CircleState.members` and verify the signature against **that** key (not
  just against the `subSigner` text — the signer must match an anchored
  member)
- Reject if `subSigner` does not match any member's `memberKey`, or if the
  signature doesn't verify against the anchored key
- In bootstrap mode the signer is the new admin being introduced, so the
  key is self-certifying: the `subSigner` text **is** the key, and the
  signature is verified against it
- E2E tests: scenarios 5, 7 (introduction anchors key; subsequent events
  verified against anchored key)

**Done when:** a member introduced with key A cannot submit events signed
with key B.

#### Step 2.4 — Challenge-response authentication

Prevent impersonation by requiring clients to prove possession of their
private key before submitting events.

**Changes:**

- New server-side state: `TVar (Map MemberId ByteString)` for pending
  nonces, `TVar (Map Text MemberId)` for session tokens
- New endpoint `GET /challenge?member=<memberId>` — generate a random
  32-byte nonce, store it keyed by member ID, return `{"nonce": "<hex>"}`
- New endpoint `POST /challenge` — body
  `{"member": "<memberId>", "signedNonce": "<CESR sig>"}`. Server looks up
  the pending nonce, verifies the signature against the member's anchored
  key, and on success: deletes the nonce (one-time use), generates a random
  session token, stores it in the session map, returns
  `{"token": "<token>"}`
- `POST /events` requires an `Authorization: Bearer <token>` header in
  normal mode. Server looks up the token to find the signer's member ID.
  The `subSigner` field must match the token's member ID.
- Bootstrap mode is exempt from challenge-response (no members exist yet
  to challenge)
- Nonces expire after a configurable TTL (e.g. 60 seconds)
- E2E tests: scenarios 8–12 (challenge issued, correct response accepted,
  wrong-key rejected, replayed nonce rejected, unauthenticated POST
  rejected)

**Done when:** all 5 challenge-response E2E scenarios pass.

#### Step 2.5 — PureScript client crypto

Browser-side Ed25519 key generation, signing, and challenge-response
flow.

**Changes:**

- PureScript FFI module wrapping the Web Crypto API (or a JS Ed25519
  library like `@noble/ed25519`) for: key generation, signing, CESR
  encoding of public key and signature
- Key storage in browser `localStorage`, encrypted with a user-chosen
  passphrase (AES-GCM via Web Crypto)
- Client codec updated: `encodeSubmission` takes a `KeyPair` and signs the
  JSON-serialized event before encoding
- Challenge-response flow in the client: on connect, call
  `GET /challenge?member=<myId>`, sign the nonce, `POST /challenge`, store
  the session token, attach it to subsequent `POST /events` requests
- Halogen UI: key generation on first use, passphrase prompt, visual
  indicator of authentication state

**Done when:** the Halogen demo can bootstrap an admin, introduce members,
and submit events — all with real Ed25519 signatures and challenge-response
auth.

#### Step 2.6 — Sequencer ordering E2E and cross-cutting invariants

Final verification pass: sequencer ordering properties and cross-cutting
invariants that must hold across all scenarios.

**Changes:**

- E2E tests: scenarios 13–16 (contiguous sequence numbers, monotonically
  increasing, monotonic timestamps, no duplicates)
- Cross-cutting invariant tests (run after every multi-event scenario):
    - Replay the full log via `GET /events` and verify every signature
      against the signer's anchored public key
    - Verify the sequencer's events are signed with the sequencer's KERI
      AID key
    - Verify no two members share the same public key prefix
    - Verify global sequence invariants (contiguity, monotonicity, no
      duplicates)

**Done when:** all 16 scenarios pass, all cross-cutting invariants hold,
CI is green.

### Security E2E scenarios (summary)

| # | Category | Scenario | Expected |
|---|----------|----------|----------|
| 1 | Signature | Valid signature accepted | 200 |
| 2 | Signature | Forged signature rejected | 422 |
| 3 | Signature | Wrong-key signature rejected | 422 |
| 4 | Signature | Tampered payload rejected | 422 |
| 5 | AID anchoring | Introduction anchors public key | key in /condition |
| 6 | AID anchoring | Non-CESR member ID rejected | 422 |
| 7 | AID anchoring | Subsequent events verified against anchored key | 200 / 422 |
| 8 | Challenge | Challenge issued on connect | nonce returned |
| 9 | Challenge | Correct challenge response accepted | 200 + token |
| 10 | Challenge | Wrong-key challenge rejected | 401 |
| 11 | Challenge | Replayed nonce rejected | 401 |
| 12 | Challenge | Unauthenticated POST rejected | 401 |
| 13 | Ordering | Sequence numbers are contiguous | verified |
| 14 | Ordering | Sequence numbers are monotonically increasing | verified |
| 15 | Ordering | Timestamps are monotonically non-decreasing | verified |
| 16 | Ordering | No duplicate sequence numbers | verified |

### Cross-cutting invariants

- Every event in the log has a valid signature verifiable against the
  signer's anchored public key.
- The sequencer's events are signed with the sequencer's KERI AID key.
- No two members share the same public key prefix.
- The event log satisfies global sequence invariants: contiguous,
  monotonically increasing sequence numbers; monotonically non-decreasing
  timestamps; no duplicate sequence numbers.

---

## Phase 3 — Protocol completeness

Features that the semantics document already specifies but the implementation
does not yet enforce.

- **Automatic proposal resolution** — the sequencer emits a
  `CEResolveProposal` event automatically when the admin-majority threshold
  is reached, or when the deadline passes (timeout)
- **Sequencer rotation with key migration** — `RotateSequencer` must carry
  the new sequencer's public key; both the old and new sequencer sign the
  rotation event
- **Freshness check** — submitted events must reference a recent sequence
  number; stale events are rejected

---

## Phase 4 — Application layer and production readiness

The library's polymorphic `(d, p, r)` layer is currently exercised only with
`Unit` types. This phase demonstrates and hardens real usage.

- **Reference application** — a concrete `(d, p, r)` instantiation with a
  non-trivial decision type, serving as documentation and integration test
- **Client UI** — extend the Halogen demo to cover the full member lifecycle
  (join, vote, rotate)
- **Deployment guide** and Nix service module
- **Hackage publication** — `cabal check`, bounds, changelog, `cabal sdist`
