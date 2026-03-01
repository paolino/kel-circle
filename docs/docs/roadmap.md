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

## Phase 2 — Cryptographic integrity (per-member KELs)

Every circle member is identified by their KERI prefix — a
self-certifying identifier backed by a full Key Event Log (KEL).
The server is the **sole writer** of all member KELs and verifies
every submission against the signer's current key state. This
phase implements that architecture.

### Design principles

1. **One KEL per member.** The server stores each member's KEL
   (inception + interaction events) alongside the global circle
   sequence.
2. **Circle events are KERI anchors.** When a member submits a
   circle event, the server wraps it as a KERI interaction event
   (in the `anchors` field) and appends it to that member's KEL.
3. **Inception on introduction.** When `IntroduceMember` is
   accepted, the new member submits their KERI inception event.
   The server creates their KEL. Until inception is received, the
   member cannot submit circle events.
4. **Signature verification via KEL replay.** The server replays
   the signer's KEL to derive their current `KeyState`, then
   verifies the submission signature against those keys.
5. **Key rotation via KERI rotation events.** Members rotate keys
   by submitting a KERI rotation event (with pre-rotation
   commitments). This updates their KEL without affecting the
   circle sequence.

### Current state

| Component | Status |
|-----------|--------|
| `keri-hs` dependency wired in | ✅ done (PR #14) |
| `MemberId` validated as CESR Ed25519 prefix | ✅ done (PR #14) |
| `KelCircle.Crypto.validateCesrPrefix` | ✅ done (PR #14) |
| `InvalidMemberId` validation error | ✅ done (PR #14) |
| E2E tests use real Ed25519 keypairs | ✅ done (PR #14) |
| Per-member KEL storage (`member_kels` table) | ✅ done (PR #23) |
| `KelCircle.MemberKel` module | ✅ done (PR #23) |
| Inception event on member introduction | ✅ done (PR #23) |
| Circle events wrapped as KERI interactions | ✅ done (PR #24) |
| `KelCircle.InteractionVerify` module | ✅ done (PR #24) |
| Signature verification via KEL key state | ✅ done (PR #24) |
| Real Ed25519 signatures in submissions | ✅ done (PR #24) |
| Key rotation with pre-rotation commitments | ✅ done (PR #26) |
| `KelCircle.RotationVerify` module | ✅ done (PR #26) |
| PureScript client crypto | ❌ remaining |
| Cross-cutting integrity E2E invariants | ❌ remaining |

**Test counts:** 79 unit tests, 33 E2E tests (112 total).

### Sub-roadmap

Phase 2 is split into six incremental steps. Each step is
independently buildable and testable.

#### Step 2.1 — CESR validation for MemberId ✅

Wire `keri-hs` into kel-circle. Validate that every `MemberId`
used in `IntroduceMember` is a well-formed CESR-encoded Ed25519
public key prefix.

**Done:** PR #14 merged. `KelCircle.Crypto.validateCesrPrefix`
rejects non-CESR member IDs at the gate. 60 unit + 21 E2E tests
passing.

#### Step 2.2 — Per-member KEL storage and inception ✅

Add a `member_kels` table and wire inception into the member
introduction flow. Every introduced member has a KEL with an
inception event.

**Done:** PR #23.

- `member_kels` SQLite table (member_id, seq_num, event_json,
  signatures_json)
- `KelCircle.MemberKel` module: `MemberKel`, `KelEvent`,
  `createInception`, `appendInteraction`, `currentKeyState`
- `FullState` gains `fsMemberKels :: Map MemberId MemberKel`
- Bootstrap flow creates admin's KEL with inception
- Members without inception cannot submit events
- KEL deleted on member removal

#### Step 2.3 — Circle events as KERI interaction events ✅

Every circle event submission is wrapped as a KERI interaction
event and appended to the signer's KEL. Signatures are verified
against the member's current key state.

**Done:** PR #24.

- `KelCircle.InteractionVerify` module
- Server builds KERI interaction event with circle data in
  `anchors`, verifies signature via `verifySignatures`
- `signSubmission` in E2E helpers produces real Ed25519
  signatures over KERI interaction events
- E2E tests: valid signature accepted, forged rejected,
  wrong-key rejected, tampered payload rejected, KEL grows

#### Step 2.4 — Key rotation ✅

Members can rotate their signing keys via KERI rotation events.
After rotation, subsequent events must be signed with the new
key.

**Done:** PR #26.

- `KelCircle.RotationVerify` module
- `POST /members/<id>/rotate` endpoint
- Pre-rotation commitment verification
- Old key rejected after rotation
- E2E tests: rotation accepted, old key rejected, bad
  pre-commitment rejected

#### Step 2.5 — PureScript client crypto

Browser-side Ed25519 key generation, KERI event construction,
and signing.

**Changes:**

- PureScript FFI module wrapping `@noble/ed25519` for key
  generation and signing
- CESR encoding of public keys and signatures
- KERI interaction event construction (matching server's
  expected format)
- Key storage in `localStorage`, encrypted with AES-GCM via
  Web Crypto
- Inception flow on first use: generate keypair, build inception
  event, sign it, submit to server
- Halogen UI: key generation, passphrase prompt, auth state
  indicator

**Done when:** the Halogen demo can bootstrap an admin, introduce
members, and submit events — all with real KERI-backed
signatures.

#### Step 2.6 — Cross-cutting integrity invariants

Final verification pass: every event in the global sequence is
backed by a valid KERI interaction event in the signer's KEL.

**E2E tests:**

| # | Scenario | Expected |
|---|----------|----------|
| 13 | Full log replay: every signature verifiable via KEL | verified |
| 14 | Sequencer events signed with sequencer key | verified |
| 15 | No two members share the same prefix | verified |
| 16 | Global sequence invariants (contiguity, monotonicity) | verified |

**Cross-cutting invariants (checked after every multi-event
scenario):**

- Every event in the log has a corresponding KERI interaction
  event in the signer's KEL
- Replaying each member's KEL yields a valid `KeyState`
- Every signature verifies against the signer's key state at
  the time of signing
- No two members share the same KERI prefix
- The global sequence is contiguous with monotonically
  increasing sequence numbers and timestamps

**Done when:** all scenarios pass, all invariants hold, CI green.

### Security E2E scenarios (summary)

| # | Step | Category | Scenario | Status |
|---|------|----------|----------|--------|
| 1 | 2.2 | Inception | Member inception stored | ✅ |
| 2 | 2.2 | Inception | No inception → cannot submit | ✅ |
| 3 | 2.2 | Inception | Bootstrap admin has inception | ✅ |
| 4 | 2.2 | Inception | Wrong-prefix inception rejected | ✅ |
| 5 | 2.3 | Signature | Valid KERI-backed signature accepted | ✅ |
| 6 | 2.3 | Signature | Forged signature rejected | ✅ |
| 7 | 2.3 | Signature | Wrong-key signature rejected | ✅ |
| 8 | 2.3 | Signature | Tampered payload rejected | ✅ |
| 9 | 2.3 | Signature | Member KEL grows per event | ✅ |
| 10 | 2.4 | Rotation | New key accepted after rotation | ✅ |
| 11 | 2.4 | Rotation | Old key rejected after rotation | ✅ |
| 12 | 2.4 | Rotation | Bad pre-commitment rejected | ✅ |
| 13 | 2.6 | Integrity | Full log verifiable via KELs | ❌ |
| 14 | 2.6 | Integrity | Sequencer events properly signed | ❌ |
| 15 | 2.6 | Integrity | No duplicate prefixes | ❌ |
| 16 | 2.6 | Integrity | Global sequence invariants hold | ❌ |

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
