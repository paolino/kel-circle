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

- **Signature verification** at the gate layer — reject events whose signature
  does not match the signer's public key
- **Member KERI AIDs** — `MemberId` must be a valid CESR-encoded Ed25519
  prefix; the corresponding public key is anchored at introduction time and
  used to verify all subsequent events from that member
- **Client key generation and storage** — browser-safe Ed25519 keygen,
  passphrase-encrypted local storage, PureScript FFI
- **Membership challenge-response** — the server issues a nonce challenge on
  connection; the client must sign it with its member key to prove identity
  before submitting events (prevents impersonation by replaying a stolen
  member ID without the private key)
- **Security E2E tests** — all scenarios below run over HTTP against a real
  server instance (same pattern as the existing E2E suite). Phase 2 is not
  complete until every scenario passes.

### Security E2E scenarios

Each scenario is an HTTP-level property that must hold. The test helpers
already provide `httpPost`, `postEventRaw`, `signSubmission` etc. — the new
tests add real Ed25519 keypairs and a challenge endpoint.

**Signature verification:**

1. **Valid signature accepted** — bootstrap an admin with a real Ed25519
   keypair; POST the introduction event signed with the matching secret key;
   expect 200.
2. **Forged signature rejected** — same setup, but replace the signature
   bytes with random data; expect 422 with a signature-verification error.
3. **Wrong-key signature rejected** — generate two keypairs A and B; sign
   A's event with B's secret key; expect 422 (signature does not match the
   declared signer's public key).
4. **Tampered payload rejected** — sign a valid event, then mutate one byte
   of the JSON body before sending; expect 422 (signature over original
   payload no longer matches).

**Member AID anchoring:**

5. **Introduction anchors public key** — introduce a member whose `MemberId`
   is a CESR-encoded Ed25519 prefix; GET /condition and verify the member's
   public key is stored.
6. **Non-CESR member ID rejected** — attempt to introduce a member whose ID
   is not a valid CESR prefix; expect 422.
7. **Subsequent events verified against anchored key** — after introduction,
   submit an event signed by the anchored key (200), then submit the same
   event signed by a different key (422).

**Challenge-response:**

8. **Challenge issued on connect** — GET /challenge?key=\<memberId\> returns
   a fresh nonce.
9. **Correct challenge response accepted** — POST /challenge with the nonce
   signed by the member's key; expect 200 and a session token.
10. **Wrong-key challenge rejected** — sign the nonce with a different key;
    expect 401.
11. **Replayed nonce rejected** — use the same signed nonce a second time;
    expect 401 (nonce already consumed).
12. **Unauthenticated POST rejected** — POST /events without a valid session
    token; expect 401.

**Sequencer ordering:**

13. **Sequence numbers are contiguous** — after a multi-event workflow, replay
    the full log via GET /events; sequence numbers must form a gap-free
    sequence 0, 1, 2, … with no holes.
14. **Sequence numbers are monotonically increasing** — no event has a sequence
    number less than or equal to any preceding event in the log.
15. **Timestamps are monotonically non-decreasing** — every event's timestamp
    is ≥ the previous event's timestamp (wall-clock ordering).
16. **No duplicate sequence numbers** — every sequence number in the log is
    unique.

**Invariants (properties that hold across all scenarios):**

- Every event in the log has a valid signature over its payload, verifiable
  against the signer's anchored public key (replay the full log via
  GET /events and verify each signature).
- The sequencer's events are signed with the sequencer's KERI AID key.
- No two members share the same public key prefix.
- The event log satisfies the global sequence invariants: contiguous,
  monotonically increasing sequence numbers; monotonically non-decreasing
  timestamps; no duplicate sequence numbers (replay the full log via
  GET /events and verify).

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
