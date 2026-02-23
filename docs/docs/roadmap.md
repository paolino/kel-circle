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
