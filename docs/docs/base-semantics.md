# Base Semantics

The base semantics are the protocol-level rules that the sequencer
enforces **independently of any application**. They give the server
the ability to control access and maintain circle integrity.

## Global sequence

The sequencer maintains a monotonic counter starting at zero. Each
accepted interaction event is assigned the next index.

**Invariant 1 — no gaps, no duplicates.** Every index from 0 to
the current counter is populated by exactly one interaction event.

**Invariant 2 — strictly increasing timestamps.** Each index
carries a UTC timestamp. For any indices *i < j*, the timestamp at
*i* is strictly less than the timestamp at *j*.

**Invariant 3 — server semantic authority.** The sequencer has full
knowledge of the protocol and application semantics. It governs
what interactions are valid and controls the fold.

## Membership

Membership is a base-layer concern. The protocol itself tracks who
is in the circle. The sequencer can always determine the current
member set by folding the decision history — no application logic
required.

A member is identified by their KERI prefix (the self-certifying
identifier from their own KEL). Each member's KEL is independent
and handles inception and key rotation per the KERI specification.

## Admin roles

Admin is a distinguished base-layer role. Admins can:

- Propose membership changes (introduce, remove, change roles)
- Vote on proposals
- Submit straight decisions that affect membership

Non-admin members can participate in application-level interactions
but cannot modify the circle's membership structure.

## The two-level gate

Every event submitted to the sequencer must pass two gates before
it is accepted and assigned a sequence number.

### Level 1: base gate

The base gate is fixed by the protocol. It checks:

1. **Sequence number freshness** — the submitter's claimed sequence
   number must still be available. If another event was already
   assigned that index, the submission is rejected. This detects
   stale state (the submitter was looking at an old fold).

2. **Timestamp bounds** — the submitted timestamp must be within a
   configured accuracy window of the server's clock. This prevents
   backdated or far-future events.

3. **Membership** — the signer must be a known member of the
   circle. The signer is identified by verifying the Ed25519
   signature against the public key from their KEL.

4. **Role authorization** — the signer must have the appropriate
   role for the event type. Membership-affecting proposals require
   admin role. Application events require membership.

### Level 2: application gate

The application gate is parameterized. Different circles supply
different validation functions:

```
applicationGate :: FoldState -> Event -> Bool
```

The sequencer applies this function after the base gate passes. If
the application gate rejects the event, it is not sequenced.

This separation means the base infrastructure (membership, roles,
sequencing, proposals) is reusable across applications, while each
application defines its own domain-specific validity rules.

## The server as circle member

The sequencer is not just an external coordinator — it is a **full
circle member** with its own KEL. This matters because:

- The server generates its own Ed25519 keypair on first start
- Event 0 in the global sequence is the server's inception
- The server can emit events (specifically, decisions that resolve
  proposals — see [Event Classification](event-classification.md))
- The server's identity is verifiable via its KEL like any other
  member

## Bootstrap mode

When the circle has zero admins (initial state or after all admins
are removed), it enters **bootstrap mode**. In this mode:

- Only admin introduction is accepted (the first member must be an
  admin)
- A passphrase challenge gates access
- Once at least one admin exists, normal mode resumes

This is carried over from the kelgroups design and ensures every
circle starts with a controlled onboarding step.

## Fold computation

The circle's current state is computed by folding only the
**decisions** in the global sequence. Proposals and responses are
coordination events — they do not change the fold state.

To compute the current state:

1. Walk the global sequence from index 0
2. Filter to decisions only
3. Apply each decision to the fold accumulator

This means a client that only cares about the current state can
skip all non-decision events. The full sequence is still available
for auditability and proposal lifecycle tracking.

## Client verification

Any client can independently verify the circle state:

1. Obtain the global sequence from the server
2. For each event, resolve the signer's KEL and verify the
   signature against their current key state
3. Filter to decisions and recompute the fold
4. Compare with the server's reported state

This provides full transparency — the server cannot fabricate
events because each event carries a signature from the member's
own KEL, and the member's key state is independently verifiable.
