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

## Circle genesis

The first two events in every circle are fixed by the protocol:

**Event 0 — sequencer self-introduction.** The sequencer introduces
itself as a non-admin member. This is a hard-coded genesis event,
not subject to any gate. The sequencer needs to be a member so it
can sign events (e.g. decisions that resolve proposals), but it is
never an admin.

**Event 1 — first admin introduction.** The first human user
introduces themselves as an admin. This happens during bootstrap
mode (no admins exist yet), gated by a passphrase challenge. After
this event, normal mode begins.

### Sequencer protection

The sequencer is a permanent, non-admin member:

- **Cannot be removed** — the base gate rejects any `removeMember`
  targeting the sequencer
- **Cannot be promoted to admin** — the base gate rejects any
  `changeRole` promoting the sequencer to admin

These protections are enforced unconditionally, regardless of mode.

**Lean predicates:** `genesisState`, `genesis_sequencer_is_member`,
`genesis_sequencer_not_admin`, `genesis_is_bootstrap`,
`sequencer_removal_rejected`, `sequencer_admin_promotion_rejected`

## Membership

Membership is a base-layer concern. The protocol itself tracks who
is in the circle. The sequencer can always determine the current
member set by folding the event history — no application logic
required.

A member is identified by their KERI prefix (the self-certifying
identifier from their own KEL). Each member's KEL is independent
and handles inception and key rotation per the KERI specification.

## Admin roles

Admin is a distinguished base-layer role. Admins can:

- Introduce new members (straight decision)
- Propose admin promotions and demotions (requires majority)
- Remove members (straight decision)

Non-admin members can participate in application-level interactions
but cannot modify the circle's membership structure.

## Base decisions

The protocol defines two categories of membership operations:

### Straight decisions (single admin)

These are accepted immediately when submitted by any admin:

- **Introduce member** — add a new non-admin member to the circle.
  Any admin can do this unilaterally.
- **Remove member** — remove a member from the circle (except the
  sequencer). Any admin can do this unilaterally.

### Proposals requiring admin majority

These require a proposal with majority approval from all current
admins:

- **Promote to admin** — change a member's role to admin
- **Demote from admin** — change an admin's role to member

Admin role changes affect the power structure of the circle, so
they require consensus rather than unilateral action.

**Lean predicates:** `BaseDecision`, `applyBaseDecision`,
`introduce_adds_member`, `introduce_admin_exits_bootstrap`,
`introduce_preserves_existing`, `baseGate`

## The two-level gate

Every event submitted to the sequencer must pass two gates before
it is accepted and assigned a sequence number.

### Level 1: base gate

The base gate is fixed by the protocol. It checks:

1. **Sequencer protection** — the event does not remove or promote
   the sequencer

2. **Mode-dependent authorization:**
    - *Bootstrap mode* — only `introduceMember` with admin role is
      accepted (passphrase-gated)
    - *Normal mode* — signer must be an admin for membership
      operations; signer must be a member for application events

3. **Sequence number freshness** — the submitter's claimed sequence
   number must still be available. If another event was already
   assigned that index, the submission is rejected. This detects
   stale state (the submitter was looking at an old fold).

4. **Timestamp bounds** — the submitted timestamp must be within a
   configured accuracy window of the server's clock. This prevents
   backdated or far-future events.

5. **Membership** — the signer must be a known member of the
   circle. The signer is identified by verifying the Ed25519
   signature against the public key from their KEL.

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

## Bootstrap mode

When the circle has zero admins, it is in **bootstrap mode**. The
genesis sequence is:

1. Event 0 — sequencer self-introduces as member (no gate)
2. Still in bootstrap — sequencer is not an admin
3. Event 1 — first user self-introduces as admin (passphrase-gated)
4. Normal mode begins

If all admins are later removed, bootstrap mode resumes.

**Lean predicates:** `isBootstrap`, `isBootstrapB`,
`empty_is_bootstrap`, `genesis_is_bootstrap`,
`bootstrap_accepts_admin_intro`, `bootstrap_rejects_member_intro`,
`bootstrap_rejects_remove`

## Fold computation

The circle's current state is computed by folding **all** events in
the global sequence. Every sequenced interaction event — decision,
proposal, or response — contributes to the fold.

The fold has two layers:

- **Base fold** — extracts membership and role information. This
  layer is fixed by the protocol. The base gate uses only the base
  fold to check membership, roles, and freshness.

- **Application fold** — accumulates domain-specific state. This
  layer is pluggable. The application gate uses the full fold
  (base + application) to validate domain rules.

To compute the current state:

1. Walk the global sequence from index 0
2. Apply each event to the fold accumulator (both base and
   application layers)

A future optimization may allow skipping fully resolved proposals
during fold replay, but the base model treats every sequenced event
as a fold input.

## Client verification

Any client can independently verify the circle state:

1. Obtain the global sequence from the server
2. For each event, resolve the signer's KEL and verify the
   signature against their current key state
3. Recompute the fold over all events
4. Compare with the server's reported state

This provides full transparency — the server cannot fabricate
events because each event carries a signature from the member's
own KEL, and the member's key state is independently verifiable.
