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

The sequencer's identity *can* be replaced via a **sequencer
rotation** — any admin can do this as a straight decision.

**Lean predicates:** `genesis`, `genesis_sequencer_is_member`,
`genesis_sequencer_not_admin`, `genesis_is_bootstrap`,
`genesis_preserves_sequencer_id`,
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

### Straight decisions (admin-gated)

These are accepted immediately when the signer is an admin:

- **Introduce member** — add a new non-admin member to the circle.
  Introducing someone directly as admin is rejected by the straight
  gate — admin role requires a majority proposal.
- **Remove member** — remove a member from the circle. Removing the
  sequencer is always rejected. The application gate also
  participates: it can reject a removal based on domain state
  (e.g. a member with frozen funds in an open order cannot be
  removed until the order resolves).
- **Rotate sequencer** — replace the sequencer's identity with a
  new KERI prefix. The old sequencer stays as a regular member;
  the new one takes over sequencing.

### Proposals requiring admin majority

These require a proposal with majority approval from all current
admins:

- **Promote to admin** — change a member's role to admin
- **Demote from admin** — change an admin's role to member

Admin role changes affect the power structure of the circle, so
they require consensus rather than unilateral action. Note that a
single remaining admin can demote themselves (majority of one),
which returns the circle to bootstrap mode.

**Lean predicates:** `BaseDecision`, `applyBaseDecision`, `Circle`,
`introduce_adds_member`, `introduce_admin_exits_bootstrap`,
`introduce_preserves_existing`, `baseGate`,
`rotate_accepted_by_admin`,
`rotate_new_sequencer_is_member`,
`rotate_old_sequencer_stays_member`,
`rotate_updates_sequencer_id`

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
different validation functions. Crucially, the application gate
receives **only the application fold state** — it does not see the
base fold state (membership, roles). Each layer sees only its own
fold:

```
applicationGate :: AppFoldState -> Event -> Bool
```

The sequencer applies this function after the base gate passes. If
the application gate rejects the event, it is not sequenced.

The application gate sees **all** events, including base decisions
like member removal. This allows the application to block removals
when domain invariants would be violated (e.g. a member with
active commitments cannot be removed until those commitments are
resolved).

This separation means the base infrastructure (membership, roles,
sequencing, proposals) is reusable across applications, while each
application defines its own domain-specific validity rules. The
strict layer separation — base gate uses base state, app gate uses
app state — prevents coupling between the two levels.

## Bootstrap mode

When the circle has zero admins, it is in **bootstrap mode**. The
genesis sequence is:

1. Event 0 — sequencer self-introduces as member (no gate)
2. Still in bootstrap — sequencer is not an admin
3. Event 1 — first user self-introduces as admin (passphrase-gated)
4. Normal mode begins

### Re-entering bootstrap

If the last admin demotes themselves (via a majority proposal —
which trivially passes with one admin), the circle returns to
bootstrap mode. The sequencer remains as a permanent non-admin
member (event 0 is never undone). A new admin can then
self-introduce using the passphrase, exactly as during initial
setup.

This means the circle is never stuck: as long as someone knows the
passphrase, the admin role can always be recovered.

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
