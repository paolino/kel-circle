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
different validation functions. In general, the application gate
can see both the base fold state and the application fold state.
However, for **base decisions** (membership operations), only the
application fold state is needed — the base gate already handles
all membership and role checks:

```
applicationGate :: AppFoldState -> BaseDecision -> Bool
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
application defines its own domain-specific validity rules.

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

### Dependency on full KELs

Both the server and clients must fold and validate against the
**full KELs** of all circle members — not just the interaction
events in the global sequence. Each member's KEL includes inception,
key rotation, and interaction events per the KERI specification.

When the server receives a new event, it resolves the signer's KEL
to verify the signature against their current key state. Only then
does it apply the two-level gate and (if accepted) assign a sequence
number. Clients perform the same verification when replaying the
global sequence.

Without access to the full KELs, neither server nor client can
confirm that a signature is valid under the signer's current public
key (which may have been rotated since the event was signed). The
global sequence provides canonical ordering; the KELs provide
cryptographic identity.

## Server vs client: asymmetric roles

Both server and clients fold the global sequence and validate
against full KELs, but their roles are fundamentally asymmetric.

### Server: gatekeeper

The server is **always online** and maintains the **source of truth
for all member KELs**. It holds copies of every member's full KEL
(inception, key rotations, interactions) so it can resolve key
state and verify signatures at any time without depending on
external infrastructure.

Members cannot update their own KELs directly. Every KEL event —
inception, key rotation, and interaction — must be submitted to the
server. The server validates each event (correct prior digest, valid
signature) and only then appends it to its copy of that member's
KEL. The server is the **sole writer** of all KELs in the circle.

For each submitted event, the server:

1. Resolves the signer's KEL and verifies the signature
2. **Challenges membership** — confirms the signer is a known
   member of the circle (from the current base fold)
3. Applies the **base gate** — sequencer protection, mode-dependent
   authorization, sequence freshness, timestamp bounds
4. Applies the **application gate** — domain-specific validation
5. If all gates pass, assigns the next sequence number

The membership challenge is critical: it happens before any other
gate logic. An event from an unknown signer is rejected immediately,
regardless of its content.

The server is also responsible for **relaying** all data to clients.
Clients do not communicate with each other — they receive the global
sequence and all member KELs exclusively from the server. This makes
the server the single point of distribution for all circle state.

### Client: auditor

Clients replay the global sequence **after** events have been
accepted and sequenced. They:

1. Obtain the global sequence and all member KELs
2. For each event, verify the signature against the signer's KEL
3. Recompute both folds (base + application) over all events
4. Confirm the resulting state matches the server's reported state

Clients do not re-run the gate logic — the server already enforced
it. Instead, they verify that the server's decisions were consistent:
every sequenced event has a valid signature from a member who was
in the circle at the time. If the server accepted an event from a
non-member, the client's replay would detect the inconsistency.

### Checkpoint-based sync

For efficiency, clients do not replay the full history from genesis
on every connection. Instead, they maintain a **checkpoint** — a
snapshot of the fold state (both base and application layers) at a
known sequence index, along with the corresponding KEL states.

To sync, a client presents its checkpoint index to the server and
requests the delta: all events and KEL updates from that index to
the current tip. The client applies the delta to its checkpoint,
advancing to the current state.

This is analogous to blockchain light sync: the client trusts its
own checkpoint (which it previously verified) and only replays the
new events. A full replay from genesis is always possible for
complete re-verification but is not required for normal operation.

### Trust model: censor but not forge

The server has **full visibility** over all events and **full
control** over what gets sequenced — it can delay, reorder, or
reject submissions. In this sense, the server is a censor.

However, the server **cannot forge events**. Every event carries a
signature from the member's private key, and the server never holds
any member's private key. A forged event would fail signature
verification during client replay. This is the fundamental security
guarantee: the server controls the sequence, but cannot fabricate
the content.
