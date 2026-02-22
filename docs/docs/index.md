# Kel-Circle

A synchronized multi-KEL circle protocol built on
[KERI](https://weboftrust.github.io/ietf-keri/draft-ssmith-keri.html).

## From KERI to circles

KERI gives each controller a sovereign **Key Event Log** (KEL) — an
append-only, hash-chained, signed event sequence that establishes
and evolves a self-certifying identifier. Inception creates the
identifier; rotation replaces keys; interaction anchors arbitrary
data. Each controller is fully autonomous: they can do whatever
they want with their own KEL.

This sovereignty is a feature for identity management, but a
problem for **group coordination**. When multiple actors need to
agree on shared state — membership, roles, proposals, votes —
their independent KELs can evolve in any order. There is no
canonical merge: the same set of events folded in different orders
can produce different results.

### The synchronicity problem

Consider a group of three admins voting on a proposal. Each admin
appends an interaction event to their own KEL recording their vote.
Without a shared ordering:

- Admin A sees votes A, B, C in that order
- Admin B sees votes B, A, C in a different order
- The fold result may differ depending on order

Worse, there is no general way to detect conflicts or reconcile
divergent folds while keeping all KELs meaningful. The KERI model
of sovereign independence breaks down when actors need **consensus
on shared state**.

### The kel-circle solution

Kel-circle resolves this by separating two concerns:

1. **Identity** — each member keeps their own KEL for inception and
   key rotation, exactly as KERI intends. Key management stays
   sovereign.

2. **Coordination** — a **global sequence** managed by a dedicated
   server (the *sequencer*) provides canonical ordering for all
   interaction events. Every interaction gets a unique index and a
   strictly increasing UTC timestamp. No gaps, no reordering.

The sequencer is itself a circle member with its own KEL. It does
not just relay events — it understands the full protocol semantics
and enforces validity before assigning a sequence number.

### Closed membership and access control

A circle is not open. Membership is a **base-layer** concern,
meaning the protocol itself (not the application) tracks who is in
the circle and what roles they hold. The sequencer can always
determine current members and admins by folding the decision
history.

This gives the server the ability to enforce access control at the
protocol level:

- Only members can submit events
- Only admins can propose membership changes
- Every submitted event passes a **two-level gate**:
    1. **Base gate** — sequence number fresh, timestamp in bounds,
       signer is a known member with appropriate role
    2. **Application gate** — event content valid per
       application-specific semantics

The base gate is fixed by the protocol. The application gate is
parameterized — different circles can enforce different domain
rules while sharing the same coordination infrastructure.

## Architecture overview

![Kel-Circle Protocol](images/circle5.svg)

Five members — four regular (teal) and one sequencer (amber) — each
maintain their own KEL. Inception events sit at the outer edge; newer
events grow inward. Rose curves trace key rotations within a KEL.
Numbered indigo arrows follow the global sequence across KELs: most
interactions originate from members, but some (like timeout or
threshold decisions) are emitted by the sequencer itself. Only
**decisions** in the sequence advance the fold; proposals and
responses are coordination machinery.

## Documentation

- [Base Semantics](base-semantics.md) — membership, admin roles,
  the two-level gate, and how the server controls access
- [Event Classification](event-classification.md) — the three event
  classes (decisions, proposals, responses) and the proposal
  lifecycle
