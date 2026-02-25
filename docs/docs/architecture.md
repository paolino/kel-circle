# Software Architecture

A deep analysis of kel-circle's structure, data flow, and key design
patterns. Each section focuses on one architectural concern and includes
a diagram.

---

## Package layout

The project is a single Cabal package with two test suites and a
companion PureScript client library.

```mermaid
graph TD
    subgraph Haskell["Haskell — kel-circle.cabal"]
        LIB["library<br/>13 exposed modules"]
        SRV["executable<br/>kel-circle-server"]
        UT["test-suite<br/>unit-tests"]
        E2E["test-suite<br/>e2e-tests"]
        SRV --> LIB
        UT  --> LIB
        E2E --> LIB
    end

    subgraph PS["PureScript — client/"]
        PSLIB["kel-circle-client<br/>API · types · codec · fold"]
        PSAPP["kel-circle-trivial<br/>demo UI (Halogen)"]
        PSAPP --> PSLIB
    end

    PSLIB -. "mirrors logic" .-> LIB
```

---

## Module dependency graph

Arrows point from importer to importee. Foundation modules have no
outgoing edges; the server sits at the top.

```mermaid
graph BT
    Types
    Events --> Types
    Sequence
    State --> Types & Events
    Fold --> Sequence
    Proposals --> Types & Events
    Gate --> Types & Events & State
    Processing --> Types & Events & Gate & State & Proposals
    Validate --> Types & Events & Processing
    ServerJSON["Server.JSON"] --> Types & Events & State & Proposals & Validate
    Store --> Types & Events & Processing & ServerJSON
    Server --> Types & Events & State & Processing & Validate & ServerJSON & Store
```

---

## Core type hierarchy

```mermaid
classDiagram
    class MemberId {
        +unMemberId : Text
    }
    class Role {
        <<enumeration>>
        Admin
        Member
    }
    class Member {
        +memberId : MemberId
        +memberRole : Role
        +memberName : Text
    }
    class CircleState {
        +members : List Member
    }
    class Circle {
        +circleState : CircleState
        +sequencerId : MemberId
    }
    class AuthMode {
        <<enumeration>>
        Bootstrap
        Normal
    }
    class FullState~g,p,r~ {
        +fsCircle : Circle
        +fsAppState : g
        +fsProposals : ProposalRegistry~p,r~
        +fsNextSeq : Int
    }
    class TrackedProposal~p,r~ {
        +tpProposalId : ProposalId
        +tpContent : p
        +tpProposer : MemberId
        +tpDeadline : Timestamp
        +tpResponses : List r
        +tpRespondents : List MemberId
        +tpStatus : ProposalStatus
    }
    class ProposalStatus {
        <<enumeration>>
        Open
        Resolved
    }
    class Resolution {
        <<enumeration>>
        ThresholdReached
        ProposerPositive
        ProposerNegative
        Timeout
    }

    Member "1" --> "1" MemberId
    Member "1" --> "1" Role
    CircleState "1" o-- "0..*" Member
    Circle "1" --> "1" CircleState
    Circle "1" --> "1" MemberId : sequencerId
    FullState "1" --> "1" Circle
    FullState "1" o-- "0..*" TrackedProposal
    TrackedProposal "1" --> "1" ProposalStatus
    ProposalStatus --> Resolution : when Resolved
    AuthMode ..> CircleState : derived from adminCount
```

---

## Event type hierarchy

```mermaid
classDiagram
    class CircleEvent~d,p,r~ {
        <<union>>
        CEBaseDecision BaseDecision
        CEAppDecision d
        CEProposal p Timestamp
        CEResponse r ProposalId
        CEResolveProposal ProposalId Resolution
    }
    class BaseDecision {
        <<union>>
        IntroduceMember MemberId Text Role
        RemoveMember MemberId
        ChangeRole MemberId Role
        RotateSequencer MemberId
    }
    class EventClass~d,p,r~ {
        <<union>>
        Decision d
        Proposal p Timestamp MemberId
        Response r ProposalId MemberId
    }
    class SequencedEvent~a~ {
        +seqIndex : Int
        +seqTimestamp : Timestamp
        +seqMember : MemberId
        +seqPayload : a
    }

    CircleEvent --> BaseDecision : contains
    SequencedEvent --> CircleEvent : wraps
    EventClass ..> CircleEvent : classifies
```

---

## Event flow: submission to state update

The path every event travels from the HTTP boundary to the updated
in-memory state.

```mermaid
flowchart TD
    CLIENT([Client]) -->|"POST /events<br/>JSON body"| EP["/events endpoint"]

    EP --> DEC{Decode JSON}
    DEC -->|fail| R400["400 Bad Request"]
    DEC -->|ok| MODE{AuthMode?}

    MODE -->|Bootstrap| PP{"Passphrase<br/>present & correct?"}
    PP -->|no| R401["401 Unauthorized"]
    PP -->|yes| GATE

    MODE -->|Normal| GATE

    GATE["Gate validation<br/>baseGate + appGate"] --> GR{Accept?}
    GR -->|no| R422["422 Unprocessable"]
    GR -->|yes| SQL[("SQLite<br/>append row")]

    SQL --> APPLY["Apply to FullState<br/>STM TVar update"]
    APPLY --> BCAST["Broadcast via<br/>SSE channel"]
    APPLY --> R200["200 OK<br/>{sequenceNumber: N}"]

    BCAST --> SSE([SSE subscribers])
```

---

## Two-level gate validation pipeline

Every base-level decision passes through a chain of Boolean checks
before it is accepted.

```mermaid
flowchart LR
    D[BaseDecision] --> PS{protectsSequencer?}
    PS -->|fail| REJ1["❌ rejected"]
    PS -->|pass| UN{hasUniqueName?}
    UN -->|fail| REJ2["❌ rejected"]
    UN -->|pass| BOOT{isBootstrap?}

    BOOT -->|yes| AI{"IntroduceMember<br/>+ Admin?"}
    AI -->|no| REJ3["❌ rejected"]
    AI -->|yes| ACC1["✅ accepted"]

    BOOT -->|no| ADM{signer isAdmin?}
    ADM -->|no| REJ4["❌ rejected"]
    ADM -->|yes| MAJ{requiresMajority?}
    MAJ -->|yes| REJ5["❌ rejected<br/>(needs proposal)"]
    MAJ -->|no| APPG{"appGate<br/>passes?"}
    APPG -->|no| REJ6["❌ rejected"]
    APPG -->|yes| ACC2["✅ accepted"]
```

Gates for other event types:

```mermaid
flowchart LR
    subgraph AppDecision
        AD[AppDecision] --> ADM2{"isMember<br/>signer?"}
        ADM2 -->|no| R1["❌"]
        ADM2 -->|yes| AG{appGate?}
        AG -->|no| R2["❌"]
        AG -->|yes| A1["✅"]
    end

    subgraph Response
        RE[Response] --> IM{"isMember<br/>signer?"}
        IM -->|no| R3["❌"]
        IM -->|yes| PO{"proposal<br/>open?"}
        PO -->|no| R4["❌"]
        PO -->|yes| HR{"already<br/>responded?"}
        HR -->|yes| R5["❌"]
        HR -->|no| A2["✅"]
    end

    subgraph Resolve
        RV[Resolve] --> IS{"signer ==<br/>sequencerId?"}
        IS -->|no| R6["❌"]
        IS -->|yes| A3["✅"]
    end
```

---

## Proposal lifecycle state machine

```mermaid
stateDiagram-v2
    [*] --> Open : CEProposal emitted<br/>by any member

    Open --> Open : CEResponse received<br/>(new respondent)

    Open --> Resolved_Threshold : CEResolveProposal<br/>(ThresholdReached)
    Open --> Resolved_PosProposer : CEResolveProposal<br/>(ProposerPositive)
    Open --> Resolved_NegProposer : CEResolveProposal<br/>(ProposerNegative)
    Open --> Resolved_Timeout : CEResolveProposal<br/>(Timeout)

    Resolved_Threshold --> [*]
    Resolved_PosProposer --> [*]
    Resolved_NegProposer --> [*]
    Resolved_Timeout --> [*]

    note right of Open
        Gate checks:
        • signer is member
        • proposal is open
        • signer has not responded yet
    end note

    note right of Resolved_Threshold
        Only the sequencer
        can emit Resolve events
    end note
```

---

## Bootstrap mode lifecycle

```mermaid
stateDiagram-v2
    [*] --> Bootstrap : server starts<br/>sequencer added<br/>(Member role, no admins)

    Bootstrap --> Bootstrap : passphrase-gated events<br/>(non-admin intros rejected)

    Bootstrap --> Normal : IntroduceMember _ Admin<br/>(first admin enters circle)

    Normal --> Normal : any base/app/proposal<br/>event under admin gate

    Normal --> Bootstrap : last admin demoted<br/>(adminCount → 0)

    note right of Bootstrap
        Only IntroduceMember with
        Admin role is permitted.
        Passphrase required.
    end note

    note right of Normal
        Admin signer required for
        membership operations.
        Proposals needed for role
        changes requiring majority.
    end note
```

---

## Fold and replay mechanism

The same fold function is used both on startup (full replay from
SQLite) and on each new event (incremental update to the TVar).

```mermaid
flowchart TD
    subgraph Startup
        DB[("SQLite<br/>events table")] -->|SELECT all rows| ROWS["Ordered rows<br/>oldest → newest"]
        ROWS --> FOLD["foldAll<br/>applyCircleEvent"]
        FOLD --> FS0[Initial FullState]
        FS0 --> TV[(TVar FullState)]
    end

    subgraph OnNewEvent["On new event"]
        EV[New event] --> VAL[Gate validation]
        VAL --> INS[INSERT INTO events]
        INS --> READ[Read current TVar]
        READ --> APP[applyCircleEvent]
        APP --> WRITE["Write TVar<br/>(STM atomic)"]
    end

    subgraph Query
        TV2[(TVar FullState)] -->|readTVarIO| RESP["HTTP response<br/>or SSE"]
    end
```

The fold is parameterized — the same infrastructure handles both the
fixed base fold and any pluggable application fold:

```mermaid
graph LR
    subgraph twoLayerFold
        BF["baseFold<br/>(membership, roles)"]
        AF["appFold<br/>(domain-specific g)"]
    end
    SEQ["[SequencedEvent (CircleEvent d p r)]"] --> BF & AF
    BF --> BS["CircleState (base)"]
    AF --> AS["g (app state)"]
```

---

## Persistence layer

```mermaid
erDiagram
    EVENTS {
        int     id          PK
        text    signer
        text    event_json
        text    signature
    }
```

```mermaid
flowchart LR
    subgraph Store["CircleStore (runtime)"]
        TV["TVar FullState<br/>(hot cache)"]
        CONN["SQLite connection"]
    end

    W[Write path] -->|1 INSERT| CONN
    CONN -->|2 apply + atomically writeTVar| TV

    R[Read path] -->|readTVarIO| TV

    CRASH([Server restart]) -->|SELECT * replay| CONN
    CONN -->|full fold| TV
```

The hot TVar ensures O(1) reads; SQLite ensures durability across
restarts without requiring a separate replay on every query.

---

## Server HTTP interface

```mermaid
sequenceDiagram
    participant C as Client
    participant S as Server
    participant DB as SQLite
    participant CH as SSE channel

    C->>S: POST /events {signer,event,...}
    S->>S: decode + gate validation
    alt rejected
        S-->>C: 401 / 422
    else accepted
        S->>DB: INSERT row
        S->>S: apply → TVar
        S->>CH: broadcast seqN
        S-->>C: 200 {sequenceNumber: N}
    end

    C->>S: GET /events?after=N
    S->>DB: SELECT row at N+1
    alt exists
        S-->>C: 200 {signer, event, signature}
    else missing
        S-->>C: 404
    end

    C->>S: GET /stream
    S-->>C: SSE text/event-stream
    loop on each new event
        CH->>S: seqN
        S-->>C: "event: new data: {sn: N}"
    end
```

---

## Client sync loop

The PureScript client never trusts state from the server directly —
it re-derives state by replaying the event log from its last known
checkpoint.

```mermaid
flowchart TD
    START([App start]) --> LOAD["Load checkpoint<br/>from localStorage"]
    LOAD --> CONN["Connect to GET /stream<br/>SSE"]

    CONN --> LISTEN{"SSE event<br/>arrives?"}
    LISTEN -->|yes: sn=N| CMP{N > checkpoint?}
    CMP -->|no| LISTEN

    CMP -->|yes| FETCH[GET /events?after=checkpoint]
    FETCH --> MORE{more events?}
    MORE -->|yes| APPLY["applyCircleEvent<br/>through fold"]
    APPLY --> INC[increment checkpoint]
    INC --> MORE
    MORE -->|no| SAVE["Save checkpoint<br/>to localStorage"]
    SAVE --> LISTEN

    USER([User action]) --> SIGN["Sign event<br/>with private key"]
    SIGN --> POST[POST /events]
    POST --> OK{200 OK?}
    OK -->|yes| INC2[update nextSeq]
    OK -->|no| ERR[Show error]
```

---

## Server/client trust boundary

```mermaid
graph LR
    subgraph SRV["Server — authority"]
        SEQ["Sequencer identity"] --> ORD["Canonical ordering"]
        GATE["Gate enforcement"] --> ORD
        ORD --> DB["SQLite persistence"]
    end

    subgraph CLI["Client — auditor"]
        KEY["Private key"] -->|signs| POST["POST /events"]
        FOLD["Fold replay"] --> CKPT["Local checkpoint"]
    end

    SRV -->|"signed + ordered event stream"| CLI
    POST -->|"submit event"| SRV
```

| Concern | Server | Client |
|---|---|---|
| Canonical ordering | ✓ | ✗ (audits) |
| Gate enforcement | ✓ (binding) | ✓ (UX hint) |
| Private key | ✗ | ✓ |
| State derivation | fold + TVar | fold + checkpoint |
| Signature creation | ✗ | ✓ |
| Signature verification | ✓ | ✓ |

---

## Apply functions: event → state transition

Each event type maps to exactly one apply function that touches a
specific subset of `FullState`.

```mermaid
flowchart LR
    subgraph FullState
        C[fsCircle]
        G[fsAppState]
        P[fsProposals]
        N[fsNextSeq]
    end

    BD[CEBaseDecision] -->|applyBase| C & N
    AD[CEAppDecision] -->|applyAppDecision| G & N
    PR[CEProposal] -->|applyProposal| P & N
    RS[CEResponse] -->|applyResponse| P & N
    RV[CEResolveProposal] -->|applyResolve| P & N
```

`fsNextSeq` is incremented by every apply function — it is the
monotonic sequence counter that clients use to detect new events.

---

## Admin majority voting

Role changes requiring admin consensus use the proposal machinery with
a `hasAdminMajority` threshold check.

```mermaid
sequenceDiagram
    participant A as Admin A
    participant S as Server
    participant B as Admin B
    participant C as Admin C

    A->>S: CEProposal (ChangeRole x Admin) deadline=T
    S-->>A: sn=5

    B->>S: CEResponse vote sn=5
    S-->>B: sn=6

    C->>S: CEResponse vote sn=5
    S-->>C: sn=7

    Note over S: adminCount=3, majority=2<br/>2 responses ≥ 2 → threshold met

    S->>S: CEResolveProposal 5 ThresholdReached
    Note over S: sn=8, proposal resolved<br/>applyResolveWithEffect applies ChangeRole
```

The majority threshold is `adminCount / 2 + 1` — with one admin,
that admin can act alone; with three admins, two are needed.

Auto-resolution happens immediately after each `CEResponse`: the
server checks `hasAdminMajority` on the updated respondent list
and, if met, emits a `CEResolveProposal ThresholdReached` event
signed by the sequencer. The `applyResolveWithEffect` function
then applies the embedded `BaseDecision` to the circle state.

---

## Sequencer rotation

When the sequencer role moves to a new member, the old sequencer's
`memberName` is renamed to its `MemberId` string to preserve name
uniqueness.

```mermaid
sequenceDiagram
    participant Admin
    participant Server

    Note over Server: sequencerId = "seq-0"<br/>members = [{id:"seq-0",name:"sequencer",...}, ...]

    Admin->>Server: CEBaseDecision (RotateSequencer "seq-1")
    Note over Server: rename "seq-0" member → name = "seq-0"<br/>prepend {id:"seq-1", name:"sequencer", role:Member}

    Note over Server: sequencerId = "seq-1"<br/>members = [{id:"seq-1",name:"sequencer"}, {id:"seq-0",name:"seq-0"}, ...]
```

---

## Testing strategy

```mermaid
graph TD
    subgraph Unit["unit-tests"]
        GEN["Generators<br/>arbitrary MemberId / Role"]
        SEQ2["Sequence<br/>invariants"]
        BD2["BaseDecisions<br/>membership ops"]
        GT["Gate<br/>two-level checks"]
        PR2["Proposals<br/>lifecycle"]
        PR3["Processing<br/>state transitions"]
    end

    subgraph E2E2["e2e-tests"]
        HTTP["Real HTTP server<br/>on random port"]
        SC[Bootstrap scenario]
        MM[Member management]
        RL[Role change proposals]
        RB[Re-bootstrap]
        REP[Event replay]
    end

    subgraph Lean["Lean 4 (formalization)"]
        INV[Invariants]
        THM[Theorems]
    end

    GEN --> SEQ2 & BD2 & GT & PR2 & PR3
    Lean -. "mirrored by" .-> Unit
    Unit -. "composed into" .-> E2E2
```

Properties are named to mirror Lean theorems:
`bootstrap_accepts_admin_intro`, `full_gate_base_rejects`,
`resolution_dichotomy`, etc., making the correspondence between the
formal proof and the executable tests explicit.
