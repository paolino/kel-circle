{- |
Module      : KelCircle.Validate
Description : Event validation and error reporting
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Validation functions that combine the gate checks with
error reporting. Each validation function returns either
@Right ()@ or @Left ValidationError@.
-}
module KelCircle.Validate
    ( -- * Validation errors
      ValidationError (..)

      -- * Event validation
    , validateBaseDecision
    , validateAppDecision
    , validateProposal
    , validateResponse
    , validateResolve
    ) where

import Data.Text (Text)
import KelCircle.Events (BaseDecision)
import KelCircle.Processing
    ( FullState (..)
    , gateAppDecision
    , gateBaseDecision
    , gateProposal
    , gateResolve
    , gateResponse
    )
import KelCircle.Types
    ( MemberId
    , ProposalId
    )

-- | Validation errors for event submission.
data ValidationError
    = -- | Base gate rejected the decision
      BaseGateRejected BaseDecision
    | -- | App gate rejected a decision
      AppGateRejected
    | -- | Proposal gate rejected
      ProposalGateRejected
    | {- | Response gate rejected (not member,
      already responded, or proposal not open)
      -}
      ResponseGateRejected ProposalId
    | -- | Only the sequencer can resolve proposals
      NotSequencer MemberId
    | -- | MemberId is not a valid CESR Ed25519 prefix
      InvalidMemberId MemberId Text
    | -- | IntroduceMember without inception event
      MissingInception MemberId
    | -- | Inception event validation failed
      InvalidInception MemberId Text
    | -- | Interaction signature verification failed
      InteractionVerifyFailed MemberId Text
    | -- | Signer has no KEL (not yet introduced)
      SignerHasNoKel MemberId
    deriving stock (Show, Eq)

-- | Validate a base decision submission.
validateBaseDecision
    :: FullState g p r
    -> MemberId
    -> BaseDecision
    -> (g -> BaseDecision -> Bool)
    -> Either ValidationError ()
validateBaseDecision s signer d appGate
    | gateBaseDecision s signer d appGate = Right ()
    | otherwise = Left (BaseGateRejected d)

-- | Validate an application decision submission.
validateAppDecision
    :: FullState g p r
    -> MemberId
    -> d
    -> (g -> d -> Bool)
    -> Either ValidationError ()
validateAppDecision s signer content appGate
    | gateAppDecision s signer content appGate =
        Right ()
    | otherwise = Left AppGateRejected

-- | Validate a proposal submission.
validateProposal
    :: FullState g p r
    -> MemberId
    -> p
    -> (g -> p -> Bool)
    -> Either ValidationError ()
validateProposal s signer content appGate
    | gateProposal s signer content appGate =
        Right ()
    | otherwise = Left ProposalGateRejected

-- | Validate a response submission.
validateResponse
    :: FullState g p r
    -> MemberId
    -> ProposalId
    -> Either ValidationError ()
validateResponse s signer pid
    | gateResponse s signer pid = Right ()
    | otherwise = Left (ResponseGateRejected pid)

-- | Validate a proposal resolution.
validateResolve
    :: FullState g p r
    -> MemberId
    -> Either ValidationError ()
validateResolve s signer
    | gateResolve s signer = Right ()
    | otherwise = Left (NotSequencer signer)
