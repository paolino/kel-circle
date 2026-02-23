{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : KelCircle.Server.JSON
Description : Aeson instances for kel-circle types and HTTP types
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Orphan 'ToJSON' and 'FromJSON' instances for all event
and state types. Kept separate to avoid an @aeson@
dependency in the core modules. Also defines HTTP-specific
types: 'Submission', 'AppendResult', 'ServerError'.
-}
module KelCircle.Server.JSON
    ( Submission (..)
    , AppendResult (..)
    , ServerError (..)
    ) where

import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Text (Text)
import KelCircle.Events
    ( BaseDecision (..)
    , CircleEvent (..)
    , Resolution (..)
    )
import KelCircle.Proposals
    ( ProposalStatus (..)
    , TrackedProposal (..)
    )
import KelCircle.State (AuthMode (..))
import KelCircle.Types
    ( Member (..)
    , MemberId (..)
    , Role (..)
    )
import KelCircle.Validate (ValidationError (..))

-- --------------------------------------------------------
-- HTTP-specific types
-- --------------------------------------------------------

-- | A submission from a client to append an event.
data Submission d p r = Submission
    { subPassphrase :: Maybe Text
    -- ^ Required in bootstrap mode
    , subSigner :: Text
    -- ^ CESR-encoded public key
    , subSignature :: Text
    -- ^ CESR-encoded Ed25519 signature
    , subEvent :: CircleEvent d p r
    -- ^ The event to append
    , subInception :: Maybe Value
    -- ^ Signed inception event (required for IntroduceMember)
    }
    deriving stock (Show, Eq)

-- | Successful append result.
newtype AppendResult = AppendResult
    { sequenceNumber :: Int
    }
    deriving stock (Show, Eq)

-- | Server-level errors.
data ServerError
    = ValidationErr ValidationError
    | PassphraseRequired
    | WrongPassphrase
    | SignatureError Text
    | BadRequest Text
    deriving stock (Show, Eq)

-- --------------------------------------------------------
-- Role
-- --------------------------------------------------------

instance ToJSON Role where
    toJSON Admin = String "admin"
    toJSON Member = String "member"

instance FromJSON Role where
    parseJSON = withText "Role" $ \case
        "admin" -> pure Admin
        "member" -> pure Member
        t -> fail $ "unknown Role: " <> show t

-- --------------------------------------------------------
-- MemberId
-- --------------------------------------------------------

instance ToJSON MemberId where
    toJSON (MemberId t) = toJSON t

instance FromJSON MemberId where
    parseJSON v = MemberId <$> parseJSON v

-- --------------------------------------------------------
-- Member
-- --------------------------------------------------------

instance ToJSON Member where
    toJSON m =
        object
            [ "memberId" .= memberId m
            , "role" .= memberRole m
            , "name" .= memberName m
            ]

instance FromJSON Member where
    parseJSON = withObject "Member" $ \o -> do
        mid <- o .: "memberId"
        role <- o .: "role"
        name <- o .: "name"
        pure $ MemberRecord mid role name

-- --------------------------------------------------------
-- ProposalStatus
-- --------------------------------------------------------

instance ToJSON ProposalStatus where
    toJSON Open = String "open"
    toJSON (Resolved r) =
        object
            [ "tag" .= ("resolved" :: Text)
            , "resolution" .= r
            ]

-- --------------------------------------------------------
-- TrackedProposal
-- --------------------------------------------------------

instance
    (ToJSON p, ToJSON r)
    => ToJSON (TrackedProposal p r)
    where
    toJSON tp =
        object
            [ "proposalId" .= tpProposalId tp
            , "content" .= tpContent tp
            , "proposer" .= tpProposer tp
            , "deadline" .= tpDeadline tp
            , "responses" .= tpResponses tp
            , "respondents" .= tpRespondents tp
            , "status" .= tpStatus tp
            ]

-- --------------------------------------------------------
-- BaseDecision
-- --------------------------------------------------------

instance ToJSON BaseDecision where
    toJSON (IntroduceMember mid name role) =
        object
            [ "tag" .= ("introduceMember" :: Text)
            , "memberId" .= mid
            , "name" .= name
            , "role" .= role
            ]
    toJSON (RemoveMember mid) =
        object
            [ "tag" .= ("removeMember" :: Text)
            , "memberId" .= mid
            ]
    toJSON (ChangeRole mid role) =
        object
            [ "tag" .= ("changeRole" :: Text)
            , "memberId" .= mid
            , "role" .= role
            ]
    toJSON (RotateSequencer mid) =
        object
            [ "tag" .= ("rotateSequencer" :: Text)
            , "memberId" .= mid
            ]

instance FromJSON BaseDecision where
    parseJSON = withObject "BaseDecision" $ \o -> do
        (tag :: Text) <- o .: "tag"
        case tag of
            "introduceMember" -> do
                mid <- o .: "memberId"
                name <- o .: "name"
                role <- o .: "role"
                pure $ IntroduceMember mid name role
            "removeMember" ->
                RemoveMember <$> o .: "memberId"
            "changeRole" ->
                ChangeRole <$> o .: "memberId" <*> o .: "role"
            "rotateSequencer" ->
                RotateSequencer <$> o .: "memberId"
            _ -> fail $ "unknown BaseDecision tag: " <> show tag

-- --------------------------------------------------------
-- Resolution
-- --------------------------------------------------------

instance ToJSON Resolution where
    toJSON ThresholdReached = String "thresholdReached"
    toJSON ProposerPositive = String "proposerPositive"
    toJSON ProposerNegative = String "proposerNegative"
    toJSON Timeout = String "timeout"

instance FromJSON Resolution where
    parseJSON = withText "Resolution" $ \case
        "thresholdReached" -> pure ThresholdReached
        "proposerPositive" -> pure ProposerPositive
        "proposerNegative" -> pure ProposerNegative
        "timeout" -> pure Timeout
        t -> fail $ "unknown Resolution: " <> show t

-- --------------------------------------------------------
-- CircleEvent d p r
-- --------------------------------------------------------

instance
    (ToJSON d, ToJSON p, ToJSON r)
    => ToJSON (CircleEvent d p r)
    where
    toJSON (CEBaseDecision bd) =
        object
            [ "tag" .= ("baseDecision" :: Text)
            , "decision" .= bd
            ]
    toJSON (CEAppDecision d) =
        object
            [ "tag" .= ("appDecision" :: Text)
            , "decision" .= d
            ]
    toJSON (CEProposal p deadline) =
        object
            [ "tag" .= ("proposal" :: Text)
            , "content" .= p
            , "deadline" .= deadline
            ]
    toJSON (CEResponse r pid) =
        object
            [ "tag" .= ("response" :: Text)
            , "content" .= r
            , "proposalId" .= pid
            ]
    toJSON (CEResolveProposal pid res) =
        object
            [ "tag" .= ("resolveProposal" :: Text)
            , "proposalId" .= pid
            , "resolution" .= res
            ]

instance
    (FromJSON d, FromJSON p, FromJSON r)
    => FromJSON (CircleEvent d p r)
    where
    parseJSON = withObject "CircleEvent" $ \o -> do
        (tag :: Text) <- o .: "tag"
        case tag of
            "baseDecision" ->
                CEBaseDecision <$> o .: "decision"
            "appDecision" ->
                CEAppDecision <$> o .: "decision"
            "proposal" ->
                CEProposal <$> o .: "content" <*> o .: "deadline"
            "response" ->
                CEResponse <$> o .: "content" <*> o .: "proposalId"
            "resolveProposal" ->
                CEResolveProposal
                    <$> o .: "proposalId"
                    <*> o .: "resolution"
            _ -> fail $ "unknown CircleEvent tag: " <> show tag

-- --------------------------------------------------------
-- AuthMode
-- --------------------------------------------------------

instance ToJSON AuthMode where
    toJSON Bootstrap = String "bootstrap"
    toJSON Normal = String "normal"

instance FromJSON AuthMode where
    parseJSON = withText "AuthMode" $ \case
        "bootstrap" -> pure Bootstrap
        "normal" -> pure Normal
        t -> fail $ "unknown AuthMode: " <> show t

-- --------------------------------------------------------
-- ValidationError
-- --------------------------------------------------------

instance ToJSON ValidationError where
    toJSON (BaseGateRejected bd) =
        object
            [ "error"
                .= ("baseGateRejected" :: Text)
            , "decision" .= bd
            ]
    toJSON AppGateRejected =
        object
            [ "error"
                .= ("appGateRejected" :: Text)
            ]
    toJSON ProposalGateRejected =
        object
            [ "error"
                .= ( "proposalGateRejected"
                        :: Text
                   )
            ]
    toJSON (ResponseGateRejected pid) =
        object
            [ "error"
                .= ( "responseGateRejected"
                        :: Text
                   )
            , "proposalId" .= pid
            ]
    toJSON (NotSequencer mid) =
        object
            [ "error"
                .= ("notSequencer" :: Text)
            , "memberId" .= mid
            ]
    toJSON (InvalidMemberId mid reason) =
        object
            [ "error"
                .= ("invalidMemberId" :: Text)
            , "memberId" .= mid
            , "reason" .= reason
            ]
    toJSON (MissingInception mid) =
        object
            [ "error"
                .= ("missingInception" :: Text)
            , "memberId" .= mid
            ]
    toJSON (InvalidInception mid reason) =
        object
            [ "error"
                .= ("invalidInception" :: Text)
            , "memberId" .= mid
            , "reason" .= reason
            ]

-- --------------------------------------------------------
-- Submission
-- --------------------------------------------------------

instance
    (ToJSON d, ToJSON p, ToJSON r)
    => ToJSON (Submission d p r)
    where
    toJSON s =
        object
            [ "passphrase" .= subPassphrase s
            , "signer" .= subSigner s
            , "signature" .= subSignature s
            , "event" .= subEvent s
            , "inception" .= subInception s
            ]

instance
    (FromJSON d, FromJSON p, FromJSON r)
    => FromJSON (Submission d p r)
    where
    parseJSON = withObject "Submission" $ \o ->
        Submission
            <$> o .:? "passphrase"
            <*> o .: "signer"
            <*> o .: "signature"
            <*> o .: "event"
            <*> o .:? "inception"

-- --------------------------------------------------------
-- AppendResult
-- --------------------------------------------------------

instance ToJSON AppendResult where
    toJSON r =
        object
            ["sequenceNumber" .= sequenceNumber r]

instance FromJSON AppendResult where
    parseJSON = withObject "AppendResult" $ \o ->
        AppendResult <$> o .: "sequenceNumber"

-- --------------------------------------------------------
-- ServerError
-- --------------------------------------------------------

instance ToJSON ServerError where
    toJSON (ValidationErr ve) =
        object
            [ "error" .= ("validationError" :: Text)
            , "detail" .= ve
            ]
    toJSON PassphraseRequired =
        object
            ["error" .= ("passphraseRequired" :: Text)]
    toJSON WrongPassphrase =
        object
            ["error" .= ("wrongPassphrase" :: Text)]
    toJSON (SignatureError msg) =
        object
            [ "error" .= ("signatureError" :: Text)
            , "message" .= msg
            ]
    toJSON (BadRequest msg) =
        object
            [ "error" .= ("badRequest" :: Text)
            , "message" .= msg
            ]
