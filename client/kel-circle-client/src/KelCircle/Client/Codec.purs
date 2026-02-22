-- | Argonaut codecs matching Haskell KelCircle JSON.
module KelCircle.Client.Codec
  ( encodeRole
  , decodeRole
  , encodeBaseDecision
  , decodeBaseDecision
  , encodeResolution
  , decodeResolution
  , encodeCircleEvent
  , decodeCircleEvent
  , encodeMember
  , decodeMember
  , encodeSubmission
  , decodeAppendResult
  , decodeServerError
  , decodeInfoResponse
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonNull)
import Data.Argonaut.Core as J
import Data.Argonaut.Decode
  ( JsonDecodeError(..)
  , decodeJson
  , (.:)
  , (.:?)
  )
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import KelCircle.Client.Events
  ( BaseDecision(..)
  , CircleEvent(..)
  , Resolution(..)
  )
import KelCircle.Client.Types (Member, Role(..))

-- --------------------------------------------------------
-- Role
-- --------------------------------------------------------

encodeRole :: Role -> Json
encodeRole Admin = encodeJson "admin"
encodeRole MemberRole = encodeJson "member"

decodeRole :: Json -> Either JsonDecodeError Role
decodeRole json = case decodeJson json of
  Right ("admin" :: String) -> Right Admin
  Right ("member" :: String) -> Right MemberRole
  Right _ -> Left $ TypeMismatch "unknown role string"
  Left e -> Left e

-- --------------------------------------------------------
-- BaseDecision
-- --------------------------------------------------------

encodeBaseDecision :: BaseDecision -> Json
encodeBaseDecision (IntroduceMember mid name role) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "introduceMember")
    , Tuple "memberId" (encodeJson mid)
    , Tuple "name" (encodeJson name)
    , Tuple "role" (encodeRole role)
    ]
encodeBaseDecision (RemoveMember mid) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "removeMember")
    , Tuple "memberId" (encodeJson mid)
    ]
encodeBaseDecision (ChangeRole mid role) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "changeRole")
    , Tuple "memberId" (encodeJson mid)
    , Tuple "role" (encodeRole role)
    ]
encodeBaseDecision (RotateSequencer mid) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "rotateSequencer")
    , Tuple "memberId" (encodeJson mid)
    ]

decodeBaseDecision :: Json -> Either JsonDecodeError BaseDecision
decodeBaseDecision json = do
  obj <- decodeJson json
  tag :: String <- obj .: "tag"
  case tag of
    "introduceMember" -> do
      mid <- obj .: "memberId"
      name <- obj .: "name"
      roleJson <- obj .: "role"
      role <- decodeRole roleJson
      pure (IntroduceMember mid name role)
    "removeMember" -> do
      mid <- obj .: "memberId"
      pure (RemoveMember mid)
    "changeRole" -> do
      mid <- obj .: "memberId"
      roleJson <- obj .: "role"
      role <- decodeRole roleJson
      pure (ChangeRole mid role)
    "rotateSequencer" -> do
      mid <- obj .: "memberId"
      pure (RotateSequencer mid)
    _ -> Left $ TypeMismatch ("unknown BaseDecision tag: " <> tag)

-- --------------------------------------------------------
-- Resolution
-- --------------------------------------------------------

encodeResolution :: Resolution -> Json
encodeResolution ThresholdReached = encodeJson "thresholdReached"
encodeResolution ProposerPositive = encodeJson "proposerPositive"
encodeResolution ProposerNegative = encodeJson "proposerNegative"
encodeResolution Timeout = encodeJson "timeout"

decodeResolution :: Json -> Either JsonDecodeError Resolution
decodeResolution json = case decodeJson json of
  Right ("thresholdReached" :: String) -> Right ThresholdReached
  Right ("proposerPositive" :: String) -> Right ProposerPositive
  Right ("proposerNegative" :: String) -> Right ProposerNegative
  Right ("timeout" :: String) -> Right Timeout
  Right _ -> Left $ TypeMismatch "unknown resolution string"
  Left e -> Left e

-- --------------------------------------------------------
-- CircleEvent d p r
-- --------------------------------------------------------

encodeCircleEvent
  :: forall d p r
   . (d -> Json)
  -> (p -> Json)
  -> (r -> Json)
  -> CircleEvent d p r
  -> Json
encodeCircleEvent _ _ _ (CEBaseDecision bd) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "baseDecision")
    , Tuple "decision" (encodeBaseDecision bd)
    ]
encodeCircleEvent encD _ _ (CEAppDecision d) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "appDecision")
    , Tuple "decision" (encD d)
    ]
encodeCircleEvent _ encP _ (CEProposal p deadline) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "proposal")
    , Tuple "content" (encP p)
    , Tuple "deadline" (encodeJson deadline)
    ]
encodeCircleEvent _ _ encR (CEResponse r pid) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "response")
    , Tuple "content" (encR r)
    , Tuple "proposalId" (encodeJson pid)
    ]
encodeCircleEvent _ _ _ (CEResolveProposal pid res) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "resolveProposal")
    , Tuple "proposalId" (encodeJson pid)
    , Tuple "resolution" (encodeResolution res)
    ]

decodeCircleEvent
  :: forall d p r
   . (Json -> Either JsonDecodeError d)
  -> (Json -> Either JsonDecodeError p)
  -> (Json -> Either JsonDecodeError r)
  -> Json
  -> Either JsonDecodeError (CircleEvent d p r)
decodeCircleEvent decD decP decR json = do
  obj <- decodeJson json
  tag :: String <- obj .: "tag"
  case tag of
    "baseDecision" -> do
      bdJson <- obj .: "decision"
      bd <- decodeBaseDecision bdJson
      pure (CEBaseDecision bd)
    "appDecision" -> do
      dJson <- obj .: "decision"
      d <- decD dJson
      pure (CEAppDecision d)
    "proposal" -> do
      pJson <- obj .: "content"
      p <- decP pJson
      deadline <- obj .: "deadline"
      pure (CEProposal p deadline)
    "response" -> do
      rJson <- obj .: "content"
      r <- decR rJson
      pid <- obj .: "proposalId"
      pure (CEResponse r pid)
    "resolveProposal" -> do
      pid <- obj .: "proposalId"
      resJson <- obj .: "resolution"
      res <- decodeResolution resJson
      pure (CEResolveProposal pid res)
    _ -> Left $ TypeMismatch ("unknown CircleEvent tag: " <> tag)

-- --------------------------------------------------------
-- Member
-- --------------------------------------------------------

encodeMember :: Member -> Json
encodeMember m =
  J.fromObject $ FO.fromFoldable
    [ Tuple "memberId" (encodeJson m.memberId)
    , Tuple "memberRole" (encodeRole m.memberRole)
    , Tuple "name" (encodeJson m.memberName)
    ]

decodeMember :: Json -> Either JsonDecodeError Member
decodeMember json = do
  obj <- decodeJson json
  memberId <- obj .: "memberId"
  roleJson <- obj .: "role"
  memberRole <- decodeRole roleJson
  memberName <- obj .: "name"
  pure { memberId, memberRole, memberName }

-- --------------------------------------------------------
-- Submission (for POST /events)
-- --------------------------------------------------------

encodeSubmission
  :: forall d p r
   . (d -> Json)
  -> (p -> Json)
  -> (r -> Json)
  -> { passphrase :: Maybe String
     , signer :: String
     , signature :: String
     , event :: CircleEvent d p r
     }
  -> Json
encodeSubmission encD encP encR sub =
  J.fromObject $ FO.fromFoldable
    [ Tuple "passphrase"
        ( case sub.passphrase of
            Nothing -> jsonNull
            Just p -> encodeJson p
        )
    , Tuple "signer" (encodeJson sub.signer)
    , Tuple "signature" (encodeJson sub.signature)
    , Tuple "event" (encodeCircleEvent encD encP encR sub.event)
    ]

-- --------------------------------------------------------
-- AppendResult
-- --------------------------------------------------------

decodeAppendResult
  :: Json -> Either JsonDecodeError { sequenceNumber :: Int }
decodeAppendResult json = do
  obj <- decodeJson json
  sn <- obj .: "sequenceNumber"
  pure { sequenceNumber: sn }

-- --------------------------------------------------------
-- ServerError
-- --------------------------------------------------------

decodeServerError
  :: Json
  -> Either JsonDecodeError
       { error :: String, message :: Maybe String }
decodeServerError json = do
  obj <- decodeJson json
  err <- obj .: "error"
  msg <- obj .:? "message"
  pure { error: err, message: msg }

-- --------------------------------------------------------
-- InfoResponse (GET /info)
-- --------------------------------------------------------

decodeInfoResponse
  :: Json
  -> Either JsonDecodeError
       { adminEmails :: Array String
       , pendingIntroduction :: Boolean
       }
decodeInfoResponse json = do
  obj <- decodeJson json
  emails <- obj .: "adminEmails"
  pending <- obj .: "pendingIntroduction"
  pure { adminEmails: emails, pendingIntroduction: pending }
