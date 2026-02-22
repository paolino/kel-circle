{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : KelCircle.Test.Generators
Description : QuickCheck generators for protocol types
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Arbitrary instances and generators for all kel-circle
types. Used by property tests that mirror Lean theorems.
-}
module KelCircle.Test.Generators
    ( -- * Re-exports for convenience
      module Test.QuickCheck

      -- * Generators
    , genMemberId
    , genDistinctMemberIds
    , genRole
    , genMember
    , genBaseDecision
    , genResolution
    , genCircleWithAdmin
    , genFullStateBootstrap
    , genFullStateNormal
    ) where

import Data.Text (pack)
import KelCircle.Events
    ( BaseDecision (..)
    , Resolution (..)
    )
import KelCircle.Processing
    ( FullState (..)
    , applyBase
    , initFullState
    )
import KelCircle.Types
    ( Member (..)
    , MemberId (..)
    , Role (..)
    )
import Test.QuickCheck

-- | Generate a member identifier.
genMemberId :: Gen MemberId
genMemberId =
    MemberId . pack . ("member-" <>)
        <$> elements
            (map show [0 :: Int .. 9])

-- | Generate n distinct member identifiers.
genDistinctMemberIds :: Int -> Gen [MemberId]
genDistinctMemberIds n = do
    let ids =
            map
                ( MemberId
                    . pack
                    . ("member-" <>)
                    . show
                )
                [0 :: Int .. n - 1]
    pure ids

-- | Generate a role.
genRole :: Gen Role
genRole = elements [Admin, Member]

-- | Generate a member record.
genMember :: Gen Member
genMember = do
    mid <- genMemberId
    role <- genRole
    pure $ MemberRecord mid role ""

-- | Generate a base decision.
genBaseDecision :: Gen BaseDecision
genBaseDecision =
    oneof
        [ IntroduceMember <$> genMemberId <*> pure "" <*> genRole
        , RemoveMember <$> genMemberId
        , ChangeRole <$> genMemberId <*> genRole
        , RotateSequencer <$> genMemberId
        ]

-- | Generate a resolution.
genResolution :: Gen Resolution
genResolution =
    elements
        [ ThresholdReached
        , ProposerPositive
        , ProposerNegative
        , Timeout
        ]

instance Arbitrary MemberId where
    arbitrary = genMemberId

instance Arbitrary Role where
    arbitrary = genRole

instance Arbitrary Member where
    arbitrary = genMember

instance Arbitrary BaseDecision where
    arbitrary = genBaseDecision

instance Arbitrary Resolution where
    arbitrary = genResolution

{- | Generate a circle that has exited bootstrap mode
(has at least one admin).
-}
genCircleWithAdmin :: Gen (FullState () () ())
genCircleWithAdmin = do
    sid <- genMemberId
    aid <- genMemberId
    let s0 = initFullState sid ()
        s1 = applyBase s0 (IntroduceMember aid "" Admin)
    pure s1

-- | Generate a FullState in bootstrap mode (no admins).
genFullStateBootstrap :: Gen (FullState () () ())
genFullStateBootstrap = do
    sid <- genMemberId
    pure (initFullState sid ())

-- | Generate a FullState with one admin (normal mode).
genFullStateNormal :: Gen (FullState () () ())
genFullStateNormal = genCircleWithAdmin
