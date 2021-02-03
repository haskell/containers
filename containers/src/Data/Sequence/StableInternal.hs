{-# OPTIONS_HADDOCK not-home #-}

{- | This module exports a portion of 'Data.Sequence.Internal'
that is relatively stable. Specifically, the package versioning
policy (PVP) applies to this module.

Note: this module intentionally exports very little; it should be used in
conjunction with "Data.Sequence" and "Data.FingerTree.IntPlus". If you need
anything internal that is not exported, please file a GitHub issue.
-}

module Data.Sequence.StableInternal
  ( Seq (..)
  , FingerTree (..)
  , Digit (..)
  , Node (..)
  , Elem (..)
  , Sized (..)
  , splitMap ) where

import Data.Sequence.Internal
