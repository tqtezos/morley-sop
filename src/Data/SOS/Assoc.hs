{-# OPTIONS -Wno-missing-export-lists #-}

module Data.SOS.Assoc where

import Data.Either
import Data.Kind
import Prelude (($))

import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (NS(..))
import qualified Data.SOP as SOP

import Data.SOS
import Data.List.Concat
import Data.NS.Append

-- | Associate `SOS` into `NS`
assocSOS :: forall a (f :: a -> Type) (xs :: [[a]]). ()
  => Sing xs
  -> SOS f xs
  -> NS f (Concat xs)
assocSOS SNil (SOS sos) =
  case sos of
assocSOS (SCons sx sxs) (SOS sos) =
  case sos of
    SOP.Z ss ->
      postpendNS ss (singConcat sxs)
    SOP.S sos' ->
      prependNS sx $
      assocSOS sxs $
      SOS sos'

-- | Un-associate `NS` into `SOS`
unAssocSOS :: forall a (f :: a -> Type) (xs :: [[a]]). ()
  => Sing xs
  -> NS f (Concat xs)
  -> SOS f xs
unAssocSOS SNil ss =
  case ss of
unAssocSOS (SCons (sx :: Sing x') (sxs :: Sing xs')) ss = SOS $
  case unAppendNS sx (singConcat sxs) ss of
    Left ssX -> SOP.Z ssX
    Right ssXs -> SOP.S $ unSOS $ unAssocSOS sxs ssXs

