{-# OPTIONS -Wno-missing-export-lists #-}

module Data.NS.Append where

import Data.Either
import Prelude (($))

import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (NS(..))
import qualified Data.SOP as SOP

import Data.List.Append

-- | Prepend a list onto `NS`
prependNS :: Sing xs -> NS f ys -> NS f (xs ++ ys)
prependNS SNil ps = ps
prependNS (SCons _ sxs) ps =
  SOP.S $ prependNS sxs ps

-- | Postpend a list onto `NS`
postpendNS :: NS f xs -> proxy ys -> NS f (xs ++ ys)
postpendNS (SOP.S ps') pys =
  SOP.S $ postpendNS ps' pys
postpendNS (SOP.Z ps) _ =
  SOP.Z ps

-- | Un-append `NS`
unAppendNS :: Sing xs -> proxy ys -> NS f (xs ++ ys) -> Either (NS f xs) (NS f ys)
unAppendNS SNil _ ps =
  Right ps
unAppendNS (SCons _ sxs) sys ps =
  case ps of
    SOP.Z ps' ->
      Left $
      SOP.Z ps'
    SOP.S ps' ->
      case unAppendNS sxs sys ps' of
        Left ps'' -> Left $
          SOP.S ps''
        Right ps'' -> Right ps''

