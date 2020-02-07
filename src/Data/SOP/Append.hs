{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Data.SOP.Append where

import Data.Either
import Prelude (($))

import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (SOP(..))
import qualified Data.SOP as SOP

import Data.List.Append

-- | Prepend a list onto `SOP`
prependSOP :: Sing xs -> SOP f ys -> SOP f (xs ++ ys)
prependSOP SNil sop = sop
prependSOP (SCons _ sxs) sop =
  SOP $ SOP.S $ SOP.unSOP $ prependSOP sxs sop

-- | Postpend a list onto `SOP`
postpendSOP :: SOP f xs -> proxy ys -> SOP f (xs ++ ys)
postpendSOP (SOP (SOP.S sop')) pys =
  SOP $ SOP.S $ SOP.unSOP $ postpendSOP (SOP sop') pys
postpendSOP (SOP (SOP.Z ps)) _ =
  SOP $ SOP.Z ps

-- | Un-append `SOP`
unappendSOP :: Sing xs -> Sing ys -> SOP f (xs ++ ys) -> Either (SOP f xs) (SOP f ys)
unappendSOP SNil _ sop =
  Right sop
unappendSOP (SCons _ sxs) sys (SOP sop) =
  case sop of
    SOP.Z sop' ->
      Left $
      SOP $
      SOP.Z sop'
    SOP.S sop' ->
      case unappendSOP sxs sys $ SOP sop' of
        Left (SOP sop'') -> Left $
          SOP $
          SOP.S sop''
        Right sop'' -> Right sop''

