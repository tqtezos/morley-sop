{-# OPTIONS -Wno-missing-export-lists #-}

module Data.NP.Append where

import Data.Bifunctor
import Data.Kind

import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (NP(..))
import qualified Data.SOP as SOP

import Data.List.Append

-- | Append two products
appendNP ::
     NP f xs
  -> NP f ys
  -> NP f (xs ++ ys)
appendNP SOP.Nil fys = fys
appendNP ((SOP.:*) fx fxs) fys =
  fx SOP.:* appendNP fxs fys

-- | Un-append two products
unAppendNP :: forall a (f :: a -> Type) (xs :: [a]) (ys :: [a]).
     Sing xs
  -> NP f (xs ++ ys)
  -> (NP f xs, NP f ys)
unAppendNP SNil fys =
  (SOP.Nil, fys)
unAppendNP (SCons _ sxs) ((SOP.:*) fy fys) =
  (fy SOP.:*) `first` unAppendNP sxs fys


