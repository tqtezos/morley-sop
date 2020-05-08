{-# OPTIONS -Wno-missing-export-lists #-}

module Data.Singletons.TypeLits.Util where

-- import GHC.TypeLits

-- import Lorentz
-- import Michelson.Typed.Annotation
-- import Michelson.Untyped.Annotation
-- import Michelson.Typed.T
-- import Lorentz.EntryPoints.Core

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Constraint
-- import Fcf (Exp)


-- | A proof that `Sing` implies `SingI` for `Symbol`
singISymbol :: forall (x :: Symbol). Sing x -> Dict (SingI x)
singISymbol SSym = Dict
