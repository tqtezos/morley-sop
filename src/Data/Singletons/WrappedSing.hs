{-# OPTIONS -Wno-missing-export-lists #-}

-- | Compat module until singletons >= 2.6 can be used with morley
module Data.Singletons.WrappedSing where

import Data.Kind (Type)
import Text.Show
import Prelude

import Data.Singletons
import Data.Singletons.ShowSing

----------------------------------------------------------------------
---- WrappedSing -----------------------------------------------------
----------------------------------------------------------------------

-- | A newtype around 'Sing'.
--
-- Since 'Sing' is a type family, it cannot be used directly in type class
-- instances. As one example, one cannot write a catch-all
-- @instance 'SDecide' k => 'TestEquality' ('Sing' k)@. On the other hand,
-- 'WrappedSing' is a perfectly ordinary data type, which means that it is
-- quite possible to define an
-- @instance 'SDecide' k => 'TestEquality' ('WrappedSing' k)@.
-- type WrappedSing :: k -> Type
newtype WrappedSing a where
  WrapSing :: forall k (a :: k). { unwrapSing :: Sing a } -> WrappedSing a

-- | The singleton for 'WrappedSing's. Informally, this is the singleton type
-- for other singletons.
-- type SWrappedSing :: forall k (a :: k). WrappedSing a -> Type
newtype SWrappedSing ws where
  SWrapSing :: forall k (a :: k) (ws :: WrappedSing a).
               { sUnwrapSing :: Sing a } -> SWrappedSing ws
data instance Sing :: WrappedSing a -> Type where
  SWrapSing' :: SWrappedSing ws -> Sing ws

-- type UnwrapSing :: WrappedSing a -> Sing a
type family UnwrapSing (ws :: WrappedSing a) :: Sing a where
  UnwrapSing ('WrapSing s) = s

instance SingKind (WrappedSing a) where
  type Demote (WrappedSing a) = WrappedSing a
  fromSing (SWrapSing' (SWrapSing s)) = WrapSing s
  toSing (WrapSing s) = SomeSing $ SWrapSing' $ SWrapSing s

instance forall a (s :: Sing a). SingI a => SingI ('WrapSing s) where
  sing = SWrapSing' $ SWrapSing sing

------------------------------------------------------------
-- (S)WrappedSing instances
------------------------------------------------------------

instance ShowSing k => Show (WrappedSing (a :: k)) where
  showsPrec p (WrapSing s) = showParen (p >= 11) $
    showString "WrapSing {unwrapSing = " . showsPrec 0 s . showChar '}'

instance ShowSing k => Show (SWrappedSing (ws :: WrappedSing (a :: k))) where
  showsPrec p (SWrapSing s) = showParen (p >= 11) $
    showString "SWrapSing {sUnwrapSing = " . showsPrec 0 s . showChar '}'

