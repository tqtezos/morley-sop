{-# OPTIONS -Wno-missing-export-lists #-}

module Data.SOS where

import Data.Kind

import Data.SOP

-- | A sum of sums
newtype SOS (f :: a -> Type) (xs :: [[a]]) where
  SOS :: forall a (f :: a -> Type) (xs :: [[a]]).
    { unSOS :: NS (NS f) xs }
    -> SOS f xs

