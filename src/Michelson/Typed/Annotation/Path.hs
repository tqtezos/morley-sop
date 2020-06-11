{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.Annotation.Path where

import Prelude hiding (show, many, note, State)
import Text.Show

import Data.Aeson (ToJSON(..))
import qualified Data.Text as T
import Data.Singletons.TH
import Data.Singletons.TypeLits

import Data.Constraint.HasDict1

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- | An entrypoint-`Path` is a `Path` where the annotations are `Symbol`'s
type EpPath = Path Symbol

-- | A @`Path` a@ is an extended entrypoint over Michelson
-- types `Annotated` with @a@
data Path a where
  (:*) :: Path a -> Path a -> Path a
  (:+) :: a -> Path a -> Path a
  Here :: Path a
  deriving (Eq, Ord, Read, Functor, Foldable, Traversable, Generic)

-- | Pretty-print a @`Path` `Text`@
ppPath :: Path Text -> Text
ppPath =
  \case
    (:*) x y -> T.intercalate "*" $ collectProds $ x :* y -- . filter (not . T.null)
    (:+) x y -> T.intercalate "%" $ collectSums  $ x :+ y -- . filter (not . T.null)
    Here -> mempty
  where
    parens :: Text -> Text
    parens xs
      | T.null xs = ""
      | not $ liftM2 (||) (== '*') (== '%') `T.any` xs = xs
      | otherwise = mconcat ["(", xs, ")"]

    collectProds :: Path Text -> [Text]
    collectProds =
      \case
        (:*) x y -> collectProds x ++ collectProds y
        x -> [parens $ ppPath x]

    collectSums :: Path Text -> [Text]
    collectSums =
      \case
        (:+) x y -> x : collectSums y
        x -> [parens $ ppPath x]

-- deriving instance Show a => Show (Path a)

instance Show (Path Text) where
  show = T.unpack . ppPath
--
--   showsPrec _ Here = showString ""
--
--   showsPrec p ((:*) x y) = showParen (p > 1) $
--     showsPrec 2 x . showString " * " . showsPrec 1 y
--   showsPrec p ((:+) x y) = showParen (p > 0) $
--     showString (T.unpack x) . showString " + " . showsPrec 0 y

instance ToJSON (Path Text) where
  toJSON = toJSON . show

-- instance Show a => ToJSON (Path a) where
--   toJSON = toJSON . id @String . show
--
-- instance FromJSON a => FromJSON (Path a)

instance a ~ Text => Arbitrary (Path a) where
  arbitrary = oneof $
    [ liftM2 (:*) arbitrary arbitrary
    , liftM2 (:+) (fromString <$> arbitrary) arbitrary
    , return Here
    ]

  shrink ((:*) x y) = x : y : liftM2 (:*) (shrink x) (shrink y)
  shrink ((:+) x y) = y : liftM2 (:+) (fmap fromString . shrink . T.unpack $ x) (shrink y)
  shrink Here = []


$(genSingletons [''Path])
$(singEqInstance ''Path)
$(singOrdInstance ''Path)
$(singShowInstance ''Path)

instance HasDict1 a => HasDict1 (Path a) where
  evidence1 = $(gen_evidence1 ''Path)

