{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.Annotation.Path where

import Prelude hiding (show)
import Text.Show

import Data.Aeson
import qualified Data.Text as T
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Constraint

import Data.Constraint.HasDict1


type EpPath = Path Symbol

data Path a where
  (:*) :: Path a -> Path a -> Path a
  (:+) :: a -> Path a -> Path a
  Here :: Path a
  deriving (Eq, Ord, Read, Functor, Foldable, Traversable, Generic)

ppPath :: Path Text -> Text
ppPath =
  \case
    (:*) x y -> T.intercalate "*" . filter (not . T.null) $ collectProds $ x :* y
    (:+) x y -> T.intercalate "%" . filter (not . T.null) $ collectSums $ x :+ y
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

--   showsPrec _ Here = showString ""

--   showsPrec p ((:*) x y) = showParen (p > 1) $
--     showsPrec 2 x . showString " * " . showsPrec 1 y
--   showsPrec p ((:+) x y) = showParen (p > 0) $
--     showString (T.unpack x) . showString " + " . showsPrec 0 y

instance ToJSON (Path Text) where
  toJSON = toJSON . show

-- instance Show a => ToJSON (Path a) where
--   toJSON = toJSON . id @String . show

-- instance FromJSON a => FromJSON (Path a)

$(genSingletons [''Path])
$(singShowInstance ''Path)

singIPath :: forall a (xs :: Path a). (forall (x :: a). Sing x -> Dict (SingI x)) -> Sing xs -> Dict (SingI xs)
singIPath singIA ((:%*) sxs sys) =
  withDict (singIPath singIA sxs) $
  withDict (singIPath singIA sys) $
  Dict
singIPath singIA ((:%+) sx sxs) =
  withDict (singIA sx) $
  withDict (singIPath singIA sxs) $
  Dict
singIPath _ SHere = Dict

instance HasDict1 a => HasDict1 (Path a) where
  evidence1 = singIPath evidence1

