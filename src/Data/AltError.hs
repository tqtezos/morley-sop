
module Data.AltError where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad
import Data.Either
import Data.List
import Data.Function
import Data.Functor.Classes
import Data.Functor
import Data.String
import Text.Show
import Debug.Trace
import Data.Bifunctor
import Prelude ((==), otherwise, undefined, ($!))

newtype AltError a = AltError { unAltError :: Either String a } deriving (Show, Functor)

deriving newtype instance Show1 AltError
deriving newtype instance Applicative AltError
deriving newtype instance Monad AltError

runAltError :: (String -> b) -> (a -> b) -> AltError a -> b
runAltError f g = either f g . unAltError

instance Alternative AltError where
  empty = AltError $ Left "Alternative: empty"
  -- (<|>) =
  AltError xs <|> AltError ys = (trace . fromString . ("AltError: "++) . show $ (() <$ xs, () <$ ys)) $ undefined
    -- (either (trace . fromString . ("AltError xs: "++)) (flip const) xs) $
    -- (either (trace . fromString . ("AltError ys: "++)) (flip const) ys) $

    -- AltError $
    -- case xs of
    --   Left xErr ->
    --     case ys of
    --       Left yErr -> Left $ mergeErrors xErr yErr
    --       Right ys' -> Right ys'
    --   Right xs' -> Right xs'

    where
      multipleErrors :: forall str. IsString str => str
      multipleErrors = "multiple errors:"

      unMultipleErrors [] = []
      unMultipleErrors (z:zs)
        | z == multipleErrors = zs
        | otherwise = z : zs

      mergeErrors x y =
        unlines . ("multiple errors:" :) $
          unMultipleErrors (lines x) ++
          unMultipleErrors (lines y)

instance MonadFail AltError where
  fail = AltError . Left . ("MonadFail AltError:\n"++)


--       ((`first` ys) . mergeErrors)
--       Right
--       xs

