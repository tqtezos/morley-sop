{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-redundant-constraints -Wno-orphans #-}

-- See `Data.ListError` for documentation
module Data.ListError.TH where

import Data.Bool
import Data.Semigroup
import Control.Applicative
import Data.Either
import Data.Eq
import Data.Function
import Data.String

import Control.AltError
import Control.AltError.TH
import Data.AltError
import Data.AltError.TH ()
import Data.ListError

import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Either
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Semigroup
import Data.Singletons.Prelude.Function
import Data.Singletons.TH
import Data.Singletons.Prelude.IsString


$(singletonsOnly [d|
  isPureListE :: ListE str a -> Bool
  isPureListE (ListE (Right [])) = False
  isPureListE (ListE (Right (_:_))) = True
  isPureListE (ListE (Left _)) = False

  listEToAltE :: forall s a. (IsString s, Eq s) => ListE [s] a -> AltE [s] [a]
  listEToAltE (ListE (Left err)) = altFail err
  listEToAltE (ListE (Right xs)) = pure xs

  instance Applicative (ListE [s]) where
    pure = ListE . pure . pure

    (<*>) (ListE fs) (ListE xs) = ListE (liftA2 (<*>) fs xs)

  instance AltError [s] (ListE [s]) where
    (<||>) (ListE (Left xs)) (ListE (Left ys)) = ListE (Left (xs <> ys))
    (<||>) (ListE (Left xs)) (ListE (Right _)) = ListE (Left xs)
    (<||>) (ListE (Right _)) (ListE (Left ys)) = ListE (Left ys)
    (<||>) (ListE (Right xs)) (ListE (Right ys)) = ListE (Right (xs <> ys))

    altErr = ListE . Right . const []
    altFail = ListE . Left

 |])

