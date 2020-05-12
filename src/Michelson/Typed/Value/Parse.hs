{-# OPTIONS -Wno-orphans -Wno-missing-export-lists #-}

module Michelson.Typed.Value.Parse where

import Prelude (($), runReaderT)
import Data.Bool
import Data.Either
import Data.String
import Data.Monoid
import Data.Functor
import Data.Bifunctor
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Text.Show
import Data.Function

import Lorentz (Value, def)
import Michelson.Typed.T
import Michelson.Parser
import Michelson.Printer.Util
import Michelson.TypeCheck
import Michelson.Macro
import Michelson.Untyped.Type (Type(..)) -- instance RenderDoc T
import qualified Michelson.TypeCheck.Types as TypeCheck

import Text.Megaparsec (eof, parse)
import qualified Options.Applicative as Opt
import qualified Data.Text as T
import Data.Singletons


-- | Parse and typecheck a Michelson value
parseTypeCheckValue ::
     forall t. (SingI t)
  => String
  -> Opt.Parser (Value t)
parseTypeCheckValue label =
  flip Opt.option info' . Opt.eitherReader $ \paramStr ->
  first show $
  (\p -> parseNoEnv p "no-contract" $ T.pack paramStr) $
  (>>= either (fail . ("parseTypeCheckValue" <>) . show) return) $
  runTypeCheckIsolated . flip runReaderT def . typeCheckValue . expandValue <$>
  (value <* eof)
  where
    -- renderDocT pn t = renderType t True pn emptyAnnSet
    metavar' = printDocS True $ renderDoc needsParens $ (\(Type t _ann) -> t) $ toUType (fromSing (sing @t))
    info' = mconcat
      [ Opt.long label
      , Opt.metavar metavar'
      ]

-- | Parse `TypeCheck.SomeContract` from `T.Text`
parseSomeContractRaw :: Applicative f => (forall a. [String] -> f a) -> T.Text -> f TypeCheck.SomeContract
parseSomeContractRaw fail' =
  either
    (fail' . ("parseSomeContractRaw" :) . (:[]) . show)
    (either
      (fail' . ("parseSomeContractRaw" :) . (:[]) . show)
      pure .
     typeCheckContract mempty .
     expandContract
    ) .
  parse program "input_contract"

-- | `parseSomeContractRaw` as a `Opt.Parser`
parseSomeContract :: (forall a. [String] -> Opt.ReadM a) -> Opt.Parser TypeCheck.SomeContract
parseSomeContract fail' =
  Opt.argument
    (Opt.str >>= parseSomeContractRaw fail') $
    mconcat
       [ Opt.metavar "Michelson Contract Source"
       , Opt.help "A Michelson contract's source code"
       ]

