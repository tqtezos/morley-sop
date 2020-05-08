{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoTemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-orphans -Wno-deprecations #-}

module Michelson.Typed.EntryPoints.Sing.Alg.OptParse where

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Kind
import Data.Either
import Data.Functor.Classes
import Data.List (isInfixOf)
import Data.String
import Data.Type.Equality
import Prelude hiding (fail, lines, unwords, unlines, show, forM_)
import GHC.Generics ((:.:)(..))
import Text.Show
import System.Environment (getArgs)

import Lorentz (IsoValue(..), EpdPlain, Value)
import Lorentz.EntryPoints.Core
import Michelson.Typed.Scope
import Michelson.Printer
import qualified Michelson.TypeCheck.Types as TypeCheck
import Michelson.Typed.EntryPoints
import Michelson.Typed.Instr

import Data.Either.Run
import Data.Either.Run.ErrorMessage

import Michelson.Typed.Annotation.Path
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.EntryPoints.Sing.Alg
import Michelson.Typed.EntryPoints.Sing.Alg.Aeson (ExampleParam, assertOpAbsense)
import Michelson.Typed.EntryPoints.Sing.Alg.Field
import Michelson.Typed.EntryPoints.Sing.Alg.FieldNames
import Michelson.Typed.EntryPoints.Sing.Alg.Fields
import Michelson.Typed.EntryPoints.Sing.Alg.Paths
import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free
import Michelson.Typed.Value.Parse
import qualified Michelson.Typed.Annotation.Sing as Michelson


import Data.Singletons
import Data.Singletons.Prelude.Either
import Data.Singletons.Prelude.List

import Data.AltError
import Data.Constraint.HasDict1

import Data.SOP (NP, NS)
import qualified Data.SOP as SOP

import Data.Aeson hiding (Value, Success)
import Options.Applicative
import Options.Applicative.Help (renderHelp) -- (parserUsage, putDoc, renderPretty, displayS)
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

traceShow' :: Show a => a -> b -> b
traceShow' = trace . fromString . ("\n" <>) . show
-- flip const

-- instance str ~ String => MonadFail (Either str) where
--   fail = Left


parseRunEither :: forall a b (f :: a -> Type) (g :: b -> Type) (xs :: Either a b). (HasDict1 a, HasDict1 b, SingI xs)
  => (forall (x :: a). SingI x => Parser (f x))
  -> (forall (x :: b). SingI x => Parser (g x))
  -> Parser (RunEither f g xs)
parseRunEither fs gs =
  case sing @xs of
    SLeft ys -> RunLeft <$> withDict1 ys fs
    SRight ys -> RunRight <$> withDict1 ys gs

parseNS :: forall a m (f :: a -> Type) (xs :: [a]). (HasDict1 a, Alternative m, SingI xs)
  => (String -> m Void)
  -> (forall (x :: a). SingI x => m (f x))
  -> m (NS f xs)
parseNS mErr f =
  case sing @xs of
    SNil -> absurd <$> mErr "parseNS @_ @_ @[]: empty sum"
    SCons (y :: Sing x') (ys :: Sing xs') ->
      fmap SOP.Z (withDict1 y $ f @x') <|>
      fmap SOP.S (withDict1 ys $ parseNS @a @m @f @xs' mErr f)

parseNP :: forall a m (f :: a -> Type) (xs :: [a]). (HasDict1 a, Applicative m, SingI xs)
  => (forall (x :: a). SingI x => m (f x))
  -> m (NP f xs)
parseNP f =
  case sing @xs of
    SNil -> pure SOP.Nil
    SCons (y :: Sing x') (ys :: Sing xs') ->
      (SOP.:*) <$>
      withDict1 y (f @x') <*>
      withDict1 ys (parseNP @a @m @f @xs' f)


parseValueOpq :: forall t. SingI t => String -> Parser (ValueOpq t)
parseValueOpq label =
  withDict1 (singFromTOpq (sing @t)) $
  case toFromTOpq (sing @t) of
    Refl ->
      \case { VOpq xs -> xs } . toValueAlg <$>
      parseTypeCheckValue @(FromTOpq t) label

parseEpField :: forall f t ann epPath fieldName. (Applicative f, (forall (x :: TOpq). SingI x => (Show (f (ValueOpq x)))), SingI t, SingI ann, SingI epPath, SingI fieldName)
  => Parser (EpField f t ann epPath fieldName)
parseEpField =
  withDict1 (sEpFieldT (sing @t) (sing @ann) (sing @epPath) (sing @fieldName)) $
  EpField (sing @fieldName) . join traceShow' <$>
  parseRunEither (pure $ SingError sing) (Comp1 . pure <$> parseValueOpq (T.unpack $ fromSing (sing @fieldName)))

parseEpFields :: forall f t ann epPath. (Applicative f, (forall (x :: TOpq). SingI x => (Show (f (ValueOpq x)))), SingI t, SingI ann, SingI epPath)
  => Mod CommandFields (EpFields f t ann epPath)
parseEpFields =
  command (show $ fromSing (sing @epPath)) . flip info mempty $
  traceShow' (fromSing $ sEpFieldNames (sing @ann) (sing @epPath)) $
  withDict1 (sEpFieldNames (sing @ann) (sing @epPath)) $
  EpFields (sing @epPath) <$>
  parseNP parseEpField

parserResultHelp :: MonadFail m => ParserResult a -> m ParserHelp
parserResultHelp (Success _) = fail "parserResultHelp: Success"
parserResultHelp (Failure (ParserFailure xs)) =
  case xs "morley-sop" of
    (parserHelp', _, _) -> return parserHelp'
parserResultHelp (CompletionInvoked _) = fail "parserResultHelp: CompletionInvoked"

getCommandHelp :: MonadFail m => Parser a -> String -> m String
getCommandHelp p cmd =
  fmap (renderHelp eightyCols) $
  parserResultHelp $
  execParserPure (prefs showHelpOnEmpty) (info p fullDesc) [cmd]
  where
    eightyCols = 80

parseEpValue ::
     forall t ann. (SingI t, SingI ann)
  => (String, Parser (EpValue t ann))
parseEpValue =
  traceShow' (fromSing $ sEpPaths (sing @ann)) $
  withDict1 (sEpPaths (sing @ann)) $
  (,) "\n[parseEpValue]\n" $
    -- (fromString $
    --  runAltError
    --    ("parseEpValue: failed: " ++)
    --    (unlines . filter (not . isHelpLine) . Data.String.lines . unlines) $
    --  getCommandHelp
    --    (parseNS
    --       @_
    --       @_
    --       @_
    --       @(EpPaths ann)
    --       (fmap (error "impossible") . flip abortOption mempty . ErrorMsg) -- readerError
    --       (hsubparser (parseEpFields @AltError @t @ann))) `mapM`
    --  (fmap show . fromSing $ sEpPaths (sing @ann))) $
  -- (fromSing $ sEpPaths (sing @ann)) $ parseNS @_ @_ @_ @(EpPaths ann) (flip option mempty . readerError) (hsubparser (parseEpFields @AltError @t @ann))) $ -- ) `displayS` "") $
  EpValue <$>
  parseNS (fmap (error "impossible") . flip abortOption mempty . ErrorMsg) (hsubparser parseEpFields) -- readerError
  where
    isHelpLine =
      liftM2 (||) ("Available options" `isInfixOf`) ("-h,--help" `isInfixOf`)

parseValue :: forall t (ann :: SymAnn t). (SingI t, SingI ann)
  => (String, Parser (AltError (Value (FromTAlg t))))
parseValue = fmap (runEpValue @AltError (sing @t) (sing @ann)) <$> parseEpValue @t @ann

parsePrintValue :: forall t (ann :: SymAnn t). (SingI t, SingI ann)
  => Bool
  -> (String, Parser (AltError TL.Text))
parsePrintValue forceSingleLine =
  traceShow' (fromSing $ singFromTAlg (sing @t)) $
  traceShow' (fromSing $ (sing @ann)) $
  withDict1 (singFromTAlg (sing @t)) $
  assertOpAbsense (singFromTAlg (sing @t)) $
  (fmap . fmap) (printTypedValue forceSingleLine) <$>
  parseValue @t @ann

-- NOTE: convert the field annotations to type annotations for convenience: sFieldToTypeAnn
parsePrintValueFromContractSource :: ()
  => Bool
  -> T.Text
  -> (String, Parser (AltError TL.Text))
parsePrintValueFromContractSource forceSingleLine contractSrc =
  case unAltError $ parseSomeContractRaw contractSrc of
    Left err -> error . fromString $ unlines ["parsePrintValueFromContractSource: error parsing/typechecking contract:", err]
    Right (TypeCheck.SomeContract (FullContract _ (ParamNotesUnsafe paramNotes' :: ParamNotes cp) _)) ->
      case toSing (Michelson.annotatedFromNotes paramNotes') of
        SomeSing (sann :: Sing ann) ->
          let sann' = sFieldToTypeAnn (singToAnnotatedAlg sann) in
            withDict1 (singToTAlg (sing @cp)) $
            traceShow' ("original" :: String, fromSing (singToAnnotatedAlg sann)) $
            traceShow' ("next" :: String, fromSing sann') $
            withDict1 sann' $
            parsePrintValue @(ToTAlg cp) @(FieldToTypeAnn (ToAnnotatedAlg ann)) forceSingleLine

parsePrintValueFromContract :: IO ()
parsePrintValueFromContract = do
  args <- getArgs
  case args of
    [] -> error "The first argument should be a Michelson contract"
    (contractSrc:args') -> do
      -- putDoc $ parserUsage entryPointsParserPrefs (parsePrintValueFromContractSource True $ fromString contractSrc) "fooooff"
      -- putStrLn @String ""
      let (helpLines, parsedValue') = parsePrintValueFromContractSource True $ fromString contractSrc
      bool
         (do
           michelsonStr' <- handleParseResult $
             execParserPure
               entryPointsParserPrefs
               (flip info fullDesc $ parsedValue')
               args'
           runAltError (\x -> fail . unlines $ "unable to parse args:" : show x : args') TL.putStrLn michelsonStr')
         (do
           putStrLn @String "Available commands:"
           putStrLn @String ""
           putStrLn helpLines)
         (fmap ("--help" `isInfixOf`) args' == [True])
  where
    entryPointsParserPrefs :: ParserPrefs
    entryPointsParserPrefs = prefs $
      showHelpOnError <>
      showHelpOnEmpty

exampleParseEpValue :: [String] -> IO ()
exampleParseEpValue xs =
  case toSing (Michelson.annotatedFromNotes (epdNotes @EpdPlain @ExampleParam)) of
    SomeSing (sann :: Sing ann) ->
      let sann' = singToAnnotatedAlg sann in
        withDict1 sann' $
        handleParseResult (execParserPure (prefs showHelpOnError) (info (snd $ parseEpValue @(ToTAlg (ToT ExampleParam)) @(ToAnnotatedAlg ann)) fullDesc) xs) >>=
        putStrLn . encode


-- > exampleParseEpValue ["(:+) \"get\" Here", "--help"]
-- Usage: <interactive> (:+) "get" Here --user (Tc CNat)

-- Available options:
--   -h,--help                Show this help text
-- *** Exception: ExitSuccess

-- > exampleParseEpValue ["--help"]
-- Invalid option `--help'

-- Usage: <interactive> (COMMAND | COMMAND | COMMAND | ARG)

-- Available commands:
--   (:+) "get" Here
--   (:+) "" ((:+) "set" ((:*) ((:*) Here Here) ((:*) Here Here)))

--   (:+) "" ((:+) "default" Here)

-- *** Exception: ExitFailure 1

-- > exampleParseEpValue ["(:+) \"\" ((:+) \"set\" ((:*) ((:*) Here Here) ((:*) Here Here)))", "--help"]
-- Usage: <interactive> (:+) "" ((:+) "set" ((:*) ((:*) Here Here) ((:*) Here Here))) --user (Tc CNat)
--                                                                                    --val (Tc CBool)
--                                                                                    --info (Tc CString)
--                                                                                    --extra (Tc CInt)

-- Available options:
--   -h,--help                Show this help text
-- *** Exception: ExitSuccess

-- > exampleParseEpValue ["(:+) \"\" ((:+) \"set\" ((:*) ((:*) Here Here) ((:*) Here Here)))", "--user", "23", "--val", "False", "--info", "\"hello_world\"", "--extra", "42"]
-- {"path":"set","fields":{"extra":{"int":"42"},"user":{"int":"23"},"val":{"args":[],"prim":"False","annots":[]},"info":{"string":"hello_world"}}}

