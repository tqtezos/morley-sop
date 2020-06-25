{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoTemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-orphans -Wno-deprecations #-}

module Michelson.Typed.EntryPoints.Sing.Alg.OptParse where

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Kind
import Data.Either
import Data.List (isInfixOf)
import Data.String
import Data.Type.Equality
import Prelude hiding (fail, lines, unwords, unlines, show, forM_, (%~))
import GHC.Generics ((:.:)(..))
import Text.Show
import System.Environment (getArgs)

import Lorentz (Value)
import Michelson.Typed.Scope
import Michelson.Printer
import qualified Michelson.TypeCheck.Types as TypeCheck
import Michelson.Typed.EntryPoints
import Michelson.Typed.Instr

import Control.AltError
import Data.AltError
import Data.AltError.TH
import Data.AltError.Run
import Data.Singletons.WrappedSing

import Michelson.Typed.Annotation.Path
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.EntryPoints.Sing.Alg
-- import Michelson.Typed.EntryPoints.Sing.Alg.Aeson (assertOpAbsense) -- ExampleParam,
import Michelson.Typed.EntryPoints.Sing.Alg.Field
import Michelson.Typed.EntryPoints.Sing.Alg.Fields
import Michelson.Typed.EntryPoints.Sing.Alg.Types.TH
import Michelson.Typed.T.Alg
import Michelson.Typed.T.Sing (assertOpAbsense)
import Michelson.Typed.Value.Free
import Michelson.Typed.Value.Parse
import qualified Michelson.Typed.Annotation.Sing.Notes as Michelson


import Data.Constraint
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TypeLits (Symbol)
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Tuple

import Data.Constraint.HasDict1

import Data.SOP (NP, NS)
import qualified Data.SOP as SOP
import qualified Data.SOP.Constraint as SOP

-- import Data.Aeson hiding (Value, Success)
import Options.Applicative
import Options.Applicative.Help (renderHelp) -- (parserUsage, putDoc, renderPretty, displayS)
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

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

-- | Unnamed fields are marked with @%unnamed@
--
-- (The @%@ symbol is not allowed in Michelson annotations.)
parseEpField :: forall f t ann epPath fieldName. (Applicative f, SingI t, SingI ann, SingI epPath, SingI fieldName)
  => Parser (EpField f t ann epPath fieldName)
parseEpField =
  withDict1 (sEpFieldT @Symbol @ErrM (sing @t) (sing @ann) (sing @epPath) (sing @fieldName)) $
  EpField (sing @fieldName) <$>
  parseRunAltE
    (pure $ WrapSing sing)
    (Comp1 . pure <$> parseValueOpq (fromMaybe "%unnamed" $ T.unpack <$> fromSing (sing @fieldName)))
    sing

parseEpFields :: forall f t ann (abbrevPath :: EpPath) epPath.
  ( Applicative f
  , SingI t, SingI ann, SingI abbrevPath, SingI epPath
  )
  => Mod CommandFields (RunSnd (EpFields f t ann) '(abbrevPath, epPath))
parseEpFields =
  command (show $ fromSing (sing @abbrevPath)) . flip info mempty $
  RunSnd <$>
  EpFields (sing @epPath) <$>
  parseRunAltE @_ @WrappedSing @_ @Parser @(EpFieldNamesErrM t ann epPath)
    (pure $ WrapSing sing)
    (parseNP parseEpField)
    (sEpFieldNamesErrM (sing @t) (sing @ann) (sing @epPath))

parserResultHelp :: AltError [String] m => ParserResult a -> m ParserHelp
parserResultHelp (Success _) = altFail ["parserResultHelp: Success"]
parserResultHelp (Failure (ParserFailure xs)) =
  case xs "morley-sop" of
    (parserHelp', _, _) -> pure parserHelp'
parserResultHelp (CompletionInvoked _) = altFail ["parserResultHelp: CompletionInvoked"]

getCommandHelp :: AltError [String] m => Parser a -> String -> m String
getCommandHelp p cmd =
  fmap (renderHelp eightyCols) $
  parserResultHelp $
  execParserPure (prefs showHelpOnEmpty) (info p fullDesc) [cmd]
  where
    eightyCols = 80

-- | Show help
voidHelpOption :: Parser Void
voidHelpOption = option (readerAbort ShowHelpText) $ mconcat
  [ noArgError ShowHelpText
  , metavar "" ]

data RunSnd (f :: b -> Type) (xs :: (a, b)) where
  RunSnd :: forall a b f (x :: a) (y :: b). f y -> RunSnd f '(x, y)

unRunSnd :: RunSnd f '(x, y) -> f y
unRunSnd (RunSnd x) = x

class SingI xs => IsSnd (xs :: (a, b)) (z :: b) where
  runSnd :: forall f. RunSnd f xs -> f z

instance (SingI x, SingI y, y ~ z) => IsSnd '(x, y) z where
  runSnd = unRunSnd

-- | This assertion will `error` if the constraint does not hold
--
-- `epPaths` could be redefined to @map snd . epPathsAbbrev@,
-- in which case this assertion could be replaced with a straightforward proof.
assertEpPathsAbbrev ::
     forall t (ann :: AnnotatedAlg Symbol t).
     Sing t
  -> Sing ann
  -> Dict ( SOP.SameShapeAs (EpPathsAbbrev ann) (EpPaths ann)
          , SOP.SameShapeAs (EpPaths ann) (EpPathsAbbrev ann)
          , SOP.AllZip IsSnd (EpPathsAbbrev ann) (EpPaths ann))
assertEpPathsAbbrev _st sann = either (error . fromString) id $ do
  let epAbbrevs = sEpPathsAbbrev sann
  let eps = sEpPaths sann
  prf1 <- decSameShapeAs epAbbrevs eps
  withDict prf1 $ do
      prf2 <- decSameShapeAs eps epAbbrevs
      withDict prf2 $ do
        prf3 <- decAllZipF decIsSnd epAbbrevs eps
        return $
          withDict prf3 $
          withDict (singAllSingI epAbbrevs) $
          withDict (singAllSingI eps) $
          Dict

-- | Decide whether `IsSnd` holds for the inputs or fail with an error
decIsSnd ::
     forall a b (x :: (a, b)) (y :: b). (HasDict1 a, HasDict1 b, SDecide b, SingKind b, Show (Demote b))
  => Sing x
  -> Sing y
  -> Either String (Dict (IsSnd x y))
decIsSnd = \case
  STuple2 sx sy -> withDict1 sx $ withDict1 sy $ \sz ->
    case sy %~ sz of
      Proved Refl -> return Dict
      Disproved _ -> Left $ "decIsSnd: unequal values: " ++ show (fromSing sy, fromSing sz)

-- | Decide whether `SOP.AllZipF` holds for the inputs or fail with an error
decAllZipF ::
     forall a b (c :: a -> b -> Constraint) (xs :: [a]) (ys :: [b]). (HasDict1 a, HasDict1 b, SOP.SameShapeAs xs ys, SOP.SameShapeAs ys xs)
  => (forall x y. Sing x -> Sing y -> Either String (Dict (c x y)))
  -> Sing xs
  -> Sing ys
  -> Either String (Dict (SOP.AllZipF c xs ys))
decAllZipF f =
  \case
    SCons sx sxs ->
      \case
        SCons sy sys -> do
          cxy <- f sx sy
          cxys <- decAllZipF f sxs sys
          return $
            withDict cxy $
            withDict cxys $
            withDict (singAllSingI sxs) $
            withDict (singAllSingI sys) Dict
    SNil ->
      \case
        SNil -> return Dict

-- | Decide whether `SOP.SameShapeAs` holds for the inputs or fail with an error
decSameShapeAs ::
     forall a b (xs :: [a]) (ys :: [b]).
     (SingKind a, Show (Demote a), SingKind b, Show (Demote b))
  => Sing xs
  -> Sing ys
  -> Either String (Dict (SOP.SameShapeAs xs ys))
decSameShapeAs =
  \case
    SNil ->
      \case
        SNil -> return Dict
        SCons sy sys -> Left $ "decSameShapeAs: right larger than left:" <> show (fromSing sy, fromSing sys)
    SCons sx sxs ->
      \case
        SNil -> Left $ "decSameShapeAs: left larger than right:" <> show (fromSing sx, fromSing sxs)
        SCons _sy sys ->
          case decSameShapeAs sxs sys of
            Left err -> Left err
            Right Dict -> return Dict

parseEpValue ::
     forall t ann. (SingI t, SingI ann)
  => (String, Parser (EpValue t ann))
parseEpValue =
  withDict1 (sEpPathsAbbrev (sing @ann)) $
  withDict (singAllSingI (sEpPaths (sing @ann))) $
  withDict (singAllSingI (sEpPathsAbbrev (sing @ann))) $
  (,)
  -- "\n[parseEpValue]\n" $
    (fromString . unlines $
     caseAltE
       (\isFail -> (("parseEpValue: " ++ bool "errored" "failed" isFail ++ ": ") :))
       (filter (not . isHelpLine) . Data.String.lines . unlines) $
     getCommandHelp
       (parseNS
          @_
          @Parser
          @_
          @(EpPathsAbbrev ann)
          (const voidHelpOption)
          -- readerError -- (fmap (error "impossible") . flip abortOption mempty . ErrorMsg)
          ((case sing @x of STuple2 sxa sxb -> withDict1 sxa $ withDict1 sxb $ hsubparser (parseEpFields @(AltE [String]) @t @ann)) :: forall x. SingI x => Parser (RunSnd (EpFields (AltE [String]) t ann) x))) `traverse`
     (fmap (show . fst) . fromSing $ sEpPathsAbbrev (sing @ann))) $
  -- (fromSing $ sEpPaths (sing @ann)) $ parseNS @_ @_ @_ @(EpPaths ann) (flip option mempty . readerError) (hsubparser (parseEpFields @AltError @t @ann))) $ -- ) `displayS` "") $
  withDict (assertEpPathsAbbrev (sing @t) (sing @ann)) $
  EpValue . SOP.htrans (Proxy @IsSnd) runSnd <$>
  parseNS @_ @_ @_ @(EpPathsAbbrev ann) (const voidHelpOption)
          ((case sing @x of STuple2 sxa sxb -> withDict1 sxa $ withDict1 sxb $ hsubparser (parseEpFields @SOP.I @t @ann)) :: forall (x :: (EpPath, EpPath)). SingI x => Parser (RunSnd (EpFields SOP.I t ann) x))
  where
    isHelpLine =
      liftM2 (||) ("Available options" `isInfixOf`) ("-h,--help" `isInfixOf`)

parseValue :: forall t (ann :: SymAnn t). (SingI t, SingI ann)
  => (String, Parser (AltE [String] (Value (FromTAlg t))))
parseValue = fmap (runEpValue @(AltE [String]) (sing @t) (sing @ann)) <$> parseEpValue @t @ann

parsePrintValue :: forall t (ann :: SymAnn t). (SingI t, SingI ann)
  => Bool
  -> (String, Parser (AltE [String] TL.Text))
parsePrintValue forceSingleLine =
  withDict1 (singFromTAlg (sing @t)) $
  assertOpAbsense (singFromTAlg (sing @t)) $
  (fmap . fmap) (printTypedValue forceSingleLine) <$>
  parseValue @t @ann

-- NOTE: convert the field annotations to type annotations for convenience: sFieldToTypeAnn
parsePrintValueFromContractSource :: ()
  => Bool
  -> T.Text
  -> (String, Parser (AltE [String] TL.Text))
parsePrintValueFromContractSource forceSingleLine contractSrc =
  case parseSomeContractRaw (Left . unlines) contractSrc of
    Left err -> error . fromString $ unlines ["parsePrintValueFromContractSource: error parsing/typechecking contract:", err]
    Right (TypeCheck.SomeContract (FullContract _ (ParamNotesUnsafe paramNotes' :: ParamNotes cp) _)) ->  -- caseAltE
      case toSing (Michelson.annotatedFromNotes paramNotes') of
        SomeSing (sann :: Sing ann) ->
          let sann' =  (sUniqifyEpPaths (singToAnnotatedAlg sann)) in
            withDict1 (singToTAlg (sing @cp)) $
            withDict1 sann' $
            parsePrintValue @(ToTAlg cp) @((UniqifyEpPaths (ToAnnotatedAlg ann))) forceSingleLine

parsePrintValueFromContract :: IO ()
parsePrintValueFromContract = do
  args <- getArgs
  case args of
    [] -> error "The first argument should be a Michelson contract"
    (contractSrc:args') -> do
      -- putDoc $ parserUsage entryPointsParserPrefs (parsePrintValueFromContractSource True $ fromString contractSrc) "hello world!"
      -- putStrLn @String ""
      let (helpLines, parsedValue') = parsePrintValueFromContractSource True $ fromString contractSrc
      bool
         (do
           michelsonStr' <- handleParseResult $
             execParserPure
               entryPointsParserPrefs
               (flip info fullDesc $ parsedValue')
               args'
           caseAltE (\isFail x -> fail . unlines $ "unable to parse args:" : show (isFail, x) : args') TL.putStrLn michelsonStr')
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

-- exampleParseEpValue :: [String] -> IO ()
-- exampleParseEpValue xs =
--   case toSing (Michelson.annotatedFromNotes (epdNotes @EpdPlain @ExampleParam)) of
--     SomeSing (sann :: Sing ann) ->
--       let sann' = singToAnnotatedAlg sann in
--         withDict1 sann' $
--         handleParseResult (execParserPure (prefs showHelpOnError) (info (snd $ parseEpValue @(ToTAlg (ToT ExampleParam)) @(ToAnnotatedAlg ann)) fullDesc) xs) >>=
--         putStrLn . encode






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


