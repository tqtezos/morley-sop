{-# OPTIONS -Wno-missing-export-lists #-}

module Data.Singletons.TH.QuoteFile where

import Control.Monad hiding (fail)
import Control.Monad.IO.Class
import Control.Monad.Fail
import Data.Either
import Data.Function
import Data.Functor
import Data.String
import Text.Show

import Data.Singletons.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import Language.Haskell.Meta.Syntax.Translate
import qualified Language.Haskell.Exts as Exts


hsFile :: QuasiQuoter
hsFile = quoteFile parseHs


hsFileSingletonsOnly :: QuasiQuoter
hsFileSingletonsOnly =
  QuasiQuoter
    { quoteExp = return $ fail "hsFileDecs only supports [Dec], not Exp"
    , quotePat = return $ fail "hsFileDecs only supports [Dec], not Pat"
    , quoteType = return $ fail "hsFileDecs only supports [Dec]. not Type"
    , quoteDec = \str -> do -- get $
        exts' <- fmap (Exts.UnknownExtension . show) <$> extsEnabled
        join . liftIO $ either fail (singletonsOnly . hsModuleDecs) . parseResultToEither <$> Exts.parseFileWithExts exts' str
    }
  -- where
  --   get :: (String -> Q a) -> String -> Q a
  --   get old_quoter file_name = do
  --     file_cts <- runIO (readFile (stripSpaces file_name))
  --     addDependentFile file_name
  --     old_quoter file_cts

  --   stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Extract all `Exts.Decl`'s from a `Exts.Module` or fail if XML is involved
getSimpleModuleDecls ::
     MonadFail m
  => Exts.Module Exts.SrcSpanInfo
  -> m [Exts.Decl Exts.SrcSpanInfo]
getSimpleModuleDecls =
  \case
    Exts.Module _loc _mModuleHead _modulePragmas _importDecls decls ->
      return decls
    _ ->
      fail
        "getSimpleModuleDecls: expected a (Haskell) Module, but got either XmlPage or XmlHybrid"

-- | Extract all `Dec`'s from a `Exts.Module`
hsModuleDecs :: MonadFail m => Exts.Module Exts.SrcSpanInfo -> m [Dec]
hsModuleDecs = fmap toDecs . getSimpleModuleDecls

-- quoteFile :: QuasiQuoter -> QuasiQuoter
-- quoteFile (QuasiQuoter { quoteExp = qe, quotePat = qp, quoteType = qt, quoteDec = qd })
--   = QuasiQuoter { quoteExp = get qe, quotePat = get qp, quoteType = get qt, quoteDec = get qd }
--   where
--    get :: (String -> Q a) -> String -> Q a
--    get old_quoter file_name = do { file_cts <- runIO (readFile file_name)
--                                  ; addDependentFile file_name
--                                  ; old_quoter file_cts }
parseHs :: QuasiQuoter
parseHs =
  QuasiQuoter
    -- | Quasi-quoter for expressions, invoked by quotes like @lhs = $[q|...]@
    { quoteExp = either fail return . parseExp
    -- | Quasi-quoter for patterns, invoked by quotes like @f $[q|...] = rhs@
    , quotePat = either fail return . parsePat
    -- | Quasi-quoter for types, invoked by quotes like @f :: $[q|...]@
    , quoteType = either fail return . parseType
    -- | Quasi-quoter for declarations, invoked by top-level quotes
    , quoteDec = either fail return . parseDecs
    }

