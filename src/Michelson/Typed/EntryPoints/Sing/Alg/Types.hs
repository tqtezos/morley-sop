-- {-# LANGUAGE NoTemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Types where

import Data.Either
import Data.Kind
import Prelude hiding (unwords, show)

import Data.Either.Run.ErrorMessage

import Michelson.Typed.Annotation.Path
import Michelson.Typed.EntryPoints.Error

import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.T.Alg

import Data.SOP.Map
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Either


class (MonadFail m, Alternative m) => MonadAlg m
instance (MonadFail m, Alternative m) => MonadAlg m

type SymAnn = AnnotatedAlg Symbol

-- type TSym0 = T

$(singletonsOnly [d|

  -- | Either we have the expected field and return the input or fail with `EpFieldTFieldError`
  epFieldTFieldEq :: TOpq -> Symbol -> Symbol -> Bool -> ErrM TOpq
  epFieldTFieldEq t _ _ True = Right t
  epFieldTFieldEq _ fieldNameA fieldNameB False = Left (epFieldTFieldError fieldNameA fieldNameB)

  -- | This fails with an error, because otherwise the @epPath@ is invalid
  epFieldTAssertHere :: TOpq -> EpPath -> ErrM TOpq -> ErrM TOpq
  epFieldTAssertHere _ _ (Left xs) = Left xs
  epFieldTAssertHere _ Here (Right xs) = Right xs
  epFieldTAssertHere t ((:*) xs ys) (Right _) = Left (epFieldTAssertHereError t ((:*) xs ys))
  epFieldTAssertHere t ((:+) xs ys) (Right _) = Left (epFieldTAssertHereError t ((:+) xs ys))

  epFieldTEntrypointEq :: forall t. TAlg -> SymAnn t -> EpPath -> Symbol -> Symbol -> Symbol -> Bool -> ErrM TOpq
  epFieldTEntrypointEq t ann epPath fieldName _ _ True = epFieldT t ann epPath fieldName
  epFieldTEntrypointEq _ ann epPath fieldName entrypointNameA entrypointNameB False =
    Left (epFieldTEntrypointError ann epPath fieldName entrypointNameA entrypointNameB)

  epFieldTResolveOr :: forall ta tb. (TAlg, TAlg) -> Symbol -> Symbol -> SymAnn ta -> SymAnn tb -> EpPath -> Symbol -> ErrM TOpq
  epFieldTResolveOr (ta, tb) aa ab as bs ((:+) entrypointName epPath) fieldName =
    epFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa == entrypointName) <+>
    epFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab == entrypointName)
  epFieldTResolveOr _ aa ab as bs ((:*) xs ys) fieldName = Left (epFieldTResolveOrError aa ab as bs ((:*) xs ys) fieldName)
  epFieldTResolveOr _ aa ab as bs Here fieldName = Left (epFieldTResolveOrError aa ab as bs Here fieldName)

  epFieldTResolvePair :: forall ta tb. TAlg -> TAlg -> SymAnn ta -> SymAnn tb -> EpPath -> Symbol -> ErrM TOpq
  epFieldTResolvePair ta tb as bs ((:*) epPathA epPathB) fieldName =
    epFieldT ta as epPathA fieldName <+>
    epFieldT tb bs epPathB fieldName
  epFieldTResolvePair _ _ as bs ((:+) xs ys) fieldName = Left (epFieldTResolvePairError as bs ((:+) xs ys) fieldName)
  epFieldTResolvePair _ _ as bs Here fieldName = Left (epFieldTResolvePairError as bs Here fieldName)

  epFieldT :: forall t. TAlg -> SymAnn t -> EpPath -> Symbol -> ErrM TOpq
  epFieldT (TOr ta tb) (ATOr _ aa ab as bs) epPath fieldName = epFieldTResolveOr (ta, tb) aa ab as bs epPath fieldName
  epFieldT (TPair ta tb) (ATPair _ _ _ as bs) epPath fieldName = epFieldTResolvePair ta tb as bs epPath fieldName
  epFieldT (TOpq t1) (ATOpq ta) epPath tb = epFieldTAssertHere t1 epPath (epFieldTFieldEq t1 (tOpqTypeAnn ta) tb (tOpqTypeAnn ta == tb))

  |])


data EpFieldTSym (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) -- = MkEpFieldTSym (Sing t, Sing ann, Sing epPath, Sing "EpFieldTSym")

type instance MapOut (EpFieldTSym _t _ann _epPath) (_xs :: Type) = ErrM TOpq


  ---- | Either we have the expected field and return the input or fail with `EpFieldTFieldError`
  --epFieldTFieldEq :: TOpq -> Symbol -> Symbol -> Bool -> ErrM TOpq
  --epFieldTFieldEq t _ _ True = Right t
  --epFieldTFieldEq _ fieldNameA fieldNameB False = Left (epFieldTFieldError fieldNameA fieldNameB)
--
  ---- | This fails with an error, because otherwise the @epPath@ is invalid
  ----
  ---- Note: the last two cases can be joined at the value-level,
  ---- but at the type-level, GHC needs explicit cases to
  ---- resolve type family instances
  --epFieldTAssertHere :: Symbol -> EpPath -> ErrM TOpq -> ErrM TOpq
  --epFieldTAssertHere _ _ (Left xs) = Left xs
  --epFieldTAssertHere _ Here (Right xs) = Right xs
  --epFieldTAssertHere t ((:+) xs ys) (Right _) = Left (epFieldTAssertHereError t ((:+) xs ys))
  --epFieldTAssertHere t ((:*) xs ys) (Right _) = Left (epFieldTAssertHereError t ((:*) xs ys))
--
  --epFieldTEntrypointEq :: forall t. SymAnn t -> EpPath -> Symbol -> Symbol -> Symbol -> Bool -> ErrM TOpq
  --epFieldTEntrypointEq ann epPath fieldName _ _ True = epFieldT ann epPath fieldName
  --epFieldTEntrypointEq ann epPath fieldName entrypointNameA entrypointNameB False =
  --  Left (epFieldTEntrypointError ann epPath fieldName entrypointNameA entrypointNameB)
--
  --epFieldTResolveOr :: forall ta tb. Symbol -> Symbol -> SymAnn ta -> SymAnn tb -> EpPath -> Symbol -> ErrM TOpq
  --epFieldTResolveOr aa ab as bs ((:+) entrypointName epPath) fieldName =
  --  epFieldTEntrypointEq as epPath fieldName aa entrypointName (aa == entrypointName) <+>
  --  epFieldTEntrypointEq bs epPath fieldName ab entrypointName (ab == entrypointName)
  --epFieldTResolveOr aa ab as bs ((:*) xs ys) fieldName =
  --  Left (epFieldTResolveOrError aa ab as bs ((:*) xs ys) fieldName)
  --epFieldTResolveOr aa ab as bs Here fieldName =
  --  Left (epFieldTResolveOrError aa ab as bs Here fieldName)
--
  --epFieldTResolvePair :: forall ta tb. SymAnn ta -> SymAnn tb -> EpPath -> Symbol -> ErrM TOpq
  --epFieldTResolvePair as bs ((:*) epPathA epPathB) fieldName =
  --  epFieldT as epPathA fieldName <+>
  --  epFieldT bs epPathB fieldName
  --epFieldTResolvePair as bs ((:+) xs ys) fieldName =
  --  Left (epFieldTResolvePairError as bs ((:+) xs ys) fieldName)
  --epFieldTResolvePair as bs Here fieldName =
  --  Left (epFieldTResolvePairError as bs Here fieldName)
--
  --epFieldT :: forall t. TOpq -> SymAnn t -> EpPath -> Symbol -> ErrM TOpq
  ---- epFieldT _ _ _ = Left ""
  --epFieldT (ATOr _ aa ab as bs) epPath fieldName = epFieldTResolveOr aa ab as bs epPath fieldName
  --epFieldT (ATPair _ _ _ as bs) epPath fieldName = epFieldTResolvePair as bs epPath fieldName
  --epFieldT (ATOpq ta) epPath tb = epFieldTAssertHere (tOpqTypeAnn ta) epPath (epFieldTFieldEq @t (tOpqTypeAnn ta) tb (tOpqTypeAnn ta == tb))
--
--
--
-- -- | Either we have the expected field and return the input or fail with `EpFieldTFieldError`
-- type family EpFieldTFieldEq (t :: TOpq) (fieldNameA :: Symbol) (fieldNameB :: Symbol) (eqAB :: Bool) :: ErrM TOpq where
--   EpFieldTFieldEq t _ _ 'True = 'Right t
--   EpFieldTFieldEq t fieldNameA fieldNameB 'False = 'Left (EpFieldTFieldError t fieldNameA fieldNameB)
--
-- singEpFieldTFieldEq :: forall t fieldNameA fieldNameB eqAB. ()
--   => Sing t
--   -> Sing fieldNameA
--   -> Sing fieldNameB
--   -> Sing eqAB
--   -> Sing (EpFieldTFieldEq t fieldNameA fieldNameB eqAB)
-- singEpFieldTFieldEq st _ _ STrue = SRight st
-- singEpFieldTFieldEq st sfieldNameA sfieldNameB SFalse = SLeft $
--   sEpFieldTFieldError st sfieldNameA sfieldNameB
--
-- -- | This fails with an error, because otherwise the @epPath@ is invalid
-- type family EpFieldTAssertHere (t :: TOpq) (epPath :: EpPath) (xs :: ErrM TOpq) :: ErrM TOpq where
--   EpFieldTAssertHere _ _ ('Left xs) = 'Left xs
--   EpFieldTAssertHere _ 'Here ('Right xs) = 'Right xs
--   EpFieldTAssertHere t nonHere _ =
--     'Left (EpFieldTAssertHereError t nonHere)
--
-- singEpFieldTAssertHere :: Sing t -> Sing epPath -> Sing xs -> Sing (EpFieldTAssertHere t epPath xs)
-- singEpFieldTAssertHere st sepPath sxs =
--   case sxs of
--     SLeft sys -> SLeft sys
--     SRight sys ->
--       case sepPath of
--         SHere -> SRight sys
--         ((:%*) _ _) -> SLeft $ sEpFieldTAssertHereError singITOpq st sepPath
--         ((:%+) _ _) -> SLeft $ sEpFieldTAssertHereError singITOpq st sepPath
--
-- type family EpFieldTEntrypointEq (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol) (entrypointNameA :: Symbol) (entrypointNameB :: Symbol) (eqAB :: Bool) :: ErrM TOpq where
--   EpFieldTEntrypointEq t ann epPath fieldName _ _ 'True = EpFieldT t ann epPath fieldName
--   EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB 'False =
--     'Left (EpFieldTEntrypointError t ann epPath fieldName entrypointNameA entrypointNameB)
--
-- singEpFieldTEntrypointEq :: forall t ann epPath fieldName entrypointNameA entrypointNameB eqAB. ()
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing fieldName
--   -> Sing entrypointNameA
--   -> Sing entrypointNameB
--   -> Sing eqAB
--   -> Sing (EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB eqAB)
-- singEpFieldTEntrypointEq t ann' epPath fieldName _ _ STrue =
--   singEpFieldT t ann' epPath fieldName
-- singEpFieldTEntrypointEq t sann epPath fieldName entrypointNameA entrypointNameB SFalse = SLeft $
--   singEpFieldTEntrypointError t sann epPath fieldName entrypointNameA entrypointNameB
--
-- type family EpFieldTResolveOr (ta :: TAlg) (tb :: TAlg) (aa :: Symbol) (ab :: Symbol) (as :: SymAnn ta) (bs :: SymAnn tb) (epPath :: EpPath) (fieldName :: Symbol) :: ErrM TOpq where
--   EpFieldTResolveOr ta tb aa ab as bs ('(:+) entrypointName epPath) fieldName =
--     EpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa == entrypointName) <+>
--     EpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab == entrypointName)
--   EpFieldTResolveOr _ _ aa ab as bs nonOrEpPath fieldName =
--     'Left (EpFieldTResolveOrError aa ab as bs nonOrEpPath fieldName)
--
-- singEpFieldTResolveOr :: forall ta tb aa ab as bs epPath fieldName. ()
--   => Sing ta
--   -> Sing tb
--   -> Sing aa
--   -> Sing ab
--   -> Sing as
--   -> Sing bs
--   -> Sing epPath
--   -> Sing fieldName
--   -> Sing (EpFieldTResolveOr ta tb aa ab as bs epPath fieldName)
-- singEpFieldTResolveOr ta tb aa ab as bs ((:%+) entrypointName epPath) fieldName =
--   singEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName) `singEitherAppendErrM`
--   singEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName)
-- singEpFieldTResolveOr _ _ aa ab sas sbs sepPath@((:%*) _ _) sfieldName =
--   SLeft $ singEpFieldTResolveOrError aa ab sas sbs sepPath sfieldName
-- singEpFieldTResolveOr _ _ aa ab sas sbs sepPath@SHere sfieldName =
--   SLeft $ singEpFieldTResolveOrError aa ab sas sbs sepPath sfieldName
--
-- type family EpFieldTResolvePair (ta :: TAlg) (tb :: TAlg) (as :: SymAnn ta) (bs :: SymAnn tb) (epPath :: EpPath) (fieldName :: Symbol) :: ErrM TOpq where
--   EpFieldTResolvePair ta tb as bs ('(:*) epPathA epPathB) fieldName =
--     EpFieldT ta as epPathA fieldName <+>
--     EpFieldT tb bs epPathB fieldName
--   EpFieldTResolvePair _ _ as bs nonOrEpPath fieldName =
--     'Left (EpFieldTResolvePairError as bs nonOrEpPath fieldName)
--
-- singEpFieldTResolvePair :: forall ta tb as bs epPath fieldName. ()
--   => Sing ta
--   -> Sing tb
--   -> Sing as
--   -> Sing bs
--   -> Sing epPath
--   -> Sing fieldName
--   -> Sing (EpFieldTResolvePair ta tb as bs epPath fieldName)
-- singEpFieldTResolvePair sta stb sas sbs ((:%*) sepPathA sepPathB) sfieldName =
--   singEitherAppendErrM
--     (singEpFieldT sta sas sepPathA sfieldName)
--     (singEpFieldT stb sbs sepPathB sfieldName)
-- singEpFieldTResolvePair _ _ sas sbs sepPath@((:%+) _ _) sfieldName =
--   SLeft $ singEpFieldTResolvePairError (singIAnnotatedAlg singISymbol) (singIAnnotatedAlg singISymbol) sas sbs sepPath sfieldName
-- singEpFieldTResolvePair _ _ sas sbs sepPath@SHere sfieldName =
--   SLeft $ singEpFieldTResolvePairError (singIAnnotatedAlg singISymbol) (singIAnnotatedAlg singISymbol) sas sbs sepPath sfieldName
--
-- type family EpFieldT (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol) :: ErrM TOpq where
--   EpFieldT ('TOr ta tb) ('ATOr _ aa ab as bs) epPath fieldName = EpFieldTResolveOr ta tb aa ab as bs epPath fieldName
--   EpFieldT ('TPair ta tb) ('ATPair _ _ _ as bs) epPath fieldName = EpFieldTResolvePair ta tb as bs epPath fieldName
--   EpFieldT ('TOpq t1) ('ATOpq ta) epPath tb = EpFieldTAssertHere t1 epPath (EpFieldTFieldEq t1 (TOpqTypeAnn ta) tb (TOpqTypeAnn ta == tb))
--
-- singEpFieldT :: ()
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing fieldName
--   -> Sing (EpFieldT t ann epPath fieldName)
-- singEpFieldT (STOr ta tb) (SATOr _ aa ab as bs) epPath fieldName = singEpFieldTResolveOr ta tb aa ab as bs epPath fieldName
-- singEpFieldT (STPair ta tb) (SATPair _ _ _ as bs) epPath fieldName = singEpFieldTResolvePair ta tb as bs epPath fieldName
-- singEpFieldT (STOpq t1) (SATOpq ta) epPath tb =
--   singEpFieldTAssertHere t1 epPath (singEpFieldTFieldEq t1 (singTOpqTypeAnn ta) tb (singTOpqTypeAnn ta %== tb))
--
-- return []


