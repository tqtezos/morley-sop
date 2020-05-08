-- {-# LANGUAGE NoTemplateHaskell #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing where

{-
import Control.Monad
import Data.Kind
import Data.Either
-- import Data.Maybe
import Data.String
-- import Data.Type.Equality
import Prelude hiding (unwords, show)
import GHC.TypeLits (sameSymbol)
import GHC.Generics ((:.:)(..))
import Text.Show
import Unsafe.Coerce

import Lorentz (Value)
import Michelson.Typed.Annotation
import Michelson.Untyped.Annotation
import Michelson.Typed.T
import Michelson.Typed.Value
import Michelson.Typed.Sing
import Michelson.TypeCheck.Helpers
import Lorentz.EntryPoints.Core

import Data.Either.Run
import Data.Either.Run.ErrorMessage

import Michelson.Typed.Annotation.Sing
import Michelson.Typed.Annotation.Path
import Michelson.Typed.Annotation.Sing.Scope
import Michelson.Typed.T.Sing
import Michelson.Typed.Value.Transformer
import Michelson.Typed.EntryPoints.Error

import Michelson.Typed.EntryPoints.Sing.Alg


import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.TypeError
import Data.Singletons.Prelude.Either
import Data.Singletons.Prelude.Eq -- (type (==))
import Data.Singletons.Prelude.Bool
import Data.Constraint

-- import Data.BTree

tt :: ()
tt = _

type SymAnn = Annotated Symbol

-- | Either we have the expected field and return the input or fail with `EpFieldTFieldError`
type family EpFieldTFieldEq (t :: T) (fieldNameA :: Symbol) (fieldNameB :: Symbol) (eqAB :: Bool) = (r :: ErrM T) | r -> t eqAB where
  EpFieldTFieldEq t fieldNameA fieldNameB 'True = 'Right t
  EpFieldTFieldEq t fieldNameA fieldNameB 'False = -- 'Left (EpFieldTFieldError t fieldNameA fieldNameB)
    'Left (
      'Text "EpFieldT " ':<>:
      'ShowType t ':<>:
      'Text " expected " ':<>:
      'Text fieldNameA ':<>:
      'Text " but got " ':<>:
      'Text fieldNameB
      )

singEpFieldTFieldEq :: forall t fieldNameA fieldNameB eqAB. ()
  => Sing t
  -> Sing fieldNameA
  -> Sing fieldNameB
  -> Sing eqAB
  -> Sing (EpFieldTFieldEq t fieldNameA fieldNameB eqAB)
singEpFieldTFieldEq st sfieldNameA sfieldNameB STrue = SRight st
singEpFieldTFieldEq st sfieldNameA sfieldNameB SFalse = SLeft $
  sing :%<>:
  SShowType st :%<>:
  sing :%<>:
  SText sfieldNameA :%<>:
  sing :%<>:
  SText sfieldNameB

getEpFieldTFieldEq :: forall t fieldNameA fieldNameB eqAB. ()
  => Sing t
  -> Sing fieldNameA
  -> Sing fieldNameB
  -> Sing eqAB
  -> (Maybe :.: Value) t
  -> RunEither SingError (Maybe :.: Value) (EpFieldTFieldEq t fieldNameA fieldNameB eqAB)
getEpFieldTFieldEq st sfieldNameA sfieldNameB STrue xs = RunRight xs
getEpFieldTFieldEq st sfieldNameA sfieldNameB SFalse xs = RunLeft $ SingError $
  sing :%<>:
  SShowType st :%<>:
  sing :%<>:
  SText sfieldNameA :%<>:
  sing :%<>:
  SText sfieldNameB

-- | This fails with an error, because otherwise the @epPath@ is invalid
type family EpFieldTAssertHere (t :: T) (epPath :: EpPath) (xs :: ErrM T) :: ErrM T where
  EpFieldTAssertHere _ _ ('Left xs) = 'Left xs
  EpFieldTAssertHere _ 'Here ('Right xs) = 'Right xs
  EpFieldTAssertHere t nonHere _ =
    'Left (EpFieldTAssertHereError t nonHere)

singEpFieldTAssertHere :: Sing t -> Sing epPath -> Sing xs -> Sing (EpFieldTAssertHere t epPath xs)
singEpFieldTAssertHere st sepPath sxs =
  case sxs of
    SLeft sys -> SLeft sys
    SRight sys ->
      case sepPath of
        SHere -> SRight sys
        ((:**) _ _) -> SLeft $ singEpFieldTAssertHereError singIT st sepPath
        ((:++) _ _) -> SLeft $ singEpFieldTAssertHereError singIT st sepPath

getEpFieldTAssertHere :: ()
  => Sing t
  -> Sing epPath
  -> Sing xs
  -> RunEither SingError (Maybe :.: Value) xs
  -> RunEither SingError (Maybe :.: Value) (EpFieldTAssertHere t epPath xs)
getEpFieldTAssertHere st sepPath sxs xss =
  case sxs of
    SLeft sys -> RunLeft $ SingError sys
    SRight sys ->
      case sepPath of
        SHere -> xss
        ((:**) _ _) -> RunLeft $ SingError $ singEpFieldTAssertHereError singIT st sepPath
        ((:++) _ _) -> RunLeft $ SingError $ singEpFieldTAssertHereError singIT st sepPath


type family EpFieldTEntrypointEq (t :: T) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol) (entrypointNameA :: Symbol) (entrypointNameB :: Symbol) (eqAB :: Bool) :: ErrM T where
  EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB 'True = EpFieldT t ann epPath fieldName
  EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB 'False =
    'Left (EpFieldTEntrypointError t ann epPath fieldName entrypointNameA entrypointNameB)

singEpFieldTEntrypointEq :: forall t ann epPath fieldName entrypointNameA entrypointNameB eqAB. ()
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> Sing entrypointNameA
  -> Sing entrypointNameB
  -> Sing eqAB
  -> Sing (EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB eqAB)
singEpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB STrue =
  singEpFieldT t ann epPath fieldName
singEpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB SFalse = SLeft $
  singEpFieldTEntrypointError t ann epPath fieldName entrypointNameA entrypointNameB

getEpFieldTEntrypointEq :: forall t ann epPath fieldName entrypointNameA entrypointNameB eqAB. ()
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> Sing entrypointNameA
  -> Sing entrypointNameB
  -> Sing eqAB
  -> (Maybe :.: Value) t
  -> RunEither SingError (Maybe :.: Value) (EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB eqAB)
getEpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB STrue xs =
  getEpFieldT t ann epPath fieldName xs
getEpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB SFalse _ = RunLeft $ SingError $
  singEpFieldTEntrypointError t ann epPath fieldName entrypointNameA entrypointNameB

type family EpFieldTResolveOr (ta :: T) (tb :: T) (aa :: Symbol) (ab :: Symbol) (as :: SymAnn ta) (bs :: SymAnn tb) (epPath :: EpPath) (fieldName :: Symbol) :: ErrM T where
  EpFieldTResolveOr ta tb aa ab as bs ('(:+) entrypointName epPath) fieldName =
    EpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa == entrypointName) <+>
      EpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab == entrypointName)
  EpFieldTResolveOr _ _ aa ab as bs nonOrEpPath fieldName =
    'Left (EpFieldTResolveOrError aa ab as bs nonOrEpPath fieldName)

singEpFieldTResolveOr :: forall ta tb aa ab as bs epPath fieldName. ()
  => Sing ta
  -> Sing tb
  -> Sing aa
  -> Sing ab
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> Sing (EpFieldTResolveOr ta tb aa ab as bs epPath fieldName)
singEpFieldTResolveOr ta tb aa ab as bs ((:++) entrypointName epPath) fieldName =
  singEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName) `singEitherAppendErrM`
  singEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName)

getEpFieldTResolveOr :: forall ta tb aa ab as bs epPath fieldName. ()
  => Sing ta
  -> Sing tb
  -> Sing aa
  -> Sing ab
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> Either ((Maybe :.: Value) ta) ((Maybe :.: Value) tb)
  -> RunEither SingError (Maybe :.: Value) (EpFieldTResolveOr ta tb aa ab as bs epPath fieldName)
getEpFieldTResolveOr ta tb aa ab as bs ((:++) entrypointName epPath) fieldName xs =
  runEitherAppendErrM
    (singEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName))
    (singEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName))
    (getEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName) (either id (const $ Comp1 Nothing) xs))
    (getEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName) (either (const $ Comp1 Nothing) id xs))
getEpFieldTResolveOr sta stb saa sab sas sbs sepPath@((:**) _ _) sfieldName xss =
  RunLeft $ SingError $ singEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName
getEpFieldTResolveOr sta stb saa sab sas sbs sepPath@SHere sfieldName xss =
  RunLeft $ SingError $ singEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName

type family EpFieldTResolvePair (ta :: T) (tb :: T) (as :: SymAnn ta) (bs :: SymAnn tb) (epPath :: EpPath) (fieldName :: Symbol) :: ErrM T where
  EpFieldTResolvePair ta tb as bs ('(:*) epPathA epPathB) fieldName =
    EpFieldT ta as epPathA fieldName <+>
    EpFieldT tb bs epPathB fieldName
  EpFieldTResolvePair _ _ as bs nonOrEpPath fieldName =
    'Left (EpFieldTResolvePairError as bs nonOrEpPath fieldName)

singEpFieldTResolvePair :: forall ta tb as bs epPath fieldName. ()
  => Sing ta
  -> Sing tb
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> Sing (EpFieldTResolvePair ta tb as bs epPath fieldName)
singEpFieldTResolvePair sta stb sas sbs ((:**) sepPathA sepPathB) sfieldName =
  singEitherAppendErrM
    (singEpFieldT sta sas sepPathA sfieldName)
    (singEpFieldT stb sbs sepPathB sfieldName)
singEpFieldTResolvePair sta stb sas sbs sepPath@((:++) _ _) sfieldName =
  SLeft $ singEpFieldTResolvePairError (singIAnnotated singISymbol) (singIAnnotated singISymbol) sas sbs sepPath sfieldName
singEpFieldTResolvePair sta stb sas sbs sepPath@SHere sfieldName =
  SLeft $ singEpFieldTResolvePairError (singIAnnotated singISymbol) (singIAnnotated singISymbol) sas sbs sepPath sfieldName

getEpFieldTResolvePair :: forall ta tb as bs epPath fieldName. ()
  => Sing ta
  -> Sing tb
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> (Maybe :.: Value) ta
  -> (Maybe :.: Value) tb
  -> RunEither SingError (Maybe :.: Value) (EpFieldTResolvePair ta tb as bs epPath fieldName)
getEpFieldTResolvePair sta stb sas sbs ((:**) sepPathA sepPathB) sfieldName xs ys =
  runEitherAppendErrM
    (singEpFieldT sta sas sepPathA sfieldName)
    (singEpFieldT stb sbs sepPathB sfieldName)
    (getEpFieldT sta sas sepPathA sfieldName xs)
    (getEpFieldT stb sbs sepPathB sfieldName ys)
getEpFieldTResolvePair sta stb sas sbs sepPath@((:++) _ _) sfieldName xs ys =
  RunLeft $ SingError $ singEpFieldTResolvePairError (singIAnnotated singISymbol) (singIAnnotated singISymbol) sas sbs sepPath sfieldName
getEpFieldTResolvePair sta stb sas sbs sepPath@SHere sfieldName xs ys =
  RunLeft $ SingError $ singEpFieldTResolvePairError (singIAnnotated singISymbol) (singIAnnotated singISymbol) sas sbs sepPath sfieldName


type family EpFieldT (t :: T) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol) :: ErrM T where
  EpFieldT ('Tc t1) ('ATc ta) epPath tb = EpFieldTAssertHere ('Tc t1) epPath (EpFieldTFieldEq ('Tc t1) ta tb (ta == tb))
  EpFieldT 'TKey ('ATKey ta) epPath tb = EpFieldTAssertHere 'TKey epPath (EpFieldTFieldEq 'TKey ta tb (ta == tb))
  EpFieldT 'TUnit ('ATUnit ta) epPath tb = EpFieldTAssertHere 'TUnit epPath (EpFieldTFieldEq 'TUnit ta tb (ta == tb))
  EpFieldT 'TSignature ('ATSignature ta) epPath tb = EpFieldTAssertHere 'TSignature epPath (EpFieldTFieldEq 'TSignature ta tb (ta == tb))
  EpFieldT 'TChainId ('ATChainId ta) epPath tb = EpFieldTAssertHere 'TChainId epPath (EpFieldTFieldEq 'TChainId ta tb (ta == tb))
  EpFieldT ('TOption t1) ('ATOption ta _) epPath tb = EpFieldTAssertHere ('TOption t1) epPath (EpFieldTFieldEq ('TOption t1) ta tb (ta == tb))
  EpFieldT ('TList t1) ('ATList ta _) epPath tb = EpFieldTAssertHere ('TList t1) epPath (EpFieldTFieldEq ('TList t1) ta tb (ta == tb))
  EpFieldT ('TSet t1) ('ATSet ta _) epPath tb = EpFieldTAssertHere ('TSet t1) epPath (EpFieldTFieldEq ('TSet t1) ta tb (ta == tb))
  EpFieldT 'TOperation ('ATOperation ta) epPath tb = EpFieldTAssertHere 'TOperation epPath (EpFieldTFieldEq 'TOperation ta tb (ta == tb))
  EpFieldT ('TContract t1) ('ATContract ta _) epPath tb = EpFieldTAssertHere ('TContract t1) epPath (EpFieldTFieldEq ('TContract t1) ta tb (ta == tb))
  EpFieldT ('TLambda t1 t2) ('ATLambda ta _ _) epPath tb = EpFieldTAssertHere ('TLambda t1 t2) epPath (EpFieldTFieldEq ('TLambda t1 t2) ta tb (ta == tb))
  EpFieldT ('TMap t1 t2) ('ATMap ta _ _) epPath tb = EpFieldTAssertHere ('TMap t1 t2) epPath (EpFieldTFieldEq ('TMap t1 t2) ta tb (ta == tb))
  EpFieldT ('TBigMap t1 t2) ('ATBigMap ta _ _) epPath tb = EpFieldTAssertHere ('TBigMap t1 t2) epPath (EpFieldTFieldEq ('TBigMap t1 t2) ta tb (ta == tb))

  EpFieldT ('TOr ta tb) ('ATOr _ aa ab as bs) epPath fieldName = EpFieldTResolveOr ta tb aa ab as bs epPath fieldName
  EpFieldT ('TPair ta tb) ('ATPair _ _ _ as bs) epPath fieldName = EpFieldTResolvePair ta tb as bs epPath fieldName

singEpFieldT :: ()
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> Sing (EpFieldT t ann epPath fieldName)
singEpFieldT (STc t1) (SATc ta) epPath tb = singEpFieldTAssertHere (STc t1) epPath (singEpFieldTFieldEq (STc t1) ta tb (ta %== tb))
singEpFieldT STKey (SATKey ta) epPath tb = singEpFieldTAssertHere STKey epPath (singEpFieldTFieldEq STKey ta tb (ta %== tb))
singEpFieldT STUnit (SATUnit ta) epPath tb = singEpFieldTAssertHere STUnit epPath (singEpFieldTFieldEq STUnit ta tb (ta %== tb))
singEpFieldT STSignature (SATSignature ta) epPath tb = singEpFieldTAssertHere STSignature epPath (singEpFieldTFieldEq STSignature ta tb (ta %== tb))
singEpFieldT STChainId (SATChainId ta) epPath tb = singEpFieldTAssertHere STChainId epPath (singEpFieldTFieldEq STChainId ta tb (ta %== tb))
singEpFieldT (STOption t1) (SATOption ta _) epPath tb = singEpFieldTAssertHere (STOption t1) epPath (singEpFieldTFieldEq (STOption t1) ta tb (ta %== tb))
singEpFieldT (STList t1) (SATList ta _) epPath tb = singEpFieldTAssertHere (STList t1) epPath (singEpFieldTFieldEq (STList t1) ta tb (ta %== tb))
singEpFieldT (STSet t1) (SATSet ta _) epPath tb = singEpFieldTAssertHere (STSet t1) epPath (singEpFieldTFieldEq (STSet t1) ta tb (ta %== tb))
singEpFieldT STOperation (SATOperation ta) epPath tb = singEpFieldTAssertHere STOperation epPath (singEpFieldTFieldEq STOperation ta tb (ta %== tb))
singEpFieldT (STContract t1) (SATContract ta _) epPath tb = singEpFieldTAssertHere (STContract t1) epPath (singEpFieldTFieldEq (STContract t1) ta tb (ta %== tb))
singEpFieldT (STLambda t1 t2) (SATLambda ta _ _) epPath tb = singEpFieldTAssertHere (STLambda t1 t2) epPath (singEpFieldTFieldEq (STLambda t1 t2) ta tb (ta %== tb))
singEpFieldT (STMap t1 t2) (SATMap ta _ _) epPath tb = singEpFieldTAssertHere (STMap t1 t2) epPath (singEpFieldTFieldEq (STMap t1 t2) ta tb (ta %== tb))
singEpFieldT (STBigMap t1 t2) (SATBigMap ta _ _) epPath tb = singEpFieldTAssertHere (STBigMap t1 t2) epPath (singEpFieldTFieldEq (STBigMap t1 t2) ta tb (ta %== tb))
singEpFieldT (STOr ta tb) (SATOr _ aa ab as bs) epPath fieldName = singEpFieldTResolveOr ta tb aa ab as bs epPath fieldName
singEpFieldT (STPair ta tb) (SATPair _ _ _ as bs) epPath fieldName = singEpFieldTResolvePair ta tb as bs epPath fieldName

getEpFieldT :: forall t (ann :: SymAnn t) epPath fieldName. ()
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> (Maybe :.: Value) t
  -> RunEither SingError (Maybe :.: Value) (EpFieldT t ann epPath fieldName)
getEpFieldT (STc t1) (SATc ta) epPath tb xs = getEpFieldTAssertHere (STc t1) epPath (singEpFieldTFieldEq (STc t1) ta tb (ta %== tb)) (getEpFieldTFieldEq (STc t1) ta tb (ta %== tb) xs)
getEpFieldT STKey (SATKey ta) epPath tb xs = getEpFieldTAssertHere STKey epPath (singEpFieldTFieldEq STKey ta tb (ta %== tb)) (getEpFieldTFieldEq STKey ta tb (ta %== tb) xs)
getEpFieldT STUnit (SATUnit ta) epPath tb xs = getEpFieldTAssertHere STUnit epPath (singEpFieldTFieldEq STUnit ta tb (ta %== tb)) (getEpFieldTFieldEq STUnit ta tb (ta %== tb) xs)
getEpFieldT STSignature (SATSignature ta) epPath tb xs = getEpFieldTAssertHere STSignature epPath (singEpFieldTFieldEq STSignature ta tb (ta %== tb)) (getEpFieldTFieldEq STSignature ta tb (ta %== tb) xs)
getEpFieldT STChainId (SATChainId ta) epPath tb xs = getEpFieldTAssertHere STChainId epPath (singEpFieldTFieldEq STChainId ta tb (ta %== tb)) (getEpFieldTFieldEq STChainId ta tb (ta %== tb) xs)
getEpFieldT (STOption t1) (SATOption ta _) epPath tb xs = getEpFieldTAssertHere (STOption t1) epPath (singEpFieldTFieldEq (STOption t1) ta tb (ta %== tb)) (getEpFieldTFieldEq (STOption t1) ta tb (ta %== tb) xs)
getEpFieldT (STList t1) (SATList ta _) epPath tb xs = getEpFieldTAssertHere (STList t1) epPath (singEpFieldTFieldEq (STList t1) ta tb (ta %== tb)) (getEpFieldTFieldEq (STList t1) ta tb (ta %== tb) xs)
getEpFieldT (STSet t1) (SATSet ta _) epPath tb xs = getEpFieldTAssertHere (STSet t1) epPath (singEpFieldTFieldEq (STSet t1) ta tb (ta %== tb)) (getEpFieldTFieldEq (STSet t1) ta tb (ta %== tb) xs)
getEpFieldT STOperation (SATOperation ta) epPath tb xs = getEpFieldTAssertHere STOperation epPath (singEpFieldTFieldEq STOperation ta tb (ta %== tb)) (getEpFieldTFieldEq STOperation ta tb (ta %== tb) xs)
getEpFieldT (STContract t1) (SATContract ta _) epPath tb xs = getEpFieldTAssertHere (STContract t1) epPath (singEpFieldTFieldEq (STContract t1) ta tb (ta %== tb)) (getEpFieldTFieldEq (STContract t1) ta tb (ta %== tb) xs)
getEpFieldT (STLambda t1 t2) (SATLambda ta _ _) epPath tb xs = getEpFieldTAssertHere (STLambda t1 t2) epPath (singEpFieldTFieldEq (STLambda t1 t2) ta tb (ta %== tb)) (getEpFieldTFieldEq (STLambda t1 t2) ta tb (ta %== tb) xs)
getEpFieldT (STMap t1 t2) (SATMap ta _ _) epPath tb xs = getEpFieldTAssertHere (STMap t1 t2) epPath (singEpFieldTFieldEq (STMap t1 t2) ta tb (ta %== tb)) (getEpFieldTFieldEq (STMap t1 t2) ta tb (ta %== tb) xs)
getEpFieldT (STBigMap t1 t2) (SATBigMap ta _ _) epPath tb xs = getEpFieldTAssertHere (STBigMap t1 t2) epPath (singEpFieldTFieldEq (STBigMap t1 t2) ta tb (ta %== tb)) (getEpFieldTFieldEq (STBigMap t1 t2) ta tb (ta %== tb) xs)
getEpFieldT (STOr ta tb) (SATOr _ aa ab as bs) epPath fieldName xs =
  case unComp1 xs of
    Nothing ->
      getEpFieldTResolveOr ta tb aa ab as bs epPath fieldName $ Right $ Comp1 Nothing -- `Right` picked arbitrarily
    Just (VOr xs') ->
      getEpFieldTResolveOr ta tb aa ab as bs epPath fieldName $ bimap (Comp1 . Just) (Comp1 . Just) xs'
getEpFieldT (STPair ta tb) (SATPair _ _ _ as bs) epPath fieldName xs =
  case unComp1 xs of
    Nothing ->
      getEpFieldTResolvePair ta tb as bs epPath fieldName (Comp1 Nothing) (Comp1 Nothing)
    Just (VPair (xs', ys')) ->
      getEpFieldTResolvePair ta tb as bs epPath fieldName (Comp1 $ Just xs') (Comp1 $ Just ys')

-- setEpFieldT :: forall f t (ann :: SymAnn t) epPath fieldName. Applicative f
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing fieldName
--   -> Value (EpFieldT t ann epPath fieldName)
--   -> ValueT f t
--   -> ValueT f t
-- setEpFieldT = _


-- data RF (f :: T -> Type) (t :: T) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol) where
--   RF :: RunEither EmbedTypeError f (EpFieldT t ann epPath fieldName) -> RF f t ann epPath fieldName

-- EpFieldNames (t :: T) (ann :: SymAnn t) (epPath :: EpPath) :: [Symbol] where
--   EpFieldNames t ('ATc ta) epPath = EpFieldNamesAssertHere t epPath '[ta]

--   EpFieldNames ('TOr ta tb) ('ATOr _ aa ab as bs) ((:+) entrypointName epPath) =
--     resolve aa == entrypointName <||> resolve ab == entrypointName

--   EpFieldNames ('TPair ta tb) ('ATPair _ _ _ as bs) ((:*) epPathA epPathB) =
--     EpFieldNames ta as epPathA ++ EpFieldNames tb bs epPathB


-- REp (f :: T -> Type) (t :: T) (ann :: SymAnn) (epPath :: EpPath) where
--   REp :: NP (RF t ann epPath) (EpFieldNames ann epPath) -> REp f t ann epPath

-- type SomeREp f t ann = SomeEmbed Sing (REp f t ann)

-- fromSomeREp :: SomeREp (Maybe :.: Value) t ann -> Maybe (Value t)

-- toSomeREp :: Sing ann -> Value t -> SomeREp Value t ann





















-- data Path a where
--   EmptyPath :: Path a
--   (:>) :: a -> Path a -> Path a
--   -- (:\) :: a -> Path a -> Path a
--   -- (:/) :: a -> Path a -> Path a
--   (:<) :: Path a -> Path a -> Path a
--
-- type EpPath = Path Symbol
--
-- RunEpPath (t :: T) (ann :: Annotated a t) (epPath :: EpPath) (fieldName :: a) :: T where
--   RunEpPath t (ATc ta) '[] ta = t
--   RunEpPath ('TOr a b) (ATOr _ aa ba as bs) (aa ':> epPath) = RunEpPath a as epPath
--   RunEpPath ('TOr a b) (ATOr _ aa ba as bs) (ba ':> epPath) = RunEpPath b bs epPath
--
--   RunEpPath ('TPair a b) ('ATPair _ aa ba as bs) (epPathA ':< epPathB) = _ (RunEpPath a as epPathA) (RunEpPath b bs epPathB)
--
-- Sing ann -> Sing epPath -> Sing fieldName -> Sing (RunEpPath ann epPath fieldName)
--
-- RF :: (NoOr a, NoPair a, SingI a, AnnotatedField (AnnotatedEP ann epPath) fieldName ~ a) => f a -> RF f t ann epPath fieldName
--
--
--
--
--
--
--
-- -- | Reduced field
-- data RF (f :: T -> Type) (t :: T) (ann :: Annotated Symbol t) (epPath :: EpPath) (fieldName :: Symbol) where
--   RF :: (NoOr t, NoPair t) => Sing t -> Sing ann -> f t -> RF f t ann 'EmptyPath (FieldAnnotation ann)
--
--   -- | Consume an element of the path
--   LeftRF  :: RF f a annA epPath fieldName -> RF f ('TOr a b) ('ATOr _ epNameA epNameB annA annB) (epNameA ':> epPath) fieldName
--
--   -- | Recurse without consuming an element of the path (since it's somewhere else in the tree of "Or"'s)
--   LeftRF' :: RF f a annA epPath fieldName -> RF f ('TOr a b) ('ATOr _ epNameA epNameB annA annB) epPath fieldName
--
--   RightRF :: RF f b annB epPath fieldName -> RF f ('TOr a b) ('ATOr _ epNameA epNameB annA annB) (epNameB ':> epPath) fieldName
--
--   (recurse, recurse) (:<)
--   (not recurse, recurse) (:>)
--   (recurse, not recurse) (:>)
--   (not recurse, not recurse) -> impossible
--
--   FstRecRF :: RF f a annA epPath fieldName -> RF f ('TPair a b) ('ATOr _ epNameA epNameB annA annB) epPath fieldName
--
--   -- | drop the other side of the path
--   FstRecRF' :: RF f a annA epPath fieldName -> RF f ('TPair a b) ('ATOr _ epNameA epNameB annA annB) (epPath :< _) fieldName
--   SndRF :: (NoOr b, NoPair b) => RF U1 a annA epPath fieldName -> f b -> RF f ('TPair a b) ('ATOr _ epNameA epNameB annA annB) epPath (FieldAnnotation annB)
--
  -- | Not-needed, we can just drop the other side of the path
  -- FstRecRF :: RF f a annA epPathA fieldName -> REP U1 b annB epPathB -> RF f ('TPair a b) ('ATOr _ epNameA epNameB annA annB) ((epNameA ':< epPathA) epNameB epPathB) fieldName
--
-- How this should work:
--
-- (or (A :a %1)
--     (pair %2
--       (B :b)
--       (or
--         (C :c %3)
--         (pair %4
--           (D :d)
--           (or
--             (E :e %5)
--             (F :f %6)
--           )
--         )
--       )
--     )
-- )
--
-- path    | fields
-- [1]     | [(a, A)]
-- [2,3]   | [(b, B), (c, C)]
-- [2,4,5] | [(b, B), (d, D), (e, E)]
-- [2,4,6] | [(b, B), (d, D), (f, F)]
--
-- -- | A Reduced EntryPoint
-- data REP (f :: T -> Type) (t :: T) (ann :: Annotated Symbol t) (epName :: Symbol) where
--   REP :: IsReduced t => Sing t -> Sing ann -> Sing name -> f t -> REP f t ann name
--   NotREPLeft :: REP f a annA name -> _ -> REP f ('TOr a b) ('ATOr _
--
-- -- | A Reduced EntryPoint field
-- data REPF (f :: T -> Type) (t :: T) (ann :: Annotated Symbol t) (epPath :: [Symbol]) (fieldName :: Symbol) where
--   -- | When not Or/Pair, we lookup the field annotation and have no remaining entrypoints in our path
--   REPF :: (OrLess t, PairLess t, FieldAnnotation t ann ~ fieldName) => f t -> REPF f t ann '[] fieldName
--
--   -- | Traversing a sum consumes an element of the entrypoint path list
--   LeftEPF :: REPF f a annA epPath fieldName -> REPF f ('TOr a b) ('ATOr annAB epNameA epNameB annA annB) (epName ': epPath) fieldName
--   RightEPF :: REPF f b annB epPath fieldName -> REPF f ('TOr a b) ('ATOr annAB epNameA epNameB annA annB) (epName ': epPath) fieldName
--
--   -- | When recursing into a product, we can either recurse into a REPF on one side or use a reduced non-path field
--   FstEPF :: REPF f a annA epPath fieldName -> REPF f ('TPair a b) ('ATPair annAB fieldNameA fieldNameB annA annB) epPath fieldName
--   -- | We want: a path into the fst element (but no field at the end, just need it to resolve) and a proof that the snd element is fully reduced
--   FstEPF' :: (NoOr b, NoPair b, HasPath a annA epPath) => f b -> REPF f ('TPair a b) ('ATPair annAB fieldNameA fieldNameB annA annB) epPath fieldNameA
--
--
--
--
-- Next, we want:
--
-- data REPF (f :: T -> Type) (t :: T) (ann :: Annotated Symbol t) (epPath :: [Symbol]) (fieldName :: Symbol) (field :: T) where
--
-- But we might be able to get away without it, since field should be uniquely determined from fieldName, ann, t
--
--
-- And then:
--
-- PathFields t ann epPath :: [(Symbol, T)]
--
-- REP :: NP (Uncurry (REPF f t ann epPath)) (PathFields t ann epPath) -> REP f t ann epPath
--
-- And then:
--
-- emptyREP :: Sing t -> Sing ann -> Sing epPath -> REP (Maybe :.: f) t ann epPath
--
-- insertREPF :: REPF f t ann epPath fieldName -> REP (Maybe :.: f) t ann epPath -> REP (Maybe :.: f) t ann epPath
--
-- runREP :: REP f t ann epPath -> f t
--
--
-- Finally, we can:
--   Resolve:
--     SingI t
--     SingI ann
--     epPath
--   from the input,
--   then calculate emptyREP,
--   resolve individual REPF's and insert them,
--   then runREP to get ((f :.: Maybe) t) and check that we added all of the fields.
--
-- -- AllPaths t ann :: [[Symbol]]
--
-- -- Sing t -> Sing ann ->

-}
