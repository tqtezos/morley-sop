{-# LANGUAGE TypeFamilyDependencies #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Types.TH where

import Prelude hiding (unwords, show)

import Control.AltError
import Control.AltError.TH
import Data.AltError
import Data.ListError
import Data.ListError.TH
import Data.Singletons.Map
import Data.Singletons.Map.TH
import Data.Singletons.Prelude.Monad.State
import Data.Singletons.Prelude.Monad.State.TH

import Michelson.Typed.Annotation.Path
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.Annotation.Sing.Alg.TH ()
import Michelson.Typed.EntryPoints.Error.TH
import Michelson.Typed.T.Alg

import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.IsString

type SymAnn = AnnotatedAlg Symbol

$(singletonsOnly [d|

  epFieldRecResolveOr :: forall s f r ta tb. (IsString s, Show s, Eq s, AltError [s] f, Show r) => (Maybe s, TOpq -> Maybe s -> f r) -> (TAlg, TAlg) -> (s, s) -> AnnotatedAlg s ta -> AnnotatedAlg s tb -> Path s -> Maybe s -> f r
  epFieldRecResolveOr (name, fs) (ta, tb) (aa, ab) as bs ((:+) entrypointName epPath) fieldAnn =
    (bool_
      (altErr [epFieldRecEntrypointError as epPath name aa entrypointName fieldAnn])
      (epFieldRec' name fs ta as epPath (Just aa))
      (aa == entrypointName)
    ) <||>
    (bool_
      (altErr [epFieldRecEntrypointError bs epPath name ab entrypointName fieldAnn])
      (epFieldRec' name fs tb bs epPath (Just ab))
      (ab == entrypointName)
    )
  epFieldRecResolveOr (name, _fs) _tab (aa, ab) as bs ((:*) xs ys) fieldAnn =
    altFail [epFieldRecResolveOrError aa ab as bs ((:*) xs ys) name fieldAnn]
  epFieldRecResolveOr (name, _fs) _tab (aa, ab) as bs Here fieldAnn =
    altFail [epFieldRecResolveOrError aa ab as bs Here name fieldAnn]

  epFieldRecResolvePair :: forall s f r ta tb. (IsString s, Show s, Eq s, AltError [s] f, Show r) => (Maybe s, TOpq -> Maybe s -> f r) -> (TAlg, TAlg) -> (s, s) -> AnnotatedAlg s ta -> AnnotatedAlg s tb -> Path s -> Maybe s -> f r
  epFieldRecResolvePair (name, fs) (ta, tb) (aa, ab) as bs ((:*) epPathA epPathB) _fieldAnn =
    epFieldRec' name fs ta as epPathA (Just aa) <||>
    epFieldRec' name fs tb bs epPathB (Just ab)
  epFieldRecResolvePair (name, _fs) (_ta, _tb) (_aa, _ab) as bs ((:+) xs ys) fieldAnn =
    altFail [epFieldRecResolvePairError as bs ((:+) xs ys) name fieldAnn]
  epFieldRecResolvePair (name, _fs) (_ta, _tb) (_aa, _ab) as bs Here fieldAnn =
    altFail [epFieldRecResolvePairError as bs Here name fieldAnn]

  epFieldRec' :: forall s f r t. (IsString s, Show s, Eq s, AltError [s] f, Show r) => Maybe s -> (TOpq -> Maybe s -> f r) -> TAlg -> AnnotatedAlg s t -> Path s -> Maybe s -> f r
  epFieldRec' name fs (TOr ta tb) (ATOr _ aa ab as bs) epPath fieldAnn = epFieldRecResolveOr (name, fs) (ta, tb) (aa, ab) as bs epPath fieldAnn
  epFieldRec' name fs (TPair ta tb) (ATPair _ aa ab as bs) epPath fieldAnn = epFieldRecResolvePair (name, fs) (ta, tb) (aa, ab) as bs epPath fieldAnn
  epFieldRec' _name fs (TOpq t1) (ATOpq _ta) epPath fieldAnn =
    bool_ (altFail [epFieldRecAssertHereError t1 epPath fieldAnn]) (pure ()) (epPath == Here) *>
    (fs t1 fieldAnn)

  epFieldRec :: forall s f r t. (IsString s, Show s, Eq s, AltError [s] f, Show r) => Maybe s -> (TOpq -> Maybe s -> f r) -> TAlg -> AnnotatedAlg s t -> Path s -> f r
  epFieldRec name fs t ann epPath = epFieldRec' name fs t ann epPath Nothing

  -- helper for epFieldT
  epFieldRecT :: forall s f. (IsString s, Show s, Eq s, AltError [s] f) => Maybe s -> TOpq -> Maybe s -> f TOpq
  epFieldRecT fieldName ta fieldNameA =
    bool_
      (altErr [epFieldTFieldError fieldName fieldNameA])
      (pure ta)
      (fieldName == fieldNameA)

  epFieldTResolveOr :: forall s f ta tb. (IsString s, Show s, Eq s, AltError [s] f) => (TAlg, TAlg) -> (s, s) -> AnnotatedAlg s ta -> AnnotatedAlg s tb -> Path s -> Maybe s -> Maybe s -> f TOpq
  epFieldTResolveOr (ta, tb) (aa, ab) as bs epPath fieldName = epFieldRecResolveOr (fieldName, epFieldRecT fieldName) (ta, tb) (aa, ab) as bs epPath

  epFieldTResolvePair :: forall s f ta tb. (IsString s, Show s, Eq s, AltError [s] f) => (TAlg, TAlg) -> (s, s) -> AnnotatedAlg s ta -> AnnotatedAlg s tb -> Path s -> Maybe s -> Maybe s -> f TOpq
  epFieldTResolvePair (ta, tb) (aa, ab) as bs epPath fieldName = epFieldRecResolvePair (fieldName, epFieldRecT fieldName) (ta, tb) (aa, ab) as bs epPath

  -- The type of an entrypoint field
  epFieldT' :: forall s f t. (IsString s, Show s, Eq s, AltError [s] f) => TAlg -> AnnotatedAlg s t -> Path s -> Maybe s -> Maybe s -> f TOpq
  epFieldT' t ann epPath fieldName = epFieldRec' fieldName (epFieldRecT fieldName) t ann epPath

  -- The type of an entrypoint field
  epFieldT :: forall s f t. (IsString s, Show s, Eq s, AltError [s] f) => TAlg -> AnnotatedAlg s t -> Path s -> Maybe s -> f TOpq
  epFieldT t ann epPath fieldName = epFieldRec fieldName (epFieldRecT fieldName) t ann epPath


  -- helper for epFields
  epFieldRecFields :: forall s f. AltError [s] f => TOpq -> Maybe s -> f (Maybe s)
  epFieldRecFields _t = pure

  -- All entrypoint field names
  epFieldNames :: forall s f t. (IsString s, Show s, Eq s, AltError [s] f) => TAlg -> AnnotatedAlg s t -> Path s -> f (Maybe s)
  epFieldNames = epFieldRec (Just (fromString "epFieldNames")) epFieldRecFields


  -- All entrypoint field names, in ErrM
  epFieldNamesErrM :: forall s t. (IsString s, Show s, Ord s) => TAlg -> AnnotatedAlg s t -> Path s -> AltE [s] [Maybe s]
  epFieldNamesErrM t ann epPath = sort <$> listEToAltE (epFieldNames t ann epPath)

  epFieldTs :: forall s t. (IsString s, Show s, Ord s) => TAlg -> AnnotatedAlg s t -> Path s -> AltE [s] [TOpq]
  epFieldTs t ann epPath =
    epFieldNamesErrM t ann epPath >>=
    traverse (epFieldT t ann epPath)


  -- All EpPath's, unsorted
  epPathsRaw :: forall s t. AnnotatedAlg s t -> [Path s]
  epPathsRaw (ATOr _ aa ab as bs) =
    ((:+) aa <$> epPathsRaw as) ++
    ((:+) ab <$> epPathsRaw bs)
  epPathsRaw (ATPair _ _ _ as bs) = liftA2 (:*) (epPathsRaw as) (epPathsRaw bs)
  epPathsRaw (ATOpq _ta) = [Here]

  -- All EpPath's, sorted
  epPaths :: forall s t. Ord s => AnnotatedAlg s t -> [Path s]
  epPaths ann = sort (epPathsRaw ann)

  traverseEpPaths :: forall a s t. (Path a -> a -> State' s a) -> AnnotatedAlg a t -> State' s (AnnotatedAlg a t)
  traverseEpPaths fs (ATOr ann aa ab as bs) =
    (fs Here aa >>>= \aa' ->
    (fs Here ab >>>= \ab' ->
    ATOr ann aa' ab' <$$>
    (traverseEpPaths (fs . (:+) aa') as) <<*>>
    (traverseEpPaths (fs . (:+) ab') bs)
    ))
  traverseEpPaths fs (ATPair ann aa ab as bs) =
    (fs Here aa >>>= \aa' ->
    (fs Here ab >>>= \ab' ->
    ATPair ann aa' ab' <$$>
    traverseEpPaths (fs . (:*) Here) as <<*>>
    traverseEpPaths (fs . (:*) Here) bs -- Here is used as a collection of all other paths
    ))
  traverseEpPaths _fs (ATOpq ta) = pureState' (ATOpq ta)

  -- count the number of occurrences of the path, then append "_n" if non-zero.
  -- skip non-pair-field empty annotations
  uniqifyEpPathsStepSimple :: forall s. (Ord s, IsString s, Semigroup s) => Path s -> s -> State' (ListMap s Nat) s
  uniqifyEpPathsStepSimple _epPath annotation =
    (lookupModifyListMap
      0
      (\numAtPath ->
        ( bool_
            (annotation <> "_" <> fromString (show_ numAtPath))
            annotation
            (numAtPath == 0)
        , numAtPath + 1
        )
      )
      annotation <$$>
      getState'
      ) >>>= \(annotation', pathsMap') ->
        putState' pathsMap' *>> pureState' annotation'

  uniqifyEpPathsSimpler :: forall s t. (Ord s, IsString s, Semigroup s) => AnnotatedAlg s t -> AnnotatedAlg s t
  uniqifyEpPathsSimpler ann =
    traverseEpPaths uniqifyEpPathsStepSimple ann `evalState'`
    emptyListMap

  |])


