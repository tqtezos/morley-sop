{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE TypeOperators #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Types where

import Prelude hiding (unwords, show, null)
import Data.List

import Control.AltError
import Data.AltError
import Data.ListError
import Data.Singletons.Map
import Data.Singletons.Prelude.Monad.State

import Michelson.Typed.Annotation.Path
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.Annotation.Sing.Alg.TH ()
import Michelson.Typed.EntryPoints.Error
import Michelson.Typed.T.Alg

import Data.Singletons.TypeLits
import Data.Singletons.Prelude

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

-- | `epFieldRec`, where we `Maybe` have an inherited `fieldAnn` (initially `Nothing`)
epFieldRec' :: forall s f r t. (IsString s, Show s, Eq s, AltError [s] f, Show r) => Maybe s -> (TOpq -> Maybe s -> f r) -> TAlg -> AnnotatedAlg s t -> Path s -> Maybe s -> f r
epFieldRec' name fs (TOr ta tb) (ATOr _ aa ab as bs) epPath fieldAnn = epFieldRecResolveOr (name, fs) (ta, tb) (aa, ab) as bs epPath fieldAnn
epFieldRec' name fs (TPair ta tb) (ATPair _ aa ab as bs) epPath fieldAnn = epFieldRecResolvePair (name, fs) (ta, tb) (aa, ab) as bs epPath fieldAnn
epFieldRec' _name fs (TOpq t1) (ATOpq _ta) epPath fieldAnn =
  bool_ (altFail [epFieldRecAssertHereError t1 epPath fieldAnn]) (pure ()) (epPath == Here) *>
  (fs t1 fieldAnn)
epFieldRec' _ _ _ _ _ _ = error "epFieldRec' annotation does not match type" -- impossible in type-level use case


-- | Traverse all EpField's with `AltError` effects
epFieldRec :: forall s f r t. (IsString s, Show s, Eq s, AltError [s] f, Show r) => Maybe s -> (TOpq -> Maybe s -> f r) -> TAlg -> AnnotatedAlg s t -> Path s -> f r
epFieldRec name fs t ann epPath = epFieldRec' name fs t ann epPath Nothing

-- Helper for epFieldT
epFieldRecT :: forall s f. (IsString s, Show s, Eq s, AltError [s] f) => Maybe s -> TOpq -> Maybe s -> f TOpq
epFieldRecT fieldName ta fieldNameA =
  bool_
    (altErr [epFieldTFieldError fieldName fieldNameA])
    (pure ta)
    (fieldName == fieldNameA)

-- | Specialized `epFieldRecResolveOr` for type applications
epFieldTResolveOr :: forall s f ta tb. (IsString s, Show s, Eq s, AltError [s] f) => (TAlg, TAlg) -> (s, s) -> AnnotatedAlg s ta -> AnnotatedAlg s tb -> Path s -> Maybe s -> Maybe s -> f TOpq
epFieldTResolveOr (ta, tb) (aa, ab) as bs epPath fieldName = epFieldRecResolveOr (fieldName, epFieldRecT fieldName) (ta, tb) (aa, ab) as bs epPath

-- | Specialized `epFieldRecResolvePair` for type applications
epFieldTResolvePair :: forall s f ta tb. (IsString s, Show s, Eq s, AltError [s] f) => (TAlg, TAlg) -> (s, s) -> AnnotatedAlg s ta -> AnnotatedAlg s tb -> Path s -> Maybe s -> Maybe s -> f TOpq
epFieldTResolvePair (ta, tb) (aa, ab) as bs epPath fieldName = epFieldRecResolvePair (fieldName, epFieldRecT fieldName) (ta, tb) (aa, ab) as bs epPath

-- | The type of an entrypoint field (helper)
epFieldT' :: forall s f t. (IsString s, Show s, Eq s, AltError [s] f) => TAlg -> AnnotatedAlg s t -> Path s -> Maybe s -> Maybe s -> f TOpq
epFieldT' t ann epPath fieldName = epFieldRec' fieldName (epFieldRecT fieldName) t ann epPath

-- | The type of an entrypoint field
epFieldT :: forall s f t. (IsString s, Show s, Eq s, AltError [s] f) => TAlg -> AnnotatedAlg s t -> Path s -> Maybe s -> f TOpq
epFieldT t ann epPath fieldName = epFieldRec fieldName (epFieldRecT fieldName) t ann epPath


-- | helper for epFields
epFieldRecFields :: forall s f. AltError [s] f => TOpq -> Maybe s -> f (Maybe s)
epFieldRecFields _t = pure

-- | All entrypoint field names
epFieldNames :: forall s f t. (IsString s, Show s, Eq s, AltError [s] f) => TAlg -> AnnotatedAlg s t -> Path s -> f (Maybe s)
epFieldNames = epFieldRec (Just (fromString "epFieldNames")) epFieldRecFields


-- | All entrypoint field names, in ErrM
epFieldNamesErrM :: forall s t. (IsString s, Show s, Ord s) => TAlg -> AnnotatedAlg s t -> Path s -> AltE [s] [Maybe s]
epFieldNamesErrM t ann epPath = sort <$> listEToAltE (epFieldNames t ann epPath)

-- | EpField types
epFieldTs :: forall s t. (IsString s, Show s, Ord s) => TAlg -> AnnotatedAlg s t -> Path s -> AltE [s] [TOpq]
epFieldTs t ann epPath =
  epFieldNamesErrM t ann epPath >>=
  traverse (epFieldT t ann epPath)


-- All EpPath's, unsorted
epPathsRaw :: forall s t. AnnotatedAlg s t -> [Path s]
epPathsRaw (ATOr _ aa ab as bs) =
  ((:+) aa <$> epPathsRaw as) <>
  ((:+) ab <$> epPathsRaw bs)
epPathsRaw (ATPair _ _ _ as bs) = liftA2 (:*) (epPathsRaw as) (epPathsRaw bs)
epPathsRaw (ATOpq _ta) = [Here]

-- All EpPath's, sorted
epPaths :: forall s t. Ord s => AnnotatedAlg s t -> [Path s]
epPaths ann = sort (epPathsRaw ann)

-- | `join` specialized to @(`->`) a@, for singletons to work
joinFn :: (a -> a -> b) -> a -> b
joinFn f x = f x x

-- | All EpPath's abbreviated, unsorted
--
-- Abbreviation is performed as follows:
-- - For `ATOr`, we omit the prefixes if @(epPathsAbbrevRaw as)@, @(epPathsAbbrevRaw bs)@ are disjoint
epPathsAbbrevRaw :: forall s t. Eq s => AnnotatedAlg s t -> [(Path s, Path s)]
epPathsAbbrevRaw (ATOr _ aa ab as bs) =
  bool_
    (
     (joinFn bimapTuple ((:+) aa) <$> as') <>
     (joinFn bimapTuple ((:+) ab) <$> bs')
    )
    (
     (fmap ((:+) aa) <$> as') <>
     (fmap ((:+) ab) <$> bs')
    )
    (null (fmap fst as' `intersect` fmap fst bs'))
  where
    bimapTuple :: (sa -> sb) -> (ta -> tb) -> (sa, ta) -> (sb, tb)
    bimapTuple f g (a, b) = (f a, g b)

    as' = epPathsAbbrevRaw as
    bs' = epPathsAbbrevRaw bs
epPathsAbbrevRaw (ATPair _ _ _ as bs) =
  liftA2 (joinFn biliftTuple (:*)) (epPathsAbbrevRaw as) (epPathsAbbrevRaw bs)
  where
    biliftTuple :: (sa -> sb -> sc) -> (sd -> se -> sf) -> (sa, sd) -> (sb, se) -> (sc, sf)
    biliftTuple f g (ax, ay) (bx, by) = (f ax bx, g ay by)
epPathsAbbrevRaw (ATOpq _ta) = [(Here, Here)]

-- All EpPath's abbreviated, sorted
--
-- We should have:
-- @
--  and . liftM2 ((==) . snd) epPathsAbbrev epPaths
-- @
epPathsAbbrev :: forall s t. Ord s => AnnotatedAlg s t -> [(Path s, Path s)]
epPathsAbbrev ann = sort (epPathsAbbrevRaw ann)

traverseEpPaths :: forall a s t. (Path a -> Bool -> a -> State' s a) -> AnnotatedAlg a t -> State' s (AnnotatedAlg a t)
traverseEpPaths fs (ATOr ann aa ab as bs) =
  (fs Here False aa >>>= \aa' ->
  (fs Here False ab >>>= \ab' ->
  ATOr ann aa' ab' <$$>
  (traverseEpPaths (fs . (:+) aa') as) <<*>>
  (traverseEpPaths (fs . (:+) ab') bs)
  ))
traverseEpPaths fs (ATPair ann aa ab as bs) =
  (fs Here True aa >>>= \aa' ->
  (fs Here True ab >>>= \ab' ->
  ATPair ann aa' ab' <$$>
  traverseEpPaths (fs . (:*) Here) as <<*>>
  traverseEpPaths (fs . (:*) Here) bs -- Here is used as a collection of all other paths
  ))
traverseEpPaths _fs (ATOpq ta) = pureState' (ATOpq ta)

uniqifyWithKey :: forall k s. (Ord k, IsString s, Semigroup s) => k -> s -> State' (ListMap k Nat) s
uniqifyWithKey key annotation =
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
    key <$$>
    getState'
    ) >>>= \(annotation', map') ->
      putState' map' *>>
      pureState' annotation'

-- count the number of occurrences of the path, then append "_n" if non-zero.
-- skip non-pair-field empty annotations
uniqifyEpPathsStepSimple :: forall s. (Ord s, IsString s, Semigroup s) => Path s -> Bool -> s -> State' (ListMap s Nat) s
uniqifyEpPathsStepSimple _epPath _isField annotation = uniqifyWithKey annotation annotation

uniqifyEpPathsSimpler :: forall s t. (Ord s, IsString s, Semigroup s) => AnnotatedAlg s t -> AnnotatedAlg s t
uniqifyEpPathsSimpler ann =
  traverseEpPaths uniqifyEpPathsStepSimple ann `evalState'`
  emptyListMap

uniqifyEpAnnotationsStep :: forall s. (Ord s, IsString s, Semigroup s) => Path s -> Bool -> s -> State' (ListMap s Nat) s
uniqifyEpAnnotationsStep _epPath False annotation = uniqifyWithKey annotation annotation
uniqifyEpAnnotationsStep _epPath True annotation = pureState' annotation

-- count the number of occurrences of the path, then append "_n" if non-zero.
-- skip non-pair-field empty annotations
uniqifyEpFieldsStep :: forall s. (Ord s, IsString s, Semigroup s) => Path s -> Bool -> s -> State' (ListMap (Path s, s) Nat) s
uniqifyEpFieldsStep _epPath False annotation = pureState' annotation
uniqifyEpFieldsStep epPath True annotation = uniqifyWithKey (epPath, annotation) annotation

uniqifyEpPaths :: forall s t. (Ord s, IsString s, Semigroup s) => AnnotatedAlg s t -> AnnotatedAlg s t
uniqifyEpPaths ann =
  traverseEpPaths uniqifyEpFieldsStep
  (traverseEpPaths uniqifyEpAnnotationsStep ann
  `evalState'` emptyListMap)
  `evalState'` emptyListMap

{-
$(singletonsOnly [d|
  -- (current path -> isPairField -> fieldName -> newFieldName)
  -- traverseEpPaths :: forall s t. (EpPath -> Bool -> Symbol -> State' s Symbol) -> SymAnn t -> State' s (SymAnn t)
  -- traverseEpPaths :: forall s t. (EpPath -> Bool -> Symbol -> State' s Symbol) -> SymAnn t -> State' s (SymAnn t)
  -- traverseEpPaths fs (ATOr ann aa ab as bs) =
  --   (fs Here False aa >>>= \aa' ->
  --   (fs Here False ab >>>= \ab' ->
  --   ATOr ann aa' ab' <$$>
  --   (traverseEpPaths (fs . ((:+) aa')) as) <<*>>
  --   (traverseEpPaths (fs . ((:+) ab')) bs)
  --   ))
  -- traverseEpPaths fs (ATPair ann aa ab as bs) =
  --   (fs Here True aa >>>= \aa' ->
  --   (fs Here True ab >>>= \ab' ->
  --   ATPair ann aa' ab' <$$>
  --   traverseEpPaths fs as <<*>>
  --   traverseEpPaths fs bs
  --   ))
  -- traverseEpPaths _fs (ATOpq ta) = pureState' (ATOpq ta)

  -- count the number of occurrences of the path, then append "_n" if non-zero.
  -- skip non-pair-field empty annotations
  uniqifyEpPathsStep :: EpPath -> Symbol -> State' (ListMap (EpPath, Symbol) Nat) Symbol
  uniqifyEpPathsStep epPath annotation = -- _isPairField
    bool_
      ((lookupModifyListMap
        0
        (\numAtPath ->
          ( bool_
              (annotation <> "_" <> show_ numAtPath)
              annotation
              (numAtPath == 0)
          , numAtPath + 1
          )
        )
        (epPath, annotation) <$$>
        getState'
        ) >>>= \(annotation', pathsMap') ->
          putState' pathsMap' *>> pureState' annotation'
      )
      (pureState' annotation)
      -- (annotation == "" && not isPairField)
      False

  uniqifyEpPaths :: forall t. SymAnn t -> SymAnn t
  uniqifyEpPaths ann =
    traverseEpPaths uniqifyEpPathsStep ann `evalState'`
    emptyListMap

  uniqifyWithA :: Symbol -> Symbol -> Symbol
  uniqifyWithA x y =
    bool_
      x
      (x <> "_1")
      (x == y)

  uniqifyWithB :: Symbol -> Symbol -> Symbol
  uniqifyWithB x y =
    bool_
      x
      (x <> "_2")
      (x == y)

  uniqifyEpPathsSimple :: forall t. SymAnn t -> SymAnn t
  uniqifyEpPathsSimple (ATOr ann aa ab as bs) =
    ATOr
      ann
      (aa `uniqifyWithA` ab)
      (ab `uniqifyWithB` aa)
      (uniqifyEpPathsSimple as)
      (uniqifyEpPathsSimple bs)
  uniqifyEpPathsSimple (ATPair ann aa ab as bs) =
    ATPair
      ann
      (aa `uniqifyWithA` ab)
      (ab `uniqifyWithB` aa)
      (uniqifyEpPathsSimple as)
      (uniqifyEpPathsSimple bs)
  uniqifyEpPathsSimple (ATOpq ta) = ATOpq ta

  |])
-}

