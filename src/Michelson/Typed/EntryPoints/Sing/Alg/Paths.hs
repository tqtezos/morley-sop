{-# OPTIONS -Wno-missing-export-lists #-}

-- TODO:
-- - this is a port from module Michelson.Typed.EntryPoints.Sing.Alg.FieldNames where
--   we return a list of all EpPath's for an annotated TAlg
-- - Along with SOP and FieldNames, this gives a SOP implementation of Michelson
--   Value's, where intermediate transformations are performed in a single type-family pass

-- | This module is a port of Michelson.Typed.EntryPoints.Sing.Alg.Types,
-- where @ErrM@ is replaced with an accumulating @[]@
module Michelson.Typed.EntryPoints.Sing.Alg.Paths where

import Prelude

import Data.Singletons.Prelude.Monad.State
import Data.Singletons.Map
import Michelson.Typed.Annotation.Path

import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.Annotation.Sing.Alg

import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.List

$(singletonsOnly [d|

  -- All EpPath's, unsorted
  epPathsRaw :: forall t. SymAnn t -> [EpPath]
  epPathsRaw (ATOr _ aa ab as bs) =
    ((:+) aa <$> epPathsRaw as) ++
    ((:+) ab <$> epPathsRaw bs)
  epPathsRaw (ATPair _ _ _ as bs) = liftA2 (:*) (epPathsRaw as) (epPathsRaw bs)
  epPathsRaw (ATOpq _ta) = [Here]

  -- All EpPath's, sorted
  epPaths :: forall t. SymAnn t -> [EpPath]
  epPaths ann = sort (epPathsRaw ann)
  |])

$(singletons [d|

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

  |])


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


$(singletonsOnly [d|

  -- count the number of occurrences of the path, then append "_n" if non-zero.
  -- skip non-pair-field empty annotations
  uniqifyEpPathsStepSimple :: EpPath -> Symbol -> State' (ListMap Symbol Nat) Symbol
  uniqifyEpPathsStepSimple _epPath annotation =
    (lookupModifyListMap
      0
      (\numAtPath ->
        ( bool_
            (annotation <> "_" <> show_ numAtPath)
            annotation
            (numAtPath == 0)
        , numAtPath + 1
        )
      )
      annotation <$$>
      getState'
      ) >>>= \(annotation', pathsMap') ->
        putState' pathsMap' *>> pureState' annotation'

  uniqifyEpPathsSimpler :: forall t. SymAnn t -> SymAnn t
  uniqifyEpPathsSimpler ann =
    traverseEpPaths uniqifyEpPathsStepSimple ann `evalState'`
    emptyListMap

  |])


