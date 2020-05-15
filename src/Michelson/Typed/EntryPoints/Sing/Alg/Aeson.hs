{-# LANGUAGE NoTemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-orphans -Wno-deprecations #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Aeson where

import Data.String
import Prelude hiding (unwords, show, forM_, view, Lens')
import GHC.Generics ((:.:)(..))
import Text.Show

import Lorentz ((:!), IsoValue(..), EpdPlain, MText)
import Michelson.Text
import Lorentz.EntryPoints.Core
import Michelson.Typed.T (T)
import Michelson.Typed.Scope
import Util.Named

import Data.AltError
import Data.ListError
import Data.AltError.Run
import Data.Singletons.WrappedSing

import Michelson.Typed.EntryPoints.Sing.Alg
import Michelson.Typed.EntryPoints.Sing.Alg.Field
import Michelson.Typed.EntryPoints.Sing.Alg.FieldNames
import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free

import Data.Singletons
import Data.Constraint

import Data.Constraint.HasDict1

import Data.SOP (I(..), K(..), NP)
import qualified Data.SOP as SOP

import Data.Aeson -- (ToJSON(..))
import qualified Data.Aeson as Aeson -- (ToJSON(..))

assertOpAbsense :: forall (t :: T) a. Sing t -> (HasNoOp t => a) -> a
assertOpAbsense st f =
  case opAbsense st of
    Nothing -> error "assertOpAbsense"
    Just Dict -> withDict1 st $ forbiddenOp @t f

data SomeValueOpq where
  SomeValueOpq :: forall (t :: TOpq). (SingI t, HasNoOp (FromTOpq t))
    => ValueOpq t
    -> SomeValueOpq

instance ToJSON SomeValueOpq where
  toJSON (SomeValueOpq xs) = toJSON xs
  toEncoding (SomeValueOpq xs) = toEncoding xs

-- epFieldsToEncoding :: forall t ann epPath. (SingI t, SingI ann)
--     => Sing epPath
--     -> NP (EpField I t ann epPath) (ListEToErrM (EpFieldNames t ann epPath)) -- (EpFieldNames ann epPath)
--     -> Aeson.Value
-- epFieldsToEncoding sepPath xss =
--   object $
--   withDict (singAllSingI $ sEpFieldNames (sing @ann) sepPath) $
--   SOP.hcollapse $
--   SOP.hmap mapper xss
--     where
--     mapper :: forall fieldName. EpField I t ann epPath fieldName -> K (Text, Aeson.Value) fieldName
--     mapper (EpField sfieldName ys) = K
--       ( fromSing sfieldName
--       , case ys of
--           RunAltThrow (WrapSing serr) ->
--             error . fromString $
--             unwords ["epFieldsToEncoding: invalid field: RunAltThrow:", show (fromSing serr)]
--           RunAltExcept (WrapSing serr) ->
--             error . fromString $
--             unwords ["epFieldsToEncoding: invalid field: RunAltExcept:", show (fromSing serr)]
--           RunPureAltE (Comp1 (I yss)) ->
--             case sEpFieldT (sing @t) (sing @ann) sepPath sfieldName of
--               SPureAltE st ->
--                 assertOpAbsense (singFromTOpq st) $
--                 withDict1 st $ toJSON $
--                 SomeValueOpq yss
--       )

-- instance (SingI t, SingI ann) => ToJSON (EpValue t ann) where
--   toJSON xs = object
--     [ "path" .= epValuePath (sing @t) (sing @ann) xs
--     , "fields" .= (epValueFields (epFieldsToEncoding @t @ann) (sing @t) (sing @ann) xs)
--     ]

--   toEncoding xs = pairs $
--     "path" .= epValuePath (sing @t) (sing @ann) xs <>
--     "fields" .= (epValueFields (epFieldsToEncoding @t @ann) (sing @t) (sing @ann) xs)




-------------
-- EXAMPLE --
-------------

data ExampleParam
  = Get ("user" :! Natural)
  | Set ("user" :! Natural, "val" :! Bool, "info" :! MText, "extra" :! Integer)
  | Default ("default_unit" :! ())
  deriving (Show, Generic)

exampleParams :: [ExampleParam]
exampleParams = [ Get (#user .! 0)
                , Set (#user .! 1, #val .! True, #info .! mkMTextUnsafe "hi", #extra .! 3)
                , Set (#user .! 2, #val .! False, #info .! mkMTextUnsafe "ok", #extra .! 4)
                , Default (#default_unit .! ())
                ]

instance IsoValue ExampleParam

instance ParameterHasEntryPoints ExampleParam where
  type ParameterEntryPointsDerivation ExampleParam = EpdPlain

exampleValueAlgs :: [ValueAlg (ToTAlg (ToT ExampleParam))]
exampleValueAlgs = toValueAlg . toVal <$> exampleParams

-- exampleJSON :: [Aeson.Value]
-- exampleJSON =
--   case toSing (Michelson.annotatedFromNotes (epdNotes @EpdPlain @ExampleParam)) of
--     SomeSing (sann :: Sing ann) ->
--       let sann' = singToAnnotatedAlg sann in
--       let getter = (lensEpValueF
--                     (sing @(ToTAlg (ToT ExampleParam)))
--                     sann'
--                     :: Lens' (ValueAlgT Maybe (ToTAlg (ToT ExampleParam))) (EpValueF Maybe (ToTAlg (ToT ExampleParam)) (ToAnnotatedAlg ann))
--                    ) in
--       withDict1 sann' $
--       toJSON . fromEpValueF (sing @(ToTAlg (ToT ExampleParam))) sann' . view getter . toValueAlgT sing <$> exampleValueAlgs

-- demoJSON :: IO ()
-- demoJSON =
--   forM_ exampleJSON $
--   BL.putStrLn.encodePretty

