-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists #-}
-- {-# OPTIONS -Wno-orphans #-}

module Data.SOP.Deep.Annotated.Test where

-- import Data.Kind
-- import Data.Bool
-- import Data.Either
-- import Data.Void
-- -- import Data.List.NonEmpty (NonEmpty(..))
-- import GHC.TypeLits
-- import Prelude (($), NonEmpty(..), Int, Either(..), fromString, error)

-- import Data.Constraint
-- import Data.Singletons
-- import Data.Singletons.Prelude.List (Sing(..))
-- -- import Data.SOP (SOP(..), I(..), Prod, AllZipN)
-- -- import Data.SOP.Constraint (LiftedCoercible)
-- -- import qualified Data.SOP as SOP
-- -- import Generics.SOP (Generic(..))

-- import Data.Constraint.HasDict1
-- -- import Data.SOP.Map
-- -- import Data.SOP.Join

-- -- import Data.List.Concat

-- import Data.SOP.Deep
-- import Data.SOP.Deep.Annotated


-- data Foo
--   = Bar Int
--   | Baz Bool
--   | Bop (Either Int Bool)
--   | Bng (Either (Either Int Bool) (Either Int Bool))

-- data TT = Bar' | Baz' | Bop' | Bng'

-- data instance Sing :: TT -> Type where
--   SBar' :: Sing 'Bar'
--   SBaz' :: Sing 'Baz'
--   SBop' :: Sing 'Bop'
--   SBng' :: Sing 'Bng'

-- instance SingI 'Bar' where sing = SBar'
-- instance SingI 'Baz' where sing = SBaz'
-- instance SingI 'Bop' where sing = SBop'
-- instance SingI 'Bng' where sing = SBng'

-- instance HasDict1 TT where
--   evidence1 SBar' = Dict
--   evidence1 SBaz' = Dict
--   evidence1 SBop' = Dict
--   evidence1 SBng' = Dict

-- data FooF (tt :: TT) where
--   BarF :: Int -> FooF 'Bar'
--   BazF :: Bool -> FooF 'Baz'
--   BopF :: Either (FooF 'Bar') (FooF 'Baz') -> FooF 'Bop'
--   BngF :: Void -> FooF 'Bng'

-- type family TTCode (tt :: TT) :: [[TT]] where
--   TTCode 'Bar' = '[ '[ 'Bar' ]]
--   TTCode 'Baz' = '[ '[ 'Baz' ]]
--   TTCode 'Bop' = '[ '[ 'Bar' ], '[ 'Baz' ]]
--   TTCode 'Bng' = '[ '[ 'Bar' ], '[ 'Baz' ], '[ 'Bop' ] ]

-- instance HasDeep TT FooF where
--   type DCode x = TTCode x
--   singDCode SBar' = SCons (SCons SBar' SNil) SNil
--   singDCode SBaz' = SCons (SCons SBaz' SNil) SNil
--   singDCode SBop' = SCons (SCons SBar' SNil) (SCons (SCons SBaz' SNil) SNil)
--   singDCode SBng' = error "singDCode HasDeep TT"

--   toD = error "HasDeep TT FooF: toD undefined"
--   fromD = error "HasDeep TT FooF: fromD undefined"

-- -- type AC = 'Annotate '[ '[ "hi, bar!" ':| '[] ], '[ "hi, baz!" ':| '[] ]] 'Bop'
-- type AC = 'Annotate '[ '[ "hi, bar!" ':| '[] ], '[ "hi, baz!" ':| '[] ], '[ "hi, bop!" ':| '[] ] ] 'Bng'

-- -- otjoke :: _
-- otjoke = Proxy @(Deep (DCode AC))

