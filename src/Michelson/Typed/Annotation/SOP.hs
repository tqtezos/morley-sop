{-# OPTIONS -Wno-missing-export-lists -Wall -Wno-orphans #-}

module Michelson.Typed.Annotation.SOP where

-- import Prelude (($))
-- -- import Data.Foldable
-- -- import Data.Maybe
-- import Data.Either
-- -- import Data.Functor
-- -- import Data.Kind
-- -- import Data.Traversable
-- -- import GHC.TypeLits
-- -- import Data.Ord

-- -- import Michelson.Typed.Haskell
-- import Michelson.Typed.Value
-- import Michelson.Typed.Instr (Instr)
-- import Michelson.Typed.Sing
-- import Michelson.Typed.T
-- -- import Michelson.Typed.Aliases

-- -- import Data.Constraint
-- import Data.Singletons
-- -- import Data.Singletons.Prelude.List (Sing(..)) -- SCons, SNil)
-- import Data.SOP (SOP(..)) -- , NS(..), NP(..), I(..), All, SListI, SListI2)
-- import qualified Data.SOP as SOP
-- -- import Generics.SOP (Generic(..))

-- -- import Data.Constraint.HasDict1
-- -- import Data.SOP.Deep
-- -- import Data.SOP.Deep.Annotated
-- -- import Data.SOP.Deep.Annotated.Test

-- AnnValue' instr = AnnValue'
--   { entryAnn :: !Text
--   , fieldAnn :: !Text
--   ,

-- Notes t -> SOP (Value' instr) (TCode t) -> SOP (AnnValue' instr) (TCode t)

-- fromValue' :: forall instr t. Sing t -> Value' instr t -> SOP (Value' instr) (TCode t)

-- data AnnT a where
--   AnnT :: forall (t :: T). [Text] -> Annotated a t -> T -> AnnT a

-- type family AnnotatedTCode (t :: T) (ann :: Annotated a t) :: [[AnnT a]] where
--   AnnotatedTCode ('Tc ct) ann = '[ '[ AnnT '[] ann ('Tc ct)]]
--   TCode ('TPair a b) ('ATPair _ x y xs ys) = '[ '[ 'AnnT '[] , '(y, ys, b)]] -- TCode a ** TCode b -- cartesian product
--   TCode ('TOr a b) ('ATOr _ x y xs ys) = '[ '[ '(x, xs, a), '[(y, ys, b)]] -- TCode a ++ TCode b


-- type family AnnotatedTCode (t :: T) (ann :: Annotated a t) :: [([a], [([a], Annotated a (t :: T))])] where
--   AnnotatedTCode ('Tc ct) ('ATc x) = '[ '( '[], '[ '( 'Just x, 'Tc ct)]]

-- type family TCode (t :: T) :: [[T]] where
--   TCode ('Tc ct) ('ATc ta) = '[ '[ '( '[ta], 'Tc ct)]]
--   TCode ('TKey) ('ATKey ta) = '[ '[ '( '[ta], 'TKey)]]
--   TCode ('TUnit) ('ATUnit ta) = '[ '[]] -- empty product
--   TCode ('TSignature) ('ATSignature ta) = '[ '[ '( '[ta], 'TSignature)]]
--   TCode ('TChainId) ('ATChainId ta) = '[ '[ '( '[ta], 'TChainId)]]
--   TCode ('TOption t) ('ATOption ta xs) = '[ '[ '( '[ta], 'TOption t)]] -- TCode ('TOption t) = '[] ': TCode t
--   TCode ('TList t) ('ATList ta xs) = '[ '[ '( '[ta], 'TList t)]]
--   TCode ('TSet ct) ('ATSet ta tb) = '[ '[ '( '[ta], 'TSet ct)]]
--   TCode ('TOperation) ('ATOperation ta) = '[ '[ '( '[ta], 'TOperation)]]
--   TCode ('TContract t) ('ATContract ta xs) = '[ '[ 'TContract t]]

--   TCode ('TPair a b) ('ATPair ta tb tc xs ys) = '[ '[a, b]] -- TCode a ** TCode b -- cartesian product

--   TCode ('TOr a b) ('ATOr ta tb tc xs ys) = '[ '[a], '[b]] -- TCode a ++ TCode b

--   TCode ('TLambda a b) ('ATLambda ta xs ys) = '[ '[ '( '[ta], 'TLambda a b)]]
--   TCode ('TMap a b) ('ATMap ta tb xs) = '[ '[ '( '[ta], 'TMap a b)]]
--   TCode ('TBigMap a b) ('ATBigMap ta tb xs) = '[ '[ '( '[ta], 'TBigMap a b)]]


-- annotateSOP :: forall instr t (ann :: Annotated Symbol t). ()
--   => Sing ann
--   -> SOP (Value' instr) (TCode t)
--   -> SOP (_)


