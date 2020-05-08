
module Data.BTree () where

-- import Data.Kind
-- -- import GHC.TypeLits

-- import Data.Singletons
-- import Data.Singletons.TypeLits
-- import Data.Constraint


-- data BTree a =
--   (:*) :: BTree a -> BTree a -> BTree a
--   Leaf :: a -> BTree a
--   deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

-- data instance Sing :: BTree a -> Type where
--   (:**) :: forall a (xs :: BTree a) (ys :: BTree a). SBTree xs -> SBTree ys -> Sing ('(:*) xs ys)
--   SLeaf :: forall a (x :: a). Sing x -> Sing ('Leaf x)

-- instance (SingI xs, SingI ys) => SingI ('(:*) xs ys) where sing = sing :** sing
-- instance SingI x => SingI ('Leaf x) where sing = SLeaf sing

-- singIBTree :: forall a (xs :: BTree a). (forall (x :: a). Sing x -> Dict (SingI x)) -> Sing xs -> Dict (SingI xs)
-- singIBTree singIA ((:**) sxs sys) =
--   withDict (singIBTree singIA sxs) $
--   withDict (singIBTree singIA sys) $
--   Dict
-- singIBTree singIA (SLeaf sx) =
--   withDict (singIA sx) $
--   Dict

-- instance SingKind a => SingKind (BTree a) where
--   type Demote (BTree a) = BTree (Demote a)

--   fromSing ((:**) sxs sys) = fromSing sxs :* fromSing sys
--   fromSing (SLeaf sx) = Leaf $ fromSing sx

--   toSing ((:*) xs ys) =
--     case (toSing xs, toSing ys) of
--       (SomeSing sxs, SomeSing sys) -> SomeSing $ sxs :** sys
--   toSing (Leaf x) =
--     case toSing x of
--       SomeSing sx -> SomeSing $ SLeaf sx

