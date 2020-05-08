
module Michelson.Typed.T.NoOr () where

-- import Data.Kind
-- import Data.Either
-- import Data.Void
-- import Data.Function

-- import Michelson.Typed.T
-- import Michelson.Typed.Sing

-- import Data.Singletons
-- -- import Data.Constraint

-- import Data.Singletons.WrappedSing
-- import Michelson.Typed.Value.Free

-- data NoOr (t :: T) where
--   NoOrTc :: WrappedSing a -> NoOr ('Tc a)
--   NoOrTKey :: NoOr 'TKey
--   NoOrTUnit :: NoOr 'TUnit
--   NoOrTSignature :: NoOr 'TSignature
--   NoOrTChainId :: NoOr 'TChainId
--   NoOrTOption :: WrappedSing a -> NoOr ('TOption a)
--   NoOrTList :: WrappedSing a -> NoOr ('TList a)
--   NoOrTSet :: WrappedSing a -> NoOr ('TSet a)
--   NoOrTOperation :: NoOr 'TOperation
--   NoOrTContract :: WrappedSing a -> NoOr ('TContract a)
--   NoOrTPair :: WrappedSing a -> WrappedSing b -> NoOr ('TPair a b)
--   NoOrTOr :: WrappedSing a -> WrappedSing b -> Void -> NoOr ('TOr a b)
--   NoOrTLambda :: WrappedSing a -> WrappedSing b -> NoOr ('TLambda a b)
--   NoOrTMap :: WrappedSing a -> WrappedSing b -> NoOr ('TMap a b)
--   NoOrTBigMap :: WrappedSing a -> WrappedSing b -> NoOr ('TBigMap a b)

-- data IsOr (t :: T) where
--   IsOr :: Sing a -> Sing b -> IsOr ('TOr a b)

-- class Promotable a where
--   type Promote (x :: a) :: WrappedSing x
--   promoteSing :: Sing x -> Sing (Promote x)

-- type family PromoteCT (ct :: CT) where

-- instance Promotable CT where
--   type Promote (ct :: CT) = PromoteCT ct

--   promoteSing = \case


-- type family PromoteT (t :: T) where
--   -- PromoteT ('Tc sa) = 'STc (PromoteCT sa)
--   -- PromoteT ('TPair sa sb) = STPair (Promote sa) (Promote sb)

--   -- PromoteT ('TKey) = 'WrapSing STKey

-- --   PromoteT ('TUnit) = STUnit
-- --   PromoteT ('TSignature) = STSignature
-- --   PromoteT ('TChainId) = STChainId
-- --   PromoteT ('TOption sa) = STOption (Promote sa)
-- --   PromoteT ('TList sa) = STList (Promote sa)
-- --   PromoteT ('TSet sa) = STSet (Promote sa)
-- --   PromoteT ('TOperation) = STOperation
-- --   PromoteT ('TContract sa) = STContract (Promote sa)
-- --   PromoteT ('TOr sa sb) = STOr (Promote sa) (Promote sb)
-- --   PromoteT ('TLambda sa sb) = STLambda (Promote sa) (Promote sb)
-- --   PromoteT ('TMap sa sb) = STMap (Promote sa) (Promote sb)
-- --   PromoteT ('TBigMap sa sb) = STBigMap (Promote sa) (Promote sb)

-- -- instance Promotable T where
-- --   type Promote (t :: T) = PromoteT t

-- --   promoteSing = \case

-- -- type family Promote (x :: a) :: Sing x where


-- -- SingT (t :: T) :: WrappedSing t where
-- --   _

-- -- type family DecOr (t :: T) :: Either (NoOr t) (IsOr t) where
-- --   DecOr ('Tc sa) = 'Left ('NoOrTc ('WrapSing (Promote sa)))
-- --   DecOr ('TPair sa sb) = 'Left ('NoOrTPair ('WrapSing (Promote sa)) ('WrapSing (Promote sb)))
-- --   DecOr ('TKey) = 'Left ('NoOrTKey)
-- --   DecOr ('TUnit) = 'Left ('NoOrTUnit)
-- --   DecOr ('TSignature) = 'Left ('NoOrTSignature)
-- --   DecOr ('TChainId) = 'Left ('NoOrTChainId)
-- --   DecOr ('TOption sa) = 'Left ('NoOrTOption ('WrapSing (Promote sa)))
-- --   DecOr ('TList sa) = 'Left ('NoOrTList ('WrapSing (Promote sa)))
-- --   DecOr ('TSet sa) = 'Left ('NoOrTSet ('WrapSing (Promote sa)))
-- --   DecOr ('TOperation) = 'Left ('NoOrTOperation)
-- --   DecOr ('TContract sa) = 'Left ('NoOrTContract ('WrapSing (Promote sa)))
-- --   DecOr ('TOr sa sb) = 'Right ('IsOr (Promote sa) (Promote sb))
-- --   DecOr ('TLambda sa sb) = 'Left ('NoOrTLambda ('WrapSing (Promote sa)) ('WrapSing (Promote sb)))
-- --   DecOr ('TMap sa sb) = 'Left ('NoOrTMap ('WrapSing (Promote sa)) ('WrapSing (Promote sb)))
-- --   DecOr ('TBigMap sa sb) = 'Left ('NoOrTBigMap ('WrapSing (Promote sa)) ('WrapSing (Promote sb)))

-- -- singDecOr :: Sing t -> Sing (DecOr t)
-- -- singDecOr (STc sa) = SLeft (SNoOrTc (WrapSing' (WrapSing sa)))
-- -- singDecOr (STPair sa sb) = SLeft (SNoOrTPair (WrapSing' (WrapSing sa)) (WrapSing' (WrapSing sb)))
-- -- singDecOr (STKey) = SLeft (SNoOrTKey)
-- -- singDecOr (STUnit) = SLeft (SNoOrTUnit)
-- -- singDecOr (STSignature) = SLeft (SNoOrTSignature)
-- -- singDecOr (STChainId) = SLeft (SNoOrTChainId)
-- -- singDecOr (STOption sa) = SLeft (SNoOrTOption (WrapSing' (WrapSing sa)))
-- -- singDecOr (STList sa) = SLeft (SNoOrTList (WrapSing' (WrapSing sa)))
-- -- singDecOr (STSet sa) = SLeft (SNoOrTSet (WrapSing' (WrapSing sa)))
-- -- singDecOr (STOperation) = SLeft (SNoOrTOperation)
-- -- singDecOr (STContract sa) = SLeft (SNoOrTContract (WrapSing' (WrapSing sa)))
-- -- singDecOr (STOr sa sb) = Right (IsOr sa sb)
-- -- singDecOr (STLambda sa sb) = SLeft (SNoOrTLambda (WrapSing' (WrapSing sa)) (WrapSing' (WrapSing sb)))
-- -- singDecOr (STMap sa sb) = SLeft (SNoOrTMap (WrapSing' (WrapSing sa)) (WrapSing' (WrapSing sb)))
-- -- singDecOr (STBigMap sa sb) = SLeft (SNoOrTBigMap (WrapSing' (WrapSing sa)) (WrapSing' (WrapSing sb)))

-- -- isOr :: Sing t -> Either (NoOr t) (IsOr t)
-- -- isOr =
-- --   \case
-- --     STc sa -> Left $ NoOrTc (WrapSing sa)
-- --     STPair sa sb -> Left $ NoOrTPair (WrapSing sa) (WrapSing sb)
-- --     STKey -> Left $ NoOrTKey
-- --     STUnit -> Left $ NoOrTUnit
-- --     STSignature -> Left $ NoOrTSignature
-- --     STChainId -> Left $ NoOrTChainId
-- --     STOption sa -> Left $ NoOrTOption (WrapSing sa)
-- --     STList sa -> Left $ NoOrTList (WrapSing sa)
-- --     STSet sa -> Left $ NoOrTSet (WrapSing sa)
-- --     STOperation -> Left $ NoOrTOperation
-- --     STContract sa -> Left $ NoOrTContract (WrapSing sa)
-- --     STOr sa sb -> Right $ IsOr sa sb
-- --     STLambda sa sb -> Left $ NoOrTLambda (WrapSing sa) (WrapSing sb)
-- --     STMap sa sb -> Left $ NoOrTMap (WrapSing sa) (WrapSing sb)
-- --     STBigMap sa sb -> Left $ NoOrTBigMap (WrapSing sa) (WrapSing sb)


-- data instance Sing :: forall t_aGLU. NoOr t_aGLU -> Type
--   where
--     SNoOrTc :: forall a_XGLW (n_aGSB :: WrappedSing a_XGLW).
--                 (Sing (n_aGSB :: WrappedSing a_XGLW)) -> Sing ('NoOrTc n_aGSB)
--     SNoOrTKey :: Sing 'NoOrTKey
--     SNoOrTUnit :: Sing 'NoOrTUnit
--     SNoOrTSignature :: Sing 'NoOrTSignature
--     SNoOrTChainId :: Sing 'NoOrTChainId
--     SNoOrTOption :: forall a_XGLX (n_aGSD :: WrappedSing a_XGLX).
--                      (Sing (n_aGSD :: WrappedSing a_XGLX))
--                      -> Sing ('NoOrTOption n_aGSD)
--     SNoOrTList :: forall a_XGLY (n_aGSF :: WrappedSing a_XGLY).
--                    (Sing (n_aGSF :: WrappedSing a_XGLY))
--                    -> Sing ('NoOrTList n_aGSF)
--     SNoOrTSet :: forall a_XGLZ (n_aGSH :: WrappedSing a_XGLZ).
--                   (Sing (n_aGSH :: WrappedSing a_XGLZ)) -> Sing ('NoOrTSet n_aGSH)
--     SNoOrTOperation :: Sing 'NoOrTOperation
--     SNoOrTContract :: forall a_XGM0 (n_aGSJ :: WrappedSing a_XGM0).
--                        (Sing (n_aGSJ :: WrappedSing a_XGM0))
--                        -> Sing ('NoOrTContract n_aGSJ)
--     SNoOrTPair :: forall a_XGM1
--                           b_XGM3
--                           (n_aGSL :: WrappedSing a_XGM1)
--                           (n_aGSM :: WrappedSing b_XGM3).
--                    (Sing (n_aGSL :: WrappedSing a_XGM1))
--                    -> (Sing (n_aGSM :: WrappedSing b_XGM3))
--                       -> Sing ('NoOrTPair n_aGSL n_aGSM)
--     SNoOrTOr :: forall a_XGM3
--                         b_XGM5
--                         (n_aGSP :: WrappedSing a_XGM3)
--                         (n_aGSQ :: WrappedSing b_XGM5)
--                         (n_aGSR :: Void).
--                  (Sing (n_aGSP :: WrappedSing a_XGM3))
--                  -> (Sing (n_aGSQ :: WrappedSing b_XGM5))
--                     -> (Sing (n_aGSR :: Void))
--                        -> Sing ('NoOrTOr n_aGSP n_aGSQ n_aGSR)
--     SNoOrTLambda :: forall a_XGM5
--                             b_XGM7
--                             (n_aGSV :: WrappedSing a_XGM5)
--                             (n_aGSW :: WrappedSing b_XGM7).
--                      (Sing (n_aGSV :: WrappedSing a_XGM5))
--                      -> (Sing (n_aGSW :: WrappedSing b_XGM7))
--                         -> Sing ('NoOrTLambda n_aGSV n_aGSW)
--     SNoOrTMap :: forall a_XGM7
--                          b_XGM9
--                          (n_aGSZ :: WrappedSing a_XGM7)
--                          (n_aGT0 :: WrappedSing b_XGM9).
--                   (Sing (n_aGSZ :: WrappedSing a_XGM7))
--                   -> (Sing (n_aGT0 :: WrappedSing b_XGM9))
--                      -> Sing ('NoOrTMap n_aGSZ n_aGT0)
--     SNoOrTBigMap :: forall a_XGM9
--                             b_XGMb
--                             (n_aGT3 :: WrappedSing a_XGM9)
--                             (n_aGT4 :: WrappedSing b_XGMb).
--                      (Sing (n_aGT3 :: WrappedSing a_XGM9))
--                      -> (Sing (n_aGT4 :: WrappedSing b_XGMb))
--                         -> Sing ('NoOrTBigMap n_aGT3 n_aGT4)

-- instance SingKind (NoOr t_aGLU) where
--   type Demote (NoOr t_aGLU) = NoOr t_aGLU
--   fromSing (SNoOrTc b_aGT7) = NoOrTc (fromSing b_aGT7)
--   fromSing SNoOrTKey = NoOrTKey
--   fromSing SNoOrTUnit = NoOrTUnit
--   fromSing SNoOrTSignature = NoOrTSignature
--   fromSing SNoOrTChainId = NoOrTChainId
--   fromSing (SNoOrTOption b_aGT8) = NoOrTOption (fromSing b_aGT8)
--   fromSing (SNoOrTList b_aGT9) = NoOrTList (fromSing b_aGT9)
--   fromSing (SNoOrTSet b_aGTa) = NoOrTSet (fromSing b_aGTa)
--   fromSing SNoOrTOperation = NoOrTOperation
--   fromSing (SNoOrTContract b_aGTb)
--     = NoOrTContract (fromSing b_aGTb)
--   fromSing (SNoOrTPair b_aGTc b_aGTd)
--     = (NoOrTPair (fromSing b_aGTc)) (fromSing b_aGTd)
--   fromSing (SNoOrTOr b_aGTe b_aGTf b_aGTg)
--     = ((NoOrTOr (fromSing b_aGTe)) (fromSing b_aGTf))
--         (fromSing b_aGTg)
--   fromSing (SNoOrTLambda b_aGTh b_aGTi)
--     = (NoOrTLambda (fromSing b_aGTh)) (fromSing b_aGTi)
--   fromSing (SNoOrTMap b_aGTj b_aGTk)
--     = (NoOrTMap (fromSing b_aGTj)) (fromSing b_aGTk)
--   fromSing (SNoOrTBigMap b_aGTl b_aGTm)
--     = (NoOrTBigMap (fromSing b_aGTl)) (fromSing b_aGTm)
--   toSing (NoOrTc (b_aGTo :: Demote (WrappedSing a_XGLW)))
--     = case toSing b_aGTo :: SomeSing (WrappedSing a_XGLW) of {
--         SomeSing c_aGTp -> SomeSing (SNoOrTc c_aGTp) }
--   toSing NoOrTKey = SomeSing SNoOrTKey
--   toSing NoOrTUnit = SomeSing SNoOrTUnit
--   toSing NoOrTSignature = SomeSing SNoOrTSignature
--   toSing NoOrTChainId = SomeSing SNoOrTChainId
--   toSing (NoOrTOption (b_aGTq :: Demote (WrappedSing a_XGLX)))
--     = case toSing b_aGTq :: SomeSing (WrappedSing a_XGLX) of {
--         SomeSing c_aGTr -> SomeSing (SNoOrTOption c_aGTr) }
--   toSing (NoOrTList (b_aGTs :: Demote (WrappedSing a_XGLY)))
--     = case toSing b_aGTs :: SomeSing (WrappedSing a_XGLY) of {
--         SomeSing c_aGTt -> SomeSing (SNoOrTList c_aGTt) }
--   toSing (NoOrTSet (b_aGTu :: Demote (WrappedSing a_XGLZ)))
--     = case toSing b_aGTu :: SomeSing (WrappedSing a_XGLZ) of {
--         SomeSing c_aGTv -> SomeSing (SNoOrTSet c_aGTv) }
--   toSing NoOrTOperation = SomeSing SNoOrTOperation
--   toSing (NoOrTContract (b_aGTw :: Demote (WrappedSing a_XGM0)))
--     = case toSing b_aGTw :: SomeSing (WrappedSing a_XGM0) of {
--         SomeSing c_aGTx -> SomeSing (SNoOrTContract c_aGTx) }
--   toSing
--     (NoOrTPair (b_aGTy :: Demote (WrappedSing a_XGM1))
--                 (b_aGTz :: Demote (WrappedSing b_XGM3)))
--     = case
--           ((,) (toSing b_aGTy :: SomeSing (WrappedSing a_XGM1)))
--             (toSing b_aGTz :: SomeSing (WrappedSing b_XGM3))
--       of {
--         (,) (SomeSing c_aGTA) (SomeSing c_aGTB)
--           -> SomeSing ((SNoOrTPair c_aGTA) c_aGTB) }
--   toSing
--     (NoOrTOr (b_aGTC :: Demote (WrappedSing a_XGM3))
--               (b_aGTD :: Demote (WrappedSing b_XGM5))
--               (b_aGTE :: Demote Void))
--     = case
--           (((,,) (toSing b_aGTC :: SomeSing (WrappedSing a_XGM3)))
--              (toSing b_aGTD :: SomeSing (WrappedSing b_XGM5)))
--             (toSing b_aGTE :: SomeSing Void)
--       of {
--         (,,) (SomeSing c_aGTF) (SomeSing c_aGTG) (SomeSing c_aGTH)
--           -> SomeSing (((SNoOrTOr c_aGTF) c_aGTG) c_aGTH) }
--   toSing
--     (NoOrTLambda (b_aGTI :: Demote (WrappedSing a_XGM5))
--                   (b_aGTJ :: Demote (WrappedSing b_XGM7)))
--     = case
--           ((,) (toSing b_aGTI :: SomeSing (WrappedSing a_XGM5)))
--             (toSing b_aGTJ :: SomeSing (WrappedSing b_XGM7))
--       of {
--         (,) (SomeSing c_aGTK) (SomeSing c_aGTL)
--           -> SomeSing ((SNoOrTLambda c_aGTK) c_aGTL) }
--   toSing
--     (NoOrTMap (b_aGTM :: Demote (WrappedSing a_XGM7))
--                (b_aGTN :: Demote (WrappedSing b_XGM9)))
--     = case
--           ((,) (toSing b_aGTM :: SomeSing (WrappedSing a_XGM7)))
--             (toSing b_aGTN :: SomeSing (WrappedSing b_XGM9))
--       of {
--         (,) (SomeSing c_aGTO) (SomeSing c_aGTP)
--           -> SomeSing ((SNoOrTMap c_aGTO) c_aGTP) }
--   toSing
--     (NoOrTBigMap (b_aGTQ :: Demote (WrappedSing a_XGM9))
--                   (b_aGTR :: Demote (WrappedSing b_XGMb)))
--     = case
--           ((,) (toSing b_aGTQ :: SomeSing (WrappedSing a_XGM9)))
--             (toSing b_aGTR :: SomeSing (WrappedSing b_XGMb))
--       of {
--         (,) (SomeSing c_aGTS) (SomeSing c_aGTT)
--           -> SomeSing ((SNoOrTBigMap c_aGTS) c_aGTT) }

-- instance SingI n_aGSB =>
--          SingI ('NoOrTc (n_aGSB :: WrappedSing a_XGLW)) where
--   sing = SNoOrTc sing

-- instance SingI 'NoOrTKey where
--   sing = SNoOrTKey

-- instance SingI 'NoOrTUnit where
--   sing = SNoOrTUnit

-- instance SingI 'NoOrTSignature where
--   sing = SNoOrTSignature

-- instance SingI 'NoOrTChainId where
--   sing = SNoOrTChainId

-- instance SingI n_aGSD =>
--          SingI ('NoOrTOption (n_aGSD :: WrappedSing a_XGLX)) where
--   sing = SNoOrTOption sing

-- instance SingI n_aGSF =>
--          SingI ('NoOrTList (n_aGSF :: WrappedSing a_XGLY)) where
--   sing = SNoOrTList sing

-- instance SingI n_aGSH =>
--          SingI ('NoOrTSet (n_aGSH :: WrappedSing a_XGLZ)) where
--   sing = SNoOrTSet sing

-- instance SingI 'NoOrTOperation where
--   sing = SNoOrTOperation

-- instance SingI n_aGSJ =>
--          SingI ('NoOrTContract (n_aGSJ :: WrappedSing a_XGM0)) where
--   sing = SNoOrTContract sing

-- instance (SingI n_aGSL, SingI n_aGSM) =>
--          SingI ('NoOrTPair (n_aGSL :: WrappedSing a_XGM1) (n_aGSM :: WrappedSing b_XGM3)) where
--   sing = (SNoOrTPair sing) sing

-- instance (SingI n_aGSP, SingI n_aGSQ, SingI n_aGSR) =>
--          SingI ('NoOrTOr (n_aGSP :: WrappedSing a_XGM3) (n_aGSQ :: WrappedSing b_XGM5) (n_aGSR :: Void)) where
--   sing = ((SNoOrTOr sing) sing) sing

-- instance (SingI n_aGSV, SingI n_aGSW) =>
--          SingI ('NoOrTLambda (n_aGSV :: WrappedSing a_XGM5) (n_aGSW :: WrappedSing b_XGM7)) where
--   sing = (SNoOrTLambda sing) sing

-- instance (SingI n_aGSZ, SingI n_aGT0) =>
--          SingI ('NoOrTMap (n_aGSZ :: WrappedSing a_XGM7) (n_aGT0 :: WrappedSing b_XGM9)) where
--   sing = (SNoOrTMap sing) sing

-- instance (SingI n_aGT3, SingI n_aGT4) =>
--          SingI ('NoOrTBigMap (n_aGT3 :: WrappedSing a_XGM9) (n_aGT4 :: WrappedSing b_XGMb)) where
--   sing = (SNoOrTBigMap sing) sing


