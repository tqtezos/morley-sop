
module Michelson.Typed.T.NoPair () where

-- import Data.Kind
-- import Data.Either
-- import Data.Void
-- import Data.Function

-- import Michelson.Typed.T
-- import Michelson.Typed.Sing

-- import Data.Singletons
-- -- import Data.Constraint

-- import Data.Singletons.WrappedSing

-- data NoPair (t :: T) where
--   NoPairTc :: WrappedSing a -> NoPair ('Tc a)
--   NoPairTKey :: NoPair 'TKey
--   NoPairTUnit :: NoPair 'TUnit
--   NoPairTSignature :: NoPair 'TSignature
--   NoPairTChainId :: NoPair 'TChainId
--   NoPairTOption :: WrappedSing a -> NoPair ('TOption a)
--   NoPairTList :: WrappedSing a -> NoPair ('TList a)
--   NoPairTSet :: WrappedSing a -> NoPair ('TSet a)
--   NoPairTOperation :: NoPair 'TOperation
--   NoPairTContract :: WrappedSing a -> NoPair ('TContract a)
--   NoPairTPair :: WrappedSing a -> WrappedSing b -> Void -> NoPair ('TPair a b)
--   NoPairTOr :: WrappedSing a -> WrappedSing b -> NoPair ('TOr a b)
--   NoPairTLambda :: WrappedSing a -> WrappedSing b -> NoPair ('TLambda a b)
--   NoPairTMap :: WrappedSing a -> WrappedSing b -> NoPair ('TMap a b)
--   NoPairTBigMap :: WrappedSing a -> WrappedSing b -> NoPair ('TBigMap a b)

-- data HasPair (t :: T) where
--   HasPair :: Sing a -> Sing b -> HasPair ('TPair a b)

-- hasPair :: Sing t -> Either (NoPair t) (HasPair t)
-- hasPair =
--   \case
--     STc sa -> Left $ NoPairTc (WrapSing sa)
--     STOr sa sb -> Left $ NoPairTOr (WrapSing sa) (WrapSing sb)
--     STKey -> Left $ NoPairTKey
--     STUnit -> Left $ NoPairTUnit
--     STSignature -> Left $ NoPairTSignature
--     STChainId -> Left $ NoPairTChainId
--     STOption sa -> Left $ NoPairTOption (WrapSing sa)
--     STList sa -> Left $ NoPairTList (WrapSing sa)
--     STSet sa -> Left $ NoPairTSet (WrapSing sa)
--     STOperation -> Left $ NoPairTOperation
--     STContract sa -> Left $ NoPairTContract (WrapSing sa)
--     STPair sa sb -> Right $ HasPair sa sb
--     STLambda sa sb -> Left $ NoPairTLambda (WrapSing sa) (WrapSing sb)
--     STMap sa sb -> Left $ NoPairTMap (WrapSing sa) (WrapSing sb)
--     STBigMap sa sb -> Left $ NoPairTBigMap (WrapSing sa) (WrapSing sb)


-- data instance Sing :: forall t_a12LB. NoPair t_a12LB -> Type
--   where
--     SNoPairTc :: forall a_X12LD (n_a12Sh :: WrappedSing a_X12LD).
--                  (Sing (n_a12Sh :: WrappedSing a_X12LD))
--                  -> Sing ('NoPairTc n_a12Sh)
--     SNoPairTKey :: Sing 'NoPairTKey
--     SNoPairTUnit :: Sing 'NoPairTUnit
--     SNoPairTSignature :: Sing 'NoPairTSignature
--     SNoPairTChainId :: Sing 'NoPairTChainId
--     SNoPairTOption :: forall a_X12LE (n_a12Sj :: WrappedSing a_X12LE).
--                       (Sing (n_a12Sj :: WrappedSing a_X12LE))
--                       -> Sing ('NoPairTOption n_a12Sj)
--     SNoPairTList :: forall a_X12LF (n_a12Sl :: WrappedSing a_X12LF).
--                     (Sing (n_a12Sl :: WrappedSing a_X12LF))
--                     -> Sing ('NoPairTList n_a12Sl)
--     SNoPairTSet :: forall a_X12LG (n_a12Sn :: WrappedSing a_X12LG).
--                    (Sing (n_a12Sn :: WrappedSing a_X12LG))
--                    -> Sing ('NoPairTSet n_a12Sn)
--     SNoPairTOperation :: Sing 'NoPairTOperation
--     SNoPairTContract :: forall a_X12LH
--                                (n_a12Sp :: WrappedSing a_X12LH).
--                         (Sing (n_a12Sp :: WrappedSing a_X12LH))
--                         -> Sing ('NoPairTContract n_a12Sp)
--     SNoPairTPair :: forall a_X12LI
--                            b_X12LK
--                            (n_a12Sr :: WrappedSing a_X12LI)
--                            (n_a12Ss :: WrappedSing b_X12LK)
--                            (n_a12St :: Void).
--                     (Sing (n_a12Sr :: WrappedSing a_X12LI))
--                     -> (Sing (n_a12Ss :: WrappedSing b_X12LK))
--                        -> (Sing (n_a12St :: Void))
--                           -> Sing ('NoPairTPair n_a12Sr n_a12Ss n_a12St)
--     SNoPairTOr :: forall a_X12LK
--                          b_X12LM
--                          (n_a12Sx :: WrappedSing a_X12LK)
--                          (n_a12Sy :: WrappedSing b_X12LM).
--                   (Sing (n_a12Sx :: WrappedSing a_X12LK))
--                   -> (Sing (n_a12Sy :: WrappedSing b_X12LM))
--                      -> Sing ('NoPairTOr n_a12Sx n_a12Sy)
--     SNoPairTLambda :: forall a_X12LM
--                              b_X12LO
--                              (n_a12SB :: WrappedSing a_X12LM)
--                              (n_a12SC :: WrappedSing b_X12LO).
--                       (Sing (n_a12SB :: WrappedSing a_X12LM))
--                       -> (Sing (n_a12SC :: WrappedSing b_X12LO))
--                          -> Sing ('NoPairTLambda n_a12SB n_a12SC)
--     SNoPairTMap :: forall a_X12LO
--                           b_X12LQ
--                           (n_a12SF :: WrappedSing a_X12LO)
--                           (n_a12SG :: WrappedSing b_X12LQ).
--                    (Sing (n_a12SF :: WrappedSing a_X12LO))
--                    -> (Sing (n_a12SG :: WrappedSing b_X12LQ))
--                       -> Sing ('NoPairTMap n_a12SF n_a12SG)
--     SNoPairTBigMap :: forall a_X12LQ
--                              b_X12LS
--                              (n_a12SJ :: WrappedSing a_X12LQ)
--                              (n_a12SK :: WrappedSing b_X12LS).
--                       (Sing (n_a12SJ :: WrappedSing a_X12LQ))
--                       -> (Sing (n_a12SK :: WrappedSing b_X12LS))
--                          -> Sing ('NoPairTBigMap n_a12SJ n_a12SK)

-- instance SingKind (NoPair t_a12LB) where
--   type Demote (NoPair t_a12LB) = NoPair t_a12LB
--   fromSing (SNoPairTc b_a12SN) = NoPairTc (fromSing b_a12SN)
--   fromSing SNoPairTKey = NoPairTKey
--   fromSing SNoPairTUnit = NoPairTUnit
--   fromSing SNoPairTSignature = NoPairTSignature
--   fromSing SNoPairTChainId = NoPairTChainId
--   fromSing (SNoPairTOption b_a12SO)
--     = NoPairTOption (fromSing b_a12SO)
--   fromSing (SNoPairTList b_a12SP) = NoPairTList (fromSing b_a12SP)
--   fromSing (SNoPairTSet b_a12SQ) = NoPairTSet (fromSing b_a12SQ)
--   fromSing SNoPairTOperation = NoPairTOperation
--   fromSing (SNoPairTContract b_a12SR)
--     = NoPairTContract (fromSing b_a12SR)
--   fromSing (SNoPairTPair b_a12SS b_a12ST b_a12SU)
--     = ((NoPairTPair (fromSing b_a12SS)) (fromSing b_a12ST))
--         (fromSing b_a12SU)
--   fromSing (SNoPairTOr b_a12SV b_a12SW)
--     = (NoPairTOr (fromSing b_a12SV)) (fromSing b_a12SW)
--   fromSing (SNoPairTLambda b_a12SX b_a12SY)
--     = (NoPairTLambda (fromSing b_a12SX)) (fromSing b_a12SY)
--   fromSing (SNoPairTMap b_a12SZ b_a12T0)
--     = (NoPairTMap (fromSing b_a12SZ)) (fromSing b_a12T0)
--   fromSing (SNoPairTBigMap b_a12T1 b_a12T2)
--     = (NoPairTBigMap (fromSing b_a12T1)) (fromSing b_a12T2)
--   toSing (NoPairTc (b_a12T4 :: Demote (WrappedSing a_X12LD)))
--     = case toSing b_a12T4 :: SomeSing (WrappedSing a_X12LD) of {
--         SomeSing c_a12T5 -> SomeSing (SNoPairTc c_a12T5) }
--   toSing NoPairTKey = SomeSing SNoPairTKey
--   toSing NoPairTUnit = SomeSing SNoPairTUnit
--   toSing NoPairTSignature = SomeSing SNoPairTSignature
--   toSing NoPairTChainId = SomeSing SNoPairTChainId
--   toSing (NoPairTOption (b_a12T6 :: Demote (WrappedSing a_X12LE)))
--     = case toSing b_a12T6 :: SomeSing (WrappedSing a_X12LE) of {
--         SomeSing c_a12T7 -> SomeSing (SNoPairTOption c_a12T7) }
--   toSing (NoPairTList (b_a12T8 :: Demote (WrappedSing a_X12LF)))
--     = case toSing b_a12T8 :: SomeSing (WrappedSing a_X12LF) of {
--         SomeSing c_a12T9 -> SomeSing (SNoPairTList c_a12T9) }
--   toSing (NoPairTSet (b_a12Ta :: Demote (WrappedSing a_X12LG)))
--     = case toSing b_a12Ta :: SomeSing (WrappedSing a_X12LG) of {
--         SomeSing c_a12Tb -> SomeSing (SNoPairTSet c_a12Tb) }
--   toSing NoPairTOperation = SomeSing SNoPairTOperation
--   toSing (NoPairTContract (b_a12Tc :: Demote (WrappedSing a_X12LH)))
--     = case toSing b_a12Tc :: SomeSing (WrappedSing a_X12LH) of {
--         SomeSing c_a12Td -> SomeSing (SNoPairTContract c_a12Td) }
--   toSing
--     (NoPairTPair (b_a12Te :: Demote (WrappedSing a_X12LI))
--                  (b_a12Tf :: Demote (WrappedSing b_X12LK))
--                  (b_a12Tg :: Demote Void))
--     = case
--           (((,,) (toSing b_a12Te :: SomeSing (WrappedSing a_X12LI)))
--              (toSing b_a12Tf :: SomeSing (WrappedSing b_X12LK)))
--             (toSing b_a12Tg :: SomeSing Void)
--       of {
--         (,,) (SomeSing c_a12Th) (SomeSing c_a12Ti) (SomeSing c_a12Tj)
--           -> SomeSing (((SNoPairTPair c_a12Th) c_a12Ti) c_a12Tj) }
--   toSing
--     (NoPairTOr (b_a12Tk :: Demote (WrappedSing a_X12LK))
--                (b_a12Tl :: Demote (WrappedSing b_X12LM)))
--     = case
--           ((,) (toSing b_a12Tk :: SomeSing (WrappedSing a_X12LK)))
--             (toSing b_a12Tl :: SomeSing (WrappedSing b_X12LM))
--       of {
--         (,) (SomeSing c_a12Tm) (SomeSing c_a12Tn)
--           -> SomeSing ((SNoPairTOr c_a12Tm) c_a12Tn) }
--   toSing
--     (NoPairTLambda (b_a12To :: Demote (WrappedSing a_X12LM))
--                    (b_a12Tp :: Demote (WrappedSing b_X12LO)))
--     = case
--           ((,) (toSing b_a12To :: SomeSing (WrappedSing a_X12LM)))
--             (toSing b_a12Tp :: SomeSing (WrappedSing b_X12LO))
--       of {
--         (,) (SomeSing c_a12Tq) (SomeSing c_a12Tr)
--           -> SomeSing ((SNoPairTLambda c_a12Tq) c_a12Tr) }
--   toSing
--     (NoPairTMap (b_a12Ts :: Demote (WrappedSing a_X12LO))
--                 (b_a12Tt :: Demote (WrappedSing b_X12LQ)))
--     = case
--           ((,) (toSing b_a12Ts :: SomeSing (WrappedSing a_X12LO)))
--             (toSing b_a12Tt :: SomeSing (WrappedSing b_X12LQ))
--       of {
--         (,) (SomeSing c_a12Tu) (SomeSing c_a12Tv)
--           -> SomeSing ((SNoPairTMap c_a12Tu) c_a12Tv) }
--   toSing
--     (NoPairTBigMap (b_a12Tw :: Demote (WrappedSing a_X12LQ))
--                    (b_a12Tx :: Demote (WrappedSing b_X12LS)))
--     = case
--           ((,) (toSing b_a12Tw :: SomeSing (WrappedSing a_X12LQ)))
--             (toSing b_a12Tx :: SomeSing (WrappedSing b_X12LS))
--       of {
--         (,) (SomeSing c_a12Ty) (SomeSing c_a12Tz)
--           -> SomeSing ((SNoPairTBigMap c_a12Ty) c_a12Tz) }

-- instance SingI n_a12Sh =>         SingI ('NoPairTc (n_a12Sh :: WrappedSing a_X12LD)) where
--   sing = SNoPairTc sing

-- instance SingI 'NoPairTKey where
--   sing = SNoPairTKey

-- instance SingI 'NoPairTUnit where
--   sing = SNoPairTUnit

-- instance SingI 'NoPairTSignature where
--   sing = SNoPairTSignature

-- instance SingI 'NoPairTChainId where
--   sing = SNoPairTChainId

-- instance SingI n_a12Sj =>         SingI ('NoPairTOption (n_a12Sj :: WrappedSing a_X12LE)) where
--   sing = SNoPairTOption sing

-- instance SingI n_a12Sl =>         SingI ('NoPairTList (n_a12Sl :: WrappedSing a_X12LF)) where
--   sing = SNoPairTList sing

-- instance SingI n_a12Sn =>         SingI ('NoPairTSet (n_a12Sn :: WrappedSing a_X12LG)) where
--   sing = SNoPairTSet sing

-- instance SingI 'NoPairTOperation where
--   sing = SNoPairTOperation

-- instance SingI n_a12Sp =>         SingI ('NoPairTContract (n_a12Sp :: WrappedSing a_X12LH)) where
--   sing = SNoPairTContract sing

-- instance (SingI n_a12Sr, SingI n_a12Ss, SingI n_a12St) =>         SingI ('NoPairTPair (n_a12Sr :: WrappedSing a_X12LI) (n_a12Ss :: WrappedSing b_X12LK) (n_a12St :: Void)) where
--   sing = ((SNoPairTPair sing) sing) sing

-- instance (SingI n_a12Sx, SingI n_a12Sy) =>         SingI ('NoPairTOr (n_a12Sx :: WrappedSing a_X12LK) (n_a12Sy :: WrappedSing b_X12LM)) where
--   sing = (SNoPairTOr sing) sing

-- instance (SingI n_a12SB, SingI n_a12SC) =>         SingI ('NoPairTLambda (n_a12SB :: WrappedSing a_X12LM) (n_a12SC :: WrappedSing b_X12LO)) where
--   sing = (SNoPairTLambda sing) sing

-- instance (SingI n_a12SF, SingI n_a12SG) =>         SingI ('NoPairTMap (n_a12SF :: WrappedSing a_X12LO) (n_a12SG :: WrappedSing b_X12LQ)) where
--   sing = (SNoPairTMap sing) sing

-- instance (SingI n_a12SJ, SingI n_a12SK) =>         SingI ('NoPairTBigMap (n_a12SJ :: WrappedSing a_X12LQ) (n_a12SK :: WrappedSing b_X12LS)) where
--   sing = (SNoPairTBigMap sing) sing




