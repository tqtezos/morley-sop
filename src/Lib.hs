
module Lib () where

-- import Data.Function

-- import Michelson.Typed.Annotation
-- import Michelson.Typed.Instr
-- import Michelson.Typed.T
-- import Michelson.Typed.Sing
-- import Michelson.Typed.EntryPoints
-- import Michelson.Untyped.Annotation

-- import Data.Singletons


-- data AnnMod = AnnMod
--   { amTypeAnn :: TypeAnn -> TypeAnn
--   , amFieldAnn :: FieldAnn -> FieldAnn
--   }

-- annModFullContract :: AnnMod -> FullContract cp st -> FullContract cp st
-- annModFullContract annMod' (FullContract code' (ParamNotesUnsafe paramNotes') notes') =
--   FullContract
--     (annModContractCode annMod' code')
--     (ParamNotesUnsafe (annModNotes annMod' paramNotes'))
--     (annModNotes annMod' notes')

-- annModNotes :: AnnMod -> Notes t -> Notes t
-- annModNotes am@AnnMod{..} = \case
--   NTc ta                  -> NTc (amTypeAnn ta)
--   NTKey ta                -> NTKey (amTypeAnn ta)
--   NTUnit ta               -> NTUnit (amTypeAnn ta)
--   NTSignature ta          -> NTSignature (amTypeAnn ta)
--   NTChainId ta            -> NTChainId (amTypeAnn ta)
--   NTOption ta nt          -> NTOption (amTypeAnn ta) (annModNotes am nt)
--   NTList ta nt            -> NTList (amTypeAnn ta) (annModNotes am nt)
--   NTSet ta1 ta2           -> NTSet (amTypeAnn ta1) ta2
--   NTOperation ta          -> NTOperation (amTypeAnn ta)
--   NTContract ta nt        -> NTContract (amTypeAnn ta) (annModNotes am nt)
--   NTPair ta fa1 fa2 np nq -> NTPair (amTypeAnn ta) (amFieldAnn fa1) (amFieldAnn fa2) (annModNotes am np) (annModNotes am nq)
--   NTOr ta fa1 fa2 np nq   -> NTOr (amTypeAnn ta) (amFieldAnn fa1) (amFieldAnn fa2) (annModNotes am np) (annModNotes am nq)
--   NTLambda ta np nq       -> NTLambda (amTypeAnn ta) (annModNotes am np) (annModNotes am nq)
--   NTMap ta1 ta2 nv        -> NTMap (amTypeAnn ta1) (amTypeAnn ta2) (annModNotes am nv)
--   NTBigMap ta1 ta2 nv     -> NTBigMap (amTypeAnn ta1) (amTypeAnn ta2) (annModNotes am nv)

-- -- data Instr (inp :: [T]) (out :: [T]) where
-- annModContractCode :: AnnMod -> ContractCode cp st -> ContractCode cp st
-- annModContractCode _ = id

