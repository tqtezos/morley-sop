{-# LANGUAGE NoRebindableSyntax #-}

module Data.Singletons.TH.Cases where

import Prelude (otherwise, error)
import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Eq
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.String
import Data.Tuple
import Text.Show

import Data.Singletons

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Desugar

-- | Example:
--
-- @
--  foo = $(sCasesFold f ''Either)
--  foo = \x ->
--    case x of
--      Left y -> $(qFunc) y $(qBase)
--      Right y -> $(qFunc) y $(qBase)
-- @
sCasesFold :: ExpQ -> ExpQ -> Name -> ExpQ
sCasesFold qFunc qBase tyName = do
  xName <- newName "x"
  [| \ $(varP xName) ->
    $(sCases' tyName (varE xName) (\ _conName ->
      foldr
        (\fieldName memo ->
          [| $(qFunc) $(varE fieldName) $(memo) |]
        )
        qBase
      )
    )
    |]

-- | The function 'sCases' generates a case expression where each right-hand side
-- is identical. This may be useful if the type-checker requires knowledge of which
-- constructor is used to satisfy equality or type-class constraints, but where
-- each constructor is treated the same. For 'sCases', unlike 'cases', the
-- scrutinee is a singleton. But make sure to pass in the name of the /original/
-- datatype, preferring @''Maybe@ over @''SMaybe@.
sCases' :: DsMonad q
       => Name        -- ^ The head of the type the scrutinee's type is based on.
                      -- (Like @''Maybe@ or @''Bool@.)
       -> q Exp       -- ^ The scrutinee, in a Template Haskell quote
       -> (Name -> [Name] -> q Exp)  -- body
       -> q Exp
sCases' tyName expq bodyq = do
  dinfo <- dsReify tyName
  case dinfo of
    Just (DTyConI (DDataD _ _ _ _ _ ctors _) _) ->
      let ctor_stuff = map (first singDataConName . extractNameArgs) ctors in
      expToTH <$> buildCases ctor_stuff expq bodyq
    Just _ ->
      fail $ "Using <<cases>> with something other than a type constructor: "
              ++ (show tyName)
    _ -> fail $ "Cannot find " ++ show tyName

buildCases :: forall m. DsMonad m
           => [(Name, Int)]
           -> m Exp  -- scrutinee
           -> (Name -> [Name] -> m Exp)  -- body
           -> m DExp
buildCases ctor_infos expq bodyq =
  DCaseE <$>
  (dsExp =<< expq) <*>
  forM ctor_infos (\con -> do
    (pat', fieldVars) <- conToPat con
    DMatch pat' <$> (dsExp =<< bodyq (fst con) fieldVars)
  )
  where
    conToPat :: (Name, Int) -> m (DPat, [Name])
    conToPat (name, num_fields) = do
      fieldVars <- replicateM num_fields (qNewName "x")
      return (DConPa name (DVarPa <$> fieldVars), fieldVars)


-- Data.Singletons.Util
------------------------------------------------------------------------
tysOfConFields :: DConFields -> [DType]
tysOfConFields (DNormalC _ stys) = map snd stys
tysOfConFields (DRecC vstys)   = map (\(_,_,ty) -> ty) vstys

-- extract the name and number of arguments to a constructor
extractNameArgs :: DCon -> (Name, Int)
extractNameArgs = fmap length . extractNameTypes -- liftSnd = fmap

-- extract the name and types of constructor arguments
extractNameTypes :: DCon -> (Name, [DType])
extractNameTypes (DCon _ _ n fields _) = (n, tysOfConFields fields)

-- Put an uppercase prefix on a constructor name. Takes two prefixes:
-- one for identifiers and one for symbols.
--
-- This is different from 'prefixName' in that infix constructor names always
-- start with a colon, so we must insert the prefix after the colon in order
-- for the new name to be syntactically valid.
prefixConName :: String -> String -> Name -> Name
prefixConName pre tyPre n = case (nameBase n) of
    (':' : rest) -> mkName (':' : tyPre ++ rest)
    alpha -> mkName (pre ++ alpha)
------------------------------------------------------------------------


-- Data.Singletons.Names
------------------------------------------------------------------------
singPkg :: String
singPkg = fromMaybe (error "singPkg name not resolved") (namePackage ''SingI)
-- (LitE . StringL . loc_package) `liftM` location

mk_name_d :: String -> String -> Name
mk_name_d = mkNameG_d singPkg

sconsName = mk_name_d "Data.Singletons.Prelude.Instances" "SCons"
snilName = mk_name_d "Data.Singletons.Prelude.Instances" "SNil"

mkTupleDataName :: Int -> Name
mkTupleDataName n = mk_name_d "Data.Singletons.Prelude.Instances" $
                    "STuple" ++ (show n)

singDataConName :: Name -> Name
singDataConName nm
  | nm == '[]                                  = snilName
  | nm == '(:)                                 = sconsName
  | Just degree <- tupleNameDegree_maybe nm        = mkTupleDataName degree
  | Just degree <- unboxedTupleNameDegree_maybe nm = mkTupleDataName degree
  | otherwise                                      = prefixConName "S" "%" nm
------------------------------------------------------------------------

