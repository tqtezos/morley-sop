{-# LANGUAGE NoRebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH.Cases
-- Copyright   :  (C) 2013 Richard Eisenberg; 2020 Michael J. Klein, TQ Tezos
-- License     :  BSD-style (see LICENSE)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A simple modification of `sCases` to allow generating the
-- body with case information: @(`Name` -> [`Name`] -> q `Exp`)@
-- and some utilities.
--
-- @
--  Copyright (c) 2012, Richard Eisenberg
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form must reproduce the above copyright notice,
--  this list of conditions and the following disclaimer in the documentation
--  and/or other materials provided with the distribution.
--
--  3. Neither the name of the author nor the names of its contributors may be
--  used to endorse or promote products derived from this software without
--  specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
--  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
--  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- @
----------------------------------------------------------------------------
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

-- | Utility for `sCases'`
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

-- | Extract the name and number of arguments to a constructor
extractNameArgs :: DCon -> (Name, Int)
extractNameArgs = fmap length . extractNameTypes -- liftSnd = fmap

-- | Extract the name and types of constructor arguments
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
-- | Singletons package
singPkg :: String
singPkg = fromMaybe (error "singPkg name not resolved") (namePackage ''SingI)
-- (LitE . StringL . loc_package) `liftM` location

-- | Make singletons name
mk_name_d :: String -> String -> Name
mk_name_d = mkNameG_d singPkg

-- | `SCons`
sconsName :: Name
sconsName = mk_name_d "Data.Singletons.Prelude.Instances" "SCons"
-- | `SNil`
snilName :: Name
snilName = mk_name_d "Data.Singletons.Prelude.Instances" "SNil"

-- | Tuple singleton `Name`'s
mkTupleDataName :: Int -> Name
mkTupleDataName n = mk_name_d "Data.Singletons.Prelude.Instances" $
                    "STuple" ++ (show n)

-- | Data constructor singleton `Name`'s
singDataConName :: Name -> Name
singDataConName nm
  | nm == '[]                                  = snilName
  | nm == '(:)                                 = sconsName
  | Just degree <- tupleNameDegree_maybe nm        = mkTupleDataName degree
  | Just degree <- unboxedTupleNameDegree_maybe nm = mkTupleDataName degree
  | otherwise                                      = prefixConName "S" "%" nm
------------------------------------------------------------------------

