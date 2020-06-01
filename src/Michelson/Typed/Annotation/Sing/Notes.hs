{-# OPTIONS -Wno-missing-export-lists #-}

{-# OPTIONS -fmax-pmcheck-iterations=20000000 #-} -- pattern-match checking this module takes extra-long

module Michelson.Typed.Annotation.Sing.Notes where

import Prelude

import Michelson.Typed.Annotation
import Michelson.Untyped.Annotation

import Michelson.Typed.Annotation.Sing


-- | Convert to `Notes`
annotatedToNotes :: Annotated Text t -> Notes t
annotatedToNotes (ATc ta) = NTc (AnnotationUnsafe ta)
annotatedToNotes (ATKey ta) = NTKey (AnnotationUnsafe ta)
annotatedToNotes (ATUnit ta) = NTUnit (AnnotationUnsafe ta)
annotatedToNotes (ATSignature ta) = NTSignature (AnnotationUnsafe ta)
annotatedToNotes (ATChainId ta) = NTChainId (AnnotationUnsafe ta)
annotatedToNotes (ATOption ta xs) = NTOption (AnnotationUnsafe ta) (annotatedToNotes xs)
annotatedToNotes (ATList ta xs) = NTList (AnnotationUnsafe ta) (annotatedToNotes xs)
annotatedToNotes (ATSet ta tb) = NTSet (AnnotationUnsafe ta) (AnnotationUnsafe tb)
annotatedToNotes (ATOperation ta) = NTOperation (AnnotationUnsafe ta)
annotatedToNotes (ATContract ta xs) = NTContract (AnnotationUnsafe ta) (annotatedToNotes xs)
annotatedToNotes (ATPair ta tb tc xs ys) = NTPair (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) (annotatedToNotes xs) (annotatedToNotes ys)
annotatedToNotes (ATOr ta tb tc xs ys) = NTOr (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) (annotatedToNotes xs) (annotatedToNotes ys)
annotatedToNotes (ATLambda ta xs ys) = NTLambda (AnnotationUnsafe ta) (annotatedToNotes xs) (annotatedToNotes ys)
annotatedToNotes (ATMap ta tb xs) = NTMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) (annotatedToNotes xs)
annotatedToNotes (ATBigMap ta tb xs) = NTBigMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) (annotatedToNotes xs)

-- | Convert from `Notes`
annotatedFromNotes :: Notes t -> Annotated Text t
annotatedFromNotes (NTc (AnnotationUnsafe ta)) = ATc ta
annotatedFromNotes (NTKey (AnnotationUnsafe ta)) = ATKey ta
annotatedFromNotes (NTUnit (AnnotationUnsafe ta)) = ATUnit ta
annotatedFromNotes (NTSignature (AnnotationUnsafe ta)) = ATSignature ta
annotatedFromNotes (NTChainId (AnnotationUnsafe ta)) = ATChainId ta
annotatedFromNotes (NTOption (AnnotationUnsafe ta) xs) = ATOption ta (annotatedFromNotes xs)
annotatedFromNotes (NTList (AnnotationUnsafe ta) xs) = ATList ta (annotatedFromNotes xs)
annotatedFromNotes (NTSet (AnnotationUnsafe ta) (AnnotationUnsafe tb)) = ATSet ta tb
annotatedFromNotes (NTOperation (AnnotationUnsafe ta)) = ATOperation ta
annotatedFromNotes (NTContract (AnnotationUnsafe ta) xs) = ATContract ta (annotatedFromNotes xs)
annotatedFromNotes (NTPair (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) xs ys) = ATPair ta tb tc (annotatedFromNotes xs) (annotatedFromNotes ys)
annotatedFromNotes (NTOr (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) xs ys) = ATOr ta tb tc (annotatedFromNotes xs) (annotatedFromNotes ys)
annotatedFromNotes (NTLambda (AnnotationUnsafe ta) xs ys) = ATLambda ta (annotatedFromNotes xs) (annotatedFromNotes ys)
annotatedFromNotes (NTMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) xs) = ATMap ta tb (annotatedFromNotes xs)
annotatedFromNotes (NTBigMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) xs) = ATBigMap ta tb (annotatedFromNotes xs)

-- | Use @`Eq` (`Notes` t)@
instance Eq (Annotated Text t) where
  xs == ys = annotatedToNotes xs == annotatedToNotes ys

