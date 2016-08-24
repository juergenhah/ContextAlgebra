{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Complexity.ProtoCalculator  (testAmountContexts,testAmountExemplars, calcProto) where

import System.IO.Unsafe
import qualified Data.Set as Set

import ContextAlgebra
import SetImplementation.ContextAlgebraSetInstance
import Concept
import Mapping
import ContextualizedConcept
import ExemplarScales.NominalExemplars

type CtxType = Context String
type Exemplar = String

-- has to be set
testAmountContexts = 12
testAmountExemplars = 20

-- function to call
calcProto :: Int -> IO (Exemplar, Double)
calcProto amountDisjunctions = do
  concept <-readConcept  testAmountContexts testAmountExemplars
  let extractContext = ordNub $ map getContext $ toObservationList concept
      toCombineContexts = take amountDisjunctions extractContext
      contextualizedCon =  m (automaticDisjunction toCombineContexts) concept
  return $ calculatePrototype contextualizedCon

readConcept testAmountContexts testAmountExemplars = readConceptFile filePath
 where
  filePath = "./src/Benchmark/TestConcepts/Concept-"
             ++(show testAmountContexts)++"-"
             ++(show testAmountExemplars)++".txt"

readConceptFile :: FilePath -> IO (Concept CtxType Exemplar)
readConceptFile filename = do
  file<-readFile filename
  let parsedConcept = read file
  return parsedConcept

automaticDisjunction ::  [CtxType] -> CtxType
automaticDisjunction = foldl Set.union leastContext -- changed

instance Universe String where
 atomicRepresentation = map (Set.elemAt 0) extractAtomicRepresentation

extractAtomicRepresentation = filter ((==) 1 . length)  allContexts
 where concept = unsafePerformIO $ readConcept  testAmountContexts testAmountExemplars
       allContexts = ordNub . map getContext . toObservationList $ concept

instance Mapping CtxType Exemplar where
 m ctx concept
  | leastContext `equals` ctx = emptyConcept
  | otherwise =  fromObservationList
                  [obs | obs <- toObservationList concept
                       , getContext obs `isMoreSelectiveOrEqualSel` ctx] 

-- Taken From Yi
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
