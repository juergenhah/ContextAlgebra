{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Complexity.ConceptGenerator (createAndStoreConcept) where

import Test.QuickCheck
import Data.List (permutations)
import qualified Data.Set as Set

import ContextAlgebra
import Concept
import SetImplementation.ContextAlgebraSetInstance

type CtxType = Context String 
type Exemplar = String 

maxAmountContextsForArbitrary = 500
maxAmountExemplarsForArbitrary = 500

createAndStoreConcept :: Int -> Int -> IO ()
createAndStoreConcept amountContexts amountExemplars = do
   exemplars <- getAmountElements getrandomExemplar amountExemplars
   atomicContexts <- getAmountElements getrandomContext amountContexts
   let allContextCombin = establishContextLattice atomicContexts
   concepts <- sequence $! map (genConcept4Ctx exemplars) allContextCombin
   let concept = unionConcept concepts
       filepath = "./src/Benchmark/TestConcepts/Concept-"
                  ++(show amountContexts)++"-"
                  ++(show amountExemplars)++".txt"
   writeConceptFile filepath concept

getAmountElements ::(Ord a)=> (IO a)-> Int -> IO [a]
getAmountElements f amount =  do
 elements <- sequence $! replicate (toAbsAndZeroToOne amount) f
 let a = Set.fromList elements
 if Set.size a == amount then return elements -- check if all elements are distinct 
   else getAmountElements f amount

getrandomContext =generate (arbitrary::Gen CtxType)
getrandomExemplar =generate (arbitrary::Gen Exemplar)
getrandomInt = generate ( arbitrary::Gen Int)

establishContextLattice :: [CtxType] -> [CtxType]
establishContextLattice atomicRep = ordNub createdContexts
 where
  disjunctions start ctxlist = scanl Set.union start ctxlist
  permutateAtomics = permutations atomicRep
  createdContexts =  concatMap (\v -> disjunctions (head v) (tail v)) permutateAtomics

genConcept4Ctx :: [Exemplar] -> CtxType  -> IO (Concept CtxType Exemplar)
genConcept4Ctx  exemplars ctx  = do
  amounts <- getAmountElements getrandomInt (length exemplars)
  let concepts = zipWith (\e a ->fromObservationList
                          $! replicate (toAbsAndZeroToOne a) (O (ctx,e)))
                          exemplars amounts
  return $! unionConcept concepts

writeConceptFile :: FilePath -> Concept CtxType Exemplar -> IO()
writeConceptFile filename concept = writeFile filename (show concept)

instance {-# OVERLAPS #-} Arbitrary CtxType where
  arbitrary = elements $ take maxAmountContextsForArbitrary generateContextsUniverse

generateContextsUniverse:: [CtxType]
generateContextsUniverse  = map createContext $ generateString  "c_"

instance {-# OVERLAPS #-} Arbitrary Exemplar where
  arbitrary = elements $ take maxAmountExemplarsForArbitrary generateExemplars

generateExemplars ::   [String]
generateExemplars  = generateString "e_"

generateString:: String -> [String]
generateString prefix =  [x++[a] | x <- prefix : strings, a <- ['a'..'z']]
 where strings = [x++[a] | x <- prefix : strings, a <- ['a'..'z']]                 

-- Taken From Yi
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

toAbsAndZeroToOne :: Int -> Int
toAbsAndZeroToOne a = if a == 0 then 1 else abs a
        


  
