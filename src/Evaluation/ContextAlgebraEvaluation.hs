{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : ContextAlgebraEvaluation
Description : Evaluation for the set implementation with three atomic representations
Copyright   : (c) Juergen Hahn, 2016
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : tested
-}
module Evaluation.ContextAlgebraEvaluation where

import Test.QuickCheck

import ContextAlgebra
import Evaluation.QuickCheckHelper
import SetImplementation.ContextAlgebraSetInstance
import GeneralContextOperation

type CtxType = Context String

-- * Function to generate all possible Inputs for quickCheck
instance Arbitrary CtxType where
  -- | input contexts for the quickCheck tests
  arbitrary = elements [leastContext
                       ,greatestContext
                       ,a1
                       ,a2
                       ,a3
                       ,inVienna
                       ,servingPizza
                       ,inViennaOrServingPizza]

main = sequence_ [putStrLn "is asymetric property:"
                 ,proofisAsymetric
                 ,putStrLn "is transitive property:"
                 ,proofisTransitive
                 ,putStrLn "is least context property:"
                 ,proofisLeastContext
                 ,putStrLn "is greatest context property:"
                 ,proofisGreatestContext
                 ,putStrLn "is idempotent property:"
                 ,proofisIdempotent
                 ,putStrLn "is commutative property:"
                 ,proofisCommutative
                 ,putStrLn "is associative property:"                  
                 ,proofisAssociative
                 ,putStrLn "is consistent property:"                  
                 ,proofisConsistent
                 ,putStrLn "is isotone property:"                  
                 ,proofisIsotone
                 ,putStrLn "proof combination with least context:"                  
                 ,proofwithCombLeastContext
                 ,putStrLn "proof combination with greatest context:"                  
                 ,proofwithCombGreatestContext
                 ,putStrLn "proof double complement property:"                  
                 ,proofisDoubleComp
                 ,putStrLn "proof less then complement property:"                  
                 ,proofisLessThenComp
                 ,putStrLn "proof combination with complement:"                  
                 ,proofwithCombBoundComp]

-- * QuickCheck tests for set implementation
-- | quickCheck executable for asymetric property assert, without output to stdoutput
proofisAsymetric = qCheckBinaryP
                  (prop_isAsymetric ::  CtxType-> CtxType-> Property)
-- | quickCheck executable for transitive property test, without output to stdoutput
proofisTransitive = qCheckTernaryP (prop_isTransitive :: CtxType-> CtxType-> CtxType->Property)
-- | quickCheck executable for least context property test, without output to stdoutput
proofisLeastContext = qCheckUnary (prop_isLeastContext:: CtxType-> Bool)
-- | quickCheck executable for greatest context property test, without output to stdoutput
proofisGreatestContext = qCheckUnary (prop_isGreatestContext :: CtxType-> Bool)
-- | quickCheck executable for idempotent property test, without output to stdoutput
proofisIdempotent = qCheckUnary (prop_isIdempotent :: CtxType-> Bool)
-- | quickCheck executable for commutative property test, without output to stdoutput
proofisCommutative = qCheckBinaryP (prop_isCommutative :: CtxType-> CtxType-> Property)
-- | quickCheck executable for associative property test, without output to stdoutput
proofisAssociative = qCheckTernaryP (prop_isAssociative :: CtxType-> CtxType-> CtxType-> Property)
-- | quickCheck executable for consistent property test, without output to stdoutput
proofisConsistent = qCheckBinaryP (prop_isConsistent :: CtxType-> CtxType-> Property)
-- | quickCheck executable for isotone property test, without output to stdoutput
proofisIsotone = qCheckTernaryP (prop_isIsotone :: CtxType-> CtxType-> CtxType-> Property)
-- | quickCheck executable for combination with leastContext context, without output to stdoutput
proofwithCombLeastContext = qCheckUnary (prop_withCombLeastContext:: CtxType-> Bool)
-- | quickCheck executable for combination with greatestContext context, without output to stdoutput
proofwithCombGreatestContext = qCheckUnary (prop_withCombGreatestContext:: CtxType-> Bool)
-- | quickCheck executable for double complement test, without output to stdoutput
proofisDoubleComp = qCheckUnary (prop_isDoubleComp:: CtxType-> Bool)
-- | quickCheck executable for complement test, without output to stdoutput
proofisLessThenComp = qCheckBinaryP (prop_isLessThenComp :: CtxType-> CtxType-> Property)
-- | quickCheck executable for complement test with leastContext and greatestContext context , without output to stdoutput
proofwithCombBoundComp = qCheckUnaryP (prop_withCombBoundComp:: CtxType-> Property)



-- | quickCheck executable for asymetric property test, including output to stdoutput
proofisAsymetricVerbose = vCheckBinaryP (prop_isAsymetric :: CtxType-> CtxType-> Property)
-- | quickCheck executable for transitive property test, including output to stdoutput
proofisTransitiveVerbose =vCheckTernaryP (prop_isTransitive :: CtxType-> CtxType-> CtxType-> Property)
-- | quickCheck executable for least context property test, including output to stdoutput
proofisLeastContextVerbose = vCheckUnary (prop_isLeastContext:: CtxType-> Bool)

-- | quickCheck executable for greatest context property test, including output to stdoutput
proofisGreatestContextVerbose = vCheckUnary (prop_isGreatestContext :: CtxType-> Bool)

-- | quickCheck executable for idempotent property test, including output to stdoutput
proofIdemPropertyVerbose = vCheckUnary (prop_isIdempotent  :: CtxType-> Bool)


-- | quickCheck executable for commutative property test, including output to stdoutput
proofisCommutativeVerbose = vCheckBinaryP (prop_isCommutative :: CtxType-> CtxType-> Property)

-- | quickCheck executable for consistent property test, including output to stdoutput
proofisConsistentVerbose = vCheckBinaryP (prop_isConsistent :: CtxType-> CtxType-> Property)

-- | quickCheck executable for associative property test, including output to stdoutput
proofisAssociativeVerbose = vCheckTernaryP (prop_isAssociative :: CtxType-> CtxType-> CtxType-> Property)

-- | quickCheck executable for isotone property test, including output to stdoutput
proofisIsotoneVerbose = vCheckTernaryP (prop_isIsotone :: CtxType-> CtxType-> CtxType-> Property)


-- | quickCheck executable for combination with leastContext context, including output to stdoutput
proofwithCombLeastContextVerbose = vCheckUnary (prop_withCombLeastContext:: CtxType-> Bool)

-- | quickCheck executable for combination with greatestContext context, including output to stdoutput
proofwithCombGreatestContextVerbose = vCheckUnary (prop_withCombGreatestContext:: CtxType-> Bool)

-- | quickCheck executable for double complement test, including output to stdoutput
proofisDoubleCompVerbose = vCheckUnary (prop_isDoubleComp:: CtxType-> Bool)


-- | quickCheck executable for complement test, including output to stdoutput
proofisLessThenCompVerbose = vCheckBinaryP (prop_isLessThenComp :: CtxType-> CtxType-> Property)

-- | quickCheck executable for complement test with leastContext and greatestContext context , including output to stdoutput
proofwithCombBoundCompVerbose = vCheckUnaryP (prop_withCombBoundComp:: CtxType-> Property)

