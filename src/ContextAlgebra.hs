{-|
Module      : ContextAlgebra
Description : Classes to build the Context Algebra
Copyright   : (c) Jürgen Hahn, 2016
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : tested

The context algebra is build by the classes: PartialOrder, Universe, Bound, ContextLattice and Complement. For all functions quickCheck tests for algebraic properties are included.
-}
module ContextAlgebra where

import Test.QuickCheck

-- * Classes that build the ContextAlgebra, refer to Figure 4.2
class PartialOrder c where
  -- | partial order relation, refer to Equation 3.1
  isMoreSelectiveOrEqualSel :: c    -- ^ first input context 
                            -> c    -- ^ second input context
                            -> Bool -- ^ true if the first context is more selective or equal selective compared to the second context
                               
  -- | partial order relation, refer to Equation 3.2
  isLessSelectiveOrEqualSel :: c    -- ^ first input context
                            -> c    -- ^ second input context
                            -> Bool -- ^ true if the first context is less selective or equal selective compared to the second contextp

  -- | equal selective relation, refer to Equation 3.3
  equals :: c    -- ^ first input context
         -> c    -- ^ second input context
         -> Bool -- ^ true if the two input context are equal
  equals c1 c2 = c1 `isMoreSelectiveOrEqualSel` c2
                 &&
                 c1 `isLessSelectiveOrEqualSel` c2 

class Universe c where
  -- | includes all atomic contexts
  atomicRepresentation :: [c]
    
class  Bound c where
  -- | response the universal least context, refer to Equation 3.8
  leastContext :: c
  -- | response the universal greatest context, refer to Equation 3.9
  greatestContext :: c 
  
class (PartialOrder c, Bound c, Complement c) => ContextLattice c where
  -- | conjunction of two context, refer to Equation 3.10
  conjunction :: (Bound c, Complement c, PartialOrder c)=>
                 c -- ^ first input context
              -> c -- ^ second input context
              -> c -- ^ resulting in a more selective context than both constituents

  -- | disjunction of two context, refer to Equation 3.11
  disjunction :: (Bound c, Complement c, PartialOrder c)=>
                 c -- ^ first input context
              -> c -- ^ second input context
              -> c -- ^ resulting in a less selective context than both constituents

class Complement c where
  -- | complements the input context, refer to Equation 3.21
  complement :: c -- ^ input context
             -> c -- ^ complement of the input context

-- * Partial order relation tests, refer to Section 3.1.1

-- | Test if two contexts are asymetric, refer to Equation 3.2
-- input restriction, only equal contexts are tested
prop_isAsymetric :: (PartialOrder c, Show c) =>
                    c        -- ^ first context
                 -> c        -- ^ second context 
                 -> Property -- ^ True if the two contexts are asymetric
prop_isAsymetric c1 c2 = (c1 `equals` c2) ==>
                    c1 `isMoreSelectiveOrEqualSel` c2
                    &&
                    c1 `isLessSelectiveOrEqualSel` c2

-- | Test if three contexts are transitive, refer to Equation 3.5
prop_isTransitive :: (PartialOrder c) =>
                     c        -- ^ context one
                  -> c        -- ^ context less selective than context one
                  -> c        -- ^ context less selective than context two
                -> Property -- ^ True if three contexts are transitive
prop_isTransitive c1 c2 c3 = (c1 `isMoreSelectiveOrEqualSel` c2
                              &&
                              c2 `isMoreSelectiveOrEqualSel` c3) ==>
                              c1 `isMoreSelectiveOrEqualSel` c3
              
-- * Context lattice bounds test

-- | Check if leastContext is the least context, refer to Equation 3.8
prop_isLeastContext :: (PartialOrder c,Bound c) =>
                       c    -- ^ less selective context
                    -> Bool -- ^ True if the input context is less selective compared to the leastContext context
prop_isLeastContext c= leastContext `isMoreSelectiveOrEqualSel` c 

-- | Check if greatestContext is the least context, refer to Equation 3.9
prop_isGreatestContext :: (PartialOrder c,Bound c) =>
                          c       -- ^ more selective context
                       -> Bool -- ^ True if the input context is more selective compared to the greatestContext context
prop_isGreatestContext c =  greatestContext `isLessSelectiveOrEqualSel` c

-- * Combination of contexts, refer to Section 3.1.2
-- | idempotent test of two context, refer to Equation 3.12
prop_isIdempotent :: (PartialOrder c, ContextLattice c) =>
                     c            -- ^ input context
                     -> Bool      -- ^ true if the context is idempotent
prop_isIdempotent c =  (c `conjunction` c) `equals` c
                       &&
                       (c `disjunction` c) `equals` c

-- | commutative test  of two contexts, refer to Equation 3.13
prop_isCommutative :: (PartialOrder c, Bound c, Complement c, ContextLattice c) =>
                c           -- ^ input context
                -> c        -- ^ input context
                -> Property -- ^ true if both contexts are commutatice
prop_isCommutative c1 c2 =  classify (c1 `isMoreSelectiveOrEqualSel` c2) "c1 <= c2" $ 
                            classify (c1 `isLessSelectiveOrEqualSel` c2) "c1 >= c2" $ 
                            (c1 `conjunction` c2) `equals` (c2 `conjunction` c1)
                            &&
                            (c1 `disjunction` c2) `equals` (c2 `disjunction` c1)

-- | associative test of three context, refer to Equation 3.14
prop_isAssociative :: (PartialOrder c, Bound c, Complement c, ContextLattice c) =>
                 c           -- ^ input context
                 -> c        -- ^ input context
                 -> c        -- ^ input context
                 -> Property -- ^ True if three contexts are associative
prop_isAssociative c1 c2 c3 = classify (c1 `isMoreSelectiveOrEqualSel` c2) "c1 <= c2" $ 
                              classify (c1 `isLessSelectiveOrEqualSel` c2) "c1 >= c2" $
                              classify (c1 `isMoreSelectiveOrEqualSel` c3) "c1 <= c3" $ 
                              classify (c1 `isLessSelectiveOrEqualSel` c3) "c1 >= c3" $
                              classify (c2 `isMoreSelectiveOrEqualSel` c3) "c2 <= c3" $ 
                              classify (c2 `isLessSelectiveOrEqualSel` c3) "c2 >= c3" $
                              m `equals` n
                              &&
                              o `equals` p
   where m = c1 `conjunction` (c2 `conjunction` c3)
         n = (c1 `conjunction` c2) `conjunction` c3
         o = c1 `disjunction` (c2 `disjunction` c3)
         p = (c1 `disjunction` c2) `disjunction` c3

-- | consistency test of two contexts, refer to Equation 3.15
prop_isConsistent  :: (PartialOrder c, Bound c,  ContextLattice c) =>
                 c           -- ^ less selective context
                 -> c        -- ^ input context
                 -> Property -- ^ True if both context are consistent
prop_isConsistent c1 c2 = c1 `isMoreSelectiveOrEqualSel` c2 ==>
                          (c1 `conjunction` c2 `equals` c1)
                          &&
                          (c1 `disjunction` c2 `equals` c2)

-- | isotone test of three context, refer to Equation 3.16
prop_isIsotone :: (PartialOrder c, Bound c, Complement c, ContextLattice c) =>
             c           -- ^ input context
             -> c        -- ^ input context
             -> c        -- ^ input context
             -> Property -- ^ True if all three context are isotone
prop_isIsotone c1 c2 c3 = c2 `isMoreSelectiveOrEqualSel` c3  ==>
                          classify (c1 `isMoreSelectiveOrEqualSel` c2) "c1 <= c2" $ 
                          classify (c1 `isLessSelectiveOrEqualSel` c2) "c1 >= c2" $
                          classify (c1 `isMoreSelectiveOrEqualSel` c3) "c1 <= c3" $ 
                          classify (c1 `isLessSelectiveOrEqualSel` c3) "c1 >= c3" $
                          ((c1 `disjunction` c2) `isMoreSelectiveOrEqualSel` (c1 `disjunction` c3))
                          &&
                          ((c1 `conjunction` c2) `isMoreSelectiveOrEqualSel` (c1 `conjunction`c3))


-- ** Combination with Bound elements

-- | proerties for combinig with leastContext context, refer to Equation 3.17
prop_withCombLeastContext  ::(PartialOrder c, Bound c, Complement c, ContextLattice c) =>
                  c       -- ^ context one
                  -> Bool -- ^ true if the property is full-filled
prop_withCombLeastContext c =  c `conjunction` leastContext `equals` leastContext
                               &&
                               leastContext `disjunction` c `equals` c

-- | proerties for combinig with leastContext context, refer to Equation 3.18
prop_withCombGreatestContext :: (PartialOrder c, Bound c, Complement c, ContextLattice c) =>
               c       -- ^ context one
               -> Bool -- ^ true if the property is full-filled
prop_withCombGreatestContext c = c `conjunction` greatestContext `equals` c
                                 &&
                                 greatestContext `disjunction` c `equals` greatestContext

-- * Complement tests

-- | test if complement of complement equals the inptu context, refer to Equation 3.22
prop_isDoubleComp :: (PartialOrder c, Complement c) =>
                 c       -- ^ input context
                 -> Bool -- ^ True if the applying the complement twice results in the input context
prop_isDoubleComp c = equals c . complement .complement $ c

-- | test if the complement property holds, refer to Eqaution 3.23
prop_isLessThenComp :: (PartialOrder c, Complement c) =>
                   c           -- ^ input context
                   -> c        -- ^ input context
                   -> Property -- ^ True if the complement holds
prop_isLessThenComp c1 c2=  c1 `isMoreSelectiveOrEqualSel` c2 ==>
                            complement c2 `isMoreSelectiveOrEqualSel` complement c1

-- | complement of greatestContext and leastContext context, refer to Equation 3.24
prop_withCombBoundComp ::(Complement c,ContextLattice c) =>
                     c           -- ^ input context
                     -> Property -- ^ True if property is fulfille
prop_withCombBoundComp c =
  classify (c `isMoreSelectiveOrEqualSel` complement c) "c <= !c" $
  classify (c `isLessSelectiveOrEqualSel` complement c) "c >= !c" $
  (c `conjunction`  complement c `equals` leastContext)
  &&
  (c `disjunction ` complement c `equals` greatestContext)

