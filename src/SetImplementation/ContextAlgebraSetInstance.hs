{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : ContextAlgebraSetInstance
Description : Instance for the ContextAlgebra using Sets
Copyright   : (c) Jürgen Hahn, 2016
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : tested

-}
module SetImplementation.ContextAlgebraSetInstance where

import qualified Data.Set as Set

import ContextAlgebra

type Context c = Set.Set c

createContext :: c
              -> Context c
createContext  = Set.singleton

instance  (Ord c) => PartialOrder (Context c) where
  isMoreSelectiveOrEqualSel = Set.isSubsetOf
  
  isLessSelectiveOrEqualSel c1 c2
   | c2 == c1 = True -- needed for the equal case
   | otherwise = not $ c1 `isMoreSelectiveOrEqualSel` c2

instance (Universe c, Ord c) => Bound (Context c) where
  greatestContext = Set.unions . map createContext $ atomicRepresentation
  
  leastContext =emptyContext
    where emptyContext = Set.empty

instance (Universe c, Ord c)=> Complement (Context c) where
  complement = Set.difference greatestContext

instance (Universe c, Complement c, Ord c)=> ContextLattice (Context c) where
  disjunction c1 c2
   | c1 `equals` leastContext = c2                 -- ^ Equation 3.17
   | c2 `equals` leastContext = c1                 -- ^ Equation 3.17
   | c1 `equals` greatestContext = greatestContext -- ^ Equation 3.18
   | c2 `equals` greatestContext = greatestContext -- ^ Equation 3.18
   | c1 `equals` c2 = c1                           -- ^ Equation 3.12
   | complement c1 `equals` c2 = greatestContext   -- ^ Equation 3.24
   | complement c2 `equals` c1 = greatestContext   -- ^ Equation 3.24 
   | c1 `isMoreSelectiveOrEqualSel` c2 = c2        -- ^ Equation 3.15
   | otherwise =  c1 `Set.union` c2                -- ^ Equation 3.20

  conjunction c1 c2
   | c1 `equals` leastContext = leastContext       -- ^ Equation 3.17
   | c2 `equals` leastContext = leastContext       -- ^ Equation 3.17
   | c1 `equals` greatestContext = c2              -- ^ Equation 3.18
   | c2 `equals` greatestContext = c1              -- ^ Equation 3.18 
   | c1 `equals` c2 = c1                           -- ^ Equation 3.12
   | complement c1 `equals` c2 = leastContext      -- ^ Equation 3.24
   | complement c2 `equals` c1 = leastContext      -- ^ Equation 3.24  
   | c1 `isMoreSelectiveOrEqualSel` c2 = c1        -- ^ Equation 3.15
   | otherwise = c1 `Set.intersection` c2          -- ^ Equation 3.19
