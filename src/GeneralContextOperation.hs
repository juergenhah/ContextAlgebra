{-# LANGUAGE ScopedTypeVariables,TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module GeneralContextOperation where

import ContextAlgebra
import Concept
import Mapping
import SetImplementation.ContextAlgebraSetInstance
import ContextualizedConcept
import ExemplarScales.NominalExemplars

generalContextOperation :: (Ord c, Ord e, Bound c, PartialOrder c, ContextLattice c, Mapping c e) => c
                        ->  c
                        -> Concept c e
                        -> Concept c e
generalContextOperation actualCtx newCtx = m moreSelCtx
 where moreSelCtx =  actualCtx `conjunction` newCtx

a1Name = "(in Vienna) and (serving pizza)"
a2Name = "(in Vienna) and not (serving pizza)"
a3Name = "not(in Vienna) and (serving pizza) "

a1 = createContext a1Name
a2 = createContext a2Name
a3 = createContext a3Name

inVienna = a1 `disjunction` a2
servingPizza =  a1 `disjunction` a3
inViennaOrServingPizza = a2 `disjunction` a3

instance Complement String

instance Bound String 

instance Universe (String) where
  atomicRepresentation = [a1Name
                         ,a2Name
                         ,a3Name]

instance Mapping (Context String) String where
  m ctx entity
   | leastContext `equals` ctx = emptyConcept
   | otherwise =  fromObservationList
                  [obs | obs <- toObservationList entity
                       , getContext obs `isMoreSelectiveOrEqualSel` ctx] 


pizzaRestV1 = O (a1, "pizzaRestaurantVienna1")
pizzaRestV2 = O (a1, "pizzaRestaurantVienna2")

regionalRestV1 = O (a2, "regionalRestaurantVienna1")
regionalRestV2 = O (a2, "regionalRestaurantVienna2")

pizzaRestRome1 = O (a3, "pizzaRestaurantRome1")
pizzaRestRome2 = O (a3, "pizzaRestaurantRome2")

plachutta1 = O (a1, "Plachutta")
plachuttaV = replicate 5 $ O (a2, "Plachutta")

pizzahut = replicate 11 $ O (a3, "Pizza Hut")

candinetta = replicate 3 $ O (a1, "Candinetta")

vapianoV = replicate 5 $ O (a1, "Vapiano")
vapianoP = replicate 5 $ O (a3, "Vapiano")

restaurant = unionConcept . map createConcept $ obs
 where obs = [plachutta1]++plachuttaV++pizzahut++candinetta++vapianoV++vapianoP

prototypeRestaurants = calculatePrototype restaurant
restsInVienna = generalContextOperation greatestContext inVienna restaurant
prototypeInVienna = calculatePrototype restsInVienna
restsInViennaServingPizza = generalContextOperation inVienna  a1 restsInVienna
protoInViennaServingPizza = calculatePrototype restsInViennaServingPizza




