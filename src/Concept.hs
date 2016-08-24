{-|
Module      : Concept
Description : Classes to build the Context Algebra
Copyright   : (c) Juergen Hahn, 2016
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : tested
-}

module Concept where

import qualified Data.MultiSet as Mset
import ContextAlgebra

data Observation c e = O (c,e) deriving (Show,Read,Eq,Ord)

getContext :: Observation c e ->  c
getContext (O (c,_))= c

getExemplar :: Observation c e -> e
getExemplar (O (_,e))= e

data Concept c e= C (Mset.MultiSet (Observation c e)) deriving (Show,Read)

createConcept :: Observation c e -> Concept c e
createConcept = C . Mset.singleton

addConcept :: (Ord c,Ord e)=>Concept c e -> Concept c e -> Concept c e
addConcept (C a) (C b) = C$ Mset.union  a  b

isNullConcept :: Concept c e -> Bool
isNullConcept (C e) = Mset.null e

emptyConcept :: Concept c e
emptyConcept = C Mset.empty

unionConcept:: (Ord c,Ord e)=>[Concept c e] -> Concept c e
unionConcept = foldl addConcept emptyConcept

toObservationList :: Concept c e -> [Observation c e]
toObservationList (C e) = Mset.toList e

fromObservationList :: (Ord c,Ord e) => [Observation c e] -> Concept c e
fromObservationList = C . Mset.fromList

toOccurenceList :: Concept c e -> [(Observation c e ,Int)]
toOccurenceList (C obs)= Mset.toOccurList obs

amountExemplars :: Concept c e -> Int
amountExemplars (C obs) = Mset.size obs
