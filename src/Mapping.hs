{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Mapping
Description : Implementation of the Mapping class for sets
Copyright   : (c) Jürgen Hahn, 2016
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : tested

-}
module Mapping where

import ContextAlgebra
import Concept


class  Mapping c e where -- datatype e equals this of the lattice datatype
  m :: (Ord c, Ord e, Bound c, PartialOrder c) =>
       c             -- ^ more selective context or equal selective context
    -> Concept c e  -- ^ knowledgebase including facts influenced by context
    -> Concept c e  -- ^ knowledgebase including facts valid for context c
 
instance Mapping c (Concept c e) where
 m ctx concept
  | leastContext `equals` ctx = emptyConcept
  | otherwise =  fromObservationList
                  [obs | obs <- toObservationList concept
                       , getContext obs `isMoreSelectiveOrEqualSel` ctx] 

