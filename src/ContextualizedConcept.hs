{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : ContextualizedConcept
Description : Class and instance for a concept and the prototype calculation
Copyright   : (c) Jürgen Hahn, 2016
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : tested

-}
module ContextualizedConcept where

import Concept

class ContextualizedConcept c e where
  calculatePrototype :: (Show e, Ord e) =>
                        Concept c e -- ^  observations representing the contextualized concept
                     -> (e,Double)  -- ^ calculated prototype for the contextualized concept, including the contextual typicality


