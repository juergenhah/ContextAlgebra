{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : NominalExemplars
Description : Instance for the ContextualizedConcept class for exemplars on a nominal measurement scale
Copyright   : (c) Juergen Hahn, 2016
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : tested

-}
module ExemplarScales.NominalExemplars where

import qualified Data.MultiSet as Mset
import qualified Data.List as L
import Data.Function


import ContextualizedConcept
import Concept


instance ContextualizedConcept c e where
  calculatePrototype  = L.maximumBy (compare `on` snd ) . rateObservations 

rateObservations :: (Show e,Ord e) =>
                    Concept c e  -- ^ observations representing the contextualized concept
                 -> [(e,Double)] -- ^ contextual typicality for exemplars included in the observations
rateObservations con = map (\(exemplar, occurences) ->
                             (exemplar,fromIntegral occurences /
                                       fromIntegral totalOccurences) )
                       .Mset.toAscOccurList . Mset.fromList . concatMap (\(O (_,e) ,o)-> replicate o e). toOccurenceList $ con
  where totalOccurences = amountExemplars con

