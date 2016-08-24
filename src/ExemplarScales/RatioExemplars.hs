{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : RatioExemplars
Description : Instance for the ContextualizedConcept class for exemplars on a rational measurement scale
Copyright   : (c) Juergen Hahn, 2016
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : tested
-}
module ExemplarScales.RatioExemplars where

import Data.Function (on)
import qualified Data.Vector.Unboxed as U
import Statistics.Sample.KernelDensity (kde)
import qualified Data.List as List
import ContextualizedConcept
import Concept

instance ContextualizedConcept c Double where
  calculatePrototype =calculatePrototype' . createKDE . extractData

calculatePrototype' :: [(Double,Double)] -> (Double, Double)
calculatePrototype' = List.maximumBy (compare `on` snd)

createKDE :: [Double] -> [(Double,Double)]
createKDE  rawdata =U.toList . uncurry U.zip . kde 64 $ dataVector
  where dataVector= U.fromList rawdata

extractData :: Concept c Double -> [Double]
extractData = map getExemplar . toObservationList

