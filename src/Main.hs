module Main where

import Criterion.Main
import System.IO

import Complexity.ProtoCalculator
import Complexity.ConceptGenerator

main :: IO()
main = do
 mapM_ makeSafe [stdout, stdin, stderr]
-- creation
 generationBench
 prototypeBench

-- ^ needed because on Manjaro Linux unicode characters abort the execution, the workaround removes the unicode characters
makeSafe h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce -> mkTextEncoding ((takeWhile (/= '/') $ show ce) ++ "//TRANSLIT") >>=
      hSetEncoding h

-- be aware to change numbers in CalcPrototype
prototypeBench = defaultMain
 [bgroup "prototypeCalculation"$  
  map (\c ->bench (label c) $! nfIO (calcProto c)) [1..ctx]
 ]
  where ctx = fromInteger testAmountContexts
        exe = testAmountExemplars
        label c = "meets-"++(show c)++"-contexts-"++(show testAmountContexts)++"-exemplars-"++(show exe)


generationBench = defaultMain
 [bgroup "Concept generation"$
  map (\c -> bench (label c) $ nfIO (createAndStoreConcept c exe)) [12..ctx]
 ]
  where ctx = 12
        exe = 1
        label c =  ("contexts-"++(show c)++"-exemplars-"++(show exe))


