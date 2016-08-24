{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Evaluation.QuickCheckHelper where
import Test.QuickCheck

-- * Helper Functions for QuickCheck
amountTests = 1000

qCheckUnary :: (Arbitrary c, Show c) =>  (c -> Bool) -> IO()
qCheckUnary  = quickCheckWith stdArgs { maxSuccess = amountTests }

vCheckUnary ::  (Arbitrary c, Show c) => (c -> Bool) -> IO()
vCheckUnary  = verboseCheckWith stdArgs { maxSuccess = amountTests }


qCheckBinary :: (Arbitrary c, Show c) => (c -> c -> Bool) -> IO()
qCheckBinary  = quickCheckWith stdArgs { maxSuccess = amountTests }

vCheckBinary ::  (Arbitrary c, Show c) =>(c -> c -> Bool) -> IO()
vCheckBinary  = verboseCheckWith stdArgs { maxSuccess = amountTests } 


qCheckTernary ::  (Arbitrary c, Show c) => (c -> c-> c -> Bool) -> IO()
qCheckTernary  = quickCheckWith stdArgs { maxSuccess = amountTests } 


qCheckUnaryP ::  (Arbitrary c, Show c) => (c -> Property) -> IO()
qCheckUnaryP  = quickCheckWith stdArgs { maxSuccess = amountTests } 

vCheckUnaryP ::  (Arbitrary c, Show c) => (c -> Property) -> IO()
vCheckUnaryP  = verboseCheckWith stdArgs { maxSuccess = amountTests } 

qCheckBinaryP ::  (Arbitrary c, Show c) => (c -> c -> Property) -> IO()
qCheckBinaryP  = quickCheckWith stdArgs { maxSuccess = amountTests } 

vCheckBinaryP ::  (Arbitrary c, Show c) =>(c -> c -> Property) -> IO()
vCheckBinaryP  = verboseCheckWith stdArgs { maxSuccess = amountTests } 


qCheckTernaryP ::  (Arbitrary c, Show c) => (c -> c-> c -> Property) -> IO()
qCheckTernaryP  = quickCheckWith stdArgs { maxSuccess = amountTests } 

vCheckTernaryP :: (Arbitrary c, Show c) => (c -> c-> c -> Property) -> IO()
vCheckTernaryP  = verboseCheckWith stdArgs { maxSuccess = amountTests } 
