module Main where
import Test.Framework

import qualified VectorTest
import qualified BeamTest
import qualified ReduceBeamsTest
import qualified RefreshBeamsTest

tests =         VectorTest.tests ++ 
                BeamTest.tests ++ 
                ReduceBeamsTest.tests ++ 
                RefreshBeamsTest.tests
                
main = defaultMain tests
