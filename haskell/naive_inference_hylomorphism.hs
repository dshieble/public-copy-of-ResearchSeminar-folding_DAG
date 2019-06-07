{--
runhaskell naive_inference_hylomorphism

See NaiveInferenceHylomorphism for details
--}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import qualified Data.List as List
import Dag
import Utilities
import BayesNetUtilities
import JointDistribution
import Control.Exception
import NaiveInferenceHylomorphism

dummyTable :: JD String
dummyTable = buildProbabilityTable (\x -> 1) dummyBNCpt1 dummyBN

exampleTable0 :: JD String
exampleTable0 = buildProbabilityTable (\x -> 1) exampleBN0Cpt1 exampleBN0

exampleTable1 :: JD String
exampleTable1 = buildProbabilityTable (\x -> 1) exampleBN1Cpt1 exampleBN1

-- 0.1
dummyTableInference :: Double
dummyTableInference = inferenceJd (Map.fromList [("C",1)]) Map.empty dummyTable

-- 0.1
exampleTable1InferenceA :: Double
exampleTable1InferenceA = inferenceJd (Map.fromList [("A",1)]) Map.empty exampleTable1

-- 0.3*0.9*0.8 + 0.2*0.9*0.2 + 0.6*0.1*0.8 + 0.1*0.1*0.2 = 0.302
exampleTable1InferenceC :: Double
exampleTable1InferenceC = inferenceJd (Map.fromList [("C",1)]) Map.empty exampleTable1


-- P(C | A, B)*P(A)*P(B) + P(C | A, -B)*P(A)*P(-B) = 0.1*0.1*0.2 + 0.6*0.1*0.8 = 0.05
exampleTable1InferenceAC :: Double
exampleTable1InferenceAC = inferenceJd (Map.fromList [("A",1), ("C",1)]) Map.empty exampleTable1

-- P(C | A) = P(C | A, B')P(B') + P(C | A, B)P(B) = 0.6*0.8 + 0.1*0.2 = 0.5
exampleTable1InferenceC_A :: Double
exampleTable1InferenceC_A = inferenceJd (Map.fromList [("C",1)]) (Map.fromList [("A",1)])  exampleTable1


-- P(C | A) = P(C | A, B')P(B') + P(C | A, B)P(B) = 0.6*0.8 + 0.1*0.2 = 0.5
exampleBN1Cpt1InferenceC_A :: Double
exampleBN1Cpt1InferenceC_A = naiveBayesianInference (\x -> 1) (Map.fromList [("C",1)]) (Map.fromList [("A",1)]) exampleBN1Cpt1 exampleBN1

------------------------------------------------------------------------------

main = do
  putStrLn "\n(printJd dummyTable)"
  putStrLn (printJd dummyTable)
  putStrLn ("sum dummyTable: " ++ show (sumJd dummyTable))
  putStrLn ""
  putStrLn "\n(printJd exampleTable0)"
  putStrLn (printJd exampleTable0)
  putStrLn ("sum exampleTable0: " ++ show (sumJd exampleTable0))
  putStrLn ""
  putStrLn "\n(printJd exampleTable1)"
  putStrLn (printJd exampleTable1)
  putStrLn ("sum exampleTable1: " ++ show (sumJd exampleTable1))
  putStrLn ("exampleTable1LookedUp1 (E: True | D: True | C: True | B: False | A: True): " ++ (show (lookupJd exampleBN1Assignment1 exampleTable1)))
  putStrLn ""
  putStrLn ("dummyTableInference (0.1): " ++ (show dummyTableInference))
  putStrLn ("exampleTable1InferenceA (0.1): " ++ (show exampleTable1InferenceA))
  putStrLn ("exampleTable1InferenceC (0.302): " ++ (show exampleTable1InferenceC))
  putStrLn ("exampleTable1InferenceAC (0.05): " ++ (show exampleTable1InferenceAC))
  putStrLn ("exampleTable1InferenceC_A (0.5): " ++ (show exampleTable1InferenceC_A))
  putStrLn ("exampleBN1Cpt1InferenceC_A (0.5): " ++ (show exampleBN1Cpt1InferenceC_A))
  putStrLn "done"





