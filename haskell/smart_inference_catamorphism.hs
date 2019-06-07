{--
runhaskell smart_inference_catamorphism.hs

See SmartInferenceCatamorphism.hs for details
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
import NaiveInferenceHylomorphism
import SmartInferenceCatamorphism

--------------------------------------- Examples -----------------------------------

-- 0.1
inference_dummy_C = topDownInference (\x -> 1) (Map.fromList [("C",1)]) Map.empty dummyBNCpt1 dummyBN

-- 0.1
inference_BN0Cpt1_C = topDownInference (\x -> 1) (Map.fromList [("C",1)]) Map.empty exampleBN0Cpt1 exampleBN0

-- 0.3*0.1 + 0.2*0.9 = 0.21
inference_BN0Cpt1_D  = topDownInference (\x -> 1) (Map.fromList [("D",1)]) Map.empty exampleBN0Cpt1 exampleBN0

-- 0.3
inference_BN0Cpt1_D_C = topDownInference (\x -> 1) (Map.fromList [("D",1)]) (Map.fromList [("C",1)]) exampleBN0Cpt1 exampleBN0

-- 0.1
inference_BN1Cpt1_A = topDownInference (\x -> 1) (Map.fromList [("A",1)]) Map.empty exampleBN1Cpt1 exampleBN1

-- 0.2
inference_BN1Cpt1_B = topDownInference (\x -> 1) (Map.fromList [("B",1)]) Map.empty exampleBN1Cpt1 exampleBN1

-- P(C | A) = P(C | A, B')P(B') + P(C | A, B)P(B) = 0.6*0.8 + 0.1*0.2 = 0.5
inference_BN1Cpt1_C_A = topDownInference (\x -> 1) (Map.fromList [("C",1)]) (Map.fromList [("A",1)]) exampleBN1Cpt1 exampleBN1

-- P(B,C | A) = P(C | B, A)*P(B) = 0.1*0.2 = 0.02
inference_BN1Cpt1_BC_A = topDownInference (\x -> 1) (Map.fromList [("B",1), ("C",1)]) (Map.fromList [("A",1)]) exampleBN1Cpt1 exampleBN1

inference_BN1Cpt1_A_C = naiveBayesianInference (\x -> 1) (Map.fromList [("A",1)]) (Map.fromList [("C",1)]) exampleBN1Cpt1 exampleBN1




main = do
  putStrLn ("inference_dummy_C (0.1): " ++ (show inference_dummy_C))
  putStrLn ""
  putStrLn ("inference_BN0Cpt1_C (0.1): " ++ (show inference_BN0Cpt1_C))
  putStrLn ""
  putStrLn ("inference_BN0Cpt1_D (0.21): " ++ (show inference_BN0Cpt1_D))
  putStrLn ""
  putStrLn ("inference_BN0Cpt1_D_C (0.3): " ++ (show inference_BN0Cpt1_D_C))
  putStrLn ""
  putStrLn ("inference_BN1Cpt1_A (0.1): " ++ (show inference_BN1Cpt1_A))
  putStrLn ""
  putStrLn ("inference_BN1Cpt1_B (0.2): " ++ (show inference_BN1Cpt1_B))
  putStrLn ""
  putStrLn ("inference_BN1Cpt1_C_A (0.5): " ++ (show inference_BN1Cpt1_C_A))
  putStrLn ""
  putStrLn ("inference_BN1Cpt1_BC_A (0.02): " ++ (show inference_BN1Cpt1_BC_A))
  putStrLn "------------------------------------------------"
  putStrLn ("inference_BN1Cpt1_A_C (0.165): " ++ (show inference_BN1Cpt1_A_C))
  putStrLn ""
  putStrLn "done"


