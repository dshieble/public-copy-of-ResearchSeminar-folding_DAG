{--
runhaskell TestInference

--}
module TestInference where

  import Control.Exception
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
  import SmartInferenceCatamorphism
  import NaiveInferenceHylomorphism

  ----------------------------------- Naive Inference Tests -----------------------------------

  -- P(A | D) = P(A and D) / P(D) = P(D | A)*P(A)/P(D)
  exampleBN1Cpt1InferenceA_D_left :: Double
  exampleBN1Cpt1InferenceA_D_left = naiveBayesianInference (\x -> 1) (Map.fromList [("A",1)]) (Map.fromList [("D",1)]) exampleBN1Cpt1 exampleBN1

  -- P(A | D) = P(A and D) / P(D) = P(D | A)*P(A)/P(D)
  exampleBN1Cpt1InferenceA_D_right :: Double
  exampleBN1Cpt1InferenceA_D_right =
    (naiveBayesianInference (\x -> 1) (Map.fromList [("D",1)]) (Map.fromList [("A",1)]) exampleBN1Cpt1 exampleBN1) *
    (naiveBayesianInference (\x -> 1) (Map.fromList [("A",1)]) Map.empty exampleBN1Cpt1 exampleBN1) /
    (naiveBayesianInference (\x -> 1) (Map.fromList [("D",1)]) Map.empty exampleBN1Cpt1 exampleBN1)

  test1a = assert (approxEq exampleBN1Cpt1InferenceA_D_left exampleBN1Cpt1InferenceA_D_right) "tests passed"
  test1b = assert (approxEq (naiveBayesianInference (\x -> 1) (Map.fromList [("C",1)]) (Map.fromList [("A",1)]) exampleBN1Cpt1 exampleBN1) 0.5) test1a


  ----------------------------------- allNodeValueCombinations Tests -----------------------------------
  test2a = assert (
    (allNodeValueCombinations (\x -> 1) ["a", "b", "c"] Map.empty)  ==
    ([[0,0,0], [1,0,0],[0,1,0],[1,1,0],[0,0,1],[1,0,1],[0,1,1],[1,1,1]])) test1b

  test2b = assert (
    (allNodeValueCombinations (\x -> 1) ["a", "b", "c"] (Map.insert "b" 1 Map.empty)) ==
    ([[0,1,0],[1,1,0],[0,1,1],[1,1,1]])) test2a

  test2c = assert (
    (allNodeValueCombinations (\x -> 1) ["a"] Map.empty) == ([[0], [1]])) test2b

  test2d = assert (
    (allNodeValueCombinations (\x -> 1) ["a"] (Map.insert "a" 0 Map.empty)) == ([[0]])) test2c

  ----------------------------------- Top Down Inference Tests -----------------------------------

  cond3A = (Map.fromList [("A",1), ("B",1)])
  pred3A = (Map.fromList [("C",1)])
  test3a = assert (approxEq
    (naiveBayesianInference (\x -> 1) pred3A cond3A exampleBN1Cpt1 exampleBN1)
    (topDownInference (\x -> 1) pred3A cond3A exampleBN1Cpt1 exampleBN1)) test2d

  cond3B = (Map.fromList [("A",1)])
  pred3B = (Map.fromList [("B",1), ("C",1)])
  test3b = assert (approxEq 
    (naiveBayesianInference (\x -> 1) pred3B cond3B exampleBN1Cpt1 exampleBN1)
    (topDownInference (\x -> 1) pred3B cond3B exampleBN1Cpt1 exampleBN1)) test3a

  cond3C = (Map.fromList [("A",1), ("B",1)])
  pred3C = (Map.fromList [("C",1), ("D",1), ("E",1)])
  test3c = assert (approxEq 
    (naiveBayesianInference (\x -> 1) pred3C cond3C exampleBN1Cpt1 exampleBN1)
    (topDownInference (\x -> 1) pred3C cond3C exampleBN1Cpt1 exampleBN1)) test3b

  cond3D = (Map.fromList [("A",1)])
  pred3D = (Map.fromList [("B",1), ("C",1), ("E",1)])
  test3d = assert (approxEq 
    (naiveBayesianInference (\x -> 1) pred3D cond3D exampleBN1Cpt1 exampleBN1)
    (topDownInference (\x -> 1) pred3D cond3D exampleBN1Cpt1 exampleBN1)) test3c

  cond3E = (Map.fromList [("A",1), ("B",1)])
  pred3E = (Map.fromList [("C",1), ("D",1), ("E",1)])
  test3e = assert (approxEq 
    (naiveBayesianInference (\x -> 1) pred3E cond3E exampleBN1Cpt1 exampleBN1)
    (topDownInference (\x -> 1) pred3E cond3E exampleBN1Cpt1 exampleBN1)) test3d

  ----------------------------------- Bottom Up Inference Tests -----------------------------------
  cond4A = (Map.fromList [("C",1)]) 
  pred4A = (Map.fromList [("A",1), ("B",1)])
  test4a = assert (approxEq
    (naiveBayesianInference (\x -> 1) pred4A cond4A exampleBN1Cpt1 exampleBN1)
    (bottomUpInference (\x -> 1) pred4A cond4A exampleBN1Cpt1 exampleBN1)) test3e

  cond4B = (Map.fromList [("B",1), ("C",1)])
  pred4B = (Map.fromList [("A",1)])
  test4b = assert (approxEq 
    (naiveBayesianInference (\x -> 1) pred4B cond4B exampleBN1Cpt1 exampleBN1)
    (bottomUpInference (\x -> 1) pred4B cond4B exampleBN1Cpt1 exampleBN1)) test4a

  cond4C = (Map.fromList [("C",1), ("D",1), ("E",1)])
  pred4C = (Map.fromList [("A",1), ("B",1)])
  test4c = assert (approxEq 
    (naiveBayesianInference (\x -> 1) pred4C cond4C exampleBN1Cpt1 exampleBN1)
    (bottomUpInference (\x -> 1) pred4C cond4C exampleBN1Cpt1 exampleBN1)) test4b

  cond4D = (Map.fromList [("B",1), ("C",1), ("E",1)]) 
  pred4D = (Map.fromList [("A",1)])
  test4d = assert (approxEq 
    (naiveBayesianInference (\x -> 1) pred4D cond4D exampleBN1Cpt1 exampleBN1)
    (bottomUpInference (\x -> 1) pred4D cond4D exampleBN1Cpt1 exampleBN1)) test4c

  cond4E = (Map.fromList [("C",1), ("D",1), ("E",1)])
  pred4E = (Map.fromList [("A",1), ("B",1)])
  test4e = assert (approxEq 
    (naiveBayesianInference (\x -> 1) pred4E cond4E exampleBN1Cpt1 exampleBN1)
    (bottomUpInference (\x -> 1) pred4E cond4E exampleBN1Cpt1 exampleBN1)) test4d


  main = do
    (putStrLn test4a)

