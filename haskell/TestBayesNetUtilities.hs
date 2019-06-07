{--
runhaskell TestBayesNetUtilities

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

  test1a = assert (
    (parentConditionalAppend "D" ["C"] [["A"], ["A", "C"]]) ==
      [["A","C","D"]]) "tests passed"
  test1b = assert (
    (parentConditionalAppend "D" ["A"] [["A"], ["A", "C"]]) ==
      [["A", "D"]]) test1a
  test1c = assert (
    (parentConditionalPrepend "D" ["A"] [["A"], ["A", "C"]]) ==
      [["D", "A"], ["D", "A", "C"]]) test1b
  main = do
    putStrLn test1c

