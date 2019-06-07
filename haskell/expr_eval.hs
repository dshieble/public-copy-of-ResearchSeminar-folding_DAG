{--
runhaskell expr_eval

Expression Evaluation Example

We can represent the application of arithmetic expressions with a DAG.
Since we only visit each edge onceand cache intermediate results, this
is an efficient implementation of expression evaluation.

Since our representation of DAGs makes each node aware of its parents, we represent
expressions such that each operator is applied to its parents rather than its children.

  1    2
   \  /|
    \/ |
    +  |
   /  \|
(+1)   /

A = 1
B = 2
C = 1 + 2 = 3
D = 1 + (1 + 2) = 4
E = (3 + 2) / 2 = 2.5

--}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Dag

-- An example expression DAG
exampleExpressionDag  =  Node "E" ["C", "B"] (
              Node "D" ["C"]  (
              Node "C" ["A", "B"] (
              Node "B" [] (
              Start "A"))))

-- The "semantics" of a node is an operation that takes values corresponding to the parents and returns a value corresponding to the node
type Semantics = String -> ([Double] -> Double)
semantics :: Semantics
semantics "A"  =  const 1
semantics "B"  =  const 2
semantics "C"  =  sum
semantics "D"  =  \ (c : _) -> c + 1
semantics "E"  =  \ (c : b : _) -> (c + b) / 2

-- Evaluation
eval :: Semantics -> DAG String -> Map String Double
eval sem    =  foldDag f g where
  g l       =  Map.insert l (sem l []) Map.empty
  f l ls m  =  Map.insert l (sem l (map f ls)) m where f l = fromMaybe 0 (Map.lookup l m)

exampleExpressionMap :: Map String Double
exampleExpressionMap = eval semantics exampleExpressionDag
a_val = Map.lookup "A" exampleExpressionMap
b_val = Map.lookup "B" exampleExpressionMap
c_val = Map.lookup "C" exampleExpressionMap
d_val = Map.lookup "D" exampleExpressionMap
e_val = Map.lookup "E" exampleExpressionMap

main = do
  putStrLn ("A: " ++ fromMaybe "" (fmap show a_val))
  putStrLn ("B: " ++ fromMaybe "" (fmap show b_val))
  putStrLn ("C: " ++ fromMaybe "" (fmap show c_val))
  putStrLn ("D: " ++ fromMaybe "" (fmap show d_val))
  putStrLn ("E: " ++ fromMaybe "" (fmap show e_val))

