{--
runhaskell bn_chain_rule

Bayesian Network Chain Rule

A Bayesian Network is a DAG that has, for each node, a conditional
probability table, i.e., a function that represents the probability
that the current node has value |x0|, given that it's parents
(e.g., left-to-right ordered) have values |x1|, ..., |xn| respectively.

--}


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Dag
import BayesNetUtilities

-- The variable assignments for this bayesian network example
testBayesAssign :: String -> Int
testBayesAssign "A"  =  1
testBayesAssign "B"  =  0
testBayesAssign "C"  =  1
testBayesAssign "D"  =  1
testBayesAssign "E"  =  1


type CPT     =  String -> [Int] -> Double
type Assign  =  String -> Int

join_prob :: CPT -> Assign -> DAG String -> Double
join_prob cpt assign  =  foldDag f g where
  g l       =  cpt l [assign l]
  f l ls p  =  cpt l (map assign (l:ls)) * p

main = do
  putStrLn ("probability: " ++ (show (join_prob exampleBN1Cpt1 testBayesAssign exampleBN1)))
