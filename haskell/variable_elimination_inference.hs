{--
runhaskell variable_elimination_inference.hs

The variable elimination algorithm is a general purpose bayesian network inference algorithm that operates
by repeatedly marginalizing over each variable in the network.

For polytrees, the variable elimination algorithm runs in linear time. 

--}


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Dag
import Utilities
import BayesNetUtilities
import Debug.Trace
import System.IO

{--
The variable elimination does the following:
  - convert each variable to a "factor"
  - for each variable V
    - collect all factors that contain B merge them into a unified factor
    - marginalize over V to remove it from the unified factor


Questions:
  - How do we make the initial factors? Do we just assume that we have access to the names of all the nodes?
--}
varElimInference :: (String -> Int) -> Map String Int -> Map String Int -> CD -> DAG String -> Double
varElimInference nodeToMaxValue predNodeVals condNodeVals cpt dag = snd (foldDag f g dag) where
  f = _
  g = _



main :: IO ()
main = do
  putStrLn "done"
  



