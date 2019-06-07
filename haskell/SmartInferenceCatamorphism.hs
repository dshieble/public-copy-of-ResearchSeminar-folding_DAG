{--
file:///Users/dshiebler/Desktop/Category_Theory/ResearchSeminar/folding_DAGs/resources/Nils%20J.%20Nilsson%20-%20Artificial%20Intelligence_%20A%20New%20Synthesis%20-Morgan%20Kaufmann%20Publishers,%20Inc.%20(1998).pdf
pg: 336

This file contains an implementation of inference over a Bayesian Network that takes advantage of sparsity. This 
implementation frames inference as a catamorphism.

--}


module SmartInferenceCatamorphism where

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

  -- Get all possible configurations of parents
  -- one element for each configuration of parents
  -- each parent that is fixed stays fixed. the other parents are flexible
  allNodeValueCombinations :: (String -> Int) -> [String] -> Map String Int -> [[Int]]
  allNodeValueCombinations _ [] _ = [[]]
  allNodeValueCombinations nodeToMaxValue (node:restOfNodes) fixedNodes =
    let
      nodeValues = fromMaybe [0..(nodeToMaxValue node)] (fmap (\x -> [x]) (Map.lookup node fixedNodes))
      rest = allNodeValueCombinations nodeToMaxValue restOfNodes fixedNodes
    in
      [[nv] ++ r| r <- rest, nv <- nodeValues]


  {--
    This is the top-down inference method for solving queries of the form P(A1, A2, A3,... | B1, B2, B3, ...) where
      all B1, B2, B3 are before all A1, A2, A3 in the topological sort of the graph.

    This function makes the assumption that all of the condNodeVals are strictly above the predNodeVals
  --}
  topDownInference :: (String -> Int) -> Map String Int -> Map String Int -> CD -> DAG String -> Double
  topDownInference nodeToMaxValue predNodeVals condNodeVals cpt dag = snd (foldDag f g dag) where
    fixedNodeVals :: Map String Int 
    fixedNodeVals = (Map.union predNodeVals condNodeVals)

    -- Update the probability of the predicted nodes at the new node
    updateProb :: Double -> [Double] -> String -> Double
    updateProb currProb nodeDist node = fromMaybe currProb (fmap mp (Map.lookup node predNodeVals))
      where mp nv = (nodeDist !! nv) * currProb

    -- returns the updated nodeDists and the product of the probabilities of the predNodeVals
    -- nodeDists stores all visited nodes that are not indexed by condNodeVals
    f :: String -> [String] -> (Map String [Double], Double) -> (Map String [Double], Double)
    f node parents (nodeDists, currProb)  =
      if (isJust (Map.lookup node condNodeVals)) then (nodeDists, currProb) else
        let -- If this is not a node we are conditioning on
          -- The list of (probability multiplier for parent configuration, parent configuration)
          parentConfigList :: [(Double, [Int])]
          parentConfigList =
            map (\a -> (parentAssignmentProb a, a)) (allNodeValueCombinations nodeToMaxValue parents fixedNodeVals) where
              -- We only add multipliers for parents who do not have fixed values (either by conditioning or prediction)
              parentMult :: (String, Int) -> Double
              parentMult (pnode, pNodeVal) =
                if (isNothing (Map.lookup pnode fixedNodeVals)) then 
                  fromMaybe 1.0 (fmap (\d -> d !! pNodeVal) (Map.lookup pnode nodeDists))
                else
                  1.0
            
              parentAssignmentProb :: [Int] -> Double
              parentAssignmentProb parentAssignment = List.foldr (*) 1.0 (map parentMult (zip parents parentAssignment))
          -- The list of [probability of this node for it's nth value]
          nodeDist :: [Double]
          nodeDist = map getNodeProbFromNodeValue [0..(nodeToMaxValue node)] where
            getNodeProbFromNodeValue :: Int -> Double
            getNodeProbFromNodeValue nodeVal = 
              (List.foldr (+) 0
                [parentProb * (cpt node ([nodeVal] ++ parentAssign)) | (parentProb, parentAssign) <- parentConfigList])
        in
          (Map.insert node nodeDist nodeDists, updateProb currProb nodeDist node)

    g :: String -> (Map String [Double], Double)
    g node = if (isJust (Map.lookup node condNodeVals)) then (Map.empty, 1.0) else
      let 
        nodeDist = (map (\x -> cpt node [x]) [0..(nodeToMaxValue node)])
      in
        (Map.insert node nodeDist Map.empty, updateProb 1.0 nodeDist node)



  {--
    This is the bottom-up inference method for solving queries of the form P(B1, B2, B3, ... | A1, A2, A3,...) where
      all B1, B2, B3 are before all A1, A2, A3 in the topological sort of the graph.

    This function makes the assumption that all of the condNodeVals are strictly below the predNodeVals

    Since this function is equivalent to a simple operation performed on three independent catamorphisms, it is equivalent
      to a catamorphism itself by the bannana split law
  --}
  bottomUpInference :: (String -> Int) -> Map String Int -> Map String Int -> CD -> DAG String -> Double
  bottomUpInference nodeToMaxValue predNodeVals condNodeVals cpt dag = 
    let 
      pab = topDownInference nodeToMaxValue condNodeVals predNodeVals cpt dag
      pb = topDownInference nodeToMaxValue predNodeVals Map.empty cpt dag
      pa = topDownInference nodeToMaxValue condNodeVals Map.empty cpt dag
    in 
      pab * pb / pa -- P(B | A) = P(A | B)*P(B) / P(A)








