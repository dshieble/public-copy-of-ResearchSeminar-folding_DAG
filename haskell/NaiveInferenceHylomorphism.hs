{--
This file contains an implementation of naive inference over a Bayesian Network (explicit queries of the
form P(A) and P(A | B) that don't take advantage of BN sparsity). This implementation frames these queries
as hylomorphisms, or compositions of a catamorphism and an anamorphism.

The anamorphism constructs a Joint Distribution starting with the Bayesian Network as the seed. The catamorphism
marginalizes over the Joint Distribution to perform the inference.

--}

module NaiveInferenceHylomorphism where

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

  ----------------------------------- Methods -----------------------------------

  {--
    This is an anamorphism to construct the joint distribution starting with the bayesian network as the seed
  --}
  buildProbabilityTable :: (String -> Int) -> CD -> DAG String -> JD String
  buildProbabilityTable nodeToMaxValue cpt dag = unfoldJd unspool (dag, Map.empty, []) where
    unspool (dag, assignments, probFuncs) = case dag of
      -- with parents case
      Node node parents subDag -> Right (node, map getAssign [0..(nodeToMaxValue node)]) where
        getAssign :: Int -> (DAG String, Map String Int, [([String], [Int] -> Double)])
        getAssign nodeValue =
          let
            parentProbFuncs = (parents, \parentAssign -> cpt node ([nodeValue] ++ parentAssign))
          in
            (subDag, (Map.insert node nodeValue assignments), probFuncs ++ [parentProbFuncs])
      -- no parents case
      Start node ->
        Left (node, map getProb [0..(nodeToMaxValue node)]) where 
          getProb nodeValue = 
            let
              updatedAssignments = Map.insert node nodeValue assignments
              currProb = List.foldr (*) 1.0 (map getProb probFuncs) where
                getProb :: ([String], [Int] -> Double) -> Double
                getProb (pnames, pfunc) = pfunc (map (\pname -> forceLookup pname updatedAssignments) pnames)
            in
              currProb * (cpt node [nodeValue])


  {--
    This is a catamorphism to perform exact probabilistic inference over a joint distribution
  --}
  inferenceJd :: Map String Int -> Map String Int -> JD String -> Double
  inferenceJd predNodeVals condNodeVals jd = tupleDiv (foldJd f g jd) -- P(A | B) = P(A and B) / P(B)
    where
      unionNodeVals = (Map.union predNodeVals condNodeVals)
      tupleDiv (first, second) = first / second 
      predLookup node pvl = case Map.lookup node unionNodeVals of
        Just value -> pvl !! value
        Nothing -> sum pvl
      condLookup node cvl = case Map.lookup node condNodeVals of
        Just value -> cvl !! value
        Nothing -> sum cvl
      f :: String -> [(Double, Double)] -> (Double, Double)
      f node vls = (predLookup node (map fst vls), condLookup node (map snd vls))
      g node prob = (prob, prob)


  -- Inference on a Bayesian Network by building up the joint distribution table and then marginalizing.
  -- This is the composition of a fold and an unfold, so it is a hylomorphism
  naiveBayesianInference :: (String -> Int) -> Map String Int -> Map String Int -> CD -> DAG String -> Double
  naiveBayesianInference nodeToMaxValue predNodeVals condNodeVals cpt dag = 
    inferenceJd predNodeVals condNodeVals (buildProbabilityTable nodeToMaxValue cpt dag)

