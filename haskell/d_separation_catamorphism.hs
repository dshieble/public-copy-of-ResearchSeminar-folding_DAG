{--
runhaskell d_separation_catamorphism

Conditional and Unconditional D-Separation as a Catamorphism

Bayesian network structures allow us to quickly reason about the dependence/independence of
two nodes in the network. The criteria for independence is "D-separation." Two nodes are D-separated
if there is no undirected path between them that does not contain at least one "violating" node.
In the absence of conditioning, "violating" nodes are equivalent to "collider" nodes "->N<-". In the
presence of conditioning on a set Z, collider nodes that are in/have dependents that are in Z are no
longer violating, and non-collider nodes that are in Z become violating.

This file contains a DAG fold implementation of the algorithm to check whether two nodes n1,n2 are
D-separated. The fold actually computes the set of all non-violated undirected paths between n1,n2.
For D-separated nodes, this set is empty.
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

type DSAcc = (Bool, [[String]], [[String]], [[String]], [([String], String)], Map String (Set String))





beforeFirst :: Set String -> String -> [String] -> [[String]] -> [[String]]
beforeFirst conditioning curr parents allPathsBeforeFirst = 
  if (Set.member curr conditioning) then allPathsBeforeFirst else
    allPathsBeforeFirst ++
      [[curr]] ++
      (parentConditionalPrepend curr parents allPathsBeforeFirst) ++
      (parentConditionalAppend curr parents allPathsBeforeFirst)





atFirst :: String -> [String] -> [[String]] -> ([[String]], [[String]])
atFirst firstN parents allPathsBeforeFirst =
  let 
    pathsStartFirst = [[firstN]] ++
      Data.List.nub (
        (parentConditionalPrepend firstN parents allPathsBeforeFirst) ++
        (map reverse (parentConditionalAppend firstN parents allPathsBeforeFirst)))
  in
    (pathsStartFirst, allPathsBeforeFirst)






afterFirst :: Set String -> String -> [String] -> ([[String]], [[String]]) -> ([[String]], [[String]])
afterFirst conditioning curr parents (pathsStartFirst, pathsNotStartFirst) = 
  if (Set.member curr conditioning) then (pathsStartFirst, pathsNotStartFirst) else
    let
      pathsNotStartFirstOut = pathsNotStartFirst ++ (parentConditionalAppend curr parents pathsNotStartFirst)
      pathsStartFirstOut = pathsStartFirst ++ (parentConditionalAppend curr parents pathsStartFirst)
    in 
      (pathsStartFirstOut, pathsNotStartFirstOut)





atSecond :: String -> [String] -> ([[String]], [[String]]) -> ([[String]], [[String]], [[String]])
atSecond secondN parents (pathsStartFirst, pathsNotStartFirst) = 
  let
    pathsStartFirstNotEndSecond = pathsStartFirst
    pathsNotStartFirstEndSecond = [[secondN]] ++ parentConditionalAppend secondN parents pathsNotStartFirst
    pathsStartFirstEndSecond = parentConditionalAppend secondN parents pathsStartFirst
  in
    (pathsStartFirstNotEndSecond, pathsNotStartFirstEndSecond, pathsStartFirstEndSecond)






combineMatchedPaths :: [[String]] -> [[String]] -> [([String], String)]
combineMatchedPaths leftPaths rightPaths = [(s ++ (drop 1 e), head e) | s <- leftPaths, e <- rightPaths]

updateDescendents :: String -> [String] -> Map String (Set String) -> Map String (Set String)
updateDescendents curr parents descendents =
  Map.union (Map.fromList [(curr, Set.fromList [curr])]) (Map.fromList (map updateDesSet (Map.toList descendents))) where
    updateDesSet (key, desSet) = 
      let
        parentSets = map pm parents where
          pm p = if (Set.member p desSet) then (Set.fromList [curr]) else Set.empty
        unionedSets = (foldr Set.union Set.empty parentSets)
      in
        (key, Set.union desSet unionedSets)
        
afterSecond :: Set String -> String -> [String] -> DSAcc -> DSAcc
afterSecond conditioning curr parents (
    _, pathsStartFirstNotEndSecond, pathsNotStartFirstEndSecond, pathsStartFirstEndSecond, potentialPaths, descendents) = 
  let
    descendentsOut = updateDescendents curr parents descendents
    pathsStartFirstNotEndSecondAug = parentConditionalAppend curr parents pathsStartFirstNotEndSecond
    pathsNotStartFirstEndSecondAug = parentConditionalPrepend curr parents pathsNotStartFirstEndSecond
    pathsStartFirstNotEndSecondOut = pathsStartFirstNotEndSecond ++ pathsStartFirstNotEndSecondAug
    pathsNotStartFirstEndSecondOut = pathsNotStartFirstEndSecond ++ pathsNotStartFirstEndSecondAug
    (pathsStartFirstEndSecondOut, potentialPathsOut) = 
      let
        combined = combineMatchedPaths pathsStartFirstNotEndSecondAug pathsNotStartFirstEndSecondAug
      in
        if (Set.member curr conditioning) then 
          let
            potentialPathsWithCurrInDescendents :: [[String]]
            potentialPathsWithCurrInDescendents = (map fst (
              filter (\(path, key) -> Set.member curr (forceLookup key descendentsOut)) potentialPaths))
          in
            (pathsStartFirstEndSecond ++ (map fst combined) ++ potentialPathsWithCurrInDescendents, potentialPaths)
            -- ([["(((this is also fake)))"]], potentialPaths ++ combined)
        else
          (pathsStartFirstEndSecond, potentialPaths ++ combined)
          -- ([["(((this is fake)))"]], potentialPaths ++ combined)
  in
    (True, pathsStartFirstNotEndSecondOut, pathsNotStartFirstEndSecondOut, pathsStartFirstEndSecondOut, potentialPathsOut, descendentsOut)





apply :: String -> String -> Set String -> String -> [String] -> DSAcc -> DSAcc
apply n1 n2 conditioning curr parents (
    visitedBoth, pathsStartFirstIn, pathsNotStartFirstIn, pathsStartFirstEndSecondIn, potentialPathsIn, descendentsIn) = 
  if (n1 == curr) then
    -- If we are at n1
    let
      (pathsStartFirstOut, pathsNotStartFirstOut) = atFirst n1 parents pathsStartFirstIn
    in
      (False, pathsStartFirstOut, pathsNotStartFirstOut, [], [], Map.empty)
  else if (n2 == curr) then
    -- If we are at n2 
    let
      (pathsStartFirstOut, pathsEndSecondOut, pathsStartFirstEndSecondOut) = atSecond n2 parents (pathsStartFirstIn, pathsNotStartFirstIn)
    in
      (True, pathsStartFirstOut, pathsEndSecondOut, pathsStartFirstEndSecondOut, [], Map.empty)
  else if visitedBoth then
    -- If we have already visited n1 and n2
    -- afterSecond Set.empty curr parents (visitedBoth, pathsStartFirstIn, pathsNotStartFirstIn, pathsStartFirstEndSecondIn, potentialPathsIn, descendentsIn)
    afterSecond conditioning curr parents (visitedBoth, pathsStartFirstIn, pathsNotStartFirstIn, pathsStartFirstEndSecondIn, potentialPathsIn, descendentsIn)
  else if (headOption pathsStartFirstIn == Just [n1]) then
    -- If we have only visited n1 and are not at n2
    let
      (pathsStartFirstOut, pathsNotStartFirstOut) = afterFirst conditioning curr parents (pathsStartFirstIn, pathsNotStartFirstIn)
    in
      (False, pathsStartFirstOut, pathsNotStartFirstOut, [], [], Map.empty)
  else
    -- If we have not visited n1 yet
    let
      pathsStartFirstOut = beforeFirst conditioning curr parents pathsStartFirstIn
    in
      (False, pathsStartFirstOut, [], [], [], Map.empty)




{--
This makes the following assumptions:
  - neither n1 nor n2 are in conditioning
  - n1 /= n2
  - Both n1 and n2 are in the DAG
  - n1 comes before n2 in the topological sort of the DAG 

This returns (complete, paths) where complete is True if both n1 and n2 are in the DAG and 
  paths is the list of all paths between n1 and n2 (if complete is true)
--}
findPaths :: String -> String -> Set String -> DAG String -> [[String]]
findPaths n1 n2 conditioning dag = let (_, _, _, out, _, _) = (foldDag f g dag) in out where
  -- g a | trace ("findPaths is printing") False = undefined
  g a   =  apply n1 n2 conditioning a [] (False, [], [], [], [], Map.empty)
  f a as c  =  apply n1 n2 conditioning a as c



{--
A    B
 \  /|
  \/ |
  C  |
 /   |
D    E
--}
bn1  =  Node "E" ["B"] (
        Node "D" ["C"]  (
        Node "C" ["A", "B"] (
        Node "B" [] (
        Start "A"))))

{--       
B       A
|\     /
| C   /
| |  E
| D /
| \/  
| F  
|/
G
--}
bn2  = Node "G" ["B", "F"] (
       Node "F" ["D", "E"] (
       Node "E" ["A"] (
       Node "D" ["C"]  (
       Node "C" ["B"] (
       Node "B" [] (
       Start "A"))))))


showPaths :: [[String]] -> String
showPaths paths = intercalate " | " (map unwords paths)


main :: IO ()
main = do
  print ("bn1 Paths From D to E, Cond [] (D C B E): " ++ (showPaths (findPaths "D" "E" Set.empty bn1)))
  print ("bn1 Paths From A to C, Cond [] (A C): " ++ (showPaths (findPaths "A" "C" Set.empty bn1)))
  print ("bn1 Paths From A to D, Cond [] (A C D): " ++ (showPaths (findPaths "A" "D" Set.empty bn1)))
  print ("bn1 Paths From A to D, Cond [C] (): " ++ (showPaths (findPaths "A" "D" (Set.fromList ["C"]) bn1)))
  print ("bn1 Paths From A to E, Cond [] (): " ++ (showPaths (findPaths "A" "E" Set.empty bn1)))
  print ("bn1 Paths From A to B, Cond [D] (A C D C B | A C B): " ++ (showPaths (findPaths "A" "B" (Set.fromList ["D"]) bn1)))

  print "---"
  print ("bn2 Paths From D to E, Cond [] (): " ++ (showPaths (findPaths "D" "E" Set.empty bn2)))
  print ("bn2 Paths From A to B, Cond [] (): " ++ (showPaths (findPaths "A" "B" Set.empty bn2)))
  print ("bn2 Paths From D to G, Cond [] (D C B G | D F G): " ++ (showPaths (findPaths "D" "G" Set.empty bn2)))
  print ("bn2 Paths From E to G, Cond [] (E F G): " ++ (showPaths (findPaths "E" "G" Set.empty bn2)))
  print ("bn2 Paths From E to G, Cond [F] (): " ++ (showPaths (findPaths "E" "G" (Set.fromList ["F"]) bn2)))

  print ("bn2 Paths From D to E, Cond [F] (D F E): " ++ (showPaths (findPaths "D" "E" (Set.fromList ["F"]) bn2)))
  print ("bn2 Paths From D to E, Cond [G] (D C B G F E | D F G F E | D F E): " ++ (showPaths (findPaths "D" "E" (Set.fromList ["G"]) bn2)))
  print ("bn2 Paths From A to B, Cond [G] (A E F G B | A E F G F D C B | A E F D C B): " ++ (showPaths (findPaths "A" "B" (Set.fromList ["G"]) bn2)))






