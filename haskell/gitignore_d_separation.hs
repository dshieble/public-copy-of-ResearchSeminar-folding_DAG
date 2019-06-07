{--
runhaskell d_separation

D-Separation

Bayesian network structures allow us to quickly reason about the dependence/independence of
two nodes in the network. The criteria for independence is "D-separation." Two nodes are D-separated
if there is no undirected path between them that does not contain at least one "violating" node.
In the absence of conditioning, "violating" nodes are equivalent to "collider" nodes "->N<-". In the
presence of conditioning on a set Z, collider nodes that are in/have dependents that are in Z are no
longer violating, and non-collider nodes that are in Z become violating.

This file contains a DAG fold implementation of the algorithm to check whether two nodes n1,n2 are
D-separated. The fold actually computes the set of all non-violated undirected paths between n1,n2.
For D-separated nodes, this set is empty.

NOTE (5-8-19):
  For now, the implementation below can only handle unconditional D-Separation. Our next
  step will be to implement conditional D-separation.
--}


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Dag
import Utilities


-- helper method for adding a node to the beginning of paths
parentConditionalPrepend :: String -> [String] -> [[String]] -> [[String]]
parentConditionalPrepend curr parents paths = flatten (map appendIfParentHead paths) where
  appendIfParentHead path = case path of 
    (x:xs) | elem x parents  -> Just (curr:x:xs)
    _ -> Nothing
  flatten = (=<<) (maybe [] (:[]))

-- helper method for adding a node to the end of paths
parentConditionalAppend :: String -> [String] -> [[String]] -> [[String]]
parentConditionalAppend curr parents paths =
    map reverse (parentConditionalPrepend curr parents (reverse paths))


{--
  Until we hit the first node:
    - for every node we add:
      - make the new path list, and add all paths in the old list to it
      - create a path with just that node and add to the new path list
      - for each path in the old list:
        - if that path has a parent of that node at it's start
          - add a path to the new list that has the current node connected to the start
        - if that path has a parent of that node at it's end
          - add a path to the new list that has the current node connected to the end
--}
beforeFirst :: String -> [String] -> [[String]] -> [[String]]
beforeFirst curr parents paths = 
  paths ++
    [[curr]] ++
    (parentConditionalPrepend curr parents paths) ++
    (parentConditionalAppend curr parents paths)

{--
Once we hit the first node:
  - make the new empty path list
    - make the first element in this list be the first node
    - for each path in the old list:
      - if that path has a parent of the first node at it's start
        - add a path to the new list that has the first node connected to the start
      - if that path has a parent of the first node at it's end
        - add a path to the new list that has the first node connected to the end
--}
atFirst :: String -> [String] -> [[String]] -> [[String]]
atFirst firstN parents paths = 
  [[firstN]] ++
    (parentConditionalPrepend firstN parents paths) ++
    (parentConditionalAppend firstN parents paths)


{--
Until we hit the second node:
  - for every node we add:
    - make the new path list, and add all paths in the old list to it
    - if this node is a child of the first node, then make the paths [this, first] and [first, this] and add them to the new list
    - for each path in the old list:
      - if that path has a non-first parent of that node at it's start
        - add a path to the new list that has the current node connected to the start
      - if that path has a non-first parent of that node at it's end
        - add a path to the new list that has the current node connected to the end
--}
afterFirst ::  String -> [String] -> [[String]] -> [[String]]
afterFirst curr parents paths = 
  let 
    firstN = head (head paths) -- the first element of paths should be [firstN]
    firstNPaths = if (elem firstN parents) then [[firstN, curr], [curr, firstN]] else []
    firstNTrimmedParents = filter (\x -> x /= firstN) parents
  in
    paths ++ 
      firstNPaths ++
      (parentConditionalPrepend curr firstNTrimmedParents paths) ++
      (parentConditionalAppend curr firstNTrimmedParents paths)

{--
Once we hit the second node
  - make the new empty path list
  - for each path in the old list:
    - if that path has a parent of the second node at it's end
      - add a path to the new list that has the second node connected to the end
--}
atSecond :: String -> [String] -> [[String]] -> [[String]]
atSecond secondN parents paths = parentConditionalAppend secondN parents paths

apply :: String -> String -> String -> [String] -> (Bool, [[String]]) -> (Bool, [[String]])
apply n1 n2 curr parents (complete, paths) = 
  if complete then -- If we have already visited the first and second nodes, do nothing
    (complete, paths)
  else if (n1 == curr) then -- If we are at n1
    if (headOption paths == Just [n2]) then -- If we have not visited n2 yet
      (True, atSecond n1 parents paths)
    else -- If we are at n1 have already visited n2
      (False, atFirst n1 parents paths)
  else if (n2 == curr) then -- If we are at n2 
    if (headOption paths == Just [n1]) then -- If we have not visited n1 yet
      (True, atSecond n2 parents paths)
    else -- If we have already visited n1
      (False, atFirst n2 parents paths)
  else  -- If we are not at n1 or n2
    if (headOption paths == Just [n1] || headOption paths == Just [n2]) then  -- If we have already visited n1 or n2
      (False, afterFirst curr parents paths)
    else -- If we have not visited n1 or n2 yet
      (False, beforeFirst curr parents paths)


-- This makes the assumption that n1 /= n2 and both n1 and n2 are in the DAG
-- This returns (complete, paths) where complete is True if both n1 and n2 are in the DAG and 
--   paths is the list of all paths between n1 and n2 (if complete is true)
findPaths :: String -> String -> DAG String -> [[String]]
findPaths n1 n2 dag = snd (foldDag f g dag) where
  g a   =  apply n1 n2 a [] (False, [])
  f a as c  =  apply n1 n2 a as c



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


main = do
  putStrLn ("bn1 Paths From E to D: " ++ (intercalate " | " (map unwords (findPaths "E" "D" bn1))))
  putStrLn ("bn1 Paths From A to C: " ++ (intercalate " | " (map unwords (findPaths "A" "C" bn1))))
  putStrLn ("bn1 Paths From A to D: " ++ (intercalate " | " (map unwords (findPaths "A" "D" bn1))))
  putStrLn ("bn1 Paths From A to E: " ++ (intercalate " | " (map unwords (findPaths "A" "E" bn1))))
  putStrLn "---"
  putStrLn ("bn2 Paths From E to D: " ++ (intercalate " | " (map unwords (findPaths "E" "D" bn2))))
  putStrLn ("bn2 Paths From A to B: " ++ (intercalate " | " (map unwords (findPaths "A" "B" bn2))))
  putStrLn ("bn2 Paths From D to G: " ++ (intercalate " | " (map unwords (findPaths "D" "G" bn2))))
  putStrLn ("bn2 Paths From E to G: " ++ (intercalate " | " (map unwords (findPaths "E" "G" bn2))))
