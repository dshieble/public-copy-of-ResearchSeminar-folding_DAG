{--
This file contains some helper functions specific to bayesian networks
--}

module BayesNetUtilities where

  import Dag

  -- Conditional Probability Distributions
  type CD = String -> [Int] -> Double

  ---------------------------- Example Networks ----------------------------

  {--
  A very simple example bayesian network, represented as a DAG
  C  


  --}
  dummyBN  = Start "C"


  dummyBNCpt1 :: CD
  dummyBNCpt1 "C" [1] = 0.1
  dummyBNCpt1 "C" [0] = 0.9


  {--
  A very simple example bayesian network, represented as a DAG
  C  
   \
    D

  P(D | C') = 0.2
  P(D | C) = 0.3
  P(C) = 0.1

  0.3*0.1 + 0.2*0.9

  P(D', C') = 0.72
  P(D', C) = 0.07
  P(D, C') = 0.18
  P(D, C) = 0.03
  --}
  exampleBN0  = Node "D" ["C"]  (
                Start "C" )


  exampleBN0Cpt1 :: CD
  exampleBN0Cpt1 "D" [1, 0]    = 0.2
  exampleBN0Cpt1 "D" [1, 1]    = 0.3
  exampleBN0Cpt1 "D" [0, c]    = 1 - exampleBN0Cpt1 "D" [1, c]
  exampleBN0Cpt1 "C" [1] = 0.1
  exampleBN0Cpt1 "C" [0] = 0.9

  {--
  An example bayesian network, represented as a DAG
  A    B
   \  /|
    \/ |
    C  |
   /  \|
  D    E
  --}
  exampleBN1  =  Node "E" ["C", "B"] (
                Node "D" ["C"]  (
                Node "C" ["A", "B"] (
                Node "B" [] (
                Start "A"))))


  {--
  The conditional probability table for this bayesian network
  P(4 | 1',2') = 0.2
  P(4 | 1,2') = 0.5
  P(4 | 1',2) = 0.1
  P(4 | 1,2) = 0.7
  P(3 | 2') = 0.5
  P(3 | 2) = 0.6
  P(2 | 0',1') = 0.3
  P(2 | 0',1) = 0.2
  P(2 | 0,1') = 0.6
  P(2 | 0,1) = 0.1
  P(0) = 0.1
  P(1) = 0.2

  --}
  exampleBN1Cpt1 :: CD
  exampleBN1Cpt1 "E" [1, 0, 0] = 0.8 -- P[4 | 2, 1]
  exampleBN1Cpt1 "E" [1, 0, 1] = 0.5
  exampleBN1Cpt1 "E" [1, 1, 0] = 0.1
  exampleBN1Cpt1 "E" [1, 1, 1] = 0.7
  exampleBN1Cpt1 "E" [0, c, b] = 1 - exampleBN1Cpt1 "E" [1, c, b]

  exampleBN1Cpt1 "D" [1, 0]    = 0.5
  exampleBN1Cpt1 "D" [1, 1]    = 0.6
  exampleBN1Cpt1 "D" [0, c]    = 1 - exampleBN1Cpt1 "D" [1, c]

  exampleBN1Cpt1 "C" [1, 0, 0] = 0.3 
  exampleBN1Cpt1 "C" [1, 0, 1] = 0.2
  exampleBN1Cpt1 "C" [1, 1, 0] = 0.6
  exampleBN1Cpt1 "C" [1, 1, 1] = 0.1
  exampleBN1Cpt1 "C" [0, a, b] = 1 - exampleBN1Cpt1 "C" [1, a, b]

  exampleBN1Cpt1 "B" [1] = 0.2
  exampleBN1Cpt1 "B" [0] = 0.8

  exampleBN1Cpt1 "A" [1] = 0.1
  exampleBN1Cpt1 "A" [0] = 0.9

  -- E: True | D: True | C: True | B: False | A: True
  exampleBN1Assignment1 :: String -> Int
  exampleBN1Assignment1 "A" = 1
  exampleBN1Assignment1 "B" = 0
  exampleBN1Assignment1 "C" = 1
  exampleBN1Assignment1 "D" = 1
  exampleBN1Assignment1 "E" = 1

  ---------------------------- D Separation Helpers ----------------------------
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
      map reverse (parentConditionalPrepend curr parents (map reverse paths))



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
  beforeFirstDSep :: String -> [String] -> [[String]] -> [[String]]
  beforeFirstDSep curr parents paths = 
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
  atFirstDSep :: String -> [String] -> [[String]] -> [[String]]
  atFirstDSep firstN parents paths = 
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
  afterFirstDSep ::  String -> [String] -> [[String]] -> [[String]]
  afterFirstDSep curr parents paths = 
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
  atSecondDSep :: String -> [String] -> [[String]] -> [[String]]
  atSecondDSep secondN parents paths = parentConditionalAppend secondN parents paths




