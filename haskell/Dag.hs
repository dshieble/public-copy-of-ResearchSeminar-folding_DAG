{--
This module contains candidate implementations of the DAG datatype and a fold over that datatype.
--}

module Dag where

  -- Assumption: Nodes are in a topological sort order. The below algorithms will not work if this is not the case.
  data DAG a   =  Node a [a] (DAG a) | Start a deriving (Show, Eq, Ord)

  foldDag :: (a -> [a] -> b -> b) -> (a -> b) -> DAG a -> b
  foldDag f g (Start a)  =  g a
  foldDag f g (Node a as dag)  =  f a as (foldDag f g dag)

  -- Example: lookup
  lookupDAG :: Eq a => a -> DAG (a, b) -> Maybe b
  lookupDAG a  =  foldDag f g where
    g (a', b)        =  if a == a' then Just b else Nothing
    f (a', b) abs m  =  if a == a' then Just b else m
