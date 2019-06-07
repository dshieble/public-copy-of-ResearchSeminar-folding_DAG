{--
This file contains some generic helper functions
--}

module Utilities where

  import Data.Map (Map)
  import qualified Data.Map as Map

  import Data.List
  import qualified Data.List as List

  boolToInt :: Bool -> Int
  boolToInt x = if x then 1 else 0

  headOption :: [a] -> Maybe a
  headOption l = case l of 
    x:xs -> Just x
    _ -> Nothing

  lastOption :: [a] -> Maybe a
  lastOption l = case (reverse l) of 
    x:xs -> Just x
    _ -> Nothing

  forceLookup :: String -> Map String b -> b
  forceLookup key dict = case Map.lookup key dict of
    Just value -> value
    Nothing -> error (key ++ " is not a valid option.")

  approxEq :: Double -> Double -> Bool
  approxEq a b = (abs (a - b)) < 0.005
