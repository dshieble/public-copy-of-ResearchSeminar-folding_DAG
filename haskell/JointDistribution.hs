{--
This module contains an implementation of a joint distribution (JD) datatype and a fold over that datatype.
--}

module JointDistribution where

  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.List

  -- This represents the full joint distribution table that maps variable assignments to probabilities
  -- The elements should sum to 1, but we can't enforce that in haskell.
  -- This is almost equivalent to a N-tree.
  data JD a = BuildJD (a, [JD a]) | BaseJD (a, [Double])

  -- P(A) = 0.4
  testJD1 :: JD String
  testJD1 = BaseJD ("a", [0.6, 0.4])

  {--
  P(A, B) = 0.1
  P(A, -B) = 0.4
  P(-A, B) = 0.2
  P(-A, -B) = 0.3
  --}
  testJD2 :: JD String
  testJD2 = BuildJD ("a", [
      BaseJD ("b", [0.4, 0.1]),
      BaseJD ("b", [0.3, 0.2])
    ])


  --------------------------- Joint Distribution Methods ---------------------------
  unfoldJd :: (b -> Either (a, [Double]) (a, [b])) -> b -> JD a
  unfoldJd unspool x = case unspool x of
    Left (label, probs) -> BaseJD (label, probs)
    Right (label, children) -> BuildJD (label, map (unfoldJd unspool) children)

  foldJd :: (a -> [b] -> b) -> (a -> Double -> b) -> JD a -> b 
  foldJd f g jd = case jd of
    BuildJD (currNode, jds) -> f currNode (map (\jd -> foldJd f g jd) jds)
    BaseJD  (currNode, probs) -> f currNode (map (\prob -> g currNode prob) probs)


  printJd :: JD String -> String
  printJd jd = intercalate "\n" (foldJd f g jd) where
    f node vl =
      let
        appendedTrueList = (map aptr (vl !! 1)) where aptr x = node ++ ": True | " ++ x
        appendedFalseList = (map apfl (vl !! 0)) where apfl x = node ++ ": False | " ++ x
      in 
        appendedTrueList ++ appendedFalseList
    g node prob = [show prob]

  sumJd :: JD String -> Double
  sumJd jd = (foldJd f g jd) where
    f node vl = sum vl
    g node prob = prob


  -- Get a particular element in the joint distribution table
  lookupJd :: (String -> Int) -> JD String -> Double
  lookupJd assignments jd = (foldJd f g jd) where
    f node vl = vl !! (assignments node)
    g node prob = prob
