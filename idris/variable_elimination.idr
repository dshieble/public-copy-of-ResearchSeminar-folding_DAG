{--
This is a first pass at a dependently typed framework for BayesianNetworks. It contains an implementation of the vertex elimination algorithm for inference in Bayesian Networks.

This implementation has two distinguishing factors:
- The dependently typed representations of Bayesian Networks and factor functions, which support structural recursion and enforce several key constraints, such as unitary sum of probability distributions and the DAG structure of Bayesian Networks.
- The fixed length dependently vector representation of joint probability tables with binary indices, where (for example) if we have 4 variables, the 13th index into the 2^4 element vector is the probability of the assignment {A=1, B=1, C=0, D=1}.

To Run:
rm variable_elimination; idris variable_elimination.idr -o variable_elimination; ./variable_elimination
--}
module Main

import Data.Fin
import Data.Vect

------------------------ Proofs ----------------------------

getFactorsRewrite : (inc : Nat) -> plus (power 2 inc) (plus (power 2 inc) 0) = mult (power 2 inc) 2
getFactorsRewrite = ?getValuesFromProbsRewrite_rhs

powerSuccProof : (km: Nat) -> IsSucc (power 2 (S km))
powerSuccProof km = ?powerSuccProof_rhs

------------------------ Misc Helpers ----------------------------
mkTightFinite : (n: Nat) -> Fin (S n)
mkTightFinite Z = FZ
mkTightFinite (S k) = FS (mkTightFinite k)

total
mkZeros : (n : Nat) -> Vect n Double
mkZeros Z = Nil
mkZeros (S k) = 0 :: (mkZeros k)

total
Zeros : Vect n Double
Zeros = mkZeros _

finToDouble : Fin n -> Double
finToDouble fn = prim__toFloatInt (toIntNat (finToNat fn))

doubleToFin : Double -> (n : Nat) -> Maybe (Fin n)
doubleToFin d = natToFin (fromIntegerNat (prim__fromFloatBigInt d))

getOrElse : Maybe b -> b -> b
getOrElse mx default = case mx of
  Nothing => default
  Just mx => mx

------------------------ Numerical Helpers ----------------------------

-- Get the index into the output vector that we want to update given the scope index of the variable and the index into the source variable
getKthBinaryDigit : Double -> Double -> Double
getKthBinaryDigit digitK bin = floor(bin / (pow 2 digitK)) - 2 * floor(bin / (pow 2 (digitK + 1)))

-- compute a mod b
modulus : Double -> Double -> Double
modulus a b = a - (b * floor(a / b))

-- Get the kth binary digit in a number
getKthBinaryDigitFinite : Fin (S n) -> Fin (power 2 (S n)) -> Bool
getKthBinaryDigitFinite digit bin = (getKthBinaryDigit (finToDouble digit) (finToDouble bin)) > 0.5


-- Remove the digit digitFloat from the binary number binFloat. This shouldn use finite arithmetic rather than Doubles, but we will need to do some binary arithmetic proofs to get around this
getReducedIndexHelper : Double -> Double -> Double
getReducedIndexHelper digitFloat binFloat = let
    -- flip the kth binary digit to 0
    choppedBin: Double = binFloat - (getKthBinaryDigit digitFloat binFloat)*(pow 2 digitFloat)
    -- remove everything to the right of the kth binary digit
    toRemove: Double = modulus choppedBin (pow 2 digitFloat)
    choppedRemovedBin: Double = choppedBin - toRemove
    -- Remove the last 0 by dividing by two
    choppedRemovedDividedBin: Double = choppedRemovedBin / 2
    -- Add the removed part back in
    choppedDividedBin: Double = choppedRemovedDividedBin + toRemove
  in
    choppedDividedBin

-- Remove the digit digitK from the binary number bin. This shouldn't return a Maybe, but we will need to do some binary arithmetic proofs to get around this
getReducedIndex : Fin (S (S km)) -> Fin (power 2 (S (S km))) -> Maybe (Fin (power 2 (S km)))
getReducedIndex {km} digitK bin = let
    digitFloat: Double = finToDouble digitK
    binFloat: Double = finToDouble bin
  in
    doubleToFin (getReducedIndexHelper digitFloat binFloat) (power 2 (S km))


------------------------ Data Structures ----------------------------

-- We represent each node with:
    -- A inc-vector of the inc incident edges (this should probably be a fixed size sorted set instead)
    -- A 2^inc table of the probabilities of each incident combination. Takes the form:
      --if inc == 1 and vect = [b]: [P(a | b'), P(a | b)]
      --if inc == 2 and vect = [b,c]: [P(a | b',c'), P(a | b',c), P(a | b',c), P(a | b,c)]
data BayesNet : Nat -> Type where
  Node0  : Double -> BayesNet Z
  NodeN  : {inc: Nat} -> Vect inc (Fin (S n)) -> Vect (power 2 inc) Double -> BayesNet n -> BayesNet (S n)


-- n is the maximum index of the variables in the bayes net ordering
-- km is one less than the number of variables indexed
public export record Factor (n: Nat) (km: Nat)  where
  constructor MkFactor
  -- The variables in the scope of the factor
  scope : Vect (S km) (Fin (S n))

  -- The probability values of the different variable settings
  -- Represented as a binary number. E.g. For [A,B,C] index 3 is [A',B,C'] and index 5 is [A,B',C']
  values : Vect (power 2 (S km)) Double


-- ideally, we could add an additional type here to represent the number of unique variables contained in all of the factors. This number should go down on each variable elimination step. We could perhaps do this with a finite type
FactorVector : Nat -> Nat -> Type
FactorVector len n = Vect len (km ** Factor n km)

NonEmptyFactorVector : Nat -> Nat -> Type
NonEmptyFactorVector l n = FactorVector (S l) n

scopeContainsVariable : (km ** Factor n km) -> Fin (S n) -> Bool
scopeContainsVariable factorPair v = elem v (scope (snd factorPair))


------------------------ Convert a Bayesian Network to Factors ----------------------------

weakenFactorVector : FactorVector len n -> FactorVector len (S n)
weakenFactorVector vec = map weakenFactor vec where
  weakenFactor : {n: Nat} -> (km ** Factor n km) -> (km ** Factor (S n) km)
  weakenFactor {n} (km ** factor) = (km ** MkFactor (map weaken (scope factor)) (values factor) {n=(S n)} {km=km})

getFactors : (n: Nat) -> BayesNet n -> FactorVector (S n) n
getFactors Z (Node0 value) = [(0 ** MkFactor [FZ] [1 - value, value])]
getFactors (S n1) (NodeN {inc} vars probs net) =
  let
    scope: Vect (S inc) (Fin (S (S n1))) = [mkTightFinite (S n1)] ++ (map weaken vars)
    values: Vect (power 2 (S inc)) Double =
      rewrite (getFactorsRewrite inc) in --(map (\x => 1-x) probs) ++ probs
      -- rewrite powerSuccPowerLeft inc 2 in
      -- rewrite multCommutative (power 2 inc) 2 in
      concat (map (\x => [1-x, x]) probs)
  in
    [(inc ** MkFactor scope values {n=(S n1)} {km=inc})] ++ (weakenFactorVector (getFactors n1 net))


------------------------ Marginalize a Factor ----------------------------

marginalizeUpdater : Vect (power 2 (S (S km))) Double -> Fin (S (S km)) -> Fin (power 2 (S (S km))) -> Vect (power 2 (S km)) Double ->  Vect (power 2 (S km)) Double
marginalizeUpdater oldValues variableScopeIndex oldValuesindex newValues =
  case (getReducedIndex variableScopeIndex oldValuesindex) of
    Nothing => newValues
    Just reducedIndex => updateAt reducedIndex (+ (index oldValuesindex oldValues)) newValues

-- Given a factor and the index to a variable in that factor's scope, remove that variable from that factor by summing over it
marginalizeFactorHelper: Factor n (S km) -> Fin (S (S km)) -> Factor n km
marginalizeFactorHelper {km} factor variableScopeIndex =
    MkFactor (getNewScope factor) (getNewValues (values factor) variableScopeIndex) {n=n} {km=km} where
      getNewScope factor = deleteAt variableScopeIndex (scope factor)
      getNewValues oldValues variableScopeIndex =
        foldr accumulator (mkZeros (power 2 (S km)))  (range {len=(power 2 (S (S km)))}) where
          accumulator : Fin (power 2 (S (S km))) -> Vect (power 2 (S km)) Double ->  Vect (power 2 (S km)) Double
          accumulator oldValuesindex newValues =
            marginalizeUpdater oldValues variableScopeIndex oldValuesindex newValues

-- Given a factor and a variable, remove that variable from that factor by summing over it
marginalizeFactor : (km ** Factor n km) -> Fin (S n) -> (kmout ** Factor n kmout)
marginalizeFactor (km ** factor) variable = case km of
  (S _) => case findIndex (== variable) (scope factor) of
    Nothing => ((S _) ** factor)
    Just variableScopeIndex => (_ ** (marginalizeFactorHelper factor variableScopeIndex))
  _ => (_ ** factor)


------------------------ Compute Product of Factors ----------------------------


-- Take the union of the scopes of two factors, and return the shared scope as well as a mappings from the component factors' scopes to the shared scope
getSharedScope : Factor n km1 -> Factor n km2 ->
  (kmout ** (Vect (S kmout) (Fin (S n)), Vect (S kmout) (Maybe (Fin (S km1))), Vect (S kmout) (Maybe (Fin (S km2)))))
getSharedScope factor1 factor2 =
  let
    -- sharedScope = snd (nub ((scope factor1) ++ (scope factor2)))
    sharedScope = (scope factor1) ++ (snd (filter (\e => isNothing (elemIndex e (scope factor1))) (scope factor2)))
    f1ScopeIndices : Vect (S _) (Maybe (Fin (S km1))) = map (\ix => elemIndex (index ix sharedScope) (scope factor1)) (range {len=(S _)})
    f2ScopeIndices : Vect (S _) (Maybe (Fin (S km2))) = map (\ix => elemIndex (index ix sharedScope) (scope factor2)) (range {len=(S _)})
  in
    (_ ** (sharedScope, f1ScopeIndices, f2ScopeIndices))


-- This should not return a Maybe, but we need to do some proofs that utilize some pretty serious binary arithmetic to prevent this
getValueIndex : Vect (S kmout) (Maybe (Fin (S km1))) -> Fin (power 2 (S kmout)) -> Maybe (Fin (power 2 (S km1)))
getValueIndex {kmout} {km1} f1ScopeIndices sharedValueIndex =
  let
    zipped: Vect (S kmout) (Maybe (Fin (S km1)), Fin (S kmout)) = zip f1ScopeIndices (range {len=(S kmout)})
  in
    natToFin (foldr (elemAccAcc sharedValueIndex) 0 zipped) {n=(power 2 (S km1))} where
      elemAccAcc : Fin (power 2 (S kmout)) -> (Maybe (Fin (S km1)), Fin (S kmout)) -> Nat -> Nat
      elemAccAcc sharedValueIndex (f1ScopeIndex, sharedScopeIndex) valueIndex = case f1ScopeIndex of
        Nothing => valueIndex
        Just f1ScopeIndexResolved =>
          if (getKthBinaryDigitFinite sharedScopeIndex sharedValueIndex) then valueIndex + (power 2 (finToNat f1ScopeIndexResolved)) else valueIndex

getValue : Vect (S kmout) (Maybe (Fin (S km1))) -> Fin (power 2 (S kmout)) -> Factor n km1 -> Double
getValue f1ScopeIndices sharedValueIndex factor = case (getValueIndex f1ScopeIndices sharedValueIndex) of
  Nothing => -1
  Just valueIndex => index valueIndex (values factor)


getSharedValue : Factor n km1 -> Factor n km2 -> Vect (S kmout) (Maybe (Fin (S km1))) -> Vect (S kmout) (Maybe (Fin (S km2))) -> Fin (power 2 (S kmout)) -> Double
getSharedValue factor1 factor2 f1ScopeIndices f2ScopeIndices sharedValueIndex =
  (getValue f1ScopeIndices sharedValueIndex factor1) * (getValue f2ScopeIndices sharedValueIndex factor2)

-- take product of factors by a union of the variables in km1 and km2
factorProduct : (km1 ** Factor n km1) -> (km2 ** Factor n km2) -> (kmout ** Factor n kmout)
factorProduct (km1 ** factor1) (km2 ** factor2) =
  let
    (kmout ** (sharedScope, f1ScopeIndices, f2ScopeIndices)) = getSharedScope factor1 factor2
    values = map (getSharedValue factor1 factor2 f1ScopeIndices f2ScopeIndices) (range {len=(power 2 (S kmout))})
  in
    (kmout ** MkFactor sharedScope values)


------------------------ Variable Elimination ----------------------------

-- Remove a variable from a factor vector by taking factor product and then marginalizing.
eliminateVariable : {n: Nat} -> NonEmptyFactorVector l n -> Fin (S n) -> (ll ** NonEmptyFactorVector ll n)
eliminateVariable {l} {n} factorVector variable =
  let
    (contains, doesNotContain) = Data.Vect.partition (\factor => scopeContainsVariable factor variable) factorVector
  in
    case contains of
      (Z ** Nil) => (l ** factorVector)
      (_ ** (x :: xs)) => ((fst doesNotContain) ** ([marginalizeFactor (Data.Vect.foldr1 factorProduct (x :: xs)) variable] ++ (snd doesNotContain)))


-- Remove each variable in the sortedVars list from a factor vector by taking factor products and then marginalizing. Ideally we could write this so that it returns "Factor n 0"
eliminateVariables : NonEmptyFactorVector fl n -> Vect vl (Fin (S n)) -> (km ** Factor n km)
eliminateVariables factors sortedVars = case sortedVars of
  [] => foldr1 factorProduct factors
  x :: xs => eliminateVariables (snd (eliminateVariable factors x)) xs

-- Orders the variables in the order of elimination. The target variable is not included.
getVariableOrdering : NonEmptyFactorVector len n -> Fin (S n) -> Vect n (Fin (S n))
getVariableOrdering {n} factors targetVar = case (Data.Vect.findIndex (== targetVar) range) of
  Nothing => drop 1 range
  Just index => reverse (Data.Vect.deleteAt index range)

-- Compute the probability distribution of a node. It can't support conditional queries (P(A | B)) yet, but that should be a small change.
computeProbabilityDistribution : BayesNet n -> Fin (S n) -> (m ** Vect m Double)
computeProbabilityDistribution {n} net targetVar =
  let
    factors = getFactors n net
    sortedVars = getVariableOrdering factors targetVar
  in
    (_ ** values (snd (eliminateVariables factors sortedVars)))

------------------------ Printing Helpers ----------------------------
printMaybe : (a -> String) -> Maybe a -> String
printMaybe printfn maybeVal  = case maybeVal of
  Nothing => "NOTHING"
  Just mat => "Just: " ++ (printfn mat)

printNat : Nat -> String
printNat n = prim__toStrBigInt (toIntegerNat n)

printFin : Fin n -> String
printFin v = prim__toStrInt (toIntNat (finToNat v))

printVector : Vect n a -> (a -> String) -> String
printVector v f = foldr1 (++) (map (++ " ") ([""] ++ (map f v)))

printDoubleVector : Vect n Double -> String
printDoubleVector v = printVector v prim__floatToStr

printFinVector : Vect n (Fin km) -> String
printFinVector v = printVector v printFin

printMaybeVector : Vect n (Maybe a) -> (a -> String) -> String
printMaybeVector v f = printVector v (printMaybe f)

printFactor : Factor n km -> String
printFactor factor = let
    scopeString: String = printFinVector (scope factor)
    valueString: String = printDoubleVector (values factor)
  in
    "scope: " ++ scopeString ++ " | values: " ++ valueString

printFactorPair : (km ** Factor n km) -> String
printFactorPair factorPair = printFactor (snd factorPair)


printFactorVector: FactorVector len n -> String
printFactorVector v = foldr1 (++) (map (++ " ") ([""] ++ (map printFactorPair v)))

printNonEmptyFactorVectorPair: (l ** NonEmptyFactorVector l n) -> String
printNonEmptyFactorVectorPair (pl ** v) = foldr1 (++) (map (++ " ") ([""] ++ (map printFactorPair v)))

printFactorVectorPair: (len ** FactorVector len n) -> String
printFactorVectorPair (len ** v) = foldr1 (++) (map (++ " ") ([""] ++ (map printFactorPair v)))

------------------------------ Examples -----------------------------------



{--
0 -> 1 -> 2
P(2 | 1') = 0.2
P(2 | 1) = 0.3
P(1 | 0') = 0.6
P(1 | 0) = 0.5
P(0) = 0.2
--}
network1 : BayesNet 2
network1 =
  NodeN [1] [0.2, 0.3] (
  NodeN [0] [0.6, 0.5]
  (Node0 0.2))


{--
0    1
 \  /|
  \/ |
  2  |
 /  \|
3    4
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
network2 : BayesNet 4
network2 = NodeN [1,2] [0.2,0.5,0.1,0.7] (
  NodeN [2] [0.5,0.6] (
  NodeN [0,1] [0.3,0.6,0.2,0.1] (
  NodeN [] [0.2]
  (Node0 0.1))))


main : IO ()
main = do
  putStrLn ("\n------------------ Network 1 ------------------")
  putStrLn ("\n Variable 0: " ++ (printDoubleVector (snd (computeProbabilityDistribution network1 0))))
  putStrLn ("\n Variable 1: " ++ (printDoubleVector (snd (computeProbabilityDistribution network1 1))))
  putStrLn ("\n Variable 2: " ++ (printDoubleVector (snd (computeProbabilityDistribution network1 2))))

  putStrLn ("\n------------------ Network 2 ------------------")
  putStrLn ("\n Variable 0: " ++ (printDoubleVector (snd (computeProbabilityDistribution network2 0))))
  putStrLn ("\n Variable 1: " ++ (printDoubleVector (snd (computeProbabilityDistribution network2 1))))
  putStrLn ("\n Variable 2: " ++ (printDoubleVector (snd (computeProbabilityDistribution network2 2))))
  putStrLn ("\n Variable 3: " ++ (printDoubleVector (snd (computeProbabilityDistribution network2 3))))
  putStrLn ("\n Variable 4: " ++ (printDoubleVector (snd (computeProbabilityDistribution network2 4))))
