module Data.Algorithm.Sat.Solver.CNFFml
  where

import qualified Data.Algorithm.Sat.Solver.Clause as Clause
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Lit as Lit

import qualified Data.List as L
import qualified Data.Tuple as T
import qualified Data.Foldable as F
import qualified Data.Ord as Ord

newtype CNFFml a = CNFFml { getClauses :: [Clause.Clause a] }

makeCNFFml :: [Clause.Clause c] -> CNFFml c
makeCNFFml cl = CNFFml { getClauses = cl }

toCNFFml :: Fml.Fml a -> CNFFml a
toCNFFml (Fml.And a (Fml.And b c)) = CNFFml ([Clause.toClause a] ++ getClauses (toCNFFml b) ++ getClauses (toCNFFml c))
toCNFFml (Fml.And a b) = CNFFml ([Clause.toClause a] ++ [Clause.toClause b])
toCNFFml a = CNFFml [Clause.toClause a]

isEmpty :: CNFFml cnf -> Bool
isEmpty c = L.length(getClauses(c)) == 0

isSatisfied :: CNFFml cnf -> Bool
isSatisfied = isEmpty

unitClauses :: CNFFml c -> [Clause.Clause c]
unitClauses c = filter (Clause.isUnit) (getClauses c)

hasUnsatisfiedClause :: CNFFml cnf -> Bool
hasUnsatisfiedClause cnf = any (Clause.isEmpty) (getClauses(cnf))

mostFrequentLiteral :: (Ord a) => CNFFml a -> Lit.Lit a
mostFrequentLiteral = T.fst . L.last . L.sortOn T.snd . L.map function . L.group . L.sort . F.concatMap Clause.getLits . getClauses
  where
    function xs = (L.head xs, L.length xs)

size :: CNFFml cnf -> Int
size cnf = L.length(getClauses(cnf))
