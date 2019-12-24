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

{-|
Fonction qui prend une liste de Clause en parametre et qui renvoie une Fml en format CNF à partir de ces clauses.
-}
makeCNFFml :: [Clause.Clause c] -> CNFFml c
makeCNFFml cl = CNFFml { getClauses = cl }

{-|
Fonction qui prend une formule Fml en format ShapedFml en parametre et qui la met en format CNF.
-}
toCNFFml :: Fml.Fml a -> CNFFml a
toCNFFml (Fml.And a (Fml.And b c)) = CNFFml ([Clause.toClause a] ++ getClauses (toCNFFml b) ++ getClauses (toCNFFml c))
toCNFFml (Fml.And a b) = CNFFml ([Clause.toClause a] ++ [Clause.toClause b])
toCNFFml a = CNFFml [Clause.toClause a]

{-|
Fonction qui prend une CNFFml en parametre et qui renvoie true si elle est vide et false sinon.
-}
isEmpty :: CNFFml cnf -> Bool
isEmpty c = L.length(getClauses(c)) == 0

isSatisfied :: CNFFml cnf -> Bool
isSatisfied = isEmpty

{-|
Fonction qui prend une CNFFml en parametre et qui renvoie la liste des clauses unitaires qu'elle contient.
-}
unitaryClauses :: CNFFml c -> [Clause.Clause c]
unitaryClauses c = filter (Clause.isUnitaryClause) (getClauses c)

{-|
Fonction qui prend une CNFFml en parametre et qui renvoie vrai si elle possède au moins une clause non satisfaites.
Une clause n'est pas satisfaite si elle est vide.
-}
hasNotSatisfiedClause :: CNFFml cnf -> Bool
hasNotSatisfiedClause cnf = any (Clause.isEmpty) (getClauses(cnf))

{-|
Fonction qui prend une CNFFml en parametre et qui renvoie le literal qui apparait le plus fréquemment dans la formule.
Une clause n'est pas satisfaite si elle est vide.
-}
mostFrequentLiteral :: (Ord a) => CNFFml a -> Lit.Lit a
mostFrequentLiteral = T.fst . L.last . L.sortOn T.snd . L.map function . L.group . L.sort . F.concatMap Clause.getLits . getClauses
  where
    function xs = (L.head xs, L.length xs)

{-|
Fonction qui prend une CNFFml en parametre et qui renvoie sa taille.
-}
size :: CNFFml cnf -> Int
size cnf = L.length(getClauses(cnf))
