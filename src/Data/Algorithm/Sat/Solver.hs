module Data.Algorithm.Sat.Solver where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Solver.Clause as Clause
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Var as Var

import qualified Data.List as L

{-|
Fonction qui prend une formule Fml en parametre et qui renvoie un Maybe(Assignement) qui correspond à la résolution de la fml.
-}
solve::(Ord a)=> Fml.Fml a-> Maybe(Assignment.Assignment a)
solve a = solveStopCase Assignment.mkEmpty (CNFFml.toCNFFml ( Fml.toShapedFml(Fml.toCNF a)))

{-|
Fonction auxiliaire de solve qui permet d'avoir les cas d'arrets de la résolution.
-}
solveStopCase ::(Ord a) => Assignment.Assignment a -> CNFFml.CNFFml a -> Maybe (Assignment.Assignment a)
solveStopCase assign fml  = if CNFFml.isSatisfied fml then Just assign else 
    case solveReduce assign fml literal of
      Just assignFinal  -> Just assignFinal
      Nothing  -> solveReduce assign fml (Lit.neg literal)
  where
    literal =  selectLiteral fml

{-|
Fonction auxiliaire de solver qui permet de réduire la formule en fonction d'un literal et d'ajouter ce literal dans l'Assignment
-}
solveReduce ::(Ord a) => Assignment.Assignment a-> CNFFml.CNFFml a -> Lit.Lit a -> Maybe (Assignment.Assignment a)
solveReduce assign fml literal = if CNFFml.hasNotSatisfiedClause fmlReduced then Nothing else solveStopCase assignUpdated fmlReduced
  where
    fmlReduced = reduceFormula literal fml
    assignUpdated = Assignment.insert literal assign


{-|
Fonction qui prend une formule en forme CNF et qui renvoie un literal.
Ce literal est un literal d'une clause unitaire si il y en a dans la formule
 ou ce literal est le literal le plus fréquent de la formule.
-}
selectLiteral :: (Ord a) => CNFFml.CNFFml a -> Lit.Lit a
selectLiteral f = if (not . CNFFml.isEmpty) clauses 
                  then (L.head) (Clause.getLits ( L.head (CNFFml.getClauses clauses)))
                  else CNFFml.mostFrequentLiteral f
  where
    clauses = CNFFml.CNFFml ( CNFFml.unitaryClauses f )

{-|
Fonction prend un literal Lit et une formule en forme CNF et qui renvoit une formule réduit du literal.
La réduction de la formule permet de retirer les clauses contenant le Lit et 
de retirer des clauses, les Literal qui correspondent à la forme negative du Lit 
-}
reduceFormula :: (Eq a, Ord a) => Lit.Lit a -> CNFFml.CNFFml a -> CNFFml.CNFFml a
reduceFormula literal = CNFFml.makeCNFFml . L.filter funct . L.map (reduceClause literal) . CNFFml.getClauses
  where
    funct c = literal `notElem` Clause.getLits c

{-|
Fonction auxiliaire du reduceFormula qui prend un literal Lit et une clause et qui réduit la clause en fonction du literal.
-}
reduceClause :: (Eq a, Ord a) => Lit.Lit a -> Clause.Clause a -> Clause.Clause a
reduceClause literal clause
  | L.elem (Lit.neg literal) (Clause.getLits clause) = Clause.makeClause (L.delete (Lit.neg(literal)) (Clause.getLits clause))
  | otherwise = clause
 