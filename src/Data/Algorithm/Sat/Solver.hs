module Data.Algorithm.Sat.Solver where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Solver.Clause as Clause
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Var as Var

import qualified Data.List as L

solve::(Ord a)=> Fml.Fml a-> Maybe(Assignment.Assignment a)
solve a = solveStopCase Assignment.mkEmpty (CNFFml.toCNFFml ( Fml.toShapedFml(Fml.toCNF a)))

solveStopCase ::(Ord a) => Assignment.Assignment a -> CNFFml.CNFFml a -> Maybe (Assignment.Assignment a)
solveStopCase assign fml  = if CNFFml.isSatisfied fml then Just assign else 
    case solveReduce assign fml literal of
      Just assignFinal  -> Just assignFinal
      Nothing  -> solveReduce assign fml (Lit.neg literal)
  where
    literal =  selectLiteral fml

solveReduce ::(Ord a) => Assignment.Assignment a-> CNFFml.CNFFml a -> Lit.Lit a -> Maybe (Assignment.Assignment a)
solveReduce assign fml literal = if CNFFml.hasUnsatisfiedClause fmlReduced then Nothing else solveStopCase assignUpdated fmlReduced
  where
    fmlReduced = reduceFormula literal fml
    assignUpdated = Assignment.insert literal assign

selectLiteral :: (Ord a) => CNFFml.CNFFml a -> Lit.Lit a
selectLiteral f = if (not . CNFFml.isEmpty) clauses 
                  then (L.head) (Clause.getLits ( L.head (CNFFml.getClauses clauses)))
                  else CNFFml.mostFrequentLiteral f
  where
    clauses = CNFFml.CNFFml ( CNFFml.unitClauses f )

reduceFormula :: (Eq a, Ord a) => Lit.Lit a -> CNFFml.CNFFml a -> CNFFml.CNFFml a
reduceFormula literal = CNFFml.makeCNFFml . L.filter funct . L.map (reduceClause literal) . CNFFml.getClauses
  where
    funct c = literal `notElem` Clause.getLits c

reduceClause :: (Eq a, Ord a) => Lit.Lit a -> Clause.Clause a -> Clause.Clause a
reduceClause literal clause
  | L.elem (Lit.neg literal) (Clause.getLits clause) = Clause.makeClause (L.delete (Lit.neg(literal)) (Clause.getLits clause))
  | otherwise = clause
 