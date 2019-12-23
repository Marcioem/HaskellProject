module Data.Algorithm.Sat.Solver where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Solver.Clause as Clause
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Var as Var

import qualified Data.List as L
import qualified Data.Map as Map

{-
solve::(Ord a)=> Fml.Fml a-> Maybe(Assignment.Assignment a)
solve a = solveAux Assignment.mkEmpty (CNFFml.toCNFFml ( Fml.toShapedFml(Fml.toCNF a)))

solveAux :: Assignment.Assignment a -> CNFFml.CNFFml a -> Maybe (Assignment.Assignment a)
solveAux assign [] = Just assign
solveAux assign fml  = case solveAux' assign fml literal of
  Just assign'  -> Just assign'
  Nothing  -> solveAux' assign fml (Lit.neg literal)
  where
    literal =  selectLiteral fml

solveAux' :: Assignment.Assignment a-> CNFFml.CNFFml a -> Lit.Lit a -> Maybe (Assignment.Assignment a)
solveAux' m f l = if CNFFml.hasUnsatisfiedClause f' then Nothing else solveAux m' f'
  where
    f' = reduceFormula l f
    m' = Assignment.insert l
-}
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
 