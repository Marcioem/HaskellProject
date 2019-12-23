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

solve :: CNFFml.CNFFml -> Maybe (Map.Map Var.Var Bool)
solve = solveAux Map.empty

selectLiteral :: Clause.Clause f -> Lit.Lit l
selectLiteral f = if (not . Clause.isEmpty) f'
                   then (L.head . L.head) f'
                   else CNFFml.mostFrequentLiteral f
  where
    f' = CNFFml.unitClauses f

reduceFormula :: (Eq a) => Lit.Lit a -> CNFFml.CNFFml a -> CNFFml.CNFFml a
reduceFormula literal = makeCNFFml . L.filter f . L.map (reduceClause literal) . getClauses
  where
    f c = literal `notElem` Clause.getLits c

reduceClause :: (Eq a) => Lit.Lit a -> Clause.Clause a -> Clause.Clause a
reduceClause literal clause
  | L.elem (Lit.neg literal) (Clause.getLits clause) = Clause.makeClause (L.filter (not. (\x -> x == Lit.neg(literal))) (Clause.getLits clause))
  | otherwise = clause

solveAux :: Map.Map Var.Var Bool -> CNFFml.CNFFml -> Maybe (Assignment.Assignment a)
solveAux m [] = Just m
solveAux m f  = case solveAux' m f l of
  Just m'  -> Just m'
  Nothing  -> solveAux' m f (negate l)
  where
    l =  selectLiteral f

solveAux' :: Map.Map Var.Var Bool -> CNFFml.CNFFml -> Lit.Lit -> Maybe (Assignment.Assignment a)
solveAux' m f l = if CNFFml.hasUnsatisfiedClause f'
                  then Nothing
                  else solveAux m' f'
  where
    f' = reduceFormula l f
    m' = Map.insert (abs l) (l > 0) m





















reduceFormula :: (Eq a) => Lit.Lit a -> CNFFml.CNFFml a -> CNFFml.CNFFml a
reduceFormula literal = makeCNFFml . L.filter f . L.map (reduceClause literal) . getClauses
  where
    f c = literal `notElem` Clause.getLits c

reduceClause :: (Eq a) => Lit.Lit a -> Clause.Clause a -> Clause.Clause a
reduceClause literal clause
  | L.elem (Lit.neg literal) (Clause.getLits clause) = Clause.makeClause (L.filter (not. (\x -> x == Lit.neg(literal))) (Clause.getLits clause))
  | otherwise = clause








reduce :: (Eq a, Ord a) => Lit.Lit a -> Clause a -> Clause a
reduce l = mk . L.filter (/= l') . getLits
  where
    l' = Lit.neg l


-- Reduce a formula according to a literal
reduce :: (Eq a, Ord a) => Lit.Lit a -> Fml a -> Fml a
reduce l = mk . L.filter f . L.map (Clause.reduce l) . getClauses
  where
    f c = l `notElem` Clause.getLits c



reduceFormula :: (Eq a) => Lit.Lit a -> CNFFml.CNFFml a -> CNFFml.CNFFml a
reduceFormula literal = makeCNFFml . L.filter f . L.map (reduceClause literal) . getClauses
  where
    f c = literal `notElem` Clause.getLits c

reduceClause :: (Eq a) => Lit.Lit a -> Clause.Clause a -> Clause.Clause a
reduceClause literal clause
  | L.elem (Lit.neg literal) (Clause.getLits clause) = Clause.makeClause (L.filter (not. (\x -> x == Lit.neg(literal))) (Clause.getLits clause))
  | otherwise = clause
-}