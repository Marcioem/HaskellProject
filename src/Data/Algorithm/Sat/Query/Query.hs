module Data.Algorithm.Sat.Query.Query where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Solver.Clause as Clause
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Solver as Solver

satisfiable :: (Ord a)=> Fml.Fml a-> Bool
satisfiable fml =  case Solver.solve fml of
                        Just assignFinal  -> True
                        Nothing  -> False

satisfyingAssignment :: (Ord a)=> Fml.Fml a-> Maybe(Assignment.Assignment a)
satisfyingAssignment fml = Solver.solve fml