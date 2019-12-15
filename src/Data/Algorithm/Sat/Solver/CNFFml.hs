module Data.Algorithm.Sat.Solver.CNFFml where

import qualified Data.Algorithm.Sat.Solver.Clause as Clause

newtype CNFFml a = CNFFml { getClauses :: [Clause.Clause a] }