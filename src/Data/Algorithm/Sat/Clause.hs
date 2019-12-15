module Data.Algorithm.Sat.Solver.Clause where

import qualified Data.List as L
import qualified Data.Algorithm.Sat.Lit as Lit

newtype Clause a = Clause { getLits :: [Lit.Lit a] } deriving (Eq)

instance (Show a) => Show (Clause a) where
  show Clause { getLits = ls } = "(" ++ L.intercalate "," (L.map show ls) ++ ")"