module Data.Algorithm.Sat.Solver.Clause where

import qualified Data.List as L
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Fml as Fml

newtype Clause a = Clause { getLits :: [Lit.Lit a] } deriving (Eq)

instance (Show a) => Show (Clause a) where
  show Clause { getLits = ls } = "(" ++ L.intercalate "," (L.map show ls) ++ ")"

makeClause :: [Lit.Lit a] -> Clause a
makeClause lit = Clause { getLits = lit }

toClause :: Fml.Fml a -> Clause a
toClause ( Fml.Or a b ) = Clause (getLits(toClause a) ++ getLits(toClause b))
toClause ( Fml.Final a ) = Clause [Lit.mkTrue a]
toClause ( Fml.Not ( Fml.Final a ) ) = Clause [Lit.mkFalse a]

size :: Clause c -> Int
size c = L.length(getLits(c))

isEmpty :: Clause c -> Bool
isEmpty c = size c == 0

isUnit :: Clause c -> Bool
isUnit c = size c == 1