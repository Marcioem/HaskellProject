module Data.Algorithm.Sat.Solver.Clause where

import qualified Data.List as L
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Fml as Fml

newtype Clause a = Clause { getLits :: [Lit.Lit a] } deriving (Eq)

instance (Show a) => Show (Clause a) where
  show Clause { getLits = ls } = "(" ++ L.intercalate "," (L.map show ls) ++ ")"

{-|
Fonction qui prend une liste de literal Lit en parametre et qui renvoie une clause à partir de ces lit.
-}
makeClause :: [Lit.Lit a] -> Clause a
makeClause lit = Clause { getLits = lit }

{-|
Fonction qui prend une formule Fml en format ShapedFml en parametre et qui la convertit en une clause.
-}
toClause :: Fml.Fml a -> Clause a
toClause ( Fml.Or a b ) = Clause (getLits(toClause a) ++ getLits(toClause b))
toClause ( Fml.Final a ) = Clause [Lit.mkTrue a]
toClause ( Fml.Not ( Fml.Final a ) ) = Clause [Lit.mkFalse a]

{-|
Fonction qui prend une clause en parametre et qui renvoie sa taille.
-}
size :: Clause c -> Int
size c = L.length(getLits(c))

{-|
Fonction qui prend une clause en parametre et qui renvoie true si elle est vide et false sinon.
-}
isEmpty :: Clause c -> Bool
isEmpty c = size c == 0

{-|
Fonction qui prend une clause en parametre et qui renvoie true si elle est unitaire ( size == 1 )et false sinon.
-}
isUnitaryClause :: Clause c -> Bool
isUnitaryClause c = size c == 1