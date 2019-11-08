module Data.Algorithm.Sat.Fml
  where

import qualified Data.List as L

import qualified Data.Algorithm.Sat.Var as Var

data Fml a = Or (Fml a) (Fml a)
  | And (Fml a) (Fml a)
  | Not (Fml a)
  | Imply (Fml a) (Fml a)
  | Equiv (Fml a) (Fml a)
  | XOr (Fml a) (Fml a)
  | Final (Var.Var a)
  deriving (Show, Eq, Ord)


toCNF :: Fml a -> Fml a
vars :: Fml a -> [Var.Var a]
prettyPrinter :: Fml a -> String



