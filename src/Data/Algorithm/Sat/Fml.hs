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
toCNF (Final a ) = a
toCNF (Imply a b ) = toCNF( Or (Not a) (b) )
toCNF (Equiv a b ) = toCNF( Or (And (a) (b)) (And (Not a) (Not b)))
toCNF (XOr a b ) = toCNF( Or (And (a)(Not b)) (And (Not a) (b)))
toCNF (Not a) = f a
  where
  f (Final b ) = Not b
  f (Not b) = toCNF (a)
  f (Or b c ) = toCNF( And(Not b) (Not c))
  f (And b c ) = toCNF( Or ( b c))
toCNF ( And a b ) = And (toCNF a) (toCNF b)
toCNF ( Or a b ) = 

vars :: Fml a -> [Var.Var a]
prettyPrinter :: Fml a -> String
atLeast :: (Eq t, Num t, Ord a) => t ->[Fml.Fml a]-> Fml.Fml a
anyOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
noneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
allOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
exactlyOneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a