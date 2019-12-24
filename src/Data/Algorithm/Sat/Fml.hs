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

mkVar = Final . Var.mk
{-|
Fonction qui une liste de formule Fml et qui renvoie une formule qui est la conjonction des formules de la liste.
-}
multAnd :: [Fml a] -> Fml a
multAnd [x] = x
multAnd (x:xs) = And x (multAnd xs)

{-|
Fonction qui prends une liste de formule Fml et qui renvoie une formule qui est la conjonction des formules de la liste.
-}
multOr :: [Fml a] -> Fml a
multOr [x] = x
multOr (x:xs) = Or x (multOr xs)

{-|
Fonction qui prends une formule Fml et qui renvoie cette formule en format CNF.
-}
toCNF :: Fml a -> Fml a
toCNF (Final a ) = Final a
toCNF (Imply a b ) = toCNF( Or (Not a) (b) )
toCNF (Equiv a b ) = toCNF( Or (And (a) (b)) (And (Not a) (Not b)))
toCNF (XOr a b ) = toCNF( Or (And (a)(Not b)) (And (Not a) (b)))
toCNF (Not a) = f a
  where
  f (Final b ) = Not ( Final b )
  f (Not b) = toCNF (a)
  f (Or b c ) = toCNF( And(Not b) (Not c))
  f (And b c ) = toCNF( Or b c)
toCNF ( And a b ) = And (toCNF a) (toCNF b)
toCNF ( Or a b ) = multAnd[(Or c d) | c <- collectClauses(toCNF a), d <- collectClauses(toCNF b)]
  where 
  collectClauses (And a b) = collectClauses a ++ collectClauses b
  collectClauses a = [a]

{-|
Fonction qui prends une formule Fml et qui renvoie la liste des var de cette formule.
-}
vars :: (Eq a) => Fml a -> [Var.Var a]
vars = L.nub . go []
  where
  go acc (Final v) = v : acc
  go acc (And p q) = (vars p) ++ (vars q)
  go acc (Or p q) = (vars p) ++ (vars q)
  go acc (Not p) = vars p
  go acc (Imply p q) = (vars p) ++ (vars q)
  go acc (Equiv p q) = (vars p) ++ (vars q)
  go acc (XOr p q) = (vars p) ++ (vars q)

{-|
Fonction qui prends une formule Fml et qui renvoie une string qui correspond à son affichage.
-}
prettyPrinter :: (Show a) => Fml a -> String
prettyPrinter = printer ""
  where 
  printer acc (Final (Var.Var v))  = show v
  printer acc (And p q)   = "(" ++ prettyPrinter p ++ "." ++ prettyPrinter q ++ ")"
  printer acc (Or p q)    = "(" ++ prettyPrinter p ++ "+" ++ prettyPrinter q ++ ")"
  printer acc (Not p)     = "~" ++ prettyPrinter p
  printer acc (Imply p q) = "(" ++ prettyPrinter p ++ "=>" ++ prettyPrinter q ++ ")"
  printer acc (Equiv p q) = "(" ++ prettyPrinter p ++ "<=>" ++ prettyPrinter q ++ ")"
  printer acc (XOr p q)   = "(" ++ prettyPrinter p ++ "°" ++ prettyPrinter q ++ ")"

{-|
Fonction qui prends une formule Fml en format CNF et qui renvoie la formule avec les and à droite.
-}
toShapedFml :: Fml a -> Fml a
toShapedFml a = multAnd (aux a)
  where
  aux (And a b) = aux a ++ aux b
  aux a = [a]