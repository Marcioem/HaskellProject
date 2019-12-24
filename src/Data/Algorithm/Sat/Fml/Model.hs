module Data.Algorithm.Sat.Fml.Model where

import qualified Data.Algorithm.Sat.Fml as Fml

{-|
Fonction qui prend un entier k et une liste de formules Fml et qui renvoie une fml satisfiable
lorsqu'au moins k fml de la liste sont satisfiables.
-}
atLeast :: (Eq t, Num t, Ord a) => t -> [Fml.Fml a] -> Fml.Fml a
atLeast k xs = Fml.multOr [Fml.multAnd l | let ss = subsets k xs, l <- ss, not (null ss)]

{-|
Fonction qui prend un entier k et une liste de formules Fml et qui renvoie une liste de liste de formule 
qui correspond à la liste des combinaisons de au moins k fml.
-}
subsets :: (Eq t, Num t, Ord a) => t -> [Fml.Fml a] -> [[Fml.Fml a]]
subsets 0 [x] = [[x], []]
subsets 0 (x:xs) = map (x:) (subsets 0 xs) ++ subsets 0 xs
subsets 1 [x] = [[x]]
subsets k [x] = []
subsets k (x:xs) = map (x:) (subsets (k-1) xs) ++ subsets k xs

{-|
Fonction qui prend une liste de formules Fml et qui renvoie une fml satisfiable
lorsqu'au moins 1 fml de la liste est satisfiable.
-}
anyOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
anyOf xs = atLeast 1 xs

{-|
Fonction qui prend une liste de formules Fml et qui renvoie une fml satisfiable
lorsqu'aucune fml de la liste n'est satisfiable.
-}
noneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
noneOf xs = allOf [Fml.Not x | x <- xs]

{-|
Fonction qui prend une liste de formules Fml et qui renvoie une fml satisfiable
lorsque toutes les fml de la liste sont satisfiables.
-}
allOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
allOf xs = atLeast (length xs) xs

{-|
Fonction qui prend une liste de formules Fml et qui renvoie une fml satisfiable
lorsqu'exactement 1 fml de la liste est satisfiable.
-}
exactlyOneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
exactlyOneOf xs = Fml.multOr xs