module Data.Algorithm.Sat.Fml.Model where

import qualified Data.Algorithm.Sat.Fml as Fml

atLeast :: (Eq t, Num t, Ord a) => t -> [Fml.Fml a] -> Fml.Fml a
atLeast k xs = Fml.multOr [Fml.multAnd l | let ss = subsets k xs, l <- ss, not (null ss)]

subsets :: (Eq t, Num t, Ord a) => t -> [Fml.Fml a] -> [[Fml.Fml a]]
subsets 0 [x] = [[x], []]
subsets 0 (x:xs) = map (x:) (subsets 0 xs) ++ subsets 0 xs
subsets 1 [x] = [[x]]
subsets k [x] = []
subsets k (x:xs) = map (x:) (subsets (k-1) xs) ++ subsets k xs

anyOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
anyOf xs = atLeast 1 xs

noneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
noneOf xs = allOf [Fml.Not x | x <- xs]

allOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
allOf xs = atLeast (length xs) xs

exactlyOneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
exactlyOneOf xs = Fml.multOr xs