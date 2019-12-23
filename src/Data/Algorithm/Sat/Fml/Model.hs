module Data.Algorithm.Sat.Fml.Model where

import qualified Data.Algorithm.Sat.Fml as Fml

subset :: (Eq t, Num t, Ord a) => t -> [Fml.Fml a] -> [[Fml.Fml a]]
subset 0 [x] = [[x], []]
subset 0 (x:xs) = map (x:) (subset (0) xs) ++ (subset 0 xs)
subset 1 [x] = [[x]]
subset k [x] = []
subset k (x:xs) = map (x:) (subset (k-1) xs) ++ (subset k xs)

atLeast :: (Eq t, Num t, Ord a) => t -> [Fml.Fml a] -> Fml.Fml a
atLeast k t = Fml.multOr [Fml.multAnd fs | fs <- subset k t]

anyOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
anyOf xs = atLeast 1 xs

noneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
noneOf xs = allOf [Fml.Not x | x <- xs]

allOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
allOf xs = atLeast (length xs) xs

exactlyOneOf :: (Ord a) => [Fml.Fml a] -> Fml.Fml a
exactlyOneOf xs = Fml.multOr xs