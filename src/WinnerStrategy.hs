-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro Couto... / Matrícula: ...

module WinnerStrategy(isWinnerConfiguration, getPossibleConfigurations) where

import Die
    ( Die(..), getFace, possibleRotations, rotateDie, removeDie )
import Data.List ( find, delete, sort )

isWinnerConfiguration :: [Die] -> Bool
isWinnerConfiguration dice = do
  if length dice <= 2 then
    isWinnerBaseConfiguration dice
  else
    isWinnerBaseConfiguration $ filterConfiguration dice

isWinnerBaseConfiguration :: [Die] -> Bool
isWinnerBaseConfiguration [] = False
isWinnerBaseConfiguration dice
  | length dice == 1 = getFace (head dice) `elem` [1, 3, 4, 6]
  | length dice == 2 = getFace (head dice) /= getFace (dice !! 1) && getFace (head dice) + getFace (dice !! 1) /= 7
  | otherwise = False

filterConfiguration :: [Die] -> [Die]
filterConfiguration dice = map Die (removePairsSumSeven $ filterEqualPairs [getFace x | x <- dice, getFace x `notElem` [2, 5]])

removePairsSumSeven :: [Int] -> [Int]
removePairsSumSeven [] = []
removePairsSumSeven xs = maybe xs (\(x, y) -> removePairsSumSeven $ delete x $ delete y xs) (findPairSumSeven xs)

findPairSumSeven :: [Int] -> Maybe (Int, Int)
findPairSumSeven [] = Nothing
findPairSumSeven (x:xs) = do
  let pair = find (\y -> x + y == 7) xs
  maybe (findPairSumSeven xs) (\y -> return (x, y)) pair

filterEqualPairs :: Ord a => [a] -> [a]
filterEqualPairs list = filterSortedEqualPairs $ sort list

filterSortedEqualPairs :: Eq a => [a] -> [a]
filterSortedEqualPairs [] = []
filterSortedEqualPairs [x] = [x]
filterSortedEqualPairs (x:y:xs) = if x == y then filterSortedEqualPairs xs else x:filterSortedEqualPairs (y:xs)

getPossibleConfigurations :: [Die] -> [[Die]]
getPossibleConfigurations dice = foldl
  (\acc current ->
    if getFace current /= 1 then
      map (\newFace -> rotateDie (getFace current) newFace dice) (possibleRotations current) ++ acc
    else
      removeDie dice : acc
  ) [] dice
