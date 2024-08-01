module WinnerStrategy(isWinnerConfiguration, getPossibleConfigurations) where

import Die
    ( Die(..), getFace, possibleRotations, rotateDie, removeDie )
import Data.List ( find, delete, sort )

isWinnerConfiguration :: [Die] -> Bool
isWinnerConfiguration [] = False
isWinnerConfiguration dice
  | length dice == 1 = getFace (head dice) `elem` [1, 3, 4, 6]
  | length dice == 2 = getFace (head dice) /= getFace (dice !! 1) && getFace (head dice) + getFace (dice !! 1) /= 7
  | otherwise = isWinnerConfiguration $ map (\x -> Die x) (removePairsSumSeven $ filterPairs [getFace x | x <- dice, getFace x `notElem` [2, 5]])

removePairsSumSeven :: [Int] -> [Int]
removePairsSumSeven [] = []
removePairsSumSeven xs = maybe xs (\(x, y) -> removePairsSumSeven $ delete x $ delete y xs) (findPairSumSeven xs)

findPairSumSeven :: [Int] -> Maybe (Int, Int)
findPairSumSeven [] = Nothing
findPairSumSeven (x:ys) = do
  y <- find (\y -> x + y == 7) ys
  return (x, y)

filterPairs :: Ord a => [a] -> [a]
filterPairs list = filterSortedPairs $ sort list

filterSortedPairs :: Eq a => [a] -> [a]
filterSortedPairs [] = []
filterSortedPairs [x] = [x]
filterSortedPairs (x:y:xs) = if x == y then filterSortedPairs xs else x:filterSortedPairs (y:xs)

getPossibleConfigurations :: [Die] -> [[Die]]
getPossibleConfigurations dice = foldl (\acc current -> if getFace current /= 1 then map (\newFace -> rotateDie (getFace current) newFace dice) (possibleRotations current) ++ acc else removeDie dice : acc) [] dice
