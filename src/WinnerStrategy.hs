-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro do Couto Filgueiras / Matrícula: 201935015

module WinnerStrategy(isWinnerConfiguration, getPossibleConfigurations) where

import Die
    ( Die(..), getFace, possibleRotations, rotateDie, removeDie )
import Data.List ( find, delete, sort )

-- Verifica se a configuração atual dos dados é uma configuração vencedora, aplicando regras para caso tenha até dois dados ou caso tenha mais
isWinnerConfiguration :: [Die] -> Bool
isWinnerConfiguration dice = do
  if length dice <= 2 then
    isWinnerBaseConfiguration dice
  else
    isWinnerBaseConfiguration $ filterConfiguration dice

-- Verifica se uma configuração de até dois dados é vencedora
isWinnerBaseConfiguration :: [Die] -> Bool
isWinnerBaseConfiguration [] = False
isWinnerBaseConfiguration dice
  | length dice == 1 = getFace (head dice) `elem` [1, 3, 4, 6]
  | length dice == 2 = getFace (head dice) /= getFace (dice !! 1) && getFace (head dice) + getFace (dice !! 1) /= 7
  | otherwise = False

-- Filtra a lista de dados removendo pares iguais e também pares que somados resultam em sete (ignorando os dados de face 2 e 5)
filterConfiguration :: [Die] -> [Die]
filterConfiguration dice = map Die (removePairsSumSeven $ filterEqualPairs [getFace x | x <- dice, getFace x `notElem` [2, 5]])

-- Remove pares de dados cuja soma é sete.
removePairsSumSeven :: [Int] -> [Int]
removePairsSumSeven [] = []
removePairsSumSeven xs = maybe xs (\(x, y) -> removePairsSumSeven $ delete x $ delete y xs) (findPairSumSeven xs)

-- Encontra par de dados cuja soma da sua face é sete.
findPairSumSeven :: [Int] -> Maybe (Int, Int)
findPairSumSeven [] = Nothing
findPairSumSeven (x:xs) = do
  let pair = find (\y -> x + y == 7) xs
  maybe (findPairSumSeven xs) (\y -> return (x, y)) pair

-- Filtra pares iguais em uma lista de números ordenada.
filterEqualPairs :: Ord a => [a] -> [a]
filterEqualPairs list = filterSortedEqualPairs $ sort list

-- Remove pares consecutivos iguais de uma lista ordenada.
filterSortedEqualPairs :: Eq a => [a] -> [a]
filterSortedEqualPairs [] = []
filterSortedEqualPairs [x] = [x]
filterSortedEqualPairs (x:y:xs) = if x == y then filterSortedEqualPairs xs else x:filterSortedEqualPairs (y:xs)

-- Gera todas as possíveis configurações dos dados, considerando rotações e remoções.
getPossibleConfigurations :: [Die] -> [[Die]]
getPossibleConfigurations dice = foldl
  (\acc current ->
    if getFace current /= 1 then
      map (\newFace -> rotateDie (getFace current) newFace dice) (possibleRotations current) ++ acc
    else
      removeDie dice : acc
  ) [] dice
