module Main (main) where

import Game (startGame)

main :: IO ()
main = startGame


-- data Die = Die Int deriving Show

-- getFace :: Die -> Int
-- getFace (Die face) = face

-- rotateDie :: Int -> Int -> [Die] -> [Die]
-- rotateDie _ _ [] = []
-- rotateDie oldFace newFace (x:xs)
--   | getFace x == oldFace = Die newFace : xs
--   | otherwise = x : rotateDie oldFace newFace xs

-- removeDie :: [Die] -> [Die]
-- removeDie [] = []
-- removeDie (x:xs)
--   | getFace x == 1 = xs
--   | otherwise = x:removeDie xs


-- minFace :: Int
-- minFace = 1

-- maxFace :: Int
-- maxFace = 6

-- isValidDie :: Die -> Bool
-- isValidDie (Die face) = face >= minFace && face <= maxFace


-- oppositeFace :: Die -> Int
-- oppositeFace die
--   | isValidDie die = 7 - getFace die
--   | otherwise = 0


-- possibleRotations :: Die -> [Int]
-- possibleRotations die
--   | isValidDie die = [x | x <- [minFace..maxFace], x < getFace die && x /= oppositeFace die]
--   | otherwise = []


-- getPossibleConfigurations :: [Die] -> [[Die]]
-- getPossibleConfigurations [] = []
-- getPossibleConfigurations dice = map (\newFace -> rotateDie (getFace $ head dice) newFace dice) (possibleRotations $ head dice)

-- [4, 2, 1]
-- [otações 4 ->  3, 2]
-- [ 3, 2, 1] [  2, 2, 1], [ 1, 2, 1]]

-- [Die 1, Die 2, Die 2, Die 3, Die 3]
-- [Die 1, Die 2, Die 3]


-- [Die 1, Die 2, Die 2, Die 2, Die 3]
-- [Die 1, Die 2, Die 2, Die 1, Die 3]
-- [Die 1, Die 2, Die 2, Die 3, Die 2]
-- [Die 1, Die 2, Die 2, Die 3, Die 1]
-- [Die 1, Die 2, Die 2, Die 3, Die 2]
-- [Die 1, Die 2, Die 2, Die 3, Die 1]
-- []

-- getPossibleConfigurations :: [Die] -> [[Die]]
-- getPossibleConfigurations dice = foldl (\acc current -> if getFace current /= 1 then map (\newFace -> rotateDie (getFace current) newFace dice) (possibleRotations current) ++ acc else removeDie dice : acc) [] dice
