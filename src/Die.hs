module Die (Die (..), rollDice, possibleRotations, oppositeFace, rotateDie, removeDie, getFace) where
import System.Random (randomRIO)

data Die = Die Int deriving Show

getFace :: Die -> Int
getFace (Die face) = face

minFace :: Int
minFace = 1

maxFace :: Int
maxFace = 6

isValidDie :: Die -> Bool
isValidDie (Die face) = face >= minFace && face <= maxFace

oppositeFace :: Die -> Int
oppositeFace die
  | isValidDie die = 7 - getFace die
  | otherwise = 0

possibleRotations :: Die -> [Int]
possibleRotations die
  | isValidDie die = [x | x <- [minFace..maxFace], x < getFace die && x /= oppositeFace die]
  | otherwise = []

rotateDie :: Int -> Int -> [Die] -> [Die]
rotateDie _ _ [] = []
rotateDie oldFace newFace (x:xs)
  | getFace x == oldFace = Die newFace : xs
  | otherwise = x : rotateDie oldFace newFace xs
  
-- rotateDieByIndex :: Int -> Int -> [Die] -> [Die]
-- rotateDieByIndex _ _ [] = []
-- rotateDieByIndex dieIndex newFace (x:xs)
--   | dieIndex == 0 = Die newFace:xs
--   | otherwise = x:rotateDieByIndex (dieIndex - 1) newFace xs

removeDie :: [Die] -> [Die]
removeDie [] = []
removeDie (x:xs)
  | getFace x == 1 = xs
  | otherwise = x:removeDie xs

rollDie :: IO Die
rollDie = do
  randomInteger <- randomRIO (minFace, maxFace) :: IO Int
  return (Die randomInteger)

rollDice :: Int -> IO [Die]
rollDice 0 = return []
rollDice amount = do
  die <- rollDie
  dice <- rollDice (amount - 1)
  return (die : dice)
