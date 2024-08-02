-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro Couto... / Matrícula: ...

module Die (Die (..), rollDice, possibleRotations, oppositeFace, rotateDie, removeDie, getFace) where
import System.Random (randomRIO)

data Die = Die Int deriving (Eq, Ord)

instance Show Die where
  show (Die face) = faceToString face

faceToString :: Int -> String
faceToString face = let
  top = " ---------\n"
  firstRow
    | face == 1 = "|         |\n"
    | face == 2 || face == 3 = "|  o      |\n"
    | otherwise = "|  o   o  |\n"
  secondRow
    | face == 6 = "|  o   o  |\n"
    | even face = "|         |\n"
    | otherwise = "|    o    |\n"
  thirdRow
    | face == 1 = "|         |\n"
    | face == 2 || face == 3 = "|      o  |\n"
    | otherwise = "|  o   o  |\n"
  bottom = " ---------"

  in top ++ firstRow ++ secondRow ++ thirdRow ++ bottom

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
