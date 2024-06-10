module Die (rollDie) where
import System.Random (randomRIO)

data Die = Die {
  face :: Int
} deriving Show

rollDie :: IO Int
rollDie = randomRIO (1, 6) :: IO Int
