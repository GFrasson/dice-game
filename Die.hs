module Die (rollDie) where
-- import System.Random (randomRIO)

-- randomIntInRange :: Int -> Int -> Int -> Int
-- randomIntInRange min max seed = (hash1 `mod` (max - min + 1)) + min
--     where
--         hash1 = (seed * 1103515245 + 12345) `mod` 2147483648

data Die = Die {
  face :: Int
} deriving Show

-- rollDie = Die $ randomRIO (1, 6)
-- rollDie = Die (randomIntInRange 0 100 123456)
rollDie = Die 3

