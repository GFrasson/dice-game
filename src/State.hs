module State
  ( Player (..),
    Difficulty (..),
    GameState (..),
    initGameState,
    rotateDieInState,
    removeDieInState,
    togglePlayer,
    isDieOnTable,
  )
where

import Die (Die (..), getFace, removeDie, rotateDie)

data Player = Person | Computer deriving (Show, Eq)
data Difficulty = Easy | Hard deriving (Show, Eq)

data GameState = GameState {
  diceTable :: [Die],
  currentPlayer :: Player,
  currentDifficulty :: Difficulty
}

initGameState :: [Die] -> Difficulty -> GameState
initGameState randomDice difficulty = GameState {diceTable = randomDice, currentPlayer = Person, currentDifficulty = difficulty}

rotateDieInState :: Int -> Int -> GameState -> GameState
rotateDieInState oldFace newFace (GameState dice player difficulty) =
  GameState (rotateDie oldFace newFace dice) (togglePlayer player) difficulty

removeDieInState :: GameState -> GameState
removeDieInState (GameState dice player difficulty) = GameState (removeDie dice) (togglePlayer player) difficulty

togglePlayer :: Player -> Player
togglePlayer player
  | player == Person = Computer
  | otherwise = Person

isDieOnTable :: Die -> GameState -> Bool
isDieOnTable (Die dieFace) (GameState dice _ _) = dieFace `elem` map getFace dice
