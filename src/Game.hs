module Game (startGame) where

-- import Control.Monad.State
-- import Data.Functor.Identity

import Die (Die, rollDice, rotateDie, removeDie)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

data Player = Person | Computer deriving (Show, Eq)

data GameState = GameState {
  diceTable :: [Die],
  currentPlayer :: Player
}

initGameState :: [Die] -> GameState
initGameState randomDice = GameState { diceTable = randomDice, currentPlayer = Person }

togglePlayer :: Player -> Player
togglePlayer player
  | player == Person = Computer
  | otherwise = Person

-- rotateDieInState :: Int -> Int -> State GameState ()
-- rotateDieInState dieIndex newFace = StateT
--   $ \(GameState dice player)
--   -> Identity ((), GameState (rotateDie dieIndex newFace dice) (togglePlayer player))

rotateDieInState :: Int -> Int -> GameState -> GameState
rotateDieInState dieIndex newFace (GameState dice player) =
  GameState (rotateDie dieIndex newFace dice) (togglePlayer player)

-- removeDieInState :: State GameState ()
-- removeDieInState = StateT
--   $ \(GameState dice player)
--   -> Identity ((), GameState (removeDie dice) (togglePlayer player))

removeDieInState :: GameState -> GameState
removeDieInState (GameState dice player) = GameState (removeDie dice) (togglePlayer player)

readDiceAmount :: IO Int
readDiceAmount = do
  putStrLn "Digite o numero de dados:"
  diceAmountInput <- getLine
  let diceAmount = readMaybe diceAmountInput :: Maybe Int

  if diceAmount == Nothing then
    readDiceAmount
  else
    return (fromJust diceAmount)

startGame :: IO ()
startGame = do
  putStrLn "---- Jogo de dados ----"

  diceAmount <- readDiceAmount
  randomDice <- rollDice diceAmount

  let gameState = initGameState randomDice
  gameEventLoop gameState

gameEventLoop (GameState dice player) = do
  -- Verify End Game

  -- Show possible moves
  
  putStrLn "Escolha uma jogada:"

  -- Show Dice
  
  putStrLn "Escolha um dado:"

  -- Show possible rotations

  putStrLn "Escolha a nova face para cima:"

  -- Update GameState
  -- Recursion
