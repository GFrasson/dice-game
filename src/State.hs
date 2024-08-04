-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro do Couto Filgueiras / Matrícula: 201935015

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

-- Instancia estado inicial do jogo de acordo com o número de dados e dificuldade selecionada 
initGameState :: [Die] -> Difficulty -> GameState
initGameState randomDice difficulty = GameState {
  diceTable = randomDice,
  currentPlayer = if difficulty == Easy then Person else Computer,
  currentDifficulty = difficulty
}

-- Rotaciona dado no estado
rotateDieInState :: Int -> Int -> GameState -> GameState
rotateDieInState oldFace newFace (GameState dice player difficulty) =
  GameState (rotateDie oldFace newFace dice) (togglePlayer player) difficulty

-- Remove dado no estado
removeDieInState :: GameState -> GameState
removeDieInState (GameState dice player difficulty) = GameState (removeDie dice) (togglePlayer player) difficulty

-- Alterna jogador
togglePlayer :: Player -> Player
togglePlayer player
  | player == Person = Computer
  | otherwise = Person

-- Verifica se face informada está na lista de dados
isDieOnTable :: Die -> GameState -> Bool
isDieOnTable (Die dieFace) (GameState dice _ _) = dieFace `elem` map getFace dice
