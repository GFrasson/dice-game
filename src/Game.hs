module Game (startGame) where

-- import Control.Monad.State
-- import Data.Functor.Identity

import Die (Die, rollDice, rotateDie, removeDie, getFace, possibleRotations)
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isJust)

data Player = Person | Computer deriving (Show, Eq)
data Move = Rotate | Remove deriving (Show, Eq)

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
rotateDieInState oldFace newFace (GameState dice player) =
  GameState (rotateDie oldFace newFace dice) (togglePlayer player)

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
  endGame <- verifyEndGame (GameState dice player)
  
  if endGame 
    then putStrLn ("=== Fim de jogo! ====\n  =" ++ show player ++ " venceu! =" ++ "\n=====================")
  
  else do
    if player == Person
      then do
        printGameState (GameState dice player)

        mapM_ printPossibleRotations dice

        -- Show possible moves
      
        putStrLn "Escolha uma jogada:"

        -- Show Dice
      
        putStrLn "Escolha um dado:"

        -- Show possible rotations

        putStrLn "Escolha a nova face para cima:"

    else do
      putStrLn "Vez do Computador"

    -- Update GameState
    --gameEventLoop (GameState dice player)

-- Função para imprimir o estado atual do jogo
printGameState (GameState dice player) = do
  let faces = map getFace dice
  putStrLn $ "Dados: " ++ show faces
  putStrLn $ "Jogador atual: " ++ show player

-- Função para imprimir as rotações possíveis para um dado
printPossibleRotations :: Die -> IO ()
printPossibleRotations die = do
  let face = getFace die
  let rotations = possibleRotations die
  putStrLn $ "Dado com face " ++ show face ++ ": possíveis rotações = " ++ show rotations

-- Função para verificar se o jogo terminou
verifyEndGame :: GameState -> IO Bool
verifyEndGame (GameState dice _) = return $ listLength dice == 0

-- Usando length para obter o comprimento de uma lista
listLength :: [a] -> Int
listLength lst = length lst