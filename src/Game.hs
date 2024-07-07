module Game (startGame) where

-- import Control.Monad.State
-- import Data.Functor.Identity

import Die (Die (..), rollDice, rotateDie, removeDie, getFace, possibleRotations)
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isJust, isNothing)

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

isDieOnTable :: Die -> GameState -> Bool
isDieOnTable (Die dieFace) (GameState dice _) = dieFace `elem` map getFace dice

readDiceAmount :: IO Int
readDiceAmount = do
  putStrLn "Digite o numero de dados:"
  diceAmountInput <- getLine
  let diceAmount = readMaybe diceAmountInput :: Maybe Int

  maybe readDiceAmount return diceAmount

readValidDieChoice :: GameState -> IO Die
readValidDieChoice gameState = do
  die <- readDieChoice

  if not $ isDieOnTable die gameState then do
    putStrLn $ "O dado de face " ++ show (getFace die) ++ " nao esta na mesa."
    readValidDieChoice gameState
  else
    return die

readDieChoice :: IO Die
readDieChoice = do
  putStrLn "Digite a face do dado com o qual deseja interagir:"
  dieFaceInput <- getLine
  let dieFace = readMaybe dieFaceInput :: Maybe Int

  maybe readDieChoice (\face -> return (Die face)) dieFace

readValidFaceRotationChoice :: Die -> GameState -> IO Int
readValidFaceRotationChoice die gameState = do
  printPossibleRotations die
  newFace <- readFaceRotationChoice

  let rotations = possibleRotations die

  if newFace `notElem` rotations then do
    putStrLn $ "Nao eh possivel rotacionar o dado de face " ++ show (getFace die) ++ " para a face " ++ show newFace
    readValidFaceRotationChoice die gameState
  else
    return newFace

readFaceRotationChoice :: IO Int
readFaceRotationChoice = do
  putStrLn "Digite a nova face do dado para rotaciona-lo:"
  newDieFaceInput <- getLine
  let newDieFace = readMaybe newDieFaceInput :: Maybe Int

  maybe readFaceRotationChoice return newDieFace

startGame :: IO ()
startGame = do
  putStrLn "---- Jogo de dados ----"

  diceAmount <- readDiceAmount
  randomDice <- rollDice diceAmount

  let gameState = initGameState randomDice
  gameEventLoop gameState

gameEventLoop :: GameState -> IO ()
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

        selectedDie <- readValidDieChoice (GameState dice player)
        newFace <- readValidFaceRotationChoice selectedDie (GameState dice player)

        let newGameState = rotateDieInState (getFace selectedDie) newFace (GameState dice player)

        putStrLn $ "Dados: " ++ show (map getFace (diceTable newGameState))
        
        gameEventLoop newGameState

    else do
      putStrLn "Vez do Computador"

-- Função para imprimir o estado atual do jogo
printGameState :: GameState -> IO ()
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
verifyEndGame (GameState dice _) = return $ length dice == 0
