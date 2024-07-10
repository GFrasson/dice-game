module Game (startGame) where

import Die (Die (..), rollDice, rotateDie, removeDie, getFace, possibleRotations)
import Text.Read (readMaybe)
import System.Random (randomRIO)

data Player = Person | Computer deriving (Show, Eq)
data Difficulty = Easy | Hard deriving (Show, Eq)

data GameState = GameState {
  diceTable :: [Die],
  currentPlayer :: Player,
  currentDifficulty :: Difficulty
}

initGameState :: [Die] -> GameState
initGameState randomDice = GameState { diceTable = randomDice, currentPlayer = Person, currentDifficulty = Easy }

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

handlePersonMove :: Die -> GameState -> IO GameState
handlePersonMove selectedDie gameState = do
  if getFace selectedDie == 1
    then return $ handleFaceOnePerson selectedDie gameState
    else handleOtherFacesPerson selectedDie gameState

handleFaceOnePerson :: Die -> GameState -> GameState
handleFaceOnePerson _ gameState = let
  in removeDieInState gameState

handleOtherFacesPerson :: Die -> GameState -> IO GameState
handleOtherFacesPerson selectedDie gameState = do
  newFace <- readValidFaceRotationChoice selectedDie gameState
  return $ rotateDieInState (getFace selectedDie) newFace gameState

handleOtherFacesComputer :: Die -> GameState -> IO GameState
handleOtherFacesComputer selectedDie gameState = do
  let possibleRotationsList = possibleRotations selectedDie
  randomIndex <- randomRIO (0, length possibleRotationsList - 1)
  putStrLn $ "O computador rotacionou um dado de face " ++ show (getFace selectedDie)
  let randomFace = possibleRotationsList !! randomIndex
  return $ rotateDieInState (getFace selectedDie) randomFace gameState

handleComputerMove :: Die -> GameState -> IO GameState
handleComputerMove selectedDie gameState = do
  if getFace selectedDie == 1 then do
    putStrLn "O computador removeu um dado."
    return $ removeDieInState gameState
  else
    handleOtherFacesComputer selectedDie gameState

-- handleFaceOneComputer

-- chooseComputerMove :: GameState -> IO ()

computerEasyMove :: GameState -> IO GameState
computerEasyMove (GameState dice player difficulty) = do
  randomIndex <- randomRIO (0, length dice - 1) :: IO Int
  let selectedDie = dice !! randomIndex
  handleComputerMove selectedDie (GameState dice player difficulty)

-- computerHardMove :: GameState -> IO ()

startGame :: IO ()
startGame = do
  putStrLn "---- Jogo de dados ----"

  diceAmount <- readDiceAmount
  randomDice <- rollDice diceAmount

  let gameState = initGameState randomDice
  gameEventLoop gameState

gameEventLoop :: GameState -> IO ()
gameEventLoop (GameState dice player difficulty) = do
  -- Verify End Game
  endGame <- verifyEndGame (GameState dice player difficulty)
  if endGame
    then putStrLn ("=== Fim de jogo! ====\n  =" ++ show (togglePlayer player) ++ " venceu! =" ++ "\n=====================")

  else do
    if player == Person
      then do
        printGameState (GameState dice player difficulty)

        selectedDie <- readValidDieChoice (GameState dice player difficulty)
        putStrLn $ show selectedDie

        newGameState <- handlePersonMove selectedDie (GameState dice player difficulty)
        printDice $ diceTable newGameState
        gameEventLoop newGameState

    else do
      putStrLn "Vez do Computador"

      newGameState <- computerEasyMove (GameState dice player difficulty)
      -- handleComputerMove (GameState dice player difficulty)
      printDice $ diceTable newGameState
      gameEventLoop newGameState
      
      putStrLn "O computador finalizou a sua jogada."


-- Função para imprimir o estado atual do jogo
printGameState :: GameState -> IO ()
printGameState (GameState dice player _) = do
  printDiceWithRotations dice
  putStrLn $ "Jogador atual: " ++ show player ++ "\n"
  putStrLn $ "======================================================\n"

printDice :: [Die] -> IO ()
printDice dice = do
  let faces = map getFace dice
  putStrLn $ "Dados: " ++ show faces

  mapM_ (\die -> putStrLn (show die ++ "\n")) dice

printDiceWithRotations :: [Die] -> IO ()
printDiceWithRotations dice = do
  let faces = map getFace dice
  putStrLn $ "Dados: " ++ show faces

  mapM_ (\die -> putStrLn (show die ++ "\n" ++ getPossibleRotationsText die ++ "\n")) dice

-- Função para imprimir as rotações possíveis para um dado
printPossibleRotations :: Die -> IO ()
printPossibleRotations die = do
  let rotations = possibleRotations die
  putStrLn $ "Possíveis rotações = " ++ show rotations

getPossibleRotationsText :: Die -> String
getPossibleRotationsText die = let
  rotations = possibleRotations die
  in "Possíveis rotações = " ++ show rotations

-- Função para verificar se o jogo terminou
verifyEndGame :: GameState -> IO Bool
verifyEndGame (GameState dice _ _) = return $ length dice == 0
