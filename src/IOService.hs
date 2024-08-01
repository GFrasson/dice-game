module IOService
  ( readValidDiceAmount,
    readDifficulty,
    readValidDieChoice,
    readValidFaceRotationChoice,
    printGameState,
    printEndGame
  )
where

import State ( GameState (GameState), Difficulty(..), isDieOnTable, Player (Person) )
import Die ( Die(..), getFace, possibleRotations )
import Text.Read (readMaybe)
import TerminalColor (Context(SystemContext), changeColorByContextSafe)

readValidDiceAmount :: IO Int
readValidDiceAmount = do
  diceAmount <- readDiceAmount

  if diceAmount < 2 || diceAmount > 100 then do
    putStrLn "O numero de dados deve ser um valor de 2 a 100"
    readValidDiceAmount
  else
    return diceAmount

readDiceAmount :: IO Int
readDiceAmount = do
  putStrLn "Digite o numero de dados:"
  diceAmountInput <- getLine
  let diceAmount = readMaybe diceAmountInput :: Maybe Int

  maybe readDiceAmount return diceAmount

readDifficulty :: IO Difficulty
readDifficulty = do
  putStrLn "[1] Facil"
  putStrLn "[2] Dificil"
  putStrLn "Escolha uma dificuldade:"
  difficultyOptionInput <- getLine
  let difficultyOption = readMaybe difficultyOptionInput :: Maybe Int
  let difficultyMaybe = getDifficultyFromOption difficultyOption

  maybe readDifficulty return difficultyMaybe

getDifficultyFromOption :: Maybe Int -> Maybe Difficulty
getDifficultyFromOption (Just 1) = Just Easy
getDifficultyFromOption (Just 2) = Just Hard
getDifficultyFromOption _ = Nothing

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

-- Função para imprimir o estado atual do jogo
printGameState :: GameState -> IO ()
printGameState (GameState dice _ _) = do
  changeColorByContextSafe SystemContext
  printDiceWithRotations dice

printEndGame :: Player -> IO ()
printEndGame player = do
  let winnerText = if player == Person then "Voce" else "Computador"
  putStrLn ("===== Fim de jogo! =====\n    " ++ winnerText ++ " venceu!" ++ "\n========================")