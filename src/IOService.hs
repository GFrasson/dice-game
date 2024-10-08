-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro do Couto Filgueiras / Matrícula: 201935015

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

-- Lê a quantidade de dados do usuário, garantindo que esteja dentro do intervalo permitido (2 a 100).
readValidDiceAmount :: IO Int
readValidDiceAmount = do
  diceAmount <- readDiceAmount
  let minValue = 2
  let maxValue = 100

  if diceAmount < minValue || diceAmount > maxValue then do
    putStrLn $ "O numero de dados deve ser um valor de " ++ show minValue ++ " a " ++ show maxValue
    readValidDiceAmount
  else
    return diceAmount

-- Lê a quantidade de dados fornecida pelo usuário como entrada e tenta convertê-la para um número inteiro.
readDiceAmount :: IO Int
readDiceAmount = do
  putStrLn "Digite o numero de dados:"
  diceAmountInput <- getLine
  let diceAmount = readMaybe diceAmountInput :: Maybe Int

  maybe readDiceAmount return diceAmount

-- Lê a dificuldade do jogo fornecida pelo usuário e a converte para um valor do tipo Difficulty.
readDifficulty :: IO Difficulty
readDifficulty = do
  putStrLn "[1] Facil"
  putStrLn "[2] Dificil"
  putStrLn "Escolha uma dificuldade:"
  difficultyOptionInput <- getLine
  let difficultyOption = readMaybe difficultyOptionInput :: Maybe Int
  let difficultyMaybe = getDifficultyFromOption difficultyOption

  maybe readDifficulty return difficultyMaybe

-- Converte a opção de dificuldade fornecida pelo usuário para um valor do tipo Difficulty..
getDifficultyFromOption :: Maybe Int -> Maybe Difficulty
getDifficultyFromOption (Just 1) = Just Easy
getDifficultyFromOption (Just 2) = Just Hard
getDifficultyFromOption _ = Nothing

-- Lê a escolha do usuário sobre qual dado deseja interagir e valida se o dado está na mesa.
readValidDieChoice :: GameState -> IO Die
readValidDieChoice gameState = do
  die <- readDieChoice

  if not $ isDieOnTable die gameState then do
    putStrLn $ "O dado de face " ++ show (getFace die) ++ " nao esta na mesa."
    readValidDieChoice gameState
  else
    return die

-- Lê a face do dado fornecida pelo usuário e tenta convertê-la para um valor do tipo Die.
readDieChoice :: IO Die
readDieChoice = do
  putStrLn "Digite a face do dado com o qual deseja interagir:"
  dieFaceInput <- getLine
  let dieFace = readMaybe dieFaceInput :: Maybe Int

  maybe readDieChoice (\face -> return (Die face)) dieFace

-- Lê a nova face para a rotação do dado e valida se a rotação é possível.
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

-- Realiza a leitura da rotação que o usuário deseja realizar
readFaceRotationChoice :: IO Int
readFaceRotationChoice = do
  putStrLn "Digite a nova face do dado para rotaciona-lo:"
  newDieFaceInput <- getLine
  let newDieFace = readMaybe newDieFaceInput :: Maybe Int

  maybe readFaceRotationChoice return newDieFace

-- Imprime os dados de uma lista, junto com suas rotações possíveis
printDiceWithRotations :: [Die] -> IO ()
printDiceWithRotations dice = do
  let faces = map getFace dice
  putStrLn $ "Dados: " ++ show faces

  mapM_ (\die -> putStrLn (show die ++ "\n" ++ getPossibleRotationsText die ++ "\n")) dice

-- Imprime as possíveis rotações de um dado
printPossibleRotations :: Die -> IO ()
printPossibleRotations die = do
  let rotations = possibleRotations die
  putStrLn $ "Possíveis rotações = " ++ show rotations

-- Retorna texto com possíveis rotações de um dado em tela
getPossibleRotationsText :: Die -> String
getPossibleRotationsText die = let
  rotations = possibleRotations die
  in "Possíveis rotações = " ++ show rotations

-- Imprime o estado atual do jogo
printGameState :: GameState -> IO ()
printGameState (GameState dice _ _) = do
  changeColorByContextSafe SystemContext
  printDiceWithRotations dice

-- Imprime vencedor do jogo
printEndGame :: Player -> IO ()
printEndGame player = do
  let winnerText = if player == Person then "Voce" else "Computador"
  putStrLn ("===== Fim de jogo! =====\n    " ++ winnerText ++ " venceu!" ++ "\n========================")