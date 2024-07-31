module Game (startGame) where

import Die (Die (..), rollDice, rotateDie, removeDie, getFace, possibleRotations)
import Text.Read (readMaybe)
import System.Random (randomRIO)
import System.Console.ANSI
    ( setSGR,
      Color(Yellow, Green, Red),
      ColorIntensity(Dull, Vivid),
      ConsoleLayer(Foreground),
      SGR(SetColor) )
import Data.List (sort, delete, find)

data Player = Person | Computer deriving (Show, Eq)
data Difficulty = Easy | Hard deriving (Show, Eq)

data GameState = GameState {
  diceTable :: [Die],
  currentPlayer :: Player,
  currentDifficulty :: Difficulty
}

initGameState :: [Die] -> Difficulty -> GameState
initGameState randomDice difficulty = GameState { diceTable = randomDice, currentPlayer = Person, currentDifficulty = difficulty }

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

handleComputerMove :: Die -> GameState -> IO GameState
handleComputerMove selectedDie gameState = do
  if getFace selectedDie == 1
    then handleFaceOneComputer gameState
    else handleOtherFacesComputer selectedDie gameState

handleFaceOneComputer :: GameState -> IO GameState
handleFaceOneComputer gameState = do
  putStrLn "O computador removeu um dado de face 1."
  return $ removeDieInState gameState

handleOtherFacesComputer :: Die -> GameState -> IO GameState
handleOtherFacesComputer selectedDie gameState = do
  let possibleRotationsList = possibleRotations selectedDie
  randomIndex <- randomRIO (0, length possibleRotationsList - 1)
  let randomFace = possibleRotationsList !! randomIndex
  putStrLn $ "O computador rotacionou um dado de face " ++ show (getFace selectedDie) ++ " para a face " ++ show (randomFace)
  return $ rotateDieInState (getFace selectedDie) randomFace gameState

getComputerMove :: Difficulty -> (GameState -> IO GameState)
getComputerMove Easy = computerEasyMove
getComputerMove Hard = computerHardMove

computerEasyMove :: GameState -> IO GameState
computerEasyMove (GameState dice player difficulty) = do
  randomIndex <- randomRIO (0, length dice - 1) :: IO Int
  let selectedDie = dice !! randomIndex
  handleComputerMove selectedDie (GameState dice player difficulty)

computerHardMove :: GameState -> IO GameState
computerHardMove (GameState dice player difficulty) = do
  -- let faces = foldl (\acc current -> [(map getFace current)] ++ acc) [] (getPossibleConfigurations dice)
  -- putStrLn $ "Dados: " ++ show faces

  let bestComputerConfigurations = filter (\configuration -> not $ isWinnerConfiguration configuration) (getPossibleConfigurations dice)
  -- let best = foldl (\acc current -> [(map getFace current)] ++ acc) [] (bestComputerConfigurations)
  -- putStrLn $ "Dados: " ++ show best

  if null bestComputerConfigurations then
    computerEasyMove (GameState dice player difficulty)
  else do
    let chosenComputerConfiguration = head bestComputerConfigurations
    return (GameState chosenComputerConfiguration (togglePlayer player) difficulty)

isWinnerConfiguration :: [Die] -> Bool
isWinnerConfiguration [] = False
isWinnerConfiguration dice
  | length dice == 1 = getFace (head dice) `elem` [1, 3, 4, 6]
  | length dice == 2 = getFace (head dice) /= getFace (dice !! 1) && getFace (head dice) + getFace (dice !! 1) /= 7
  | otherwise = isWinnerConfiguration $ map (\x -> Die x) (removePairsSummingTo7 $ filterPairs [getFace x | x <- dice, getFace x `notElem` [2, 5]])

filterPairs :: Ord a => [a] -> [a]
filterPairs list = filterSortedPairs $ sort list

filterSortedPairs :: Eq a => [a] -> [a]
filterSortedPairs [] = []
filterSortedPairs [x] = [x]
filterSortedPairs (x:y:xs) = if x == y then filterSortedPairs xs else x:filterSortedPairs (y:xs)

removePairsSummingTo7 :: [Int] -> [Int]
removePairsSummingTo7 [] = []
removePairsSummingTo7 xs = 
    case findPair xs of
        Just (x, y) -> removePairsSummingTo7 (delete x (delete y xs))
        Nothing     -> xs
  where
    findPair [] = Nothing
    findPair (x:ys) = 
        case find ((== 7) . (+ x)) ys of
            Just y  -> Just (x, y)
            Nothing -> findPair ys

getPossibleConfigurations :: [Die] -> [[Die]]
getPossibleConfigurations dice = foldl (\acc current -> if getFace current /= 1 then map (\newFace -> rotateDie (getFace current) newFace dice) (possibleRotations current) ++ acc else removeDie dice : acc) [] dice

startGame :: IO ()
startGame = do
  putStrLn "---- Jogo de dados ----"

  diceAmount <- readDiceAmount
  randomDice <- rollDice diceAmount

  difficulty <- readDifficulty

  let gameState = initGameState randomDice difficulty
  gameEventLoop gameState

gameEventLoop :: GameState -> IO ()
gameEventLoop (GameState dice player difficulty) = do
  -- Verify End Game
  endGame <- verifyEndGame (GameState dice player difficulty)
  if endGame
    then putStrLn ("=== Fim de jogo! ====\n  =" ++ show (togglePlayer player) ++ " venceu! =" ++ "\n=====================")

  else do
    printGameState (GameState dice player difficulty)
    if player == Person
      then do
        setSGR [SetColor Foreground Vivid Green]

        selectedDie <- readValidDieChoice (GameState dice player difficulty)
        putStrLn $ show selectedDie

        newGameState <- handlePersonMove selectedDie (GameState dice player difficulty)
        gameEventLoop newGameState

    else do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "Vez do Computador"

      let computerMove = getComputerMove difficulty
      newGameState <- computerMove (GameState dice player difficulty)

      putStrLn "O computador finalizou a sua jogada."
      gameEventLoop newGameState



-- Função para imprimir o estado atual do jogo
printGameState :: GameState -> IO ()
printGameState (GameState dice _ _) = do
  setSGR [SetColor Foreground Dull Yellow]
  printDiceWithRotations dice

--printDice :: [Die] -> IO ()
--printDice dice = do
--  let faces = map getFace dice
--  putStrLn $ "Dados: " ++ show faces
--
--  mapM_ (\die -> putStrLn (show die ++ "\n")) dice

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