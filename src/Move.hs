-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro do Couto Filgueiras / Matrícula: 201935015

module Move (handlePersonMove, getComputerMove) where

import Die ( Die (..), getFace, possibleRotations )
import State
    ( GameState(GameState),
      Difficulty(..),
      rotateDieInState,
      removeDieInState,
      togglePlayer )
import IOService ( readValidFaceRotationChoice )
import System.Random (randomRIO)
import WinnerStrategy
    ( isWinnerConfiguration, getPossibleConfigurations )

-- Lida com o movimento de um jogador humano com base no dado selecionado e no estado do jogo.
handlePersonMove :: Die -> GameState -> IO GameState
handlePersonMove selectedDie gameState = do
  if getFace selectedDie == 1
    then return $ handleFaceOnePerson selectedDie gameState
    else handleOtherFacesPerson selectedDie gameState

-- Lida com a remoção de um dado com uma face 1 no movimento do jogador.
handleFaceOnePerson :: Die -> GameState -> GameState
handleFaceOnePerson _ gameState = let
  in removeDieInState gameState

-- Lida com a rotação de um dado com uma face diferente de 1 no movimento do jogador.
handleOtherFacesPerson :: Die -> GameState -> IO GameState
handleOtherFacesPerson selectedDie gameState = do
  newFace <- readValidFaceRotationChoice selectedDie gameState
  return $ rotateDieInState (getFace selectedDie) newFace gameState

-- Lida com o movimento do computador com base no dado selecionado.
handleComputerMove :: Die -> GameState -> IO GameState
handleComputerMove selectedDie gameState = do
  if getFace selectedDie == 1
    then handleFaceOneComputer gameState
    else handleOtherFacesComputer selectedDie gameState

-- Lida com a remoção de um dado com uma face 1 no movimento do computador.
handleFaceOneComputer :: GameState -> IO GameState
handleFaceOneComputer gameState = do
  putStrLn "O computador removeu um dado de face 1."
  return $ removeDieInState gameState

-- Lida com a rotação de um dado com uma face diferente de 1 no movimento do computador.
handleOtherFacesComputer :: Die -> GameState -> IO GameState
handleOtherFacesComputer selectedDie gameState = do
  let possibleRotationsList = possibleRotations selectedDie

  randomIndex <- randomRIO (0, length possibleRotationsList - 1)
  let randomFace = possibleRotationsList !! randomIndex

  putStrLn $ "O computador rotacionou um dado de face " ++ show (getFace selectedDie) ++ " para a face " ++ show randomFace

  return $ rotateDieInState (getFace selectedDie) randomFace gameState

-- Retorna a função de movimento do computador com base na dificuldade
getComputerMove :: Difficulty -> (GameState -> IO GameState)
getComputerMove Easy = computerEasyMove
getComputerMove Hard = computerHardMove

-- Movimeto na dificuldade fácil: seleciona um dado aleatório e realiza uma jogada também aleatória
computerEasyMove :: GameState -> IO GameState
computerEasyMove (GameState dice player difficulty) = do
  randomIndex <- randomRIO (0, length dice - 1) :: IO Int
  let selectedDie = dice !! randomIndex
  handleComputerMove selectedDie (GameState dice player difficulty)

-- ovimeto na dificuldade difícil: tenta evitar configurações vencedoras para o jogador.
computerHardMove :: GameState -> IO GameState
computerHardMove (GameState dice player difficulty) = do
  let bestComputerConfigurations = filter (\configuration -> not $ isWinnerConfiguration configuration) (getPossibleConfigurations dice)

  if null bestComputerConfigurations then
    computerEasyMove (GameState dice player difficulty)
  else do
    let chosenComputerConfiguration = head bestComputerConfigurations
    let chosenDieFaces = map getFace dice
    let newDieFaces = map getFace chosenComputerConfiguration

    if length newDieFaces < length chosenDieFaces then do
      putStrLn "O computador removeu um dado."
      return (GameState chosenComputerConfiguration (togglePlayer player) difficulty)
    else do
      let (changedDie, newFace) = head [(die, newFace) | (die, newFace) <- zip dice newDieFaces, getFace die /= newFace]
      putStrLn $ "O computador rotacionou um dado de face " ++ show (getFace changedDie) ++ " para a face " ++ show newFace
      return (GameState chosenComputerConfiguration (togglePlayer player) difficulty)
