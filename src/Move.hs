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

  putStrLn $ "O computador rotacionou um dado de face " ++ show (getFace selectedDie) ++ " para a face " ++ show randomFace

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
  let possibleConfigurations = getPossibleConfigurations dice
  let bestComputerConfigurations = filter (not . isWinnerConfiguration) possibleConfigurations

  if null bestComputerConfigurations then
    computerEasyMove (GameState dice player difficulty)
  else do
    let chosenComputerConfiguration = head bestComputerConfigurations
    return (GameState chosenComputerConfiguration (togglePlayer player) difficulty)
