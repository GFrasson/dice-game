module Game (startGame) where

import TerminalColor (changeColorByContextSafe, Context(..))
import IOService
    ( readValidDiceAmount,
      readDifficulty,
      readValidDieChoice,
      printGameState,
      printEndGame )
import Move ( handlePersonMove, getComputerMove )
import State
    ( GameState(GameState),
      Player(Person),
      initGameState,
      togglePlayer )
import Die ( rollDice )

startGame :: IO ()
startGame = do
  putStrLn "---- Jogo de dados ----"

  diceAmount <- readValidDiceAmount
  randomDice <- rollDice diceAmount

  difficulty <- readDifficulty

  let gameState = initGameState randomDice difficulty
  gameEventLoop gameState

gameEventLoop :: GameState -> IO ()
gameEventLoop (GameState dice player difficulty) = do
  -- Verify End Game
  let endGame = verifyEndGame (GameState dice player difficulty)
  if endGame
    then printEndGame (togglePlayer player)

  else do
    printGameState (GameState dice player difficulty)
    if player == Person
      then do
        changeColorByContextSafe PersonContext

        selectedDie <- readValidDieChoice (GameState dice player difficulty)
        putStrLn $ show selectedDie

        newGameState <- handlePersonMove selectedDie (GameState dice player difficulty)
        gameEventLoop newGameState

    else do
      changeColorByContextSafe ComputerContext
      putStrLn "Vez do Computador"

      let computerMove = getComputerMove difficulty
      newGameState <- computerMove (GameState dice player difficulty)

      putStrLn "O computador finalizou a sua jogada."
      gameEventLoop newGameState

-- Função para verificar se o jogo terminou
verifyEndGame :: GameState -> Bool
verifyEndGame (GameState dice _ _) = length dice == 0