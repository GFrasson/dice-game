-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro do Couto Filgueiras / Matrícula: 201935015

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

  -- Obtém os número de dados desejados pelo usuário
  diceAmount <- readValidDiceAmount

  -- Gera os dados aleatórios
  randomDice <- rollDice diceAmount

  -- Configura a dificuldade desejada pelo usuário
  difficulty <- readDifficulty

  -- Inicializa o estado do jogo
  let gameState = initGameState randomDice difficulty
  gameEventLoop gameState

-- Loop principal do jogo
gameEventLoop :: GameState -> IO ()
gameEventLoop (GameState dice player difficulty) = do
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

-- Verifica se o jogo terminou, retornando True se não houver dados restantes no estado atual do jogo.
verifyEndGame :: GameState -> Bool
verifyEndGame (GameState dice _ _) = length dice == 0