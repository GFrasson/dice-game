module Game (startGame) where

import Die (rollDie)

startGame :: IO ()
startGame = do
  putStrLn "---- Jogo de dados ----"
  let randomDie = rollDie
  putStrLn $ show randomDie
