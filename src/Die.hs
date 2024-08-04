-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro do Couto Filgueiras / Matrícula: 201935015

module Die (Die (..), rollDice, possibleRotations, oppositeFace, rotateDie, removeDie, getFace) where
import System.Random (randomRIO)

-- Define o tipo de dados Die como um inteiro, representando o valor da face de um dado.
data Die = Die Int deriving (Eq, Ord)

-- Implementa Show para o tipo Die, para que ele possa ser convertido em uma string.
instance Show Die where
  show (Die face) = faceToString face

-- Converte o valor inteiro da face para uma representação gráfica de um dado.
faceToString :: Int -> String
faceToString face = let
  top = " ---------\n"
  firstRow
    | face == 1 = "|         |\n"
    | face == 2 || face == 3 = "|  o      |\n"
    | otherwise = "|  o   o  |\n"
  secondRow
    | face == 6 = "|  o   o  |\n"
    | even face = "|         |\n"
    | otherwise = "|    o    |\n"
  thirdRow
    | face == 1 = "|         |\n"
    | face == 2 || face == 3 = "|      o  |\n"
    | otherwise = "|  o   o  |\n"
  bottom = " ---------"

  in top ++ firstRow ++ secondRow ++ thirdRow ++ bottom

-- Obtem a face virada para cima
getFace :: Die -> Int
getFace (Die face) = face

-- Constante para retornar o valor mínimo da face e abstrair chamadas
minFace :: Int
minFace = 1

-- Constante para retornar o valor máximo da face e abstrair chamadas
maxFace :: Int
maxFace = 6

-- Função utilizada  o dado
isValidDie :: Die -> Bool
isValidDie (Die face) = face >= minFace && face <= maxFace

-- Função que retonar a face oposta de um dado
oppositeFace :: Die -> Int
oppositeFace die
  | isValidDie die = 7 - getFace die
  | otherwise = 0

-- Função que retorna uma lista de inteiros com as possíveis rotações, excluindo a face oposta e atual
possibleRotations :: Die -> [Int]
possibleRotations die
  | isValidDie die = [x | x <- [minFace..maxFace], x < getFace die && x /= oppositeFace die]
  | otherwise = []

-- Função que executa a rotação de um dado
rotateDie :: Int -> Int -> [Die] -> [Die]
rotateDie _ _ [] = []
rotateDie oldFace newFace (x:xs)
  | getFace x == oldFace = Die newFace : xs
  | otherwise = x : rotateDie oldFace newFace xs

-- Função utilizada para remover um dado de face 1 da lista
removeDie :: [Die] -> [Die]
removeDie [] = []
removeDie (x:xs)
  | getFace x == 1 = xs
  | otherwise = x:removeDie xs

-- Função que obtém um valor aleatório para face
rollDie :: IO Die
rollDie = do
  randomInteger <- randomRIO (minFace, maxFace) :: IO Int
  return (Die randomInteger)

-- Função que gera uma lista de tamanho escolhido pelo jogador, de dados aleatórios
rollDice :: Int -> IO [Die]
rollDice 0 = return []
rollDice amount = do
  die <- rollDie
  dice <- rollDice (amount - 1)
  return (die : dice)
