-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro do Couto Filgueiras / Matrícula: 201935015

module TerminalColor (changeColorByContextSafe, Context(..)) where

import System.IO (stdout)
import System.Console.ANSI
    ( setSGR,
      Color(Yellow, Green, Red),
      ColorIntensity(Dull, Vivid),
      ConsoleLayer(Foreground),
      SGR(SetColor), hNowSupportsANSI )

-- Define os contextos possíveis para alterar a cor do terminal.
data Context = PersonContext | ComputerContext | SystemContext deriving (Show, Eq)

-- Verifica se terminal possui suporte a cores
hasColorSupport :: IO Bool
hasColorSupport = hNowSupportsANSI stdout

-- Altera a cor do texto no terminal de forma segura, verificando se o terminal suporta cores.
changeColorByContextSafe :: Context -> IO ()
changeColorByContextSafe context = do
  hasColorSupport
  >>= (\x -> if x then changeColorByContext context else return ())

-- Altera cor do texto no terminal de acordo com o contexto
changeColorByContext :: Context -> IO ()
changeColorByContext PersonContext = setSGR [SetColor Foreground Vivid Green]
changeColorByContext ComputerContext = setSGR [SetColor Foreground Vivid Red]
changeColorByContext SystemContext = setSGR [SetColor Foreground Dull Yellow]
