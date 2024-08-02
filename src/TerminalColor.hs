-- Nome: Gabriel Frasson Costa / Matrícula: 202035001
-- Nome: Pedro Couto... / Matrícula: ...

module TerminalColor (changeColorByContextSafe, Context(..)) where

import System.IO (stdout)
import System.Console.ANSI
    ( setSGR,
      Color(Yellow, Green, Red),
      ColorIntensity(Dull, Vivid),
      ConsoleLayer(Foreground),
      SGR(SetColor), hNowSupportsANSI )

data Context = PersonContext | ComputerContext | SystemContext deriving (Show, Eq)

hasColorSupport :: IO Bool
hasColorSupport = hNowSupportsANSI stdout

changeColorByContextSafe :: Context -> IO ()
changeColorByContextSafe context = do
  hasColorSupport
  >>= (\x -> if x then changeColorByContext context else return ())

changeColorByContext :: Context -> IO ()
changeColorByContext PersonContext = setSGR [SetColor Foreground Vivid Green]
changeColorByContext ComputerContext = setSGR [SetColor Foreground Vivid Red]
changeColorByContext SystemContext = setSGR [SetColor Foreground Dull Yellow]
