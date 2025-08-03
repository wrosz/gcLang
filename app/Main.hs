module Main where

import System.Environment (getArgs)
import Parser (parseMiniGC)
import Semantics (runProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseMiniGC content of
        Left _ -> putStrLn "nie dziala sparsowanie"
        Right p -> do
          putStrLn "=== Rozpoczynam ewaluację programu ==="
          (val, _) <- runProgram p
          putStrLn "\n=== Ewaluacja zakończona ==="
          putStrLn $ "Wynik programu: " ++ show val
    _ -> putStrLn "nie dziala danie argumentu"
