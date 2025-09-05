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
        Left _ -> putStrLn "Program was not parsed successfully"
        Right prog -> do
          putStrLn "Program was parsed successfully"
          putStrLn "=== Starting program evaluation ==="
          (val, _) <- runProgram prog
          putStrLn "\n=== Evaluation finished ==="
          putStrLn $ "Program result: " ++ show val
    _ -> putStrLn "Invalid arguments. Please provide a single filename."