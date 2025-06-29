module Main (main) where

import System.Environment (getArgs)
import Parser 

-- | Main function to parse a file
main :: IO ()
main = parseFile 

parseFile :: IO ()
parseFile = do
  args <- getArgs
  case args of
    [fileName] -> do
      contents <- readFile fileName
      case parseMiniGC contents of
        Left err -> do
          putStrLn "Parse error:"
          print err
        Right ast -> do
          putStrLn "Successfully parsed program:"
          print ast
    _ -> putStrLn "Usage: parser <filename>"
