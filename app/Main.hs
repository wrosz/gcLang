module Main (main) where

import System.Environment (getArgs)
import Parser 
import Semantics (evalProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      prog <- parseFile filename
      case prog of
        Just p -> do
          let val = evalProgram p
          putStrLn $ "program sie ewaluuje, zwrocono " ++ show val
        Nothing -> putStrLn "nie dziala sparsowanie"
    _ -> putStrLn "nie dziala danie argumentu"
        

parseFile :: String -> IO (Maybe Program)
parseFile fileName = do
  contents <- readFile fileName
  case parseMiniGC contents of
    Left err -> do
      putStrLn "Parse error:"
      print err
      return Nothing
    Right ast -> do
      putStrLn "Successfully parsed program:"
      print ast
      return $ Just ast