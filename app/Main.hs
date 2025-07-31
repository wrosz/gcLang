module Main (main) where

import System.Environment (getArgs)
import Parser 
import Semantics (runProgram, heap, GCState(..))
import qualified Data.Map as Map
import Semantics (Heap, HeapObject(..), Value(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      prog <- parseFile filename
      case prog of
        Just p -> do
          let (val, finalState) = runProgram p
          putStrLn "Stan sterty po Garbage Collector:"
          printHeap (heap finalState)
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

printHeap :: Heap -> IO ()
printHeap h = do
  putStrLn "Heap:"
  mapM_ printEntry (Map.toList h)
  where
    printEntry (ref, obj) = do
      putStrLn $ "Ref " ++ show ref ++ ": " ++ showHeapObject obj

showHeapObject :: HeapObject -> String
showHeapObject (HObj fields) = "Object " ++ show (Map.toList fields)
showHeapObject (HArr elems)  = "Array " ++ show elems