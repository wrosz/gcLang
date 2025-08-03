{-# LANGUAGE NamedFieldPuns #-}

module Semantics (runProgram, GCState(..), printHeap) where

import Parser
import Data.Map as Map
import Control.Monad (when)
import Control.Monad.State.Strict
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set

-- unique reference IDs for objects and arrays
type Ref = Int

-- Runtime values
data Value
  = VInt Int
  | VBool Bool
  | VRef Ref
  | VNull
  deriving (Show, Eq)

-- environments
type Env = Map String Value
type FuncEnv = Map String FuncDef

-- arrays and objects
data HeapObject =
    HObj (Map String Value)
  | HArr [Value]

type Heap = Map Ref HeapObject

-- runtime state
data GCState = GCState {
  env :: Env,
  funcEnv :: FuncEnv,
  heap :: Heap,
  nextRef :: Int
}

initialState :: GCState
initialState = GCState {
  env = Map.empty,
  funcEnv = Map.empty,
  heap = Map.empty,
  nextRef = 0
}

type Interpreter a = StateT GCState IO a

-- allocation helper
alloc :: HeapObject -> Interpreter Value
alloc obj = do
  st@GCState{heap, nextRef} <- get
  let ref = nextRef
  put st { heap = Map.insert ref obj heap, nextRef = ref + 1 }
  return (VRef ref)

-- === eval expressions ===

eval :: Expr -> Interpreter Value
eval (IntLit n) = return $ VInt n
eval (BoolLit b) = return $ VBool b

eval (Var x) = do
  GCState{env} <- get
  case Map.lookup x env of
    Just v -> return v
    Nothing -> error $ "Unbound variable: " ++ x

eval (BinOp Add e1 e2) = binOpInt (+) e1 e2
eval (BinOp Sub e1 e2) = binOpInt (-) e1 e2
eval (BinOp Eq e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ VBool (v1 == v2)

eval (If e1 e2 e3) = do
  cond <- eval e1
  case cond of
    VBool True  -> eval e2
    VBool False -> eval e3
    _ -> error $ "Type error in If: expected bool, got " ++ show cond

eval (Let str e1 e2) = do
  v1 <- eval e1
  modify (\st -> st { env = Map.insert str v1 (env st) })
  eval e2

eval (Call func args) = do
  st@GCState{env, funcEnv} <- get
  case Map.lookup func funcEnv of
    Nothing -> error $ "Undefined function: " ++ func
    Just (FuncDef _ paramNames body) -> do
      when (length paramNames /= length args) $
        error $ "Function " ++ func ++ " expects " ++ show (length paramNames)
              ++ " arguments, but got " ++ show (length args)
      argVals <- mapM eval args
      let newEnv = Map.fromList (zip paramNames argVals)
      put st { env = newEnv }
      result <- eval body
      modify $ \s -> s { env = env }
      return result

eval (New fields exprs) = do
  vs <- mapM eval exprs
  alloc (HObj (Map.fromList (zip fields vs)))

eval (NewArray sizeExpr valExpr) = do
  VInt size <- eval sizeExpr
  initVal <- eval valExpr
  when (size < 1) $ error "Array size must be positive"
  alloc (HArr (replicate size initVal))

eval (FieldAccess objExpr field) = do
  GCState{heap} <- get
  VRef ref <- eval objExpr
  case Map.lookup ref heap of
    Just (HObj fields) ->
      case Map.lookup field fields of
        Just v -> return v
        Nothing -> error $ "No such field: " ++ field
    _ -> error "FieldAccess on non-object"

eval (FieldAssign objExpr field valExpr) = do
  VRef ref <- eval objExpr
  val <- eval valExpr
  st@GCState{heap} <- get
  case Map.lookup ref heap of
    Just (HObj fields) ->
      put st { heap = Map.insert ref (HObj (Map.insert field val fields)) heap }
    _ -> error "FieldAssign on non-object"
  return val

eval (ArrayAccess arrExpr idxExpr) = do
  VRef ref <- eval arrExpr
  VInt idx <- eval idxExpr
  GCState{heap} <- get
  case Map.lookup ref heap of
    Just (HArr elems) -> return (elems !! idx)
    _ -> error "ArrayAccess on non-array"

eval (ArrayAssign arrExpr idxExpr valExpr) = do
  VRef ref <- eval arrExpr
  VInt idx <- eval idxExpr
  val <- eval valExpr
  st@GCState{heap} <- get
  case Map.lookup ref heap of
    Just (HArr elems) ->
      let newArr = Prelude.take idx elems ++ [val] ++ Prelude.drop (idx+1) elems
      in put st { heap = Map.insert ref (HArr newArr) heap }
    _ -> error "ArrayAssign on non-array"
  return val

eval (Seq e1 e2) = eval e1 >> eval e2
eval Null = return VNull

binOpInt :: (Int -> Int -> Int) -> Expr -> Expr -> Interpreter Value
binOpInt op e1 e2 = do
  VInt v1 <- eval e1
  VInt v2 <- eval e2
  return $ VInt (v1 `op` v2)

-- === function definitions and program ===

evalFuncDef :: FuncDef -> Interpreter FuncDef
evalFuncDef f@(FuncDef name _ _) = do
  st@GCState{funcEnv} <- get
  put st { funcEnv = Map.insert name f funcEnv }
  return f

programInterpreter :: Program -> Interpreter Value
programInterpreter (Program funcs body) = do
  mapM_ evalFuncDef funcs
  result <- eval body
  modify (\st -> st { env = Map.insert "__result__" result (env st) })

  -- Sterta przed GC
  stBefore <- get
  liftIO $ putStrLn "Stan sterty przed Garbage Collector:"
  liftIO $ printHeap (heap stBefore)

  gc

  -- Sterta po GC
  stAfter <- get
  liftIO $ putStrLn "Stan sterty po Garbage Collector:"
  liftIO $ printHeap (heap stAfter)

  return result

runProgram :: Program -> IO (Value, GCState)
runProgram program = runStateT (programInterpreter program) initialState

-- === Garbage Collector (Mark-and-Sweep) ===

gc :: Interpreter ()
gc = do
  st@GCState{env, heap} <- get
  let reachable = markReachableFromEnv env heap
  put st { heap = removeUnreachable reachable heap }

markReachableFromEnv :: Env -> Heap -> Set.Set Ref
markReachableFromEnv env heap =
  execState (mapM_ visitVal (Map.elems env)) Set.empty
  where
    visitVal :: Value -> State (Set.Set Ref) ()
    visitVal (VRef r) = visitRef r
    visitVal _ = return ()

    visitRef :: Ref -> State (Set.Set Ref) ()
    visitRef r = do
      visited <- get
      if Set.member r visited
        then return ()
        else do
          put (Set.insert r visited)
          case Map.lookup r heap of
            Just (HObj fields) -> mapM_ visitVal (Map.elems fields)
            Just (HArr elems)  -> mapM_ visitVal elems
            Nothing             -> return ()

removeUnreachable :: Set.Set Ref -> Heap -> Heap
removeUnreachable reachable heap =
  Map.filterWithKey (\k _ -> Set.member k reachable) heap

-- Pomocnicze drukowanie sterty
printHeap :: Heap -> IO ()
printHeap heap = do
  putStrLn "[Heap]"
  mapM_ printObj (Map.toList heap)
  where
    printObj (ref, HObj fields) = putStrLn $ "  " ++ show ref ++ ": Object " ++ show fields
    printObj (ref, HArr elems) = putStrLn $ "  " ++ show ref ++ ": Array " ++ show elems
