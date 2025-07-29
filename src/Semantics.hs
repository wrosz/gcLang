{-# LANGUAGE NamedFieldPuns #-}

module Semantics (evalProgram, runProgram, GCState(..)) where

import Parser
import Data.Map as Map
import Control.Monad (when)
import Control.Monad.State
import qualified Data.Set as Set

eval :: Expr -> Interpreter Value

-- | Definitions

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
type Env = Map String Value  -- variable names and values
type FuncEnv = Map String FuncDef  -- function definitions

-- arrays and objects
data HeapObject =
    HObj (Map String Value) |  -- objects
    HArr [Value]               -- arrays

-- maps reference IDs to objects and arrays
type Heap = Map Ref HeapObject

-- runtime state
data GCState = GCState {
  env :: Env,
  funcEnv :: FuncEnv,
  heap :: Heap,
  nextRef :: Int         -- to allocate new refs
}

initialState :: GCState
initialState = GCState {
  env = Map.empty,
  funcEnv = Map.empty,
  heap = Map.empty,
  nextRef = 0
}

type Interpreter a = State GCState a

--  Evaluating expressions

-- alocation helper (update state when a new object allocation happens)
alloc :: HeapObject -> Interpreter Value
alloc obj = do
  st@GCState{heap, nextRef} <- get
  let ref = nextRef  -- allocated reference ID
  put st { heap = Map.insert ref obj heap, nextRef = ref + 1 }  -- update state (insert new reference to the heap, update last reference number)
  return (VRef ref)  -- return reference ID

--eval :: Expr -> Interpreter Value

-- integers and bools
eval (IntLit n) = return $ VInt n
eval (BoolLit bool) = return $ VBool bool

-- variables (return value of given variable)
eval (Var x) = do
  GCState{env} <- get
  case Map.lookup x env of
    Just v -> return v
    Nothing -> error $ "Unbound variable: " ++ x

-- binary operations
eval (BinOp Add expr1 expr2) = do
  v1 <- eval expr1
  v2 <- eval expr2
  case (v1, v2) of
    (VInt i1, VInt i2) -> return $ VInt (i1 + i2)
    _ -> error $ "Type error in Add: expected two integers, got " ++ show (v1, v2)

eval (BinOp Sub expr1 expr2) = do
  v1 <- eval expr1
  v2 <- eval expr2
  case (v1, v2) of
    (VInt i1, VInt i2) -> return $ VInt (i1 - i2)
    _ -> error $ "Type error in Sub: expected two integers, got " ++ show (v1, v2)

eval (BinOp Eq expr1 expr2) = do
  v1 <- eval expr1
  v2 <- eval expr2
  return $ VBool (v1 == v2)

-- if statements
eval (If e1 e2 e3) = do
  cond <- eval e1
  tr <- eval e2
  fl <- eval e3
  case cond of
    VBool cond' -> if cond' then return tr else return fl
    _ -> error $ "Type error in If: expected bool, got " ++ show cond

-- let statements
eval (Let str e1 e2) = do
  v1 <- eval e1
  modify (\st -> st { env = Map.insert str v1 (env st) })
  eval e2

-- function calls
eval (Call func exprs) = do
  st@GCState{env, funcEnv} <- get
  case Map.lookup func funcEnv of
    Nothing -> error $ "Undefined function: " ++ show func
    Just (FuncDef _ paramNames body) -> do
      -- check arity
      when (length paramNames /= length exprs) $
        error $ "Function " ++ func ++ " expects " ++ show (length paramNames)
              ++ " arguments, but got " ++ show (length exprs)
      argVals <- mapM eval exprs  -- evaluate arguments
      let newEnv = Map.fromList (zip paramNames argVals) -- create new environment with parameters bound to argument values
      put st { env = newEnv } -- save old environment and switch to new one
      result <- eval body  -- evaluate the body
      modify $ \s -> s { env = env } -- restore old environment
      return result

-- new objects
eval (New fields exprs) = do
  vs <- mapM eval exprs  -- evaluated expressions values
  let fieldMap = Map.fromList (zip fields vs)
  alloc (HObj fieldMap)  -- returns object ID

-- new arrays
eval (NewArray e1 e2) = do
  arrSize <- eval e1
  initValue <- eval e2
  case arrSize of
    VInt n -> do
      when (n < 1) $ error $ "Value error in NewArray: expected positive size, got " ++ show n
      let arr = HArr $ initValue : replicate (n-1) VNull
      alloc arr  -- returns array ID
    _ -> error $ "Value error in NewArray: expected integer size, got " ++ show arrSize

-- field access
eval (FieldAccess expr str) = do
  GCState{heap} <- get
  ref <- eval expr
  objId <- case ref of  -- check if ref is a reference
    VRef k -> return k
    _ -> error $ "Type error in FieldAcces: expected object field reference, got " ++ show ref
  obj <- case Map.lookup objId heap of  -- check if ref maps to an actual object
    Just (HObj o) -> return o
    Just (HArr arr) -> error $ "Type error in FieldAccess: expected object, got array " ++ show arr
    Nothing -> error $ "Undefined object: " ++ show expr
  case Map.lookup str obj of -- return field value, if it exists
    Just v -> return v
    Nothing -> error $ "Undefined field: " ++ show expr ++ "." ++ str


-- field assignments
eval (FieldAssign e1 str e2) = do
  ref <- eval e1
  newVal <- eval e2
  st@GCState{heap} <- get
  objId <- case ref of  -- check if ref is a reference
    VRef k -> return k
    _ -> error "Field assignment to non-object"
  case Map.lookup objId heap of
    Just (HObj fields) -> do  -- if objId maps to an object, then update it with a new field and value
      let updatedObj = HObj (Map.insert str newVal fields)
      put st { heap = Map.insert objId updatedObj heap }
      return newVal
    -- if not, throw errors
    Just (HArr arr) -> error $ "Type error in FieldAssign: expected object, got array " ++ show arr
    Nothing -> error $ "Undefined object: " ++ show e1


-- array access
eval (ArrayAccess e1 e2) = do
  GCState {heap} <- get
  ref <- eval e1
  idx <- eval e2
  arrId <- case ref of  -- check if ref is a reference
    VRef k -> return k
    _ -> error $ "Type error in ArrayAccess: expected array reference, got " ++ show ref
  i <- case idx of  -- check if idx is an integer
    VInt j -> return j
    _ -> error $ "Type error in ArrayAcces: expected array index, got " ++ show idx
  arr <- case Map.lookup arrId heap of  -- check if arrId maps to an actual array
    Just (HArr a) -> return a
    Just (HObj _) -> error $ "Type error in ArrayAccess: expected array, got object instead: " ++ show e1
    Nothing -> error $ "Undefined array: " ++ show e1
  return $ arr !! i

-- array assignment
eval (ArrayAssign e1 e2 e3) = do
  st@GCState{heap} <- get
  ref <- eval e1
  idx <- eval e2
  newVal <- eval e3
  arrId <- case ref of  -- check if ref is a reference
    VRef k -> return k
    _ -> error $ "Type error in ArrayAccess: expected array reference, got " ++ show ref
  i <- case idx of  -- check if idx is an integer
    VInt j -> return j
    _ -> error $ "Type error in ArrayAcces: expected array index, got " ++ show idx
  arr <- case Map.lookup arrId heap of  -- check if arrId maps to an actual array
    Just (HArr a) -> return a
    Just (HObj _) -> error $ "Type error in ArrayAccess: expected array, got object instead: " ++ show e1
    Nothing -> error $ "Undefined array: " ++ show e1
  let updateArr = Prelude.take i arr ++ [newVal] ++ Prelude.drop (i+1) arr -- update array by replacing idx-th element with new value
  put st { heap = Map.insert arrId (HArr updateArr) heap }
  return newVal

-- sequence of expressions
eval (Seq e1 e2) = do
  _ <- eval e1
  eval e2

-- null expression
eval Null = return VNull


--  run a program

-- evaluate function definitions
evalFuncDef :: FuncDef -> Interpreter FuncDef
evalFuncDef (FuncDef name args body) = do
  st@GCState{funcEnv} <- get
  let newFuncEnv = Map.insert name (FuncDef name args body) funcEnv
  put (st {funcEnv = newFuncEnv})
  return (FuncDef name args body)

-- evaluates whole program
programInterpreter :: Program -> Interpreter Value
programInterpreter (Program funcs body) = do
  mapM_ evalFuncDef funcs
  result <- eval body
  modify (\st -> st { env = Map.insert "__result__" result (env st) })
  gc
  return result


-- same as runState (runs program on a new environment, returns final state and value)
runProgram :: Program -> (Value, GCState)
runProgram program = runState (programInterpreter program) initialState

-- same as evalState (runs program on a new environment, returns only value)
evalProgram :: Program -> Value
evalProgram program = evalState (programInterpreter program) initialState

-- === Garbage Collector (Mark-and-Sweep) ===
-- Wersja podstawowa: uruchamiana ręcznie lub po każdej alokacji

-- Uruchom garbage collector
gc :: Interpreter ()
gc = do
  st@GCState{env, heap} <- get
  let reachable = markReachableFromEnv env heap
  let heap' = removeUnreachable reachable heap
  put st { heap = heap' }

-- Znajdź wszystkie referencje osiągalne z aktualnego środowiska (env)
markReachableFromEnv :: Env -> Heap -> Set.Set Ref
markReachableFromEnv env heap = execState (mapM_ visitVal (Map.elems env)) Set.empty
  where
    visitVal :: Value -> State (Set.Set Ref) ()
    visitVal (VRef r) = visitRef r
    visitVal _        = return ()

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
            Nothing            -> return ()

-- Usuń wszystkie nieosiągalne obiekty z heap
removeUnreachable :: Set.Set Ref -> Heap -> Heap
removeUnreachable reachable heap = Map.filterWithKey (\k _ -> Set.member k reachable) heap
