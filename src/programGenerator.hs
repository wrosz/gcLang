{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad.State
import Test.QuickCheck
import Parser
import Semantics
import Control.Monad

type GenM a = StateT GenState Gen a
data GenState = GenState {
    intVars :: [Expr],  -- integer variable names
    boolVars :: [Expr],  -- boolean viariable names
    intObjFields :: [Expr],  -- (object name, field name) for integer fields
    boolObjFields :: [Expr],  -- (object name, field name) for boolean fields
    intArrays :: [(String, Int)],
    intArraysEls :: [Expr],  -- integer array names and sizes
    boolArrays :: [(String, Int)],
    boolArraysEls :: [Expr],  -- boolean array names and sizes
    intFuncs :: [(String, Int)], -- function name, number of parameters
    boolFuncs :: [(String, Int)],
    nextName :: Int
    }
    deriving Show

initState :: GenState
initState = GenState [] [] [] [] [] [] [] [] [] [] 0

genIdentifier :: Gen String
genIdentifier = do
    first <- elements ['a' .. 'z']
    rest <- listOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']
    return (first : Prelude.take 10 rest)

-- Generator for small integers
genSmallInt :: Gen Int
genSmallInt = choose (-100, 100)
genBool :: Gen Bool
genBool = arbitrary

newName :: GenM String
newName = do
    st@GenState{nextName} <- get
    put st {nextName = nextName + 1}
    return $ show nextName

-- generate expressions that will evaluate to bool
genBoolExpr :: Int -> GenM Expr
genBoolExpr 0 = do
    GenState{boolVars, boolObjFields, boolArraysEls} <- get
    lift $ elements (boolVars ++ boolObjFields ++ boolArraysEls ++ [BoolLit True, BoolLit False])
genBoolExpr n = do
    sub1 <- genBoolExpr (n `div` 3)
    sub2 <- genBoolExpr (n `div` 3)
    sub3 <- genBoolExpr (n `div` 3)
    lift $ elements [sub1, BinOp Eq sub1 sub2, If sub1 sub2 sub3]

-- generate expressions that will evaluate to int
genIntExpr :: Int -> GenM Expr
genIntExpr 0 = do
    randomInt <- lift genSmallInt
    GenState{intVars, intObjFields, intArraysEls} <- get
    lift $ elements (intVars ++ intObjFields ++ intArraysEls ++ [IntLit randomInt])
genIntExpr n = do
    sub1 <- genIntExpr (n `div` 2)
    sub2 <- genIntExpr (n `div` 2)
    cond <- genBoolExpr (n `div` 2)
    lift $ elements [sub1, BinOp Add sub1 sub2, BinOp Sub sub1 sub2, If cond sub1 sub2]

-- create new boolean variable
newBoolVar :: GenM Expr
newBoolVar = do
    num <- newName
    let name = "bool" ++ num
    st@GenState{boolVars} <- get
    depth <- lift $ elements [1..4]
    expr <- genBoolExpr depth
    put st{boolVars = Var name:boolVars}
    return $ Let name expr (Var name)

-- create new integer variable
newIntVar :: GenM Expr
newIntVar = do
    num <- newName
    let name = "int" ++ num
    st@GenState{intVars} <- get
    depth <- lift $ elements [1..4]
    expr <- genIntExpr depth
    put st{intVars = Var name:intVars}
    return $ Let name expr (Var name)

genNewBoolArr :: GenM Expr
genNewBoolArr = do
    size <- lift $ elements [1..20]
    initVal <- lift genBool
    num <- newName
    let name = "boolArr" ++ num
    st@GenState{boolArraysEls, boolArrays} <- get
    put st{boolArraysEls = ArrayAccess (Var name) (IntLit 0):boolArraysEls, boolArrays = (name, size):boolArrays}
    return $ Let name (NewArray (IntLit size) (BoolLit initVal)) (ArrayAccess (Var name) (IntLit 0))

genNewIntArr :: GenM Expr
genNewIntArr = do
    size <- lift $ elements [1..20]
    initVal <- lift genSmallInt
    num <- newName
    let name = "intArr" ++ num
    st@GenState{intArraysEls, intArrays} <- get
    put st{intArraysEls = ArrayAccess (Var name) (IntLit 0):intArraysEls, intArrays = (name, size):intArrays}
    return $ Let name (NewArray (IntLit size) (IntLit initVal)) (ArrayAccess (Var name) (IntLit 0))

-- allocate new object, next evaluate a randomly chosen object field value
genNewObj :: GenM Expr
genNewObj = do
    nFields <- lift $ elements [1..6]
    num <- newName
    let name = "obj" ++ num
    st@GenState{intObjFields, boolObjFields} <- get
    let fieldNames = map (\y -> "field" ++ show y) [1..nFields]
    let nBoolFields = nFields `div` 2
    fieldValsBool <- replicateM nBoolFields (genBoolExpr 3)
    fieldValsInt <- replicateM (nFields - nBoolFields) (genIntExpr 3)
    put st{boolObjFields = boolObjFields ++ map (FieldAccess (Var name)) (take nBoolFields fieldNames),
        intObjFields = intObjFields ++ map (FieldAccess (Var name)) (drop nBoolFields fieldNames)}
    let fieldVals = fieldValsBool ++ fieldValsInt
    chosenFieldName <- lift $ elements fieldNames
    let e = Let name (New fieldNames fieldVals) (FieldAccess (Var name) chosenFieldName)
    return e

manipulateBools :: GenM Expr
manipulateBools = do
    GenState{boolVars, boolObjFields, boolArraysEls} <- get
    choice <- lift $ elements $ boolVars ++ boolObjFields ++ boolArraysEls ++ [BoolLit False]
    depth <- lift $ elements [1..4]
    expr <- genBoolExpr depth
    case choice of
        Var name -> return $ Let name expr (Var name)
        FieldAccess (Var objName) fieldName -> return $ FieldAssign (Var objName) fieldName expr
        ArrayAccess (Var arrName) (IntLit idx) -> return $ ArrayAssign (Var arrName) (IntLit idx) expr
        _ -> return Null

manipulateInts :: GenM Expr
manipulateInts = do
    GenState{intVars, intObjFields, intArraysEls} <- get
    choice <- lift $ elements $ intVars ++ intObjFields ++ intArraysEls ++ [IntLit 0]
    depth <- lift $ elements [1..4]
    expr <- genIntExpr depth
    case choice of
        Var name -> return $ Let name expr (Var name)
        FieldAccess (Var objName) fieldName -> return $ FieldAssign (Var objName) fieldName expr
        ArrayAccess (Var arrName) (IntLit idx) -> return $ ArrayAssign (Var arrName) (IntLit idx) expr
        _ -> return Null

boolArrAssgn :: GenM Expr
boolArrAssgn = do
    st@GenState{boolArrays, boolArraysEls} <- get
    (if null boolArrays then return Null else (do
        (name, size) <- lift $ elements boolArrays
        idx <- lift $ elements [0..(size-1)]
        depth <- lift $ elements [1..4]
        expr <- genBoolExpr depth
        put st {boolArraysEls = ArrayAccess (Var name) (IntLit idx) : boolArraysEls}
        return $ ArrayAssign (Var name) (IntLit idx) expr))

intArrAssgn :: GenM Expr
intArrAssgn = do
    st@GenState{intArrays, intArraysEls} <- get
    (if null intArrays then return Null else (do
        (name, size) <- lift $ elements intArrays
        idx <- lift $ elements [0..(size-1)]
        depth <- lift $ elements [1..4]
        expr <- genIntExpr depth
        put st {intArraysEls = ArrayAccess (Var name) (IntLit idx) : intArraysEls}
        return $ ArrayAssign (Var name) (IntLit idx) expr))

genAnyExpr :: GenM Expr
genAnyExpr = do
    join $ lift $ elements [newBoolVar, newIntVar,
        genNewBoolArr, genNewIntArr, genNewObj, manipulateBools, manipulateInts, boolArrAssgn, intArrAssgn]

genBody :: GenM Expr
genBody = do
    bodyLength <- lift $ elements [3..20]
    e1 <- newBoolVar
    e2 <- newIntVar
    e3 <- genNewObj
    exprs <- replicateM bodyLength genAnyExpr
    return $ toSeq $ [e1,e2,e3]++exprs where
        toSeq :: [Expr] -> Expr
        toSeq []     = Null
        toSeq [e]    = e
        toSeq exprs  = foldr1 Seq exprs

genIntFunc :: GenM FuncDef
genIntFunc = do
    st@GenState {intFuncs} <- get
    n <- lift $ elements [1..6]
    let nBoolParams = n `div` 2
    let boolParamNames = map (\y -> "p"++show y) [1..nBoolParams]
    let intParamNames = map (\y -> "p"++show y) [nBoolParams+1 .. n]
    put $ initState {intVars = map Var intParamNames, boolVars = map Var boolParamNames}
    name <- lift genIdentifier
    body <- genBody
    lastExpr <- genIntExpr 2
    put st {intFuncs = (name, n):intFuncs}
    return $ FuncDef name (boolParamNames ++ intParamNames) (Seq body lastExpr)

genBoolFunc :: GenM FuncDef
genBoolFunc = do
    st@GenState {boolFuncs} <- get
    n <- lift $ elements [1..6]
    let nBoolParams = n `div` 2
    let boolParamNames = map (\y -> "p"++show y) [1..nBoolParams]
    let intParamNames = map (\y -> "p"++show y) [nBoolParams+1 .. n]
    put $ initState {intVars = map Var intParamNames, boolVars = map Var boolParamNames}
    name <- lift genIdentifier
    body <- genBody
    lastExpr <- genBoolExpr 2
    put st {boolFuncs = (name, n):boolFuncs}
    return $ FuncDef name (boolParamNames ++ intParamNames) (Seq body lastExpr)

intFuncCall :: GenM Expr
intFuncCall = do
    GenState {intFuncs} <- get
    if null intFuncs then return (IntLit 0) else do 
        (name, nParams) <- lift $ elements intFuncs
        let nBoolParams = nParams `div` 2
        let nIntParams = nParams - nBoolParams
        boolParams <- replicateM nBoolParams (genBoolExpr 1)
        intParams <- replicateM  nIntParams (genIntExpr 1)
        return $ Call name (boolParams ++ intParams)

boolFuncCall :: GenM Expr
boolFuncCall = do
    GenState {boolFuncs} <- get
    if null boolFuncs then return (BoolLit False) else do 
        (name, nParams) <- lift $ elements boolFuncs
        let nBoolParams = nParams `div` 2
        let nIntParams = nParams - nBoolParams
        boolParams <- replicateM nBoolParams (genBoolExpr 1)
        intParams <- replicateM  nIntParams (genIntExpr 1)
        return $ Call name (boolParams ++ intParams)

programGenerator :: GenM Program
programGenerator = do
    nIntFunc <- lift $ elements [0..2]
    nBoolFunc <- lift $ elements [0..2]
    intfuncdefs <- replicateM nIntFunc genIntFunc
    boolfuncdefs <- replicateM nBoolFunc genBoolFunc
    Program (intfuncdefs ++ boolfuncdefs) <$> genBody


example1 :: Gen Program
example1 = evalStateT programGenerator initState

example1' = generate example1

sprawdzam :: IO ()
sprawdzam = do
    print "program:"
    prog <- example1'
    print prog
    print "\n \n wynik:"
    print (evalProgram prog)
