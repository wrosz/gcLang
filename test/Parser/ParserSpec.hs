module Parser.ParserSpec (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Control.Monad (liftM)
import Text.Parsec (ParseError, parse)

-- Import the parser module from your library
import Parser

-- | All tests - organized into unit tests and property tests
tests = []
    -- [ testGroup "Unit tests" unitTests
    -- , testGroup "Property-based tests" propertyTests
    -- ]

-- | Helper to run a parser on input and check against expected AST
parseTest :: (Show a, Eq a) => (String -> Either ParseError a) -> String -> a -> Assertion
parseTest parser input expected =
    case parser input of
        Left err -> assertFailure $ "Parse error: " ++ show err
        Right result -> assertEqual ("Parsing: " ++ input) expected result


-- | Unit tests

-- test integer literals, let statements and variable references
test_intLit :: Assertion
test_intLit = parseTest parseMiniGC "let var = 436; var" $ Program [] $ Let "var" (IntLit 436) (Var "var")

-- test new object creation
test_parseNew :: Assertion
test_parseNew = parseTest parseMiniGC "new [\"a\", \"b\"] [true, null]" $ Program [] $ New ["a", "b"] [(BoolLit True), Null]

-- test array assignment
test_parseArrayAssign :: Assertion
test_parseArrayAssign = parseTest parseMiniGC "arr[10] = x" $ Program [] $ ArrayAssign (Var "arr") (IntLit 10) (Var "x")

-- test sequences of expressions (binary operation; function call; null)
test_parseSeq :: Assertion
test_parseSeq = parseTest parseMiniGC "x + y; f(x); null" (
    Program [] (
        Seq (BinOp Add (Var "x") (Var "y")) (
            Seq (Call "f" [Var "x"]) (Null))))

-- test function definitions
test_parseFuncDef :: Assertion
test_parseFuncDef = parseTest parseMiniGC "def func(x,y) = x == y; null" (
     Program [FuncDef "func" ["x", "y"] (BinOp Eq (Var "x") (Var "y"))] Null)
