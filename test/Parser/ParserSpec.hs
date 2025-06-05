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
tests = [ testGroup "Unit tests" unitTests]

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
test_new :: Assertion
test_new = parseTest parseMiniGC "new [\"a\", \"b\"] [true, null]" $ Program [] $ New ["a", "b"] [(BoolLit True), Null]

-- test array assignment
test_arrayAssign :: Assertion
test_arrayAssign = parseTest parseMiniGC "arr[10] = x" $ Program [] $ ArrayAssign (Var "arr") (IntLit 10) (Var "x")

-- test sequences of expressions (binary operation; function call; null)
test_seq :: Assertion
test_seq = parseTest parseMiniGC "x + y; f(x); null" (
    Program [] (
        Seq (BinOp Add (Var "x") (Var "y")) (
            Seq (Call "f" [Var "x"]) (Null))))

-- test function definitions
test_funcDef :: Assertion
test_funcDef = parseTest parseMiniGC "def func(x,y) = x == y null" (
     Program [FuncDef "func" ["x", "y"] (BinOp Eq (Var "x") (Var "y"))] Null)

unitTests =
    [ testCase "Parse integer literal" test_intLit
    , testCase "Parse new object creation" test_new
    , testCase "Parse array assignment" test_arrayAssign
    , testCase "Parse a sequence of expressions" test_seq
    , testCase "Parse a function definition" test_funcDef
    ]
