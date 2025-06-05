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
tests = 
  [ testGroup "Unit tests" unitTests
  , testGroup "Property-based tests"
      [ testProperty "Parse let with integer" prop_parseLetWithInt
      , testProperty "Parse addition expression" prop_parseAddExpr
      , testProperty "Parse if-then-else expression" prop_parseIfExpr
      , testProperty "Parse simple function call" prop_parseSimpleCall
      , testProperty "Parse null literal" prop_parseNull
      , testCase "Fail: missing semicolon" test_fail_missingSemicolon
      , testCase "Fail: invalid operator" test_fail_invalidOp
      , testCase "Fail: incomplete if" test_fail_incompleteIf
      , testCase "Fail: unclosed bracket" test_fail_unclosedBracket
      , testCase "Fail: empty input" test_fail_empty
      , testCase "Edge: empty object" test_edge_emptyObject
      , testCase "Edge: newArray with zero" test_edge_zeroArray
      ]
  ]


-- | Helper to run a parser on input and check against expected AST
parseTest :: (Show a, Eq a) => (String -> Either ParseError a) -> String -> a -> Assertion
parseTest parser input expected =
    case parser input of
        Left err -> assertFailure $ "Parse error: " ++ show err
        Right result -> assertEqual ("Parsing: " ++ input) expected result

-- | Helper: assert that parsing fails
parseShouldFail :: String -> Assertion
parseShouldFail input =
  case parseMiniGC input of
    Left _  -> return ()
    Right v -> assertFailure $ "Expected parse failure, but got: " ++ show (v)

-- | Invalid syntax tests

-- Missing semicolon in let binding
test_fail_missingSemicolon :: Assertion
test_fail_missingSemicolon = parseShouldFail "let x = 5 x"


-- Invalid binary operator
test_fail_invalidOp :: Assertion
test_fail_invalidOp = parseShouldFail "5 ** 2"

-- If without then/else
test_fail_incompleteIf :: Assertion
test_fail_incompleteIf = parseShouldFail "if true 5 else 6"


-- Unclosed brackets
test_fail_unclosedBracket :: Assertion
test_fail_unclosedBracket = parseShouldFail "new [\"a\", \"b\"] [true, null"

-- Empty input
test_fail_empty :: Assertion
test_fail_empty = parseShouldFail ""

-- | Edge cases

-- Pusty obiekt: new [] []
test_edge_emptyObject :: Assertion
test_edge_emptyObject =
  parseTest parseMiniGC "new [] []" $
    Program [] (New [] [])

-- array 0 0
test_edge_zeroArray :: Assertion
test_edge_zeroArray =
  parseTest parseMiniGC "newArray 0 0" $
    Program [] (NewArray (IntLit 0) (IntLit 0))


-- | Property-based tests

-- Property-based test: any integer literal should parse correctly in a let-binding
prop_parseLetWithInt :: Int -> Property
prop_parseLetWithInt n =
  let input = "let x = " ++ show n ++ "; x"
      expected = Program [] $ Let "x" (IntLit n) (Var "x")
  in parseMiniGC input === Right expected

-- binary op
prop_parseAddExpr :: Int -> Int -> Property
prop_parseAddExpr x y =
  let input = "let z = " ++ show x ++ " + " ++ show y ++ "; z"
      expected = Program [] $ Let "z" (BinOp Add (IntLit x) (IntLit y)) (Var "z")
  in parseMiniGC input === Right expected

-- bools
prop_parseIfExpr :: Bool -> Int -> Int -> Property
prop_parseIfExpr cond x y =
  let condStr = if cond then "true" else "false"
      input = "if " ++ condStr ++ " then " ++ show x ++ " else " ++ show y
      expected = Program [] $ If (BoolLit cond) (IntLit x) (IntLit y)
  in parseMiniGC input === Right expected

-- function calls
prop_parseSimpleCall :: Int -> Property
prop_parseSimpleCall n =
  let input = "f(" ++ show n ++ ")"
      expected = Program [] $ Call "f" [IntLit n]
  in parseMiniGC input === Right expected

-- null
prop_parseNull :: Property
prop_parseNull =
  parseMiniGC "null" === Right (Program [] Null)


-- | Unit tests

-- test integer literals, let statements and variable references
test_intLit :: Assertion
test_intLit = parseTest parseMiniGC "let var = 436; var" $ Program [] $ Let "var" (IntLit 436) (Var "var")

-- test new object creation
test_new :: Assertion
test_new = parseTest parseMiniGC "new [\"a\", \"b\"] [true, null]" $ Program [] $ New ["a", "b"] [BoolLit True, Null]

-- test array assignment
test_arrayAssign :: Assertion
test_arrayAssign = parseTest parseMiniGC "arr[10] = x" $ Program [] $ ArrayAssign (Var "arr") (IntLit 10) (Var "x")

-- test sequences of expressions (binary operation; function call; null)
test_seq :: Assertion
test_seq = parseTest parseMiniGC "x + y; f(x); null" (
    Program [] (
        Seq (BinOp Add (Var "x") (Var "y")) (
            Seq (Call "f" [Var "x"]) Null)))

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
