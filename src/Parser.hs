-- MiniGC Parser using Parsec

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE InstanceSigs #-}
module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Data.List (intercalate)

-- AST Definitions

data Program = Program [FuncDef] Expr
  deriving Eq

data FuncDef = FuncDef String [String] Expr
  deriving Eq

data Type
  = TInt
  | TBool
  | TObject [String]
  | TArray
  deriving (Show, Eq)

data BinOp = Add | Sub | Eq
  deriving (Show, Eq)

data Expr
  = Var String
  | IntLit Int
  | BoolLit Bool
  | BinOp BinOp Expr Expr
  | If Expr Expr Expr
  | Let String Expr Expr
  | Call String [Expr]
  | New [String] [Expr]
  | NewArray Expr Expr
  | FieldAccess Expr String
  | FieldAssign Expr String Expr
  | ArrayAccess Expr Expr
  | ArrayAssign Expr Expr Expr
  | Seq Expr Expr
  | Null
  deriving Eq

instance Show Expr where
  show :: Expr -> String
  show (Var name) = name
  show (IntLit n) = show n
  show (BoolLit True) = "true"
  show (BoolLit False) = "false"
  show (BinOp Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (BinOp Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (BinOp Eq x y) = "(" ++ show x ++ " == " ++ show y ++ ")"
  show (If cond tr fl) = "if " ++ show cond ++ "\n"
                            ++ indent ("then {\n" ++ indent (show tr) ++ "}")
                            ++ indent ("else {\n" ++ indent (show fl) ++ "}")
  show (Let var val body) = "let " ++ var ++ " = " ++ show val ++ ";\n" ++ show body
  show (Call func args) = func ++ "(" ++ commaSep' (map show args) ++ ")"
  show (New names values) = "new " ++ " ["
                          ++ commaSep' (map (\name -> "\"" ++ name ++ "\"") names) ++ "] ["
                          ++ commaSep' (map show values) ++ "]"
  show (NewArray size initVal) = "newArray " ++ show size ++ " " ++ show initVal
  show (FieldAccess obj field) = show obj ++ "." ++ field
  show (FieldAssign obj field val) = show obj ++ "." ++ field ++ " = " ++ show val
  show (ArrayAccess arr idx) = show arr ++ "[" ++ show idx ++ "]"
  show (ArrayAssign arr idx val) = show arr ++ "[" ++ show idx ++ "] = " ++ show val
  show (Seq expr1 expr2) = show expr1 ++";\n" ++ show expr2
  show Null = "null"

instance Show FuncDef where
  show :: FuncDef -> String
  show (FuncDef name args body) = "def " ++ name ++ "(" ++ commaSep' args ++ ") = {\n"
                                  ++ indent (show body) ++ "}\n"

instance Show Program where
  show :: Program -> String
  show (Program funcs body) = unlines (map show funcs) ++ show body

-- Helper functions
indent :: String -> String
indent = unlines . map ("  " ++) . lines

commaSep' :: [String] -> String
commaSep' = intercalate ", "


-- Lexer
languageDef :: Tok.LanguageDef ()
languageDef = emptyDef
  { Tok.commentLine     = "--"
  , Tok.reservedNames   = ["def", "let", "new", "newArray", "null", "if", "then", "else"]
  , Tok.reservedOpNames = ["=", ".", "+", "-", "=="]
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser languageDef

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

integer :: Parser Integer
integer = Tok.integer lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

semi :: Parser String
semi = Tok.semi lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer


-- Expression Parser
parseExpr :: Parser Expr
parseExpr = do
  notFollowedBy eof <?> "expression, but input is empty."
  buildExpressionParser table parseTerm
  where
    table = [ [Infix (reservedOp "+" >> return (BinOp Add)) AssocLeft
              ,Infix (reservedOp "-" >> return (BinOp Sub)) AssocLeft]
            , [Infix (reservedOp "==" >> return (BinOp Eq)) AssocNone]
            ]

parseBaseTerm :: Parser Expr
parseBaseTerm = parens parseExpr
            <|> parseLiteral
            <|> parseIf
            <|> parseLet
            <|> parseNewArray
            <|> parseNew
            <|> try parseCall
            <|> Var <$> identifier

parseTerm :: Parser Expr
parseTerm = try parseArrayAssign
            <|> try parseArrayAccess
            <|> try parseFieldAssign
            <|> try parseFieldAccess
            <|> parseBaseTerm

-- parse "null", "true", "false"
parseLiteral :: Parser Expr
parseLiteral =
      (reserved "null" >> return Null)
  <|> (IntLit . fromInteger <$> integer)
  <|> (BoolLit True <$ reserved "true")
  <|> (BoolLit False <$ reserved "false")

-- parse "if cond then tr else fl"
parseIf :: Parser Expr
parseIf = do
  reserved "if"
  notFollowedBy (reserved "then") <?> "condition expression after 'if'"
  cond <- parseExpr
  reserved "then"
  notFollowedBy (reserved "else") <?> "condition expression after 'then'"
  tr <- braces parseSeqExpr <|> parseExpr
  reserved "else"
  fl <- braces parseSeqExpr <|> parseExpr
  return $ If cond tr fl

-- parse "let var = val; body"
parseLet :: Parser Expr
parseLet = do
  reserved "let"
  var <- identifier
  reservedOp "="
  val <- parseExpr
  semi
  body <- parseSeqExpr
  return $ Let var val body

-- parse "newArray size initVal"
parseNewArray :: Parser Expr
parseNewArray = do
  reserved "newArray"
  size <- parseExpr
  initVal <- parseExpr
  return $ NewArray size initVal

-- parse "new [names] [values]"
parseNew :: Parser Expr
parseNew = do
  reserved "new"
  names <- brackets $ commaSep stringLiteral
  values <- brackets $ commaSep parseExpr
  return $ New names values

-- parse "func(args)"
parseCall :: Parser Expr
parseCall = do
  func <- identifier
  args <- parens $ commaSep parseExpr
  return $ Call func args

-- parse "arr[idx]"
parseArrayAccess :: Parser Expr
parseArrayAccess = do
  arr <- parseBaseTerm
  idx <- brackets parseExpr
  return $ ArrayAccess arr idx

-- parse "arr[idx] = val"
parseArrayAssign :: Parser Expr
parseArrayAssign = do
  arr <- parseBaseTerm
  idx <- brackets parseExpr
  reservedOp "="
  val <- parseExpr
  return $ ArrayAssign arr idx val

-- parse "obj.field"
parseFieldAccess :: Parser Expr
parseFieldAccess = do
  obj <- parseBaseTerm
  reservedOp "."
  field <- identifier
  return $ FieldAccess obj field

-- parse "obj.field = val"
parseFieldAssign :: Parser Expr
parseFieldAssign = do
  obj <- parseBaseTerm
  reservedOp "."
  field <- identifier
  reservedOp "="
  val <- parseExpr
  return $ FieldAssign obj field val

-- parse a sequence of expressions separated by commas
parseSeqExpr :: Parser Expr
parseSeqExpr = do
  exprs <- parseExpr `sepBy1` symbol ";"
  return $ foldr1 Seq exprs

-- function and program Parser
parseFuncDef :: Parser FuncDef
parseFuncDef = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep identifier
  reservedOp "="
  body <- braces parseSeqExpr <|> parseExpr
  return $ FuncDef name args body

parseProgram :: Parser Program
parseProgram = do
  whiteSpace
  funcs <- many parseFuncDef
  expr <- parseSeqExpr
  eof
  return $ Program funcs expr

-- Top-level parser
parseMiniGC :: String -> Either ParseError Program
parseMiniGC input = parse parseProgram "<stdin>" input