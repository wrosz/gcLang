-- MiniGC Parser using Parsec
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Data.List (intercalate)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Data.Functor.Identity (Identity)

-- AST Definitions

data Program = Program [FuncDef] Expr
  deriving (Show, Eq)

data FuncDef = FuncDef String [String] Expr
  deriving (Show, Eq)

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
  deriving (Show, Eq)

-- Lexer
languageDef :: Tok.LanguageDef ()
languageDef = emptyDef
  { Tok.commentLine     = "--"
  , Tok.reservedNames   = ["def", "let", "new", "newArray", "null", "if", "then", "else"]
  , Tok.reservedOpNames = ["=", ".", "+", "-", "=="]
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser languageDef

identifier = Tok.identifier lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
parens     = Tok.parens lexer
braces     = Tok.braces lexer
brackets   = Tok.brackets lexer
commaSep   = Tok.commaSep lexer
whiteSpace = Tok.whiteSpace lexer
integer    = Tok.integer lexer
symbol     = Tok.symbol lexer

-- Expression Parser
parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseTerm

  where
    table = [ [Infix (reservedOp "+" >> return (BinOp Add)) AssocLeft
              ,Infix (reservedOp "-" >> return (BinOp Sub)) AssocLeft]
            , [Infix (reservedOp "==" >> return (BinOp Eq)) AssocNone]
            ]

parseTerm :: Parser Expr
parseTerm =  try parseCall
         <|> parens parseExpr
         <|> parseLiteral
         <|> Var <$> identifier
         <|> parseIf
         <|> parseLet
         <|> parseNew
         <|> parseNewArray
         <|> parseAccess

parseLiteral :: Parser Expr
parseLiteral =
      (reserved "null" >> return Null)
  <|> (IntLit . fromInteger <$> integer)
  <|> (BoolLit True <$ reserved "true")
  <|> (BoolLit False <$ reserved "false")

parseIf :: Parser Expr
parseIf = do
  reserved "if"
  cond <- parseExpr
  reserved "then"
  tr <- parseExpr
  reserved "else"
  fl <- parseExpr
  return $ If cond tr fl

parseLet :: Parser Expr
parseLet = do
  reserved "let"
  var <- identifier
  reservedOp "="
  val <- parseExpr
  body <- parseExpr
  return $ Let var val body

parseNew :: Parser Expr
parseNew = do
  reserved "new"
  names <- brackets $ commaSep identifier
  values <- brackets $ commaSep parseExpr
  return $ New names values

parseNewArray :: Parser Expr
parseNewArray = do
  reserved "newArray"
  size <- parseExpr
  initVal <- parseExpr
  return $ NewArray size initVal

parseCall :: Parser Expr
parseCall = do
  func <- identifier
  args <- parens $ commaSep parseExpr
  return $ Call func args

parseAccess :: Parser Expr
parseAccess = try parseArrayAssign <|> try parseArrayAccess <|> try parseFieldAssign <|> parseFieldAccess

parseFieldAccess :: Parser Expr
parseFieldAccess = do
  obj <- parseTerm
  reservedOp "."
  field <- identifier
  return $ FieldAccess obj field

parseFieldAssign :: Parser Expr
parseFieldAssign = do
  obj <- parseTerm
  reservedOp "."
  field <- identifier
  reservedOp "="
  val <- parseExpr
  return $ FieldAssign obj field val

parseArrayAccess :: Parser Expr
parseArrayAccess = do
  arr <- parseTerm
  idx <- brackets parseExpr
  return $ ArrayAccess arr idx

parseArrayAssign :: Parser Expr
parseArrayAssign = do
  arr <- parseTerm
  idx <- brackets parseExpr
  reservedOp "="
  val <- parseExpr
  return $ ArrayAssign arr idx val

-- Function and Program Parser
parseFuncDef :: Parser FuncDef
parseFuncDef = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep identifier
  reservedOp "="
  body <- braces parseExpr <|> parseExpr
  return $ FuncDef name args body

parseProgram :: Parser Program
parseProgram = do
  whiteSpace
  funcs <- many parseFuncDef
  expr <- parseExpr
  eof
  return $ Program funcs expr

-- Top-level parser
parseMiniGC :: String -> Either ParseError Program
parseMiniGC input = parse parseProgram "<stdin>" input
