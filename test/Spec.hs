module Main (main) where

import Test.Framework (defaultMain)
import qualified Parser.ParserSpec
import qualified SemanticsSpec

main :: IO ()
main = defaultMain $
  Parser.ParserSpec.tests ++
  SemanticsSpec.tests
