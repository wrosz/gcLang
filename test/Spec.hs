module Main (main) where

import Test.Framework (defaultMain)
import qualified Parser.ParserSpec

main :: IO ()
main = defaultMain Parser.ParserSpec.tests