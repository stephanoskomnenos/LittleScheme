module Main where

import System.Environment

import SkScheme.Eval
import SkScheme.Parser

main :: IO ()
main = do
  args <- getArgs
  (print . eval . readExpr . head) args

