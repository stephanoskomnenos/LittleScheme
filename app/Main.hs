module Main where

import System.Environment

import SkScheme.Eval
import SkScheme.Parser

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled

