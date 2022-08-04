module Main where

import SkScheme.Eval
import SkScheme.Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
