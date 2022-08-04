module SkScheme.Repl
  ( runOne,
    runRepl,
  )
where

import SkScheme.Env
import SkScheme.Eval
import SkScheme.Parser
import SkScheme.Types
import System.IO hiding (try)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
  do
    result <- prompt
    if pred result
      then return ()
      else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args =
  do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (fmap show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "LittleScheme > ") . evalAndPrint
