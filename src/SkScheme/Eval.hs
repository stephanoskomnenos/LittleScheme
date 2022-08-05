{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module SkScheme.Eval
  ( eval,
    primitiveBindings,
  )
where

import Control.Monad.Error
import Data.Functor ((<&>))
import SkScheme.Env
import SkScheme.Parser
import SkScheme.Types
import System.IO

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Float _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval env pred
    case result of
      Bool False -> eval env alt
      _ -> eval env conseq
eval env (List [Atom "load", String filename]) =
  load filename >>= fmap last . mapM (eval env)
eval env (List [Atom "eval", Atom varId]) = getVar env varId >>= eval env
eval env (List [Atom "eval", List [Atom "quote", quoted]]) =
  eval env quoted
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "let" : (List valueDefList : body))) =
  do
    varBindTuples <- mapM (makeVarBindTuples env) valueDefList
    liftIO (bindVars env varBindTuples) >>= evalBody
  where
    evalBody env = last <$> mapM (eval env) body
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List (Atom "cond" : xs)) =
  do
    let expr = head xs
    case expr of
      (List [Atom "else", pred]) -> eval env pred
      (List [pred, conseq]) -> do
        result <- eval env pred
        case result of
          Bool False -> eval env $ List (Atom "cond" : drop 1 xs)
          _ -> eval env conseq
      badForm -> throwError $ BadSpecialForm "Syntax error in `cond` expression" badForm
eval env (List (function : args)) =
  do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm = throwError $ BadSpecialForm "eval: Unrecognized special form" badForm

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
    >>= flip
      bindVars
      (map (makeFunc PrimitiveFunc) primitives ++ map (makeFunc IOFunc) ioPrimitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop numericAdd),
    ("-", numericBinop numericSub),
    ("*", numericBinop numericMul),
    ("/", numericBinop numericDiv),
    ("mod", numericBinop numericMod),
    ("not", unaryOp notP),
    ("symbol?", unaryOp symbolP),
    ("list?", unaryOp listP),
    ("boolean?", unaryOp booleanP),
    ("string?", unaryOp stringP),
    ("number?", unaryOp numberP),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string>=?", strBoolBinop (>=)),
    ("string<=?", strBoolBinop (<=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else
      liftIO (bindVars closure $ zip params args)
        >>= bindVarArgs varargs
        >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = fmap last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env
apply (IOFunc func) args = func args
apply badForm _ = throwError $ BadSpecialForm "apply: Unrecognized special form" badForm

-- TODO: symbol-handling funtions

notP (Bool n) = (Bool . not) n
notP _ = Bool False

symbolP (Atom _) = Bool True
symbolP _ = Bool False

listP (List _) = Bool True
listP _ = Bool False

booleanP (Bool _) = Bool True
booleanP _ = Bool False

stringP (String _) = Bool True
stringP _ = Bool False

numberP (Number _) = Bool True
numberP _ = Bool False

numericAdd :: LispVal -> LispVal -> ThrowsError LispVal
numericAdd (Number a) (Number b) = return $ Number $ a + b
numericAdd (Float a) (Float b) = return $ Float $ a + b
numericAdd (Float a) (Number b) = return $ Float $ a + fromInteger b
numericAdd (Number a) (Float b) = return $ Float $ fromInteger a + b

numericSub :: LispVal -> LispVal -> ThrowsError LispVal
numericSub (Number a) (Number b) = return $ Number $ a - b
numericSub (Float a) (Float b) = return $ Float $ a - b
numericSub (Float a) (Number b) = return $ Float $ a - fromInteger b
numericSub (Number a) (Float b) = return $ Float $ fromInteger a - b

numericMul :: LispVal -> LispVal -> ThrowsError LispVal
numericMul (Number a) (Number b) = return $ Number $ a * b
numericMul (Float a) (Float b) = return $ Float $ a * b
numericMul (Float a) (Number b) = return $ Float $ a * fromInteger b
numericMul (Number a) (Float b) = return $ Float $ fromInteger a * b

numericDiv :: LispVal -> LispVal -> ThrowsError LispVal
numericDiv (Number a) (Number b) = return $ Number $ div a b
numericDiv (Float a) (Float b) = return $ Float $ a / b
numericDiv (Float a) (Number b) = return $ Float $ a / fromInteger b
numericDiv (Number a) (Float b) = return $ Float $ fromInteger a / b

numericMod :: LispVal -> LispVal -> ThrowsError LispVal
numericMod (Number a) (Number b) = return $ Number $ mod a b
numericMod (Number _) notNumber = throwError $ TypeMismatch "number" notNumber
numericMod notNumber _ = throwError $ TypeMismatch "number" notNumber

numericBinop :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = foldl1M op params

foldl1M :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldl1M f (x : xs) = foldM f x xs

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [param] = return $ op param
unaryOp op params = throwError $ NumArgs 1 params

-- TODO: boolBinop: support Float mixed with Number

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] =
  return $
    Bool $
      (length arg1 == length arg2)
        && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left err -> False
      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- equal :: [LispVal] -> ThrowsError LispVal
-- equal [arg1, arg2] =
--   do
--     primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
--     eqvEquals <- eqv [arg1, arg2]
--     return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

-- data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
-- unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
--   catchError
--     (do
--       unpacked1 <- unpacker arg1
--       unpacked2 <- unpacker arg2
--       return $ unpacked1 == unpacked2)
--     (const $ return False)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarargs = makeFunc . Just . showVal

makeVarBindTuples :: Env -> LispVal -> IOThrowsError (String, LispVal)
makeVarBindTuples env (List [Atom name, valueExpr]) =
  do
    value <- eval env valueExpr
    return (name, value)
makeVarBindTuples _ badForm = throwError $ BadSpecialForm "Syntax error in `let` expression: " badForm
