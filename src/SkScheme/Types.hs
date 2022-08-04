module SkScheme.Types
  ( LispVal (..),
    LispError (..),
    ThrowsError,
    Env,
    IOThrowsError,
  )
where

import Control.Monad.Error
import Data.IORef
import Text.Parsec (ParseError)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | Rational Rational
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

type ThrowsError = Either LispError

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO
