module SkScheme.Types
  ( LispVal (..),
    LispError (..),
    ThrowsError,
  )
where

import Control.Monad.Except
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

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

type ThrowsError = Either LispError
