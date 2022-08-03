module SkScheme.Types
  ( LispVal (..),
  )
where

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | Rational Rational
  | String String
  | Bool Bool
