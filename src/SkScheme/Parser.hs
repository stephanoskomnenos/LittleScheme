module SkScheme.Parser
  ( readExpr,
  )
where

import Data.Functor
import Data.Ratio (Rational, (%))
import Numeric (readHex, readOct)
import SkScheme.Eval
import SkScheme.Types
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error 

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf ['\\', '"'] <|> escapeCharactors)
  char '"'
  return $ String x

escapeCharactors :: Parser Char
escapeCharactors = do
  char '\\'
  c <- oneOf ['\\', '"', 'r', 'n', 't']
  return $ case c of
    '\\' -> c
    'r' -> '\r'
    'n' -> '\n'
    't' -> '\t'
    _ -> '\0'

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseBool :: Parser LispVal
parseBool = do
  char '#'
  val <- oneOf "tf"
  return $ case val of
    't' -> Bool True
    _ -> Bool False

parseNumber :: Parser LispVal
parseNumber =
  parseNumberNoPrefix
    <|> parseNumberWithPrefix

parseNumberWithPrefix :: Parser LispVal
parseNumberWithPrefix = do
  char '#'
  parseNumberOct <|> parseNumberDec <|> parseNumberHex <|> parseNumberBin

parseNumberDec :: Parser LispVal
parseNumberDec = do
  char 'd'
  parseNumberNoPrefix

binStrToInt :: String -> Int -> Int
binStrToInt str sum =
  if str == ""
    then sum
    else
      ( do
          let x : restStr = str
          let thisBit = if x == '0' then 0 else 1
          binStrToInt restStr (sum * 2 + thisBit)
      )

parseNumberBin :: Parser LispVal
parseNumberBin = do
  char 'b'
  num <- many1 $ oneOf "01"
  return . Number . toInteger $ binStrToInt num 0

parseNumberOct :: Parser LispVal
parseNumberOct = do
  char 'o'
  num <- many1 $ oneOf "01234567"
  return $ (Number . fst . head . readOct) num

parseNumberHex :: Parser LispVal
parseNumberHex = do
  char 'x'
  num <- many1 $ oneOf "0123456789ABCDEFabcdef"
  return $ (Number . fst . head . readHex) num

parseNumberNoPrefix :: Parser LispVal
parseNumberNoPrefix = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr =
  try parseBool
    <|> try parseFloat
    <|> try parseRational
    <|> parseNumber
    <|> parseString
    <|> parseAtom
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

-- parseNumberA :: Parser LispVal
-- parseNumberA = do
--   digitStr <- many1 digit
--   return $ (Number . read) digitStr

-- parseNumberB :: Parser LispVal
-- parseNumberB = many1 digit >>= return . Number . read

parseFloat :: Parser LispVal
parseFloat = do
  int <- many1 digit
  char '.'
  frac <- many1 digit
  return $ Float (read (int ++ "." ++ frac) :: Double)

parseRational :: Parser LispVal
parseRational = do
  num <- read <$> many1 digit
  char '/'
  denom <- read <$> many1 digit
  return $ Rational (num % denom)

-- TODO: parseComplex

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
