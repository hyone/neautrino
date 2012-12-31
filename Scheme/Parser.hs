-- module Scheme.Parser (
--   readExpr,
--   readExprList
-- ) where
module Scheme.Parser where

import Scheme.Type
import Scheme.Error (ThrowsError, LispError(ParserError))

import Control.Applicative ((<*), (*>), (<$>))
import Control.Monad
import Control.Monad.Error (throwError)
import Data.Array (listArray)
import Data.Char (digitToInt)
import Data.Complex (Complex(..))
import Data.Ratio ((%))
import Numeric (readInt, readOct, readDec, readHex, readFloat)
import Text.ParserCombinators.Parsec hiding (spaces)


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


-- Atom

parseAtom :: Parser LispVal
parseAtom = try $ do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  return $ Atom (first : rest)


-- Char and String

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    characterName <|> character
  where
    character :: Parser LispVal
    character = liftM Character (anyChar <* notFollowedBy alphaNum)

    characterName :: Parser LispVal
    characterName = do
      s <- try (string "space") <|> try (string "newline")
      notFollowedBy alphaNum
      return $ case s of
        "space"   -> Character ' '
        "newline" -> Character '\n'

escapedChar :: Parser Char
escapedChar = do
  char '\\'
  c <- oneOf "tnr\\\""
  return $ case c of
    't' -> '\t'
    'n' -> '\n'
    'r' -> '\r'
    _   -> c

parseString :: Parser LispVal
parseString = try $ do
  char '"'
  x <- many (noneOf "\"\\" <|> escapedChar)
  char '"'
  return (String x)


-- Boolean

parseBoolean :: Parser LispVal
parseBoolean = try $ do
  char '#'
  s <- oneOf "tf"
  return $ case s of
    't' -> Bool True
    'f' -> Bool False


-- Number

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (`elem` "o01") digitToInt

parseBinLiteral :: Parser LispVal
parseBinLiteral = do
  try $ string "#b"
  s <- many1 (oneOf "01")
  notFollowedBy alphaNum
  case readBin s of
    [(x, _)] -> return (Number x)
    _        -> fail "invalid binary number"

parseOctLiteral :: Parser LispVal
parseOctLiteral = do
  try $ string "#o"
  s <- many1 octDigit
  notFollowedBy alphaNum
  case readOct s of
    [(x, _)] -> return (Number x)
    _        -> fail "invalid octal number"

parseDecLiteral :: Parser LispVal
parseDecLiteral = do
  try $ string "#d"
  s <- many1 digit
  notFollowedBy alphaNum
  case readDec s of
    [(x, _)] -> return (Number x)
    _        -> fail "invalid decimal number"

parseHexLiteral :: Parser LispVal
parseHexLiteral = do
  try $ string "#x"
  s <- many1 digit
  notFollowedBy alphaNum
  case readDec s of
    [(x, _)] -> return (Number x)
    _ -> fail "invalid hex number"

parseDigit :: Parser LispVal
parseDigit = (Number . read) <$> many1 digit <* notFollowedBy alphaNum

parseNumber :: Parser LispVal
parseNumber = parseBinLiteral
          <|> parseOctLiteral
          <|> parseDecLiteral
          <|> parseHexLiteral
          <|> parseDigit

-- Float

float = do
  s1 <- many digit
  c  <- char '.'
  s2 <- many digit
  return (s1 ++ c : s2)

floatWithExp = do
  s1 <- try float <|> many1 digit
  c  <- char 'e'
  s2 <- many1 digit
  return (s1 ++ c : s2)

parseFloat :: Parser LispVal
parseFloat = try $ do
  s <- try floatWithExp <|> float
  case readFloat s of
    [(x, _)] -> return (Float x)
    _        -> fail "invalid float number"

-- Ratio

parseRatio :: Parser LispVal
parseRatio = try $ do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio (read x % read y)

-- Complex

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = try $ do
  x <- parseFloat <|> parseNumber
  char '+'
  y <- parseFloat <|> parseNumber
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)


parseNumeric :: Parser LispVal
parseNumeric = parseComplex
           <|> parseRatio
           <|> parseFloat
           <|> parseNumber


-- Vector, List, DottedList

parseVector :: Parser LispVal
parseVector = try $ string "#(" *> parseVector' <* char ')'
  where
    parseVector' :: Parser LispVal
    parseVector' = do
      arrayValues <- sepBy parseExpr spaces
      return . Vector $ listArray (0, length arrayValues - 1) arrayValues

parseListLiteral :: Parser LispVal
parseListLiteral = List <$> sepEndBy parseExpr spaces

parseDottedListLiteral :: Parser LispVal
parseDottedListLiteral = do
  head <- sepEndBy1 parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseAnyList :: Parser LispVal
parseAnyList = try $ do
  char '('
  optional spaces
  e <- try parseDottedListLiteral <|> parseListLiteral
  optional spaces
  char ')'
  return e


-- Quote

parseQuoted :: Parser LispVal
parseQuoted = try $ do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = try $ do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = try $ do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseComment :: Parser ()
parseComment = try $ do
  char ';'
  manyTill anyChar (char '\n')
  return ()


-- API

parseExpr :: Parser LispVal
parseExpr = do
  skipMany parseComment
  -- remove newline or spaces just after comment
  skipMany (oneOf "\n\r" <|> space)
  parseAtom <|> parseChar
            <|> parseString
            <|> parseNumeric
            <|> parseBoolean
            <|> parseVector
            <|> parseAnyList
            <|> parseQuoted
            <|> parseQuasiQuoted
            <|> parseUnQuote

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left  err -> throwError (ParserError err)
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ 
  optional spaces *> sepEndBy parseExpr spaces
