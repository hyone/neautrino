{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Parser implementation for Scheme language
module Neautrino.Internal.Parser where

import Neautrino.Type
import Neautrino.Error (throwError, ErrorM, LispError(ParserError))

import Control.Applicative ((<*), (*>), (<$>))
import Control.Monad
import Data.Array (listArray)
import Data.Char (digitToInt)
import Data.Complex (Complex(..))
import Data.Ratio ((%))
import Numeric (readInt, readOct, readDec, readHex, readFloat)
import Text.Parsec hiding (spaces)
import Text.Parsec.String (Parser)


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

eol :: Parser Char
eol = newline <|> (eof >> return '\n')


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
      case s of
        "space"   -> return $ Character ' '
        "newline" -> return $ Character '\n'
        _         -> fail $ "parse error at #\\" ++ s


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
  c <- oneOf "tf"
  case c of
    't' -> return $ Bool True
    'f' -> return $ Bool False
    _   -> fail $ "parse error at #\\" ++ [c] 


-- Integer

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (`elem` "o01") digitToInt

parseBinLiteral :: Parser LispVal
parseBinLiteral = do
  try $ string "#b"
  s <- many1 (oneOf "01")
  case readBin s of
    [(x, _)] -> return (Integer x)
    _        -> fail "invalid binary number"

parseOctLiteral :: Parser LispVal
parseOctLiteral = do
  try $ string "#o"
  s <- many1 octDigit
  case readOct s of
    [(x, _)] -> return (Integer x)
    _        -> fail "invalid octal number"

parseDecLiteral :: Parser LispVal
parseDecLiteral = do
  try $ string "#d"
  s <- many1 digit
  case readDec s of
    [(x, _)] -> return (Integer x)
    _        -> fail "invalid decimal number"

parseHexLiteral :: Parser LispVal
parseHexLiteral = do
  try $ string "#x"
  s <- many1 hexDigit
  case readHex s of
    [(x, _)] -> return (Integer x)
    _ -> fail "invalid hex number"

parseDigit :: Parser LispVal
parseDigit = (Integer . read) <$> many1 digit

parseNumber :: Parser LispVal
parseNumber = parseBinLiteral
          <|> parseOctLiteral
          <|> parseDecLiteral
          <|> parseHexLiteral
          <|> parseDigit

-- Float

float :: Parser String
float = do
  s1 <- many digit
  c  <- char '.'
  s2 <- many digit
  return (s1 ++ c : s2)

floatWithExp :: Parser String
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
  Integer x <- parseNumber
  char '/'
  y <- many1 digit
  return $ Ratio (x % read y)

-- Complex

toDouble :: LispVal -> Double
toDouble (Float f)  = f
toDouble (Integer n) = fromIntegral n
toDouble _          = error "Not a number."

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


-- Vector, List, Pair

parseVector :: Parser LispVal
parseVector = try $ string "#(" *> parseVector' <* char ')'
  where
    parseVector' :: Parser LispVal
    parseVector' = do
      arrayValues <- sepBy parseExpr spaces
      return . Vector $ listArray (0, length arrayValues - 1) arrayValues

parseListLiteral :: Parser LispVal
parseListLiteral = List <$> sepEndBy parseExpr spaces

parsePairLiteral :: Parser LispVal
parsePairLiteral = do
  h <- sepEndBy1 parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ Pair h t

parseAnyList :: Parser LispVal
parseAnyList = try $ do
  char '('
  optional spaces
  e <- try parsePairLiteral <|> parseListLiteral
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

-- Note: need to try before parseUnQuote
parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = try $ do
  try $ string ",@"
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseComment :: Parser ()
parseComment = try $ do
  char ';'
  manyTill anyChar eol
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
            <|> parseUnQuoteSplicing
            <|> parseUnQuote

readOrThrow :: Parser a -> String -> ErrorM a
readOrThrow parser input = case parse parser "lisp" input of
  Left  err -> throwError (ParserError err)
  Right val -> return val

readExpr :: String -> ErrorM LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ErrorM [LispVal]
readExprList = readOrThrow $ 
  optional spaces *> sepEndBy parseExpr spaces
