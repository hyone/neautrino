{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Parser implementation for Scheme language
module Neautrino.Internal.Parser where

import Neautrino.Type
import Neautrino.Error (throwError, ErrorM, LispError(ParserError))

import Control.Applicative ((<*), (*>), (<$>))
import Control.Monad
import Data.Array (listArray)
import Data.Char (digitToInt, isDigit)
import Data.Complex (Complex(..))
import Data.Ratio ((%))
import Numeric (readInt, readOct, readDec, readHex, readFloat)
import Text.Parsec hiding (spaces)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P


-- Tokens

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~."

schemeDef :: LanguageDef ()
schemeDef = emptyDef    
  { P.commentStart   = "#|"
  , P.commentEnd     = "|#"
  , P.commentLine    = ";"
  , P.nestedComments = True
  , P.identStart     = letter <|> symbol
  , P.identLetter    = letter <|> digit <|> symbol
  , P.reservedNames  = []
  , P.caseSensitive  = True
  } 

lexer = P.makeTokenParser schemeDef

parens :: Parser a -> Parser a
parens = P.parens lexer

dot :: Parser String
dot = P.dot lexer

identifier :: Parser String
identifier = P.identifier lexer

whiteSpace :: Parser()
whiteSpace = P.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer


-- Atom

parseAtom :: Parser LispVal
-- parseAtom = Atom <$> identifier
parseAtom = do
  name <- identifier
  case name of
    "."                             -> parserZero
    (a:b:_) | a == '.' && isDigit b -> parserZero
    _                               ->  return (Atom name)


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


-- Bool

parseBool :: Parser LispVal
parseBool = try $ do
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
parseVector = string "#(" *> parseVector' <* char ')'
  where
    parseVector' :: Parser LispVal
    parseVector' = do
      arrayValues <- sepBy parseExpr whiteSpace
      return . Vector $ listArray (0, length arrayValues - 1) arrayValues

parseList :: Parser LispVal
parseList = List <$> sepEndBy parseExpr whiteSpace

parsePair :: Parser LispVal
parsePair = do
  h <- sepEndBy1 parseExpr whiteSpace
  t <- dot >> parseExpr
  return $ Pair h t

parseAnyList :: Parser LispVal
parseAnyList = try (parens parseList) <|> parens parsePair


-- Quote

parseQuoted :: Parser LispVal
parseQuoted = do
  lexeme $ char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  lexeme $ char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  lexeme $ char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- Note: need to try before parseUnQuote
parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = try $ do
  lexeme $ try (string ",@")
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]


-- API

parseExpr :: Parser LispVal
parseExpr = do
  optional whiteSpace 
  lexeme $ try parseAtom
       <|> parseChar
       <|> parseString
       <|> parseNumeric
       <|> parseBool
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
readExprList = readOrThrow $ sepEndBy parseExpr whiteSpace
