module Parsing.Token
  ( char
  , string
  , digit
  , alphabetic
  , alphaNum
  , integer
  , parenthesized
  , doubleQuoted
  , singleQuoted
  , spaces
  , token
  ) where

import           Control.Applicative
import           Data.Char
import           Parsing.Combinators
import           Parsing.Primitives

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string [] = return []
string (c:rest) = do
  char c
  string rest
  return (c : rest)

digit :: Parser Char
digit = satisfy isDigit

alphabetic :: Parser Char
alphabetic = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

integer :: Parser Integer
integer = do
  sign <- string "-" <|> return []
  digits <- some digit
  return $ read (sign ++ digits)

parenthesized :: Parser a -> Parser a
parenthesized p = between (char '(') p (char ')'<?> "unmatched ')'")

doubleQuoted :: Parser a -> Parser a
doubleQuoted p = between (char '"') p (char '"' <?> "unmatched '\"'")

singleQuoted :: Parser a -> Parser a
singleQuoted p = between (char '\'') p (char '\'' <?> "unmatched '''")

spaces :: Parser String
spaces = many $ oneOf " \n\r\t"

token :: Parser a -> Parser a
token p = do
  spaces
  a <- p
  spaces
  return a
