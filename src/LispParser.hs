module LispParser
  ( expr
  ) where

import           Control.Applicative
import           Lisp
import           Synapto.Combinators
import           Synapto.Primitives
import           Synapto.Token

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

atom :: Parser LispValue
atom = do
  firstChar <- alphabetic <|> symbol
  rest <- many $ alphabetic <|> digit <|> symbol
  return $
    let parsed = (firstChar : rest)
     in case parsed of
          "#t"      -> Bool True
          "#f"      -> Bool False
          otherwise -> Atom parsed

stringLiteral :: Parser LispValue
stringLiteral = do
  literalValue <- doubleQuoted (many $ noneOf "\"")
  return $ String literalValue

numberLiteral :: Parser LispValue
numberLiteral = integer >>= \i -> return $ Number i

list :: Parser LispValue
list = sepBy expr spaces >>= \values -> return $ List values

dottedList :: Parser LispValue
dottedList = do
  sepBy expr (spaces >> char '.' >> spaces) >>= \values -> return $ List values

quotedAtom :: Parser LispValue
quotedAtom = do
  char '\''
  x <- expr
  return $ List [Atom "quote", x]

expr :: Parser LispValue
expr = atom <|> stringLiteral <|> numberLiteral <|> quotedAtom <|> parenthesized (try list <|> dottedList)
