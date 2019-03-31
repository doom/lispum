module LispParser
  ( expr
  ) where

import           Control.Applicative
import qualified Lisp
import           Synapto.Combinators
import           Synapto.Primitives
import           Synapto.Token

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

atom :: Parser Lisp.Value
atom = do
  firstChar <- alphabetic <|> symbol
  rest <- many $ alphabetic <|> digit <|> symbol
  return $
    let parsed = (firstChar : rest)
     in case parsed of
          "#t"      -> Lisp.Bool True
          "#f"      -> Lisp.Bool False
          otherwise -> Lisp.Atom parsed

stringLiteral :: Parser Lisp.Value
stringLiteral = do
  literalValue <- doubleQuoted (many $ noneOf "\"")
  return $ Lisp.String literalValue

numberLiteral :: Parser Lisp.Value
numberLiteral = integer >>= \i -> return $ Lisp.Number i

list :: Parser Lisp.Value
list = sepBy expr spaces >>= \values -> return $ Lisp.List values

dottedList :: Parser Lisp.Value
dottedList = do
  sepBy expr (spaces >> char '.' >> spaces) >>= \values -> return $ Lisp.List values

quotedAtom :: Parser Lisp.Value
quotedAtom = do
  char '\''
  x <- expr
  return $ Lisp.List [Lisp.Atom "quote", x]

expr :: Parser Lisp.Value
expr = atom <|> stringLiteral <|> numberLiteral <|> quotedAtom <|> parenthesized (try list <|> dottedList)
