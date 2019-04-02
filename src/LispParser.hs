module LispParser
  ( expr
  , readOneExpr
  , readMultipleExpr
  ) where

import           Control.Applicative
import           Control.Monad.Except
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

runAndCheck :: Parser a -> String -> Lisp.ThrowsError a
runAndCheck p input =
  case parse p input of
    (Left errInfo, _) -> throwError $ Lisp.Invalid errInfo
    (Right res, _)    -> return res

-- | Parse a single LISP expression from a given string
readOneExpr :: String -> Lisp.ThrowsError Lisp.Value
readOneExpr =
  runAndCheck
    (do expr <- token (LispParser.expr <??> "expected LISP expression")
        eof <?> "expected EOL after LISP expression"
        return expr)

-- | Parse one or multiple LISP expressions from a given string
readMultipleExpr :: String -> Lisp.ThrowsError [Lisp.Value]
readMultipleExpr = runAndCheck (many $ token expr)
