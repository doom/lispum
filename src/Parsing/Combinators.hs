module Parsing.Combinators
  ( satisfy
  , oneOf
  , noneOf
  , chainl1
  , chainl
  , chainr1
  , chainr
  , sepBy1
  , sepBy
  , endBy1
  , endBy
  , between
  , notFollowedBy
  , eof
  , try
  , choice
  , debug
  ) where

import           Control.Applicative
import           Debug.Trace
import           Parsing.Primitives

-- Given a predicate taking a Char, create a parser who succeeds if the predicate returns true, and fails otherwise
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred =
  Parser $ \input ->
    case parse item input of
      success@(Right result, remaining) ->
        if pred result
          then success
          else (Left "failure", input)
      err -> err

-- Given a list of Char, create a parser that matches and consumes a Char if it is contained in the given list
oneOf :: [Char] -> Parser Char
oneOf acceptedChars = satisfy (flip elem acceptedChars)

-- Given a list of Char, create a parser that matches and consumes a Char if it is not contained in the given list
noneOf :: [Char] -> Parser Char
noneOf rejectedChars = satisfy (flip notElem rejectedChars)

-- Given two parsers p and op, create a parser that matches p (op p)* (op being left-associative)
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  leftOperand <- p
  rest leftOperand
  where
    rest leftOperand =
      (do f <- op
          rightOperand <- p
          rest (f leftOperand rightOperand)) <|>
      return leftOperand

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op init = (chainl1 p op) <|> return init

-- Given two parsers p and op, create a parser that matches p (op p)* (op being right-associative)
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = run
  where
    run = do
      leftOperand <- p
      rest leftOperand
    rest leftOperand =
      (do f <- op
          rightOperand <- run
          rest (f leftOperand rightOperand)) <|>
      return leftOperand

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op init = (chainr1 p op) <|> return init

-- Create a parser to match multiple (at least one) sequences matched by the parser p,
-- separated by sequences matching the parser sep
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  a <- p
  as <- many (sep >> p)
  return (a : as)

-- Create a parser to match multiple (zero or more) sequences matched by the parser p,
-- separated by sequences matching the parser sep
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (sepBy1 p sep) <|> return []

-- Create a parser to match multiple (at least one) sequences matched by the parser p,
-- separated and ended by sequences matching the parser sep
endBy1 :: Parser a -> Parser b -> Parser [a]
endBy1 p sep = many (sep >> p)

-- Create a parser to match multiple (zero or more) sequences matched by the parser p,
-- separated and ended by sequences matching the parser sep
endBy :: Parser a -> Parser b -> Parser [a]
endBy p sep = endBy1 p sep <|> return []

-- Create a parser to match an opening sequence, then apply a parser, then a closing sequence
-- The opening and closing sequences are discarded from the result
between :: Parser open -> Parser a -> Parser close -> Parser a
between openP p closeP = do
  openP
  a <- p
  closeP
  return a

-- Create a parser that fails if the given parser succeeds, or succeeds without consuming anything
notFollowedBy :: Parser a -> Parser ()
notFollowedBy pa =
  Parser $ \input ->
    case parse pa input of
      (Left _, _)  -> (Right (), input)
      (Right _, _) -> (Left "failure", input)

-- Create a parser that succeeds only if there is no more data to read
eof :: Parser ()
eof =
  Parser $ \input ->
    case parse item input of
      (Left _, _)  -> (Right (), input)
      (Right _, _) -> (Left "failure", input)

-- Given a parser, create a parser that applies it, but rewinds the input if it fails
try :: Parser a -> Parser a
try p =
  Parser $ \input ->
    case parse p input of
      (Left errInfo, _) -> (Left errInfo, input)
      result            -> result

choice :: [Parser a] -> Parser a
choice pList = foldl1 (<|>) pList

debug :: Show a => String -> Parser a -> Parser a
debug name p =
  Parser $ \input ->
    case trace (name ++ " input=" ++ input ++ ", ") $ parse p input of
      r@(Left errInfo, remaining) -> trace ("KO: (\"" ++ errInfo ++ "\", \"" ++ remaining ++ "\")") r
      r@(Right result, remaining) -> trace ("OK: (\"" ++ (show result) ++ "\", \"" ++ remaining ++ "\")") r
