module Parsing.Primitives
  ( ErrorReason
  , Parser(Parser)
  , parse
  , item
  , unit
  , bind
  , failure
  , option
  , (<|>)
  , (<?>)
  , (<??>)
  ) where

import           Control.Applicative
import           Control.Monad

type ErrorReason = String

type SourceLine = Integer

type SourceColumn = Integer

data SourcePoint = SourcePoint
  { sourceLine   :: SourceLine
  , sourceColumn :: SourceColumn
  } deriving (Eq, Ord, Show)

nextSourceLine :: SourcePoint -> SourcePoint
nextSourceLine point = SourcePoint ((sourceLine point) + 1) (sourceColumn point)

nextSourceColumn :: SourcePoint -> SourcePoint
nextSourceColumn point = SourcePoint (sourceLine point) ((sourceColumn point) + 1)

newtype Parser a = Parser
    -- The parser result is a pair, whose
    -- - first element is either an ErrorReason containing information about the error, or the result
    -- - second element is the remaining characters to read
  { parse :: String -> (Either ErrorReason a, String)
  }

runParser :: Parser a -> String -> (Either ErrorReason a, String)
runParser = parse

item :: Parser Char
item =
  Parser $ \input ->
    case input of
      []            -> (Left "no data", [])
      (c:remaining) -> (Right c, remaining)

-- fmap allows applying a function to a parser's result, on success
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input ->
      case p input of
        (Left errInfo, remaining) -> (Left errInfo, remaining)
        (Right result, remaining) -> (Right (f result), remaining)

instance Applicative Parser where
  pure = return
  Parser p1 <*> Parser p2 =
    Parser $ \input ->
      case p1 input of
        (Left errInfo, remaining) -> (Left errInfo, remaining)
        (Right applyF, remaining) ->
          case p2 remaining of
            (Left errInfo, remaining) -> (Left errInfo, remaining)
            (Right result, remaining) -> (Right (applyF result), remaining)

-- Create a parser based on the result of another parser
-- (by sequencing a parser and a "parser constructor")
bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser pa) fapb =
  Parser $ \input ->
    case pa input of
      (Left errInfo, remaining) -> (Left errInfo, remaining)
      (Right result, remaining) -> parse (fapb result) remaining

-- Given a value, create a parser that always returns the same value, and doesn't consume any input
unit :: a -> Parser a
unit value = Parser (\input -> (Right value, input))

instance Monad Parser where
  return = unit
  (>>=) = bind

-- A parser that always fail
failure :: Parser a
failure = Parser $ \input -> (Left "failure", input)

-- Create a parser that chooses between two parsers.
-- The first one is tried first, and then, only on failure, the second one is tried
option :: Parser a -> Parser a -> Parser a
option (Parser pa1) (Parser pa2) =
  Parser $ \input ->
    case pa1 input of
      err@(Left errInfo, remaining) ->
        if remaining == input
          then pa2 input
          else err
      result -> result

instance Alternative Parser where
  empty = failure
  (<|>) = option

infix 0 <??>

(<??>) :: Parser a -> String -> Parser a
(Parser p) <??> msg =
  Parser $ \input ->
    case p input of
      (Left errInfo, remaining) -> (Left (msg ++ ": " ++ errInfo), remaining)
      result                    -> result

infix 0 <?>

(<?>) :: Parser a -> String -> Parser a
(Parser p) <?> msg =
  Parser $ \input ->
    case p input of
      (Left errInfo, remaining) -> (Left msg, remaining)
      result                    -> result
