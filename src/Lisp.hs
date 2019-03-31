module Lisp
  ( Value(..)
  , Error(..)
  , Function(..)
  , FunctionArgumentSpec(..)
  , ThrowsError
  , trapError
  , extractValue
  , validateArguments
  ) where

import           Control.Monad.Except

data Value
  = Atom String
  | Number Integer
  | String String
  | Bool Bool
  | List [Value]

instance Show Value where
  show (Atom a)   = a
  show (Number i) = (show i)
  show (String s) = "\"" ++ s ++ "\""
  show (Bool b)   = (show b)
  show (List l)   = "(" ++ (unwords . map show) l ++ ")"

data FunctionArgumentSpec
  = Exactly Integer
  | AtLeast Integer
  | AtMost Integer

instance Show FunctionArgumentSpec where
  show (Exactly n)
    | n <= 1 = "exactly " ++ show n ++ " argument"
    | otherwise = "exactly " ++ show n ++ " arguments"
  show (AtLeast n)
    | n <= 1 = "at least " ++ show n ++ " argument"
    | otherwise = "at least " ++ show n ++ " arguments"
  show (AtMost n)
    | n <= 1 = "at most " ++ show n ++ " argument"
    | otherwise = "at most " ++ show n ++ " arguments"

validateArguments :: FunctionArgumentSpec -> Integer -> Bool
validateArguments (Exactly n) = (== n)
validateArguments (AtLeast n) = (>= n)
validateArguments (AtMost n)  = (<= n)

data Error
  = ArgsNumberMismatch FunctionArgumentSpec
                       [Value]
  | TypeMismatch String
                 Value
  | Invalid String
  | InvalidSpecialForm Value
  | NotAFunction String
  | UnboundVariable String
                    String

instance Show Error where
  show (ArgsNumberMismatch n actual) = "expected " ++ show n ++ ", got: " ++ (unwords . map show) actual
  show (TypeMismatch typ actual) = "expected type " ++ typ ++ ", got " ++ show actual
  show (Invalid err) = "parse error: " ++ err
  show (InvalidSpecialForm form) = "invalid special form: '" ++ show form ++ "'"
  show (NotAFunction funcname) = funcname ++ " is not a function"
  show (UnboundVariable msg varname) = "unbound variable '" ++ varname ++ "'"

type ThrowsError = Either Error

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

data Function = Function
  { argSpec :: FunctionArgumentSpec
  , call    :: ([Value] -> ThrowsError Value)
  }
