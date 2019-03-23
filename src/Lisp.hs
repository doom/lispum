module Lisp
  ( LispValue(..)
  , LispError(..)
  , ThrowsLispError
  , trapLispError
  , extractLispValue
  ) where

import           Control.Monad.Except

data LispValue
  = Atom String
  | Number Integer
  | String String
  | Bool Bool
  | List [LispValue]

instance Show LispValue where
  show (Atom a)   = a
  show (Number i) = (show i)
  show (String s) = "\"" ++ s ++ "\""
  show (Bool b)   = (show b)
  show (List l)   = "(" ++ (unwords . map show) l ++ ")"

data LispError
  = ArgsNumberMismatch Integer
                       [LispValue]
  | TypeMismatch String
                 LispValue
  | Invalid String
  | InvalidSpecialForm LispValue
  | NotAFunction
                 String
  | UnboundVariable String
                    String

instance Show LispError where
  show (ArgsNumberMismatch n actual) = "expected " ++ show n ++ " arguments, got " ++ (unwords . map show) actual
  show (TypeMismatch typ actual) = "expected type " ++ typ ++ ", got " ++ show actual
  show (Invalid err) = "parse error: " ++ err
  show (InvalidSpecialForm form) = "invalid special form: '" ++ show form ++ "'"
  show (NotAFunction funcname) = funcname ++ " is not a function"
  show (UnboundVariable msg varname) = "unbound variable '" ++ varname ++ "'"

type ThrowsLispError = Either LispError

trapLispError action = catchError action (return . show)

extractLispValue :: ThrowsLispError a -> a
extractLispValue (Right val) = val
