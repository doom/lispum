module Lisp
  ( Value(..)
  , Env
  , Error(..)
  , FunctionArgumentSpec(..)
  , IOThrowsError
  , ThrowsError
  , emptyEnv
  , extractValue
  , liftThrows
  , runIOThrows
  , trapError
  , validateArguments
  ) where

import           Control.Monad.Except
import           Data.IORef
import           Data.List
import qualified Data.Map.Strict      as MapStrict

data Value
  = Atom String
  | Number Integer
  | String String
  | Bool Bool
  | List [Value]
  | BuiltinFunction { argSpec :: FunctionArgumentSpec
                    , call    :: ([Value] -> ThrowsError Value) }
  | Function { params :: [String]
             , body   :: [Value]
             , env    :: Env }

instance Show Value where
  show (Atom a) = a
  show (Number i) = (show i)
  show (String s) = "\"" ++ s ++ "\""
  show (Bool b) = (show b)
  show (List l) = "(" ++ (unwords . map show) l ++ ")"
  show (BuiltinFunction _ _) = "<builtin function>"
  show (Function params _ _) = "(fun (" ++ (intercalate " " . map show) params ++ "))"

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

validateArguments :: Lisp.Value -> Integer -> Bool
validateArguments (BuiltinFunction argSpec _) = check argSpec
  where
    check (Exactly n) = (== n)
    check (AtLeast n) = (>= n)
    check (AtMost n)  = (<= n)
validateArguments (Function params _ _) = (== (toInteger $ length params))

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

type Env = IORef (MapStrict.Map String (IORef Lisp.Value))

emptyEnv :: IO Env
emptyEnv = newIORef MapStrict.empty

type IOThrowsError = ExceptT Lisp.Error IO

liftThrows :: Lisp.ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (Lisp.trapError action) >>= return . Lisp.extractValue
