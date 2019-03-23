module LispEvaluator
  ( eval
  ) where

import           Control.Monad.Except
import qualified Data.Map.Strict      as MapStrict
import           Lisp

checkNumericArg :: LispValue -> ThrowsLispError LispValue
checkNumericArg n@(Number _) = return n
checkNumericArg v = throwError $ TypeMismatch "number" v

numtoNumBinop :: (Integer -> Integer -> Integer) -> [LispValue] -> ThrowsLispError LispValue
numtoNumBinop _ args@[] = throwError $ ArgsNumberMismatch 2 args
numtoNumBinop _ args@[_] = throwError $ ArgsNumberMismatch 2 args
numtoNumBinop f args = mapM checkNumericArg args >>= return . foldl1 (\(Number a) (Number b) -> Number $ f a b)

builtinFunctions :: MapStrict.Map String ([LispValue] -> ThrowsLispError LispValue)
builtinFunctions =
  MapStrict.fromList
    [ ("+", numtoNumBinop (+))
    , ("-", numtoNumBinop (-))
    , ("*", numtoNumBinop (*))
    , ("/", numtoNumBinop (div))
    , ("%", numtoNumBinop (mod))
    ]

eval :: LispValue -> ThrowsLispError LispValue
eval n@(Number _) = return n
eval s@(String _) = return s
eval b@(Bool _) = return b
eval (List [Atom "quote", value]) = return value
eval (List (Atom function:arguments)) =
  case MapStrict.lookup function builtinFunctions of
    Just binop -> mapM eval arguments >>= binop
    Nothing    -> throwError $ NotAFunction function
eval bad = throwError $ InvalidSpecialForm bad
