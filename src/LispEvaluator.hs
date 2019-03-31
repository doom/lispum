module LispEvaluator
  ( eval
  ) where

import           Control.Monad.Except
import qualified Data.Map.Strict      as MapStrict
import qualified Lisp

callLispFunction :: Lisp.Function -> [Lisp.Value] -> Lisp.ThrowsError Lisp.Value
callLispFunction f args =
  if Lisp.validateArguments (Lisp.argSpec f) (toInteger $ length args)
    then Lisp.call f $ args
    else throwError $ Lisp.ArgsNumberMismatch (Lisp.argSpec f) args

unwrapNumber :: Lisp.Value -> Lisp.ThrowsError Integer
unwrapNumber (Lisp.Number n) = return n
unwrapNumber v               = throwError $ Lisp.TypeMismatch "number" v

unwrapString :: Lisp.Value -> Lisp.ThrowsError String
unwrapString (Lisp.String s) = return s
unwrapString v               = throwError $ Lisp.TypeMismatch "string" v

unwrapBool :: Lisp.Value -> Lisp.ThrowsError Bool
unwrapBool (Lisp.Bool b) = return b
unwrapBool v             = throwError $ Lisp.TypeMismatch "bool" v

unwrapList :: Lisp.Value -> Lisp.ThrowsError [Lisp.Value]
unwrapList (Lisp.List l) = return l
unwrapList v             = throwError $ Lisp.TypeMismatch "list" v

numToNumBinop :: (Integer -> Integer -> Integer) -> Lisp.Function
numToNumBinop f =
  Lisp.Function
    {Lisp.argSpec = Lisp.AtLeast 2, Lisp.call = \args -> (mapM unwrapNumber args >>= return . Lisp.Number . foldl1 f)}

toBoolBinop :: (Lisp.Value -> Lisp.ThrowsError a) -> (a -> a -> Bool) -> Lisp.Function
toBoolBinop unwrap f =
  Lisp.Function
    { Lisp.argSpec = Lisp.Exactly 2
    , Lisp.call =
        \[a1, a2] -> do
          first <- unwrap a1
          second <- unwrap a2
          return $ Lisp.Bool $ f first second
    }

-- (mapM unwrap args >>= return . Lisp.Bool . foldl1 f)}
numToBoolBinop :: (Integer -> Integer -> Bool) -> Lisp.Function
numToBoolBinop = toBoolBinop unwrapNumber

stringToBoolBinop :: (String -> String -> Bool) -> Lisp.Function
stringToBoolBinop = toBoolBinop unwrapString

car :: [Lisp.Value] -> Lisp.ThrowsError Lisp.Value
car [Lisp.List (h:_)] = return h
car [x]               = throwError $ Lisp.TypeMismatch "list" x

carFunction :: Lisp.Function
carFunction = Lisp.Function {Lisp.argSpec = Lisp.Exactly 1, Lisp.call = car}

cdr :: [Lisp.Value] -> Lisp.ThrowsError Lisp.Value
cdr [Lisp.List (_:t)] = return $ Lisp.List t
cdr [x]               = throwError $ Lisp.TypeMismatch "list" x

cdrFunction :: Lisp.Function
cdrFunction = Lisp.Function {Lisp.argSpec = Lisp.Exactly 1, Lisp.call = cdr}

cons :: [Lisp.Value] -> Lisp.ThrowsError Lisp.Value
cons [x, Lisp.List l] = return $ Lisp.List $ x : l

consFunction :: Lisp.Function
consFunction = Lisp.Function {Lisp.argSpec = Lisp.Exactly 2, Lisp.call = cons}

builtinFunctions :: MapStrict.Map String Lisp.Function
builtinFunctions =
  MapStrict.fromList
    [ ("+", numToNumBinop (+))
    , ("-", numToNumBinop (-))
    , ("*", numToNumBinop (*))
    , ("/", numToNumBinop (div))
    , ("%", numToNumBinop (mod))
    , ("=", numToBoolBinop (==))
    , ("/=", numToBoolBinop (/=))
    , ("<", numToBoolBinop (<))
    , (">", numToBoolBinop (>))
    , ("<=", numToBoolBinop (<=))
    , (">=", numToBoolBinop (>=))
    , ("string=", stringToBoolBinop (==))
    , ("string/=", stringToBoolBinop (/=))
    , ("string<", stringToBoolBinop (<))
    , ("string>", stringToBoolBinop (>))
    , ("string<=", stringToBoolBinop (<=))
    , ("string>=", stringToBoolBinop (>=))
    , ("car", carFunction)
    , ("cdr", cdrFunction)
    , ("cons", consFunction)
    ]

eval :: Lisp.Value -> Lisp.ThrowsError Lisp.Value
eval n@(Lisp.Number _) = return n
eval s@(Lisp.String _) = return s
eval b@(Lisp.Bool _) = return b
eval (Lisp.List [Lisp.Atom "quote", value]) = return value
eval (Lisp.List [Lisp.Atom "if", pred, ifTrue, ifFalse]) = do
  result <- eval pred
  case result of
    (Lisp.Bool True) -> eval ifTrue
    otherwise        -> eval ifFalse
eval (Lisp.List (Lisp.Atom function:arguments)) =
  case MapStrict.lookup function builtinFunctions of
    Just lispfunc -> mapM eval arguments >>= callLispFunction lispfunc
    Nothing       -> throwError $ Lisp.NotAFunction function
eval bad = throwError $ Lisp.InvalidSpecialForm bad
