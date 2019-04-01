module LispEvaluator
  ( eval
  , builtinEnv
  ) where

import           Control.Monad.Except
import           Data.IORef
import qualified Data.Map.Strict      as MapStrict
import           Lisp

isBoundVariable :: Env -> String -> IO Bool
isBoundVariable env name = do
  env <- readIORef env
  return . maybe False (const True) . MapStrict.lookup name $ env

getBoundVariable :: Env -> String -> IOThrowsError Value
getBoundVariable env name = do
  env <- liftIO $ readIORef env
  maybe (throwError $ UnboundVariable "" name) (liftIO . readIORef) . MapStrict.lookup name $ env

setVariable :: Env -> String -> Value -> IOThrowsError Value
setVariable env name val = do
  env <- liftIO $ readIORef env
  maybe (throwError $ UnboundVariable "" name) (liftIO . (flip writeIORef val)) . MapStrict.lookup name $ env
  return val

defineVariable :: Env -> String -> Value -> IOThrowsError Value
defineVariable env name val = do
  alreadyDefined <- liftIO $ isBoundVariable env name
  if alreadyDefined
    then setVariable env name val
    else liftIO $ do
           valueRef <- newIORef val
           envIO <- readIORef env
           writeIORef env $ MapStrict.insert name valueRef envIO
           return val

callUserDefinedFunction :: Value -> [Value] -> IOThrowsError Value
callUserDefinedFunction (Lisp.Function params body env) args =
  (liftIO $ defineInEnv env (MapStrict.fromList $ zip params args)) >>= evalBody
  where
    evalBody env = liftM last $ mapM (eval env) body

callLispFunction :: Value -> [Value] -> IOThrowsError Value
callLispFunction f args =
  if validateArguments f (toInteger $ length args)
    then call f args
    else throwError $ ArgsNumberMismatch (argSpec f) args
  where
    call (Lisp.BuiltinFunction _ doCall) args = liftThrows $ doCall args
    call f args                               = callUserDefinedFunction f args

unwrapNumber :: Value -> ThrowsError Integer
unwrapNumber (Number n) = return n
unwrapNumber v          = throwError $ TypeMismatch "number" v

unwrapString :: Value -> ThrowsError String
unwrapString (String s) = return s
unwrapString v          = throwError $ TypeMismatch "string" v

unwrapBool :: Value -> ThrowsError Bool
unwrapBool (Bool b) = return b
unwrapBool v        = throwError $ TypeMismatch "bool" v

unwrapList :: Value -> ThrowsError [Value]
unwrapList (List l) = return l
unwrapList v        = throwError $ TypeMismatch "list" v

numToNumBinop :: (Integer -> Integer -> Integer) -> Value
numToNumBinop f =
  BuiltinFunction {argSpec = AtLeast 2, call = \args -> (mapM unwrapNumber args >>= return . Number . foldl1 f)}

toBoolBinop :: (Value -> ThrowsError a) -> (a -> a -> Bool) -> Value
toBoolBinop unwrap f =
  BuiltinFunction
    { argSpec = Exactly 2
    , call =
        \[a1, a2] -> do
          first <- unwrap a1
          second <- unwrap a2
          return $ Bool $ f first second
    }

numToBoolBinop :: (Integer -> Integer -> Bool) -> Value
numToBoolBinop = toBoolBinop unwrapNumber

stringToBoolBinop :: (String -> String -> Bool) -> Value
stringToBoolBinop = toBoolBinop unwrapString

car :: [Value] -> ThrowsError Value
car [List (h:_)] = return h
car [x]          = throwError $ TypeMismatch "list" x

carFunction :: Value
carFunction = BuiltinFunction {argSpec = Exactly 1, call = car}

cdr :: [Value] -> ThrowsError Value
cdr [List (_:t)] = return $ List t
cdr [x]          = throwError $ TypeMismatch "list" x

cdrFunction :: Value
cdrFunction = BuiltinFunction {argSpec = Exactly 1, call = cdr}

cons :: [Value] -> ThrowsError Value
cons [x, List l] = return $ List $ x : l

consFunction :: Value
consFunction = BuiltinFunction {argSpec = Exactly 2, call = cons}

builtinFunctions :: MapStrict.Map String Value
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

defineInEnv :: Env -> MapStrict.Map String Value -> IO Env
defineInEnv env builtins = readIORef env >>= (addToEnv builtins) >>= newIORef
  where
    addToEnv builtins env = liftM (MapStrict.union env) (mapM makeDefinition builtins)
    makeDefinition (val) = do
      newVal <- newIORef val
      return newVal

builtinEnv :: IO Env
builtinEnv = emptyEnv >>= flip defineInEnv builtinFunctions

defineFunction env params body = return $ Lisp.Function (map show params) body env

eval :: Env -> Value -> IOThrowsError Value
eval _ n@(Number _) = return n
eval _ s@(String _) = return s
eval _ b@(Bool _) = return b
eval _ (List [Atom "quote", value]) = return value
eval env (Atom varName) = getBoundVariable env varName
eval env (List [Atom "defvar", Atom varName, value]) = eval env value >>= defineVariable env varName
eval env (List [Atom "set", Atom varName, value]) = eval env value >>= setVariable env varName
eval env (List (Atom "defun":Atom fname:List params:body)) = defineFunction env params body >>= defineVariable env fname
eval env (List (Atom "lambda":List params:body)) = defineFunction env params body
eval env (List [Atom "if", pred, ifTrue, ifFalse]) = do
  result <- eval env pred
  case result of
    (Bool True) -> eval env ifTrue
    otherwise   -> eval env ifFalse
eval env (List (function:arguments)) = do
  func <- eval env function
  args <- mapM (eval env) arguments
  callLispFunction func args
eval _ bad = throwError $ InvalidSpecialForm bad
