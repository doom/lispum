module LispEvaluator
  ( eval
  , builtinEnv
  , runProgram
  ) where

import           Control.Monad.Except
import           Data.IORef
import qualified Data.Map.Strict      as MapStrict
import           Lisp
import qualified LispParser

-- | Check whether a given environment binds a given name to a variable
isBoundVariable :: Env -> String -> IO Bool
isBoundVariable env name = do
  env <- readIORef env
  return . maybe False (const True) . MapStrict.lookup name $ env

-- | Get a variable with a given name from a given environment
getBoundVariable :: Env -> String -> IOThrowsError Value
getBoundVariable env name = do
  env <- liftIO $ readIORef env
  maybe (throwError $ UnboundVariable name) (liftIO . readIORef) . MapStrict.lookup name $ env

-- | Set the value of an existing variable in a given environment
setVariable :: Env -> String -> Value -> IOThrowsError Value
setVariable env name val = do
  env <- liftIO $ readIORef env
  maybe (throwError $ UnboundVariable name) (liftIO . (flip writeIORef val)) . MapStrict.lookup name $ env
  return val

-- | Define a variable with a given name/value pair in a given environment
-- | If the variable is already defined, its value is set instead
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

-- | Call the given user-defined function with the given arguments
callUserDefinedFunction :: Value -> [Value] -> IOThrowsError Value
callUserDefinedFunction (Lisp.Function params body env) args =
  (liftIO $ defineInEnv env (MapStrict.fromList $ zip params args)) >>= evalBody
  where
    evalBody env = liftM last $ mapM (eval env) body

-- | Call the given LISP function with the given arguments
callLispFunction :: Value -> [Value] -> IOThrowsError Value
callLispFunction f args =
  if validateArguments f (toInteger $ length args)
    then call f args
    else throwError $ ArgsNumberMismatch (argSpec f) args
  where
    call (Lisp.BuiltinFunction _ doCall) args = liftThrows $ doCall args
    call f args                               = callUserDefinedFunction f args

-- | Unwrap a number from a LISP value, if possible
unwrapNumber :: Value -> ThrowsError Integer
unwrapNumber (Number n) = return n
unwrapNumber v          = throwError $ TypeMismatch "number" v

-- | Unwrap a string from a LISP value, if possible
unwrapString :: Value -> ThrowsError String
unwrapString (String s) = return s
unwrapString v          = throwError $ TypeMismatch "string" v

-- | Unwrap a boolean from a LISP value, if possible
unwrapBool :: Value -> ThrowsError Bool
unwrapBool (Bool b) = return b
unwrapBool v        = throwError $ TypeMismatch "bool" v

-- | Unwrap a list from a LISP value, if possible
unwrapList :: Value -> ThrowsError [Value]
unwrapList (List l) = return l
unwrapList v        = throwError $ TypeMismatch "list" v

-- | Lift a given binary function operating on integers to a LISP function operating on LISP values
numToNumBinop :: (Integer -> Integer -> Integer) -> Value
numToNumBinop f =
  BuiltinFunction {argSpec = AtLeast 2, call = \args -> (mapM unwrapNumber args >>= return . Number . foldl1 f)}

-- | Given a function unwrapping LISP values, lift a binary predicate to a LISP function operating on LISP values
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

-- | Given a predicate operating on numbers, lift it to operate on LISP values
numToBoolBinop :: (Integer -> Integer -> Bool) -> Value
numToBoolBinop = toBoolBinop unwrapNumber

-- | Given a predicate operating on strings, lift it to operate on LISP values
stringToBoolBinop :: (String -> String -> Bool) -> Value
stringToBoolBinop = toBoolBinop unwrapString

-- | Builtin for the CAR function
carFunction :: Value
carFunction =
  BuiltinFunction
    { argSpec = Exactly 1
    , call =
        \[args] -> do
          list <- unwrapList args
          if length list > 0
            then return $ head list
            else return $ Bool False
    }

-- | Builtin for the CDR function
cdrFunction :: Value
cdrFunction =
  BuiltinFunction
    { argSpec = Exactly 1
    , call =
        \[args] -> do
          list <- unwrapList args
          if length list > 0
            then return $ List $ tail list
            else return $ Bool False
    }

-- | Builtin for the CONS function
consFunction :: Value
consFunction =
  BuiltinFunction
    { argSpec = Exactly 2
    , call =
        \[x, l] -> do
          list <- unwrapList l
          return $ List $ x : list
    }

-- | Builtin bindings for the builtin environment
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

-- | Import the given bindings into the given environments
defineInEnv :: Env -> MapStrict.Map String Value -> IO Env
defineInEnv env builtins = readIORef env >>= (addToEnv builtins) >>= newIORef
  where
    addToEnv builtins env = liftM (MapStrict.union env) (mapM makeDefinition builtins)
    makeDefinition (val) = do
      newVal <- newIORef val
      return newVal

-- | Builtin environment, populated with the builtin functions defined above
builtinEnv :: IO Env
builtinEnv = emptyEnv >>= flip defineInEnv builtinFunctions

-- | Define a LISP function inside a given environment, given a list of parameters names and a list of expressions
defineFunction :: Env -> [Value] -> [Value] -> IOThrowsError Value
defineFunction env params body = return $ Lisp.Function (map show params) body env

-- | Read all LISP expressions from a given file
readFileExpressions :: String -> IOThrowsError [Value]
readFileExpressions fileName = (liftIO $ readFile fileName) >>= liftThrows . LispParser.readMultipleExpr

-- | Evaluate the expressions of a file into the given environment
evalFile :: Env -> String -> IOThrowsError Value
evalFile env fileName = readFileExpressions fileName >>= liftM last . mapM (eval env)

-- | Evaluate a LISP expression
eval :: Env -> Value -> IOThrowsError Value
eval _ n@(Number _) = return n
eval _ s@(String _) = return s
eval _ b@(Bool _) = return b
eval _ (List [Atom "quote", value]) = return value
eval env (Atom varName) = getBoundVariable env varName
eval env (List [Atom "load", String fileName]) = evalFile env fileName
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

-- | Run the LISP program stored in the given filename
runProgram :: String -> IO ()
runProgram fileName = do
  env <- builtinEnv
  result <- runIOThrows $ liftM show $ evalFile env fileName
  putStrLn result
