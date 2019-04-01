module Main where

import           Control.Monad.Except
import qualified Lisp
import qualified LispEvaluator
import qualified LispParser
import           Prompt
import           Synapto.Combinators
import           Synapto.Primitives

-- | Parse a LISP expression from a given string
readExpr :: String -> Lisp.ThrowsError Lisp.Value
readExpr input =
  let result = do
        expr <- LispParser.expr <??> "expected LISP expression"
        eof <?> "expected EOL after LISP expression"
        return expr
   in case parse result input of
        (Left errInfo, _) -> throwError $ Lisp.Invalid errInfo
        (Right res, _)    -> return res

-- | Evaluate a string representing a LISP expression to a string representing the result
evaluateToPrintable :: Lisp.Env -> String -> IO String
evaluateToPrintable env expr =
  Lisp.runIOThrows $ liftM show $ (Lisp.liftThrows $ readExpr expr) >>= LispEvaluator.eval env

processLine :: Lisp.Env -> String -> IO ()
processLine env expr = evaluateToPrintable env expr >>= putStrLn

repeatPrompt :: IO (Maybe String) -> (String -> IO ()) -> IO ()
repeatPrompt readStr f = do
  str <- readStr
  case str of
    (Just str) -> f str >> repeatPrompt readStr f
    Nothing    -> return ()

replLoop :: IO ()
replLoop = LispEvaluator.builtinEnv >>= repeatPrompt (prompt "> ") . processLine

main :: IO ()
main = replLoop
