module Main where

import           Control.Monad.Except
import qualified Lisp
import qualified LispEvaluator
import qualified LispParser
import           Prompt
import           Synapto.Combinators
import           Synapto.Primitives
import           System.Environment

-- | Evaluate a string representing a LISP expression to a string representing the result
evaluateToPrintable :: Lisp.Env -> String -> IO String
evaluateToPrintable env expr =
  Lisp.runIOThrows $ liftM show $ (Lisp.liftThrows $ LispParser.readOneExpr expr) >>= LispEvaluator.eval env

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
main = do
  args <- getArgs
  if length args > 0
    then LispEvaluator.runProgram (args !! 0)
    else replLoop
