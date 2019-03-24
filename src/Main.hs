module Main where

import Control.Monad.Except
import qualified Lisp
import qualified LispEvaluator
import qualified LispParser
import           Synapto.Combinators
import           Synapto.Primitives
import           Prompt

readExpr :: String -> Lisp.ThrowsLispError Lisp.LispValue
readExpr input =
  case parse LispParser.expr input of
    (Left errInfo, _) -> throwError $ Lisp.Invalid errInfo
    (Right res, _)    -> LispEvaluator.eval res

replLoop :: IO ()
replLoop = loop
  where
    loop = do
      maybeInput <- prompt "> "
      case maybeInput of
        Nothing -> return ()
        Just input -> do
          putStrLn $ Lisp.extractLispValue $ Lisp.trapLispError $ liftM show $ readExpr input
          loop

main :: IO ()
main = replLoop
