module Main where

import           Control.Monad.Except
import qualified Lisp
import qualified LispEvaluator
import qualified LispParser
import           Prompt
import           Synapto.Combinators
import           Synapto.Primitives

readExpr :: String -> Lisp.ThrowsError Lisp.Value
readExpr input =
  let result = do
        expr <- LispParser.expr <??> "expected LISP expression"
        eof <?> "expected EOL after LISP expression"
        return expr
   in case parse result input of
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
          putStrLn $ Lisp.extractValue $ Lisp.trapError $ liftM show $ readExpr input
          loop

main :: IO ()
main = replLoop
