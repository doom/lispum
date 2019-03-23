module Prompt where

import           System.IO

prompt :: String -> IO (Maybe String)
prompt s = do
  putStr s
  hFlush stdout
  end <- isEOF
  if end
    then do
      putStrLn ""
      return Nothing
    else do
      line <- getLine
      return (Just line)
