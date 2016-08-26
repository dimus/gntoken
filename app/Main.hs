module Main where

import qualified Token as T
import Control.Exception(catch)
import System.Environment(getProgName, getArgs)
import System.IO.Error(isUserError)

main :: IO ()
main = do
  (file_path:_) <- getArgs
  text <- readFile file_path
  let tokens = T.tokenize T.zeroToken text
  putStrLn ((unwords . map (T.verbatim)) $ tokens)
