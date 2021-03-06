module Main where

import qualified Language.Fim.Eval as Eval
import Language.Fim (runIO)

import Prelude hiding (readFile)
import Data.Text.IO (readFile)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName)

errorMsg :: String -> IO ()
errorMsg s = do
  hPutStrLn stderr s
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      progName <- getProgName
      errorMsg $ "usage: " ++ progName ++ " <input file>"
    (sourceFile:_) -> do
      file <- readFile sourceFile
      err <- runIO file
      case err of
        Just errMsg -> errorMsg errMsg
        Nothing -> return ()
