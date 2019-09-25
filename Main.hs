module Main where

import Prelude hiding (putStrLn, readFile)

import Data.Text.IO hiding (putStr)
import Text.Megaparsec
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import Parser
import Syntax

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do putStrLn "Usage: makeeval Makefile"
            exitWith (ExitFailure 1)
    else do let fname = head args
            r <- parse makefile fname <$> readFile fname
            case r of
              Left err -> putStr (errorBundlePretty err)
              Right x -> print x
