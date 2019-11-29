module Main where

import Prelude hiding (putStrLn, readFile)

import Data.Text (pack)
import Data.Text.IO hiding (putStr)
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import System.Console.Docopt
import System.Environment (getArgs)

import Parser
import Syntax

usagePat :: Docopt
usagePat = [docopt|
makeeval

Usage:
  makeeval [--raw] <file>

Options:
  -r, --raw    Print the raw (show) parse tree without pretty-printing.
|]

main :: IO ()
main = do
  args <- parseArgsOrExit usagePat =<< getArgs
  fname <- getArgOrExitWith usagePat args (argument "file")
  r <- parse makefile fname <$> readFile fname
  case r of
    Left err -> putStr (errorBundlePretty err)
    Right p -> TIO.putStr ((if args `isPresent` longOption "raw" then pack . show else prettyProgram) p)
