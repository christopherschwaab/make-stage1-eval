module Main where

import Prelude hiding (putStrLn, readFile)

import Data.Text (pack)
import Data.Text.IO hiding (putStr)
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Options.Applicative

import Parser (makefile)
import Syntax

data Options = Options
  { optPrintRaw :: Bool
  , optFilename :: String
  }

optsParser :: Parser Options
optsParser = Options
  <$> switch
      ( long "raw"
     <> short 'r'
     <> help "Print the raw (show) parse tree without pretty-printing.")
  <*> argument str (metavar "FILE")

opts :: ParserInfo Options
opts = info (optsParser <**> helper)
  ( fullDesc
 <> progDesc "Parse a makefile"
 <> header "makeeval")

main :: IO ()
main = do
  args <- execParser opts
  let fname = optFilename args
  r <- parse makefile fname <$> readFile fname
  case r of
    Left err -> putStr (errorBundlePretty err)
    Right p -> TIO.putStr ((if optPrintRaw args then pack . show else prettyProgram) p)
