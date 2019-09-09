module ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

import Parser
import Syntax

spec :: Spec
spec = do
  describe "parseLWord" $ do
    it "matches ambiguous leading binder characters in variable names" $ do
      parse (parseLWord (Lit "")) "" "xvar!??=2" `shouldParse` LVarDecl (Lit("xvar!?")) DefaultValueBinder
