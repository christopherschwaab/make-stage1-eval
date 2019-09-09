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
      parse (parseLWord (Lit "")) "" "xvar!??=" `shouldParse` LVarDecl (Lit("xvar!?")) DefaultValueBinder
      parse (parseLWord (Lit "")) "" "y!var!?!=" `shouldParse` LVarDecl (Lit("y!var!?")) ShellBinder

    it "supports slashes and punctuation in variable names" $ do
      parse (parseLWord (Lit "")) "" "a/decl???=" `shouldParse` LVarDecl (Lit("a/decl??")) DefaultValueBinder
      parse (parseLWord (Lit "")) "" "x_x??=" `shouldParse` LVarDecl (Lit("x_x?")) DefaultValueBinder
      parse (parseLWord (Lit "")) "" "T/!/T!!!=" `shouldParse` LVarDecl (Lit("T/!/T!!")) ShellBinder
      parse (parseLWord (Lit "")) "" "?y!:=" `shouldParse` LVarDecl (Lit("?y!")) ImmediateBinder

    it "differentiates unambiguous rule and variable declarations" $ do
      parse (parseLWord (Lit "")) "" "xvar:=" `shouldParse` LVarDecl (Lit("xvar")) ImmediateBinder
      parse (parseLWord (Lit "")) "" "xrule:" `shouldParse` LRuleDecl (Lit("xrule"))
