module ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.RawString.QQ

import Parser
import Syntax

import Gen

simpleRecipe = [r|	echo calling all|]
emptyLinesRecipe = [r|
	

#
	    
#####
	
|]
mostlyEmptyLinesRecipe = [r|
	

#
	    
	echo single command between empty lines
#####
	
|]

spec :: Spec
spec = do
  describe "parseLWord" $ do
    it "matches ambiguous leading binder characters in variable names" $ do
      parse parseLWord "" "xvar!??=" `shouldParse` LVarDecl (Lit "xvar!?") DefaultValueBinder
      parse parseLWord "" "y!var!?!=" `shouldParse` LVarDecl (Lit "y!var!?") ShellBinder

    it "supports slashes and punctuation in variable names" $ do
      parse parseLWord "" "a/decl???=" `shouldParse` LVarDecl (Lit "a/decl??") DefaultValueBinder
      parse parseLWord "" "x_x??=" `shouldParse` LVarDecl (Lit "x_x?") DefaultValueBinder
      parse parseLWord "" "T/!/T!!!=" `shouldParse` LVarDecl (Lit "T/!/T!!") ShellBinder
      parse parseLWord "" "?y!:=" `shouldParse` LVarDecl (Lit "?y!") ImmediateBinder

    it "differentiates unambiguous rule and variable declarations" $ do
      parse parseLWord "" "xvar:=" `shouldParse` LVarDecl (Lit "xvar") ImmediateBinder
      parse parseLWord "" "xrule:" `shouldParse` LRuleDecl (Lit "xrule")

    it "supports unusual characters in rule names" $ do
      parse parseLWord "" "!rule:" `shouldParse` LRuleDecl (Lit "!rule")
      parse parseLWord "" "a?!rule.:" `shouldParse` LRuleDecl (Lit "a?!rule.")

    it "supports backslashes in names" $ do
      parse parseLWord "" "\\xvar:=" `shouldParse` LVarDecl (Lit "\\xvar") ImmediateBinder
      parse parseLWord "" "a\\decl?=" `shouldParse` LVarDecl (Lit "a\\decl") DefaultValueBinder
      parse parseLWord "" "xrule\\:" `shouldParse` LRuleDecl (Lit "xrule\\")

    it "stops at spaces" $ do
      parse parseLWord "" "xrule\\  :" `shouldParse` LRuleOrVarDecl (Lit "xrule\\")
      parse parseLWord "" "xvar\\  :=" `shouldParse` LRuleOrVarDecl (Lit "xvar\\")

  describe "parseRecipe" $ do
    it "matches simple rules" $ do
      parse parseRecipe "" simpleRecipe `shouldParse` Recipe [Lit "echo calling all"]

    it "accepts an empty rule of whitespace" $ do
      runParser' parseRecipe (initialState emptyLinesRecipe) `succeedsLeaving` ""

    it "accepts a non-empty line between empty lines" $ do
      runParser' parseRecipe (initialState mostlyEmptyLinesRecipe ) `succeedsLeaving` ""
      parse parseRecipe "" mostlyEmptyLinesRecipe `shouldParse` Recipe [Lit "echo single command between empty lines"]
