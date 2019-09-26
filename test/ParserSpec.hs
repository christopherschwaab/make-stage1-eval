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

spec :: Spec
spec = do
  describe "litExp" $ do
    it "matches non-escaping backslashes" $ do
      parse (litExp " " []) "" "\\x" `shouldParse` "\\x"

  describe "rexp" $ do
    it "matches literals" $ do
      parse rexp "" "some literal \\string" `shouldParse` Lit "some literal \\string"

    it "matches variables" $ do
      parse rexp "" "$(x0)" `shouldParse` Var (Lit "x0")
      parse rexp "" "$($(xx0))" `shouldParse` Var (Var (Lit "xx0"))
      parse rexp "" "$($($(xx0)))" `shouldParse` Var (Var (Var (Lit "xx0")))

    it "sequences literals and non-literals" $ do
      parse rexp "" "app$(ly) sequence" `shouldParse` (Lit "app" `Cat` Var (Lit "ly") `Cat` Lit " sequence")

    it "doesn't match builtins occuring in literals" $ do
      parse rexp "" "foreach" `shouldParse` Lit "foreach"

    it "matches builtins" $ do
      parse rexp "" "$(foreach 1,2,3)" `shouldParse` Builtin (PApp (App Foreach (Lit "1", Lit "2", Lit "3")))
      parse rexp "" "$(filter apple, banana)" `shouldParse` Builtin (PApp (App Filter (Lit "apple", Lit " banana")))
      parse rexp "" "$(wildcard *.cpp)" `shouldParse` Builtin (PApp (App Wildcard (Lit "*.cpp")))

    it "uses case sensitive builtin names" $ do
      parse rexp "" "$(Wildcard *.cpp)" `shouldParse` Var (Lit "Wildcard *.cpp")

    it "matches subst shorthand notation" $ do
      parse rexp "" "$(SRCS:.cpp=.o)" `shouldParse` Varsubst (Lit "SRCS") (Lit ".cpp") (Lit ".o")

    it "matches escape sequences" $ do
      let lineCont = [r|a line with a\
continued next line\
and more|]
      let lineContEnd = [r|a line with a continued, empty next line\
|]
      parse rexp "" lineCont `shouldParse` Lit "a line with a continued next line and more"
      parse rexp "" lineContEnd `shouldParse` Lit "a line with a continued, empty next line "
      parse rexp "" "$$" `shouldParse` Lit "$"

    --FIXME
    --it [r|handles x := xval\\\$\\\#2|] $ do
    --  let weirdVar = [r|xval\\\$\\\#2|]
    --  parse rexp "" weirdVar  `shouldParse` (Lit "xval\\\\")

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

    --it [r|handles x := xval\\\$\\\#2|] $ do
    --  let weirdVar = [r|x := xval\\\$\\\#2|]
    --  parse parseLWord "" weirdVar  `shouldParse` LRuleOrVarDecl (Lit "x")
    --  runParser' parseLWord (initialState weirdVar) `succeedsLeaving` [r|:= xval\\\$\\\#2|]

  describe "parseRecipe" $ do
    let simpleRecipe1 = [r|	echo calling all 1|]
    let simpleRecipe2 = [r|	echo calling all 2
	sed -i Makefile 's/cat/bat/'
	echo "calling all 2"|]
    let lineCont1 = [r|	echo calling all 3 \
with a newline in it|]
    let emptyLinesRecipe = [r|
	

#
	    
#####
	
|]
    let mostlyEmptyLinesRecipe = [r|
	
   

#
	    
	echo single command between empty lines
#####
	
|]

    it "matches simple rules" $ do
      parse parseRecipe "" simpleRecipe1 `shouldParse` Recipe [Lit "echo calling all 1"]
      parse parseRecipe "" simpleRecipe2 `shouldParse` Recipe [Lit "echo calling all 2",
                                                               Lit "sed -i Makefile 's/cat/bat/'",
                                                               Lit "echo \"calling all 2\""]

    it "replaces line continuations with newlines" $ do
      parse parseRecipe "" lineCont1 `shouldParse` Recipe [Lit "echo calling all 3 \nwith a newline in it"]

    it "accepts an empty rule of whitespace" $ do
      runParser' parseRecipe (initialState emptyLinesRecipe) `succeedsLeaving` ""

    it "accepts a non-empty line between empty lines" $ do
      runParser' parseRecipe (initialState mostlyEmptyLinesRecipe ) `succeedsLeaving` ""
      parse parseRecipe "" mostlyEmptyLinesRecipe `shouldParse` Recipe [Lit "echo single command between empty lines"]

  describe "parseTopLevel" $ do
    it [r|handles x := xval\\\$\\\#2|] $ do
      let weirdVar = [r|x := xval\\\$\\\#2|]
      parse parseTopLevel "" weirdVar `shouldParse` Stmt (Bind ImmediateBinder (Lit "x") (Lit "xval\\\\" `Cat` Var (Lit "\\") `Cat` Lit "#2"))

    it "parses variable-named variable-binding" $ do
      parse parseTopLevel "" "$(backslash) = backslash_value" `shouldParse` Stmt (Bind DeferredBinder (Var (Lit "backslash")) (Lit "backslash_value"))

  describe "makefile" $ do
    it "parses basic makefiles" $ do
      let basic = [r|all:
	echo all target called
|]
      let multiTarget = [r|all:
	echo all target called

test:
	echo doing test target
|]
      let deps = [r|all: t1 t2
	echo doing all with deps t1 and t2
|]

      parse makefile "" basic `shouldParse` [RuleDecl (Rule (Lit "all") Nothing (Recipe [Lit "echo all target called"]))]
      parse makefile "" multiTarget `shouldParse` [RuleDecl (Rule (Lit "all") Nothing (Recipe [Lit "echo all target called"]))
                                                  ,RuleDecl (Rule (Lit "test") Nothing (Recipe [Lit "echo doing test target"]))]
      parse makefile "" deps `shouldParse` [RuleDecl (Rule (Lit "all") (Just (Lit " t1 t2")) (Recipe [Lit "echo doing all with deps t1 and t2"]))]
