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

    it [r|handles x := xval\\\$\\\#2|] $ do
      let weirdVar = [r|xval\\\$\\\#2|]
      parse rexp "" weirdVar  `shouldParse` (Lit "xval\\\\" `Cat` Var (Lit "\\") `Cat` Lit "#2")

    it "allows : literals outside varsubst" $ do
      parse rexp "" "dir/path/:$(fname)" `shouldParse` (Lit "dir/path/:" `Cat` Var (Lit "fname"))
      parse rexp "" "$(foreach fname,$(FNAMES),dir/path/:$(fname))" `shouldParse`
        (Builtin (PApp (App Foreach (Lit "fname",Var (Lit "FNAMES"),Lit "dir/path/:" `Cat` Var (Lit "fname")))))

    it "handles escaped parentheses" $ do
      parse rexp "" "$(shell find -regex '.*\\.\\(cpp\\|qrc\\)' )" `shouldParse`
        (Builtin (PApp (App Shell (Lit "find -regex '.*\\.\\(cpp\\|qrc\\)' "))))

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
      parse parseLWord "" "xrule:" `shouldParse` LRuleDecl [Lit "xrule"]

    it "supports unusual characters in rule names" $ do
      parse parseLWord "" "!rule:" `shouldParse` LRuleDecl [Lit "!rule"]
      parse parseLWord "" "a?!rule.:" `shouldParse` LRuleDecl [Lit "a?!rule."]

    it "supports backslashes in names" $ do
      parse parseLWord "" "\\xvar:=" `shouldParse` LVarDecl (Lit "\\xvar") ImmediateBinder
      parse parseLWord "" "a\\decl?=" `shouldParse` LVarDecl (Lit "a\\decl") DefaultValueBinder
      parse parseLWord "" "xrule\\:" `shouldParse` LRuleDecl [Lit "xrule\\"]

    it "consumes trailing spaces" $ do
      parse parseLWord "" "xrule\\  :" `shouldParse` LRuleDecl [Lit "xrule\\"]
      parse parseLWord "" "xvar\\  :=" `shouldParse` LVarDecl (Lit "xvar\\") ImmediateBinder

    it "stops at =" $ do
      parse parseLWord "" "xvar=" `shouldParse` LVarDecl (Lit "xvar") DeferredBinder

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
      parse parseTopLevel "" weirdVar `shouldParse` Stmt (Bind False ImmediateBinder (Lit "x") (Lit "xval\\\\" `Cat` Var (Lit "\\") `Cat` Lit "#2"))

    it "parses variable-named variable-binding" $ do
      parse parseTopLevel "" "$(backslash) = backslash_value" `shouldParse` Stmt (Bind False DeferredBinder (Var (Lit "backslash")) (Lit "backslash_value"))

    it "supportes standalone expressions" $ do
      parse parseTopLevel "" "$(if $(filter-out none,$(PRODUCT)),,$(error Unable to determine the target product))" `shouldParse`
        (Stmt (SExp (Builtin
                     (PApp (App If
                            (Builtin (PApp (App FilterOut
                                            (Lit "none",Var (Lit "PRODUCT"))))
                            ,Lit ""
                            ,Just (Builtin (PApp (App Error
                                                  (Lit "Unable to determine the target product"))))))))))

      parse parseTopLevel "" [r|$(error Empty "$(EMPTY)" isn't empty.)|] `shouldParse`
        (Stmt (SExp (Builtin (PApp
                              (App Error
                               (Lit "Empty \"" `Cat` (Var (Lit "EMPTY") `Cat` Lit "\" isn't empty.")))))))

      let ifNeqMultiLine = [r|ifneq (x x,x y)
	x = ok line  1  
	x += appending stuff to x 
endif
|]
      parse ifStmt "" ifNeqMultiLine `shouldParse` IfStmt (NeqPred (Lit "x x") (Lit "x y"))
                                                          [Stmt (Bind False DeferredBinder (Lit "x") (Lit "ok line  1  "))
                                                          ,Stmt (Bind False AppendBinder (Lit "x") (Lit "appending stuff to x "))]
                                                          []

    it "parses else blocks" $ do
      let ifEq11 = [r|ifeq (1,1)
	eq11 = true
else
	eq11 = false
endif
|]
      parse ifStmt "" ifEq11 `shouldParse` IfStmt (EqPred (Lit "1") (Lit "1"))
                                                  [Stmt (Bind False DeferredBinder (Lit "eq11") (Lit "true"))]
                                                  [Stmt (Bind False DeferredBinder (Lit "eq11") (Lit "false"))]

    it "accepts expressions in test fields" $ do
      let ifEqMultiLine = [r|ifneq (x$(ok),xok_value)
	x$(ok) = ok line  $1  
	x$(ok) += append $(x$(stuff))
endif
|]
      parse ifStmt "" ifEqMultiLine `shouldParse`
        IfStmt (NeqPred (Lit "x" `Cat` Var (Lit "ok"))
                        (Lit "xok_value"))
               [Stmt (Bind False DeferredBinder (Lit "x" `Cat` Var (Lit "ok")) (Lit "ok line  " `Cat` Var (Lit "1") `Cat` Lit "  "))
               ,Stmt (Bind False AppendBinder (Lit "x" `Cat` Var (Lit "ok")) (Lit "append " `Cat` Var (Lit "x" `Cat` Var (Lit "stuff"))))]
               []

    it "accepts empty test fields" $ do
      let emptyField1 = [r|ifneq (,xok_value)
	42 = 43
endif
|]
      let emptyField2 = [r|ifeq ($x,)
	42 = 43
endif
|]
      let bothEmpty = [r|ifeq (,)
	42 = 43
endif
|]
      parse ifStmt "" emptyField1 `shouldParse`
        IfStmt (NeqPred (Lit "") (Lit "xok_value"))
               [Stmt (Bind False DeferredBinder (Lit "42") (Lit "43"))]
               []

      parse ifStmt "" emptyField2 `shouldParse`
        IfStmt (EqPred (Var (Lit "x")) (Lit ""))
               [Stmt (Bind False DeferredBinder (Lit "42") (Lit "43"))]
               []

      parse ifStmt "" bothEmpty `shouldParse`
        IfStmt (EqPred (Lit "") (Lit ""))
               [Stmt (Bind False DeferredBinder (Lit "42") (Lit "43"))]
               []

    it "matches binding of else variable in if block" $ do
      let bindElseInIf = [r|ifeq (1,1)
	else = this is terrible
endif
|]
      parse ifStmt "" bindElseInIf `shouldParse`
        IfStmt (EqPred (Lit "1") (Lit "1"))
               [Stmt (Bind False DeferredBinder (Lit "else") (Lit "this is terrible"))]
               []

      let bindElseInIfWithElse = [r|ifeq (1,1)
	else = this is terrible
else
	else = this is still terrible
endif
|]
      parse ifStmt "" bindElseInIfWithElse `shouldParse`
        IfStmt (EqPred (Lit "1") (Lit "1"))
               [Stmt (Bind False DeferredBinder (Lit "else") (Lit "this is terrible"))]
               [Stmt (Bind False DeferredBinder (Lit "else") (Lit "this is still terrible"))]

    it "requires the if block be closed" $ do
      let openIf = [r|ifeq (1,1)
	x = unclosed if block
|]
      let openElse = [r|ifeq (1,1)
	x = unclosed if block
else
	y = unclosed if block
|]
      parse ifStmt "" `shouldFailOn` openIf
      parse ifStmt "" `shouldFailOn` openElse

    it "accepts standalone expressions" $ do
     let nakedError = [r|ifneq ($(EMPTY),)
      $(error Empty "$(EMPTY)" isn't empty.)
endif
|]
     parse ifStmt "" nakedError `shouldParse`
       IfStmt (NeqPred (Var (Lit "EMPTY")) (Lit ""))
              [Stmt (SExp (Builtin (PApp
                                    (App Error (Lit "Empty \"" `Cat` (Var (Lit "EMPTY") `Cat` Lit "\" isn't empty."))))))]
              []

    it "allows indented else and endif" $ do
     let indentedEndIf = [r|ifneq (42,6*7)
      x = 42
    endif
|]
     parse ifStmt "" indentedEndIf `shouldParse`
       IfStmt (NeqPred (Lit "42") (Lit "6*7"))
              [Stmt (Bind False DeferredBinder (Lit "x") (Lit "42"))]
              []

     let indentedElse = [r|ifneq (42,6*7)
      x = 42
    else
      x = 6*7
endif
|]
     parse ifStmt "" indentedElse `shouldParse`
       IfStmt (NeqPred (Lit "42") (Lit "6*7"))
              [Stmt (Bind False DeferredBinder (Lit "x") (Lit "42"))]
              [Stmt (Bind False DeferredBinder (Lit "x") (Lit "6*7"))]

     let indentedBoth = [r|ifeq (iec61508,b26262)
         so = oh
    else
       oh = so
  endif
|]
     parse ifStmt "" indentedBoth `shouldParse`
       IfStmt (EqPred (Lit "iec61508") (Lit "b26262"))
              [Stmt (Bind False DeferredBinder (Lit "so") (Lit "oh"))]
              [Stmt (Bind False DeferredBinder (Lit "oh") (Lit "so"))]

    it "accepts nested ifs" $ do
     let nestedIf = [r|ifneq (42,6*7)
      ifeq (6*7,42)
       $(info huh)
    else
       $(info oh)
       # dang
  endif
    endif
|]
     parse ifStmt "" nestedIf `shouldParse`
       IfStmt (NeqPred (Lit "42") (Lit "6*7"))
              [Stmt (IfStmt (EqPred (Lit "6*7") (Lit "42"))
                     [Stmt (SExp (Builtin (PApp (App Info (Lit "huh")))))]
                     [Stmt (SExp (Builtin (PApp (App Info (Lit "oh")))))])]
              []

    it "accepts rules" $ do
     let ruleIf = [r|ifneq ($(SRCDIR),src)
      all:
	cd src && make -B
   else
 all:
	cd not-src; make -B

  endif
    endif
|]
     parse ifStmt "" ruleIf `shouldParse`
       IfStmt (NeqPred (Var (Lit "SRCDIR")) (Lit "src"))
              [RuleDecl (Rule [Lit "all"]
                              Nothing
                              (Recipe [Lit "cd src && make -B"]))]
              [RuleDecl (Rule [Lit "all"]
                              Nothing
                              (Recipe [Lit "cd not-src; make -B"]))]


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

      parse makefile "" basic `shouldParse` [RuleDecl (Rule [Lit "all"] Nothing (Recipe [Lit "echo all target called"]))]
      parse makefile "" multiTarget `shouldParse` [RuleDecl (Rule [Lit "all"] Nothing (Recipe [Lit "echo all target called"]))
                                                  ,RuleDecl (Rule [Lit "test"] Nothing (Recipe [Lit "echo doing test target"]))]
      parse makefile "" deps `shouldParse` [RuleDecl (Rule [Lit "all"] (Just (Lit " t1 t2")) (Recipe [Lit "echo doing all with deps t1 and t2"]))]

    it "accepts multiple binders" $ do
      let twoBinds = [r|x0=true
      x1 += more_x1_value
|]
      parse makefile "" twoBinds `shouldParse` [Stmt (Bind False DeferredBinder (Lit "x0") (Lit "true"))
                                               ,Stmt (Bind False AppendBinder (Lit "x1") (Lit "more_x1_value"))]
