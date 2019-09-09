module Syntax where

import Data.Map (Map)
import Data.Text (Text)

type Name = Text

data Exp
  = Foreach Exp Exp Exp -- Collapses whitespace
  | Filter Exp Exp -- collapses whitespace?
  | FilterOut Exp Exp -- collapses whitespace?
  | If Exp Exp Exp -- collapses whitespace?
  | And [Exp] -- collapses whitespace?
  | Shell Exp -- Collapses whitespace
  | Lit Text
  | Var Exp
  | Exp `Cat` Exp
  | Wildcard Exp
  | Patsubst Exp Exp Exp -- Collapses whitespace?
  | Varsubst Exp Exp Exp -- Collapses whitespace
  | Call Exp Exp -- Collapses whitespace
  | Error Exp
  | Sort Exp
  | Subst Exp Exp Exp
  | Word Exp Exp
  | Firstword Exp
  | Dir Exp
  | Notdir Exp
  | Basename Exp
  -- | Wordlist Exp Exp Exp
  -- | Strip Exp
  -- | AddSuffix Exp Exp
  -- | Abspath Exp Exp
  -- | Eval FIXME
  deriving (Eq, Show)

data Binder = DeferredBinder | ImmediateBinder | DefaultValueBinder | ShellBinder | AppendBinder
  deriving (Eq, Show)

data Stmt
  = BindDeferred Exp Exp
  | Exp := Exp
  | Exp :+= Exp
  | Exp :!= Exp
  | SExp Exp
  | Skip
  | Ifeq Exp Exp Stmt Stmt
  | Ifneq Exp Exp Stmt Stmt
  deriving (Eq, Show)

type Program = [Stmt]

newtype Value = Value { fromValue :: Text }
  deriving (Eq, Show)
data Binding = Immediate Value | Deferred Exp
  deriving (Eq, Show)
type Env = Map Name Binding
data EvalState = EvalState
  { env :: Env }
  deriving (Eq, Show)

newtype Recipe = Recipe [Text]
  deriving (Eq, Show)
data Rule = Rule Exp Exp Recipe
  deriving (Eq, Show)
data TopLevel = Stmt Stmt | RuleDecl Rule
  deriving (Eq, Show)
