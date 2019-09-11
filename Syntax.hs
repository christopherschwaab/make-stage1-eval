{-#LANGUAGE GADTs, StandaloneDeriving #-}
module Syntax where

import Data.Map (Map)
import Data.Text (Text)

type Name = Text

data Builtin sh where
  Foreach    :: Builtin (Exp, Exp, Exp) -- Collapses whitespace
  Filter     :: Builtin (Exp, Exp)-- collapses whitespace?
  FilterOut  :: Builtin (Exp, Exp) -- collapses whitespace?
  If         :: Builtin (Exp, Exp, Exp) -- collapses whitespace?
  And        :: Builtin [Exp] -- collapses whitespace?
  Shell      :: Builtin Exp -- Collapses whitespace
  Wildcard   :: Builtin Exp
  Patsubst   :: Builtin (Exp, Exp, Exp) -- Collapses whitespace?
  Call       :: Builtin [Exp] -- Collapses whitespace
  Error      :: Builtin Exp
  Sort       :: Builtin Exp
  Subst      :: Builtin (Exp, Exp, Exp)
  Word       :: Builtin (Exp, Exp)
  Firstword  :: Builtin Exp
  Dir        :: Builtin Exp
  Notdir     :: Builtin Exp
  Basename   :: Builtin Exp
deriving instance Eq (Builtin sh)
deriving instance Show (Builtin sh)

data AppliedBuiltin sh where
  App :: Builtin sh -> sh -> AppliedBuiltin sh
  deriving (Eq, Show)

data PAppliedBuiltin where
  PApp :: AppliedBuiltin sh -> PAppliedBuiltin

instance Eq PAppliedBuiltin where
  PApp x == PApp y = undefined
instance Show PAppliedBuiltin where
  show (PApp x) = undefined

--  = Foreach Exp Exp Exp -- Collapses whitespace
--  | Filter Exp Exp -- collapses whitespace?
--  | FilterOut Exp Exp -- collapses whitespace?
--  | If Exp Exp Exp -- collapses whitespace?
--  | And [Exp] -- collapses whitespace?
--  | Shell Exp -- Collapses whitespace
--  | Wildcard Exp
--  | Patsubst Exp Exp Exp -- Collapses whitespace?
--  | Call Exp [Exp] -- Collapses whitespace
--  | Error Exp
--  | Sort Exp
--  | Subst Exp Exp Exp
--  | Word Exp Exp
--  | Firstword Exp
--  | Dir Exp
--  | Notdir Exp
--  | Basename Exp
--  deriving (Eq, Show)

data Exp
  = Lit Text
  | Var Exp
  | Exp `Cat` Exp
  | Varsubst Exp Exp Exp -- Collapses whitespace
  | Builtin PAppliedBuiltin
  -- | Wordlist Exp Exp Exp
  -- | Strip Exp
  -- | AddSuffix Exp Exp
  -- | Abspath Exp Exp
  -- | Eval FIXME
  deriving (Eq, Show)

data Binder = DeferredBinder | ImmediateBinder | DefaultValueBinder | ShellBinder | AppendBinder
  deriving (Eq, Show)

data Stmt
  = Bind Binder Exp Exp
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
data Rule = Rule Exp [Exp] Recipe
  deriving (Eq, Show)
data TopLevel = Stmt Stmt | RuleDecl Rule
  deriving (Eq, Show)
