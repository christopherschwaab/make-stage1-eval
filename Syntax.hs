{-#LANGUAGE GADTs, StandaloneDeriving #-}
module Syntax where

import Data.Map (Map)
import Data.Text (Text)
import Unsafe.Coerce (unsafeCoerce)

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
  PApp (App Foreach   x) == PApp (App Foreach   y) = x == y
  PApp (App Filter    x) == PApp (App Filter    y) = x == y
  PApp (App FilterOut x) == PApp (App FilterOut y) = x == y
  PApp (App If        x) == PApp (App If        y) = x == y
  PApp (App And       x) == PApp (App And       y) = x == y
  PApp (App Shell     x) == PApp (App Shell     y) = x == y
  PApp (App Wildcard  x) == PApp (App Wildcard  y) = x == y
  PApp (App Patsubst  x) == PApp (App Patsubst  y) = x == y
  PApp (App Call      x) == PApp (App Call      y) = x == y
  PApp (App Error     x) == PApp (App Error     y) = x == y
  PApp (App Sort      x) == PApp (App Sort      y) = x == y
  PApp (App Subst     x) == PApp (App Subst     y) = x == y
  PApp (App Word      x) == PApp (App Word      y) = x == y
  PApp (App Firstword x) == PApp (App Firstword y) = x == y
  PApp (App Dir       x) == PApp (App Dir       y) = x == y
  PApp (App Notdir    x) == PApp (App Notdir    y) = x == y
  PApp (App Basename  x) == PApp (App Basename  y) = x == y
  _ == _ = False
instance Show PAppliedBuiltin where
  show (PApp a@(App Foreach   x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Filter    x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App FilterOut x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App If        x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App And       x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Shell     x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Wildcard  x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Patsubst  x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Call      x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Error     x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Sort      x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Subst     x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Word      x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Firstword x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Dir       x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Notdir    x)) = "(PApp (" ++ show a ++ "))"
  show (PApp a@(App Basename  x)) = "(PApp (" ++ show a ++ "))"

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

newtype Value = Value { fromValue :: Text }
  deriving (Eq, Show)
data Binding = Immediate Value | Deferred Exp
  deriving (Eq, Show)
type Env = Map Name Binding
data EvalState = EvalState
  { env :: Env
  , targets :: Map Text ([Text], [Exp]) }
  deriving (Eq, Show)

newtype Recipe = Recipe [Exp]
  deriving (Eq, Show)
data Rule = Rule Exp (Maybe Exp) Recipe
  deriving (Eq, Show)
data TopLevel = Stmt Stmt
              | RuleDecl Rule
              | Include Exp
  deriving (Eq, Show)

type Program = [TopLevel]
