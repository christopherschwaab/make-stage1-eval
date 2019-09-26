{-#LANGUAGE GADTs, StandaloneDeriving #-}
module Syntax where

import Data.Map (Map)
import Data.Text (Text, append)
import qualified Data.Text as T
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
  Info       :: Builtin Exp
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
  PApp (App Info      x) == PApp (App Info      y) = x == y
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
  show (PApp a@(App Info      x)) = "(PApp (" ++ show a ++ "))"

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
infixr `Cat`

data Binder = DeferredBinder | ImmediateBinder | DefaultValueBinder | ShellBinder | AppendBinder
  deriving (Eq, Show)

data IfPred = EqPred | NeqPred deriving (Eq, Show)
data Stmt
  = Bind Binder Exp Exp
  | SExp Exp
  | Skip
  | IfStmt IfPred Exp Exp Stmt Stmt
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

class PrettyAppExp sh where
  prettyAppExp :: sh -> Text
instance PrettyAppExp a => PrettyAppExp [a] where
  prettyAppExp es = T.intercalate "," (map prettyAppExp es)
instance (PrettyAppExp a, PrettyAppExp b, PrettyAppExp c) => PrettyAppExp (a, b, c) where
  prettyAppExp (e1, e2, e3) = prettyAppExp e1
                     `append` ","
                     `append` prettyAppExp e2
                     `append` ","
                     `append` prettyAppExp e3
instance (PrettyAppExp a, PrettyAppExp b) => PrettyAppExp (a, b) where
  prettyAppExp (e1, e2) = prettyAppExp e1 `append` "," `append` prettyAppExp e2
instance PrettyAppExp Exp where
  prettyAppExp = prettyExp

prettyPApp :: PAppliedBuiltin -> Text
prettyPApp (PApp (App Foreach   x)) = "foreach " `append` prettyAppExp x
prettyPApp (PApp (App Filter    x)) = "filter " `append` prettyAppExp x
prettyPApp (PApp (App FilterOut x)) = "filter-out " `append` prettyAppExp x
prettyPApp (PApp (App If        x)) = "if " `append` prettyAppExp x
prettyPApp (PApp (App And       x)) = "and " `append` prettyAppExp x
prettyPApp (PApp (App Shell     x)) = "shell " `append` prettyAppExp x
prettyPApp (PApp (App Wildcard  x)) = "wildcard " `append` prettyAppExp x
prettyPApp (PApp (App Patsubst  x)) = "patsubst " `append` prettyAppExp x
prettyPApp (PApp (App Call      x)) = "call " `append` prettyAppExp x
prettyPApp (PApp (App Error     x)) = "error " `append` prettyAppExp x
prettyPApp (PApp (App Sort      x)) = "sort " `append` prettyAppExp x
prettyPApp (PApp (App Subst     x)) = "subst " `append` prettyAppExp x
prettyPApp (PApp (App Word      x)) = "word " `append` prettyAppExp x
prettyPApp (PApp (App Firstword x)) = "firstword " `append` prettyAppExp x
prettyPApp (PApp (App Dir       x)) = "dir " `append` prettyAppExp x
prettyPApp (PApp (App Notdir    x)) = "notdir " `append` prettyAppExp x
prettyPApp (PApp (App Basename  x)) = "basename " `append` prettyAppExp x
prettyPApp (PApp (App Info      x)) = "info " `append` prettyAppExp x

prettyExp :: Exp -> Text
prettyExp (Lit t) = t -- FIXME escape
prettyExp (Var e) = "$(" `append` prettyExp e `append` ")"
prettyExp (e1 `Cat` e2) = prettyExp e1 `append` prettyExp e2
prettyExp (Varsubst e1 e2 e3) = T.concat ["$(", prettyExp e1, ":", prettyExp e2, "=", prettyExp e3, ")"]
prettyExp (Builtin app) = "$(" `append` prettyPApp app `append` ")"

prettyBinder :: Binder -> Text
prettyBinder DeferredBinder = "="
prettyBinder ImmediateBinder = ":="
prettyBinder DefaultValueBinder = "?="
prettyBinder ShellBinder = "!="
prettyBinder AppendBinder = "+="

prettyRecipe :: Recipe -> Text
prettyRecipe (Recipe es) = "\t" `append` T.intercalate "\n\t" (map prettyExp es)

prettyRule :: Rule -> Text
prettyRule (Rule e mdeps r) = prettyExp e `append` ":" `append` prettyDeps mdeps `append` "\n" `append` prettyRecipe r
  where prettyDeps (Just deps) = " " `append` prettyExp deps
        prettyDeps Nothing = ""

prettyStmt :: Stmt -> Text
prettyStmt (Bind b e1 e2) = T.intercalate " " [prettyExp e1, prettyBinder b, prettyExp e2]
prettyStmt (SExp e) = prettyExp e
prettyStmt Skip = T.empty
prettyStmt (IfStmt p e1 e2 s1 s2) = case s2 of
  Skip -> T.intercalate "\n" [prettyEq, "\t" `append` prettyStmt s1, "else", "\t" `append` prettyStmt s2, "endif"]
  _    -> T.intercalate "\n" [prettyEq, "\t" `append` prettyStmt s1, "endif"]
  where prettyEq = T.concat [prettyIfPred p, " (", prettyExp e1, ",", prettyExp e2, ")"]
        prettyIfPred EqPred = "ifeq"
        prettyIfPred NeqPred = "ifneq"

prettyTopLevel :: TopLevel -> Text
prettyTopLevel (Stmt s) = prettyStmt s
prettyTopLevel (RuleDecl r) = T.concat ["\n", prettyRule r, "\n"]
prettyTopLevel (Include e) = "include " `append` prettyExp e

prettyProgram :: Program -> Text
prettyProgram ts = T.intercalate "\n" (map prettyTopLevel ts)
