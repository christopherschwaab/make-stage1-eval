{-#LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances #-}
module Syntax where

import Data.Map (Map)
import Data.Text (Text, append)
import qualified Data.Text as T
import Unsafe.Coerce (unsafeCoerce)

data Builtin sh where
  Foreach    :: Builtin (Exp, Exp, Exp) -- Collapses whitespace
  Filter     :: Builtin (Exp, Exp)-- collapses whitespace?
  FilterOut  :: Builtin (Exp, Exp) -- collapses whitespace?
  IfExp      :: Builtin (Exp, Exp, Maybe Exp) -- collapses whitespace?
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
  PApp (App IfExp     x) == PApp (App IfExp     y) = x == y
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
  show (PApp a@(App IfExp     x)) = "(PApp (" ++ show a ++ "))"
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

data IfPred
  = EqPred Exp Exp
  | NeqPred Exp Exp
  | DefinedPred Exp
  | NotDefinedPred Exp
  deriving (Eq, Show)
data Stmt
  = Bind Bool Binder Exp Exp
  | SExp Exp
  deriving (Eq, Show)
data Directive = Include Exp
               | If IfPred [TopLevel] [TopLevel]
               | VPath (Maybe (Either Exp (Exp, Exp)))
  deriving (Eq, Show)

newtype Recipe = Recipe [Exp]
  deriving (Eq, Show)
data RuleExp = RExp Exp | RBind Exp Exp
  deriving (Eq, Show)
data Rule = Rule [Exp] (Maybe Exp) Recipe -- FIXME add locally bound variables to deps
  deriving (Eq, Show)
data TopLevel = Stmt Stmt
              | RuleDecl Rule
              | Directive Directive
  deriving (Eq, Show)

type Program = [TopLevel]

class PrettyAppExp sh where
  prettyAppExp :: sh -> Text
instance PrettyAppExp a => PrettyAppExp [a] where
  prettyAppExp es = T.intercalate "," (map prettyAppExp es)
instance (PrettyAppExp a, PrettyAppExp b) => PrettyAppExp (a, b, Maybe Exp) where
  prettyAppExp (e1, e2, Just e3) = prettyAppExp (e1, e2, e3)
  prettyAppExp (e1, e2, Nothing) = prettyAppExp (e1, e2)
instance (PrettyAppExp a, PrettyAppExp b) => PrettyAppExp (a, b, Exp) where
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
prettyPApp (PApp (App IfExp     x)) = "if " `append` prettyAppExp x
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

prettyRuleExp :: RuleExp -> Text
prettyRuleExp (RExp e) = prettyExp e
prettyRuleExp (RBind e1 e2) = T.concat [prettyExp e1, "=", prettyExp e2]

prettyRule :: Rule -> Text
prettyRule (Rule es mdeps r) = T.concat [T.intercalate " " (map prettyExp es)
                                        ,":"
                                        ,prettyDeps mdeps
                                        ,"\n"
                                        ,prettyRecipe r]
  where prettyDeps (Just deps) = " " `append` prettyExp deps
        prettyDeps Nothing = ""

indent :: Int -> Text -> Text
indent d t = T.replicate d "\t" `append` t

prettyStmt :: Stmt -> Text
prettyStmt (Bind override b e1 e2) =
  let t = T.intercalate " " [prettyExp e1, prettyBinder b, prettyExp e2]
  in if override then "override " `append` t
       else t
prettyStmt (SExp e) = prettyExp e

prettyDirective :: Directive -> Text
prettyDirective (Include e) = "include " `append` prettyExp e
prettyDirective (VPath Nothing) = "vpath"
prettyDirective (VPath (Just (Left e))) = "vpath " `append` prettyExp e
prettyDirective (VPath (Just (Right (e1, e2)))) = "vpath " `append` prettyExp e1 `append` prettyExp e2
prettyDirective (If p ss1 ss2) =
  let prettySs1 = T.intercalate "\n\t" (map prettyTopLevel ss1)
      prettyPred = prettyIfPred p
  in if null ss2
       then T.intercalate "\n" [prettyPred, "\t" `append` prettySs1, "endif"]
       else let prettySs2 = T.intercalate "\n\t" (map prettyTopLevel ss2)
            in T.intercalate "\n" [prettyPred, "\t" `append` prettySs1, "else", "\t" `append` prettySs2, "endif"]
  where prettyBinIf e1 e2 = T.concat [" (", prettyExp e1, ",", prettyExp e2, ")"]
        prettyIfPred (EqPred e1 e2) = "ifeq " `append` prettyBinIf e1 e2
        prettyIfPred (NeqPred e1 e2) = "ifneq " `append` prettyBinIf e1 e2
        prettyIfPred (DefinedPred e) = "ifdef " `append` prettyExp e
        prettyIfPred (NotDefinedPred e) = "ifndef "  `append` prettyExp e

prettyTopLevel :: TopLevel -> Text
prettyTopLevel (Stmt s) = prettyStmt s
prettyTopLevel (RuleDecl r) = T.concat ["\n", prettyRule r, "\n"]
prettyTopLevel (Directive d) = prettyDirective d

prettyProgram :: Program -> Text
prettyProgram ts = T.intercalate "\n" (map prettyTopLevel ts)
