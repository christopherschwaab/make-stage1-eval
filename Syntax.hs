{-#LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances #-}
module Syntax where

import Data.Map (Map)
import Data.Text (Text, append)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec)
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

data Directive s = If IfPred s s
                 | VPath (Maybe (Either Exp (Exp, Exp)))
                 | Include Exp
  deriving (Eq, Show)

data RecipeLine = RIf IfPred [RecipeLine] [RecipeLine]
                | RExp Exp
  deriving (Eq, Show)
newtype Recipe = Recipe [RecipeLine]
  deriving (Eq, Show)
data Rule = Rule [Exp] (Maybe Exp) Recipe
  deriving (Eq, Show)

data Term
  = Indented Term
  | TExp [Exp]
  | RuleDecl [Exp] (Maybe Exp)
  | TAssign Binder Exp [Exp] Text Exp
  | TOverride Binder Exp [Exp] Text Exp
  | TDirective (Directive [Term])
  deriving (Eq, Show)

data Stmt
  = SExp Exp
  | SRule Rule
  | Assign Binder Exp Exp
  | Override Binder Exp Exp
  | SDirective (Directive [Stmt])
  deriving (Eq, Show)

type SProgram = [Stmt]
type Program = [Term]

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

prettyRecipeLine :: RecipeLine -> Text
prettyRecipeLine (RIf p rs1 rs2) = prettyIf prettyRecipeLine p rs1 rs2
prettyRecipeLine (RExp e) = "\t" `append` prettyExp e

prettyRecipe :: Recipe -> Text
prettyRecipe (Recipe es) = "\t" `append` T.intercalate "\n" (map prettyRecipeLine es)

prettyRuleDecl :: [Exp] -> Maybe Exp -> Text
prettyRuleDecl es mdeps = T.concat [T.intercalate " " (map prettyExp es)
                                   ,":"
                                   ,prettyDeps mdeps]
  where prettyDeps (Just deps) = " " `append` prettyExp deps
        prettyDeps Nothing = ""

prettyRule :: Rule -> Text
prettyRule (Rule es mdeps r) = prettyRuleDecl es mdeps `append` "\n" `append` prettyRecipe r

indent :: Int -> Text -> Text
indent d t = T.replicate d "\t" `append` t

prettyAssign :: Binder -> Exp -> Exp -> Text
prettyAssign b e1 e2 = T.intercalate " " [prettyExp e1, prettyBinder b, prettyExp e2]

prettyTAssign :: Binder -> Exp -> [Exp] -> Text -> Exp -> Text
prettyTAssign b e1 es w e2 = T.concat (prettyExp e1:map prettyExp es ++ [prettyBinder b, w, prettyExp e2])

prettyTerm :: Term -> Text
prettyTerm (Indented t) = indent 1 (prettyTerm t)
prettyTerm (TExp es) = T.concat (map prettyExp es)
prettyTerm (RuleDecl es mdeps) = prettyRuleDecl es mdeps
prettyTerm (TAssign b e1 es w e2) = prettyTAssign b e1 es w e2
prettyTerm (TOverride b e1 es w e2) = "override " `append` prettyTAssign b e1 es w e2
prettyTerm (TDirective d) = prettyDirective prettyTerm d

prettyStmt :: Stmt -> Text
prettyStmt (SExp e) = prettyExp e
prettyStmt (SRule r) = prettyRule r
prettyStmt (Assign b e1 e2) = prettyAssign b e1 e2
prettyStmt (Override b e1 e2) = "override " `append` prettyAssign b e1 e2
prettyStmt (SDirective d) = prettyDirective prettyStmt d

prettyIf :: (a -> Text) -> IfPred -> [a] -> [a] -> Text
prettyIf prettyDStmt p ss1 ss2 =
  let prettySs1 = T.intercalate "\n" (map prettyDStmt ss1)
      prettyPred = prettyIfPred p
  in if null ss2
       then T.intercalate "\n" [prettyPred, "  " `append` prettySs1, "endif"]
       else let prettySs2 = T.intercalate "\n  " (map prettyDStmt ss2)
            in T.intercalate "\n" [prettyPred, "  " `append` prettySs1, "else", "  " `append` prettySs2, "endif"]
  where prettyBinIf e1 e2 = T.concat ["(", prettyExp e1, ",", prettyExp e2, ")"]
        prettyIfPred (EqPred e1 e2) = "ifeq " `append` prettyBinIf e1 e2
        prettyIfPred (NeqPred e1 e2) = "ifneq " `append` prettyBinIf e1 e2
        prettyIfPred (DefinedPred e) = "ifdef " `append` prettyExp e
        prettyIfPred (NotDefinedPred e) = "ifndef "  `append` prettyExp e

prettyDirective :: (a -> Text) -> Directive [a] -> Text
prettyDirective prettyDStmt (VPath Nothing) = "vpath"
prettyDirective prettyDStmt (VPath (Just (Left e))) = "vpath " `append` prettyExp e
prettyDirective prettyDStmt (VPath (Just (Right (e1, e2)))) = "vpath " `append` prettyExp e1 `append` prettyExp e2
prettyDirective prettyDStmt (If p ss1 ss2) = prettyIf prettyDStmt p ss1 ss2
prettyDirective prettyDStmt (Include e) = "include " `append` prettyExp e

prettyProgram :: Program -> Text
prettyProgram ts = T.intercalate "\n" (map prettyTerm ts)

cat :: Exp -> Exp -> Exp
cat (Lit t1) (Lit t2)
  | T.null t1 = Lit t2
  | T.null t2 = Lit t1
  | otherwise = Lit (t1 `append` t2)
cat e1 (Lit t2) | T.null t2 = e1
                | otherwise = e1 `Cat` Lit t2
cat (Lit t1) e2 | T.null t1 = e2
                | otherwise = Lit t1 `Cat` e2
cat e1 e2 = e1 `Cat` e2
{-# INLINE cat #-}

catr :: Exp -> Text -> Exp
catr e t = e `cat` Lit t
{-# INLINE catr #-}

catl :: Text -> Exp -> Exp
catl t e = Lit t `cat` e
{-# INLINE catl #-}
