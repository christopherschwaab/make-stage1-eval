{-#LANGUAGE OverloadedStrings, TypeOperators, ViewPatterns #-}
module Main where

import Prelude hiding (lex)

import Control.Applicative
import Control.Applicative.Combinators (between)
import Control.Monad.State.Lazy
import Data.Char (isSpace)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import Data.Text (Text, append, intercalate, pack, uncons, unpack)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Traversable (sequence)
import qualified Shelly as Sh
import Data.Void (Void)
import System.FilePath (dropExtensions, dropFileName, takeFileName)
import System.FilePath.Glob (compileWith, compPosix, globDir1)
import Text.Megaparsec
import Text.Megaparsec.Char
import TextShow

type Parser = Parsec Void Text

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
  deriving Show

data Binder = DeferredBinder | ImmediateBinder | DefaultValueBinder | ShellBinder | AppendBinder
  deriving Show

data Stmt
  = BindDeferred Exp Exp
  | Exp := Exp
  | Exp :+= Exp
  | Exp :!= Exp
  | SExp Exp
  | Skip
  | Ifeq Exp Exp Stmt Stmt
  | Ifneq Exp Exp Stmt Stmt
  deriving Show

type Program = [Stmt]

newtype Value = Value { fromValue :: Text }
  deriving Show
data Binding = Immediate Value | Deferred Exp
  deriving Show
type Env = Map Name Binding
data EvalState = EvalState
  { env :: Env }
  deriving Show

evalBinding :: Binding -> Interpreter Value
evalBinding (Immediate v) = return v
evalBinding (Deferred e)  = evalExp e

type Interpreter = StateT EvalState IO

splitFields :: Text -> [Text]
splitFields = T.words

data SubText = SubText { offset :: Int, subtext :: Text }
subtextUncons :: SubText -> Maybe (Char, SubText)
subtextUncons (SubText i t) = fmap (\(x, t') -> (x, SubText (i+1) t')) (uncons t)

matchExact :: (SubText -> Maybe Text) -> Text -> Text -> SubText -> Maybe Text
matchExact backtrack matched pat t = case (uncons pat, subtextUncons t) of
  (Just (x, pat'), Just (y, t')) -> if x == y then matchExact backtrack matched pat' t'
                                      else backtrack t
  (Nothing, Just _) -> backtrack t
  (Nothing, Nothing) -> Just matched
  (Just _, Nothing) -> Nothing

matchWild :: Text -> Char -> Text -> SubText -> Maybe Text
matchWild matched x pat t = do
  (y, t') <- subtextUncons t
  if x == y
    then matchExact (matchWild matched x pat) (T.take (offset t) matched) pat t'
    else matchWild matched x pat t'

matchPat :: Text -> Text -> Maybe Text
matchPat pat = case uncons pat of
  Just ('%', pat') -> \t -> maybe (Just t) (\(x, pat'') -> matchWild t x pat'' (SubText 0 t)) (uncons pat')
  Just (x, pat') -> \t -> do (y, t') <- uncons t
                             guard (x == y)
                             matchPat pat' t'
  Nothing -> \t -> if T.null t then Just "" else Nothing

patsubst :: Text -> Text -> Text -> Text
patsubst pat replacement t = case matchPat pat t of
  Just match -> T.replace "%" match replacement -- FIXME does make only replace the first %?
  Nothing -> t

-- FIXME Whitespace preservation/collapse is surely wrong
evalExp :: Exp -> Interpreter Value
evalExp (Foreach e1 e2 e3) = do
  Value x <- evalExp e1
  ws <- splitFields . fromValue <$> evalExp e3
  ws' <- mapM (\w -> withStateT (\st -> EvalState{env=Map.insert x (Immediate (Value w)) (env st)})
                                (evalExp e2))
              ws
  return (Value (intercalate " " (map fromValue ws')))
evalExp (Filter e1 e2) = do pats <- splitFields . fromValue <$> evalExp e1
                            ws <- splitFields . fromValue <$> evalExp e2
                            let ws' = filter (\w -> any isJust (map (\p -> matchPat p w) pats)) ws
                            return (Value (intercalate " " ws'))
evalExp (FilterOut e1 e2) = do pat <- splitFields . fromValue <$> evalExp e1
                               ws <- splitFields . fromValue <$> evalExp e2
                               let ws' = filter (\w -> any isNothing (map (\p -> matchPat p w) pat)) ws
                               return (Value (intercalate " " ws'))
evalExp (If e1 e2 e3) = do Value p <- evalExp e1
                           if T.null p then evalExp e2
                             else evalExp e3
evalExp (And es) = evalAnd "" es where
  evalAnd r (e:es) = do Value v <- evalExp e
                        if T.null v then return (Value "")
                          else evalAnd v es
  evalAnd r [] = return (Value r)
evalExp (Shell e) = do Value t <- evalExp e
                       (exitStatus, out) <- lift (Sh.shelly . Sh.silently $ do
                         out <- Sh.run "/bin/sh" ["-c", t]
                         exitStatus <- Sh.lastExitCode
                         return (Value (showt exitStatus), T.replace "\n" " " out))
                       modify (\st -> EvalState{env=Map.insert ".SHELLOUT" (Immediate exitStatus) (env st)})
                       return (Value out)
evalExp (Lit t) = return (Value t)
evalExp (Var e) = do Value x <- evalExp e
                     p <- env <$> get
                     case Map.lookup x p of
                       Just b -> evalBinding b
                       Nothing -> undefined
evalExp (Cat e1 e2) = Value <$> (append <$> (fromValue <$> evalExp e1)
                                        <*> (fromValue <$> evalExp e2))
evalExp (Patsubst e1 e2 e3) = do
  Value pat <- evalExp e1
  Value replacement <- evalExp e2
  ts <- T.words . fromValue <$> evalExp e3
  let ts' = map (patsubst pat replacement) ts
  return (Value (intercalate " " ts'))
evalExp (Wildcard e) = do Value pattern <- evalExp e
                          lift (spaceSeparate . map pack <$> globInCwd (unpack pattern))
  where spaceSeparate = Value . intercalate " "
        globInCwd p = globDir1 (compileWith compPosix p) ""
evalExp (Varsubst e1 e2 e3) = do
  Value x <- evalExp e1
  Value suffix <- evalExp e2
  Value replacement <- evalExp e3
  p <- env <$> get
  case Map.lookup x p of
    Nothing -> undefined
    Just b -> do Value v <- evalBinding b
                 let x' = map (substSuffix suffix replacement) (T.words v)
                 return (Value (intercalate " " x'))
  where substSuffix :: Text -> Text -> Text -> Text
        substSuffix suffix replacement t = case T.stripSuffix suffix t of
          Nothing -> t
          Just t' -> t' `append` replacement
evalExp (Call e1 e2) = do
  -- This explicitly assumes that make has implemented arguments incorrectly so check if
  -- assignment can overwrite argument values.
  args <- T.lines . fromValue <$> evalExp e2
  withStateT (\st ->
    EvalState{env=foldr (\(n, arg) p -> Map.insert (showt n) (Immediate (Value arg)) p)
                        (env st)
                        (zip [(1::Int)..] args)})
    (evalExp e1)
evalExp (Error e) = error . unpack . fromValue <$> evalExp e
evalExp (Sort e) = Value . intercalate " " . sort . splitFields . fromValue <$> evalExp e
evalExp (Subst e1 e2 e3) = undefined
evalExp (Word e1 e2) = do Value v1 <- evalExp e1
                          case TR.decimal v1 of
                            Left err -> undefined -- what does make do?
                            Right (n,v1') -> if T.null v1'
                                               then do vs <- splitFields . fromValue <$> evalExp e2
                                                       return (Value (vs!!n))
                                               else undefined -- what does make do?
evalExp (Firstword e) = Value . head . splitFields . fromValue <$> evalExp e -- what does make do for an empty list?
evalExp (Dir e) = Value . intercalate " " . map dirname . splitFields . fromValue <$> evalExp e
  where dirname = pack . dropFileName . unpack
evalExp (Notdir e) = Value . intercalate " " . map notdir . splitFields . fromValue <$> evalExp e
  where notdir = pack . takeFileName . unpack
evalExp (Basename e) = Value . intercalate " " . map basename . splitFields . fromValue <$> evalExp e
  where basename = pack . dropExtensions . unpack

evalStmt :: Stmt -> Interpreter ()
evalStmt (BindDeferred e1 e2) = do Value x <- evalExp e1
                                   modify (\st -> EvalState {env=Map.insert x (Deferred e2) (env st)})
evalStmt (e1 := e2) = do Value x <- evalExp e1
                         v <- evalExp e2
                         modify (\st -> EvalState {env=Map.insert x (Immediate v) (env st)})
evalStmt (e1 :+= e2) = do
  p <- env <$> get
  Value x <- evalExp e1
  p' <- case Map.lookup x p of
          Just (Immediate (Value v')) -> do
            Value v <- evalExp e2
            return (Map.insert x (Immediate (Value (v' `append` " " `append` v))) p)
          Just (Deferred e') -> return (Map.insert x (Deferred (e' `cat` Lit " " `cat` e2)) p)
          Nothing -> undefined -- what does make do in this case?
  put (EvalState{env=p'})
evalStmt (e1 :!= e2) = do Value x <- evalExp e1
                          v <- evalExp (Shell e2)
                          modify (\st -> EvalState {env=Map.insert x (Immediate v) (env st)})
evalStmt (SExp e) = () <$ evalExp e
evalStmt Skip = return ()
evalStmt (Ifeq e1 e2 s1 s2) = do Value v1 <- evalExp e1
                                 Value v2 <- evalExp e2
                                 if v1 == v2 then evalStmt s1
                                   else evalStmt s2
evalStmt (Ifneq e1 e2 s1 s2) = do Value v1 <- evalExp e1
                                  Value v2 <- evalExp e2
                                  if v1 /= v2 then evalStmt s1
                                    else evalStmt s2

run :: Program -> IO EvalState
run p = execStateT (mapM_ evalStmt p) (EvalState{env=Map.empty})

newtype Recipe = Recipe Text
  deriving Show
data Rule = Rule Exp Exp Recipe
  deriving Show
data TopLevel = Stmt Stmt | RuleDecl Rule
  deriving Show

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

parseRExp :: Parser Exp
parseRExp = undefined

parseExp :: Char -> Parser Exp
parseExp closer = do
  l <- takeWhileP (Just "expression (non-'$') character") (not . flip elem ['$', '\n', closer])
  choice [char '$' *> (catl l <$> parseDollarExp)
         ,char '\n' *> expLineEnd l
         ,Lit l<$ lookAhead (char closer)]
  where expLineEnd l = if T.last l == '\\'
                         then catl (l `append` " ") <$> parseExp closer
                         else pure (Lit l)

parseDollarExp :: Parser Exp
parseDollarExp = dollarLit <|> (Var <$> expr) where
  dollarLit = Lit . T.singleton <$> char '$'
  expr = parens (parseExp ')')
     <|> braces (parseExp '}')
     <|> Lit <$> takeP (Just "unbracketed identifier character") 1

parseWord :: Parser Text
parseWord =  takeWhileP (Just "word") (not . isSpace)

cat :: Exp -> Exp -> Exp
cat (Lit t1) (Lit t2) = Lit (t1 `append` t2)
cat e1 e2 = e1 `Cat` e2

catr :: Exp -> Text -> Exp
catr (Lit t1) t2 = Lit (t1 `append` t2)
catr e t = e `Cat` Lit t

catl :: Text -> Exp -> Exp
catl t1 (Lit t2) = Lit (t1 `append` t2)
catl t e = Lit t `Cat` e

lineContinuation :: Parser ()
lineContinuation = void (char '\\' >> eol) -- Is this even correct? What does make consider a line end?

expectLineContinuation :: Char -> Parser ()
expectLineContinuation '\n' = pure ()
expectLineContinuation _ = fail "Newlines in rule identifiers must be escaped by a backslash \\."

data LWord = LRuleOrVarDecl Exp | LVarDecl Exp Binder | LRuleDecl Exp
  deriving Show

-- This seems to basically accept anything (including strange unicode space
-- chars) except ascii space, newline, tab, and carriage return.
parseLWord :: Exp -> Parser LWord
parseLWord prefix =  do
  leading <- takeWhileP (Just "identifier character") (not . flip elem stopChars)
  -- FIXME Tidy mess and fail on empty identifiers.
  choice [oneOf sepChars *> pure (LRuleOrVarDecl (prefix `catr` leading))
         ,char '\n' *> expectLineContinuation (T.last leading) *> parseLWord (prefix `catr` (leading `append` " "))
         ,char '=' *> pure (charToBinder leading)
         ,char ':' *> ((LVarDecl (prefix `catr` leading) ImmediateBinder <$ char '=') <|> pure (LRuleDecl prefix))
         ,char '$' *> parseDollarExp >>= \e -> parseLWord (prefix `catr` leading `cat` e)]
  where sepChars = [' ', '\t']
        stopChars = sepChars ++ ['\n', '=', ':', '$']
        charToBinder l = Map.findWithDefault
                           (LVarDecl (prefix `catr` l) DeferredBinder)
                           (T.last l)
                           (Map.fromList [('?', LVarDecl (prefix `catr` T.init l) DefaultValueBinder)
                                         ,('!', LVarDecl (prefix `catr` T.init l) ShellBinder)
                                         ,('+', LVarDecl (prefix `catr` T.init l) AppendBinder)])
{-# INLINE parseLWord #-}

parseLExp :: Parser Exp
parseLExp = (lwordCont =<< parseLWord (Lit ""))
  where lwordCont :: LWord -> Parser Exp
        lwordCont = undefined

parseRule :: Parser Rule
parseRule = undefined

parseBinding :: Exp -> Parser Stmt
parseBinding e = lex binder <*> pure e <*> parseRExp
  where binder :: Parser (Exp -> Exp -> Stmt)
        binder = choice [char '='   *> pure BindDeferred
                        ,chunk ":=" *> pure (:=)
                        ,chunk "!=" *> pure (:!=)
                        ,chunk "+=" *> pure (:+=)]

spaces :: Parser ()
spaces = void (takeWhileP (Just "space") (== ' '))

lex :: Parser a -> Parser a
lex p = p <* space

parseStmt :: Parser Stmt
parseStmt = optional tab *> space *> lex parseLExp >>= parseBinding

parseTopLevel :: Parser TopLevel
parseTopLevel = spaces *> (Stmt <$> parseStmt
                       <|> RuleDecl <$> parseRule)

main :: IO ()
main = print =<< (run $
  [Lit "x" := Lit "xvalue"
  ,Lit "xvalue" := Lit "pig"
  ,Lit "z" := Var (Lit "x")
  ,SExp (Shell (Lit "echo " `Cat` Wildcard (Lit "*/*.cache")))
  ,Lit "y" := Shell (Lit "ls -1")
  ,Lit "q" := Patsubst (Lit "%.c") (Lit "%.o") (Lit "a.c b.c c.c.c a.h")
  ,Lit "q'" := Varsubst (Lit "q") (Lit ".o") (Lit ".c")])
