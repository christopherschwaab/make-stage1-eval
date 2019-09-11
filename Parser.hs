{-#LANGUAGE GADTs, FlexibleInstances #-}
module Parser where

import Syntax

import Prelude hiding (lex)

import Control.Applicative (pure, (<*>), (*>), (<*), (<$))
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
import Data.Void (Void)
import Numeric.Natural
import qualified Shelly as Sh
import System.FilePath (dropExtensions, dropFileName, takeFileName)
import System.FilePath.Glob (compileWith, compPosix, globDir1)
import Text.Megaparsec
import Text.Megaparsec.Char
import TextShow

type Parser = Parsec Void Text

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
evalExp (Lit t) = return (Value t)
evalExp (Var e) = do Value x <- evalExp e
                     p <- env <$> get
                     case Map.lookup x p of
                       Just b -> evalBinding b
                       Nothing -> undefined
evalExp (Cat e1 e2) = Value <$> (append <$> (fromValue <$> evalExp e1)
                                        <*> (fromValue <$> evalExp e2))
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
evalExp (Builtin (PApp (App b e))) = evalBuiltin b e

evalBuiltin :: Builtin sh -> sh -> Interpreter Value
evalBuiltin Foreach (e1, e2, e3) = do
  Value x <- evalExp e1
  ws <- splitFields . fromValue <$> evalExp e3
  ws' <- mapM (\w -> withStateT (\st -> EvalState{env=Map.insert x (Immediate (Value w)) (env st)})
                                (evalExp e2))
              ws
  return (Value (intercalate " " (map fromValue ws')))
evalBuiltin Filter (e1, e2) = do
  pats <- splitFields . fromValue <$> evalExp e1
  ws <- splitFields . fromValue <$> evalExp e2
  let ws' = filter (\w -> any isJust (map (\p -> matchPat p w) pats)) ws
  return (Value (intercalate " " ws'))
evalBuiltin FilterOut (e1, e2) = do
  pat <- splitFields . fromValue <$> evalExp e1
  ws <- splitFields . fromValue <$> evalExp e2
  let ws' = filter (\w -> any isNothing (map (\p -> matchPat p w) pat)) ws
  return (Value (intercalate " " ws'))
evalBuiltin If (e1, e2, e3) = do Value p <- evalExp e1
                                 if T.null p then evalExp e2
                                   else evalExp e3
evalBuiltin And es = evalAnd "" es where
  evalAnd r (e:es) = do Value v <- evalExp e
                        if T.null v then return (Value "")
                          else evalAnd v es
  evalAnd r [] = return (Value r)
evalBuiltin Shell e = do
  Value t <- evalExp e
  (exitStatus, out) <- lift (Sh.shelly . Sh.silently $ do
    out <- Sh.run "/bin/sh" ["-c", t]
    exitStatus <- Sh.lastExitCode
    return (Value (showt exitStatus), T.replace "\n" " " out))
  modify (\st -> EvalState{env=Map.insert ".SHELLOUT" (Immediate exitStatus) (env st)})
  return (Value out)
evalBuiltin Patsubst (e1, e2, e3) = do
  Value pat <- evalExp e1
  Value replacement <- evalExp e2
  ts <- T.words . fromValue <$> evalExp e3
  let ts' = map (patsubst pat replacement) ts
  return (Value (intercalate " " ts'))
evalBuiltin Wildcard e = do Value pattern <- evalExp e
                            lift (spaceSeparate . map pack <$> globInCwd (unpack pattern))
  where spaceSeparate = Value . intercalate " "
        globInCwd p = globDir1 (compileWith compPosix p) ""
evalBuiltin Call [] = return (Value "")
evalBuiltin Call (e:es) = do
  -- This explicitly assumes that make has implemented arguments incorrectly so check if
  -- assignment can overwrite argument values.
  args <- map fromValue <$> mapM evalExp es
  withStateT (\st ->
    EvalState{env=foldr (\(n, arg) p -> Map.insert (showt n) (Immediate (Value arg)) p)
                        (env st)
                        (zip [(1::Int)..] args)})
    (evalExp e)
evalBuiltin Error e = error . unpack . fromValue <$> evalExp e
evalBuiltin Sort e = Value . intercalate " " . sort . splitFields . fromValue <$> evalExp e
evalBuiltin Subst (e1, e2, e3) = undefined
evalBuiltin Word (e1, e2) = do
  Value v1 <- evalExp e1
  case TR.decimal v1 of
    Left err -> undefined -- what does make do?
    Right (n,v1') -> if T.null v1'
                       then do vs <- splitFields . fromValue <$> evalExp e2
                               return (Value (vs!!n))
                       else undefined -- what does make do?
evalBuiltin Firstword e = Value . head . splitFields . fromValue <$> evalExp e -- what does make do for an empty list?
evalBuiltin Dir e = Value . intercalate " " . map dirname . splitFields . fromValue <$> evalExp e
  where dirname = pack . dropFileName . unpack
evalBuiltin Notdir e = Value . intercalate " " . map notdir . splitFields . fromValue <$> evalExp e
  where notdir = pack . takeFileName . unpack
evalBuiltin Basename e = Value . intercalate " " . map basename . splitFields . fromValue <$> evalExp e
  where basename = pack . dropExtensions . unpack

evalStmt :: Stmt -> Interpreter ()
evalStmt (Bind b e1 e2) = do
  Value x <- evalExp e1
  case b of
    DeferredBinder -> modify (\st -> EvalState {env=Map.insert x (Deferred e2) (env st)})
    ImmediateBinder -> do v <- evalExp e2
                          modify (\st -> EvalState {env=Map.insert x (Immediate v) (env st)})
    DefaultValueBinder -> do
      p <- env <$> get
      p' <- case Map.lookup x p of
        Just _ -> return p
        Nothing -> do v <- evalBuiltin Shell e2
                      return (Map.insert x (Immediate v) p) -- is this immediate or deferred?
      put (EvalState{env=p'})
    ShellBinder -> do v <- evalBuiltin Shell e2
                      modify (\st -> EvalState {env=Map.insert x (Immediate v) (env st)})
    AppendBinder -> do
      p <- env <$> get
      p' <- case Map.lookup x p of
        Just (Immediate (Value v')) -> do
          Value v <- evalExp e2
          return (Map.insert x (Immediate (Value (v' `append` " " `append` v))) p)
        Just (Deferred e') -> return (Map.insert x (Deferred (e' `cat` Lit " " `cat` e2)) p)
        Nothing -> undefined -- what does make do in this case?
      put (EvalState{env=p'})
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

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

innerLitExp :: Maybe Char -> Parser Text
innerLitExp closeBracket = do
  let nonLitChar = ['\\', '$', '\n', '#'] ++ maybe [] return closeBracket
  l <- takeWhileP (Just "right-hand literal character") (\c -> not (c `elem` nonLitChar))
  choice [append l . append " " <$> (lineContinuation *> innerLitExp closeBracket)
         ,append "$" <$> (chunk "$$" *> innerLitExp closeBracket)
         ,append <$> (char '\\' *> option "\\" escaped) <*> innerLitExp closeBracket
         ,pure l]
  where escaped = choice [chunk "$ " *> pure ""
                         ,char '#'   *> pure "#"
                         ,char '\\'  *> pure "\\"
                         ,char '\n'  *> pure " "]

class ParseBuiltinArgs sh where
  parseBuiltinArgs :: Parser sh
instance ParseBuiltinArgs [Exp] where
  parseBuiltinArgs = expVarArgs
instance ParseBuiltinArgs Exp where
  parseBuiltinArgs = expArgs 1 >>= \[x] -> return x
instance ParseBuiltinArgs (Exp, Exp) where
  parseBuiltinArgs = expArgs 2 >>= \[x,y] -> return (x, y)
instance ParseBuiltinArgs (Exp, Exp, Exp) where
  parseBuiltinArgs = expArgs 3 >>= \[x,y,z] -> return (x, y, z)

builtin :: Parser PAppliedBuiltin
builtin = choice builtins where
  reserved x = undefined
  builtins = [reserved "foreach" >> PApp . App Foreach <$> parseBuiltinArgs
             ,reserved "filter" >> PApp . App Filter <$> parseBuiltinArgs
             ,reserved "filter-out" >> PApp . App FilterOut <$> parseBuiltinArgs
             ,reserved "if" >> PApp . App If <$> parseBuiltinArgs
             ,reserved "and" >> PApp . App And <$> parseBuiltinArgs
             ,reserved "shell" >> PApp . App Shell <$> parseBuiltinArgs
             ,reserved "wildcard" >> PApp . App Wildcard <$> parseBuiltinArgs
             ,reserved "patsubst" >> PApp . App Patsubst <$> parseBuiltinArgs
             ,reserved "call" >> PApp . App Call <$> parseBuiltinArgs
             ,reserved "error" >> PApp . App Error <$> parseBuiltinArgs
             ,reserved "sort" >> PApp . App Sort <$> parseBuiltinArgs
             ,reserved "subst" >> PApp . App Subst <$> parseBuiltinArgs
             ,reserved "word" >> PApp . App Word <$> parseBuiltinArgs
             ,reserved "firstword" >> PApp . App Firstword <$> parseBuiltinArgs
             ,reserved "dir" >> PApp . App Dir <$> parseBuiltinArgs
             ,reserved "notdir" >> PApp . App Notdir <$> parseBuiltinArgs
             ,reserved "basename" >> PApp . App Basename <$> parseBuiltinArgs]

appliedBuiltin :: Parser PAppliedBuiltin
appliedBuiltin = undefined

innerExp :: Maybe Char -> Parser Exp
innerExp closeBracket = foldr1 cat <$> some exp where
  exp = choice [Builtin <$> appliedBuiltin
               ,Lit <$> innerLitExp closeBracket
               ,Var <$> (char '$' *> dollarExp)]

dollarExp :: Parser Exp
dollarExp = choice [parens (innerExp (Just ')'))
                   ,braces (innerExp (Just '}'))
                   ,Lit <$> (option " " (T.singleton <$> anySingleBut '\n'))]

rexp :: Parser Exp
rexp = innerExp Nothing

lineContinuation :: Parser Text
lineContinuation = " " <$ (char '\\' >> eol) -- Is this even correct? What does make consider a line end?

expectLineContinuation :: Char -> Parser ()
expectLineContinuation '\n' = pure ()
expectLineContinuation _ = fail "Newlines in rule identifiers must be escaped by a backslash \\."

data LWord = LRuleOrVarDecl Exp | LVarDecl Exp Binder | LRuleDecl Exp
  deriving (Eq, Show)

lwordExp :: LWord -> Exp
lwordExp (LRuleOrVarDecl e) = e
lwordExp (LVarDecl e _) = e
lwordExp (LRuleDecl e) = e

-- This seems to basically accept anything (including strange unicode space
-- chars) except ascii space, newline, tab, and carriage return.
lword :: Exp -> Parser LWord
lword prefix =  do
  leading <- takeWhileP (Just "identifier character") (not . flip elem stopChars)
  -- FIXME Tidy mess and fail on empty identifiers.
  choice [oneOf sepChars *> pure (LRuleOrVarDecl (prefix `catr` leading))
         ,char '\n' *> expectLineContinuation (T.last leading) *> lword (prefix `catr` (leading `append` " "))
         ,char '=' *> pure (charToBinder leading)
         ,char ':' *> ((LVarDecl (prefix `catr` leading) ImmediateBinder <$ char '=') <|> pure (LRuleDecl (prefix `catr` leading)))
         ,char '$' *> dollarExp >>= \e -> lword (prefix `catr` leading `cat` e)]
  where sepChars = [' ', '\t']
        stopChars = sepChars ++ ['\n', '=', ':', '$']
        charToBinder l = Map.findWithDefault
                           (LVarDecl (prefix `catr` l) DeferredBinder)
                           (T.last l)
                           (Map.fromList [('?', LVarDecl (prefix `catr` T.init l) DefaultValueBinder)
                                         ,('!', LVarDecl (prefix `catr` T.init l) ShellBinder)
                                         ,('+', LVarDecl (prefix `catr` T.init l) AppendBinder)])
{-# INLINE lword #-}

parseLWord :: Parser LWord
parseLWord = lword (Lit "")
{-# INLINE parseLWord #-}

expArgs :: Natural -> Parser [Exp]
expArgs 0 = pure []
expArgs 1 = return <$> innerExp Nothing
expArgs n = (:) <$> innerExp (Just ',') <*> args (pred n) where
  args :: Natural -> Parser [Exp]
  args 1 = return <$> innerExp Nothing
  args n = (:) <$> (char ',' *> innerExp (Just ',')) <*> args (pred n)

expVarArgs :: Parser [Exp]
expVarArgs = innerExp (Just ',') `sepBy` char ','

collapseContLines :: (a -> a -> a) -> Parser a -> Parser a
collapseContLines f p = foldr1 f <$> ((:) <$> p <*> many (lineContinuation *> p))

parseBinding :: Exp -> Parser Stmt
parseBinding e = binder <* expSpaces <*> pure e <*> rexp
  where binder :: Parser (Exp -> Exp -> Stmt)
        binder = choice [char '='   *> pure (Bind DeferredBinder)
                        ,chunk ":=" *> pure (Bind ImmediateBinder)
                        ,chunk "!=" *> pure (Bind ShellBinder)
                        ,chunk "+=" *> pure (Bind AppendBinder)]

spaces :: Parser Text
spaces = takeWhileP (Just "space") (== ' ')

expSpaces :: Parser Text
expSpaces = collapseContLines (\s1 s2 -> s1 `append` " " `append` s2) spaces

parseRule :: Exp -> Parser Rule
parseRule e = Rule e <$> parseDependencies <*> parseRecipe

parseRuleOrVarDecl :: Exp -> Parser TopLevel
parseRuleOrVarDecl e = (Stmt <$> parseBinding e)
                   <|> (RuleDecl <$> (char ':' *> parseRule e))

parseDependencies :: Parser [Exp]
parseDependencies = undefined

parseRecipe :: Parser Recipe
parseRecipe = undefined

parseTopLevel :: Parser TopLevel
parseTopLevel = parseLWord >>= lwordCont
  where lwordCont :: LWord -> Parser TopLevel
        lwordCont (LRuleOrVarDecl e) = collapseContLines (\_ _ -> ()) (void spaces) *> parseRuleOrVarDecl e
        lwordCont (LVarDecl e b) = Stmt . Bind b e <$> (expSpaces *> rexp)
        lwordCont (LRuleDecl e) = RuleDecl <$> parseRule e

recipeWhite :: Parser ()
recipeWhite = undefined

{-
parseRecipe :: Parser Recipe
parseRecipe = many (many1 tab *> lineCont)
  where emptySpace = undefined *> return "\n"
        lineCont = do prefix <- takeWhileP (Just "recipe character") (/= '\n')
                      line <- if T.last line == '\\' then append (prefix `append` " ") <$> lineCont
                                else pure line
                      recipeWhite
                      return line

parseTopLevel :: Parser TopLevel
parseTopLevel = spaces *> (Stmt <$> parseStmt
                       <|> RuleDecl <$> parseRule)
-}
