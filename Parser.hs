{-#LANGUAGE GADTs, FlexibleInstances #-}
module Parser where

import Syntax

import Prelude hiding (lex)

import Control.Applicative (pure, (<*>), (*>), (<*), (<$))
import Control.Applicative.Combinators (between)
import Control.Monad.State.Lazy
import Data.Char (isSpace)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty, NonEmpty(..), toList)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, isJust, isNothing)
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
import Text.Megaparsec.Char.Lexer (skipLineComment)
import TextShow

type Name = Text

newtype Value = Value { fromValue :: Text }
  deriving (Eq, Show)

data Binding = Immediate Value | Deferred Exp
  deriving (Eq, Show)

type Env = Map Name Binding
data EvalState = EvalState
  { env :: Env
  , targets :: Map Text ([Text], [Exp]) }
  deriving (Eq, Show)

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
  ws' <- mapM (\w -> withStateT (\st -> st{env=Map.insert x (Immediate (Value w)) (env st)})
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
                                   else maybe (pure (Value "")) evalExp e3
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
  modify (\st -> st{env=Map.insert ".SHELLOUT" (Immediate exitStatus) (env st)})
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
    st{env=foldr (\(n, arg) p -> Map.insert (showt n) (Immediate (Value arg)) p)
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
evalBuiltin Info e = evalExp e

evalStmt :: Stmt -> Interpreter ()
evalStmt (Bind override b e1 e2) = do -- FIXME override
  Value x <- evalExp e1
  case b of
    DeferredBinder -> modify (\st -> st{env=Map.insert x (Deferred e2) (env st)})
    ImmediateBinder -> do v <- evalExp e2
                          modify (\st -> st{env=Map.insert x (Immediate v) (env st)})
    DefaultValueBinder -> do
      st <- get
      let p = env st
      p' <- case Map.lookup x p of
        Just _ -> return p
        Nothing -> do v <- evalBuiltin Shell e2
                      return (Map.insert x (Immediate v) p) -- is this immediate or deferred?
      put (st{env=p'})
    ShellBinder -> do v <- evalBuiltin Shell e2
                      modify (\st -> st{env=Map.insert x (Immediate v) (env st)})
    AppendBinder -> do
      st <- get
      let p = env st
      p' <- case Map.lookup x p of
        Just (Immediate (Value v')) -> do
          Value v <- evalExp e2
          return (Map.insert x (Immediate (Value (v' `append` " " `append` v))) p)
        Just (Deferred e') -> return (Map.insert x (Deferred (e' `cat` Lit " " `cat` e2)) p)
        Nothing -> undefined -- what does make do in this case?
      put (st{env=p'})
evalStmt (SExp e) = () <$ evalExp e
evalStmt (IfStmt pred ss1 ss2) = do
  b <- evalIfPred pred
  if b then mapM_ evalTopLevel ss1
    else mapM_ evalTopLevel ss2
  where evalIfPred (EqPred e1 e2) = (==) <$> fmap fromValue (evalExp e1) <*> fmap fromValue (evalExp e2)
        evalIfPred (NeqPred e1 e2) = (/=) <$> fmap fromValue (evalExp e1) <*> fmap fromValue (evalExp e2)
        evalIfPred (DefinedPred e) = flip Map.member <$> fmap env get <*> fmap fromValue (evalExp e)
        evalIfPred (NotDefinedPred e) = flip Map.member <$> fmap env get <*> fmap fromValue (evalExp e)

-- FIXME double check the order things get evaluated here
evalRule :: Rule -> Interpreter ()
evalRule (Rule ts d (Recipe es)) = do
  vts <- mapM (fmap fromValue . evalExp) ts
  deps <- maybe (pure []) (fmap (T.lines . fromValue) . evalExp) d
  modify (\st -> st{targets = foldr (addTarget deps) (targets st) vts}) -- FIXME think make dies on  double defined targets? (except ::)
  where addTarget deps t m = Map.insert t (deps, es) m

evalTopLevel :: TopLevel -> Interpreter ()
evalTopLevel (Stmt s) = evalStmt s
evalTopLevel (RuleDecl r) = evalRule r

run :: Program -> IO EvalState
run p = execStateT (mapM_ evalTopLevel p) (EvalState{env=Map.empty, targets=Map.empty})

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

escaped :: Text -> Parser Text
escaped nl = choice [char '#'   *> pure "#"
                    ,char '\\'  *> pure "\\"
                    ,char '\n'  *> pure nl]

litExp :: Text -> [Char] -> Parser Text
litExp nl tchars = foldr1 append <$> some (do
  let nonLitChar = ['\\', '$', '\n', '#', ':'] ++ tchars
  l <- takeWhileP (Just "literal character") (\c -> not (c `elem` nonLitChar))
  choice [l `append` nl <$ try lineContinuation
         ,"$" <$ chunk "$$"
         ,append l <$> (char '\\' *> option "\\" (escaped nl))
         ,l <$ guard (not (T.null l))])

class ParseBuiltinArgs sh where
  parseBuiltinArgs :: [Char] -> Parser sh
instance ParseBuiltinArgs [Exp] where
  parseBuiltinArgs tchars = expVarArgs tchars
instance ParseBuiltinArgs Exp where
  parseBuiltinArgs tchars = expArgs tchars 1 >>= \[x] -> return x
instance ParseBuiltinArgs (Exp, Exp) where
  parseBuiltinArgs tchars = expArgs tchars 2 >>= \[x,y] -> return (x, y)
instance ParseBuiltinArgs (Exp, Exp, Exp) where
  parseBuiltinArgs tchars = expArgs tchars 3 >>= \[x,y,z] -> return (x, y, z)
instance ParseBuiltinArgs (Exp, Exp, Maybe Exp) where
  parseBuiltinArgs tchars = ifArgs tchars

ifArgs :: [Char] -> Parser (Exp, Exp, Maybe Exp)
ifArgs tchars = (,,) <$> expArg (',':tchars)
                     <*> (char ',' *> expArg (',':tchars))
                     <*> optional (char ',' *> expArg tchars)

builtin :: [Char] -> Parser PAppliedBuiltin
builtin tchars = choice builtins where
  -- FIXME does reserved eat all following whitespace or just the first?
  reserved x = try (chunk x >> choice [void lineContinuation, void (char ' '), void (char '\t')] )
  builtins = [reserved "foreach" >> PApp . App Foreach <$> parseBuiltinArgs tchars
             ,reserved "filter" >> PApp . App Filter <$> parseBuiltinArgs tchars
             ,reserved "filter-out" >> PApp . App FilterOut <$> parseBuiltinArgs tchars
             ,reserved "if" >> PApp . App If <$> ifArgs tchars
             ,reserved "and" >> PApp . App And <$> parseBuiltinArgs tchars
             ,reserved "shell" >> PApp . App Shell <$> parseBuiltinArgs tchars
             ,reserved "wildcard" >> PApp . App Wildcard <$> parseBuiltinArgs tchars
             ,reserved "patsubst" >> PApp . App Patsubst <$> parseBuiltinArgs tchars
             ,reserved "call" >> PApp . App Call <$> parseBuiltinArgs tchars
             ,reserved "error" >> PApp . App Error <$> parseBuiltinArgs tchars
             ,reserved "sort" >> PApp . App Sort <$> parseBuiltinArgs tchars
             ,reserved "subst" >> PApp . App Subst <$> parseBuiltinArgs tchars
             ,reserved "word" >> PApp . App Word <$> parseBuiltinArgs tchars
             ,reserved "firstword" >> PApp . App Firstword <$> parseBuiltinArgs tchars
             ,reserved "dir" >> PApp . App Dir <$> parseBuiltinArgs tchars
             ,reserved "notdir" >> PApp . App Notdir <$> parseBuiltinArgs tchars
             ,reserved "basename" >> PApp . App Basename <$> parseBuiltinArgs tchars
             ,reserved "info" >> PApp . App Info <$> parseBuiltinArgs tchars]

varsubstOrLit :: [Char] -> Exp -> Parser Exp
varsubstOrLit tchars e = do char ':'
                            l <- stmtInnerExp ('=':tchars)
                            varsubst l <|> pure (e `catr` ":" `cat` l)
  where varsubst l = Varsubst e l <$> (char '=' *> stmtInnerExp tchars)

innerExp :: Text -> [Char] -> Parser Exp
innerExp nl tchars = foldr1 cat <$> some exp where
  exp :: Parser Exp
  exp = do e <- choice [Lit <$> litExp nl tchars
                       ,char '$' *> dollarExp]
           option e (varsubstOrLit tchars e)

stmtInnerExp = innerExp " "
recipeInnerExp = innerExp "\n"

dollarExp :: Parser Exp
dollarExp = choice [parens (builtinCall ')' <|> builtinOrVar <$> stmtInnerExp [')'])
                   ,braces (builtinCall '}' <|> builtinOrVar <$> stmtInnerExp ['}'])
                   ,Var . Lit <$> singleCharVar]
  where singleCharVar = (char '\\' *> escaped " ")
                    <|> option "" (T.singleton <$> anySingleBut '\n')
        builtinCall = fmap Builtin . builtin . return
        builtinOrVar e@(Varsubst _ _ _) = e
        builtinOrVar e = Var e

rexp :: Parser Exp
rexp = stmtInnerExp []

lineContinuation :: Parser Text
lineContinuation = " " <$ (char '\\' >> eol) -- Is this even correct? What does make consider a line end?

expectLineContinuation :: Char -> Parser ()
expectLineContinuation '\\' = pure ()
expectLineContinuation _ = fail "Newlines in rule identifiers must be escaped by a backslash \\."

data LWord = LVarDecl Exp Binder | LRuleDecl [Exp] | LExp Exp
  deriving (Eq, Show)

-- FIXME This is a mess.
--moreLWord :: [Exp] -> Parser LWord
--moreLWord es = lword =<< (:es) <$> (lexp <* expSpaces) where
--  lexp = choice [Lit . T.concat <$> llits
--                ,char '$' *> dollarExp]
--  llitExp = litExp " " [' ', '?', '!', '+', '=', '\t']
--  llits = (:) <$> llitExp
--              <*> many (do notFollowedBy binder
--                           append <$> fmap T.singleton (oneOf ['?', '!', '+']) <*> option "" llitExp)

--lword :: [Exp] -> Parser LWord
--lword es = choice [LVarDecl <$> projectVarName es <*> binder
--                  ,LRuleDecl es <$ char ':'
--                  ,moreLWord es <|> (LExp <$> projectExp es)]
--  where projectVarName = projectHead "variable name without space literals"
--        projectExp = projectHead "single top-level expression"
--        projectHead msg es = do (guard (length es == 1) <?> msg)
--                                return (head es)
--{-# INLINE lword #-}

llit :: Parser Text
llit = T.concat <$> some (binderChar <|> litExp " " [' ', '\t', '?', '!', '+', '='])
  where binderChar = notFollowedBy binder *> (T.singleton <$> oneOf ['?', '!', '+'])

lexp :: Parser Exp
lexp = choice [Lit <$> llit
              ,char '$' *> dollarExp]

lword :: [Exp] -> Parser LWord
lword es = do e <- foldr1 cat <$> some lexp
              expSpaces
              choice [LVarDecl <$> projectVarName (e:es) <*> binder -- FIXME die if es is non-empty
                     ,LRuleDecl (e:es) <$ char ':'
                     ,lword (e:es)
                     ,LExp <$> projectExp (e:es)]
  where projectVarName = projectHead "a variable name without spaces"
        projectExp = projectHead "a single top-level expression"
        projectHead :: String -> [Exp] -> Parser Exp
        projectHead msg es = do (guard (length es == 1) <?> msg)
                                return (head es)

parseLWord :: Parser LWord
parseLWord = lword []

optionalStmtInnerExp :: [Char] -> Parser Exp
optionalStmtInnerExp = option (Lit "") . stmtInnerExp

-- Make allows literal right-parentheses as long as they are balanced with some
-- opening left-parentheses *except* for final arguments which seems to allow
-- arbitrary parentheses and the longest match is used.
expArg :: [Char] -> Parser Exp
expArg tchars = do e <- optionalStmtInnerExp ('(':tchars)
                   (cat e <$> catParens (expArg tchars)) <|> pure e
  where catParens p = do t <- parens p
                         cat ("(" `catl` t `catr` ")") <$> p

expArgs :: [Char] -> Natural -> Parser [Exp]
expArgs tchars 0 = pure []
expArgs tchars 1 = return <$> expArg tchars
expArgs tchars n = (:) <$> expArg (',':tchars) <*> args (pred n) where
  args :: Natural -> Parser [Exp]
  args 1 = return <$> (char ',' *> optionalStmtInnerExp tchars)
  args n = (:) <$> (char ',' *> expArg (',':tchars)) <*> args (pred n)

expVarArgs :: [Char] -> Parser [Exp]
expVarArgs tchars = optionalStmtInnerExp (',':tchars) `sepBy` char ','

collapseContLines :: Parser a -> Parser (NonEmpty a)
collapseContLines p = (:|) <$> p <*> many (lineContinuation *> p)

binder :: Parser Binder
binder = choice [char '='   *> pure DeferredBinder
                ,chunk ":=" *> pure ImmediateBinder
                ,chunk "!=" *> pure ShellBinder
                ,chunk "?=" *> pure DefaultValueBinder
                ,chunk "+=" *> pure AppendBinder]

parseBinding :: Bool -> Exp -> Parser Stmt
parseBinding override e = Bind override <$> (binder <* expSpaces) <*> pure e <*> option (Lit "") rexp

expSpaces :: Parser Text
expSpaces = foldr1 (\s1 s2 -> s1 `append` " " `append` s2) <$> collapseContLines spaces
  where spaces :: Parser Text
        spaces = takeWhileP (Just "space") (`elem` [' ', '\t'])

parseRule :: [Exp] -> Parser Rule
parseRule es = Rule es <$> (optional parseDependencies <* eol) <*> parseRecipe

-- FIXME double-colon rules?
--parseRuleOrVarDecl :: Bool -> Exp -> Parser TopLevel
--parseRuleOrVarDecl override e = do
--  lspaces <- expSpaces
--  choice [Stmt <$> parseBinding override e
--         ,RuleDecl <$> (char ':' *> parseRule ruleName)]
--  where ruleName = if override then "override " `catl` e else e

-- FIXME when does evaluation occur here?
parseDependencies :: Parser Exp
parseDependencies = rexp

emptyLine :: Parser (Maybe a)
emptyLine = Nothing <$ (void eol <|> skipLineComment "#" <* option () (void eol))

recipeLineLeadingWhite :: Parser ()
recipeLineLeadingWhite = void (collapseContLines leadingWhite)
  where leadingWhite = takeWhileP (Just "recipe leading whitespace") (`elem` [' ', '\t'])

recipeLine :: Parser (Maybe Exp)
recipeLine = char '\t' *> recipeLineLeadingWhite *> (nonEmptyRecipeLine <|> emptyLine)
  where nonEmptyRecipeLine = Just <$> recipeInnerExp []

parseRecipe :: Parser Recipe
parseRecipe = Recipe . catMaybes <$> many (recipeLine <|> blankLine)
  where blankLine = recipeLineLeadingWhite *> emptyLine

includeExp :: Parser Exp
includeExp = try (chunk "include" *> expSpaces *> rexp <* (void (char '\n') <|> eof))

eatLine :: Parser ()
eatLine = void (expSpaces >> emptyLine)

ifPred :: Parser IfPred
ifPred = unaryIfPred <|> binIfPred
  where unaryIfPred = choice [DefinedPred <$ try (chunk "ifdef" >> expSpaces)
                             ,NotDefinedPred <$ try (chunk "ifndef" >> expSpaces)]
                        <*> option (Lit "") (stmtInnerExp [])
        binIfPred = choice [EqPred <$ try (chunk "ifeq" >> expSpaces >> char '(')
                           ,NeqPred <$ try (chunk "ifneq" >> expSpaces >> char '(')]
                      <*> (option (Lit "") (stmtInnerExp [',']) <* char ',')
                      <*> (option (Lit "") (stmtInnerExp [')']) <* char ')')

guardedTopLevel :: Parser a -> Parser Program
guardedTopLevel p = catMaybes <$> many (notFollowedBy p *> expSpaces *> topLevel)
  where topLevel = (Just <$> parseTopLevel) <|> emptyLine

ifStmt :: Parser Stmt
ifStmt = do
  p <- ifPred
  ss1 <- nonElseEndIfTopLevels
  ss2 <- choice [elseLine *> nonEndIfTopLevels <* endIfLine
                ,endIfLine *> pure []]
  return (IfStmt p ss1 ss2)
  where elseLine = expSpaces *> chunk "else" *> eatLine
        endIfLine = expSpaces *> chunk "endif" *> eatLine
        nonElseEndIfTopLevels = guardedTopLevel (expSpaces *> (chunk "else" <|> chunk "endif") *> eatLine)
        nonEndIfTopLevels = guardedTopLevel (expSpaces *> chunk "endif" *> eatLine)

-- FIXME add export
decl :: Bool -> Parser TopLevel
decl override = do
  l <- parseLWord
  case l of
    LVarDecl e b -> Stmt . Bind override b e <$> (expSpaces *> option (Lit "") rexp)
    LRuleDecl es -> RuleDecl <$> parseRule (if override then Lit "override":es else es)
    LExp e -> return (Stmt (SExp e))

-- FIXME double-colon rules?
-- FIXME Factor into directive <|> decl.
-- Also things like keywords followed by a \\n are probably broken everywhere (and in builtin parsing)
parseTopLevel :: Parser TopLevel
parseTopLevel = choice [Include <$> includeExp
                       ,Stmt <$> ifStmt
                       ,override
                       ,decl False]
  where override = expSpaces *> chunk "override" *> expSpaces *> decl True

makefile :: Parser Program
makefile = catMaybes <$> many ((Just <$> parseTopLevel) <|> emptyLine)
