{-#LANGUAGE GADTs, FlexibleInstances #-}
module Parser where

import Syntax

import Prelude hiding (lex)

import Control.Applicative (pure, (<*>), (*>), (<*), (<$))
import Control.Applicative.Combinators (between)
import Control.Monad (guard, void)
import Data.List.NonEmpty (NonEmpty, NonEmpty(..), toList)
import Data.Maybe (catMaybes)
import Data.Text (Text, append)
import qualified Data.Text as T
import Data.Void (Void)
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (skipLineComment)

type Parser = Parsec Void Text

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

optionalTExp :: [Char] -> Parser Exp
optionalTExp = option (Lit "") . termExp

-- Make allows literal right-parentheses as long as they are balanced with some
-- opening left-parentheses *except* for final arguments which seems to allow
-- arbitrary parentheses and the longest match is used.
expArg :: [Char] -> Parser Exp
expArg tchars = do e <- optionalTExp ('(':tchars)
                   (cat e <$> catParens (expArg tchars)) <|> pure e
  where catParens p = do t <- parens p
                         cat ("(" `catl` t `catr` ")") <$> p

expArgs :: [Char] -> Natural -> Parser [Exp]
expArgs tchars 0 = pure []
expArgs tchars 1 = return <$> expArg tchars
expArgs tchars n = (:) <$> expArg (',':tchars) <*> args (pred n) where
  args :: Natural -> Parser [Exp]
  args 1 = return <$> (char ',' *> optionalTExp tchars)
  args n = (:) <$> (char ',' *> expArg (',':tchars)) <*> args (pred n)

expVarArgs :: [Char] -> Parser [Exp]
expVarArgs tchars = optionalTExp (',':tchars) `sepBy` char ','

ifArgs :: [Char] -> Parser (Exp, Exp, Maybe Exp)
ifArgs tchars = (,,) <$> expArg (',':tchars)
                     <*> (char ',' *> expArg (',':tchars))
                     <*> optional (char ',' *> expArg tchars)

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

lineContinuation :: Parser Text
lineContinuation = " " <$ (char '\\' >> eol) -- Is this even correct? What does make consider a line end?

builtin :: [Char] -> Parser PAppliedBuiltin
builtin tchars = choice builtins where
  -- FIXME does reserved eat all following whitespace or just the first?
  reserved x = try (chunk x >> choice [void lineContinuation, void (char ' '), void (char '\t')] )
  builtins = [reserved "foreach" >> PApp . App Foreach <$> parseBuiltinArgs tchars
             ,reserved "filter" >> PApp . App Filter <$> parseBuiltinArgs tchars
             ,reserved "filter-out" >> PApp . App FilterOut <$> parseBuiltinArgs tchars
             ,reserved "if" >> PApp . App IfExp <$> ifArgs tchars
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
                            l <- termExp ('=':tchars)
                            varsubst l <|> pure (e `catr` ":" `cat` l)
  where varsubst l = Varsubst e l <$> (char '=' *> termExp tchars)

escaped :: Text -> Parser Text
escaped nl = choice [char '#'   *> pure "#"
                    ,char '\\'  *> pure "\\"
                    ,char '\n'  *> pure nl]

dollarExp :: Parser Exp
dollarExp = choice [parens (builtinCall ')' <|> builtinOrVar <$> termExp [':', ')'])
                   ,braces (builtinCall '}' <|> builtinOrVar <$> termExp [':', '}'])
                   ,Var . Lit <$> singleCharVar]
  where singleCharVar = (char '\\' *> escaped " ")
                    <|> option "" (T.singleton <$> anySingleBut '\n')
        builtinCall = fmap Builtin . builtin . return
        builtinOrVar e@(Varsubst _ _ _) = e
        builtinOrVar e = Var e

litExp :: Text -> [Char] -> Parser Text
litExp nl tchars = foldr1 append <$> some (do
  let nonLitChar = ['\\', '$', '\n', '#'] ++ tchars
  l <- takeWhileP (Just "literal character") (\c -> not (c `elem` nonLitChar))
  choice [l `append` nl <$ try lineContinuation
         ,l `append` "$" <$ chunk "$$"
         ,append l <$> (char '\\' *> option "\\" (escaped nl))
         ,l <$ guard (not (T.null l))])

expr :: Text -> [Char] -> Parser Exp
expr nl tchars = foldr1 cat <$> some expr' where
  expr' :: Parser Exp
  expr' = do e <- choice [Lit <$> litExp nl tchars
                         ,char '$' *> dollarExp]
             option e (varsubstOrLit tchars e)

termExp = expr " "
ruleExp = expr "\n"

rexp :: Parser Exp
rexp = termExp []

collapseContLines :: Parser a -> Parser (NonEmpty a)
collapseContLines p = (:|) <$> p <*> many (lineContinuation *> p)

expSpaces :: Parser Text
expSpaces = foldr1 (\s1 s2 -> s1 `append` " " `append` s2) <$> collapseContLines spaces
  where spaces :: Parser Text
        spaces = takeWhileP (Just "space") (`elem` [' ', '\t'])

vpathDirective :: Parser (Directive s)
vpathDirective = VPath <$> try (chunk "vpath" *> expSpaces *> optional vpathArgs)
  where vpathArgs = do l <- rexp
                       maybe (Left l) (\r -> Right (l, r)) <$> (expSpaces *> optional rexp)

includeDirective :: Parser (Directive s)
includeDirective = Include <$> try (chunk "include" *> expSpaces *> rexp)

emptyLine :: Parser (Maybe a)
emptyLine = Nothing <$ choice [void eol
                              ,skipLineComment "#" <* option () (void eol)]

eatLine :: Parser ()
eatLine = expSpaces >> try (void emptyLine <|> eof)

guardedDStmt :: Parser s -> Parser a -> Parser [s]
guardedDStmt dstmt p = catMaybes <$> many (notFollowedBy p *> expSpaces *> d)
  where d = (Just <$> dstmt) <|> try emptyLine

ifPred :: Parser IfPred
ifPred = unaryIfPred <|> binIfPred
  where unaryIfPred = choice [DefinedPred <$ try (chunk "ifdef" >> expSpaces)
                             ,NotDefinedPred <$ try (chunk "ifndef" >> expSpaces)]
                        <*> option (Lit "") (termExp [])
        binIfPred = choice [EqPred <$ try (chunk "ifeq" >> expSpaces >> char '(')
                           ,NeqPred <$ try (chunk "ifneq" >> expSpaces >> char '(')]
                      <*> (option (Lit "") (termExp [',']) <* char ',')
                      <*> (option (Lit "") (termExp [')']) <* char ')')

ifDirective :: Parser s -> Parser (Directive [s])
ifDirective dstmt = do
  p <- ifPred
  ss1 <- nonElseEndIfDStmts
  ss2 <- expSpaces *> choice [elseLine *> nonEndIfDStmts <* expSpaces <* endIfLine
                             ,endIfLine *> pure []]
  return (If p ss1 ss2)
  where elseLine = chunk "else" *> eatLine
        endIfLine = chunk "endif" *> eatLine
        nonElseEndIfDStmts = guardedDStmt dstmt (expSpaces *> (chunk "else" <|> chunk "endif") *> eatLine)
        nonEndIfDStmts = guardedDStmt dstmt (expSpaces *> chunk "endif" *> eatLine)

directive :: Parser s -> Parser (Directive [s])
directive dstmt = choice [ifDirective dstmt
                         ,vpathDirective <* eatLine
                         ,includeDirective <* eatLine]

llit :: Parser Text
llit = T.concat <$> some (binderChar <|> litExp " " [' ', '\t', '?', '!', '+', '=', ':'])
  where binderChar = notFollowedBy binder *> (T.singleton <$> oneOf ['?', '!', '+'])

lexp :: Parser Exp
lexp = choice [Lit <$> llit
              ,char '$' *> dollarExp]

binder :: Parser Binder
binder = choice [char '='   *> pure DeferredBinder
                ,chunk ":=" *> pure ImmediateBinder
                ,chunk "!=" *> pure ShellBinder
                ,chunk "?=" *> pure DefaultValueBinder
                ,chunk "+=" *> pure AppendBinder]

data LWord = LVarDecl Binder | LRuleDecl | LExp
  deriving (Eq, Show)

lword :: Parser ([Exp], LWord)
lword = do
  e <- foldr1 cat <$> some lexp
  s <- expSpaces
  (es, l) <- choice [(,) [] . LVarDecl <$> binder
                    ,([], LRuleDecl) <$ char ':'
                    ,lword
                    ,pure ([], LExp)]
  return ((if T.null s then [e] else [e, Lit s]) ++ es, l)

decl :: Bool -> Parser Term
decl override = do
  (es, l) <- lword
  case l of
    LVarDecl b -> (if override then TOverride else TAssign) b (head es) (tail es) <$>
                    expSpaces <*> option (Lit "") rexp
    LRuleDecl -> RuleDecl es <$> (optional rexp <* eol)
    LExp -> return (TExp es)

unindentedTerm :: Parser Term
unindentedTerm = expSpaces *> choice [TDirective <$> directive term
                                     ,override
                                     ,decl False]
  where override = expSpaces *> chunk "override" *> expSpaces *> decl True

term :: Parser Term
term = choice [Indented <$> (char '\t' *> unindentedTerm)
              ,unindentedTerm]

makefile :: Parser Program
makefile = catMaybes <$> many ((Just <$> term) <|> try emptyLine)

