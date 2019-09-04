{-#LANGUAGE OverloadedStrings, TypeOperators, ViewPatterns #-}
module Main where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Map.Strict (Map)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import Data.Text (Text, append, intercalate, pack, uncons, unpack)
import qualified Data.Text as T
import Data.Traversable (sequence)
import qualified Shelly as Sh
import Data.Void (Void)
import System.FilePath.Glob (compileWith, compPosix, globDir1)
import Text.Megaparsec
import TextShow

type Parser = Parsec Text Void

type Name = Text

data Exp
  = Foreach Exp Exp Exp -- Collapses whitespace
  | Filter Exp Exp -- collapses whitespace?
  | FilterOut Exp Exp -- collapses whitespace?
  | If Exp Exp Exp -- collapses whitespace?
  | Shell Exp -- Collapses whitespace
  | Lit Text
  | Var Exp
  | Cat Exp Exp
  | Wildcard Exp
  | Patsubst Exp Exp Exp -- Collapses whitespace?
  | Varsubst Exp Exp Exp -- Collapses whitespace
  | Call Exp Exp -- Collapses whitespace
  | Error Exp
  deriving Show

data Stmt
  = BindDeferred Name Exp
  | Name := Exp
  | Name :+= Exp
  | Name :!= Exp
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
  args <- T.lines . fromValue <$> evalExp e2
  withStateT (\st ->
    EvalState{env=foldr (\(n, arg) p -> Map.insert (showt n) (Immediate (Value arg)) p)
                        (env st)
                        (zip [(1::Int)..] args)})
    (evalExp e1)
evalExp (Error e) = error . unpack . fromValue <$> evalExp e

evalStmt :: Stmt -> Interpreter ()
evalStmt (BindDeferred x e) = modify (\st -> EvalState {env=Map.insert x (Deferred e) (env st)})
evalStmt (x := e) = do v <- evalExp e
                       modify (\st -> EvalState {env=Map.insert x (Immediate v) (env st)})
evalStmt (x :+= e) = do
  p <- env <$> get
  p' <- case Map.lookup x p of
          Just (Immediate (Value v')) -> do
            Value v <- evalExp e
            return (Map.insert x (Immediate (Value (v' `append` " " `append` v))) p)
          Just (Deferred e') -> return (Map.insert x (Deferred (e' `Cat` Lit " " `Cat` e)) p)
          Nothing -> undefined -- what does make do in this case?
  put (EvalState{env=p'})
evalStmt (x :!= e) = do v <- evalExp (Shell e)
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

main :: IO ()
main = print =<< (run $
  ["x" := Lit "xvalue"
  ,"xvalue" := Lit "pig"
  ,"z" := Var (Lit "x")
  ,SExp (Shell (Lit "echo " `Cat` Wildcard (Lit "*/*.cache")))
  ,"y" := Shell (Lit "ls -1")
  ,"q" := Patsubst (Lit "%.c") (Lit "%.o") (Lit "a.c b.c c.c.c a.h")
  ,"q'" := Varsubst (Lit "q") (Lit ".o") (Lit ".c")])
