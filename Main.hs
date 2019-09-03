{-#LANGUAGE OverloadedStrings, TypeOperators, ViewPatterns #-}
module Main where

import Prelude hiding (lines, words)

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Map.Strict (Map)
import Data.Traversable (sequence)
import qualified Data.Map.Strict as Map
import Data.Text (Text, append, intercalate, lines, pack, replace, uncons, unpack, words)
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
  | Shell Exp -- Collapses whitespace
  | Lit Text
  | Var Exp
  | Cat Exp Exp
  | Wildcard Exp
  -- | Patsubst Exp Exp Exp
  | Varsubst Name Exp Exp -- Collapses whitespace
  | Call Exp Exp -- Collapses whitespace
  deriving Show

data Stmt
  = Name := Exp
  | Name :+= Exp
  | Name :!= Exp
  | SExp Exp
  deriving Show

type Program = [Stmt]

newtype Value = Value { fromValue :: Text }
  deriving Show
type Env = Map Name Value
data EvalState = EvalState
  { env :: Env }
  deriving Show

type Interpreter = StateT EvalState IO

splitFields :: Text -> [Text]
splitFields = words

matchWild :: Bool -> Text -> Text -> (Char -> Text -> Bool) -> Bool
matchWild wildMatchesEmpty pat t backtrack = case (uncons pat, uncons t) of
  (Just ('%', pat'), yt'@(Just (y, t'))) -> longestMatch (uncons pat') y t' where
    longestMatch p@(Just ('%', pat'')) y t' = y == '%'
                                           && matchWild wildMatchesEmpty pat'' t' (longestMatch p)
    longestMatch (Just (x,   pat''))   y t' = anyTill x pat'' (Just (y, t'))
    longestMatch Nothing               _ _  = True
    anyTill x pat'' (Just (y, t'))
      | x == y    = matchWild wildMatchesEmpty pat'' t' (longestMatch (Just (x, pat'')))
      | otherwise = anyTill x pat'' (uncons t')
    anyTill x pat'' Nothing = False
  (Just ('%', _),  Nothing) -> wildMatchesEmpty
  (Just (x, pat'), Just (y, t')) -> if x == y then matchWild wildMatchesEmpty pat' t' backtrack
                                      else backtrack y t'
  (Just (x, _),    Nothing) -> False
  (Nothing,        Just _)  -> False
  (Nothing,        Nothing) -> True

wildcard :: Text -> Text -> Bool
wildcard pat t = matchWild True pat t (\_ _ -> False)

-- FIXME Whitespace preservation/collapse is surely wrong
evalExp :: Exp -> Interpreter Value
evalExp (Foreach e1 e2 e3) = do
  Value x <- evalExp e1
  ws <- splitFields . fromValue <$> evalExp e3
  ws' <- mapM (\w -> withStateT (\st -> EvalState{env=Map.insert x (Value w) (env st)})
                                (evalExp e2))
              ws
  return (Value (intercalate " " (map fromValue ws')))
evalExp (Filter e1 e2) = do pat <- fromValue <$> evalExp e1
                            ws <- splitFields . fromValue <$> evalExp e2
                            let ws' = filter (wildcard pat) ws
                            return (Value (intercalate " " ws'))
evalExp (Shell e) = do Value t <- evalExp e
                       (exitStatus, out) <- lift (Sh.shelly . Sh.silently $ do
                         out <- Sh.run "/bin/sh" ["-c", t]
                         exitStatus <- Sh.lastExitCode
                         return (Value (showt exitStatus), replace "\n" " " out))
                       modify (\st -> EvalState{env=Map.insert ".SHELLOUT" exitStatus (env st)})
                       return (Value out)
evalExp (Lit t) = return (Value t)
evalExp (Var e) = do Value x <- evalExp e
                     p <- env <$> get
                     case Map.lookup x p of
                       Just v -> return v
                       Nothing -> undefined
evalExp (Cat e1 e2) = Value <$> (append <$> (fromValue <$> evalExp e1)
                                        <*> (fromValue <$> evalExp e2))
evalExp (Wildcard e) = do Value pattern <- evalExp e
                          lift (spaceSeparate . map pack <$> globInCwd (unpack pattern))
  where spaceSeparate = Value . intercalate " "
        globInCwd p = globDir1 (compileWith compPosix p) ""
evalExp (Varsubst x e1 e2) = Value <$> (replace <$> (fromValue <$> evalExp e1)
                                                <*> (fromValue <$> evalExp e2)
                                                <*> undefined) -- lookup x in env
evalExp (Call e1 e2) = do
  args <- lines . fromValue <$> evalExp e2
  withStateT (\st ->
    EvalState{env=foldr (\(n, arg) p -> Map.insert (showt n) (Value arg) p)
                        (env st)
                        (zip [(1::Int)..] args)})
    (evalExp e1)

evalStmt :: Stmt -> Interpreter ()
evalStmt (x := e) = do v <- evalExp e
                       modify (\st -> EvalState {env=Map.insert x v (env st)})
evalStmt (x :+= e) = do v' <- evalExp e
                        modify (\st -> EvalState {env=Map.alter (Just . insertOrAppend v') x (env st)})
  where insertOrAppend v' (Just v) = Value (fromValue v `append` " " `append` fromValue v')
        insertOrAppend v' Nothing  = v'
evalStmt (x :!= e) = do v <- evalExp (Shell e)
                        modify (\st -> EvalState {env=Map.insert x v (env st)})
evalStmt (SExp e) = () <$ evalExp e

run :: Program -> IO EvalState
run p = execStateT (mapM_ evalStmt p) (EvalState{env=Map.empty})

main :: IO ()
main = print =<< (run $
  ["x" := Lit "xvalue"
  ,"xvalue" := Lit "pig"
  ,"z" := Var (Lit "x")
  ,SExp (Shell (Lit "echo " `Cat` Wildcard (Lit "*/*.cache")))
  ,"y" := Shell (Lit "ls -1")])
