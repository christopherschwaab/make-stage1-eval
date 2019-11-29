{-#LANGUAGE GADTs #-}
module Eval where

import Syntax

import Control.Monad.State.Lazy
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text, append, intercalate, pack, uncons, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Shelly as Sh
import System.FilePath (dropExtensions, dropFileName, takeFileName)
import System.FilePath.Glob (compileWith, compPosix, globDir1)
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
evalBuiltin IfExp (e1, e2, e3) = do Value p <- evalExp e1
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

--evalStmt :: Stmt -> Interpreter ()
--evalStmt (Bind override b e1 e2) = do -- FIXME override
--  Value x <- evalExp e1
--  case b of
--    DeferredBinder -> modify (\st -> st{env=Map.insert x (Deferred e2) (env st)})
--    ImmediateBinder -> do v <- evalExp e2
--                          modify (\st -> st{env=Map.insert x (Immediate v) (env st)})
--    DefaultValueBinder -> do
--      st <- get
--      let p = env st
--      p' <- case Map.lookup x p of
--        Just _ -> return p
--        Nothing -> do v <- evalBuiltin Shell e2
--                      return (Map.insert x (Immediate v) p) -- is this immediate or deferred?
--      put (st{env=p'})
--    ShellBinder -> do v <- evalBuiltin Shell e2
--                      modify (\st -> st{env=Map.insert x (Immediate v) (env st)})
--    AppendBinder -> do
--      st <- get
--      let p = env st
--      p' <- case Map.lookup x p of
--        Just (Immediate (Value v')) -> do
--          Value v <- evalExp e2
--          return (Map.insert x (Immediate (Value (v' `append` " " `append` v))) p)
--        Just (Deferred e') -> return (Map.insert x (Deferred (e' `cat` Lit " " `cat` e2)) p)
--        Nothing -> undefined -- what does make do in this case?
--      put (st{env=p'})
--evalStmt (SExp e) = () <$ evalExp e
--
--evalDirective :: Directive s -> Interpreter s
--evalDirective (VPath Nothing) = undefined -- FIXME
--evalDirective (VPath (Just (Left e))) = undefined -- FIXME
--evalDirective (VPath (Just (Right (e1, e2)))) = undefined -- FIXME
--evalDirective (If pred ss1 ss2) = do
--  b <- evalIfPred pred
--  return (if b then ss1 else ss2)
--  where evalIfPred (EqPred e1 e2) = (==) <$> fmap fromValue (evalExp e1) <*> fmap fromValue (evalExp e2)
--        evalIfPred (NeqPred e1 e2) = (/=) <$> fmap fromValue (evalExp e1) <*> fmap fromValue (evalExp e2)
--        evalIfPred (DefinedPred e) = flip Map.member <$> fmap env get <*> fmap fromValue (evalExp e)
--        evalIfPred (NotDefinedPred e) = flip Map.member <$> fmap env get <*> fmap fromValue (evalExp e)
--
--evalRecipeLine :: RecipeLine -> Interpreter [Exp]
--evalRecipeLine (RExp e) = return [e]
--evalRecipeLine (RDirective d) = do rs <- evalDirective d
--                                   concat <$> mapM evalRecipeLine rs
--
---- FIXME double check the order things get evaluated here
--evalRule :: Rule -> Interpreter ()
--evalRule (Rule ts d (Recipe es)) = do
--  vts <- mapM (fmap fromValue . evalExp) ts
--  deps <- maybe (pure []) (fmap (T.lines . fromValue) . evalExp) d
--  es' <- concat <$> mapM evalRecipeLine es
--  modify (\st -> st{targets = foldr (addTarget deps es') (targets st) vts}) -- FIXME think make dies on  double defined targets? (except ::)
--  where addTarget deps es' t m = Map.insert t (deps, es') m
--
--evalTopLevelDirective :: TopLevelDirective -> Interpreter Program
--evalTopLevelDirective (Include e) = do
--  f <- unpack . fromValue <$> evalExp e
--  r <- liftIO (parse makefile f <$> TIO.readFile f)
--  case r of
--    Left err -> error (errorBundlePretty err)
--    Right s -> return s
--
--evalTopLevel :: TopLevel -> Interpreter ()
--evalTopLevel (Stmt s) = evalStmt s
--evalTopLevel (Directive d) = evalProgram =<< evalDirective d
--evalTopLevel (TopLevelDirective d) = evalProgram =<< evalTopLevelDirective d
--evalTopLevel (RuleDecl r) = evalRule r
--
--evalProgram :: Program -> Interpreter ()
--evalProgram = mapM_ evalTopLevel
--
--run :: Program -> IO EvalState
--run p = execStateT (evalProgram p) (EvalState{env=Map.empty, targets=Map.empty})
