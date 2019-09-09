module Main where

import Prelude hiding (lex)

import Parser
import Syntax

main :: IO ()
main = print =<< (run $
  [Lit "x" := Lit "xvalue"
  ,Lit "xvalue" := Lit "pig"
  ,Lit "z" := Var (Lit "x")
  ,SExp (Shell (Lit "echo " `Cat` Wildcard (Lit "*/*.cache")))
  ,Lit "y" := Shell (Lit "ls -1")
  ,Lit "q" := Patsubst (Lit "%.c") (Lit "%.o") (Lit "a.c b.c c.c.c a.h")
  ,Lit "q'" := Varsubst (Lit "q") (Lit ".o") (Lit ".c")])
