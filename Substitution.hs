module Substitution where

import Type
import Variables

type Subst = [(Term, Term)]

domain :: Subst -> [VarName]
domain s = concatMap allVars (right s)
  where
  right :: [(a,a)] -> [a]
  right []        = []
  right ((x,y):r) = y : right r

empty :: Subst
empty = []

single :: VarName -> Term -> Subst
single v t = [(Var v, t)]

apply :: Subst -> Term -> Term
apply []        t = t
apply ((x,y):s) t = if x == t then y else apply s t