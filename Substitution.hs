module Substitution where

import Type
import Variables

type Subst = [(Term, Term)]

{- outputs the domain of a substitution
-}
domain :: Subst -> [VarName]
domain s = concatMap allVars (right s)
  where
  right :: [(a,a)] -> [a]
  right []        = []
  right ((x,y):r) = y : right r

{- Creates a empty substitution
-}
empty :: Subst
empty = []

{- Creates a substitution that maps a single variable to a term
-}
single :: VarName -> Term -> Subst
single v t = [(Var v, t)]

{- Applys a substitution to a given term
-}
apply :: Subst -> Term -> Term
apply []            t          = t
apply ((Var x,y):s) (Var v)    = if x == v then y else apply s (Var v)
apply s             (Comb n t) = Comb n [apply s x| x <- t]