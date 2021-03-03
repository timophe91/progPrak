module Substitution where

import Type
import Variables

type Subst = [(VarName , Term)]

{- outputs the domain of a substitution
-}
domain :: Subst -> [VarName]
domain []            = []
domain ((x,Var v):s) = if x /= v then x : domain s else domain s
domain ((x,_):s)     = x : domain s

{- Creates a empty substitution
-}
empty :: Subst
empty = []

{- Creates a substitution that maps a single variable to a term
 - substitutions on self r empty
-}
single :: VarName -> Term -> Subst
single v (Var v2) = if v == v2 then empty else [(v, Var v2)]
single v t        = [(v, t)]

{- Applys a substitution to a given term
-}
apply :: Subst -> Term -> Term
apply []            t          = t
apply ((x,y):s) (Var v)    = if x == v then y else apply s (Var v)
apply s             (Comb n t) = Comb n [apply s x| x <- t]

{-compose :: Subst -> Subst -> Subst
compose s1 [] = s1
compose [] s2 = s2
compose x  y  = 
  where
  help (x1,y1) []           = []
  help (x1,y1) ((x2,y2):s2) = if y2 == x1 then (x2,y1) : help (x1,y2) s2 else (x2,y2) : help (x1,y1) s2-}

{- Restricts the domain of a substitution to a given set of variables
-}
restrictTo :: Subst -> [VarName] -> Subst
restrictTo []        _ = []
restrictTo ((x,y):s) n = if x `elem` n then (x,y) : restrictTo s n else restrictTo s n