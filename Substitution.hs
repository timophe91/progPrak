module Substitution where

import Type
import Variables

type Subst = [(VarName , Term)]

{- outputs the domain of a substitution
-}
domain :: Subst -> [VarName]
domain s = left s
  where
  left :: Subst -> [VarName]
  left []        = []
  left ((x,y):l) = if x /= y then x : left l else left l

{- Creates a empty substitution
-}
empty :: Subst
empty = []

{- Creates a substitution that maps a single variable to a term
-}
single :: VarName -> Term -> Subst
single v t = [(v, t)]

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