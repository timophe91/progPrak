module Substitution where

import Type
import Variables

--type Subst = [(VarName , Term)]
data Subst = Empty | Subs [(VarName, Term)]
  deriving (Eq, Show)

{- outputs the domain of a substitution
TODO: abbildung auf sich selbst müssen nicht mehr untersucht werden
-}
domain :: Subst -> [VarName]
domain Empty                = []
domain (Subs ((x,Var v):s)) = if x /= v then x : domain (Subs s) else domain (Subs s)
domain (Subs ((x,_):s))     = x : domain (Subs s)

{- Creates a empty substitution
-}
empty :: Subst
empty = Empty

{- Creates a substitution that maps a single variable to a term
 - substitutions on self r empty
-}
single :: VarName -> Term -> Subst
single v (Var v2) = if v == v2 then empty else Subs [(v, Var v2)]
single v t        = Subs [(v, t)]

{- Applys a substitution to a given term
-}
apply :: Subst -> Term -> Term
apply Empty      t                = t
apply (Subs ((x,y):s)) (Var v)    = if x == v then y else apply (Subs s) (Var v)
apply s             (Comb n t)    = Comb n [apply s x| x <- t]

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
restrictTo (Subs []) _        = Empty
restrictTo (Subs ((x,y):s)) n = if x `elem` n then  compose (single x,y) restrictTo s n else restrictTo (Subs s) n
