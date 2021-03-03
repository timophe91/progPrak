module Substitution where

import Type

--type Subst = [(VarName , Term)] kann weg
data Subst = Empty | Subs [(VarName, Term)]
  deriving (Eq, Show)

{- outputs the domain of a substitution
TODO: abbildung auf sich selbst müssen nicht mehr untersucht werden
-}
domain :: Subst -> [VarName]
domain Empty                = []
--domain (Subs ((x,Var v):s)) = if x /= v then x : domain (Subs s) else domain (Subs s) kann weg
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

compose :: Subst -> Subst -> Subst
compose s1    Empty             = s1
compose Empty s2                = s2
compose x     (Subs ((v,t):ys)) = concatSingle (help x v t) (compose x (Subs ys))
  where
  varNames :: Subst -> [VarName]
  varNames Empty    = []
  varNames (Subs (v:vs)) = fst v : varNames (Subs vs)
  varTerms Empty    = []
  varTerms (Subs (v:vs)) = snd v : varTerms (Subs vs)
  help :: Subst -> VarName -> Term -> Subst
  help x v t = single v (apply x t)

concatSingle :: Subst -> Subst -> Subst
concatSingle Empty x           = x
concatSingle x     Empty       = x
contatSingle (Subs x) (Subs y) = Subs (x ++ y)

{- Restricts the domain of a substitution to a given set of variables
-}
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subs []) _        = Empty
restrictTo (Subs ((x,y):s)) n = if x `elem` n then  compose (single x y) (restrictTo (Subs s) n) else restrictTo (Subs s) n
