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
apply (Subs [])  t                = t
apply (Subs ((x,y):s)) (Var v)    = if x == v then y else apply (Subs s) (Var v)
apply s             (Comb n t)    = Comb n [apply s x| x <- t]

compose :: Subst -> Subst -> Subst
compose s1    Empty             = s1
compose Empty s2                = s2
compose (Subs [])     s2        = Empty
compose s1    (Subs [])         = Empty
compose (Subs (y:ys)) s2 = concatSubs (composeHelp (Subs [y]) s2) (compose (Subs ys) s2)
  where
    composeHelp :: Subst -> Subst -> Subst
    composeHelp (Subs [(v, t)]) (Subs ((v2, t2):ys))
     | v == v2     = concatSubs (single v2 (apply (Subs [(v, t)]) t2)) (compose (Subs [(v, t)]) (Subs ys))
     | t == Var v2 = concatSubs (single v (apply (Subs [(v, t)]) t2)) (compose (Subs [(v, t)]) (Subs ys))
     | otherwise   = concatSubs (compose (single v2 (apply (Subs [(v, t)]) t2)) (compose (Subs [(v, t)]) (Subs ys))) (single v t)

concatSubs :: Subst -> Subst -> Subst
concatSubs Empty x           = x
concatSubs x     Empty       = x
concatSubs (Subs x) (Subs y) = Subs (x ++ y)

{- Restricts the domain of a substitution to a given set of variables
-}
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subs []) _        = Empty
restrictTo (Subs ((x,y):s)) n = if x `elem` n then  compose (single x y) (restrictTo (Subs s) n) else restrictTo (Subs s) n
