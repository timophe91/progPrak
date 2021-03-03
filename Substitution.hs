{-# LANGUAGE TemplateHaskell #-}
module Substitution where

import Type
import Test.QuickCheck

data Subst = Empty | Subs [(VarName, Term)]
  deriving (Eq, Show)

{- outputs the domain of a substitution
-}
domain :: Subst -> [VarName]
domain Empty                = []
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

{- Merge two Prolog Substitutions
-}
compose :: Subst -> Subst -> Subst
compose s1    Empty             = s1
compose Empty s2                = s2
compose (Subs [])     s2        = Empty
compose s1    (Subs [])         = Empty
compose (Subs (su:sus)) s2 = concatSubs (composeHelp (Subs [su]) s2) (compose (Subs sus) s2)
  where
    composeHelp :: Subst -> Subst -> Subst
    composeHelp (Subs [(v, t)]) (Subs ((v2, t2):ys))
     | v == v2     = concatSubs (compose (Subs [(v, t)]) (Subs ys)) (single v2 (apply (Subs [(v, t)]) t2)) -- if two VarName r the same: A -> B, A -> C
     | t == Var v2 = concatSubs (compose (Subs [(v, t)]) (Subs ys)) (single v  t2) -- if u have A -> B , B -> C
     | otherwise   = concatSubs (concatSubs (single v t) (single v2 (apply (Subs [(v, t)]) t2))) (compose (Subs [(v, t)]) (Subs ys)) -- every other case

{- Concatenate two Subst and removing duplicates
-}
concatSubs :: Subst -> Subst -> Subst
concatSubs Empty    x           = x
concatSubs x        Empty       = x
concatSubs (Subs x) (Subs y)    = Subs (removeDups (x ++ y))
  where 
    removeDups []     = []
    removeDups (x:xs) = if x `elem` xs then removeDups xs else x : removeDups xs


{- Restricts the domain of a substitution to a given set of variables
-}
restrictTo :: Subst -> [VarName] -> Subst
restrictTo _         []       = Empty
restrictTo Empty     _        = Empty
restrictTo (Subs []) _        = Empty
restrictTo (Subs ((x,y):s)) n = if x `elem` n then  compose (single x y) (restrictTo (Subs s) n) else restrictTo (Subs s) n


instance Arbitrary Subst where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [return Empty, return (Subs [(v,t)| v <- x, t <- y])]

prop_ApplyEmpty :: Term -> Bool
prop_ApplyEmpty t = t == apply Empty t

prop_ApplySingle :: VarName -> Term -> Bool
prop_ApplySingle v t = apply (single v t) (Var v) == t

--prop_ApplyCompose :: Subst -> Subst -> Term -> Bool 
--prop_ApplyCompose s1 s2 t = apply (compose s1 s2) t == apply s1 (apply s2 t)

prop_DomainEmpty :: Bool
prop_DomainEmpty = null (domain Empty)

--prop_domainrestrict :: Subst -> [VarName] -> Bool
--prop_domainrestrict s v = domain(restrictTo s v) == v

-- Check all properties in this module:
return []
testAll = $quickCheckAll