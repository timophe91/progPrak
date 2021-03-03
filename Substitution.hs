{-# LANGUAGE TemplateHaskell #-}
module Substitution where

import Type
import Test.QuickCheck
import Variables
import Data.List

instance Vars Subst where
    allVars Empty    = []
    allVars (Subs v) = removeDups (concatMap (\(v, t) -> v : allVars t) v)

data Subst = Empty | Subs [(VarName, Term)]
  deriving (Eq, Show)



{- outputs the domain of a substitution
-}
domain :: Subst -> [VarName]
domain (Subs [])        = []
domain Empty            = []
domain (Subs ((x,_):s)) = nub (x : domain (Subs s))

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
compose s1              Empty     = s1
compose Empty           s2        = s2
compose (Subs [])       s2        = s2
compose s1              (Subs []) = s1
compose (Subs ((v0, t0):subs0))  (Subs ((v1, t1):subs1)) 
  | (v0 == v1) || (Var v1 `partOfSubs` subs0)     = concatSubs (single v1 t1) (compose (Subs subs0) (Subs subs1)) -- {A-> B} ° {A -> C} => {A -> C}
  | (v0 /= v1) || not (Var v1 `partOfSubs` subs0) = concatSubs (single v1 (apply (Subs ((v0, t0):subs0)) t1)) (compose (Subs ((v0, t0):subs0)) (Subs subs1)) -- (v1, t1) NICHT in s1 enthalten
  | (v0 /= v1) || not (Var v0 `partOfSubs` subs1) = concatSubs (single v0 (apply (Subs ((v1, t1):subs1)) t0)) (compose (Subs subs0) (Subs ((v1, t1):subs1))) -- (v0, t0) NICHT in s2
  | Var v1 == t0                              = concatSubs (single v0 t1) (compose (Subs subs0) (Subs subs1)) -- {A-> B} ° {B -> C} => {A -> C}
  | t0 `partOfSubs` subs1                     = concatSubs (single v0 (apply (Subs ((v0, t0):subs0)) (firstAppearance t0 subs1))) (compose (Subs subs0) (Subs ((v1, t1):subs1)))
  | otherwise                                 = compose (Subs subs0) (Subs subs1)

firstAppearance :: Term -> [(VarName, Term)] -> Term
firstAppearance t ((v, t1):r) = if t == Var v then t1 else firstAppearance t r


partOfSubs :: Term -> [(VarName, Term)] -> Bool
partOfSubs _ []          = False 
partOfSubs v ((vs, _):r) = (v == (Var vs)) || partOfSubs v r

{- Concatenate two Subst and removing duplicates
-}
concatSubs :: Subst -> Subst -> Subst
concatSubs Empty    x           = x
concatSubs x        Empty       = x
concatSubs (Subs x) (Subs y)    = Subs (x ++ y)


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
    oneof [return Empty, return (Subs (nub [(v,t)| v <- nub x, t <- y]))]

prop_ApplyEmpty :: Term -> Bool
prop_ApplyEmpty t = t == apply Empty t

prop_ApplySingle :: VarName -> Term -> Bool
prop_ApplySingle v t = apply (single v t) (Var v) == t

prop_ApplyCompose :: Subst -> Subst -> Term -> Bool 
prop_ApplyCompose s1 s2 t = termEquals (apply (compose s1 s2) t)  (apply s1 (apply s2 t))

prop_DomainEmpty :: Bool
prop_DomainEmpty = null (domain Empty)

prop_DomainSingleSelfReference :: VarName -> Bool 
prop_DomainSingleSelfReference x = null (domain (single x (Var x)))

prop_DomainSingle :: VarName -> Term -> Property  
prop_DomainSingle v t = t /= Var v ==> domain (single v t) == [v]

--Funktioniert brauch nur ewig
--prop_DomainCompose :: Subst -> Subst -> Bool 
--prop_DomainCompose s1 s2 = listElem (nub (domain (compose s1 s2))) (nub (domain s1 `union` domain s2))

prop_DomainComposeSingle :: VarName -> VarName -> Property  
prop_DomainComposeSingle x y = x /= y ==> domain (compose (single y (Var x)) (single x (Var y))) == [y]

prop_allVarsEmpty :: Bool 
prop_allVarsEmpty = null (allVars empty)

prop_allVarsSingleSelfReference :: VarName -> Bool
prop_allVarsSingleSelfReference x = null (allVars (single x (Var x)))

prop_allVarsSingle :: VarName -> Term -> Property 
prop_allVarsSingle v t = t /= Var v ==> listElem (allVars (single v t))  (v : allVars t)

--Funktioniert brauch nur ewig
--prop_allVarsCompose :: Subst -> Subst -> Bool
--prop_allVarsCompose s1 s2 = listElem (allVars (compose s1 s2)) (allVars s1 ++ allVars s2)

prop_allVarsComposeSingle :: VarName -> VarName -> Property  
prop_allVarsComposeSingle v1 v2 = v1 /= v2 ==> listElem (allVars (compose (single v2 (Var v1)) (single v1 (Var v2)))) [v1,v2] && listElem [v1,v2] (allVars (compose (single v2 (Var v1)) (single v1 (Var v2))))

prop_DomainAllVars :: Subst -> Bool
prop_DomainAllVars s = listElem (domain s) (allVars s)

prop_DomainRestrictEmpty :: [VarName] -> Bool 
prop_DomainRestrictEmpty n = null (domain (restrictTo Empty n))

--Funktioniert brauch nur ewig
--prop_domainrestrict :: Subst -> [VarName] -> Bool
--prop_domainrestrict s n = listElem (domain(restrictTo s n)) n

listElem :: Eq a => [a] -> [a] -> Bool
listElem [] _     = True
listElem (x:xs) y = x `elem` y && listElem xs (delete x y)

termEquals :: Term -> Term -> Bool
termEquals (Var _)           (Comb _ _)        = False 
termEquals (Comb _ _)        (Var _)           = False
termEquals (Var (VarName x)) (Var (VarName y)) = x == y
termEquals (Comb x t1)       (Comb y t2)       = x == y && help t1 t2
  where
  help :: [Term] -> [Term] -> Bool
  help []     []     = True
  help (x:xs) (y:ys) = termEquals x y && help xs ys

-- Check all properties in this module:
return []
testAll = $quickCheckAll