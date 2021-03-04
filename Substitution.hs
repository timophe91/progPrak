{-# LANGUAGE TemplateHaskell #-}
module Substitution
 (domain, empty, single, apply, compose, restrictTo, pretty, allVars, testAllSubstitution)
 where

import Type
import Test.QuickCheck
import Variables
import Data.List
import Pretty


{- Wrapper for our List of Tupel
-}
newtype Subst
  = Subs [(VarName, Term)]
  deriving (Eq, Show)

{- Instance for allVars from Vars
 - get all Variables from the Subs, also those from the Term
-}
instance Vars Subst where
    allVars (Subs []) = []
    allVars (Subs v)  = removeDups (concatMap (\(v1, t1) -> v1 : allVars t1) v)

{- Pretty output for Substitutions
 - pretty :: Subst -> String
-}
instance Pretty Subst where
  pretty (Subs []) = "{}" 
  pretty (Subs s)  = "{" ++ comma (map (\(VarName v, t) -> v ++ " -> " ++ pretty t) s) ++ "}"


{- outputs the domain of a substitution
-}
domain :: Subst -> [VarName]
domain (Subs [])        = []
domain (Subs ((x,_):s)) = nub (x : domain (Subs s))

{- Creates a empty substitution
-}
empty :: Subst
empty = Subs []

{- Creates a substitution that maps a single variable to a term
 - substitutions on self r empty
-}
single :: VarName -> Term -> Subst
single v (Var v2) = if v == v2 then empty else Subs [(v, Var v2)] -- prevent figure on self
single v t        = Subs [(v, t)]

{- Applys a substitution to a given term
-}
apply :: Subst -> Term -> Term
apply (Subs [])  t             = t
apply (Subs ((x,y):s)) (Var v) = if x == v then y else apply (Subs s) (Var v) -- replace Variable if found with first occurenc
apply s             (Comb n t) = Comb n [apply s x| x <- t] -- apply Subst on every Term in Comb

{- Merge two Prolog Substitutions
 - first removeAll duplicated VarNames from the Subst for CleanUp, if there r given Subst e.g. {A -> B, A -> C} => {A -> B}
 - apply s1 on all Terms of s2 and concat them with s1 and then CleanUp Again, slow but works
-}
compose :: Subst -> Subst -> Subst
compose (Subs [])       s2        = removeDupsInSubs s2
compose s1              (Subs []) = removeDupsInSubs s1
compose s1              s2        = removeDupsInSubs (concatSubs (applyToAll (removeDupsInSubs s1) (removeDupsInSubs s2)) (removeDupsInSubs s1))

{- remove every Subst, except the first occurrence
-}
removeDupsInSubs :: Subst -> Subst
removeDupsInSubs (Subs [])         = empty
removeDupsInSubs (Subs ((v, t):r)) = concatSubs (single v t) (removeDupsInSubs (Subs (removeOthers v r)))
  where 
    removeOthers :: VarName -> [(VarName, Term)] -> [(VarName, Term)] -- remove every occurrence of v
    removeOthers _ []          =  []
    removeOthers v0 ((v1, t1):r1) = if v0 == v1 then removeOthers v0 r1 else (v1, t1) : removeOthers v0 r1

{- Apply Subst on every Term of the second Subst
-}
applyToAll :: Subst -> Subst -> Subst
applyToAll _ (Subs [])          =  empty
applyToAll s (Subs ((v, t):xs)) =  concatSubs (single v (apply s t)) (applyToAll s (Subs xs)) 

{- Concatenate two Subst
-}
concatSubs :: Subst -> Subst -> Subst
concatSubs (Subs []) x         = x
concatSubs x         (Subs []) = x
concatSubs (Subs x)  (Subs y)  = Subs (x ++ y)


{- Restricts the domain of a substitution to a given set of variables
-}
restrictTo :: Subst -> [VarName] -> Subst
restrictTo _                [] = empty
restrictTo (Subs [])        _  = empty
restrictTo (Subs ((x,y):s)) n  = if x `elem` n then  compose (single x y) (restrictTo (Subs s) n) else restrictTo (Subs s) n

{- Arbitraty instance for Subst
-}
instance Arbitrary Subst where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [return (Subs []), return (Subs (nub [(v,t)| v <- nub x, t <- y]))]


prop_ApplyEmpty :: Term -> Bool
prop_ApplyEmpty t = t == apply empty t

prop_ApplySingle :: VarName -> Term -> Bool
prop_ApplySingle v t = apply (single v t) (Var v) == t

prop_ApplyCompose :: Subst -> Subst -> Term -> Bool 
prop_ApplyCompose s1 s2 t = apply (compose s1 s2) t ==  apply s1 (apply s2 t)

prop_DomainEmpty :: Bool
prop_DomainEmpty = null (domain empty)

prop_DomainSingleSelfReference :: VarName -> Bool 
prop_DomainSingleSelfReference x = null (domain (single x (Var x)))

prop_DomainSingle :: VarName -> Term -> Property  
prop_DomainSingle v t = t /= Var v ==> domain (single v t) == [v]

prop_DomainCompose :: Subst -> Subst -> Bool 
prop_DomainCompose s1 s2 = listElem (nub (domain (compose s1 s2))) (nub (domain s1 `union` domain s2))

prop_DomainComposeSingle :: VarName -> VarName -> Property  
prop_DomainComposeSingle x y = x /= y ==> domain (compose (single y (Var x)) (single x (Var y))) == [y]

prop_allVarsEmpty :: Bool 
prop_allVarsEmpty = null (allVars empty)

prop_allVarsSingleSelfReference :: VarName -> Bool
prop_allVarsSingleSelfReference x = null (allVars (single x (Var x)))

prop_allVarsSingle :: VarName -> Term -> Property 
prop_allVarsSingle v t = t /= Var v ==> listElem (allVars (single v t))  (v : allVars t)

prop_allVarsCompose :: Subst -> Subst -> Bool
prop_allVarsCompose s1 s2 = listElem (allVars (compose s1 s2)) (allVars s1 ++ allVars s2)

prop_allVarsComposeSingle :: VarName -> VarName -> Property  
prop_allVarsComposeSingle v1 v2 = v1 /= v2 ==> listElem (allVars (compose (single v2 (Var v1)) (single v1 (Var v2)))) [v1,v2] && listElem [v1,v2] (allVars (compose (single v2 (Var v1)) (single v1 (Var v2))))

prop_DomainAllVars :: Subst -> Bool
prop_DomainAllVars s = listElem (domain s) (allVars s)

prop_DomainRestrictEmpty :: [VarName] -> Bool 
prop_DomainRestrictEmpty n = null (domain (restrictTo empty n))

prop_domainrestrict :: Subst -> [VarName] -> Bool
prop_domainrestrict s n = listElem (domain(restrictTo s n)) n

{- Test if the first list is fully part of the second List
-}
listElem :: Eq a => [a] -> [a] -> Bool
listElem []     _ = True
listElem (x:xs) y = x `elem` y && listElem xs (delete x y)



return []
testAllSubstitution :: IO Bool
testAllSubstitution = $quickCheckAll