{-# LANGUAGE TemplateHaskell #-}
module Substitution
 (domain, pretty, allVars, empty, single, apply, compose, restrictTo, testAllSubstitution, Subst)
 where

import Type
import Test.QuickCheck
import Variables
import Data.List
import Pretty

{- Wrapper for our List of tuple
-}
newtype Subst
  = Subs [(VarName, Term)]
  deriving (Show)

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
single v (Var v2) = if v == v2 then empty else Subs [(v, Var v2)]  -- prevent figure on self
single v t        = Subs [(v, t)]

{- Applys a substitution to a given term
-}
apply :: Subst -> Term -> Term
apply (Subs [])        t          = t
apply (Subs ((x,y):s)) (Var v)    = if x == v then y else apply (Subs s) (Var v)  -- replace Variable if found with first occurence
apply s                (Comb n t) = Comb n [apply s x| x <- t]                    -- apply Subst on every Term in Comb

{- Merge two Prolog Substitutions
 - first removeAll duplicated VarNames from the Subst for CleanUp, if there r given Subst e.g. {A -> B, A -> C} => {A -> B}
 - apply s1 on all Terms of s2 and concat them with s1 and then CleanUp Again, slow but works
-}
compose :: Subst -> Subst -> Subst
compose (Subs []) s2        = removeDupsInSubs s2
compose s1        (Subs []) = removeDupsInSubs s1
compose s1        s2        = let cleans1 = removeDupsInSubs s1 in removeDupsInSubs (concatSubs (applyToAll cleans1 (removeDupsInSubs s2)) cleans1)

{- remove every Subst, except the first occurrence
-}
removeDupsInSubs :: Subst -> Subst
removeDupsInSubs (Subs [])         = empty
removeDupsInSubs (Subs ((v, t):r)) = concatSubs (single v t) (removeDupsInSubs (Subs (removeOthers v r)))
  where 
    removeOthers :: VarName -> [(VarName, Term)] -> [(VarName, Term)]                                       -- remove every occurrence of v
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
    x <- arbitrary                                                      -- List of VarNames
    y <- arbitrary                                                      -- List of Terms
    oneof [return (Subs []), return (Subs ([(v,t)| v <-  x, t <- y]))]  -- returns ether the empty substitution or a random substitution created using the List of VarNames and Terms

{- 1. Empty applied to a term shouldn't change the term
-}
prop_ApplyEmpty :: Term -> Bool
prop_ApplyEmpty t = t == apply empty t

{- 2. A single applied to a term with the same VarName should return the term defined in Single
-}
prop_ApplySingle :: VarName -> Term -> Bool
prop_ApplySingle v t = apply (single v t) (Var v) == t

{- 3. Applying a compose should act as if the second substitution was applyed first and then the first
-}
prop_ApplyCompose :: Subst -> Subst -> Term -> Bool 
prop_ApplyCompose s1 s2 t = apply (compose s1 s2) t ==  apply s1 (apply s2 t)

{- 4. The domain of Empty should be []
-}
prop_DomainEmpty :: Bool
prop_DomainEmpty = null (domain empty)

{- 5. The domain of a single which references itself should be []
-}
prop_DomainSingleSelfReference :: VarName -> Bool 
prop_DomainSingleSelfReference x = null (domain (single x (Var x)))

{- 6. The domain of a single which does not refer to itself
-}
prop_DomainSingle :: VarName -> Term -> Property  
prop_DomainSingle v t = t /= Var v ==> domain (single v t) == [v]

{- 7. the domain of a compose of two substitutions should be a subset of the union of the domains of the substitutions 
-}
prop_DomainCompose :: Subst -> Subst -> Bool 
prop_DomainCompose s1 s2 = subSet (domain (compose s1 s2)) (domain s1 `union` domain s2)

{- 8. the domain of a compose of two singles forming a loop should be the domain of the single which is used second
-}
prop_DomainComposeSingle :: VarName -> VarName -> Property  
prop_DomainComposeSingle x y = x /= y ==> domain (compose (single y (Var x)) (single x (Var y))) == [y]

{- 9. allVars of Empty should be []
-}
prop_allVarsEmpty :: Bool 
prop_allVarsEmpty = null (allVars empty)

{- 10. allVars of a single which references itself should be []
-}
prop_allVarsSingleSelfReference :: VarName -> Bool
prop_allVarsSingleSelfReference x = null (allVars (single x (Var x)))

{- 11. allVars of a single which dose not reference itself should be the same as the union of allVars from the term and the varName of the single
-}
prop_allVarsSingle :: VarName -> Term -> Property 
prop_allVarsSingle v t = t /= Var v ==> subSet (allVars (single v t)) (allVars t `union` [v]) && subSet (allVars t `union` [v]) (allVars (single v t))

{- 12. allVars of a compose should be a subset of the union of allVars of the two substitutions 
-}
prop_allVarsCompose :: Subst -> Subst -> Bool
prop_allVarsCompose s1 s2 = subSet (allVars (compose s1 s2)) (allVars s1 ++ allVars s2)

{- 13. allVars of a compose containing two singles which form a loop should be both terms used in the singles
-}
prop_allVarsComposeSingle :: VarName -> VarName -> Property  
prop_allVarsComposeSingle v1 v2 = v1 /= v2 ==> subSet (allVars (compose (single v2 (Var v1)) (single v1 (Var v2)))) [v1,v2] && subSet [v1,v2] (allVars (compose (single v2 (Var v1)) (single v1 (Var v2))))

{- 14. the domain of a term should be a subset of allVars from that term
-}
prop_DomainAllVars :: Subst -> Bool
prop_DomainAllVars s = subSet (domain s) (allVars s)

{- 15. the domain of Empty restricted to a list of variables should still be []
-}
prop_DomainRestrictEmpty :: [VarName] -> Bool 
prop_DomainRestrictEmpty n = null (domain (restrictTo empty n))

{- 16. the domain of a substitution restrictet to a list of variables should be a subset of the given list
-}
prop_domainrestrict :: Subst -> [VarName] -> Bool
prop_domainrestrict s n = subSet (domain(restrictTo s n)) n

{- the domain of a compose of a substitution and empty should be a subset of the domain of the substitution
 - given the fact that our compose cleanUp allways its not allways the same
-}
prop_domainComposeSubstEmpty :: Subst -> Bool
prop_domainComposeSubstEmpty s = subSet (domain (compose s empty)) (domain s)

{- allVars from a compose of a substitution and empty should be a subset of allVars from the substitution
-}
prop_allVarsComposeSubstEmpty :: Subst -> Bool
prop_allVarsComposeSubstEmpty s = subSet (allVars (compose s empty)) (allVars s)

{- the compose of a substitution and empty (regardless which is the first substitution in compose) applyed to a term should be the same as just the substitution applyed to the term
-}
prop_applyComposeSubstEmpty :: Subst -> Term -> Bool
prop_applyComposeSubstEmpty s t = let applyST = apply s t in apply (compose s empty) t == applyST && apply (compose empty s) t == applyST


{- Test if the first list is fully part of the second List
-}
subSet :: Eq a => [a] -> [a] -> Bool
subSet []     _ = True
subSet (x:xs) y = x `elem` y && subSet xs (delete x y)

{- Test all props_
-}
return []
testAllSubstitution :: IO Bool
testAllSubstitution = $quickCheckAll