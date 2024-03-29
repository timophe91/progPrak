{-# LANGUAGE TemplateHaskell #-}
module Unifikation (ds, unify, testAllUnifikation)where

import Type
import Test.QuickCheck
import Substitution
import Data.Maybe

{- Calculates the disagreement set of two Terms. Returns Nothing if the disagreement set is empty
-}
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var (VarName "_")) _                   = Nothing -- If one of the Variables is anonymous return Nothing
ds _                   (Var (VarName "_")) = Nothing
ds (Var v)             t                   = if Var v == t then Nothing else Just (Var v, t) -- If one of the terms is a variable and the terms r not equal return the tuple of both terms
ds t                   (Var v)             = if Var v == t then Nothing else Just (Var v, t) -- for unify changed order
ds (Comb v1 t1)        (Comb v2 t2)        = if v1 /= v2 || length t1 /= length t2 then Just (Comb v1 t1, Comb v2 t2) else help t1 t2 -- If both terms are funktions with the same name and number of terms in the list return the first element which differs
  where
  {- Surches and returns the first tuple of terms in two lists which are not the same
  -}
  help :: [Term] -> [Term] -> Maybe (Term, Term)
  help []     []     = Nothing
  help (x:xs) (y:ys) = let result = ds x y in if isNothing result then help xs ys else result
  help _      _      = Nothing --Wegen -Wall


{- Calculates the most general unifier of two terms
-}
unify :: Term -> Term -> Maybe Subst
unify x y = if x == y then Just empty else help x y empty -- terms r equal, 
  where
  help :: Term -> Term -> Subst -> Maybe Subst
  help t1 t2 s = let d = ds (apply s t1) (apply s t2) in if isNothing d then Just s else help2 t1 t2 d s -- calculate the ds and generate the new substitution if ds is not Nothing
  help2 :: Term -> Term -> Maybe (Term, Term) -> Subst -> Maybe Subst
  help2 i j (Just (Var d1, d2)) s = if d1 `elem` allVars d2 then Nothing else help i j (compose (single d1 d2) s) -- if the var d1 is element of the d2 its a fail, by definition, otherwise go to step 2 (help) again
  help2 _ _ _                   _ = Nothing --Wegen -Wall


{- The ds of a Term and itself should be nothing
-}
prop_Equals :: Term -> Bool
prop_Equals t = isNothing (ds t t)

{- If the ds of two Term is not nothing the Terms schould not be the same
-}
prop_dsNotEmpty :: Term -> Term -> Property 
prop_dsNotEmpty t1 t2 = isJust (ds t1 t2) ==> t1 /= t2

{- If the ds of two Term is nothing unify should return a value and the domain of that value should be null
-}
prop_dsEmpty :: Term -> Term -> Property
prop_dsEmpty t1 t2 = isNothing (ds t1 t2) ==> let u = unify t1 t2 in isJust u && null (domain (fromJust u))

{- If unify return a substitution for two Term the ds of the substitution applyed to both Term should be nothing
-}
prop_unify :: Term -> Term -> Property
prop_unify t1 t2 = isJust (unify t1 t2) ==> let u = fromJust (unify t1 t2) in isNothing (ds (apply u t1) (apply u t2))


{- Test all props_
-}
return []
testAllUnifikation :: IO Bool
testAllUnifikation = $quickCheckAll