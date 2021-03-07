{-# LANGUAGE TemplateHaskell #-}
module Unifikation where

import Type
import Test.QuickCheck
import Substitution
import Data.Maybe
import Variables

{- Calculates the disagreement set of two Terms. Returns Nothing if the disagreement set is empty
-}
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var (VarName "_")) _                   = Nothing -- If one of the Variables is anonymous return Nothing
ds _                   (Var (VarName "_")) = Nothing
ds (Var v)             t                   = if Var v == t then Nothing else Just (Var v, t) -- If one of the terms is a variable and the terms a not equal return the tuple of both terms
ds t                   (Var v)             = if Var v == t then Nothing else Just (Var v, t) -- for unify changed order
ds (Comb v1 t1)        (Comb v2 t2)        = if v1 /= v2 || length t1 /= length t2 then Just (Comb v1 t1, Comb v2 t2) else help t1 t2 -- If both terms are funktions with the same name and number of terms in the list return the first element which differs
  where
  {- Surches and returns the first tuple of terms in two lists which are not the same
  -}
  help []     []     = Nothing
  help (x:xs) (y:ys) = let result = ds x y in if isNothing result then help xs ys else result
  help _      _      = Nothing --Wegen -Wall wird aber nie genutzt



unify :: Term -> Term -> Maybe Subst
unify x y = let t = help x y empty in if t == Just empty then Nothing else t
  where
  help t1 t2 s = let d = ds (apply s t1) (apply s t2) in if isNothing d then Just s else help2 t1 t2 d s
  help2 _ _ Nothing             _ = Nothing
  help2 i j (Just (Var d1, d2)) s = if d1 `elem` allVars d2 then Nothing else help i j (compose (single d1 d2) s)
  help2 _ _ _                   _ = Nothing

prop_Equals :: Term -> Bool
prop_Equals t = isNothing (ds t t)

prop_dsNotEmpty :: Term -> Term -> Property 
prop_dsNotEmpty t1 t2 = isJust (ds t1 t2) ==> t1 /= t2

prop_dsEmpty :: Term -> Term -> Property
prop_dsEmpty t1 t2 = isNothing (ds t1 t2) ==> let u = unify t1 t2 in isJust u && null (domain (fromMaybe empty u))

prop_unify :: Term -> Term -> Property
prop_unify t1 t2 = isJust (unify t1 t2) ==> let u = fromMaybe empty (unify t1 t2) in isNothing (ds (apply u t1) (apply u t2))

{-prop_NotEquals :: Term -> Term -> Property
prop_NotEquals (Var v1) (Var v2) = Var v1 /= Var v2 ==> if v1 /= VarName "_" && v2 /= VarName "_" then  isJust (ds (Var v1) (Var v2)) else isNothing (ds (Var v1) (Var v2))
prop_NotEquals (Var v)  t        = Var v /= t ==> if v /= VarName "_" then isJust (ds (Var v) t) else isNothing (ds (Var v) t)
prop_NotEquals t        (Var v)  = Var v /= t ==> if v /= VarName "_" then isJust (ds (Var v) t) else isNothing (ds (Var v) t)
prop_NotEquals t1       t2       = t1 /= t2 ==> isJust (ds t1 t2)-}


{- Test all props_
-}
return []
testAllUnifikation :: IO Bool
testAllUnifikation = $quickCheckAll