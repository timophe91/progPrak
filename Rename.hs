{-# LANGUAGE TemplateHaskell #-}
module Rename where

import Type
import Variables
import Data.List
import Substitution
import Test.QuickCheck

rename :: [VarName] -> Rule -> Rule
rename [] r = renameVars [] (allVars r) r
rename v  r = renameVars v  (allVars r) r


renameVars :: [VarName] -> [VarName]-> Rule -> Rule
renameVars v vR (Rule t ts)  = let var         = freshVars \\ (v ++ vR)
                                   subs        = compose (foldr compose empty (zipWith (\varN1 varN2 -> single varN1 (Var varN2))  vR var)) (single (VarName "_") (Var (VarName "renameMe_")))
                                   newAllVar   = map (\(VarName vN) -> VarName ("_" + vN)) freshVars \\ (v ++ allVars subs)
                                   renamedAnon = renameAnonymousVars newAllVar (apply subs t : map (apply subs) ts)
                               in  Rule  ft rt

-- renming the anonymous vars
renameAnonymousVars :: [VarName] -> [Term] -> [Term]
renameAnonymousVars _ []           = []
renameAnonymousVars (vn:vns) ((Var v):ts)  = if v == VarName "renameMe_" then Var vn : renameAnonymousVars vns ts else Var v : renameAnonymousVars vns ts
renameAnonymousVars (vn:vns) ((Comb cN cTS):ts) =  Comb cN 
 

-- all variables forbidden and those of the old rule intersected with the variables of the renamed rule should be null  
prop_allVarsDiffEmpty :: [VarName] -> Rule -> Bool
prop_allVarsDiffEmpty xs r = null (allVars (rename xs r) `intersect` allVars r)

-- all variables forbidden and those of the old rule intersected with the variables of forbidden list should be null  
prop_DiffEmpty :: [VarName] -> Rule -> Bool
prop_DiffEmpty xs r = null (allVars (rename xs r) `intersect` xs)

-- the anonymous variable is not an element of rule, after rename
prop_noAnonymousVar :: [VarName] -> Rule -> Bool 
prop_noAnonymousVar xs r = VarName "_" `notElem` allVars (rename xs r)

-- the length of the lists of variables after renaming should be the same, if "_" is not part of the Rule
prop_sameLengthNoAnonymousVar :: [VarName] -> Rule -> Property 
prop_sameLengthNoAnonymousVar xs r = VarName "_" `notElem` allVars r ==> length (allVars r) == length (allVars (rename xs r))

-- the length of the lists of variables after renaming should differ, the renamed should greater or equal
prop_biggerLengthWithAnonymousVar :: [VarName] -> Rule -> Bool 
prop_biggerLengthWithAnonymousVar xs r =  length (allVars (rename xs r)) >= length (allVars r)

{- Test all props_
-}
return []
testAllRename :: IO Bool
testAllRename = $quickCheckAll