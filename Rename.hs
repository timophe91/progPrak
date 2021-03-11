{-# LANGUAGE TemplateHaskell #-}
module Rename (rename, testAllRename) where

import Type
import Variables
import Data.List
import Substitution
import Test.QuickCheck

{- Renames Terms in Rules
 - do a subst for every variable in rules terms, then apply them, so every Variable is renamed the same
 - tagging anonymous Vars _ with renameMe_ and renaming them with another endless set of fresh var
-}
rename :: [VarName] -> Rule -> Rule
rename v  r = renameVars v  (allVars r) r

-- renaming Variables with forbidden Vars and the set of Var from Rule
renameVars :: [VarName] -> [VarName]-> Rule -> Rule
renameVars v vR (Rule t ts)  = let var              = freshVars \\ (v ++ vR) -- filter all Variables allready used or forbidden
                                   -- Create a Subst, in which every Variable will be 'renamed' to a Variable of var, then tagging anonymous Vars with renameMe_
                                   subs             = compose (foldr compose empty (zipWith (\varN1 varN2 -> single varN1 (Var varN2))  vR var)) (single (VarName "_") (Var (VarName "renameMe_")))
                                   -- filter all variables allready used 
                                   newFreshVar      = map (\(VarName fV) -> VarName ("_" ++ fV)) freshVars \\ (allVars subs ++ v)
                                   -- renaming every signle anonymous variable to a single other var
                                   (_, renamedAnon) = renameAnonymousVars (apply subs t : map (apply subs) ts) newFreshVar
                                   ft               = head renamedAnon -- get the split first and the rest terms to redo the Rule again
                                   rt               = tail renamedAnon
                               in  Rule  ft rt

-- renaming the anonymous vars
renameAnonymousVars :: [Term] -> [VarName] -> ([VarName], [Term])
renameAnonymousVars []                     n = (n, [])                                                                                       -- all done
renameAnonymousVars ((Var (VarName v)):ts) n = let (n', tns) = renameAnonymousVars ts n                                                      -- rename the last variables first, pass trough the VarName List
                                               in  if v == "renameMe_" then (tail n', Var (head n') : tns) else (n', Var (VarName v) : tns)  -- if the VarName has to be renamed use the head and pass the tail
renameAnonymousVars ((Comb cN cTs):ts)     n = let (n', ncT)   = renameAnonymousVars cTs n  -- rename the [Term] of the Comb
                                                   (n'', nts)  = renameAnonymousVars ts n'  -- rename the rest List
                                               in  (n'', Comb cN ncT:nts) 
 

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