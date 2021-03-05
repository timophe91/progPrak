module Variables 
  (allVars, freshVars, removeDups, Vars)
  where

import Type

class Vars v where
    allVars :: v -> [VarName]


instance Vars Term where -- All VarNames of a Term
    allVars (Var v)    = [v]
    allVars (Comb _ t) = removeDups (concatMap allVars t)

instance Vars Rule where -- All VarNames of a Rule 
    allVars (Rule x t) = removeDups (allVars x ++ concatMap allVars t)

instance Vars Prog where -- All VarNames of Prog
    allVars (Prog r) = removeDups (concatMap allVars r)

instance Vars Goal where -- All VarNames of Goal
    allVars (Goal t) = removeDups (concatMap allVars t)

{- Generating a endless list of VarNames with the form:
 - [VarName "A", ... ,VarName "Z", VarName "A0", ... ,VarName "Z0", VarName "A1", ... , VarName "Z1", ... ]
-}
freshVars :: [VarName]    
freshVars = [VarName [x] | x <- ['A'..'Z']] ++ [VarName (l:show x) | x <- ['0'..],  l <- ['A'..'Z']]

{- Remove duplicated VarNames
-}
removeDups :: [VarName] -> [VarName]
removeDups []     = []
removeDups (v:vs) = if v `elem` vs then removeDups vs else v : removeDups vs