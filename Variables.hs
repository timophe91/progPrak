module Variables where

import Type

class Vars v where
    allVars :: v -> [VarName]


instance Vars Term where
    allVars (Var v)    = [v]
    allVars (Comb _ t) = removeDups (concatMap allVars t)

instance Vars Rule where
    allVars (Rule x t) = removeDups (allVars x ++ concatMap allVars t)

instance Vars Prog where
    allVars (Prog r) = removeDups (concatMap allVars r)

instance Vars Goal where
    allVars (Goal t) = removeDups (concatMap allVars t)

{- Generating a endless list of VarNames with the form:
 - [VarName "A", ... ,VarName "Z", VarName "A0", ... ,VarName "Z0", VarName "A1", ... , VarName "Z1", ... ]
-}
freshVars :: [VarName]    
freshVars = [VarName [x] | x <- ['A'..'Z']] ++ [VarName (l:show x) | x <- ['0'..],  l <- ['A'..'Z']]

removeDups :: [VarName] -> [VarName]
removeDups []     = []
removeDups (v:vs) = if v `elem` vs then removeDups vs else v : removeDups vs