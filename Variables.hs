module Variables where

import Type
import Substitution

class Vars v where
    allVars :: v -> [VarName]


instance Vars Term where
    allVars (Var v)    = [v]
    allVars (Comb _ t) = concatMap allVars t

instance Vars Rule where
    allVars (Rule x t) = allVars x ++ concatMap allVars t

instance Vars Prog where
    allVars (Prog r) = concatMap allVars r

instance Vars Goal where
    allVars (Goal t) = concatMap allVars t

instance Vars Subst where
    allVars Empty    = []
    allVars (Subs v) = concatMap fst v


{- Generating a endless list of VarNames with the form:
 - [VarName "A", ... ,VarName "Z", VarName "A0", ... ,VarName "Z0", VarName "A1", ... , VarName "Z1", ... ]
-}
freshVars :: [VarName]    
freshVars = [VarName [x] | x <- ['A'..'Z']] ++ [VarName (l:show x) | x <- [0..],  l <- ['A'..'Z']]