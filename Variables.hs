module Variables where

import Type

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




{-freshVars :: [VarName]    
freshVars = do
    a <- [0..]
    b <- ['A'..'Z']
    return (VarName <$> a b)-}