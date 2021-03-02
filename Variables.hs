module Variables where

import Type

class Vars v where
    allVars :: v -> [VarName]


instance Vars Term where
    allVars  (Var v) = [v]



freshVars :: [VarName]    
freshVars = do
    a <- [0..]
    b <- ['A'..'Z']
    return (VarName <$> a b)