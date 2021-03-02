module Variables where

import Type
import Control.Monad

class Vars v where
    allVars :: v -> [VarName]


instance Vars Term where
    allVars (Var v)    = [v]
    allVars (Comb _ t) = concatMap allVars t

instance Vars Rule where
    allVars (Rule x t) = allVars x ++ concatMap allVars t

<<<<<<< HEAD
instance Vars Prog where
    allVars (Prog r) = concatMap allVars r

instance Vars Goal where
    allVars (Goal t) = concatMap allVars t




{-freshVars :: [VarName]    
freshVars = do
    a <- [0..]
    b <- ['A'..'Z']
    return (VarName <$> a b)-}
=======
{- Generating a endless list of VarNames with the form:
 - [VarName "A", ... ,VarName "Z", VarName "A0", ... ,VarName "Z0", VarName "A1", ... , VarName "Z1", ... ]
-}
freshVars :: [VarName]    
freshVars = [VarName [x] | x <- ['A'..'Z']] ++ [VarName (l:show x) | x <- [0..],  l <- ['A'..'Z']]
>>>>>>> 02492f6e415e30f25c3cb44cfb8bebfa7a145692
