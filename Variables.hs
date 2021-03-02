module Variables where

import Type
import Control.Monad

class Vars v where
    allVars :: v -> [VarName]


instance Vars Term where
    allVars  (Var v) = [v]


{- Generating a endless list of VarNames with the form:
 - [VarName "A", ... ,VarName "Z", VarName "A0", ... ,VarName "Z0", VarName "A1", ... , VarName "Z1", ... ]
-}
freshVars :: [VarName]    
freshVars = [VarName [x] | x <- ['A'..'Z']] ++ [VarName (l:show x) | x <- [0..],  l <- ['A'..'Z']]