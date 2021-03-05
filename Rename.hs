module Rename where

import Type
import Variables
import Data.List
import Substitution

rename :: [VarName] -> Rule -> Rule
rename [] r = renameVars [] (allVars r) r
rename v  r = renameVars v  (allVars r) r


renameVars :: [VarName] -> [VarName]-> Rule -> Rule
renameVars v vR (Rule t ts)  = let var  = (freshVars \\ (v ++ vR)) 
                                   subs = foldr compose empty (zipWith (\varN1 varN2 -> single varN1 (Var varN2))  vR var)
                               in  Rule (apply subs t) (map (apply subs) ts)