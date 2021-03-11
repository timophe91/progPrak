module Pretty
  (pretty, comma, Pretty)
  where

import Type


{- A Class for a pretty output of datatypes
-}
class Show a => Pretty a where
  pretty ::  a -> String
  pretty a = show a


{- Pretty output Simple Prolog Terms
 - Using the Type.hs
 - pretty :: Term -> String
-}
instance Pretty Term where 
  pretty (Var (VarName v))  = v
  pretty (Comb "." [t1,t2]) = "[" ++ prettyLists t1 t2 ++ "]"
  pretty (Comb c [])        = c
  pretty (Comb c t)         = c ++ "(" ++ comma (map pretty t) ++ ")"


{- A function to pretty Prolog Lists
 - in their different ways
-}
prettyLists :: Term -> Term -> String
prettyLists (Comb c [])        (Comb "[]" [])      = c
prettyLists (Comb c [])        (Comb "." [t1, t2]) = c ++ ", " ++ prettyLists t1 t2
prettyLists t                  (Comb "[]" [])      = pretty t
prettyLists t1                 t2                  = pretty t1 ++ "|" ++ pretty t2

-- setting a comma between all elements and concat them to one string
comma :: [String] -> String 
comma []     = []
comma [x]    = x         
comma (x:xs) = (x ++ ", ") ++ comma xs