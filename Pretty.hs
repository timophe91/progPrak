import Type

pretty :: Term -> String 
pretty (Var (VarName v)) = v
pretty (Comb "." [t1,t2]) = prettyCombination
pretty (Comb c []) = c
pretty (Comb c t) = c ++ "(" ++ coma (map pretty t) ++ ")"
  where coma:: [String] -> String
        coma [x]    = x
        coma (x:xs) = (x ++ ", ") ++ coma xs


prettyCombination :: Term -> Term -> String 