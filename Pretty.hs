import Type

class Pretty p where 
    pretty :: a -> String
    pretty :: Term -> String
    pretty Var (VarName a) = a
    pretty Comb (CombName [t:ts]) = pretty t  ++ pretty (Comb (CombName ts))