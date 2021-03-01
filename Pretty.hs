import Type

pretty :: Term -> String 
pretty (Var (VarName v)) = v
pretty (Comb "." [t1,t2]) = "[" ++ prettyCombination t1 t2 ++ "]"
pretty (Comb c []) = c
pretty (Comb c t) = c ++ "(" ++ coma (map pretty t) ++ ")"
  where coma:: [String] -> String
        coma [x]    = x
        coma (x:xs) = (x ++ ", ") ++ coma xs


prettyCombination :: Term -> Term -> String
prettyCombination (Var (VarName v1)) (Var (VarName v2))  = v1 ++ ", " ++ v2
prettyCombination (Var (VarName v))  (Comb "." [t1, t2]) = v ++ ", " ++ prettyCombination t1 t2
prettyCombination (Var (VarName v))  c                   = v ++ "|" ++ pretty c
prettyCombination t1                 t2                  = pretty t2 ++ pretty t2














----------------------------------------------------
test:: IO()
test = do putStrLn "Var (VarName \"A\")"
          putStrLn (pretty (Var (VarName "A")))
          putStrLn ("A" ++ "\n")
          putStrLn "Comb \"true\" []"
          putStrLn (pretty (Comb "true" []))
          putStrLn ("true" ++ "\n")
          putStrLn "Comb \"[]\" []"
          putStrLn (pretty (Comb "[]" []))
          putStrLn ("[]" ++ "\n")
          putStrLn "Comb \"f\" [Var (VarName \"B\"), Var (VarName \"_\"), Comb \"true\" []]"
          putStrLn (pretty (Comb "f" [Var (VarName "B"), Var (VarName "_"), Comb "true" []]))
          putStrLn ("f(B, _, true)" ++ "\n")
          putStrLn "Comb \".\" [Comb \"true\" [], Comb \"[]\" []]"
          putStrLn (pretty (Comb "." [Comb "true" [], Comb "[]" []]))
          putStrLn ("[true]" ++ "\n")
          putStrLn "Comb \".\" [Comb \"true\" [], Comb \".\" [Comb \"g\" [Var (VarName \"C\")], Comb \"[]\" []]]"
          putStrLn (pretty (Comb "." [Comb "true" [], Comb "." [Comb "g" [Var (VarName "C")], Comb "[]" []]]))
          putStrLn ("[true, g(C)]" ++ "\n")
          putStrLn "Comb \".\" [Comb \"1\" [], Comb \".\" [Comb \"2\" [], Comb \".\" [Comb \"3\" [], Comb \"[]\" []]]]"
          putStrLn (pretty (Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]]))
          putStrLn ("[1, 2, 3]" ++ "\n")
          putStrLn "Comb \".\" [Comb \"true\" [], Var (VarName \"D\")]"
          putStrLn (pretty (Comb "." [Comb "true" [], Var (VarName "D")]))
          putStrLn ("[true|D]" ++ "\n")
          putStrLn "Comb \".\" [Var (VarName \"E\"), Comb \"h\" [Var (VarName \"F\"), Comb \"i\" [Var (VarName \"G\")]]]"
          putStrLn (pretty (Comb "." [Var (VarName "E"), Comb "h" [Var (VarName "F"), Comb "i" [Var (VarName "G")]]]))
          putStrLn ("[E|h(F, i(G))]" ++ "\n")
          putStrLn "Comb \".\" [Comb \"true\" [], Comb \".\" [Comb \"true\" [], Comb \"true\" []]]"
          putStrLn (pretty (Comb "." [Comb "true" [], Comb "." [Comb "true" [], Comb "true" []]]))
          putStrLn ("[true, true|true]" ++ "\n")
          putStrLn "Comb \".\" [Comb \"[]\" [], Comb \"[]\" []]"
          putStrLn (pretty (Comb "." [Comb "[]" [], Comb "[]" []]))
          putStrLn ("[[]]" ++ "\n")
          putStrLn "Comb \".\" [Comb \".\" [Comb \"true\" [], Comb \"[]\" []], Comb \"[]\" []]"
          putStrLn (pretty (Comb "." [Comb "." [Comb "true" [], Comb "[]" []], Comb "[]" []]))
          putStrLn ("[[true]]" ++ "\n")
          putStrLn "Comb \".\" [Var (VarName \"H\")]"
          putStrLn (pretty (Comb "." [Var (VarName "H")]))
          putStrLn (".(H)" ++ "\n")
          putStrLn "Comb \".\" [Var (VarName \"I\"), Comb \"true\" [], Comb \"j\" [Var (VarName \"J\")]]"
          putStrLn (pretty (Comb "." [Var (VarName "I"), Comb "true" [], Comb "j" [Var (VarName "J")]]))
          putStrLn (".(I, true, j(J))" ++ "\n")
          putStrLn "Comb \".\" [Var (VarName \"K\"), Comb \".\" [Var (VarName \"L\"), Var (VarName \"M\"), Var (VarName \"N\"), Var (VarName \"O\")]]"
          putStrLn (pretty (Comb "." [Var (VarName "K"), Comb "." [Var (VarName "L"), Var (VarName "M"), Var (VarName "N"), Var (VarName "O")]]))
          putStrLn ("[K|.(L, M, N, O)]" ++ "\n")