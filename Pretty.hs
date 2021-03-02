import Type

{- A Class for a pretty output of datatypes
-}
class Show a => Pretty a where
  pretty ::  a -> String
  pretty a = "\"" ++ show a ++ "\"\n"


{- Pretty output Simple Prolog Terms
 - Using the Type.hs
 - pretty :: Term -> String
-}
instance Pretty Term where 
  pretty (Var (VarName v)) = v
  pretty (Comb "." [t1,t2]) = "[" ++ prettyLists t1 t2 ++ "]"
  pretty (Comb c []) = c
  pretty (Comb c t) = c ++ "(" ++ comma (map pretty t) ++ ")"
    where comma:: [String] -> String -- setting a comma between all elements and concat them to one string
          comma [x]    = x
          comma (x:xs) = (x ++ ", ") ++ comma xs


{- A function to pretty Prolog Lists
 - in their different ways
-}
prettyLists :: Term -> Term -> String
prettyLists (Var (VarName v1)) (Var (VarName v2))  = v1 ++ ", " ++ v2
prettyLists (Var (VarName v))  (Comb "." [t1, t2]) = v ++ ", " ++ prettyLists t1 t2
prettyLists (Var (VarName v))  (Comb [] [])        = v
prettyLists (Var (VarName v))  t                   = v ++ "|" ++ pretty t
prettyLists t                  (Var (VarName v))   = pretty t ++ "|" ++ v
prettyLists (Comb c [])        (Comb "[]" [])      = c
prettyLists (Comb c [])        (Comb "." [t1, t2]) = c ++ ", " ++ prettyLists t1 t2
prettyLists t                  (Comb "[]" [])      = pretty t
prettyLists t1                 t2                  = pretty t1 ++ "|" ++ pretty t2














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