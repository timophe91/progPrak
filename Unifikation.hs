module Unifikation where

import Type

ds :: Term -> Term -> Maybe (Term, Term)
ds (Var (VarName "_")) t                   = Nothing
ds t                   (Var (VarName "_")) = Nothing
ds (Comb "_" [])       t                   = Nothing
ds t                   (Comb "_" [])       = Nothing
ds (Var v)             t                   = if Var v == t then Nothing else Just (Var v, t)
ds t                   (Var v)             = if Var v == t then Nothing else Just (t, Var v)
ds (Comb v [])         t                   = if Comb v  [] == t then Nothing else Just (Comb v [], t)
ds t                   (Comb v [])         = if Comb v  [] == t then Nothing else Just (t, Comb v [])
ds (Comb v1 t1)        (Comb v2 t2)        = if v1 /= v2 || length t1 /= length t2 then Just (Comb v1 t1, Comb v2 t2) else help t1 t2
  where
  help []     []     = Nothing
  help (x:xs) (y:ys) = if x == y then help xs ys else Just (x, y)