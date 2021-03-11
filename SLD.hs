module SLD where

import Substitution
import Type
import Rename
import Unifikation
import Data.Maybe

data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving (Show)

type Strategy = SLDTree -> [Subst]

dfs :: Strategy
dfs = dfss empty []
  where
  dfss :: Subst -> [Subst] -> SLDTree -> [Subst]
  dfss s lst (Node (Goal []) [])   = s : lst
  dfss s lst (Node _ [])           = [] -- Fail
  dfss s lst (Node g t)            = dfsList s lst t
  
  dfsList :: Subst -> [Subst] -> [(Subst, SLDTree)] -> [Subst]
  dfsList _  _    []         = []
  dfsList s' lst' ((x,y):rt) = dfss (compose x s') lst' y ++ dfsList s' lst' rt


bfs :: Strategy
bfs = bfss
  where
  bfss :: SLDTree -> [Subst]
  bfss s = []

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g st = map (\x -> restrictTo x (allVars g)) (st (sld p g))

sld :: Prog -> Goal -> SLDTree
sld p g = let p1 = renameProg p (allVars g) in Node g (newTree p1 g p1 (allVars g))
  where
  newTree :: Prog -> Goal -> Prog -> [VarName] -> [(Subst, SLDTree)]
  newTree (Prog [])               _             _ _ = []
  newTree (Prog ((Rule xt []):xs))  (Goal (y:ys)) p v = let s = unify xt y
                                                            n = newTree (Prog xs) (Goal (y:ys)) p v
                                                        in if isJust s
                                                           then (fromJust s, Node (Goal []) []) : n
                                                           else n
  newTree (Prog ((Rule xt xts):xs)) (Goal (y:ys)) p v = let s = unify xt y
                                                            n = newTree (Prog xs) (Goal (y:ys)) p v
                                                        in if isJust s
                                                           then let g1 = Goal (map (apply (fromJust s)) xts ++ map (apply (fromJust s)) ys)
                                                                    v1 = v ++ allVars p
                                                                    p1 = renameProg p v1
                                                                in (fromJust s, Node g1 (newTree p1 g1 p1 v1)) : n
                                                           else n

renameProg :: Prog -> [VarName] -> Prog
renameProg (Prog x) y = Prog [rename y z | z <- x]