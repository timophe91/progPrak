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
dfs = dfss
  where
  dfss :: SLDTree -> [Subst]
  dfss s = []

bfs :: Strategy
bfs = bfss
  where
  bfss :: SLDTree -> [Subst]
  bfss s = []

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g s = []

sld :: Prog -> Goal -> SLDTree
sld p g = let p1 = renameProg p (allVars g)  in Node g (newTree p1 g p1 (allVars g))
  where
  newTree :: Prog -> Goal -> Prog -> [VarName] -> [(Subst, SLDTree)]
  newTree (Prog [])               _             _ _ = []
  newTree (Prog (Rule xt []:xs))  (Goal (y:ys)) p v = let s = unify xt y
                                                          n = newTree (Prog xs) (Goal (y:ys)) p v
                                                      in if isJust s
                                                         then (fromJust s, Node (Goal []) []) : n
                                                         else n
  newTree (Prog ((Rule xt xts):xs)) (Goal (y:ys)) p v = let s = unify xt y
                                                            n = newTree (Prog xs) (Goal (y:ys)) p v
                                                        in if isJust s
                                                           then let g1 = Goal (map (apply (fromJust s)) xts)
                                                                    v1 = v ++ allVars p
                                                                in (fromJust s, Node g1 (newTree (renameProg p v1) g1 p v1)) : n
                                                           else n

renameProg :: Prog -> [VarName] -> Prog
renameProg (Prog x) y = Prog [rename y z | z <- x]