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
sld (Prog (x:xs)) g = let p1 = Prog [rename (allVars g) p| p <- x:xs] in Node g (newTree p1 g p1)
  where
  newTree :: Prog -> Goal -> Prog ->[(Subst, SLDTree)]
  newTree (Prog [])               _             _ = []
  newTree (Prog (Rule xt []:xs))  (Goal (y:ys)) p = let s = unify xt y in if isJust s then let g1 = Goal [apply (fromJust s) z | z <- y:ys] in (fromJust s, Node (Goal [apply (fromJust s) xt]) []) : newTree (Prog xs) (Goal (y:ys)) p else newTree (Prog xs) (Goal (y:ys)) p
  newTree (Prog (Rule xt xts:xs)) (Goal (y:ys)) p = let s = unify xt y in if isJust s then let g1 = Goal [apply (fromJust s) z | z <- y:ys] in (fromJust s, Node (Goal (map (apply (fromJust s)) xts)) (newTree (Prog xs) g1 p)) : newTree (Prog xs) (Goal (y:ys)) p else newTree (Prog xs) (Goal (y:ys)) p