module SLD where

import Substitution
import Type
import Rename
import Unifikation
import Data.Maybe

data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving (Show)

type Strategy = SLDTree -> [Subst]

sld :: Prog -> Goal -> SLDTree
sld p g = sld' g []
  where
  sld' :: Goal -> [VarName] -> SLDTree
  sld' (Goal [])     _ = Node (Goal []) []                             --If the goal is empty the node is empty
  sld' (Goal (t:ts)) v = Node (Goal (t:ts))                            --Create a new node with the current goal
    [(mgu, sld' (Goal (map (apply mgu) (ts ++ r))) (allVars mgu ++ v)) --Apply the usable rules and call sld' on the new goal
    | (Rule l r) <- renameProg p (allVars (Goal (t:ts)) ++ v)          --Rename of all rules
    , let mmgu = unify l t                                             --Check for usable rules
    , isJust mmgu
    , let mgu = fromJust mmgu]

dfs :: Strategy
dfs = dfss empty []
  where
  dfss :: Subst -> [Subst] -> SLDTree -> [Subst]
  dfss s lst (Node (Goal []) [])   = s : lst
  dfss _ _   (Node _ [])           = [] -- Fail
  dfss s lst (Node _ t)            = dfsList s lst t
  
  dfsList :: Subst -> [Subst] -> [(Subst, SLDTree)] -> [Subst]
  dfsList _  _    []         = []
  dfsList s' lst' ((x,y):rt) = dfss (compose x s') lst' y ++ dfsList s' lst' rt


bfs :: Strategy
bfs t = bfs' [(empty, t)] [(empty, t)]
  where
  bfs' :: [(Subst, SLDTree)] -> [(Subst, SLDTree)] -> [Subst]
  bfs' []                        [] = []
  bfs' []                        x  = let y = level x in bfs' y y
  bfs' ((s,Node (Goal []) []):r) x  = s : bfs' r x
  bfs' (x:xs)                    y  = bfs' xs y
  level :: [(Subst, SLDTree)] -> [(Subst, SLDTree)]
  level x = concatMap level' x
  level' :: (Subst, SLDTree) -> [(Subst, SLDTree)]
  level' (s, Node g x) = map (\(y,z) -> (compose y s, z)) x
  

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g st = map (\x -> restrictTo x (allVars g)) (st (sld p g))


renameProg :: Prog -> [VarName] -> [Rule]
renameProg (Prog x) y = [rename y z | z <- x]