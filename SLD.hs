module SLD where

import Substitution
import Type
import Rename
import Unifikation
import Data.Maybe

data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving (Show)

type Strategy = SLDTree -> [Subst]

{- Creates a sld tree for a given Prog and Goal
-}
sld :: Prog -> Goal -> SLDTree
sld p g = sld' g []
  where
  sld' :: Goal -> [VarName] -> SLDTree
  sld' (Goal [])     _ = Node (Goal []) []                             --If the goal is empty, we r done and the node is empty
  sld' (Goal (t:ts)) v = Node (Goal (t:ts))                            --Create a new node with the current goal
    [(mgu, sld' (Goal (map (apply mgu) (ts ++ r))) (allVars mgu ++ v)) --Apply the usable rules and call sld' on the new goal
    | (Rule l r) <- renameProg p (allVars (Goal (t:ts)) ++ v)          --Rename of all rules
    , let mmgu = unify l t                                             --Check for usable rules
    , isJust mmgu
    , let mgu = fromJust mmgu]


{- Surches the sld tree für solutions using Depth-first search
-}
dfs :: Strategy
dfs = dfss empty []
  where
  dfss :: Subst -> [Subst] -> SLDTree -> [Subst]      
  dfss s lst (Node (Goal []) [])   = s : lst  -- Goal and List of tuple is empty, found a solution
  dfss _ _   (Node _ [])           = []       -- Fail, there is a goal, but no more way
  dfss s lst (Node _ t)            = dfsList s lst t -- go throug the list if there is more way
  
  dfsList :: Subst -> [Subst] -> [(Subst, SLDTree)] -> [Subst]
  dfsList _  _    []         = [] -- we r done
  dfsList s' lst' ((x,y):rt) = dfss (compose x s') lst' y ++ dfsList s' lst' rt -- take first tuple and concat the rest later

{- Surches the sld tree für solutions using Breadth-first search
-}
bfs :: Strategy
bfs t = bfs' [(empty, t)] [(empty, t)]
  where
  bfs' :: [(Subst, SLDTree)] -> [(Subst, SLDTree)] -> [Subst]
  bfs' []                        [] = []                           --If both the current List and the current level is empty all Nodes where checked
  bfs' []                        x  = let y = level x in bfs' y y  --If the current List is empty go one level deeper
  bfs' ((s,Node (Goal []) []):r) x  = s : bfs' r x                 --If the current Node is a success add its substitution to the result
  bfs' (_:xs)                    y  = bfs' xs y                    --If the current Node is not a success check the next one
  level :: [(Subst, SLDTree)] -> [(Subst, SLDTree)]
  level x = concatMap level' x                                     --Go one level deeper for every tree of the list
  level' :: (Subst, SLDTree) -> [(Subst, SLDTree)]
  level' (s, Node _ x) = map (\(y,z) -> (compose y s, z)) x        --Compose the current substitution with the substitution for the subtree for every subtree
  

{- Solve a Goal for a given Prog using the given strategy
-}
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g st = map (\x -> restrictTo x (allVars g)) (st (sld p g))  --Create a sld tree, use the strategy on it and restrict the substitutions to the variables from the Goal

{- Renames a Prog not using names given in the List
-}
renameProg :: Prog -> [VarName] -> [Rule]
renameProg (Prog x) y = [rename y z | z <- x]