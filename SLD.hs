module SLD where

import Substitution
import Type

newtype Edge = Edge Subst
  deriving (Show, Eq)

data SLDTree = Node Goal Edge [SLDTree]
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
sld p g = Node g (Edge empty) (newTree p g)
  where
  newTree :: Prog -> Goal -> [SLDTree]
  newTree p g = []