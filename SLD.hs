module SLD where

import Substitution
import Type

newtype Edge = Edge Subst
  deriving (Show, Eq)

data SLDTree = Node Goal Edge [SLDTree]
  deriving (Show)

sld :: Prog -> Goal -> SLDTree
sld p g = Node g (Edge empty) (newTree p g)
  where
  newTree :: Prog -> Goal -> [SLDTree]
  newTree (Prog (Rule t x:ps)) g = tree (Rule t x) g : newTree (Prog ps) g
  tree r g = Node g (Edge empty) []