module SLD2 where 
import Type
import Substitution
import Unifikation
import Rename

data SLDTree = Node (Maybe (Subst, Goal)) [SLDTree]
    deriving Show

-- Builds SLD Tree to a given program and goal.
sld :: Prog -> Goal -> SLDTree
sld _         (Goal []) = Node (Just (empty, Goal [])) []
sld (Prog []) _         = Node Nothing []
sld p         goal      = sldRec (allVars p ++ allVars goal) p goal empty (allVars goal)

    where
        sldRec :: [VarName] -> Prog -> Goal -> Subst -> [VarName] -> SLDTree
        sldRec _ _ (Goal []) fSubs fVar = Node (Just (restrictTo fSubs fVar, Goal [])) []
        sldRec used (Prog pr) g s fVar  = 
            let renamedP = renameProg pr used                                                          -- renames program
                newUsed = used ++ concatMap allVars renamedP                                                -- updates used variables
            in Node (Just (s, g)) (map (\x -> case applyRule g x of                                         -- applies rules to goal
                Nothing           -> Node Nothing []                                                        -- failure
                Just (subst, goa) -> sldRec newUsed (Prog renamedP) goa (compose subst s) fVar) renamedP)   -- new node

        -- Applies rule to a goal if a substitution exists.
        applyRule :: Goal -> Rule -> Maybe (Subst, Goal)
        applyRule (Goal (go:al)) (Rule r rs) = 
            case unify r go of 
                Nothing  -> Nothing
                Just mgu -> Just (mgu, Goal (map (apply mgu) rs ++ map (apply mgu) al))
        applyRule _ _     = Nothing

type Strategy = SLDTree -> [Subst]

renameProg :: [Rule] -> [VarName]-> [Rule]
renameProg []     _ = []
renameProg (r:rs) v = let rR = rename v r
                      in  rR : renameProg rs (v ++ allVars rR)

-- Determines all solutions using depth-first search.
dfs :: Strategy
dfs = dfsRec []

    where 
        dfsRec :: [Subst] -> SLDTree -> [Subst]
        dfsRec _ (Node Nothing _)               = []
        dfsRec list (Node (Just (s, Goal []))_) = s:list
        dfsRec list (Node (Just _) trees)       = dfsList list trees

        dfsList _ []     = []
        dfsList l (t:ts) = dfsRec l t ++ dfsList l ts



-- Determines all solutions using breadth-first search.
bfs :: Strategy
bfs t = map (\(Just (s, Goal [])) -> s) (filter isSuccess (concat $ levels t))
    
    where
        -- Lists of nodes at each level of the tree.
        levels :: SLDTree -> [[Maybe (Subst, Goal)]]
        levels (Node x xs) = [x] : foldr (longZip . levels) [] xs

        longZip :: [[Maybe (Subst, Goal)]] -> [[Maybe (Subst, Goal)]] -> [[Maybe (Subst, Goal)]] 
        longZip x [] = x 
        longZip [] y = y
        longZip (x:xs) (y:ys) = (x ++ y) : longZip xs ys

        isSuccess (Just (_, Goal [])) = True
        isSuccess _ = False

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g st = st (sld p g)