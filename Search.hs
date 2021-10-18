{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

data Node s a = CNode{  state::s,
                        parent::Maybe (Node s a),
                        action::Maybe a,
                        depth::Int,
                        estimCost::Float,
                        children::[Node s a]}


instance Eq s => Eq (Node s a) where
    (==) (CNode state1 _ _ _ _ _) (CNode state2 _ _ _ _ _) = state1 == state2

instance Ord s => Ord (Node s a) where
    (<=) (CNode state1 _ _ _ _ _) (CNode state2 _ _ _ _ _) = state1 <= state2


nodeState :: Node s a -> s
nodeState node= state node

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node= parent node

nodeDepth :: Node s a -> Int
nodeDepth node = depth node

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = children node

nodeHeuristic ::(ProblemState s a) => Node s a -> Float
nodeHeuristic node = h (nodeState node) 

nodeAction :: Node s a -> Maybe a
nodeAction node = action node


createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = thisNode
            where thisNode = (CNode initialState  Nothing  Nothing  0  0 (computeChildren thisNode))

computeChildren :: (ProblemState s a, Eq s) => Node s a -> [Node s a]
computeChildren parent = map (createNodeRecursively parent) (successors (nodeState parent))
                        
createNodeRecursively :: (ProblemState s a, Eq s) => Node s a -> (a,s) -> Node s a
createNodeRecursively parent (nodeaction,nodestate) = newNode
    where
        newNode = (CNode nodestate (Just parent) (Just nodeaction) (1 + (nodeDepth parent)) (estimCost parent + (h nodestate)) (computeChildren newNode))

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq


suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = filter (\thisNode -> not(S.member (nodeState thisNode) newSet)) (nodeChildren node)
    where
        newSet = S.insert (nodeState node) visited

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = PQ.insertWith min node (nodeHeuristic node) frontier -- newFrontier


insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldr (flip insertSucc) frontier (suitableSuccs node visited) --newFrontier


astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier
    | isGoal (nodeState nodAdecv) = nodAdecv
    | otherwise = astar' updatedVisited (insertSuccs  nodAdecv newFrontier updatedVisited)
        where
            nodAdecv    = fst $ deleteFindMin frontier
            newFrontier = snd $ deleteFindMin frontier
            updatedVisited = S.insert (nodeState nodAdecv) visited 
                                 

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty (PQ.insert initialNode (nodeHeuristic initialNode) PQ.empty)-- goalNode


extractPath :: Node s a -> [(a, s)]
extractPath goalNode = map createPair nodeList
                where 
                    nodeList = reverse $ take (depth goalNode) $ iterate (\node -> fromJust $ parent node) goalNode
                    createPair node = (fromJust $ nodeAction node, nodeState node)