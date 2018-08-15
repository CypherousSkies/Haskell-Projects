module Random.Tree
        (RTree(RNode,RLeaf)
        ,evalTree
        ) where

import Random.Dist
import System.Random
import Control.Monad.Random
import Data.Tree

data RTree a = RNode (RDist a) (RDist [RTree a])
	     | RLeaf (RDist a)

evalTree :: RTree a -> Rand g (Tree a)
evalTree (RLeaf rl)    = liftRand (\g -> (Node (evalRand rl g) [], execRand rl g))
evalTree (RNode rl rf) = liftRand (\g -> (Node l f, g''))
	where
		(l,g')  = runRand rl g
		(f,g'') = runRand (mapM evalTree rf) g'
