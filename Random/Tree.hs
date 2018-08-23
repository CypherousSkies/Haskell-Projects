module Random.Tree
        (RTree(RNode,RLeaf)
        ,evalTree
        ) where

import Control.Monad.Random
import Data.Tree
import Random.Dist
import System.Random

data RTree g a = RNode (Rand g a) (Rand g [RTree g a])
               | RLeaf (Rand g a)

evalTree :: RTree g a -> Rand g (Tree a)
evalTree (RLeaf rl) = rl >>= (\l -> return $ Node l [])
evalTree (RNode rl rf) = do { l <- rl
                            ; rf' <- rf
                            ; f <- mapM evalTree rf'
                            ; return $ Node l f
                            }
