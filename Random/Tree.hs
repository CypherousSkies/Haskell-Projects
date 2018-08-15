module Random.Tree
        (AccRTree(ARTrunk,ARBranch,ARLeaf)
        ,foldRTree
        ) where

import Random.Dist
import System.Random

data AccRTree a = ARTrunk (RDist (a,(AccRTree a)))
                | ARBranch (a,(RDist a))
                | ARLeaf a

foldRTree::RandomGen g => AccRTree a -> b -> g -> (a -> b -> b) -> (b,g)
foldRTree (ARTrunk d) _ g f = (f x xs,g'')
        where
                ((x,t),g') = evalDist d g
                (xs,g'') = foldRTree xs g'
foldRTree (ARBranch (x,d)) z g f = (f (f x xs) z,g')
        where
                (xs,g') = evalDist d g
foldRTree (ARLeaf a) z g f = (f a z,g)

