module Random.Dist
        (RDist
        ,mkDist
        ) where

import System.Random
import Control.Monad.Random

type RDist a = Rand g a

mkDist :: Fractional b => [(a,b)] -> Rand g a
mkDist d = liftRand $ evalDist d

evalDist::RandomGen g => RDist a -> g -> (a,g)
evalDist d g = ((evalHelper d k),g')
        where
                (k,g') = (randomR (0.0,(sumof d)) g)

evalHelper::[(a,Double)] -> Double -> a
evalHelper ((x,p):xs) m = if (m - p)>0.0 then evalHelper xs (m - p) else x

sumof::[(a,Double)] -> Double
sumof [] = 0
sumof ((_,p):xs) = p + (sumof xs)
