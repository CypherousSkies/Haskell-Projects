module Random.Dist
        (mkDist
        ) where

import System.Random
import Control.Monad.Random

mkDist :: (Real b, Fractional b, RandomGen g) => [(a,b)] -> Rand g a
mkDist d = liftRand $ evalDist d

evalDist :: (Real b, Fractional b, RandomGen g) => [(a,b)] -> g -> (a,g)
evalDist d g = ((evalHelper d k),g')
        where
            range = (0.0, sumof d)
            (k,g') = (randomR (0.0,sumof d) g)

evalHelper :: (Real b, Fractional b) => [(a,b)] -> Double -> a
evalHelper ((x,p):xs) m = if ((realToFrac m) - (realToFrac p)) > 0 then evalHelper xs ((realToFrac m) - (realToFrac p)) else x

sumof :: Real b => [(a,b)] -> Double
sumof [] = 0
sumof ((_,p):xs) = (realToFrac p) + (realToFrac (sumof xs))
