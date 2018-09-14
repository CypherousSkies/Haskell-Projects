module Random.Dist
        (mkDist
        ) where

import System.Random
import Control.Monad.Random

mkDist :: (Real b, Fractional b, RandomGen g) => [(a,b)] -> Rand g a
mkDist [(x,_)] = return x
mkDist d = liftRand $ evalDist (normalize d)

normalize :: (Ord b, Num b) => [(a,b)] -> [(a,b)]
normalize d = let min = minimum $ map snd d
               in if min >= 0 then d else map (\(a,v) -> (a,v - (min + min))) d

evalDist :: (Real b, Fractional b, RandomGen g) => [(a,b)] -> g -> (a,g)
evalDist d g = ((evalHelper d k),g')
        where
            range = (0.0, sumof d)
            (k,g') = (randomR range g)

evalHelper :: (Real b, Fractional b) => [(a,b)] -> Double -> a
evalHelper ((x,p):xs) m = if ((realToFrac m) - (realToFrac p)) <= 0 then x else if (null xs) then x else evalHelper xs ((realToFrac m) - (realToFrac p))

sumof :: Real b => [(a,b)] -> Double
sumof [] = 0
sumof ((_,p):xs) = (realToFrac p) + (realToFrac (sumof xs))
