module Random.Dist
        (RDist
        ,evalDist
        ) where

import System.Random

type RDist a = [(a,Double)]

evalDist::RandomGen g => RDist a -> g -> (a,g)
evalDist d g = ((evalHelper d k),g')
        where
                (k,g') = (randomR (0.0,(sumof d)) g)

evalHelper::RDist a -> Double -> a
evalHelper ((x,p):xs) m = if (m - p)>0.0 then evalHelper xs (m - p) else x

sumof::RDist a -> Double
sumof [] = 0
sumof ((_,p):xs) = p + (sumof xs)

toDist::[a]->RDist a
toDist [] = []
toDist (x:xs) = (x,1.0):(toDist xs)
