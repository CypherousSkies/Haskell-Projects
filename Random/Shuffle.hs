module Random.Shuffle
    (shuffle
    ) where

import System.Random
import Control.Monad.Random

shuffle :: (RandomGen g) => [a] -> Rand g [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle l = do
    n <- pickElem l
    ls <- shuffle $ withoutN n l
    return $ (l !! n) : ls

pickElem :: (RandomGen g) => [a] -> Rand g Int
pickElem l = liftRand $ randomR (0,(length l) - 1)

withoutN :: Int -> [a] -> [a]
withoutN _ [] = []
withoutN n (l:ls) = if n == 0
                   then ls
                   else l : (withoutN (n - 1) ls)
