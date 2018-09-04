module Libs.Misc
    (doNTimes
    ,traceInPlace
    ) where

doNTimes :: (a -> a) -> a -> Int -> a
doNTimes _ x 0 = x
doNTimes f x i = doNTimes f (f x) (i - 1)


