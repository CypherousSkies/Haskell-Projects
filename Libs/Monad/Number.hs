{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

module Libs.Monad.Numbers where

doBin :: Monad m => (a -> b -> c) -> m a -> m b -> m c
doBin f x y = x >>= (\a -> y >>= (return . (f a)))

doMon :: Monad m => (a -> b) -> m a -> m b
doMon f x = x >>= (return . f)

instance (Num a,Monad m) => Num (m a) where
    (+) = doBin (+)
    (*) = doBin (*)
    negate = doMon negate
    abs = doMon abs
    signum = doMon signum
    fromInteger = return . fromInteger

instance (Fractional a,Monad m) => Fractional (m a) where
    fromRational = return . fromRational
    recip = doMon recip

instance (Floating a,Monad m) => Floating (m a) where
    pi = return pi
    exp = doMon exp
    log = doMon log
    sqrt = doMon sqrt
    (**) = doBin (**)
    sin = doMon sin
    cos = doMon cos
    asin = doMon asin
    acos = doMon acos
    atan = doMon atan
    sinh = doMon sinh
    cosh = doMon cosh
    asinh = doMon asinh
    acosh = doMon acosh
    atanh = doMon atanh


