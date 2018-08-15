module Random.Monad
	(RGened(RGened)
	) where

import System.Random

data (RandomGen g) => RGened g a = RGened g a
instance (RandomGen g) => Monad (Rgened g) where
	(>>=)::RandomGen g a -> (a -> b) -> RandomGen g b
