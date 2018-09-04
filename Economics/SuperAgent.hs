{-# LANGUAGE ExistentialQuantification #-}

module Economics.SuperAgent
    (Money
    ,Mass
    ,Amount
    ,Identifier
    ,Tradable
    ,Occupation
    ,Agent
    ,Market
    ) where

import Libs.AssList

data Money      = Fractional a => Money a
data Mass       = Fractional a => Mass a
data Amount     = Num a => Num a
data Identifier = Eq a => Identifier a

class (Eq a, Show a) => Tradable a where
    unit_mass :: a -> Mass

data Inentory = Tradable t => AssList t Amount

class (Eq a, Show a) => Occupation a where
    needs   :: Tradable t => a -> [t]
    makes   :: Tradable t => a -> [t]
    perform :: Tradable t => a -> Inventory -> Inventory

class Agent a where
    getID :: a -> Identifier
    observedRange :: (Tradable t) => a -> t -> (Money, Money)
    observedMean  :: (Tradable t) => a -> t -> Money
    determinePriceOf :: (Tradable t) => a -> t -> Money
    favorability :: (Tradable t, Real b) => a -> t -> Money -> b
    determineBuyQuantity :: (Tradable t) => a -> t -> Amount
    determineSellQuantity :: (Tradable t) => a -> t -> Amount
    
