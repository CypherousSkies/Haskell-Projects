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

data Money a      = Fractional a => Money a
instance Num (Money a) where
    (Money x) + (Money y) = Money (x + y)
    (Money x) * (Money y) = Money (x * y)
    abs (Money x) = Money (abs x)
    signum (Money x) = Money (signum x)
    fromInteger i = Money (fromInteger i)
    negate (Money x) = Money (negate x)
instance Fractional (Money a) where
    fromRational r = Money (fromRational r)
    recip (Money r) = Money (recip r)

data Mass a       = Fractional a => Mass a
instance Num (Mass a) where
    (Mass x) + (Mass y) = Mass (x + y)
    (Mass x) * (Mass y) = Mass (x * y)
    abs (Mass x) = Mass (abs x)
    signum (Mass x) = Mass (signum x)
    fromInteger i = Mass (fromInteger i)
    negate (Mass x) = Mass (negate x)
instance Fractional (Mass a) where
    fromRational r = Mass (fromRational r)
    recip (Mass r) = Mass (recip r)

data Amount a     = Num a => Amount a
instance Num (Amount a) where
    (Amount x) + (Amount y) = Amount (x + y)
    (Amount x) * (Amount y) = Amount (x * y)
    abs (Amount x) = Amount (abs x)
    signum (Amount x) = Amount (signum x)
    fromInteger i = Amount (fromInteger i)
    negate (Amount x) = Amount (negate x)

data Identifier a = Eq a => Identifier a deriving Eq

class (Eq a, Show a) => Tradable a where
    unit_mass :: a -> Mass b

data Inventory t k = (Tradable t,Num k) => Inventory (AssList t (Amount k))

class (Eq a, Show a) => Occupation a where
    needs   :: Tradable t => a -> [t]
    makes   :: Tradable t => a -> [t]
    perform :: Tradable t => a -> Inventory t -> Inventory t

amountOf :: Eq t => Inventory t k -> t -> Amount k
amountOf (Inventory inv) t = maybe (Amount 0) id $ lookup t inv

data Offer t m e a = (Tradable t) => Offer { bidder :: Identifier e
                                           , isSell :: Bool
                                           , item   :: t
                                           , price  :: Money m
                                           , amount :: Amount a
                                           }

data Transaction t m e a = (Tradable t) => Transaction { buyer :: Identifier e
                                                       , seller :: Identifier e
                                                       , item :: t
                                                       , price :: Money m
                                                       , amount :: Amount a
                                                       }

class (Show a) => Agent a where
    getID :: a -> Identifier i
    getInv :: a -> Inventory t
    getLastMoney :: a -> Money m
    getJob :: (Occupation o) => a -> o
    setInventory :: a -> Inventory t -> a
    setMoney :: a -> Money m -> Money m
    observedRange :: (Tradable t) => a -> t -> (Money m, Money m)
    observedMean  :: (Tradable t) => a -> t -> Money m
    determinePriceOf :: (Tradable t) => a -> t -> Money m
    favorability :: (Tradable t) => a -> t -> Money m -> Maybe (Money m)
    idealAmount :: (Tradable t) => a -> t -> Amount k
    determineBuyQuantity :: (Tradable t) => a -> t -> Amount k
    determineBuyQuantity a t = case (favorability a t $ observedMean a t) of
                                 Just (Money m) -> Amount $ max 0 $ ceiling $ (1 - m) * ((amountOf (getInv a) t) - (idealAmount a t))
                                 Nothing -> Amount 0
    determineSellQuantity :: (Tradable t) => a -> t -> Amount k
    determineSellQuantity a t = case (favorability a t $ observedMean a t) of
                                  Just (Money m) -> Amount $ max 0 $ floor $ m * ((idealAmount a t) - (amountOf (getInv a) t))
                                  Nothing -> Amount 0
    createAsk :: (Traddable t,Num k) => a -> t -> Amount k -> Offer t m e k i
    createBid :: (Traddable t,Num k) => a -> t -> Amount k -> Offer t m e k i
    generateOffers :: (Tradable t) => a -> t -> [Offer t m e k i]
    updatePrieceModel :: a -> Either (Offer t m e k) (Transaction t m e k) -> a
    doRound :: a -> (a,[Offer t m e k])
instance Eq (Agent a) where
    a == b = (getID a) == (getID b)

class Market a where
    getGoods :: (Tradable t) => a -> [t]
    getAgents :: (Agent m) => a -> [m]
    replaceAgent :: (Agent m) => a -> m -> m
    getAvgHistPrice :: (Tradable t,Fractional m) => a -> t -> Money m
    getHottestGood :: (Tradable t) => a -> t
    getMostProfitableAgentClass :: (Occupation o) => a -> o
    updateMarket :: (Agent m) => a -> [m] -> a
    doTurn :: a -> a
