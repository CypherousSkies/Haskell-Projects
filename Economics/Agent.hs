{-# LANGUAGE ExistentialQuantification, FunctionalDependencies,
             MultiParamTypeClasses, AllowAmbiguousTypes #-}

module Economics.Agent
        (Money
        ,Mass
        ,Amount
        ,Identifier
        ,Tradable(unit_mass,recipes,needs)
        ,Transaction(Transaction,seller,buyer,item,quantity,unit_price)
        ,Bid(Bid,bidder,thing,number,cost)
        ,Agent(getID
              ,getInventory
              ,getJob
              ,getMoney
              ,replaceMoney
              ,replaceInventory
              ,spaceInInventory
              ,updatePriceBeleifs
              ,estimateValue
              ,amountToSell
              ,amountToBuy
              ,doProduction
              ,doTurn
              )
        ,ClearingHouse(getAgents
                      ,haggle
                      ,defaultPrice
                      ,tradeHistory
                      ,updateHouse
                      ,replaceAgent
                      ,updateAgent
                      ,lastMean
                      ,doRound
                      )
        ) where

import Control.Monad.Random
import Data.Either
import Data.List
import Data.Maybe
import System.Random
import Libs.AssList
import Random.Shuffle

import Debug.Trace

type Money  = Double
type Mass   = Double
type Amount = Int
type Identifier = Int

class (Eq a,Show a) => Tradable a where
    unit_mass :: a -> Mass
    recipes :: RandomGen g => a -> [( AssList a Amount , Rand g (AssList a Amount) )]
    needs :: a -> [a]
    needs a = nub $ map fst $ join $ map fst $ map (\(a,rt) -> (a, execRand rt (mkStdGen 0))) $ recipes a

data Transaction t = Transaction { seller :: Identifier
                                 , buyer :: Identifier
                                 , item :: t
                                 , quantity :: Amount
                                 , unit_price :: Money
                                 } deriving (Eq,Show)

data Bid t = Bid { bidder :: Identifier
                 , thing :: t
                 , number :: Amount
                 , cost :: Money
                 } deriving (Eq,Show)

thingf :: Rand g Money -> Amount -> Rand g Money
thingf rm am = fmap (\x -> x * (realToFrac am)) rm

recSide :: (t -> Rand g Money) -> [(t,Amount)] -> Rand g Money
recSide est thing = fmap sum (mapM (\(t,am) -> thingf (est t) am) thing)

recVal :: (t -> Rand g Money) -> ([(t,Amount)],[(t,Amount)]) -> Rand g Money
recVal est (reac,prod) = liftM2 (\r p -> p - r) (recSide est reac) (recSide est prod)

netValue :: (t -> Rand g Money) -> [([(t,Amount)],[(t,Amount)])] -> Rand g [Money]
netValue est = mapM (recVal est)

class (Tradable t, Eq a) => Agent a t | a -> t where
    getID :: a -> Identifier
    getInventory :: a -> [(t,Amount)]
    replaceInventory :: a -> [(t,Amount)] -> a
    spaceInInventory :: a -> Mass
    getJob :: a -> t
    getMoney :: a -> Money
    replaceMoney :: a -> Money -> a
    updatePriceBeleifs :: RandomGen g  => a -> Either (Transaction t) (Bid t) -> Rand g a -- Either a transaction occured or a bid was rejected
    estimateValue :: RandomGen g => a -> t -> Rand g Money
    amountToSell :: RandomGen g => a -> t -> Maybe (Rand g (Bid t))
    amountToBuy :: RandomGen g => a -> t -> Maybe (Rand g (Bid t))
    doProduction :: RandomGen g => a -> Rand g a
    doProduction a = do { let possibleRecipes = filter (\(r,_) -> and $ map (\(t,am) -> maybe False (\v -> (am <= v) && (v /= 0)) (lookup t (getInventory a))) r) (recipes $ getJob a)
                        ; case possibleRecipes of { [] -> return $ replaceMoney a ((getMoney a) - 2)
                                                  ; pr -> do { guessRecipe <- (\(l,rl) -> fmap (zip l) (sequence rl)) $ unzip pr
                                                             ; valueRecipe <- netValue (estimateValue a) guessRecipe
                                                             ; let recAndVal = zip valueRecipe guessRecipe
                                                             ; let chosenRecipe = snd $ head $ reverse $ sortBy (\(v1,_) (v2,_) -> compare v1 v2) recAndVal
                                                             ; let removedReactants = foldl' (\inv (t,am) -> adjust (\n -> n - am) t inv) (getInventory a) (fst chosenRecipe)
                                                             ; let addProducts      = foldl' (\inv (t,am) -> adjust (\n -> n + am) t inv) removedReactants (snd chosenRecipe)
                                                             ; return $ (\a' -> replaceMoney a' ((getMoney a) - 2)) $ replaceInventory a addProducts 
                                                             }
                                                  }
                        }
    doTurn :: RandomGen g => a -> Rand g (a,[Bid t],[Bid t])
    doTurn a = do { postProd <- doProduction a
                  ; sells    <- sequence $ mapMaybe (\(k,_) -> amountToSell a k) $ getInventory postProd
                  ; buys     <- sequence $ mapMaybe (\(k,_) -> amountToBuy  a k) $ getInventory postProd
                  ; let sells' = filter (\(Bid _ _ n c) -> (n > 0) && (c > 0)) sells
                  ; let buys'  = filter (\(Bid _ _ n c) -> (n > 0) && (c > 0)) buys
                  ; return (postProd,sells',buys')
                  }

getByID :: (Tradable t, Agent a t) => [a] -> Identifier -> Maybe a
getByID [] _ = Nothing
getByID (x:xs) i = if (getID x) == i then Just x else getByID xs i

amountOf :: (Tradable t) => AssList t Amount -> t -> Amount
amountOf inv item = sum $ map snd $ filter (\(i,_) -> i == item) inv

eligableBuyer :: (Tradable t, Agent a t) => [Bid t] -> Bid t -> [a] -> Maybe (Bid t)
eligableBuyer [] _ _ = Nothing
eligableBuyer (buy:bs) sell as = do
    { buyer <- getByID as $ bidder buy
    ; let amount = realToFrac $ min (number buy) $ number sell 
    ; let charge = (cost buy) * amount
    ; if ((getMoney buyer) > charge) && ((spaceInInventory buyer) >= (amount * (unit_mass $ thing buy))) then Just buy else eligableBuyer bs sell as
    }

resolveBids :: (Tradable t, Agent a t, RandomGen g) => [Bid t] -> [Bid t] -> (Bid t -> Bid t -> (Rand g (Transaction t), Maybe (Bid t, Bool))) -> [a] -> Rand g ([Either (Transaction t) (Bid t)], [a])
resolveBids [] l _ as = return (map (\b -> (Right b)) l, as)
resolveBids l [] f as = resolveBids [] l f as
resolveBids (s:ss) bs f as = let sHas = maybe 0 id $ do { sa <- (getByID as (bidder s))
                                                        ; return $ amountOf (getInventory sa) (thing s)
                                                        }
                                 s'   = Bid (bidder s) (thing s) (min sHas (number s)) (cost s)
                              in if (elem (thing s) (map thing bs)) && (sHas > 0)
                                    then case filter (\b -> (thing b) == (thing s)) bs of 
                                           [] -> (resolveBids ss bs f as) >>= (\(rest, as') -> return $ ((Right s) : rest, as')) 
                                           b' -> case (eligableBuyer b' s' as) of 
                                               Just buy -> do 
                                                   { let (rtrans, mbb) = f s' buy
                                                   ; rt <- rtrans
                                                   ; let ss' = (maybe ss (\(bid,isSell) -> if isSell then bid:ss else ss) mbb)
                                                   ; let bs' = (maybe ss (\(bid,isSell) -> if isSell then b' else bid:b') mbb) \\ (buy:[])
                                                   ; let mas =  do { sa <- getByID as (bidder s')
                                                                   ; ba <- getByID as (bidder buy)
                                                                   ; let charge = (realToFrac $ quantity rt) * (unit_price rt) 
                                                                   ; let saM = replaceMoney sa ((getMoney sa) + charge)
                                                                   ; let sa' = replaceInventory saM $ adjust (\n -> n - (quantity rt)) (thing s') (getInventory sa)
                                                                   ; let baM = replaceMoney sa ((getMoney ba) - charge)
                                                                   ; let ba' = replaceInventory baM $ adjust (\n -> n - (quantity rt)) (thing s') (getInventory sa)
                                                                   ; return $ sa' : ba' : (as \\ (sa:ba:[]))
                                                                   }
                                                   ; case mas of
                                                       Just as' -> (resolveBids ss bs' f as') >>= (\(rb, as'') -> return ((Left rt) : rb, as''))
                                                       Nothing -> resolveBids ss bs f as
                                                   }
                                               Nothing -> resolveBids ss (bs \\ b') f as
                                else do { (rb, as) <- resolveBids ss bs f as
                                        ; return (if (sHas > 0) then rb else (Right s) : rb, as)
                                        }

updateAgents :: (RandomGen g) => (Tradable t, Agent a t) => [Either (Transaction t) (Bid t)] -> [a] -> Rand g [a]
updateAgents etbs as = mapM (\a -> do { let filtered = filter (either (\t -> ((seller t) == (getID a)) || ((buyer t) == (getID a))) (\b -> (bidder b) == (getID a))) etbs
                                      ; foldM (\a' etb -> updatePriceBeleifs a' etb) a filtered
                                      }) as

turnMean :: (Tradable t) => t -> [Transaction t] -> Maybe Money
turnMean t l = let items  = filter (\ti -> (item ti) == t) l
                   pairs  = map (\ti -> (unit_price ti, quantity ti)) items
                   total  = sum $ map (\(_,am) -> (realToFrac am)) pairs
                   weight = sum $ map (\(x,am) -> (realToFrac x) * (realToFrac am)) pairs
                in if total <= 0 then Nothing else Just $ weight / total

class (Tradable t, Agent a t) => ClearingHouse c a t | c -> t, c -> a where
        getAgents :: c -> [a]
        getAgentByID :: c -> Identifier -> Maybe a
        getAgentByID c = getByID (getAgents c)
        haggle :: (RandomGen g) => c -> Bid t -> Bid t -> (Rand g (Transaction t), Maybe (Bid t, Bool)) -- -> sell -> buy; If bool is true, then is a SELL
        defaultPrice :: c -> t -> Money
        tradeHistory :: c -> [[Transaction t]]
        updateHouse :: c -> [a] -> [Transaction t] -> c
        lastMean :: c -> t -> Money
        lastMean c t = case (tradeHistory c) of [] -> defaultPrice c t
                                                (x:_) -> maybe (defaultPrice c t) id $ turnMean t x
        replaceAgent :: (RandomGen g) => c -> [(t,Amount)] -> Identifier -> Rand g a
        updateAgent :: (RandomGen g) => c -> a -> Rand g a
        doRound :: (RandomGen g) => c -> Rand g c
        doRound c = do { asbp <- mapM doTurn $ getAgents c
                       ; let (agents,ss,bs) = (\(as,sl,bl) -> (as, concat sl, concat bl)) $ unzip3 asbp
                       ; sells <- shuffle ss
                       ; buys  <- shuffle bs
                       ; let sortSells = sortBy (\s1 s2 -> compare (cost s1) (cost s2)) sells
                       ; let sortBuys  = reverse $ sortBy (\b1 b2 -> compare (cost b1) (cost b2)) buys
                       ; (resolved, a') <- resolveBids sortSells sortBuys (haggle c) agents
                       ; updatedAgents  <- updateAgents resolved a'
                       ; let transactions = lefts resolved
                       ; let excessDemand = map (\t -> (t, (sum $ map number $ filter (\bid -> t == (thing bid)) buys) - (sum $ map number $ filter (\bid -> t == (thing bid)) sells))) $ nub $ map thing (buys ++ sells)
                       ; newAgents <- mapM (\a -> if (getMoney a) <= 0 then replaceAgent c excessDemand (getID a) else return a) updatedAgents
                       ; let uh = updateHouse c newAgents transactions
                       ; newAgents' <- mapM (\a -> updateAgent uh a) newAgents
                       ; return $ updateHouse c newAgents' transactions
                       }
