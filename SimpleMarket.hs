{-# LANGUAGE InstanceSigs, MultiParamTypeClasses #-}

import Control.Monad.Random
import Data.List
import Economics.Agent
import Libs.AssList
import Random.Dist

import Debug.Trace

type Components = AssList Commodity Amount
type Recipe g = (Components, Rand g Components)

data Commodity = Food | Wood | Ore | Metal | Tool deriving (Show, Read, Eq)
instance Tradable Commodity where
        unit_mass _ = 1
        recipes Food = create [(Wood, 1)] (Food, 2)
        recipes Ore = create [(Food, 1)] (Ore, 2)
        recipes Metal = refine [(Food, 1)] Ore (Metal, 1) 2 20
        recipes Wood = create [(Food, 1)] (Wood, 2)
        recipes Tool = map (\n -> ([(Metal, n), (Food, 1)], return [(Tool, n)])) [1..20]

toolUsed :: RandomGen g => Components -> Rand g Components
toolUsed woTool = mkDist [((Tool, 1) : woTool, 0.9), (woTool, 0.1)]

create :: RandomGen g => Components -> (Commodity,Amount) -> [Recipe g]
create ins (out, n) = (ins, return $ [(out, n)]) : (ins, toolUsed $ [(out, 2 * n)]):[]

refine :: RandomGen g => Components -> Commodity -> (Commodity,Amount) -> Amount -> Amount -> [Recipe g]
refine base limiting (out, ratio) maxWOTool max = let woTool = map (\n -> ((limiting, n) : base, return [(out, ratio * n)])) [1..maxWOTool]
                                                      wTool  = map (\n -> ((limiting, n) : (Tool, 1) : base, toolUsed [(out, ratio * n)])) [1..max]
                                                   in woTool ++ wTool


showjob :: Commodity -> String
showjob Food = "Farmer"
showjob Ore = "Miner"
showjob Metal = "Refiner"
showjob Wood = "Lumberjack"
showjob Tool = "Blacksmith"

changeRange :: Double -> (Money,Money) -> (Money, Money)
changeRange p (a, b) = (a - p * 0.5 * (b - a), b + p * 0.5 * (b - a))

succSell :: Money -> (Money,Money) -> (Money,Money)
succSell mu = changeRange (0 - 0.05)

failToSell :: RandomGen g => Money -> (Money,Money) -> Money -> Rand g (Money,Money)
failToSell mu r has = do 
    { let p = 0 --p <- liftRand $ randomR (0, 0.8)
    ; return $ (\(a,b) -> ((1-p) * a + p * (mu - a), (1-p) * b + p * (mu - b))) $ changeRange p r
    }

logistic :: Double -> Double -> Double ->  Double
logistic mu s x = 1 / (1 + (exp ((mu - x) / s)))

favorability :: Maybe (Money, Money) -> Money -> Double
favorability (Just (a,b)) v = min 1 $ max 0 $ ((0.95 - 0.05) / (b - a)) * (v - a) + 0.05
favorability Nothing _ = 0
--favorability (Just (a,b)) v = if a == b then 0.5 else logistic ((b + a)/2) ((a - b) / 8) v

upb :: (RandomGen g) => Person -> Either (Transaction Commodity) (Bid Commodity) -> Rand g Person
upb (Person id inv ranges job money market) (Left (Transaction _ _ item _ _)) = return $ Person id inv (update (\r -> Just (succSell (lastMean market item) r)) item ranges) job money market
upb (Person id inv ranges job money market) (Right (Bid _ item amount _))     = do
    { r' <- mapM (\(c,r) -> if c == item then (failToSell (lastMean market item) r money) >>= (\k -> return (c,k)) else return (c,r)) ranges
    ; return $ Person id inv r' job money market
    }

inventoryMass :: AssList Commodity Amount -> Mass
inventoryMass inv = sum $ map (\(c,a) -> (realToFrac a) * (unit_mass c)) inv

amountOf :: AssList Commodity Amount -> Commodity -> Amount
amountOf inv item = sum $ map snd $ filter (\(i,_) -> i == item) inv

type Inventory = AssList Commodity Amount

data Person = Person { ident :: Identifier
                     , inv :: Inventory
                     , price_ranges :: AssList Commodity (Money,Money)
                     , job :: Commodity
                     , money :: Money
                     , market :: Market
                     }
instance Eq Person where
    (Person i1 _ _ _ _ _) == (Person i2 _ _ _ _ _) = i1 == i2
instance Agent Person Commodity where
    { getID a = ident a
    ; getInventory a = inv a
    ; getJob a = job a
    ; getMoney a = money a
    ; replaceInventory (Person id _ pr j mon mar) inv = Person id inv pr j mon mar
    ; replaceMoney (Person id inv pr j _ mar) money = Person id inv pr j money mar
    ; spaceInInventory a = realToFrac $ 20 - (inventoryMass $ getInventory a)
    ; estimateValue (Person _ _ pr _ _ _) c = return $ (\(a,b) -> (a + b) / 2) $ maybe (0,0) id $ lookup c pr
    ; updatePriceBeleifs = upb
    ; amountToSell (Person id inv ranges job _ market) item = do 
        { let price  = maybe (defaultPrice market item) snd (lookup item ranges)
        ; let amount = if (item == job)
                          then amountOf inv item
                          else ceiling $ (favorability (lookup item ranges) (lastMean market item)) * (realToFrac $ amountOf inv item)
        ; if (amount > 0) && (price > 0) then Just $ return $ Bid id item amount price else Nothing
        }
    ; amountToBuy (Person id inv ranges job money market) item = if (item `elem` (needs job)) && (not $ member item ranges)
                                                                    then Nothing
                                                                    else do
                                                                        { let price  = maybe (defaultPrice market item) fst (lookup item ranges)
                                                                        ; let top    = if item `elem` (needs job) then 1 + (favorability (lookup item ranges) (lastMean market item)) else (min 0.5 $ money / price)
                                                                        ; let amount = floor $ (top - (favorability (lookup item ranges) (lastMean market item))) * ((spaceInInventory (Person id inv ranges job money market)) / (unit_mass item))
                                                                        ; let bool = (amount > 0) && (price > 0)
                                                                        ; if bool then Just $ return $ Bid id item amount price else Nothing 
                                                                        }
    }
instance Show Person where
    show (Person i _ _ j _ _) = "Person #" ++ (show i) ++ " the " ++ (show j) ++ " producer\n"

data Market = Market { population :: [Person]
                     , history :: [[Transaction Commodity]]
                     , defaults :: AssList Commodity Money
                     }
instance ClearingHouse Market Person Commodity where
    getAgents (Market pop _ _) = pop
    haggle _ sell buy = let value = ((cost sell) + (cost buy)) / 2
                            delta = (number sell) - (number buy)
                            agent = if delta > 0 then sell else buy
                         in if delta == 0
                               then (return $ Transaction (bidder sell) (bidder buy) (thing buy) (number sell) value, Nothing)
                               else (return $ Transaction (bidder sell) (bidder buy) (thing buy) (min (number sell) (number buy)) value
                                    , Just (Bid (bidder agent) (thing buy) (abs delta) (cost agent), delta > 0))
    defaultPrice (Market _ _ def) t = maybe 1 id (lookup t def)
    tradeHistory (Market _ his _) = his
    updateHouse (Market _ old def) pop his = Market pop (his:old) (map (\(c,v) -> (c, 0.8 * v + 0.2 * (lastMean (Market pop old def) c))) def)
    replaceAgent this [] ident = let coms = map (\(c,_) -> (c,1)) $ defaults this
                                  in do
                                      { job <- mkDist coms
                                      ; p <- liftRand $ randomR (0.05,0.5)
                                      ; return $ Person ident (map (\(c,_) -> (c,0)) coms) (map (\(c,m) -> (c, (m * (1-p), m * (1+p)))) $ defaults this) job 100 this
                                      }
    replaceAgent this supdem ident = let job = fst $ head $ reverse $ sortBy (\(_,am1) (_,am2) -> compare am1 am2) supdem
                                      in return $ Person ident ((map (\(c,_) -> (c,0))) $ defaults this) (map (\(c,m) -> (c, (m * 0.2, m * 1.8))) $ map (\(c,v) -> (c, if v /= 0 then v else maybe 1 id (lookup c $ defaults this))) $ map (\(c,_) -> (c,lastMean this c)) $ defaults this) job 150 this
    updateAgent this (Person i inv pr j m _) = return $ Person i inv pr j m this

emptyPerson :: Int -> Person
emptyPerson k = Person k [] [] Tool (0-10) (Market [] [] [])

mkMarket :: RandomGen g => Int -> AssList Commodity Money -> Rand g Market
mkMarket n def = (mapM (replaceAgent (Market [] [] def) []) [1..n]) >>= (\p -> return $ Market p [] def)

allComs :: [Commodity]
allComs = [Food,Wood,Ore,Metal,Tool]

doNRounds :: (RandomGen g) => Market -> Int -> Rand g Market
doNRounds mar n = foldM (\m _ -> doRound m) mar [1..n]

defaultMarket :: (RandomGen g) => Rand g Market
defaultMarket = do
    { vs <- mapM (\c -> liftRand $ randomR (1.0,20.0)) allComs
    ; mkMarket 100 (zip allComs vs)
    }

currentPrices :: Market -> AssList Commodity Money
currentPrices m = map (\c -> (c, lastMean m c)) allComs

mean :: [(Amount,Money)] -> Money
mean [] = 0
mean xs = (\(ams,mos) -> (sum $ zipWith (\a b -> (realToFrac a) * (realToFrac b)) ams mos) / (realToFrac (sum ams))) $ unzip xs

formatHistory :: [[Transaction Commodity]] -> String
formatHistory transes = foldl' (\acc b -> acc ++ "\n" ++ (head b) ++ (foldl' (\acc' b' -> acc' ++ ", " ++ b') "" (tail b))) "" $ map (\(c,ms) -> (show c) : (map show ms)) $ formatHelper transes

formatHelper :: [[Transaction Commodity]] -> [(Commodity,[Money])]
formatHelper transes = map (\(c,m) -> (c, reverse m)) $ map (\c -> (c, map (mean . map (\(Transaction _ _ _ q u) -> (q, u)) . filter (\(Transaction _ _ c' _ _) -> c' == c)) transes)) allComs

printMarket :: Market -> IO ()
printMarket test = putStrLn $ formatHistory $ tradeHistory test

start :: IO Market
start = getStdRandom $ runRand defaultMarket

gofu :: Int -> Market -> IO Market
gofu n market = do { test <- getStdRandom $ runRand (doNRounds market n)
                   --; putStrLn $ show $ map (\(c,ms) -> (c,head $ reverse ms)) $ formatHelper $ tradeHistory test --Show Prices
                   --; putStrLn $ show $ map (map (\(Transaction _ _ i q c) -> (i,q,c))) $ tradeHistory test
                   --; let thing = formatHistory $ tradeHistory test
                   --; putStrLn $ seq test thing
                   --; writeFile "MarketTest.csv" $ thing
                   --; putStrLn "DONE!!!!"
                   --; putStrLn $ show $ map (\(Person _ _ r _ _ _) -> r) $ population test
                   --; putStrLn $ show $ map (\(Person _ i _ _ _ _) -> i) $ population test --Print inventories
                   --; putStrLn $ show $ map (\(Person _ _ _ j m _) -> (showjob j, m)) $ population test --Print Money
                   ; return test
                   }
