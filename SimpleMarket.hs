{-# LANGUAGE InstanceSigs, MultiParamTypeClasses #-}

import Control.Monad.Random
import Data.List
import Economics.Agent
import Libs.AssList
import Random.Dist

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

succSell :: (Money,Money) -> (Money,Money)
succSell r = changeRange (0 - 0.05) r

failToSell :: Money -> (Money,Money) -> (Money,Money)
failToSell mu = (\(a,b) -> (0.95 * a + 0.05 * mu, 0.95 * b + 0.05 * mu)) . changeRange 0.05

logistic :: Double -> Double -> Double ->  Double
logistic mu s x = 1 / (1 + (exp ((mu - x) / s)))

favorability :: Maybe (Money, Money) -> Money -> Double
favorability (Just (a,b)) v = logistic ((b + a)/2) ((a - b)/(2 * (log ((1/0.95) - 1)))) v
favorability Nothing _ = 0

upb :: Person -> Either (Transaction Commodity) (Bid Commodity) -> Rand g Person
upb (Person id inv ranges job money market) (Left (Transaction _ _ item _ _)) = return $ Person id inv (update (\r -> Just (succSell                          r)) item ranges) job money market
upb (Person id inv ranges job money market) (Right (Bid _ item amount _))     = return $ Person id inv (update (\r -> Just (failToSell (lastMean market item) r)) item ranges) job money market

inventoryMass :: AssList Commodity Amount -> Mass
inventoryMass inv = sum $ map (\(c,a) -> (realToFrac a) * (unit_mass c)) inv

spaceInInventory :: AssList Commodity Amount -> Mass
spaceInInventory inv = realToFrac $ 20 - (inventoryMass inv)

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

instance Agent Person Commodity where {
                                      getID a = ident a ;
                                      getInventory a = inv a ;
                                          getJob a = job a ;
                                          getMoney a = money a ;
                                      replaceInventory (Person id _ pr j mon mar) inv = Person id inv pr j mon mar ;
                                      replaceMoney (Person id inv pr j _ mar) money = Person id inv pr j money mar ;
                                      estimateValue (Person _ _ pr _ _ _) c = return $ (\(a,b) -> (a + b) / 2) $ maybe (0,0) id $ lookup c pr ;
                                      updatePriceBeleifs = upb ;
                                      amountToSell (Person id inv ranges job _ market) item = if (item `elem` (needs job)) && (not $ member item ranges)
                                                                                                then Nothing
                                                                else do { let price  = maybe 1 snd (lookup item ranges)
                                                                        ; let amount = abs $ ceiling $ (favorability (lookup item ranges) (lastMean market item)) * (realToFrac $ amountOf inv item)
                                                                        ; Just $ return $ Bid id item amount price
                                                                        } ;
                                          amountToBuy (Person id inv ranges job money market) item = if (item == job)
                                                                then Nothing
                                                                else do { let price  = maybe 1 fst (lookup item ranges)
                                                                        ; let max    = if item `elem` (needs job) then 1 else money / (2 * price)
                                                                        ; let amount = abs $ floor $ (max - (favorability (lookup item ranges) (lastMean market item))) * (spaceInInventory inv)
                                                                        ; Just $ return $ Bid id item amount price
                                                                        } ;
                                      }
instance Show Person where
    show (Person i _ _ j _ _) = "Person #" ++ (show i) ++ " the " ++ (show j) ++ " producer\n"

data Market = Market { population :: [Person]
                     , history :: [[Transaction Commodity]]
                     , defaults :: AssList Commodity Money
                     }
instance ClearingHouse Market Person Commodity where
    getAgents (Market pop _ _) = pop
    haggle _ sell buy = let value = ((cost sell) - (cost buy)) / 2
                            delta = (number sell) - (number buy)
                            agent = if delta > 0 then sell else buy
                         in if delta == 0
                               then (return $ Transaction (bidder sell) (bidder buy) (thing buy) (number sell) value, Nothing)
                               else (return $ Transaction (bidder sell) (bidder buy) (thing buy) (min (number sell) (number buy)) value
                                    , Just (Bid (bidder agent) (thing buy) (abs delta) (cost agent), delta > 0))
    defaultPrice (Market _ _ def) t = maybe 1 id (lookup t def)
    tradeHistory (Market _ his _) = his
    updateHouse (Market _ old def) pop his = Market pop (his:old) def
    replaceAgent this [] ident = let coms = map (\(c,_) -> (c,1)) $ defaults this
                                  in (mkDist coms) >>= (\job -> return $ Person ident (map (\(c,_) -> (c,5)) coms) (map (\(c,m) -> (c, (m * 0.2, m * 1.8))) $ defaults this) job 1000 this)
    replaceAgent this supdem ident = let job = fst $ head $ sortBy (\(_,am1) (_,am2) -> compare am1 am2) supdem
                                      in return $ Person ident (map (\(c,_) -> (c,5)) $ defaults this) (map (\(c,m) -> (c, (m * 0.2, m * 1.8))) $ defaults this) job 500 this
    updateAgent this (Person i inv pr j m _) = return $ Person i inv pr j m this

emptyPerson :: Int -> Person
emptyPerson k = Person k [] [] Tool (0-10) (Market [] [] [])

mkMarket :: Int -> AssList Commodity Money -> Market
mkMarket n def = evalRand ((mapM (replaceAgent (Market [] [] def) []) [1..n]) >>= (\p -> return $ Market p [] def)) (mkStdGen 0)

allComs :: [Commodity]
allComs = [Food,Wood,Ore,Metal,Tool]

doNRounds :: (RandomGen g) => Market -> Int -> Rand g Market
doNRounds mar n = foldM (\m _ -> doRound m) mar [1..n]

defaultMarket :: Market
defaultMarket = mkMarket 10 (map (\c -> (c,20)) allComs)

currentPrices :: Market -> AssList Commodity Money
currentPrices m = map (\c -> (c,lastMean m c)) allComs

mean :: [(Amount,Money)] -> Money
mean [] = 0
mean xs = (\(ams,mos) -> (sum $ zipWith (\a b -> (realToFrac a) * (realToFrac b)) ams mos) / (realToFrac (sum ams))) $ unzip xs

formatHistory :: [[Transaction Commodity]] -> String
formatHistory transes = foldl' (\acc b -> acc ++ "\n" ++ (head b) ++ (foldl' (\acc' b' -> acc' ++ ", " ++ b') "" (tail b))) "" $ map (\(c,ms) -> (show c) : (map show ms)) $ formatHelper transes

formatHelper :: [[Transaction Commodity]] -> [(Commodity,[Money])]
formatHelper transes = map (\(c,m) -> (c, reverse m)) $ map (\c -> (c, map (mean . map (\(Transaction _ _ _ q u) -> (q, u)) . filter (\(Transaction _ _ c' _ _) -> c' == c)) transes)) allComs

main :: IO ()
main = do { gen0 <- getStdGen 
          ; let test = evalRand (doNRounds defaultMarket 1000) gen0
          --; putStrLn $ show $ tradeHistory test
          ; let thing = formatHistory $ tradeHistory test
          --; putStrLn $ seq test thing
          ; writeFile "SimpleMarketTest.csv" $ thing
          ; putStrLn "DONE!!!!"
          ; putStrLn $ show $ map (\(Person _ _ _ _ m _) -> m) $ population test
          }
