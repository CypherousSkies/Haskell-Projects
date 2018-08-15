import Economics.Agent
import Data.HashMap.Lazy

data Commodities = Food | Wood | Ore | Metal | Tool deriving (Show, Read, Eq)
instance Tradable Commodities where
        unit_mass _ = 1

toolUsed::(RandomGen g) => Inventory Commodities -> Rand g (Inventory Commodities)
toolUsed inv = if (amountOf Tool inv) <= 0
                  then return inv
                  else do
                          p <- getRandomR (0.0,1.0) :: (RandomGen g) => Rand g Double
                          return (if (p > 0.9) then subFromInv (Tool,1) inv else inv)

data Jobs = Farmer | Miner | Refiner | Lumberjack | Blacksmith deriving (Show, Read, Eq)
instance Occupation Jobs Commodities where
        needs Farmer = [(Wood,1)]
        needs Miner = [(Food,1)]
        needs Refiner = [(Ore,1),(Food,1)]
        needs Lumberjack = [(Food,1)]
        needs Blacksmith = [(Metal,1),(Food,1)]
        makes Farmer = [Food]
        makes Miner = [Ore]
        makes Refiner = [Metal]
        makes Lumberjack = [Wood]
        makes Blacksmith = [Tool]
        produce Farmer inv = if (needsMet Farmer inv)
                                    then if (amountOf Tool inv) > 0
                                            then (toolUsed . (addToInv (Food,4)) . (subFromInv (Wood,1))) inv
                                            else return (((addToInv (Food,2)) . (subFromInv (Wood,1))) inv)
                                    else return inv
        produce Miner inv = if (needsMet Miner inv)
                                     then if (amountOf Tool inv) > 0
                                            then (toolUsed . (addToInv (Ore,4)) . (subFromInv (Food,1))) inv
                                            else return (((addToInv (Ore,2)) . (subFromInv (Food,1))) inv)
                                     else return inv
        produce Refiner inv = if (needsMet Refiner inv)
                                       then if (amountOf Tool inv) > 0
                                             then (toolUsed . (addToInv (Metal,(amountOf Ore inv))) . (repInInv (Ore,0)) . (subFromInv (Food,1))) inv
                                            else return (((addToInv (Metal,(min (amountOf Ore inv) 2))) . (subFromInv (Ore,(min (amountOf Ore inv) 2))) . (subFromInv (Food,1))) inv)
                                       else return inv
        produce Lumberjack inv = if (needsMet Lumberjack inv)
                                        then if (amountOf Tool inv) > 0
                                            then (toolUsed . (addToInv (Wood,2)) . (subFromInv (Food,1))) inv
                                            else return (((addToInv (Wood,1)) . (subFromInv (Food,1))) inv)
                                        else return inv
        produce Blacksmith inv = if (needsMet Blacksmith inv)
                                         then return (((addToInv (Tool,(amountOf Metal inv))) . (repInInv (Metal,0)) . (subFromInv (Food,1))) inv)
                                         else return inv

changeRange :: Double -> (Money,Money) -> Money
changeRange p (a,b) = (a - p * 0.5 * (b - a), b + p * 0.5 * (b - a)) 

succSell :: (Money,Money) -> (Money,Money)
succSell r = changeRange -0.05 r

failToSell :: Money -> (Money,Money) -> (Money,Money)
failToSell mu = (\(a,b) -> (0.95 * a + 0.05 * mu, 0.95 * b + 0.05 * mu)) . changeRange 0.05

logistic :: Double -> Double -> Double ->  Double
logistic mu s x = 1 / (1 + (exp ((mu - x) / s)))

favorability :: Maybe (Money, Money) -> Money -> Double
favorability (Just (a,b)) = logistic ((b + a)/2) ((a - b)/(2 * (log ((1/0.95) - 1))))
favorability Nothing _ = 0

extractInv :: Inventory Commodity -> [(Commodity,Amount)]
extractInv (Inventory inv) = inv

allComs :: [Commodity]
allComs = [Food, Wood, Ore, Metal, Tools]

data Person = Person {ident :: Identifier
		     ,inv :: Inventory Commodity
		     ,price_ranges :: HashMap Commodity (Money,Money)
		     ,job :: Jobs
		     ,money :: Money
		     ,market :: Market
		     }
instance Agent Person Jobs Commodities where
	getID = ident
	getInventory = inv
	spaceInInventory inv = 20 - (inventoryMass inv)
	getJob = job
	getMoney = money 
	updatePriceBeleifs (Person id inv ranges job money market) (Left (Transaction _ _ item _ _)) = return (Person id inv (update (\r ->Just (succSell r)) ranges item price_ranges) job money market)
	updatePriceBeleifs (Person id inv ranges job money market) (Right (Bid item Amount Money)) = return (Person id inv (update (\r -> Just (failToSell (lastMean market) r)) item price_ranges) job money market)
	amountToSell (Person _ inv ranges job _ market) item  = if (item `elem` (map fst (needs job))) && (not (member ranges item))
							    	then return Nothing
								else return (Just (Bid item (ceiling ((favorability (lookup ranges item) (lastMean market item)) * (amountOf inv item))) (maybe 1 snd (lookup ranges item))))
	amountToBuy (Person _ inv ranges job money market) item  = if (item `elem` (makes job))
							   	then return Nothing
							   	else return (Just (Bid item ((\max -> return (floor ((max - (favorability (lookup ranges item) (lastMean market iteem))) * (spaceInInventory )))) (if item `elem` (map fst (needs job)) then 1 else (money / (2 * price)))) price))
									where
										price = (maybe 1 fst (lookup ranges item)) 
	doTurn (Person id inv ranges job money market) =  do
								newinv <- produce job inv
								toBuy <- (filter (\a -> a == Nothing) . map (\(item,_) -> amountToBuy (Person id newinv ranges job money market) item) . extractInv) allComs
								toSell <- (filter (\a -> a == Nothing) . map (\(item,_) -> amountToSell (Person id newinv ranges job money market) item) . extractInv) newinv
 

mean :: [(Money,Double)] -> Money
mean [] = 0
mean ((x,p):xs) = (((x * p) + (pn * (mean xs))) / (p + pn)
	where
		pn = (sum . map (\(_,p) -> p)) xs

lmean :: [Money] -> Money
lmean l = (sum l) / (length xs)

turnMean :: Commodities -> Market -> Money
turnMean t = mean . map (\trade -> (unit_price trade, quantity trade)) . filter (\trade -> t == (item trade))  


