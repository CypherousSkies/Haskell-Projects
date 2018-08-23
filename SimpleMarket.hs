import Economics.Agent
import Random.Dist

adjust :: Eq k => (v -> v) -> k -> [(k,v)] -> [(k,v)]
adjust _ _ [] = []
adjust f k ((k1,v1):ks) = if k == k1 then (k1,f v1):ks else adjust f k ks

toolUsed :: [(Commodity,Amount)] -> Rand g [(Commodity,Amount)]
toolUsed woTool = mkDist [((Tool,1):woTool, 0.9),(woTool,0.1)]

create :: [(Commodity,Amount)] -> (Commodity,Amount) -> Rand g [(Commodity,Amount)]
create ins (out,n) = [(ins, return [(out,n)]),(ins, toolUsed [(out, 2 * n)])]

refine :: [(Commodity,Amount)] -> Commodity -> (Commodity,Amount) -> Amount -> Amount -> Rand g [(Commodity,Amount)]
refine base limiting (out,ratio) maxWOTool max = let woTool = map (\n -> ((limiting,n):base,return [(out,ratio * n)])) [1..maxWOTool]
                                                     wTool  = map (\n -> ((limiting,n):(Tool,1):base,toolUsed [(out,ratio * n)]) [1..max]
                                                 in woTool ++ wTool

data Commodity = Food | Wood | Ore | Metal | Tool deriving (Show, Read, Eq)
instance Tradable Commodity where
        unit_mass _ = 1
        recipes Food  = create [(Wood,1)] (Food,2)
        recipes Ore   = create [(Food,1)] (Ore,2)
        recipes Metal = refine [(Food,1)] Ore (Metal,1) 2 20
        recipes Wood  = create [(Food,1)] (Wood, 2)
        recipes Tool  = map (\n -> ([(Metal,n),(Food,1)],[(Tool,n)])) [1..20]

job :: Commodity -> String
job Food  = "Farmer"
job Ore   = "Miner"
job Metal = "Refiner"
job Wood  = "Lumberjack"
job Tool  = "Blacksmith"

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
		     ,job :: Commodity
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
 

mean :: [(Money,Double)] -> Money
mean [] = 0
mean ((x,p):xs) = (((x * p) + (pn * (mean xs))) / (p + pn)
	where
		pn = (sum . map (\(_,p) -> p)) xs

lmean :: [Money] -> Money
lmean l = (sum l) / (length xs)

turnMean :: Commodities -> Market -> Money
turnMean t = mean . map (\trade -> (unit_price trade, quantity trade)) . filter (\trade -> t == (item trade))  


