import Economics.Agent
import Random.Dist
import Libs.AssList
import Control.Monad.Random

toolUsed :: [(Commodity,Amount)] -> Rand g [(Commodity,Amount)]
toolUsed woTool = mkDist [((Tool,1):woTool, 0.9),(woTool,0.1)]

create :: [(Commodity,Amount)] -> (Commodity,Amount) -> Rand g [(Commodity,Amount)]
create ins (out,n) = [(ins, return [(out,n)]),(ins, toolUsed [(out, 2 * n)])]

refine :: [(Commodity,Amount)] -> Commodity -> (Commodity,Amount) -> Amount -> Amount -> Rand g [(Commodity,Amount)]
refine base limiting (out,ratio) maxWOTool max = let woTool = map (\n -> ((limiting,n):base,return [(out,ratio * n)])) [1..maxWOTool]
                                                     wTool  = map (\n -> ((limiting,n):(Tool,1):base,toolUsed [(out,ratio * n)]) [1..max])
                                                 in woTool ++ wTool

data Commodity = Food | Wood | Ore | Metal | Tool deriving (Show, Read, Eq)
instance Tradable Commodity where
        unit_mass _ = 1
        recipes Food  = create [(Wood,1)] (Food,2)
        recipes Ore   = create [(Food,1)] (Ore,2)
        recipes Metal = refine [(Food,1)] Ore (Metal,1) 2 20
        recipes Wood  = create [(Food,1)] (Wood, 2)
        recipes Tool  = map (\n -> ([(Metal,n),(Food,1)],[(Tool,n)])) [1..20]

showjob :: Commodity -> String
showjob Food  = "Farmer"
showjob Ore   = "Miner"
showjob Metal = "Refiner"
showjob Wood  = "Lumberjack"
showjob Tool  = "Blacksmith"

needs :: Commodity -> [(Commodity,Amount)]
needs c = fst $ head $ recipes c

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

upb :: a -> Either (Transaction t) (Bid t) -> Rand g a
upb (Person id inv ranges job money market) (Left (Transaction _ _ item _ _)) = return $ Person id inv (update (\r -> Just (succSell r)) ranges item price_ranges) job money market
upb (Person id inv ranges job money market) (Right (Bid item amount _)) = return $ Person id inv (update (\r -> Just (failToSell (lastMean market) r)) item  price_ranges) job money market

type Inventory = AssList Commodity Amount

data Person = Person { ident :: Identifier
		             , inv :: Inventory
		             , price_ranges :: AssList Commodity (Money,Money)
		             , job :: Commodity
		             , money :: Money
		             , market :: Market
		             }

instance Agent Person Commodity where
	getID a = ident a
	getInventory a = inv a
	spaceInInventory inv = 20 - (inventoryMass inv)
    replaceInventory (Person id _ pr j mon mar) inv = Person id inv pr j mon mar
	getJob a = job a
	getMoney a = money a
    replaceMoney (Person id inv pr j mon mar) m = Person id inv pr j (mon + m) mar 
    updatePriceBeleifs = upb
	amountToSell (Person _ inv ranges job _ market) item  = if (item `elem` (map fst (needs job))) && (not (member ranges item))
							    	                            then return Nothing
								                                else return (Just (Bid item (ceiling ((favorability (lookup ranges item) (lastMean market item)) * (amountOf inv item))) (maybe 1 snd (lookup ranges item))))
	amountToBuy (Person _ inv ranges job money market) item  = return $ if (item `elem` (makes job))
							   	                                            then Nothing
							   	                                            else (Just (Bid item ((\max -> return (floor ((max - (favorability (lookup ranges item) (lastMean market iteem))) * (spaceInInventory )))) (if item `elem` (map fst (needs job)) then 1 else (money / (2 * price)))) price))
									where
										price = (maybe 1 fst (lookup ranges item))
