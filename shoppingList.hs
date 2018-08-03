{-# OPTIONS_GHC -Wall -threaded #-}

module Optimizer where

import Data.Ratio
import Data.List (union, partition, find, minimumBy)
import Data.Ord (comparing)
import Control.Parallel.Strategies (parMap, rdeepseq)
  
type Price = Integer
type Quantity = Integer

type Probability = Rational

data Roll = Roll Item Quantity | NullRoll
  deriving (Show, Eq)
type RollSet = [Roll]

data Item = Item String
  deriving (Show, Eq)

type ChanceItem = (Item, Quantity, Probability)
data Container = Container [ChanceItem]
  deriving (Show)

type ShoppingListEntry = (Item, Quantity)
data ShoppingList = ShoppingList [ShoppingListEntry]
  deriving (Show)

type StockListEntry = (Container, Price)
data StockList = StockList [StockListEntry]
  deriving (Show)

-- | Example: "Getting one X takes us in this direction")
type Consequence = (RollSet, Probability)

-- | Used for attaching probabilities to branches
type ConsequenceBranch = (Consequence, Probability, [ConsequenceNode])
-- | Current container, odds of looping, price per movement, remaining items, possible branches
data ConsequenceNode = EmptyConsequence
                     | ConsequenceNode { cnContainer  :: Container
                                       , cnLoopChance :: Probability
                                       , cnMoveCost   :: Integer
                                       , cnBranches   :: [ConsequenceBranch]}
                                      deriving (Show)

type ProbabilityChain = [Roll]

-- | Removes StockEntries which do not contain at least one item requested by the ShoppingList
sanitizeStockList :: StockList -> ShoppingList -> StockList
sanitizeStockList s (ShoppingList []) = s
sanitizeStockList (StockList []) _    = StockList []
sanitizeStockList (StockList xs) l    = StockList (filter grabContainers xs) where
  grabContainers :: StockListEntry -> Bool
  grabContainers ((Container cs),_) = length (filter (\(Item s, _, _) -> elem s (getShoppingItemNames l)) cs) > 0

  getShoppingItemNames :: ShoppingList -> [String]
  getShoppingItemNames (ShoppingList [])               = []
  getShoppingItemNames (ShoppingList ((Item s, _):ss)) = s : getShoppingItemNames (ShoppingList ss)

-- | Returns every possible combination of the inputted lists as tuples
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

-- | Converts an item and probability into a roll set
-- | In other words, `Item "x" 1 2%5` will become `[Roll Item "x" 1, Roll Item "x" 1, NullRoll, NullRoll, NullRoll]`
makeRollSet :: ChanceItem -> RollSet
makeRollSet (item, quantity, probability) = repeatItems ++ repeatNulls where
  a = fromIntegral $ numerator probability
  b = fromIntegral $ denominator probability
  repeatItems = replicate a (Roll item quantity)
  repeatNulls = replicate (b - a) (NullRoll)

-- | Converts each `ChanceItem` in a `Container` to a `RollSet`
containerToRollSet :: Container -> [RollSet]
containerToRollSet (Container cs) = map makeRollSet cs

-- | If a `Roll`'s item isn't in the `ShoppingList`, it can be considered a `NullRoll` as it is useless
matchRoll :: ShoppingList -> Roll -> Roll
matchRoll _ NullRoll                        = NullRoll
matchRoll (ShoppingList []) _               = NullRoll
matchRoll (ShoppingList ls) roll@(Roll i _) =
  if elem i (listItems ls) then
    roll else
    NullRoll where
  listItems = map (\(i2,_) -> i2) -- | Extracts `Item`s from `ShoppingList`

-- | Converts all unneeded `Roll`s in a `RollSet` into `NullRoll`s
matchRollSet :: ShoppingList -> RollSet -> RollSet
matchRollSet s rs = map (matchRoll s) rs

-- | Converts all unneeded `Roll`s in a set of `RollSet`s into `NullRoll`s
matchAllRollSets :: ShoppingList -> [RollSet] -> [RollSet]
matchAllRollSets s rs = map (matchRollSet s) rs

--nullifyStockList :: StockList -> StockList
--nullifyStockList stockList@(StockList entries) = map (\((Container is),_) -> filter (\i -> i elem ) e) entries

-- | Creates all possible combinations of taking one element from
  -- | each sublist
allWalks :: [[a]] -> [[a]]
--allWalks l = mapM id l
allWalks l = sequence l

-- | Removes all `NullRoll`s from a `RollSet`
trimNulls :: RollSet -> RollSet
trimNulls [] = error "Trimming empty RollSet"
trimNulls rs = filter (\x -> case x of
                               Roll _ _ -> True
                               NullRoll -> False) rs

trimAllNulls :: [RollSet] -> [RollSet]
trimAllNulls [] = error "Trimming empty RollSet set"
trimAllNulls rs = map trimNulls rs

-- | Checks if two `RollSet`s are equal to each other. Two `RollSet`s are equal
  -- | if and only if their unions are the same size as both of them.
rollSetEq :: RollSet -> RollSet -> Bool
rollSetEq a b = length (union a b) == length a && length a == length b

-- | Takes a `RollSet` and finds all instances of it in a list of `RollSet`s.
  -- | Combines the `RollSet`, the number of found instances, and the remaining,
  -- | unmatching `RollSet` list.
collapseRoll :: RollSet -> [RollSet] -> ((RollSet, Integer), [RollSet])
collapseRoll s [] = ((s, 1), [])
collapseRoll s r  = ((s, matchCount), rest) where
  matchCount = (fromIntegral . length . fst $ partition (rollSetEq s) r) + 1
  rest       = snd $ partition (rollSetEq s) r

-- | For a list of `RollSet`s, returns the unique `RollSet`s and their counts.
collapseRollSet :: [RollSet] -> [(RollSet, Integer)]
collapseRollSet []     = []
collapseRollSet (r:rs) = collapsed : (collapseRollSet rest) where
  result    = collapseRoll r rs
  collapsed = fst result
  rest      = snd result

-- | Correctly applies the ratios to what was a count
applyRatios :: [(RollSet, Integer)] -> Integer ->  [(RollSet, Rational)]
applyRatios rs p = map (\(s, i) -> (s, i%p)) rs

-- | Returns the items in common
matchItems :: [ShoppingListEntry] -> [ChanceItem] -> [Item]
matchItems [] _  = []
matchItems _ []  = []
matchItems ls cs = getItm1 $ filter (\(a, b) -> a == b) (cartesianProduct (getItm1 ls) (getItm2 cs)) where
  getItm1 = map (\(itm, _) -> itm)
  getItm2 = map (\(itm, _, _) -> itm)

-- | Removes a `Roll` from a `ShoppingList`.
removeRoll :: ShoppingList -> Roll -> ShoppingList
removeRoll (ShoppingList []) _                     = ShoppingList []
removeRoll shoppingList NullRoll                   = shoppingList
removeRoll (ShoppingList ls) (Roll item remAmount) = ShoppingList (map stillSome dropEmpties ++ snd split)  where
  -- We only care about items this roll actually influences
  split       :: ([ShoppingListEntry], [ShoppingListEntry])
  split                  = partition (\(i,_) -> i == item) ls

  -- Checks if an entry will be fully satisfied or not
  willBeEmpty :: ShoppingListEntry -> Bool
  willBeEmpty (_, stock) = if stock > remAmount then True else False

  -- Any satisfied entry is no longer required
  dropEmpties :: [ShoppingListEntry]
  dropEmpties            = filter (\x -> willBeEmpty x) (fst split)

  -- When more items are needed than the roll provides, subtract the provided from the needed
  stillSome   :: ShoppingListEntry -> ShoppingListEntry
  stillSome (i, q)       = (i, q - remAmount)

removeRollSet :: ShoppingList -> RollSet -> ShoppingList
removeRollSet (ShoppingList []) _ = ShoppingList []
removeRollSet shoppingList []     = shoppingList
removeRollSet lst (r:rs)          = removeRollSet (removeRoll lst r) rs




testShoppingList :: ShoppingList
testShoppingList = ShoppingList [(Item "x", 1), (Item "y", 1)]--, (Item "z", 1)]

c1 :: Container
c1 = Container [(Item "x", 1, 1%2)]
c2 :: Container
c2 = Container [(Item "y", 1, 1%2)]
c3 :: Container  
c3 = Container [(Item "x", 1, 1%3),
                (Item "y", 1, 1%3)]
c4 :: Container
c4 = Container [(Item "z", 1, 3%4)]
c5 :: Container
c5 = Container [(Item "x", 1, 1%4),
                (Item "y", 1, 1%5),
                (Item "z", 2, 1%6)]
c6 :: Container
c6 = Container [(Item "z", 2, 1%3),
                (Item "x", 1, 1%2)]

testStockList :: StockList
testStockList = StockList [(c1, 10), (c2, 10), (c3, 20), (c4, 15), (c5, 30), (c6, 25)]

{- |

Consequences represent the potential, nondeterministic outcomes of opening
a container.

-}
consequences :: ShoppingList -> Container -> [Consequence]
consequences s c = applyRatios collapsedList rawLength where
  trimmedList    = trimAllNulls . allWalks $ matchAllRollSets s (containerToRollSet c)
  collapsedList  = collapseRollSet trimmedList
  rawLength      = fromIntegral $ length trimmedList

makeConsequenceTree :: StockListEntry -> ShoppingList ->  StockList -> ConsequenceNode
makeConsequenceTree ((Container []), _)_ _ = error "Empty entry" 
makeConsequenceTree _ _ (StockList [])    = error "Unsatisfiable list"
makeConsequenceTree _ (ShoppingList []) _ = EmptyConsequence
makeConsequenceTree (container, price) shop stock = ConsequenceNode container loopChance price branches where
  consq      = consequences shop container
  noNull     = filter (\(x,_) -> not (null x)) consq

  loopChance = case (find (\(x,_) -> null x) consq) of
                 Nothing     -> 0
                 Just (_,p)  -> p

  consqShoppingLists = map (\(rs,_) -> removeRollSet shop rs) noNull
  consqStockOptions = map (\s -> sanitizeStockList stock s) consqShoppingLists
  
  nextNodes =  getLists (zip consqStockOptions consqShoppingLists) where
    getLists = map (\(stockList, shopList) -> consequenceNodes shopList stockList)

    consequenceNodes _ (StockList [])    = error "Unsatisfiable shopping list"
    consequenceNodes l s@(StockList lst) = map (\entry -> makeConsequenceTree entry l s) lst
    
  branches   = map (\(nodes, con@(_,p)) -> (con, p, nodes)) (zip nextNodes noNull)

--collapseConTree :: ConsequenceNode -> Rational
--collapseConTree tree = convergingSigma (\i -> )

nodeSum :: ConsequenceNode -> Integer -> Rational
nodeSum tree n = (cnLoopChance tree)^n * (branchSums (cnBranches tree)) where
  thisPrice = toRational $ cnMoveCost tree

  branchSums :: [ConsequenceBranch] -> Rational
  branchSums branches = sum $ map getNextNodes branches where
    getNextNodes :: ConsequenceBranch -> Rational
    getNextNodes (_, prob, next) = minimum $ map (\x -> prob * (thisPrice * ((n+1)%1) + (getPrice x))) next

    getPrice :: ConsequenceNode -> Rational
    getPrice EmptyConsequence    =  0
    getPrice node                =  convergingSigma $ nodeSum node

getBestBuy :: ShoppingList -> StockList -> (StockListEntry, Double)
getBestBuy shop stock = minimumBy (comparing snd) (zip justList costs) where
  cleanStock = sanitizeStockList stock shop
  justList = case (cleanStock) of
    StockList s -> s
  costs = parMap rdeepseq (\entry -> rationalToDecimal . convergingSigma . nodeSum $ makeConsequenceTree entry shop stock) justList
    
printConsequenceTree :: Integer -> ConsequenceNode -> String
printConsequenceTree _ EmptyConsequence = ""
printConsequenceTree t tree             = label ++ loopCons where
  tabs = concat $ replicate (fromInteger t) "\t"
  tabsP n = concat $ replicate (fromInteger t + n) "\t"
  
  container = show $ cnContainer tree
  price = show $ cnMoveCost tree
  label = tabs ++ container ++ " $" ++ price ++ ":\n"


  cons = cnBranches tree
  conProb (_,p,_) = show p
  consConsq (c, _, _) = show c
  consBranch (_, _, b) = b
  loopBranches branches = concatMap (\b -> printConsequenceTree (t + 2) b) branches
  conLabel branch = (tabsP 1) ++ "| " ++ (conProb branch) ++ " " ++ (consConsq branch) ++ "\n" ++ (loopBranches (consBranch branch))
  loopCons = concatMap (\b -> (conLabel b) ++ "\n") cons

writeConsequenceTree :: ConsequenceNode -> FilePath -> IO ()
writeConsequenceTree tree path = writeFile path $ printConsequenceTree 0 tree

countConsequenceNodes :: ConsequenceNode -> Integer
countConsequenceNodes EmptyConsequence           = 0
countConsequenceNodes ConsequenceNode{cnBranches = branches} = 1 + sum (map (\b -> count b) branches) where
  count (_,_,bs) = sum $ map (\bch -> countConsequenceNodes bch) bs

rationalToDecimal :: Rational -> Double
rationalToDecimal n = fromRational n

convergingSigma :: (Integer -> Rational) -> Rational
convergingSigma f = sigma 0 53 f
  
sigma :: Integer -> Integer -> (Integer -> Rational) -> Rational 
sigma lower upper f = sum $ map f [lower..upper]

collapseTree :: ConsequenceNode -> Double
collapseTree tree = rationalToDecimal . convergingSigma $ nodeSum tree

containerAverageCost :: StockListEntry -> ShoppingList -> StockList -> Double
containerAverageCost entry shop stock = collapseTree $ makeConsequenceTree entry shop stock

{-
Process:

Sanitize the `StockList` to the `ShoppingList`

Take this `StockList` and generate a `ConsequenceTree` per `StockListEntry`

  - To generate the tree:
  - Find all consequences for this container
  - For each consequence, remove the potential items from the `ShoppingList`
  - Then, sanitize the stock list to this new shopping list
  - From here, we can repeat the process until we've exhausted the shopping list

Next we collapse the consequence tree:

  - First, go down each path until you hit a terminal branch
  - Upon reaching this point, we've reached a scenario in which we've satisfied
    the shopping list
  - Calculate the cost of this final consequence using the infinite summation
  - March up the branch, using previously calculated consequence costs to
    calculate the cost of these more complex branches
  - Any consequence node which has more than one branch extending from it
    represents a "decision point." These are points in which you're free
    to choose which path you want to pursue. Thus, if the price of all
    branches is known, we can choose the cheapest branch. This means,
    when considering the cost of this node, we will only factor in the
    cost of the cheapest branch, as we would not logically pursue more
    expensive branches.
      - Trim these more expensive branches from the tree -- they're unnecessary
  - Eventually we will work our way back to the root node. Thanks to the previous
    steps, this node's branches can all be considered optimal, decision-wise.
    Therefore, we can do a final summation on this root node to find the average
    cost of starting with this node.

Repeating this process on all nodes will show the average cost of starting at each
node.

At this point we can consider the optimal next move to be the purchase of the cheapest
starting container, and depending on the results, simply follow the decision tree to
termination (remember that only the best decisions will remain for any possible outcome)
-}

{-
walk :: a -> [a] -> [[a]]
walk _ []     = []
walk a (x:xs) = (a : x : []) : (walk a xs)

walks :: [a] -> [a] -> [[a]]
walks _ [] = []
walks x y  = concatMap (\z -> walk z y) x
-}

{-
-- | Adds an item to the head of every list in a list of lists
tieredWalk :: a -> [[a]] -> [[a]]
tieredWalk _ []     = []
tieredWalk a (l:ls) = (a : l) : (tieredWalk a ls)

-- | Creates all possible combinations of taking one element from
  -- | each sublist
allWalks :: [[a]] -> [[a]]
allWalks []         = []
allWalks (l:[])     = [[a] | a <- l]
allWalks (l:ls)     = concatMap (\x -> tieredWalk x (allWalks ls)) l
-}
