import           Data.List
import           Data.Semigroup


myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = foldr (&&) True . map testFunc

-- Quick Check 17.1
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = foldr (||) False . map testFunc

instance Semigroup Integer where
    (<>) x y = x + y

data Color = Red
           | Yellow
           | Blue
           | Green
           | Purple
           | Orange
           | Brown
           | Clear deriving (Show, Eq)

instance Semigroup Color where
    (<>) any Clear = any
    (<>) Clear any = any
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
             | otherwise = Brown

-- Question 17.1
instance Monoid Color where
    mempty = Clear
    mappend = (<>)

howMuch :: Int -> String
howMuch n | n > 10    = "a whole bunch"
          | n > 0     = "not much"
          | otherwise = "we're in debt!"

-- Question 17.2
newtype Events = Events [String]
instance Semigroup Events where
    (<>) = combineEvents
instance Monoid Events where
    mempty = Events []
    mappend = (<>)

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
  where combiner x y = mconcat [x, "-", y]

newtype Probs = Probs [Double]
instance Semigroup Probs where
    (<>) = combineProbs
instance Monoid Probs where
    mempty = Probs []
    mappend = (<>)

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

data PTable = PTable Events Probs

instance Show PTable where
    show (PTable (Events events) (Probs probs)) = mconcat pairs
        where pairs = zipWith showPair events probs

instance Semigroup PTable where
    (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
    (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = e1 <> e2
              newProbs = p1 <> p2

instance Monoid PTable where
    mempty = PTable (Events []) (Probs [])
    mappend = (<>)

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events (Probs normalizedProbs)
 where
  totalProbs      = sum probs
  normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
 where
  nToAdd     = length l2
  repeatedL1 = map (take nToAdd . repeat) l1
  newL1      = mconcat repeatedL1
  cycledL2   = cycle l2

coin :: PTable
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner =
  createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])
