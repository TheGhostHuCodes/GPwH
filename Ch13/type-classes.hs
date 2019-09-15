simple x = x

addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where describe :: a -> String

data IceCream = Chocolate | Vanilla deriving (Show, Eq, Ord)
