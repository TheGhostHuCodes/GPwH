simple x = x

class Describable a where
    describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)