-- Question 14.2
data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Show, Eq, Ord)

class (Eq a, Enum a) => Die a where
    roll :: Int -> a

instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)