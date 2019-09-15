-- Question 14.1
data Number = One | Two | Three deriving Enum

instance Eq Number where
    (==) num1 num2 = fromEnum num1 == fromEnum num2
instance Ord Number where
    compare num1 num2 = compare (fromEnum num1) (fromEnum num2)


-- Question 14.2
data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die a where
    roll :: Int -> a

instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)
