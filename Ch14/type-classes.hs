data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum, Eq, Ord)

instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

-- Quick Check 14.1
-- instance Show SixSidedDie where
--     show S1 = "I"
--     show S2 = "II"
--     show S3 = "III"
--     show S4 = "IV"
--     show S5 = "V"
--     show S6 = "VI"

data Test1 = AA | ZZ deriving (Eq, Ord)
data Test2 = ZZZ | AAA  deriving (Eq, Ord)

newtype Name = Name (String, String) deriving (Show, Eq)

names :: [Name]
names =
  [ Name ("Emil", "Cioran")
  , Name ("Eugene", "Thacker")
  , Name ("Friedrich", "Nietzsche")
  ]

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)
