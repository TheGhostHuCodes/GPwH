data AuthorName = AuthorName {
    firstName :: String
  , lastName :: String
}

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName
          | FirstNameWithTwoInits FirstName Char Char

data Creator = AuthorCreator Author
             | ArtistCreator Artist

data Author = Author Name
data Artist = Person Name | Band String

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data Book = Book {
    author :: Creator
  , isbn :: String
  , bookTitle :: String
  , bookYear :: Int
  , bookPrice :: Double
}

data VinylRecord = VinylRecord {
    artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name :: String
  , toyDescription :: String
  , toyPrice :: Double
}

-- Question 16.1
data Pamphlet = Pamphlet {
    title :: String
  , pamphletDescription :: String
  , contact :: String
}

data StoreItem = BookItem Book 
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

-- Question 16.2
type Radius = Double
type Height = Double
type Width = Double

data Shape = Circle Radius
           | Square Height
           | Rectangle Height Width
           deriving Show

parimeter :: Shape -> Double
parimeter (Circle r) = 2 * pi * r
parimeter (Square h) = 4 * h
parimeter (Rectangle h w) = 2 * h + 2 * w

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Square h) = h ^ 2
area (Rectangle h w) = h * w