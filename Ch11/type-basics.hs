x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1, 2, 3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

letters :: [Char]
letters = ['a', 'b', 'c']

aPet ::[Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch", 'D')

streetAddress :: (Int, String)
streetAddress = (123, "Happy St.")

double :: Int -> Int
double n = n * 2

half :: Int -> Double
half n = (fromIntegral n) / 2

halve :: Integer -> Integer
halve n = div n 2

printDouble :: Int -> String
printDouble n = show (n * 2)

z = read "6"
q = z / 2

anotherNumber :: Int
anotherNumber = read "6"

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

makeAddressLambda = (\number ->
                        (\street ->
                            (\town -> (number, street, town))))

ifEven :: (Int -> Int) -> Int -> Int
ifEven fn n = if even n
              then fn n
              else n

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

nameTriple = makeTriple "Oscar" 'D' "Grouch"