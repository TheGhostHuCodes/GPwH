data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize ch = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum ch + halfAlphabet
          rotation = offset `mod` alphabetSize

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
    where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot4l = rotN alphaSize

data ThreeLetterAlphabet = Alpha
                         | Beta
                         | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3l = rotN alphaSize

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder n ch = toEnum rotation
    where halfN = n `div` 2
          offset = if even n
                  then fromEnum ch + halfN
                  else 1 + fromEnum ch + halfN
          rotation = offset `mod` n

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3ldecoder vals
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3ldecoder = rotNDecoder alphaSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNDecoder alphaSize

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (not (v1 && v2))
xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2
xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0  = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
    where remainder = n `mod` 2
          nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' n)
          missingBits = maxBits - (length reversedBits)
          leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
    where size = length bits
          indices = [size - 1, size - 2 .. 0]
          trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair ->
                                (fst pair) `xor` (snd pair))
                          (zip padBits plaintextBits)
    where padBits = map charToBits pad
          plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
    where bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])