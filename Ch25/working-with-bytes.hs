{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.IO                  as TIO
import           System.Random

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

sampleString :: String
sampleString = BC.unpack sampleBytes

-- Quick Check 25.1
bcInt :: BC.ByteString
bcInt = "6"

bc2Int :: BC.ByteString -> Int
bc2Int = read . BC.unpack

-- Quick Check 25.3
randomChar :: IO Char
randomChar = do
  randomInt <- randomRIO (0, 255)
  return (toEnum randomInt)

nagarjunaBC :: BC.ByteString
nagarjunaBC = "नागर्जुन"

nagarjunaText :: T.Text
nagarjunaText = "नागर्जुन"

nagarjunaB :: B.ByteString
nagarjunaB = (BC.pack . T.unpack) nagarjunaText

nagarjunaSafe :: B.ByteString
nagarjunaSafe = E.encodeUtf8 nagarjunaText
