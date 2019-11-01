import           Control.Monad
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           System.Environment
import           System.Random

intToChar :: Int -> Char
intToChar int = toEnum safeInt where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte location charValue bytes = mconcat [before, newChar, after]
 where
  (before, rest) = BC.splitAt location bytes
  after          = BC.drop 1 rest
  newChar        = intToBC charValue

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location  <- randomRIO (1, bytesLength)
  charValue <- randomRIO (0, 255)
  return (replaceByte location charValue bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
 where
  (before, rest ) = BC.splitAt start bytes
  (target, after) = BC.splitAt size rest
  changed         = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

-- Question 25.2
reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before, reversed, after]
 where
  (before, rest ) = BC.splitAt start bytes
  (target, after) = BC.splitAt size rest
  reversed        = BC.reverse target

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (reverseSection start sectionSize bytes)

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReverseBytes
  , randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  , randomReverseBytes
  , randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  ]

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  imageFile <- BC.readFile filename
  glitched  <- foldM (\bytes func -> func bytes) imageFile glitchActions
  let glitchedFilename = mconcat ["glitched_", filename]
  BC.writeFile glitchedFilename glitched
  print "all done"
