import qualified Data.Map                      as Map

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe Nothing  = Nothing
incMaybe (Just n) = Just (n + 1)

-- Quick Check 27.1
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing  = Nothing
reverseMaybe (Just s) = Just (reverse s)

successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

data RobotPart = RobotPart { name :: String , description :: String , cost :: Double , count :: Int } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
  { name        = "left arm"
  , description = "left arm for face punching!"
  , cost        = 1000.00
  , count       = 3
  }

rightArm :: RobotPart
rightArm = RobotPart
  { name        = "right arm"
  , description = "right arm for kind hand gestures"
  , cost        = 1025.00
  , count       = 5
  }

robotHead :: RobotPart
robotHead = RobotPart
  { name        = "robot head"
  , description = "this head looks mad"
  , cost        = 5092.25
  , count       = 2
  }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat
  [ "<h2>"
  , partName
  , "</h2>"
  , "<p><h3>desc</h3>"
  , partDesc
  , "</p><p><h3>count</h3>"
  , partCount
  , "</p>"
  ]
 where
  partName  = name part
  partDesc  = description part
  partCost  = show (cost part)
  partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
 where
  keys    = [1, 2, 3]
  vals    = [leftArm, rightArm, robotHead]
  keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- Quick Check 27.3
allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- Question 27.1
newtype Box a = Box a deriving Show

instance Functor Box where
    fmap func (Box val) = Box (func val)

morePresents :: Int -> Box a -> Box [a]
morePresents n present = replicate n <$> present

-- Question 27.2
myBox :: Box Int
myBox = Box 1

wrapped :: Box (Box Int)
wrapped = fmap Box myBox

unwrap :: Box a -> a
unwrap (Box val) = val

-- Question 27.3
printCost :: Maybe Double -> IO ()
printCost Nothing     = putStrLn "item not found"
printCost (Just cost) = print cost

main :: IO ()
main = do
  putStrLn "Enter a part number:"
  partNo <- getLine
  let part = Map.lookup (read partNo) partsDB
  printCost (cost <$> part)
