import qualified Data.Map                      as Map

data RobotPart = RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    } deriving Show

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

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList [(0, leftArm), (1, rightArm), (2, robotHead)]

printCost :: Maybe Double -> IO ()
printCost Nothing     = putStrLn "Missing item"
printCost (Just cost) = print cost

main :: IO ()
main = do
  putStrLn "Enter a part ID:"
  partId1 <- getLine
  putStrLn "Enter a part ID:"
  partId2 <- getLine
  let part1    = Map.lookup (read partId1) partsDB
  let part2    = Map.lookup (read partId2) partsDB
  let cheapest = min <$> (cost <$> part1) <*> (cost <$> part2)
  printCost cheapest
