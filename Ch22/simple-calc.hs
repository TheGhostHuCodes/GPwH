import           Data.List.Split

calc :: [String] -> Int
calc (val1 : "+" : val2 : rest) = read val1 + read val2
calc (val1 : "*" : val2 : rest) = read val1 * read val2

main :: IO ()
main = do
  userInput <- getContents
  let values = splitOn " " userInput
  print (calc values)
