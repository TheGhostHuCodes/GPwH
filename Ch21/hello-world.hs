import qualified Data.Map                      as Map

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

-- Question 21.1
nameData :: Map.Map String String
nameData = Map.fromList [("name", "Joey")]

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup "name" nameData
  return (helloPerson name)

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

