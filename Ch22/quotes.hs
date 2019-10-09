quotes :: [String]
quotes = ["quote 1", "quote 2", "quote 3", "quote 4", "quote 5"]

lookupQuote :: [String] -> [String]
lookupQuote []         = []
lookupQuote ("n" : xs) = []
lookupQuote (""  : xs) = lookupQuote xs
lookupQuote (x   : xs) = quote : lookupQuote xs
  where quote = quotes !! (read x - 1)

main :: IO ()
main = do
  userInput <- getContents
  mapM_ putStrLn (lookupQuote (lines userInput))
