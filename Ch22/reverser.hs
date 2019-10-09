-- Quick Check 22.3
main :: IO ()
main = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed
