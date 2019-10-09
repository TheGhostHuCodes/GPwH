-- Quick Check 22.1
main :: IO ()
main = do
  inputs <- mapM (\_ -> getLine) [1 .. 3]
  mapM_ putStrLn inputs
