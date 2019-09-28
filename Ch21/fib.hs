-- Question 21.2
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)

main :: IO ()
main = do
  putStrLn "enter a number"
  number <- getLine
  let fib = fastFib 1 1 (read number)
  print fib
