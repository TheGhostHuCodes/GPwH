-- Quick Check 8.1
myLength []       = 0
myLength (_ : xs) = 1 + myLength xs

myTake _ []       = []
myTake 0 _        = []
myTake n (x : xs) = x : rest where rest = myTake (n - 1) xs


myCycle (x : xs) = x : myCycle (xs ++ [x])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n = if even n then 1 + collatz (n `div` 2) else 1 + collatz (n * 3 + 1)
