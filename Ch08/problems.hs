-- Question 8.1
myReverse []       = []
myReverse [x     ] = [x]
myReverse (x : xs) = myReverse xs ++ [x]

-- Question 8.2
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)
