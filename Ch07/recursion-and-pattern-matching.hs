myGCD a b = if remainder == 0 then b else myGCD b remainder
  where remainder = a `mod` b

-- Question 7.2
myGCD2 a 0 = a
myGCD2 a b = myGCD2 b (a `mod` b)

sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "a bunch"

isEmpty [] = True
isEmpty _  = False

myHead (x : _) = x
myHead []      = error "No head for empty list"

-- Quick Check 7.3
myTail (_ : xs) = xs
-- Question 7.1
myTail []       = []
