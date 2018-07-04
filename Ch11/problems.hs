-- Question 11.1
-- filter :: (a -> Bool) -> [a] -> [a]

-- Question 11.2
-- No, the type signature of head is, head :: [a] -> a, so we couldn't return an
-- empty list which is of type list.

-- Question 11.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x