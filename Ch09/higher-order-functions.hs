addAnA [] = []
addAnA (x:xs) = ("a " ++ x):addAnA xs

squareAll [] = []
squareAll (x:xs) = (x ^ 2):squareAll xs

myMap f [] = []
myMap f (x:xs) = (f x):myMap f xs

myFilter pred [] = []
myFilter pred (x:xs) = if pred x
                       then x:myFilter pred xs
                       else myFilter pred xs

myRemove pred [] = []
myRemove pred (x:xs) = if pred x
                       then myRemove pred xs
                       else x:myRemove pred xs

myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^ 2) xs)

rcons x y = y:x
myReverse xs = foldl rcons [] xs

myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f newAcc xs
    where newAcc = f acc x

myFoldr f init [] = init
myFoldr f init (x:xs) = f x rightResult
    where rightResult = myFoldr f init xs