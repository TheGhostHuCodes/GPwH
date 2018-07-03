import Data.Char


-- Question 9.1
myElem el xs = length (filteredList) /= 0
    where filteredList = filter (== el) xs

-- Question 9.2
isPalindrome s = chars == reverse chars
    where chars = map toLower (filter (/= ' ') s)

-- Question 9.3
harmonic n = sum (take n series)
    where series = map (1.0 /) [1.0, 2.0 .. ]