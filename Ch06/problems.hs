-- Question 6.1
myRepeat n = cycle [n]

-- Question 6.2
subseq i j list = drop i (take j list)

-- Question 6.3
inFirstHalf el list = el `elem` firstHalf
    where firstHalf = take (length list `quot` 2) list