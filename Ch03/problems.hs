-- Question 3.1
simple = (\x -> x)
makeChange = (\owed given ->
                if given - owed > 0
                then given -owed
                else 0)

-- Question 3.2
counter = (\x -> (\x -> x + 1) x + 1)