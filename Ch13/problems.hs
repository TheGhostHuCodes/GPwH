-- Question 13.1
-- maxBound :: Word is 18446744073709551615, and minBound :: Word is 0, so Word
-- looks to be an unsigned Int.

-- Question 13.2
-- As it is defined, applying inc to maxBound :: Int will cause the Int to cycle
-- back to -9223372036854775808 while calling succ on maxBound :: Int will cause
-- an exception.

-- Question 13.3
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n