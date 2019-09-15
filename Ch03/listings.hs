sumSquareOrSquareSum x y = if sumSquare > squareSum
  then sumSquare
  else squareSum
 where
  sumSquare = x ^ 2 + y ^ 2
  squareSum = (x + y) ^ 2

-- Quick check 3.2
doubleDouble x = (\dubs -> dubs * 2) (x * 2)

sumSquareOrSquareSumLet x y =
  let sumSquare = x ^ 2 + y ^ 2
      squareSum = (x + y) ^ 2
  in  if sumSquare > squareSum then sumSquare else squareSum

overwrite x = let x = 2 in let x = 3 in let x = 4 in x

-- Quick check 3.3
overwriteLambda x = (\x -> (\x -> (\x -> x) 4) 3) 2

x = 4
add1 y = x + y
add2 y = (\x -> y + x) 3
add3 y = (\y -> (\x -> y + x) 1) 2
