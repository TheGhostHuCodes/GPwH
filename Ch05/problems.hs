-- Question 5.1
inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEven fn n = if even n then fn n else n

ifEvenInc = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square

-- Question 5.2
binaryPartialApplication fn x = \y -> fn x y
