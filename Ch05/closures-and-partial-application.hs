inc n = n + 1

ifEven fn x = if even x
              then fn x
              else x                

genIfEven f = (\x -> ifEven f x)

ifEvenInc = genIfEven inc

genIfXEven x = (\f -> ifEven f x)

getRequestUrl host apiKey resource id = host
                                        ++ "/"
                                        ++ resource
                                        ++ "/"
                                        ++ id
                                        ++ "?token="
                                        ++ apiKey

exampleUrlBuilder = getRequestUrl "http://example.com"
myExampleUrlBuilder = exampleUrlBuilder "1337hAsk311"

add4 a b c d = a + b + c + d

exampleBuilder = getRequestUrl "http://example.com" "1337hAsk311" "books"

subtract2 = flip (-) 2