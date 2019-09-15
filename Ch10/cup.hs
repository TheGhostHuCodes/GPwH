cup flOz = \message -> message flOz

coffeeCup = cup 12


getOz aCup = aCup (\flOz -> flOz)
drink aCup ozDrank = if ozDiff >= 0 then cup ozDiff else cup 0
 where
  flOz   = getOz aCup
  ozDiff = flOz - ozDrank
isEmpty aCup = getOz aCup == 0

afterManySips = foldl drink coffeeCup [1, 1, 1, 1, 1]
