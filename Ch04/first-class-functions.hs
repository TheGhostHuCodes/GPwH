import           Data.List


ifEven myFunction x = if even x then myFunction x else x

inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n

names =
  [ ("Ian"    , "Curtis")
  , ("Bernard", "Sumner")
  , ("Peter"  , "Hook")
  , ("Stephen", "Morris")
  , ("Steve"  , "Morris")
  ]

-- Question 4.1
compareLastNames name1 name2 = if lastNameResult == EQ
  then compare (fst name1) (fst name2)
  else lastNameResult
  where lastNameResult = compare (snd name1) (snd name2)

sfOffice name = if lastName < "L"
  then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
  else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
 where
  lastName = snd name
  nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

-- Question 4.2
dcOffice name =
  nameText ++ " - 1600 Pennsylvania Ave NW - Washington, DC 20500"
  where nameText = fst name ++ " " ++ snd name ++ ", Esq."

getLocationFunction location = case location of
  "ny"   -> nyOffice
  "sf"   -> sfOffice
  "reno" -> renoOffice
  "dc"   -> dcOffice
  _      -> \name -> fst name ++ " " ++ snd name

addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location
