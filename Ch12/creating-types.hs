type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (FirstName, LastName)

firstName :: PatientName -> FirstName
firstName patient = fst patient
lastName :: PatientName -> LastName
lastName patient = snd patient

-- Quick Check 12.1
patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
 where
  name      = lname ++ ", " ++ fname
  ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _                = True
canDonateTo _               (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A  _) = True
canDonateTo (BloodType B _) (BloodType B  _) = True
canDonateTo _               _                = False -- otherwise

patient1BT :: BloodType
patient1BT = BloodType A Pos
patient2BT :: BloodType
patient2BT = BloodType O Neg
patient3BT :: BloodType
patient3BT = BloodType AB Pos

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l            ) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight ::  Int
                       , bloodType :: BloodType }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

-- Quick Check 12.2
janeESmith :: Patient
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith")
                     Female
                     28
                     62
                     140
                     (BloodType O Neg)

jackieSmith :: Patient
jackieSmith = Patient
  { name      = Name "Jackie" "Smith"
  , age       = 43
  , sex       = Female
  , height    = 62
  , weight    = 115
  , bloodType = BloodType O Neg
  }

jackieSmithUpdated = jackieSmith { age = 44 }

-- Question 12.1
canDonateBlood :: Patient -> Patient -> Bool
canDonateBlood p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

-- Question 12.2
showNameLastFirst :: Name -> String
showNameLastFirst (Name f l            ) = l ++ ", " ++ f
showNameLastFirst (NameWithMiddle f m l) = l ++ ", " ++ f ++ " " ++ m

showSex :: Sex -> String
showSex Male   = "Male"
showSex Female = "Female"

patientSummary :: Patient -> String
patientSummary p =
  "**************\n"
    ++ "Patient Name: "
    ++ showNameLastFirst (name p)
    ++ "\n"
    ++ "Sex: "
    ++ showSex (sex p)
    ++ "\n"
    ++ "Age: "
    ++ show (age p)
    ++ "\n"
    ++ "Height: "
    ++ show (height p)
    ++ " in.\n"
    ++ "Weight: "
    ++ show (weight p)
    ++ " lbs.\n"
    ++ "Blood Type: "
    ++ showBloodType (bloodType p)
    ++ "\n"
    ++ "**************"
