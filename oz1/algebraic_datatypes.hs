module Template where

data Name = N String
    deriving (Show)

data Pair = MkPair Int Int
    deriving (Show)

data Gender = Male|Female|Other
    deriving (Show)

data Person = MkPerson Name Int Gender 
    deriving (Show)

data TestResult 
    = Pass Int
    | Fail [String]
    deriving (Show)


stringToGender :: String -> Gender
stringToGender gender
    | gender == "Male" = Male
    | gender == "Female" = Female
    | otherwise = Other

genderToString :: Gender -> String
genderToString Male = "Male"
genderToString Female = "Female"
genderToString Other = "Other"


passing :: Int -> TestResult
passing x = Pass x

failing :: [String] -> TestResult
failing list = Fail list

grade :: TestResult -> Int
grade (Pass x) = x
grade (Fail list) = 0

comments :: TestResult -> [String]
comments (Fail list) = list
comments (Pass x) = []
