module Template where

newtype Name = Name String deriving (Show)
newtype Pair = Pair (Int, Int) deriving (Show)
data Gender =  Female | Male | Other deriving (Show, Eq)
data Person = Person Name Int Gender deriving (Show)
data TestResult = Pass { score :: Int } | Fail { list_comments :: [String] } deriving (Show)

stringToGender :: String -> Gender
stringToGender gender
    | gender == "Male" = Male
    | gender == "Female" = Female
    | otherwise = Other

genderToSTring :: Gender -> String
genderToSTring Male = "Male"
genderToSTring Female = "Female"
genderToSTring Other = "Other"

passing :: Int -> TestResult
passing = Pass

failing :: [String] -> TestResult
failing = Fail

grade :: TestResult -> Int
grade (Pass x) = x
grade (Fail x) = 0

comments :: TestResult -> [String]
comments (Pass x) = []
comments (Fail x) = x


