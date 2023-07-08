module Template where

data Name = MkName String
    deriving (Show)

data Pair = MkPair Int Int
    deriving (Show)

data Gender 
    = Male 
    | Female
    | Other
    deriving (Show)

data Person = MkPerson Name Int Gender
    deriving (Show)

data TestResult 
    = Pass Int
    | Fail [String]
    deriving (Show)

stringToGender :: String -> Gender
stringToGender "Male"   = Male
stringToGender "Female" = Female
stringToGender _        = Other

genderToString :: Gender -> String
genderToString Male   = "Male"
genderToString Female = "Female"
genderToString Other  = "Other"

passing :: Int -> TestResult
passing score = Pass score

failing :: [String] -> TestResult
failing comments = Fail comments

grade :: TestResult -> Int
grade (Pass score) = score
grade (Fail _)     = 0

comments :: TestResult -> [String]
comments (Pass _)        = []
comments (Fail comments) = comments
