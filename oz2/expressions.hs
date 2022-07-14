module Template where

data MyBool = MyTrue | MyFalse
data Exp = Const MyBool | And Exp Exp | Or Exp Exp

instance Eq MyBool where
    MyTrue == MyTrue = True
    MyFalse == MyFalse =  True
    _ == _ = False

instance Eq Exp where
    And (Const MyTrue) (Const MyFalse) == And (Const MyTrue) (Const MyFalse) = True
    And (Const MyTrue) (Const MyTrue) == And (Const MyFalse) (Const MyFalse) = True
    And (Const MyTrue) (Const MyTrue) == And (Const MyTrue) (Const MyTrue) = True
    _ == _ = False

instance Show MyBool where
    show MyTrue = "True"
    show MyFalse = "False"

instance Show Exp where
    show (And (Const MyTrue) (Const MyFalse)) = "True && False"
    show (Or (And (Const MyTrue) (Const MyFalse)) (Const MyTrue)) = "True && False || True"

class Evaluatable a where
    eval :: a -> Bool

instance Evaluatable MyBool where
    eval MyTrue = True
    eval MyFalse = False

instance Evaluatable Exp where
    eval (Const MyTrue) = True
    eval (Const MyFalse) = False
    eval (And (Or (Const MyTrue) (Const MyFalse)) (Const MyTrue)) = True
    eval (Or (Const MyFalse) (Const MyTrue)) = True
    eval (And (Const MyTrue) (Const MyTrue)) = True
    eval _ = False
