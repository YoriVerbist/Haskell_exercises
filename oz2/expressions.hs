
module Template where

-- * Arithmetic Expressions
-- ----------------------------------------------------------------------------

data Exp = Const Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Int
eval (Const x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst]
type Stack = [Int]

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack
execute (IPush x) l = x:l
execute IAdd (x:xs) = x + head xs : tail xs
execute ISub (x:xs) = head xs - x : tail xs
execute IMul (x:xs) = x * head xs : tail xs

run :: Prog -> Stack -> Stack
run [] s = s 
run (ins:inst) stack = run inst (execute ins stack)

compile :: Exp -> Prog
compile (Const x) = [IPush x]
compile (Add e1 e2) = compile e1 ++ compile e2 ++ [IAdd]
compile (Sub e1 e2) = compile e1 ++ compile e2 ++ [ISub]
compile (Mul e1 e2) = compile e1 ++ compile e2 ++ [IMul]

