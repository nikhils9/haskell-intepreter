module Interpreter where

data Expr = 
    Lit Int
    | Add Expr Expr
    deriving (Show)

ex1, ex2 :: Expr
ex1 = Lit 5
ex2 = Add (Lit 5) (Add (Lit 6) (Lit 7))

eval :: Expr -> Int
eval (Lit i) = i
eval (Add e1 e2) =
    eval e1 + eval e2