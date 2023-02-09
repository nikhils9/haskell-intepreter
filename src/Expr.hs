module Expr where

import Eval (Eval, getVar, setVar, divisionByZero)

data Expr = 
    Lit Int
    | Add Expr Expr
    | Div Expr Expr
    | Var String
    | Dec String Expr
    | Seq Expr Expr
    deriving (Show)

ex1, ex2, ex3, ex4, ex5, ex6 :: Expr
ex1 = Lit 5
ex2 = Add (Lit 5) (Add (Lit 6) (Lit 7))
ex3 = Add (Lit 7) (Div (Lit 9) (Lit 3))
ex4 = Div (Lit 7) (Lit 0)
ex5 = Add (Lit 5) (Add (Lit 6) (Var "x"))
ex6 = Dec "new" (Add (Lit 5) (Var "x")) `Seq` Div (Lit 5) (Var "new")

{-
eval :: Expr -> Int
eval (Lit i) = i
eval (Add e1 e2) =
    eval e1 + eval e2
eval (Div e1 e2) =
    if eval e2 == 0
        then error "division by zero"
        else eval e1 `div` eval e2
-}

eval :: Expr -> Eval Int
eval (Lit i) = return i

eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2

eval (Div e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    if v2 == 0
        then divisionByZero
        else return $ div v1 v2

eval (Var x) = getVar x
    
eval (Dec v ex) = eval ex >>= \a -> setVar v a >> return a
    
eval (Seq e1 e2) = do
    _ <- eval e1
    eval e2