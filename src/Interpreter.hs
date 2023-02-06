module Interpreter where

data Expr = 
    Lit Int
    | Add Expr Expr
    | Div Expr Expr
    deriving (Show)

ex1, ex2, ex3, ex4 :: Expr
ex1 = Lit 5
ex2 = Add (Lit 5) (Add (Lit 6) (Lit 7))
ex3 = Add (Lit 7) (Div (Lit 9) (Lit 3))
ex4 = Div (Lit 7) (Lit 0)

{-
eval :: Expr -> Int
eval (Lit i) = i
eval (Add e1 e2) =
    eval e1 + eval e2
-}

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

eval :: Expr -> Either String Int
eval (Lit i) = return i
eval (Add e1 e2) =
    (+) <$> eval e1 <*> eval e2
eval (Div e1 e2) =
    case eval e2 of
        Right 0 -> divisionByZero
        _ -> div <$> eval e1 <*> eval e2

divisionByZero :: Either String Int
divisionByZero = Left "division by zero"
