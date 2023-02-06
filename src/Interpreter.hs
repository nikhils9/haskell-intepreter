{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter where

import Data.Functor.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M

data Expr = 
    Lit Int
    | Add Expr Expr
    | Div Expr Expr
    | Var String
    deriving (Show)

ex1, ex2, ex3, ex4,ex5 :: Expr
ex1 = Lit 5
ex2 = Add (Lit 5) (Add (Lit 6) (Lit 7))
ex3 = Add (Lit 7) (Div (Lit 9) (Lit 3))
ex4 = Div (Lit 7) (Lit 0)
ex5 = Add (Lit 5) (Add (Lit 6) (Var "x"))

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

newtype Eval a = Eval (ReaderT Env (ExceptT String Identity) a)
    deriving (Functor, Applicative, Monad, MonadError String, MonadReader Env)

type Env = Map String Int

eval :: Expr -> Eval Int
eval (Lit i) = return i
eval (Add e1 e2) =
    (+) <$> eval e1 <*> eval e2
eval (Div e1 e2) =
    do
        v1 <- eval e1
        v2 <- eval e2
        if v2 == 0
            then divisionByZero
            else return $ div v1 v2
eval (Var x) =
    do
        env <- ask
        case M.lookup x env of
            Nothing -> undefinedVariable x
            Just a -> return a

runEval :: Eval a -> Env -> Either String a
runEval (Eval e) env = runIdentity $ runExceptT $ runReaderT e env

divisionByZero :: Eval Int
divisionByZero = throwError "division by zero"

undefinedVariable :: String -> Eval Int
undefinedVariable s = throwError $ "undefined variable " ++ s
 
