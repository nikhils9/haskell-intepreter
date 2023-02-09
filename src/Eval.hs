{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval (
    Eval,
    Env,
    runEval,
    divisionByZero,
    undefinedVariable,
    getVar,
    setVar
) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Control.Monad.Except (ExceptT (..), MonadError, throwError, runExceptT)
import Control.Monad.State (StateT (..), MonadState, get, modify, evalStateT)
import Data.Map (Map)
import qualified Data.Map as M


newtype Eval a = Eval (StateT Env (ExceptT String Identity) a)
    deriving (Functor, Applicative, Monad, MonadError String, MonadState Env)

type Env = Map String Int

runEval :: Eval a -> Env -> Either String a
runEval (Eval e) env = runIdentity $ runExceptT $ evalStateT e env

divisionByZero :: Eval Int
divisionByZero = throwError "division by zero"

undefinedVariable :: String -> Eval Int
undefinedVariable s = throwError $ "undefined variable " ++ s
 
getVar :: String -> Eval Int
getVar x = do
    env <- get
    case M.lookup x env of
        Nothing -> undefinedVariable x
        Just a -> return a

setVar :: String -> Int -> Eval ()
setVar s i = modify $ M.insert s i