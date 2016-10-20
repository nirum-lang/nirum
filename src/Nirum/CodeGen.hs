{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Nirum.CodeGen ( CodeGen
                     , Failure
                     , fromString
                     , runCodeGen
                     ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.Except ( MonadError
                            , ExceptT (ExceptT)
                            , mapExceptT
                            , runExceptT
                            )
import Control.Monad.State (MonadState, State, mapState, runState)
import Data.Functor (Functor)


newtype CodeGen s e a = CodeGen (ExceptT e (State s) a)
    deriving ( Applicative
             , Functor
             , MonadError e
             , MonadState s
             )

class Failure s a where
    fromString :: MonadState s m => String -> m a

instance (Failure s e) => Monad (CodeGen s e) where
    return a = CodeGen $ ExceptT $ return (Right a)
    {-# INLINE return #-}
    (CodeGen m) >>= k = CodeGen $ ExceptT $ do
        a <- runExceptT m
        case a of
            Left e -> return (Left e)
            Right x -> let CodeGen n = k x in runExceptT n
    {-# INLINE (>>=) #-}
    fail str = CodeGen $ mapExceptT mutate (fromString str)
      where
        mutate = mapState (\ (a, s) -> case a of
                                          Left _ -> undefined
                                          Right e -> (Left e, s))
    {-# INLINE fail #-}

runCodeGen :: CodeGen s e a -> s -> (Either e a, s)
runCodeGen (CodeGen a) = runState (runExceptT a)
