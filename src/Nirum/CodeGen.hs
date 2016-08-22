{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nirum.CodeGen ( CodeGen
                     , runCodeGen
                     ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState, StateT(StateT), runStateT)
import Data.Functor (Functor)
import Data.String (IsString, fromString)


newtype (IsString e) => CodeGen c e a = CodeGen (StateT c (Either e) a)
    deriving ( Applicative
             , Functor
             , MonadError e
             , MonadState c
             )

instance (IsString e) => Monad (CodeGen c e) where
    return a = CodeGen $ StateT $ \s -> return (a, s)
    {-# INLINE return #-}
    (CodeGen m) >>= k = CodeGen $ StateT $ \s -> do
        ~(a, s') <- runStateT m s
        let (CodeGen n) = k a
        runStateT n s'
    {-# INLINE (>>=) #-}
    fail str = CodeGen $ StateT $ \_ -> Left $ fromString str
    {-# INLINE fail #-}

runCodeGen :: (IsString e) => CodeGen c e a -> c -> Either e (a, c)
runCodeGen (CodeGen a) = runStateT a
