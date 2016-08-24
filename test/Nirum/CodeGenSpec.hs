{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, RankNTypes, MultiParamTypeClasses #-}
module Nirum.CodeGenSpec where

import Control.Monad.Except (throwError)
import Control.Monad.State (modify)
import Data.Text as T (Text, pack)

import Test.Hspec.Meta

import Nirum.CodeGen (CodeGen, Failure, fromString, runCodeGen)


data SampleError = SampleError Text
    deriving (Eq, Ord, Show)

instance forall s. Failure s SampleError where
    fromString = return . SampleError . T.pack


spec :: Spec
spec = parallel $ do
    specify "fail" $ do
        let codeGen' :: CodeGen Integer SampleError () = do
                modify (+1)
                modify (+1)
                fail "test"
        runCodeGen codeGen' 0 `shouldBe` (Left (SampleError "test"), 2)
        let codeGen'' :: CodeGen Integer SampleError Integer = do
                modify (+1)
                _ <- fail "test"
                modify (+1)
                return 42
        runCodeGen codeGen'' 0 `shouldBe` (Left (SampleError "test"), 1)
    specify "throwError" $ do
        let codeGen' :: CodeGen Integer SampleError () = do
                modify (+1)
                _ <- throwError $ SampleError "test"
                modify (+2)
        runCodeGen codeGen' 0 `shouldBe` (Left (SampleError "test"), 1)
