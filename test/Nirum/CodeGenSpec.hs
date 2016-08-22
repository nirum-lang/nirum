{-# LANGUAGE ScopedTypeVariables #-}
module Nirum.CodeGenSpec where

import Control.Monad.Except (throwError)
import Control.Monad.State (modify)
import Data.Text (Text)

import Test.Hspec.Meta

import Nirum.CodeGen (CodeGen, runCodeGen)


spec :: Spec
spec = parallel $ do
    describe "CodeGen" $ do
        specify "fail" $ do
            let codeGen' :: CodeGen Integer String () = do
                    modify (+1)
                    modify (+1)
                    fail "test"
            runCodeGen codeGen' 0 `shouldBe` Left "test"
            let codeGen'' :: CodeGen Integer String Integer = do
                    modify (+1)
                    fail "test"
                    modify (+1)
                    return 42
            runCodeGen codeGen'' 0 `shouldBe` Left "test"
        specify "throwError" $ do
            let codeGen' :: CodeGen Integer (Maybe Text) () = do
                    throwError $ Just "test"
            runCodeGen codeGen' 0 `shouldBe` Left (Just "test")
