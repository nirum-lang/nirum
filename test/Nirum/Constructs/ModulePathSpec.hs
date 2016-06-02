{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.ModulePathSpec where

import Control.Exception (evaluate)
import GHC.Exts (IsList(fromList, toList))

import Test.Hspec.Meta

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.ModulePath (ModulePath(ModuleName, ModulePath))

spec :: Spec
spec =
    describe "ModulePath" $ do
        let foo = ModuleName "foo"
            fooBar = ModulePath (ModuleName "foo") "bar"
            fooBarBaz = ModulePath (ModulePath (ModuleName "foo") "bar") "baz"
            fooBarBaz2 = ModulePath (ModuleName "foo") "bar-baz2"
        context "Construct" $
            specify "toCode" $ do
                toCode foo `shouldBe` "foo"
                toCode fooBar `shouldBe` "foo.bar"
                toCode fooBarBaz `shouldBe` "foo.bar.baz"
                toCode fooBarBaz2 `shouldBe` "foo.bar-baz2"
        context "IsList" $ do
            specify "fromList" $ do
                evaluate (fromList [] :: ModulePath) `shouldThrow`
                    errorCall "ModulePath cannot be empty"
                fromList ["foo"] `shouldBe` foo
                fromList ["foo", "bar"] `shouldBe` fooBar
                fromList ["foo", "bar", "baz"] `shouldBe` fooBarBaz
                fromList ["foo", "bar-baz2"] `shouldBe` fooBarBaz2
            specify "toList" $ do
                toList foo `shouldBe` ["foo"]
                toList fooBar `shouldBe` ["foo", "bar"]
                toList fooBarBaz `shouldBe` ["foo", "bar", "baz"]
                toList fooBarBaz2 `shouldBe` ["foo", "bar-baz2"]
