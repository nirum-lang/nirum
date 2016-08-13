{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.Constructs.ModulePathSpec where

import Control.Exception (evaluate)
import GHC.Exts (IsList(fromList, toList))

import System.FilePath ((</>))
import Test.Hspec.Meta

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.ModulePath ( ModulePath(ModuleName, ModulePath)
                                   , ancestors
                                   , fromFilePath
                                   , fromIdentifiers
                                   )

spec :: Spec
spec =
    describe "ModulePath" $ do
        let foo = ModuleName "foo"
            fooBar = ModulePath (ModuleName "foo") "bar"
            fooBarBaz = ModulePath (ModulePath (ModuleName "foo") "bar") "baz"
            fooBarBaz2 = ModulePath (ModuleName "foo") "bar-baz2"
        specify "fromIdentifiers" $ do
            fromIdentifiers [] `shouldBe` Nothing
            fromIdentifiers ["foo"] `shouldBe` Just foo
            fromIdentifiers ["Foo"] `shouldBe` Just foo
            fromIdentifiers ["FOO"] `shouldBe` Just foo
            fromIdentifiers ["foo", "bar"] `shouldBe` Just fooBar
            fromIdentifiers ["foo", "bar", "baz"] `shouldBe` Just fooBarBaz
            fromIdentifiers ["Foo", "BAR", "baZ"] `shouldBe` Just fooBarBaz
            fromIdentifiers ["foo", "bar-baz2"] `shouldBe` Just fooBarBaz2
            fromIdentifiers ["FOO", "bar_baz2"] `shouldBe` Just fooBarBaz2
        specify "fromFilePath" $ do
            fromFilePath "" `shouldBe` Nothing
            fromFilePath "foo.nrm" `shouldBe` Just foo
            fromFilePath "FOO.NRM" `shouldBe` Just foo
            fromFilePath "Foo.Nrm" `shouldBe` Just foo
            fromFilePath ("foo" </> "bar.nrm") `shouldBe` Just fooBar
            fromFilePath ("FOO" </> "bar.NRM") `shouldBe` Just fooBar
            fromFilePath ("foo" </> "bar" </> "baz.nrm") `shouldBe`
                Just fooBarBaz
            fromFilePath ("foo" </> "BAR" </> "baz.nrm") `shouldBe`
                Just fooBarBaz
            fromFilePath ("foo" </> "bar-baz2.nrm") `shouldBe` Just fooBarBaz2
            fromFilePath ("foo" </> "bar_baz2.NRM") `shouldBe` Just fooBarBaz2
        specify "ancestors" $ do
            ancestors ["foo", "bar", "baz"] `shouldBe`
                [ ["foo"]
                , ["foo", "bar"]
                , ["foo", "bar", "baz"]
                ]
            ancestors ["foo"] `shouldBe` [["foo"]]
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
