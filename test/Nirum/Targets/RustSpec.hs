{-# LANGUAGE OverloadedLists, PartialTypeSignatures #-}
module Nirum.Targets.RustSpec where

import qualified Data.Set as S
import Test.Hspec.Meta

import Nirum.Constructs.ModulePath
import Nirum.Targets.Rust


spec :: Spec
spec = parallel $
    describe "childModules" $ do
        specify "empty list" $ do
            childModules S.empty ["a"] `shouldBe` []
            childModules S.empty ["a", "b"] `shouldBe` []
        specify "singleton" $ do
            childModules ([["a"]] :: [ModulePath]) ["a"] `shouldBe` []
            childModules (S.singleton ["foo"]) ["foo"] `shouldBe` []
            childModules ([["foo"]] :: [ModulePath]) ["bar"] `shouldBe` []
        specify "simple" $ do
            let input = [ ["a"]
                        , ["a", "b"]
                        , ["a", "c", "e"]
                        , ["b"]
                        , ["b", "c"]
                        , ["b", "d"]
                        ] :: [ModulePath]
            childModules input ["a"] `shouldBe` ["b", "c"]
            childModules input ["b"] `shouldBe` ["c", "d"]
            childModules input ["c"] `shouldBe` []
            childModules input ["a", "b"] `shouldBe` []
            childModules input ["a", "c"] `shouldBe` ["e"]
