{-# LANGUAGE OverloadedStrings #-}
module Nirum.VersionSpec where

import Data.Text (unpack)
import qualified Data.SemVer as SV
import Test.Hspec.Meta

import Nirum.Version (version, versionString, versionText)

spec :: Spec
spec = do
    describe "version" $ do
        it "must not error" $
            SV.toText version `shouldBe` versionText
            -- assertion is meaningless; it's just for testing whether
            -- `version` is evaluated without error
        it "is development version yet" $
            version `shouldSatisfy` SV.isDevelopment
        it "is the proper version" $
            -- is it a necessary test?
            version `shouldBe` SV.version 0 1 0 [] []
    describe "versionText" $ do
        it "is equivalent to version" $
            versionText `shouldBe` SV.toText version
        it "is equivalent to versionString" $
            unpack versionText `shouldBe` versionString
    describe "versionString" $
        it "is equivalent to version" $
            versionString `shouldBe` SV.toString version
