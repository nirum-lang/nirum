{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.DeclarationSpec where

import Test.Hspec.Meta

import Nirum.Constructs (Construct(..))
import Nirum.Constructs.Declaration (Docs(Docs), toCodeWithPrefix)

spec :: Spec
spec =
    describe "Docs" $ do
        context "toCode" $ do
            it "has leading sharps every line" $ do
                toCode (Docs "test") `shouldBe` "# test"
                toCode (Docs "test\ntest2") `shouldBe` "# test\n# test2"
            specify "... except of the last EOL" $ do
                toCode (Docs "test\n") `shouldBe` "# test"
                toCode (Docs "test\ntest2\n") `shouldBe` "# test\n# test2"
        context "toCodeWithPrefix" $ do
            it "simply returns an empty string if input is Nothing" $ do
                toCodeWithPrefix "\n" Nothing `shouldBe` ""
                toCodeWithPrefix "(prefix)" Nothing `shouldBe` ""
            it "prepends the given prefix if input is Just Docs" $ do
                toCodeWithPrefix "\n" (Just "docs") `shouldBe` "\n# docs"
                toCodeWithPrefix "(prefix)" (Just "doc\ndoc")
                    `shouldBe` "(prefix)# doc\n# doc"
        context "fromString" $ do
            it "is equivalent to Docs data constructor" $ do
                ("test\n" :: Docs) `shouldBe` Docs "test\n"
                ("test\ntest2\n" :: Docs) `shouldBe` Docs "test\ntest2\n"
            specify "... except it automatically appends EOL if omitted" $ do
                ("test" :: Docs) `shouldBe` Docs "test\n"
                ("test\ntest2" :: Docs) `shouldBe` Docs "test\ntest2\n"
