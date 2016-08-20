module Nirum.Constructs.NameSpec where

import Test.Hspec.Meta

import Nirum.Constructs.Name (Name(Name), isComplex, isSimple, toCode)

spec :: Spec
spec =
    describe "Name" $ do
        describe "toCode Name" $ do
            it "prints only its facialName if its behindName is the same" $
                toCode (Name "a" "a") `shouldBe` "a"
            it "prints only facialName with behindName if they are not same" $
                toCode (Name "a" "b") `shouldBe` "a/b"
        specify "fromString" $
            ("foo" :: Name) `shouldBe` Name "foo" "foo"
        specify "isSimple" $ do
            Name "foo" "foo" `shouldSatisfy` isSimple
            Name "foo" "bar" `shouldNotSatisfy` isSimple
        specify "isComplex" $ do
            Name "foo" "bar" `shouldSatisfy` isComplex
            Name "foo" "foo" `shouldNotSatisfy` isComplex
