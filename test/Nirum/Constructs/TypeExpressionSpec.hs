module Nirum.Constructs.TypeExpressionSpec where

import Test.Hspec.Meta

import Nirum.Constructs.TypeExpression (TypeExpression (..), toCode)

spec :: Spec
spec =
    describe "toCode TypeExpression" $ do
        context "TypeIdentifier" $
            it "reverses the given tree to the code string" $ do
                toCode (TypeIdentifier "text") `shouldBe` "text"
                toCode (TypeIdentifier "enum") `shouldBe` "`enum`"
        context "OptionModifier" $
            it "reverses the given tree to the code string" $ do
                toCode (OptionModifier "text") `shouldBe` "text?"
                toCode (OptionModifier $ SetModifier "text")
                    `shouldBe` "{text}?"
        context "SetModifier" $
            it "reverses the given tree to the code string" $ do
                toCode (SetModifier "text") `shouldBe` "{text}"
                toCode (SetModifier $ OptionModifier "text")
                    `shouldBe` "{text?}"
        context "ListModifier" $
            it "reverses the given tree to the code string" $
                toCode (ListModifier "text") `shouldBe` "[text]"
        context "MapModifier" $
            it "reverses the given tree to the code string" $ do
                toCode (MapModifier "uuid" "text") `shouldBe` "{uuid: text}"
                toCode (MapModifier "uuid" $ ListModifier "text")
                    `shouldBe` "{uuid: [text]}"
