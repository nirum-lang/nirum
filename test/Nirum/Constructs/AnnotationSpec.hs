{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.AnnotationSpec where

import Test.Hspec.Meta

import Nirum.Constructs.Annotation ( Annotation(Annotation)
                                   , toCode
                                   )

spec :: Spec
spec =
    describe "Annotation" $
        describe "toCode Annotation" $
            it "prints annotation properly" $
                toCode (Annotation "foo" "bar") `shouldBe` "[foo: \"bar\"]"
