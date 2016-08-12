{-# LANGUAGE OverloadedStrings #-}
module Nirum.Constructs.AnnotationSpec where

import Test.Hspec.Meta
import qualified Data.Map.Strict as M

import Nirum.Constructs.Annotation
    ( Annotation (Annotation)
    , AnnotationSet (AnnotationSet)
    , NameDuplication (AnnotationNameDuplication)
    , empty
    , fromList
    , toCode
    , toList
    )

spec :: Spec
spec = do
    let annotation = Annotation "foo" "bar"
        loremAnno = Annotation "lorem" "ipsum"
    describe "Annotation" $
        describe "toCode Annotation" $
            it "prints annotation properly" $
                toCode annotation `shouldBe` "[foo: \"bar\"]"
    describe "AnnotationSet" $ do
        it "empty" $ empty `shouldBe` AnnotationSet M.empty
        describe "fromList" $ do
            it "success" $ do
                fromList [] `shouldBe` Right (AnnotationSet M.empty)
                fromList [annotation] `shouldBe` Right
                    (AnnotationSet $ M.fromList [("foo", annotation)])
            it "name duplication" $ do
                let duplicationAnnotations = fromList [ annotation
                                                      , loremAnno
                                                      , annotation
                                                      ]
                duplicationAnnotations `shouldBe`
                    Left (AnnotationNameDuplication "foo")
        let annotationSet = case fromList [annotation, loremAnno] of
                                Right set -> set
                                Left _ -> empty
        it "toList" $
            fromList (toList annotationSet) `shouldBe` Right annotationSet
