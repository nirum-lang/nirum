{-# LANGUAGE OverloadedLists #-}
module Nirum.Constructs.AnnotationSpec where

import Prelude hiding (null)

import Test.Hspec.Meta
import qualified Data.Map.Strict as M

import Nirum.Constructs.Annotation as A
import Nirum.Constructs.Annotation.Internal ( AnnotationSet (AnnotationSet) )

spec :: Spec
spec = do
    let annotation = Annotation "foo" M.empty
        loremAnno = Annotation "lorem" [("arg", "ipsum")]
        escapeCharAnno = Annotation "quote" [("arg", "\"")]
        longNameAnno = Annotation "long-cat-is-long" [("long", "nyancat")]
        docsAnno = docs "Description"
    describe "Annotation" $ do
        describe "toCode Annotation" $
            it "prints annotation properly" $ do
                toCode annotation `shouldBe` "@foo"
                toCode loremAnno `shouldBe` "@lorem(arg = \"ipsum\")"
                toCode escapeCharAnno `shouldBe` "@quote(arg = \"\\\"\")"
        specify "docs" $
            docsAnno `shouldBe`
                Annotation "docs" [("docs", "Description\n")]
    describe "AnnotationSet" $ do
        specify "empty" $
            empty `shouldSatisfy` null
        specify "singleton" $ do
            singleton (Annotation "foo" []) `shouldBe`
                AnnotationSet [("foo", [])]
            singleton (Annotation "bar" [("arg", "baz")]) `shouldBe`
                AnnotationSet [("bar", [("arg", "baz")])]
        describe "fromList" $ do
            it "success" $ do
                let Right empty' = fromList []
                empty' `shouldSatisfy` null
                fromList [annotation] `shouldBe`
                    Right (AnnotationSet [("foo", [])])
            it "name duplication" $ do
                let duplicationAnnotations = fromList [ annotation
                                                      , loremAnno
                                                      , annotation
                                                      ]
                duplicationAnnotations `shouldBe`
                    Left (AnnotationNameDuplication "foo")
        specify "union" $ do
            let Right a = fromList [annotation, loremAnno]
            let Right b = fromList [docsAnno, escapeCharAnno]
            let c = AnnotationSet [("foo", [("arg", "bar")])]
            A.union a b `shouldBe`
                AnnotationSet [ ("foo", [])
                              , ("lorem", [("arg", "ipsum")])
                              , ("quote", [("arg", "\"")])
                              , ("docs", [("docs", "Description\n")])
                              ]
            A.union a c `shouldBe` a
        let Right annotationSet = fromList [ annotation
                                           , loremAnno
                                           , escapeCharAnno
                                           , longNameAnno
                                           , docsAnno
                                           ]
        it "toList" $
            fromList (toList annotationSet) `shouldBe` Right annotationSet
        describe "lookup" $ do
            it "should find proper annotation" $ do
                A.lookup "foo" annotationSet `shouldBe` Just annotation
                A.lookup "FOO" annotationSet `shouldBe` Just annotation
                A.lookup "lorem" annotationSet `shouldBe` Just loremAnno
                A.lookup "quote" annotationSet `shouldBe` Just escapeCharAnno
                A.lookup "long-cat-is-long" annotationSet
                    `shouldBe` Just longNameAnno
                A.lookup "long_cat_is_long" annotationSet
                    `shouldBe` Just longNameAnno
                A.lookup "docs" annotationSet `shouldBe` Just docsAnno
            it "should be Nothing if lookup fails" $ do
                A.lookup "bar" annotationSet `shouldBe` Nothing
                A.lookup "longCatIsLong" annotationSet `shouldBe` Nothing
        specify "lookupDocs" $ do
            A.lookupDocs annotationSet `shouldBe` Just "Description"
            A.lookupDocs empty `shouldBe` Nothing
        describe "insertDocs" $ do
            it "should insert the doc comment as an annotation" $
                A.insertDocs "yay" empty `shouldReturn`
                    AnnotationSet [("docs", [("docs", "yay\n")])]
            it "should fail on the annotation that already have a doc" $
                A.insertDocs "yay" annotationSet `shouldThrow` anyException
