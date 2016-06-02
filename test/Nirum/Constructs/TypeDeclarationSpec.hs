{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.Constructs.TypeDeclarationSpec where

import qualified Data.Text as T
import Test.Hspec.Meta

import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Declaration (Declaration(name, docs))
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Field(Field)
                                        , JsonType(String)
                                        , PrimitiveTypeIdentifier(Text)
                                        , Tag(Tag)
                                        , Type(..)
                                        , TypeDeclaration(..)
                                        )

spec :: Spec
spec = do
    describe "TypeDeclaration" $ do
        context "Alias" $ do
            let alias = Alias "text"
                a = TypeDeclaration { typename = "path"
                                    , type' = alias
                                    , typeDocs = Nothing
                                    }
                b = a { typeDocs = Just "docs"}
            specify "name" $ do
                name a `shouldBe` "path"
                name b `shouldBe` "path"
            specify "docs" $ do
                docs a `shouldBe` Nothing
                docs b `shouldBe` Just "docs"
            specify "toCode" $ do
                toCode a `shouldBe` "type path = text;"
                toCode b `shouldBe` "type path = text;\n# docs"
        context "BoxedType" $ do
            let boxed = BoxedType "float64"
                a = TypeDeclaration { typename = "offset"
                                    , type' = boxed
                                    , typeDocs = Nothing
                                    }
                b = a { typeDocs = Just "docs" }
            specify "name" $ do
                name a `shouldBe` "offset"
                name b `shouldBe` "offset"
            specify "docs" $ do
                docs a `shouldBe` Nothing
                docs b `shouldBe` Just "docs"
            specify "toCode" $ do
                toCode a `shouldBe` "boxed offset (float64);"
                toCode b `shouldBe` "boxed offset (float64);\n# docs"
        context "EnumType" $ do
            let enumMembers = [ EnumMember "kr" Nothing
                              , EnumMember "jp" $ Just "Japan"
                              , EnumMember "us" $ Just "United States"
                              ] :: DeclarationSet EnumMember
                enum = EnumType enumMembers
                a = TypeDeclaration { typename = "country"
                                    , type' = enum
                                    , typeDocs = Nothing
                                    }
                b = a { typeDocs = Just "country codes" }
            specify "toCode" $ do
                toCode a `shouldBe` "enum country\n\
                                    \    = kr\n\
                                    \    | jp\n\
                                    \    # Japan\n\
                                    \    | us\n\
                                    \    # United States\n\
                                    \    ;"
                toCode b `shouldBe` "enum country\n\
                                    \    # country codes\n\
                                    \    = kr\n\
                                    \    | jp\n\
                                    \    # Japan\n\
                                    \    | us\n\
                                    \    # United States\n\
                                    \    ;"
        context "RecordType" $ do
            let fields' = [ Field "name" "text" Nothing
                          , Field "dob" "date" $ Just "date of birth"
                          , Field "gender" "gender" Nothing
                          ] :: DeclarationSet Field
                record = RecordType fields'
                a = TypeDeclaration { typename = "person"
                                    , type' = record
                                    , typeDocs = Nothing
                                    }
                b = a { typeDocs = Just "person record type" }
            specify "toCode" $ do
                toCode a `shouldBe` "record person (\n\
                                    \    text name,\n\
                                    \    date dob,\n\
                                    \    # date of birth\n\
                                    \    gender gender,\n\
                                    \);"
                toCode b `shouldBe` "record person (\n\
                                    \    # person record type\n\n\
                                    \    text name,\n\
                                    \    date dob,\n\
                                    \    # date of birth\n\
                                    \    gender gender,\n\
                                    \);"
        context "UnionType" $ do
            let circleFields = [ Field "origin" "point" Nothing
                               , Field "radius" "offset" Nothing
                               ]
                rectangleFields = [ Field "upper-left" "point" Nothing
                                  , Field "lower-right" "point" Nothing
                                  ]
                tags' = [ Tag "circle" circleFields Nothing
                        , Tag "rectangle" rectangleFields Nothing
                        , Tag "none" [] Nothing
                        ]
                union = UnionType tags'
                a = TypeDeclaration { typename = "shape"
                                    , type' = union
                                    , typeDocs = Nothing
                                    }
                b = a { typeDocs = Just "shape type" }
            specify "toCode" $ do
                toCode a `shouldBe` "union shape\n\
                                    \    = circle (point origin, \
                                                  \offset radius,)\n\
                                    \    | rectangle (point upper-left, \
                                                     \point lower-right,)\n\
                                    \    | none\n\
                                    \    ;"
                toCode b `shouldBe` "union shape\n\
                                    \    # shape type\n\
                                    \    = circle (point origin, \
                                                  \offset radius,)\n\
                                    \    | rectangle (point upper-left, \
                                                     \point lower-right,)\n\
                                    \    | none\n\
                                    \    ;"
        context "PrimitiveType" $ do
            let primitiveType = PrimitiveType Text String
                decl = TypeDeclaration "text" primitiveType Nothing
            specify "toCode" $
                T.lines (toCode decl) `shouldSatisfy`
                    all (T.isPrefixOf "//" . T.stripStart)
    describe "EnumMember" $ do
        let kr = EnumMember "kr" Nothing
            jp = EnumMember "jp" $ Just "Japan"
        specify "toCode" $ do
            toCode kr `shouldBe` "kr"
            toCode jp `shouldBe` "jp\n# Japan"
        specify "fromString" $
            "test" `shouldBe` EnumMember "test" Nothing
    describe "Field" $
        specify "toCode" $ do
            toCode (Field "name" "text" Nothing) `shouldBe` "text name,"
            toCode (Field "dob" "date" $ Just "date of birth") `shouldBe`
                "date dob,\n# date of birth"
    describe "Tag" $
        specify "toCode" $ do
            let fields' = [ Field "origin" "point" Nothing
                          , Field "radius" "offset" Nothing
                          ]
            toCode (Tag "circle" fields' Nothing)
                `shouldBe` "circle (point origin, offset radius,)"
            toCode (Tag "circle" fields' $ Just "docs")
                `shouldBe` "circle (point origin, offset radius,)\n# docs"
            toCode (Tag "unit" [] Nothing) `shouldBe` "unit"
            toCode (Tag "unit" [] $ Just "docs") `shouldBe` "unit\n# docs"
