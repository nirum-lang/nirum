{-# LANGUAGE OverloadedLists #-}
module Nirum.Constructs.TypeDeclarationSpec where

import Data.Either (rights)
import qualified Data.Text as T
import Test.Hspec.Meta

import Nirum.Constructs (Construct(toCode))
import qualified Nirum.Constructs.Annotation as A
import Nirum.Constructs.Annotation ( Annotation (Annotation)
                                   , AnnotationSet
                                   , fromList
                                   , empty
                                   , singleton
                                   )
import Nirum.Constructs.Declaration (Declaration(name, docs), Docs)
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.Service (Method(Method), Service(Service))
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Field(Field)
                                        , JsonType(String)
                                        , PrimitiveTypeIdentifier(Text)
                                        , Tag(Tag)
                                        , Type(..)
                                        , TypeDeclaration(..)
                                        )

barAnnotationSet :: AnnotationSet
barAnnotationSet = head $ rights [fromList [Annotation "bar" (Just "baz")]]

docsAnno :: Docs -> AnnotationSet
docsAnno = singleton . A.docs

spec :: Spec
spec = do
    describe "TypeDeclaration" $ do
        context "Alias" $ do
            let alias = Alias "text"
                a = TypeDeclaration { typename = "path"
                                    , type' = alias
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = docsAnno "docs" }
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
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = docsAnno "docs" }
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
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = docsAnno "country codes" }
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
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = docsAnno "person record type" }
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
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = docsAnno "shape type" }
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
                decl = TypeDeclaration "text" primitiveType empty
            specify "toCode" $
                T.lines (toCode decl) `shouldSatisfy`
                    all (T.isPrefixOf "//" . T.stripStart)
        context "ServiceDeclaration" $ do
            let nullService = Service []
                nullDecl = ServiceDeclaration "null-service" nullService empty
                nullDecl' =
                    ServiceDeclaration "null-service" nullService
                                       (docsAnno "Null service declaration.")
                pingService = Service [ Method "ping" [] "bool" Nothing empty ]
                pingDecl = ServiceDeclaration "ping-service" pingService empty
                pingDecl' =
                    ServiceDeclaration "ping-service" pingService
                                       (docsAnno "Ping service declaration.")
                annoDecl = ServiceDeclaration "anno-service" pingService
                                              barAnnotationSet
            specify "toCode" $ do
                toCode nullDecl `shouldBe` "service null-service ();"
                toCode nullDecl' `shouldBe` "service null-service (\n\
                                            \    # Null service declaration.\n\
                                            \);"
                toCode pingDecl `shouldBe`
                    "service ping-service (bool ping ());"
                toCode pingDecl' `shouldBe`
                    "service ping-service (\n\
                    \    # Ping service declaration.\n\
                    \    bool ping ()\n\
                    \);"
                toCode annoDecl `shouldBe`
                    "@bar(\"baz\")\n\
                    \service anno-service (bool ping ());"
                -- TODO: more tests
        context "Import" $ do
            let import' = Import ["foo", "bar"] "baz"
            specify "name" $
                name import' `shouldBe` "baz"
            specify "docs" $
                docs import' `shouldBe` Nothing
            specify "toCode" $
                toCode import' `shouldBe` "import foo.bar (baz);\n"
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
