{-# LANGUAGE OverloadedLists, QuasiQuotes #-}
module Nirum.Constructs.TypeDeclarationSpec where


import Data.String.QQ (s)
import qualified Data.Text as T
import Test.Hspec.Meta

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Annotation hiding (docs, fromList, name)
import Nirum.Constructs.Annotation.Internal hiding (name)
import Nirum.Constructs.Declaration (Declaration (name), docs)
import Nirum.Constructs.DeclarationSet hiding (empty)
import Nirum.Constructs.Service (Method (Method), Service (Service))
import Nirum.Constructs.TypeDeclaration ( EnumMember (EnumMember)
                                        , Field (Field)
                                        , JsonType (String)
                                        , PrimitiveTypeIdentifier (Text)
                                        , Tag (Tag)
                                        , Type (..)
                                        , TypeDeclaration (..)
                                        , unionType
                                        )
import Util (singleDocs)

barAnnotationSet :: AnnotationSet
barAnnotationSet = singleton $ Annotation "bar" [("val", AText "baz")]

spec :: Spec
spec = do
    describe "TypeDeclaration" $ do
        context "Alias" $ do
            let alias = Alias "text"
                a = TypeDeclaration { typename = "path"
                                    , type' = alias
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = singleDocs "docs" }
            specify "name" $ do
                name a `shouldBe` "path"
                name b `shouldBe` "path"
            specify "docs" $ do
                docs a `shouldBe` Nothing
                docs b `shouldBe` Just "docs"
            specify "toCode" $ do
                toCode a `shouldBe` "type path = text;"
                toCode b `shouldBe` "type path = text;\n# docs"
        context "UnboxedType" $ do
            let unboxed = UnboxedType "float64"
                a = TypeDeclaration { typename = "offset"
                                    , type' = unboxed
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = singleDocs "docs" }
            specify "name" $ do
                name a `shouldBe` "offset"
                name b `shouldBe` "offset"
            specify "docs" $ do
                docs a `shouldBe` Nothing
                docs b `shouldBe` Just "docs"
            specify "toCode" $ do
                toCode a `shouldBe` "unboxed offset (float64);"
                toCode b `shouldBe` "unboxed offset (float64);\n# docs"
        context "EnumType" $ do
            let enumMembers = [ EnumMember "kr" empty
                              , EnumMember "jp" (singleDocs "Japan")
                              , EnumMember "us" (singleDocs "United States")
                              ] :: DeclarationSet EnumMember
                enum = EnumType enumMembers
                a = TypeDeclaration { typename = "country"
                                    , type' = enum
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = singleDocs "country codes" }
            specify "toCode" $ do
                toCode a `shouldBe` [s|
enum country
    = kr
    | jp
    # Japan
    | us
    # United States
    ;|]
                toCode b `shouldBe` [s|
enum country
    # country codes
    = kr
    | jp
    # Japan
    | us
    # United States
    ;|]
        context "RecordType" $ do
            let fields' = [ Field "name" "text" empty
                          , Field "dob" "date" (singleDocs "date of birth")
                          , Field "gender" "gender" empty
                          ] :: DeclarationSet Field
                record = RecordType fields'
                a = TypeDeclaration { typename = "person"
                                    , type' = record
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = singleDocs "person record type" }
            specify "toCode" $ do
                toCode a `shouldBe` [s|
record person (
    text name,
    date dob,
    # date of birth
    gender gender,
);|]
                toCode b `shouldBe` [s|
record person (
    # person record type

    text name,
    date dob,
    # date of birth
    gender gender,
);|]
        context "UnionType" $ do
            let circleFields = [ Field "origin" "point" empty
                               , Field "radius" "offset" empty
                               ]
                rectangleFields = [ Field "upper-left" "point" empty
                                  , Field "lower-right" "point" empty
                                  ]
                tags' = [ Tag "circle" circleFields empty
                        , Tag "rectangle" rectangleFields empty
                        , Tag "none" [] empty
                        ]
            let Right union' = unionType tags' Nothing
                a = TypeDeclaration { typename = "shape"
                                    , type' = union'
                                    , typeAnnotations = empty
                                    }
                b = a { typeAnnotations = singleDocs "shape type" }
            specify "toCode" $ do
                toCode a `shouldBe` [s|
union shape
    = circle (point origin, offset radius,)
    | rectangle (point upper-left, point lower-right,)
    | none
    ;|]
                toCode b `shouldBe` [s|
union shape
    # shape type
    = circle (point origin, offset radius,)
    | rectangle (point upper-left, point lower-right,)
    | none
    ;|]
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
                                       (singleDocs "Null service declaration.")
                pingService = Service
                    [Method "ping" [] (Just "bool") Nothing empty]
                pingDecl = ServiceDeclaration "ping-service" pingService empty
                pingDecl' =
                    ServiceDeclaration "ping-service" pingService
                                       (singleDocs "Ping service declaration.")
                annoDecl = ServiceDeclaration "anno-service" pingService
                                              barAnnotationSet
            specify "toCode" $ do
                toCode nullDecl `shouldBe` "service null-service ();"
                toCode nullDecl' `shouldBe` [s|
service null-service (
    # Null service declaration.
);|]
                toCode pingDecl `shouldBe`
                    "service ping-service (bool ping ());"
                toCode pingDecl' `shouldBe` [s|
service ping-service (
    # Ping service declaration.
    bool ping ()
);|]
                toCode annoDecl `shouldBe`
                    "@bar(val = \"baz\")\nservice anno-service (bool ping ());"
                -- TODO: more tests
        context "Import" $ do
            let import' = Import ["foo", "bar"] "baz" "baz" empty
            let importAliasing = Import ["foo", "bar"] "qux" "baz" empty
            specify "name" $
                name import' `shouldBe` "baz"
            specify "docs" $
                docs import' `shouldBe` Nothing
            specify "toCode" $
                toCode import' `shouldBe` "import foo.bar (baz);\n"
            specify "name" $
                name importAliasing `shouldBe` "qux"
            specify "docs" $
                docs importAliasing `shouldBe` Nothing
            specify "toCode" $
                toCode importAliasing `shouldBe`
                    "import foo.bar (baz as qux);\n"

        context "member/tag name shadowing" $ do
            let fromRight either' = head [v | Right v <- [either']]
            let unionType' t a = fromRight $ unionType t a
            let unionFoo = TypeDeclaration
                    { typename = "foo"
                    , type' = unionType'
                          [ Tag "bar" [Field "a" "text" empty] empty
                          , Tag "baz" [Field "b" "text" empty] empty
                          ]
                          Nothing
                    , typeAnnotations = empty
                    }
            let enumQux = TypeDeclaration
                    { typename = "qux"
                    , type' = EnumType
                          [ EnumMember "quux" empty
                          , EnumMember "bar" empty
                          ]
                    , typeAnnotations = empty
                    }
            let unboxedBar = TypeDeclaration "bar" (UnboxedType "text") empty
            specify "a union tag is disallowed to shadow a type name" $ do
                fromList [unionFoo, unboxedBar]
                    `shouldBe` Left (FacialNameDuplication "bar")
                fromList
                    [ TypeDeclaration
                          { typename = "bar"
                          , type' = unionType'
                                [ Tag "foo" [Field "a" "text" empty] empty
                                , Tag "bar" [Field "b" "text" empty] empty
                                ]
                                Nothing
                          , typeAnnotations = empty
                          }
                    ]
                    `shouldBe` Left (FacialNameDuplication "bar")
            specify "an enum member is disallowed to shadow a type name" $ do
                fromList [enumQux, unboxedBar]
                    `shouldBe` Left (FacialNameDuplication "bar")
                fromList
                    [ TypeDeclaration
                          { typename = "foo"
                          , type' = EnumType
                                [ EnumMember "foo" empty
                                , EnumMember "bar" empty
                                ]
                          , typeAnnotations = empty
                          }
                    ]
                    `shouldBe` Left (FacialNameDuplication "foo")
            it "is disallowed two unions to have a tag of the same name" $
                fromList
                    [ unionFoo
                    , TypeDeclaration
                          { typename = "qux"
                          , type' = unionType'
                                [ Tag "quux" [Field "c" "text" empty] empty
                                , Tag "baz" [Field "d" "text" empty] empty
                                ]
                                Nothing
                          , typeAnnotations = empty
                          }
                    ]
                    `shouldBe` Left (FacialNameDuplication "baz")
            it "is disallowed two enums to have a member of the same name" $
                fromList
                    [ TypeDeclaration
                          { typename = "foo"
                          , type' = EnumType
                                [ EnumMember "bar" empty
                                , EnumMember "baz" empty
                                ]
                          , typeAnnotations = empty
                          }
                    , enumQux
                    ]
                    `shouldBe` Left (FacialNameDuplication "bar")
            it ("is disallowed an enum member and a union tag to " ++
                "have the same name") $
                fromList [unionFoo, enumQux] `shouldBe`
                    Left (FacialNameDuplication "bar")

    describe "EnumMember" $ do
        let kr = EnumMember "kr" empty
            jp = EnumMember "jp" (singleDocs "Japan")
        specify "toCode" $ do
            toCode kr `shouldBe` "kr"
            toCode jp `shouldBe` "jp\n# Japan"
        specify "fromString" $
            "test" `shouldBe` EnumMember "test" empty
    describe "Field" $
        specify "toCode" $ do
            toCode (Field "name" "text" empty) `shouldBe` "text name,"
            toCode (Field "dob" "date" (singleDocs "date of birth")) `shouldBe`
                "date dob,\n# date of birth"
    describe "Tag" $
        specify "toCode" $ do
            let fields' = [ Field "origin" "point" empty
                          , Field "radius" "offset" empty
                          ]
            toCode (Tag "circle" fields' empty)
                `shouldBe` "circle (point origin, offset radius,)"
            toCode (Tag "circle" fields' (singleDocs "docs"))
                `shouldBe` "circle (point origin, offset radius,)\n# docs"
            toCode (Tag "unit" [] empty) `shouldBe` "unit"
            toCode (Tag "unit" [] (singleDocs "docs")) `shouldBe` "unit\n# docs"
