{-# LANGUAGE OverloadedLists, QuasiQuotes, TypeFamilies #-}
module Nirum.ParserSpec where

import Control.Monad (forM_)
import Data.Either (isLeft, isRight, lefts, rights)
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import System.Directory (getDirectoryContents)

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import Data.String.QQ (s)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Test.Hspec.Meta
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Error (errorPos, parseErrorPretty)
import Text.Megaparsec.Pos (Pos, SourcePos (sourceColumn, sourceLine), mkPos)
import Text.Megaparsec.Text (Parser)

import qualified Nirum.Parser as P
import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Annotation as A ( Annotation (Annotation)
                                        , AnnotationSet
                                        , empty
                                        , fromList
                                        , union
                                        )
import Nirum.Constructs.Docs (Docs (Docs))
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.DeclarationSetSpec (SampleDecl (..))
import Nirum.Constructs.Identifier (fromText)
import Nirum.Constructs.Module (Module (Module))
import Nirum.Constructs.Name (Name (..))
import Nirum.Constructs.Service ( Method (Method)
                                , Parameter (Parameter)
                                , Service (Service)
                                )
import Nirum.Constructs.TypeDeclaration ( EnumMember (EnumMember)
                                        , Field (Field, fieldAnnotations)
                                        , Tag (Tag, tagAnnotations, tagFields)
                                        , Type (..)
                                        , TypeDeclaration (..)
                                        )
import Nirum.Constructs.TypeExpression ( TypeExpression ( ListModifier
                                                        , MapModifier
                                                        , OptionModifier
                                                        , SetModifier
                                                        , TypeIdentifier
                                                        )
                                       )
import Util (singleDocs)

shouldBeRight :: (Eq l, Eq r, Show l, Show r) => Either l r -> r -> Expectation
shouldBeRight actual expected = actual `shouldBe` Right expected

erroredPos :: Either P.ParseError a -> (Pos, Pos)
erroredPos left =
    (sourceLine pos, sourceColumn pos)
  where
    error' = head $ lefts [left] :: P.ParseError
    pos = NE.head (errorPos error') :: SourcePos

helperFuncs :: (Show a)
            => Parser a
            -> ( T.Text -> Either P.ParseError a
               , T.Text -> Int -> Int -> Expectation
               )
helperFuncs parser =
    (parse', expectError)
  where
    parserAndEof = do
        r <- parser
        eof
        return r
    parse' = runParser parserAndEof ""
    expectError inputString line col = do
        line' <- mkPos line
        col' <- mkPos col
        let parseResult = parse' inputString
        parseResult `shouldSatisfy` isLeft
        let Left parseError = parseResult
            msg = parseErrorPretty parseError
        -- msg is just for debug print
        (erroredPos parseResult, msg) `shouldBe` ((line', col'), msg)


fooAnnotationSet :: AnnotationSet
fooAnnotationSet = head $ rights [fromList [Annotation "foo" (Just "bar")]]

bazAnnotationSet :: AnnotationSet
bazAnnotationSet = head $ rights [fromList [Annotation "baz" Nothing]]


spec :: Spec
spec = do
    describe "identifier" $ do
        let (parse', expectError) = helperFuncs P.identifier
        it "fails to parse an empty string" $
            expectError "" 1 1
        it "fails to parse an identifier starting with hyphen/underscore" $ do
            expectError "-ident" 1 1
            expectError "-ident-ifier" 1 1
            expectError "_ident" 1 1
            expectError "_ident_ifier" 1 1
            expectError "_ident-ifier" 1 1
            expectError "-ident_ifier" 1 1
        it "fails to parse an identifier ending with hyphen/underscore" $ do
            expectError "ident-" 1 7
            expectError "ident-ifier-" 1 13
            expectError "ident_" 1 7
            expectError "ident_ifier_" 1 13
            expectError "ident_ifier-" 1 13
            expectError "ident-ifier_" 1 13
        it "fails to parse an identifier with double hyphens/underscores" $ do
            expectError "ident--ifier" 1 7
            expectError "ident__ifier" 1 7
            expectError "ident-_ifier" 1 7
            expectError "ident_-ifier" 1 7
        it "fails to parse an identifier containing disallowed chars" $ do
            expectError "\xbb34\xd6a8\xd55c-\xc2dd\xbcc4\xc790" 1 1
            expectError "invalid-\xc2dd\xbcc4\xc790" 1 9
        let keywords = [ "enum", "record", "type", "unboxed", "union"
                       , "Enum", "rEcord", "tyPE", "UNBOXED", "unioN"
                       ] :: [T.Text]
        it "fails to parse bare identifier if it's a reserved keyword" $
            forM_ keywords $ \ kwd ->
                expectError kwd 1 1
        let identifier' = fromJust . fromText
        it "emits Identifier if succeeded to parse" $ do
            parse' "identifier" `shouldBeRight` identifier' "identifier"
            parse' "valid-identifier" `shouldBeRight`
                identifier' "valid-identifier"
            parse' "valid_identifier" `shouldBeRight`
                identifier' "valid_identifier"
        it "can parse even if a keyword is a prefix of identifier" $ do
            parse' "enumeration" `shouldBeRight` identifier' "enumeration"
            parse' "types" `shouldBeRight` identifier' "types"
        it "can parse reserved keywords iff they are quoted" $
            forM_ keywords $ \ kwd ->
                parse' ('`' `T.cons` kwd `T.snoc` '`') `shouldBeRight`
                    identifier' kwd

    describe "name" $ do
        let (parse', expectError) = helperFuncs P.name
        it "fails to parse an empty string" $
            expectError "" 1 1
        it "fails to parse if the name is not a valid identifier" $ do
            expectError "-ident" 1 1
            expectError "-ident-ifier" 1 1
            expectError "ident-" 1 7
            expectError "ident-ifier-" 1 13
            expectError "ident--ifier" 1 7
            expectError "ident__ifier" 1 7
            expectError "\xbb34\xd6a8\xd55c-\xc2dd\xbcc4\xc790" 1 1
            expectError "invalid-\xc2dd\xbcc4\xc790" 1 9
        it "fails to parse if the facial name is not a valid identifier" $ do
            expectError "-ident/valid" 1 1
            expectError "-ident-ifier/valid" 1 1
            expectError "ident-/valid" 1 7
            expectError "ident-ifier-/valid" 1 13
            expectError "ident--ifier/valid" 1 7
            expectError "ident__ifier/valid" 1 7
            expectError "\xbb34\xd6a8\xd55c-\xc2dd\xbcc4\xc790/valid" 1 1
            expectError "invalid-\xc2dd\xbcc4\xc790/valid" 1 9
        it "fails to parse if the behind name is not a valid identifier" $ do
            expectError "valid/-ident" 1 6
            expectError "valid/-ident-ifier" 1 6
            expectError "valid/ident-" 1 6
            expectError "valid/ident-ifier-" 1 6
            expectError "valid/ident--ifier" 1 6
            expectError "valid/ident__ifier" 1 6
            expectError "valid/\xbb34\xd6a8\xd55c-\xc2dd\xbcc4\xc790" 1 6
            expectError "valid/invalid-\xc2dd\xbcc4\xc790" 1 6
        it "emits Name if succeeded to parse" $ do
            parse' "name" `shouldBeRight` Name "name" "name"
            parse' "`enum`" `shouldBeRight` Name "enum" "enum"
            parse' "facial/behind" `shouldBeRight` Name "facial" "behind"
            parse' "facial / behind" `shouldBeRight` Name "facial" "behind"
            parse' "`enum`/`unboxed`" `shouldBeRight` Name "enum" "unboxed"
            parse' "`enum` / `unboxed`" `shouldBeRight` Name "enum" "unboxed"

    describe "annotation" $ do
        let (parse', expectError) = helperFuncs P.annotation
        context "with single argument" $ do
            let rightAnnotaiton = Annotation "name-abc" (Just "wo\"rld")
            it "success" $ do
                parse' "@name-abc(\"wo\\\"rld\")"
                    `shouldBeRight` rightAnnotaiton
                parse' "@name-abc( \"wo\\\"rld\")"
                    `shouldBeRight` rightAnnotaiton
                parse' "@name-abc(\"wo\\\"rld\" )"
                    `shouldBeRight` rightAnnotaiton
                parse' "@name-abc( \"wo\\\"rld\" )"
                    `shouldBeRight` rightAnnotaiton
                parse' "@ name-abc ( \"wo\\\"rld\")"
                    `shouldBeRight` rightAnnotaiton
                parse' "@name-abc ( \"wo\\\"rld\")"
                    `shouldBeRight` rightAnnotaiton
                parse' "@name-abc(\"wo\\\"rld\\n\")" `shouldBeRight`
                    Annotation "name-abc" (Just "wo\"rld\n")
            it "fails to parse if annotation name start with hyphen" $ do
                expectError "@-abc(\"helloworld\")" 1 2
                expectError "@-abc-d(\"helloworld\")" 1 2
            it "fails to parse without parentheses" $
                expectError "@foobar \"helloworld\"" 1 9
            it "fails to parse without double quotes" $
                expectError "@foobar(helloworld)" 1 9
        context "without arguments" $ do
            let rightAnnotaiton = Annotation "name-abc" Nothing
            it "success" $ do
                parse' "@name-abc" `shouldBeRight` rightAnnotaiton
                parse' "@name-abc " `shouldBeRight` rightAnnotaiton
                parse' "@ name-abc" `shouldBeRight` rightAnnotaiton
                parse' "@name-abc()" `shouldBeRight` rightAnnotaiton
                parse' "@name-abc ( )" `shouldBeRight` rightAnnotaiton
                parse' "@ name-abc ( )" `shouldBeRight` rightAnnotaiton
            it "fails to parse if annotation name start with hyphen" $ do
                expectError "@-abc" 1 2
                expectError "@-abc-d" 1 2

    describe "annotationSet" $ do
        let (parse', expectError) = helperFuncs P.annotationSet
            annotationSet = head $ rights [fromList [ Annotation "a" (Just "b")
                                                    , Annotation "c" Nothing
                                                    ]
                                          ]
        it "success" $ do
            parse' "@a(\"b\")@c" `shouldBeRight` annotationSet
            parse' "@a(\"b\") @c" `shouldBeRight` annotationSet
            parse' "@a(\"b\") @c() " `shouldBeRight` annotationSet
        it "fails to parse if has duplicated name" $
            expectError "@a(\"b\")@a" 1 10

    describe "typeIdentifier" $ do
        let (parse', expectError) = helperFuncs P.typeIdentifier
        it "fails to parse if the input is not a valid identifier" $
            expectError "-invalid-type-identifier" 1 1
        it "emits TypeIdentifier if succeeded to parse" $ do
            parse' "text" `shouldBeRight` TypeIdentifier "text"
            parse' "uuid" `shouldBeRight` TypeIdentifier "uuid"

    describe "optionModifier" $ do
        let (_, expectError') = helperFuncs P.optionModifier
        it "fails to parse if the input lacks a question mark" $
            expectError' "lacks-qmark" 1 12
        let parsers = [ P.optionModifier
                      , P.typeExpression
                      ] :: [Parser TypeExpression]
        forM_ parsers $ \ parser' -> do
            let (parse', expectError) = helperFuncs parser'
            it "cannot append two or more option modifiers" $ do
                expectError "text??" 1 6
                expectError "text???" 1 6
                expectError "text????" 1 6
            it "emits OptionModifier if succeeded to parse" $ do
                parse' "text?" `shouldBeRight`
                    OptionModifier (TypeIdentifier "text")
                parse' "uuid ?" `shouldBeRight`
                    OptionModifier (TypeIdentifier "uuid")
                parse' "{text}?" `shouldBeRight`
                    OptionModifier (SetModifier $ TypeIdentifier "text")
            it "can be appended to set modifier" $
                parse' "{text}?" `shouldBeRight`
                    OptionModifier (SetModifier $ TypeIdentifier "text")

    describe "setModifier" $ do
        let parsers = [ (1, P.setModifier)
                      , (29, P.typeExpression)
                      ] :: [(Int, Parser TypeExpression)]
        forM_ parsers $ \ (beginErrorPos, parser') -> do
            let (parse', expectError) = helperFuncs parser'
            it "fails to parse if input doesn't start with a curly bracket" $
                expectError "not-start-with-curly-bracket}" 1 beginErrorPos
            it "fails to parse if input doesn't end with a curly bracket" $
                expectError "{not-end-with-curly-bracket" 1 28
            it "emits SetModifier if succeeded to parse" $ do
                parse' "{text}" `shouldBeRight`
                    SetModifier (TypeIdentifier "text")
                parse' "{ uuid }" `shouldBeRight`
                    SetModifier (TypeIdentifier "uuid")
            it "can be nested to represent 2d set" $ do
                parse' "{{text}}" `shouldBeRight`
                    SetModifier (SetModifier $ TypeIdentifier "text")
                parse' "{[text]}" `shouldBeRight`
                    SetModifier (ListModifier $ TypeIdentifier "text")
            it "can consist of optional elements" $
                parse' "{uuid?}" `shouldBeRight`
                    SetModifier (OptionModifier $ TypeIdentifier "uuid")

    describe "listModifier" $ do
        let parsers = [ (1, P.listModifier)
                      , (30, P.typeExpression)
                      ] :: [(Int, Parser TypeExpression)]
        forM_ parsers $ \ (beginErrorPos, parser') -> do
            let (parse', expectError) = helperFuncs parser'
            it "fails to parse if input doesn't start with a square bracket" $
                expectError "not-start-with-square-bracket]" 1 beginErrorPos
            it "fails to parse if input doesn't end with a square bracket" $
                expectError "[not-end-with-square-bracket" 1 29
            it "emits ListModifier if succeeded to parse" $ do
                parse' "[text]" `shouldBeRight`
                    ListModifier (TypeIdentifier "text")
                parse' "[ uuid ]" `shouldBeRight`
                    ListModifier (TypeIdentifier "uuid")
            it "can be nested to represent 2d list" $ do
                parse' "[[text]]" `shouldBeRight`
                    ListModifier (ListModifier $ TypeIdentifier "text")
                parse' "[{text}]" `shouldBeRight`
                    ListModifier (SetModifier $ TypeIdentifier "text")
            it "can consist of optional elements" $
                parse' "[uuid?]" `shouldBeRight`
                    ListModifier (OptionModifier $ TypeIdentifier "uuid")

    describe "mapModifier" $ do
        let parsers = [ (1, P.mapModifier)
                      , (15, P.typeExpression)
                      ] :: [(Int, Parser TypeExpression)]
        forM_ parsers $ \ (beginErrorPos, parser') -> do
            let (parse', expectError) = helperFuncs parser'
            it "fails to parse if input doesn't start with a curly bracket" $
                expectError "not-start-with: curly-bracket}" 1 beginErrorPos
            it "fails to parse if input doesn't end with a curly bracket" $
                expectError "{not-end-with: curly-bracket" 1 29
            it "emits MapModifier if succeeded to parse" $ do
                parse' "{uuid: text}" `shouldBeRight`
                    MapModifier (TypeIdentifier "uuid") (TypeIdentifier "text")
                parse' "{ text : uuid }" `shouldBeRight`
                    MapModifier (TypeIdentifier "text") (TypeIdentifier "uuid")
            it "can be nested to represent 2d map" $ do
                parse' "{uuid: {uuid: text}}" `shouldBeRight`
                    MapModifier (TypeIdentifier "uuid")
                                (MapModifier (TypeIdentifier "uuid")
                                             (TypeIdentifier "text"))
                parse' "{uuid: [text]}" `shouldBeRight`
                    MapModifier (TypeIdentifier "uuid")
                                (ListModifier $ TypeIdentifier "text")

    describe "typeExpression" $ do
        let (_, expectError) = helperFuncs P.typeExpression
        it "cannot append two or more option modifiers" $ do
            expectError "text??" 1 6
            expectError "text???" 1 6
            expectError "text????" 1 6

    describe "docs" $ do
        let (parse', expectError) = helperFuncs P.docs
        it "emits Docs if succeeded to parse" $ do
            parse' "#docs" `shouldBeRight` Docs "docs\n"
            parse' "#docs\n#docs..." `shouldBeRight` Docs "docs\ndocs...\n"
        it "may ignore a leading space" $ do
            parse' "# docs" `shouldBeRight` Docs "docs\n"
            parse' "# docs\n# docs..." `shouldBeRight` Docs "docs\ndocs...\n"
            parse' "#  docs" `shouldBeRight` Docs " docs\n"
            parse' "#  docs\n#  docs..." `shouldBeRight`
                Docs " docs\n docs...\n"
        it "may be mixed with whitespaces" $ do
            parse' "# docs\n\n# docs..." `shouldBeRight` Docs "docs\ndocs...\n"
            parse' "# docs\n    # docs..." `shouldBeRight`
                Docs "docs\ndocs...\n"
        it "differs from comment" $
            expectError "// comment" 1 1

    let descTypeDecl label parser spec' =
            let parsers = [ (label, parser)
                          , (label ++ " (typeDescription)", P.typeDeclaration)
                          ] :: [(String, Parser TypeDeclaration)]
            in forM_ parsers $ \ (label', parser') ->
                describe label' $
                    spec' $ helperFuncs parser'

    describe "handleNameDuplication" $ do
        let cont dset = do
                _ <- string "a"
                return dset :: Parser (DeclarationSet SampleDecl)
        it "fails if there are any duplication on facial names" $ do
            let ds = [ "a"
                     , "b"
                     , SampleDecl (Name "b" "c") A.empty
                     ] :: [SampleDecl]
                p = P.handleNameDuplication "LABEL" ds cont
                (parse', expectError) = helperFuncs p
            expectError "a" 1 1
            let (Left e) = parse' "a"
            parseErrorPretty e `shouldBe`
                "1:1:\nthe facial LABEL name `b` is duplicated\n"
        it "fails if there are any duplication on behind names" $ do
            let ds = [ "a"
                     , "b"
                     , SampleDecl (Name "c" "b") A.empty
                     ] :: [SampleDecl]
                p = P.handleNameDuplication "LABEL" ds cont
                (parse', expectError) = helperFuncs p
            expectError "a" 1 1
            let (Left e) = parse' "a"
            parseErrorPretty e `shouldBe`
                "1:1:\nthe behind LABEL name `b` is duplicated\n"
        it "continues using the given DeclarationSet if there are no dups" $ do
            let ds = ["a", "b", "c"] :: [SampleDecl]
                p = P.handleNameDuplication "LABEL" ds cont
                (parse', _) = helperFuncs p
            parse' "a" `shouldBeRight`
                (["a", "b", "c"] :: DeclarationSet SampleDecl)

    descTypeDecl "aliasTypeDeclaration" P.aliasTypeDeclaration $ \ helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (Alias ...)) if succeeded to parse" $ do
            parse' "type path = text;" `shouldBeRight`
                TypeDeclaration "path" (Alias "text") empty
            parse' "type path = text;\n# docs" `shouldBeRight`
                TypeDeclaration "path" (Alias "text")
                                (singleDocs "docs\n")
            parse' "type path = text;\n# docs\n# docs..." `shouldBeRight`
                TypeDeclaration "path" (Alias "text")
                                (singleDocs "docs\ndocs...\n")
            parse' "@foo ( \"bar\" ) type path = text;\n# docs\n# docs..."
                `shouldBeRight`
                TypeDeclaration "path" (Alias "text")
                                (A.union (singleDocs "docs\ndocs...\n")
                                         fooAnnotationSet)
            parse' "@baz  type path = text;\n# docs\n# docs..."
                `shouldBeRight`
                TypeDeclaration "path" (Alias "text")
                                (A.union (singleDocs "docs\ndocs...\n")
                                         bazAnnotationSet)
        specify ("its name can't have behind name since " ++
                 "its canonical type's behind name would be used instead") $
            expectError "type path/error = text;" 1 10
        it "fails to parse if trailing semicolon is missing" $ do
            let (_, expectErr) = helperFuncs P.module'
            expectErr "type a = text;\ntype b = text\ntype c = text;" 3 1
            expectErr "unboxed a (text);\ntype b = text\nunboxed c (text);" 3 1
            expectErr "type a = text;\nunboxed b (text)\ntype c = text;" 3 1

    descTypeDecl "unboxedTypeDeclaration"
                 P.unboxedTypeDeclaration $ \ funs -> do
        let (parse', expectError) = funs
        it "emits (TypeDeclaration (UnboxedType ..)) if succeeded to parse" $ do
            parse' "unboxed offset (float64);" `shouldBeRight`
                TypeDeclaration "offset" (UnboxedType "float64") empty
            parse' "unboxed offset (float64);\n# docs" `shouldBeRight`
                TypeDeclaration "offset" (UnboxedType "float64")
                                (singleDocs "docs\n")
            parse' "unboxed offset (float64);\n# docs\n# docs..."
                `shouldBeRight`
                    TypeDeclaration "offset" (UnboxedType "float64")
                                    (singleDocs "docs\ndocs...\n")
            parse' "@foo(\"bar\")\nunboxed offset (float64);\n# docs\n# docs..."
                `shouldBeRight`
                    TypeDeclaration "offset" (UnboxedType "float64")
                                    (A.union (singleDocs "docs\ndocs...\n")
                                             fooAnnotationSet)
            parse' "@baz\nunboxed offset (float64);\n# docs\n# docs..."
                `shouldBeRight`
                    TypeDeclaration "offset" (UnboxedType "float64")
                                    (A.union (singleDocs "docs\ndocs...\n")
                                             bazAnnotationSet)
            expectError "unboxed offset/behind (float64);" 1 15
        it "fails to parse if trailing semicolon is missing" $ do
            let (_, expectErr) = helperFuncs P.module'
            expectErr "unboxed a (text);\nunboxed b (text)\nunboxed c (text);"
                      3 1
            expectErr "type a = text;\nunboxed b (text)\ntype c = text;" 3 1
            expectErr "unboxed a (text);\ntype b = text\nunboxed c (text);" 3 1

    descTypeDecl "enumTypeDeclaration" P.enumTypeDeclaration $ \ helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (EnumType ...)) if succeeded to parse" $ do
            let members' = [ "male"
                           , "female"
                           , "unknown"
                           ] :: DeclarationSet EnumMember
                membersWithDocs = [ EnumMember "male" (singleDocs "docs\n")
                                  , "female"
                                  , EnumMember "unknown" (singleDocs "docs2\n")
                                  ] :: DeclarationSet EnumMember
                membersWithAnnots = [ EnumMember "male" fooAnnotationSet
                                    , "female"
                                    , "unknown"
                                    ] :: DeclarationSet EnumMember
                expected = TypeDeclaration "gender" (EnumType members') empty
            parse' "enum gender = male | female | unknown;"
                `shouldBeRight` expected
            parse' "enum gender=male|female|unknown;" `shouldBeRight` expected
            -- forward docs of enum type
            parse' "enum gender\n# gender type\n= male | female | unknown;"
                `shouldBeRight`
                    expected { typeAnnotations = singleDocs "gender type\n" }
            -- backward docs of enum type
            parse' "enum gender =\n# gender type\nmale | female | unknown;"
                `shouldBeRight`
                    expected { typeAnnotations = singleDocs "gender type\n" }
            parse' "enum gender = male # docs\n| female | unknown # docs2\n;"
                `shouldBeRight` TypeDeclaration "gender"
                                                (EnumType membersWithDocs)
                                                empty
            parse' "@foo (\"bar\")\nenum gender=male|female|unknown;"
                `shouldBeRight`
                    TypeDeclaration "gender" (EnumType members')
                                    fooAnnotationSet
            parse' "@baz\nenum gender=male|female|unknown;"
                `shouldBeRight`
                    TypeDeclaration "gender" (EnumType members')
                                    bazAnnotationSet
            parse' "@baz\nenum gender=\n@foo (\"bar\")\nmale|female|unknown;"
                `shouldBeRight`
                    TypeDeclaration "gender" (EnumType membersWithAnnots)
                                    bazAnnotationSet
        it "fails to parse if there are duplicated facial names" $
            expectError [s|
enum dup = a/b
         | b/c
         | a/d
         ;|] 4 10
        it "fails to parse if there are duplicated behind names" $
            expectError [s|
enum dup = a/b
         | b/c
         | c/b
         ;|] 4 10
        it "fails to parse if trailing semicolon is missing" $ do
            let (_, expectErr) = helperFuncs P.module'
            expectErr "enum a = x | y;\nenum b = x | y\nenum c = x | y;" 3 1
            expectErr "unboxed a (text);\nenum b = x | y\nunboxed c (text);" 3 1
            expectErr "enum a = x | y;\nunboxed b (text)\nenum c = x | y;" 3 1

    descTypeDecl "recordTypeDeclaration"
                 P.recordTypeDeclaration $ \ helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (RecordType ...)) if succeeded to parse" $ do
            let nameF = Field "name" "text" empty
                dobF = Field "dob" "date" (singleDocs "date of birth")
                genderF = Field "gender" "gender" empty
                fields' = [nameF, dobF, genderF] :: DeclarationSet Field
                record = RecordType fields'
                a = TypeDeclaration "person" record empty
                b = a { typeAnnotations = singleDocs "person record type" }
            -- without docs, last field with trailing comma
            parse' [s|
record person (
    text name,
    date dob,
    # date of birth
    gender gender,
);|] `shouldBeRight` a
            -- without docs, last field without trailing comma
            parse' [s|
record person (
    text name,
    date dob,
    # date of birth
    gender gender
);|] `shouldBeRight` a
            -- with docs, last field with trailing comma
            parse' [s|
record person (
    # person record type

    text name,
    date dob,
    # date of birth
    gender gender,
);|] `shouldBeRight` b
            -- with docs, last field without trailing comma
            parse' [s|
record person (
    # person record type

    text name,
    date dob,
    # date of birth
    gender gender
);|] `shouldBeRight` b
            -- without docs, last field with trailing comma,
            -- with annotation with single argument
            parse' [s|
@foo("bar")
record person (
    text name,
    date dob,
    # date of birth
    gender gender,
);|] `shouldBeRight`
                TypeDeclaration "person" record fooAnnotationSet
            -- without docs, last field with trailing comma,
            -- with annotation without arguments
            parse' [s|
@baz
record person (
    text name,
    date dob,
    # date of birth
    gender gender,
);|] `shouldBeRight`
                TypeDeclaration "person" record bazAnnotationSet
            -- with docs, last field with trailing comma,
            -- and annotation without arguments
            parse' [s|
@baz
record person (
    # person record type

    text name,
    date dob,
    # date of birth
    gender gender,
);|] `shouldBeRight`
                    TypeDeclaration "person" record
                        (union bazAnnotationSet $
                                singleDocs "person record type")
            -- without docs, last field with trailing comma,
            -- and annotations on fields
            parse' [s|
record person (
    text name,
    @foo ("bar")
    date dob,
    # date of birth
    @baz
    gender gender,
);|] `shouldBeRight`
                    TypeDeclaration "person" (RecordType
                        [ nameF
                        , dobF { fieldAnnotations = union fooAnnotationSet $
                                     singleDocs "date of birth"
                               }
                        , genderF { fieldAnnotations = bazAnnotationSet }
                        ]) empty
        it "should have one or more fields" $ do
            expectError "record unit ();" 1 14
            expectError "record unit (\n# docs\n);" 3 1
        it "fails to parse if there are duplicated facial names" $
            expectError [s|
record dup (
    text a/b,
    text b/c,
    text a/d,
);|] 5 1
        it "fails to parse if there are duplicated behind names" $
            expectError [s|
record dup (
    text a/b,
    text b/c,
    text c/b,
);|] 5 1
        it "fails to parse if there's no space between field type and name" $ do
            expectError "record a (texta);" 1 16
            expectError "record a (textb\n#docs\n);" 2 1
        it "fails to parse if trailing semicolon is missing" $ do
            let (_, expectErr) = helperFuncs P.module'
            expectErr
                "record a (text x);\nrecord b (text y)\nrecord c (text z);"
                3 1
            expectErr "type a = text;\nrecord b (text x)\ntype c = text;" 3 1
            expectErr "record a (text x);\ntype b = text\nrecord c (text y);"
                      3 1

    descTypeDecl "unionTypeDeclaration" P.unionTypeDeclaration $ \ helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (UnionType ...)) if succeeded to parse" $ do
            let cOriginF = Field "origin" "point" empty
                cRadiusF = Field "radius" "offset" empty
                circleFields = [cOriginF, cRadiusF]
                rUpperLeftF = Field "upper-left" "point" empty
                rLowerRightF = Field "lower-right" "point" empty
                rectangleFields = [rUpperLeftF, rLowerRightF]
                circleTag = Tag "circle" circleFields empty
                rectTag = Tag "rectangle" rectangleFields empty
                noneTag = Tag "none" [] empty
                tags' = [circleTag, rectTag, noneTag]
                union' = UnionType tags'
                a = TypeDeclaration "shape" union' empty
                b = a { typeAnnotations = singleDocs "shape type" }
            parse' [s|
union shape
    = circle (point origin, offset radius,)
    | rectangle (point upper-left, point lower-right,)
    | none
    ;|] `shouldBeRight` a
            parse' [s|
union shape
    = circle (
        point origin,
        offset radius,
      )
    | rectangle (
        point upper-left,
        point lower-right,
      )
    | none
    ;|] `shouldBeRight` a
            parse' [s|
union shape
    # shape type
    = circle (point origin, offset radius,)
    | rectangle (point upper-left, point lower-right,)
    | none
    ;|] `shouldBeRight` b
            parse' [s|
@docs ("shape type\n")
union shape
    = circle (point origin, offset radius,)
    | rectangle (point upper-left, point lower-right,)
    | none
    ;|] `shouldBeRight` b
            parse' [s|
union shape
    = circle (point origin, offset radius,)
    | rectangle (point upper-left, point lower-right,)
    | @foo ("bar") none
    ;|] `shouldBeRight`
                    a { type' = union' { tags = [ circleTag
                                                , rectTag
                                                , Tag "none" [] fooAnnotationSet
                                                ]
                                       }
                      }
            parse' [s|
union shape
    = circle (point origin, offset radius,)
    # tag docs
    | rectangle (
          # front docs
          point upper-left, point lower-right,
      )
    | none
    ;|] `shouldBeRight`
                    a { type' = union'
                            { tags = [ circleTag
                                        { tagAnnotations = singleDocs "tag docs"
                                        }
                                     , rectTag
                                        { tagAnnotations =
                                              singleDocs "front docs"
                                        }
                                     , noneTag
                                     ]
                            }
                      }
            parse' [s|
union shape
    = circle (point origin, offset radius,)
    | rectangle (point upper-left, point lower-right,)
    | none  # tag docs
    ;|] `shouldBeRight`
                    a { type' = union'
                            { tags = [ circleTag, rectTag
                                     , noneTag
                                        { tagAnnotations = singleDocs "tag docs"
                                        }
                                     ]
                            }
                      }
            parse' [s|
union shape
    = circle (point origin, @baz offset radius,)
    | rectangle (point upper-left, @foo ("bar") point lower-right,)
    | none
    ;|] `shouldBeRight`
                    a { type' = union'
                            { tags = [ circleTag
                                           { tagFields =
                                                 [ cOriginF
                                                 , cRadiusF { fieldAnnotations =
                                                              bazAnnotationSet }
                                                 ]
                                           }
                                     , rectTag
                                           { tagFields =
                                                 [ rUpperLeftF
                                                 , rLowerRightF
                                                       { fieldAnnotations =
                                                         fooAnnotationSet }
                                                 ]
                                           }
                                     , noneTag
                                     ]
                             }
                      }
        it "fails to parse if there are duplicated facial names" $ do
            expectError [s|
union dup
    = a/b
    | b/c
    | a/d
    ;|] 5 6
            expectError [s|
union dup
    = a (text a/b, text b/c, text a/d)
    | b
    ;|] 2 38
        it "fails to parse if there are duplicated behind names" $ do
            expectError [s|
union dup
    = a/b
    | b/c
    | c/b
    ;|] 5 6
            expectError [s|
union dup
    = a (text a/b, text b/c, text c/b)
    | b
    ;|] 2 38
        it "fails to parse if trailing semicolon is missing" $ do
            let (_, expectErr) = helperFuncs P.module'
            expectErr "union a = x | y;\nunion b = x | y\nunion c = x | y;" 3 1
            expectErr "unboxed a (text);\nunion b = x | y\nunboxed c (text);"
                      3 1
            expectErr "union a = x | y;\nunboxed b (text)\nunion c = x | y;" 3 1

    describe "method" $ do
        let (parse', expectError) = helperFuncs P.method
            httpGetAnnotation = head $
                rights [fromList [Annotation "http-get" (Just "/get-name/")]]
        it "emits Method if succeeded to parse" $ do
            parse' "text get-name()" `shouldBeRight`
                Method "get-name" [] "text" Nothing empty
            parse' "text get-name (person user)" `shouldBeRight`
                Method "get-name" [Parameter "user" "person" empty]
                       "text" Nothing empty
            parse' "text get-name  ( person user,text default )" `shouldBeRight`
                Method "get-name"
                       [ Parameter "user" "person" empty
                       , Parameter "default" "text" empty
                       ]
                       "text" Nothing empty
            parse' ("@http-get(\"/get-name/\") text get-name  " `T.append`
                    "( person user,text default )") `shouldBeRight`
                Method "get-name"
                       [ Parameter "user" "person" empty
                       , Parameter "default" "text" empty
                       ]
                       "text" Nothing httpGetAnnotation
            parse' "text get-name() throws name-error" `shouldBeRight`
                Method "get-name" [] "text" (Just "name-error") empty
            parse' [s|
text get-name  ( person user,text default )
               throws get-name-error|] `shouldBeRight`
                Method "get-name"
                       [ Parameter "user" "person" empty
                       , Parameter "default" "text" empty
                       ]
                       "text" (Just "get-name-error") empty
            parse' [s|
@http-get("/get-name/")
text get-name  ( person user,text default )
               throws get-name-error|] `shouldBeRight`
                Method "get-name"
                       [ Parameter "user" "person" empty
                       , Parameter "default" "text" empty
                       ]
                       "text" (Just "get-name-error")
                       httpGetAnnotation
        it "can have docs" $ do
            parse' [s|
text get-name (
  # Gets the name.
)|] `shouldBeRight`
                Method "get-name" [] "text"
                       Nothing (singleDocs "Gets the name.")
            parse' [s|
text get-name (
  # Gets the name.
)throws name-error  |] `shouldBeRight`
                Method "get-name" [] "text"
                       (Just "name-error")
                       (singleDocs "Gets the name.")
            parse' [s|
text get-name (
  # Gets the name of the user.
  person user,
)|] `shouldBeRight`
                Method "get-name"
                       [Parameter "user" "person" empty]
                       "text"
                       Nothing
                       (singleDocs "Gets the name of the user.")
            parse' [s|
text get-name (
  # Gets the name of the user.
  person user,
) throws get-name-error|] `shouldBeRight`
                Method "get-name"
                       [Parameter "user" "person" empty]
                       "text"
                       (Just "get-name-error")
                       (singleDocs "Gets the name of the user.")
            let expectedUserPDocs = "The person to find their name."
                expectedDefaultPDocs =
                    "The default name used when the user has no name."
                expectedMethod =
                    Method "get-name"
                           [ Parameter "user" "person" $
                                       singleDocs expectedUserPDocs
                           , Parameter "default" "text" $
                                       singleDocs expectedDefaultPDocs
                           ]
                           "text"
                           Nothing
                           (singleDocs "Gets the name of the user.")
            parse' [s|
text get-name (
  # Gets the name of the user.
  person user,
  # The person to find their name.
  text default
  # The default name used when the user has no name.
)|] `shouldBeRight` expectedMethod
        it "fails to parse if there are parameters of the same facial name" $ do
            expectError "bool pred(text a, text a/b)" 1 11
            expectError "bool pred(text a/b, text a)" 1 11
            expectError "bool pred(text c/a, text c/b)" 1 11
        it "fails to parse if there are parameters of the same behind name" $ do
            expectError "bool pred(text a, text b/a)" 1 11
            expectError "bool pred(text a/b, text b)" 1 11
            expectError "bool pred(text a/c, text b/c)" 1 11

    describe "serviceDeclaration" $ do
        let (parse', expectError) = helperFuncs P.serviceDeclaration
            getUserD = singleDocs "Gets an user by its id."
            createUserD = singleDocs "Creates a new user"
            noMethodsD = singleDocs "Service having no methods."
            oneMethodD = singleDocs "Service having only one method."
            multiMethodsD = singleDocs "Service having multiple methods."
            userIdD = singleDocs "The unique user identifier."
        it "emits ServiceDeclaration if succeeded to parse" $ do
            parse' "service null-service();" `shouldBeRight`
                ServiceDeclaration "null-service" (Service []) empty
            parse' [s|
service null-service (
  # Service having no methods.
);|] `shouldBeRight`
                ServiceDeclaration "null-service" (Service []) noMethodsD
            parse' [s|
service one-method-service(
  user get-user(uuid user-id)
);|] `shouldBeRight`
                ServiceDeclaration
                    "one-method-service"
                    (Service [ Method "get-user"
                                      [Parameter "user-id" "uuid" empty]
                                      "user"
                                      Nothing
                                      empty
                             ])
                    empty
            parse' [s|
service one-method-service (
  # Service having only one method.
  user get-user (
    # Gets an user by its id.
    uuid user-id
  ) throws get-user-error,
);|] `shouldBeRight`
                ServiceDeclaration
                    "one-method-service"
                    (Service [ Method "get-user"
                                      [Parameter "user-id" "uuid" empty]
                                      "user"
                                      (Just "get-user-error")
                                      getUserD
                             ])
                    oneMethodD
            parse' [s|
service user-service (
  # Service having multiple methods.
  user create-user (
    # Creates a new user
    user user
  ),
  user get-user (
    # Gets an user by its id.
    uuid user-id
  ),
);|] `shouldBeRight`
                ServiceDeclaration
                    "user-service"
                    (Service [ Method "create-user"
                                      [Parameter "user" "user" empty]
                                      "user"
                                      Nothing
                                      createUserD
                             , Method "get-user"
                                      [Parameter "user-id" "uuid" empty]
                                      "user"
                                      Nothing
                                      getUserD
                             ])
                    multiMethodsD
            parse' [s|
@foo("bar")
service null-service (
  # Service having no methods.
);|] `shouldBeRight`
                ServiceDeclaration "null-service"
                                   (Service [])
                                   (A.union noMethodsD fooAnnotationSet)
            parse' [s|
@baz
service null-service (
  # Service having no methods.
);|] `shouldBeRight`
                ServiceDeclaration "null-service"
                                   (Service [])
                                   (A.union noMethodsD bazAnnotationSet)
            parse' [s|
service user-service (
  @docs ("Creates a new user\n")
  user create-user (
    user user
  ),
  @docs ("Gets an user by its id.\n")
  user get-user (
    uuid user-id
  ),
);|] `shouldBeRight`
                ServiceDeclaration
                    "user-service"
                    (Service [ Method "create-user"
                                      [Parameter "user" "user" empty]
                                      "user"
                                      Nothing
                                      createUserD
                             , Method "get-user"
                                      [Parameter "user-id" "uuid" empty]
                                      "user"
                                      Nothing
                                      getUserD
                             ])
                    empty
            parse' [s|
service user-service (
  user get-user (
    @baz
    uuid user-id
    # The unique user identifier.
  ),
);|] `shouldBeRight`
                ServiceDeclaration
                    "user-service"
                    (Service [ Method "get-user"
                                      [ Parameter "user-id" "uuid" $
                                            A.union bazAnnotationSet userIdD
                                      ]
                                      "user"
                                      Nothing
                                      empty
                             ])
                    empty
        it "fails to parse if there are methods of the same facial name" $ do
            expectError [s|
service method-dups (
  bool same-name ()
  text same-name (uuid id)
);|] 3 3
            expectError [s|
service method-dups (
  bool same-name ()
  text same-name/different-behind-name (uuid id)
);|] 3 3
            expectError [s|
service method-dups (
  bool same-name/unique-behind-name ()
  text same-name/different-behind-name (uuid id)
);|] 3 3
        it "fails to parse if there are methods of the same behind name" $ do
            expectError [s|
service method-dups (
  bool same-name ()
  text same-name (uuid id)
);|] 3 3
            expectError [s|
service method-dups (
  bool same-name ()
  text unique-name/same-name (uuid id)
);|] 3 3
            expectError [s|
service method-dups (
  bool unique-name/same-name ()
  text different-facial-name/same-name (uuid id)
);|] 3 3
        it "fails to parse if trailing semicolon is missing" $ do
            let (_, expectErr) = helperFuncs P.module'
            expectErr "service a ();\nservice b ()\nservice c ();" 3 1
            expectErr "type a = text;\nservice b ()\ntype c = text;" 3 1
            expectErr "service a ();\ntype b = text\nservice c ();" 3 1
    let moduleParsers = [ ("module'", P.module')
                        , ("file", P.file)
                        ] :: [(String, Parser Module)]
    forM_ moduleParsers $ \ (label, parser') ->
        describe label $ do
            let (parse', expectError) = helperFuncs parser'
            it "emits Module if succeeded to parse" $ do
                let decls = [ TypeDeclaration "path" (Alias "text") empty
                            , TypeDeclaration "offset"
                                              (UnboxedType "float64") empty
                            ]
                parse' "type path = text; unboxed offset (float64);"
                    `shouldBeRight` Module decls Nothing
                parse'
                    "#docs\n#...\ntype path = text; unboxed offset (float64);"
                        `shouldBeRight` Module decls (Just "docs\n...")
            it "may have no type declarations" $ do
                parse' "" `shouldBeRight` Module [] Nothing
                parse' "# docs" `shouldBeRight` Module [] (Just "docs")
            it "errors if there are any duplicated facial names" $
                expectError "type a = text;\ntype a/b = text;" 2 7
            it "errors if there are any duplicated behind names" $
                expectError "type b = text;\ntype a/b = text;" 2 7

    describe "modulePath" $ do
        let (parse', expectError) = helperFuncs P.modulePath
        it "emits ModulePath if succeeded to parse" $ do
            parse' "foo" `shouldBeRight` ["foo"]
            parse' "foo.bar" `shouldBeRight` ["foo", "bar"]
            parse' "foo.bar.baz" `shouldBeRight` ["foo", "bar", "baz"]
        it "errors if it's empty" $
            expectError "" 1 1
        it "errors if it starts with period" $ do
            expectError "." 1 1
            expectError ".foo" 1 1
            expectError ".foo.bar" 1 1
            expectError ".foo.bar.baz" 1 1
        it "errors if it ends with period" $ do
            expectError "." 1 1
            expectError "foo." 1 5
            expectError "foo.bar." 1 9
            expectError "foo.bar.baz." 1 13

    describe "imports" $ do
        let (parse', expectError) = helperFuncs P.imports
        it "emits Import values if succeeded to parse" $
            parse' "import foo.bar (a, b);" `shouldBeRight`
                [ Import ["foo", "bar"] "a" empty
                , Import ["foo", "bar"] "b" empty
                ]
        it "can be annotated" $ do
            parse' "import foo.bar (@foo (\"bar\") a, @baz b);" `shouldBeRight`
                [ Import ["foo", "bar"] "a" fooAnnotationSet
                , Import ["foo", "bar"] "b" bazAnnotationSet
                ]
            parse' "import foo.bar (@foo (\"bar\") @baz a, b);" `shouldBeRight`
                [ Import ["foo", "bar"] "a" $
                         union fooAnnotationSet bazAnnotationSet
                , Import ["foo", "bar"] "b" empty
                ]
        it "errors if parentheses have nothing" $
            expectError "import foo.bar ();" 1 17

    specify "parse & parseFile" $ do
        files <- getDirectoryContents "examples"
        let examples = map ("examples/" ++) $ filter (isSuffixOf ".nrm") files
        forM_ examples $ \ filePath -> do
            sourceCode <- B.readFile filePath
            let parseResult = P.parse (filePath ++ " (text)")
                                      (E.decodeUtf8 sourceCode)
            parseResult `shouldSatisfy` isRight
            let Right module' = parseResult
            P.parse (filePath ++ " (text inverse)") (toCode module')
                `shouldBeRight` module'
            parseFileResult <- P.parseFile filePath
            parseFileResult `shouldSatisfy` isRight
            parseFileResult `shouldBe` parseResult

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
