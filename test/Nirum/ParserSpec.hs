{-# LANGUAGE OverloadedLists, OverloadedStrings, TypeFamilies #-}
module Nirum.ParserSpec where

import Control.Monad (forM_)
import Data.Either (isLeft, isRight, lefts)
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import Prelude hiding (readFile)
import System.Directory (getDirectoryContents)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Test.Hspec.Meta
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Error (errorPos, parseErrorPretty)
import Text.Megaparsec.Pos (Pos, SourcePos(sourceColumn, sourceLine), mkPos)
import Text.Megaparsec.Text (Parser)

import qualified Nirum.Parser as P
import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Declaration (Docs(Docs))
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.DeclarationSetSpec (SampleDecl(..))
import Nirum.Constructs.Identifier (fromText)
import Nirum.Constructs.Module (Module(Module))
import Nirum.Constructs.Name (Name(..))
import Nirum.Constructs.Service ( Method (Method)
                                , Parameter (Parameter)
                                , Service (Service)
                                )
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Field(Field)
                                        , Tag(Tag)
                                        , Type(..)
                                        , TypeDeclaration(..)
                                        )
import Nirum.Constructs.TypeExpression (TypeExpression(..))

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
        erroredPos parseResult `shouldBe` (line', col')

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
            expectError "무효한-식별자" 1 1
            expectError "invalid-식별자" 1 9
        let keywords = [ "boxed", "enum", "record", "type", "union"
                       , "BOXED", "Enum", "rEcord", "tyPE", "unioN"
                       ] :: [T.Text]
        it "fails to parse bare identifier if it's a reserved keyword" $
            forM_ keywords $ \kwd ->
                expectError kwd 1 1
        let identifier' = fromJust . fromText
        it "emits Identifier if succeeded to parse" $ do
            parse' "identifier" `shouldBeRight` identifier' "identifier"
            parse' "valid-identifier" `shouldBeRight`
                identifier' "valid-identifier"
            parse' "valid_identifier" `shouldBeRight`
                identifier' "valid_identifier"
        it "can parse reserved keywords iff they are quoted" $
            forM_ keywords $ \kwd ->
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
            expectError "무효한-식별자" 1 1
            expectError "invalid-식별자" 1 9
        it "fails to parse if the facial name is not a valid identifier" $ do
            expectError "-ident/valid" 1 1
            expectError "-ident-ifier/valid" 1 1
            expectError "ident-/valid" 1 7
            expectError "ident-ifier-/valid" 1 13
            expectError "ident--ifier/valid" 1 7
            expectError "ident__ifier/valid" 1 7
            expectError "무효한-식별자/valid" 1 1
            expectError "invalid-식별자/valid" 1 9
        it "fails to parse if the behind name is not a valid identifier" $ do
            expectError "valid/-ident" 1 6
            expectError "valid/-ident-ifier" 1 6
            expectError "valid/ident-" 1 6
            expectError "valid/ident-ifier-" 1 6
            expectError "valid/ident--ifier" 1 6
            expectError "valid/ident__ifier" 1 6
            expectError "valid/무효한-식별자" 1 6
            expectError "valid/invalid-식별자" 1 6
        it "emits Name if succeeded to parse" $ do
            parse' "name" `shouldBeRight` Name "name" "name"
            parse' "`enum`" `shouldBeRight` Name "enum" "enum"
            parse' "facial/behind" `shouldBeRight` Name "facial" "behind"
            parse' "facial / behind" `shouldBeRight` Name "facial" "behind"
            parse' "`enum`/`boxed`" `shouldBeRight` Name "enum" "boxed"
            parse' "`enum` / `boxed`" `shouldBeRight` Name "enum" "boxed"

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
        forM_ parsers $ \parser' -> do
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
        forM_ parsers $ \(beginErrorPos, parser') -> do
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
        forM_ parsers $ \(beginErrorPos, parser') -> do
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
        forM_ parsers $ \(beginErrorPos, parser') -> do
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
            in forM_ parsers $ \(label', parser') ->
                describe label' $
                    spec' $ helperFuncs parser'

    describe "handleNameDuplication" $ do
        let cont dset = do _ <- string "a"
                           return dset :: Parser (DeclarationSet SampleDecl)
        it "fails if there are any duplication on facial names" $ do
            let ds = [ "a"
                     , "b"
                     , SampleDecl (Name "b" "c") Nothing
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
                     , SampleDecl (Name "c" "b") Nothing
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

    descTypeDecl "aliasTypeDeclaration" P.aliasTypeDeclaration $ \helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (Alias ...)) if succeeded to parse" $ do
            parse' "type path = text;" `shouldBeRight`
                TypeDeclaration "path" (Alias "text") Nothing
            parse' "type path = text;\n# docs" `shouldBeRight`
                TypeDeclaration "path" (Alias "text") (Just $ Docs "docs\n")
            parse' "type path = text;\n# docs\n# docs..." `shouldBeRight`
                TypeDeclaration "path" (Alias "text")
                                (Just $ Docs "docs\ndocs...\n")
        specify "its name can't have behind name since \
                \its canonical type's behind name would be used instead" $
            expectError "type path/error = text;" 1 10

    descTypeDecl "boxedTypeDeclaration" P.boxedTypeDeclaration $ \helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (BoxedType ...)) if succeeded to parse" $ do
            parse' "boxed offset (float64);" `shouldBeRight`
                TypeDeclaration "offset" (BoxedType "float64") Nothing
            parse' "boxed offset (float64);\n# docs" `shouldBeRight`
                TypeDeclaration "offset" (BoxedType "float64")
                                (Just $ Docs "docs\n")
            parse' "boxed offset (float64);\n# docs\n# docs..." `shouldBeRight`
                TypeDeclaration "offset" (BoxedType "float64")
                                (Just $ Docs "docs\ndocs...\n")
        it "cannot have behind name" $
            expectError "boxed offset/behind (float64);" 1 13

    descTypeDecl "enumTypeDeclaration" P.enumTypeDeclaration $ \helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (EnumType ...)) if succeeded to parse" $ do
            let members' = [ "male"
                           , "female"
                           , "unknown"
                           ] :: DeclarationSet EnumMember
                membersWithDocs = [ EnumMember "male" (Just "docs\n")
                                  , "female"
                                  , EnumMember "unknown" (Just "docs2\n")
                                  ] :: DeclarationSet EnumMember
                expected = TypeDeclaration "gender" (EnumType members') Nothing
            parse' "enum gender = male | female | unknown;"
                `shouldBeRight` expected
            parse' "enum gender=male|female|unknown;" `shouldBeRight` expected
            -- forward docs of enum type
            parse' "enum gender\n# gender type\n= male | female | unknown;"
                `shouldBeRight` expected { typeDocs = Just "gender type\n" }
            -- backward docs of enum type
            parse' "enum gender =\n# gender type\nmale | female | unknown;"
                `shouldBeRight` expected { typeDocs = Just "gender type\n" }
            parse' "enum gender = male # docs\n| female | unknown # docs2\n;"
                `shouldBeRight` TypeDeclaration "gender"
                                                (EnumType membersWithDocs)
                                                Nothing
        it "fails to parse if there are duplicated facial names" $
            expectError "enum dup = a/b\n\
                        \         | b/c\n\
                        \         | a/d\n\
                        \         ;" 4 10
        it "fails to parse if there are duplicated behind names" $
            expectError "enum dup = a/b\n\
                        \         | b/c\n\
                        \         | c/b\n\
                        \         ;" 4 10

    descTypeDecl "recordTypeDeclaration" P.recordTypeDeclaration $ \helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (RecordType ...)) if succeeded to parse" $ do
            let fields' = [ Field "name" "text" Nothing
                          , Field "dob" "date" $ Just "date of birth"
                          , Field "gender" "gender" Nothing
                          ] :: DeclarationSet Field
                record = RecordType fields'
                a = TypeDeclaration "person" record Nothing
                b = a { typeDocs = Just "person record type" }
            -- without docs, last field with trailing comma
            parse' "record person (\n\
                   \    text name,\n\
                   \    date dob,\n\
                   \    # date of birth\n\
                   \    gender gender,\n\
                   \);" `shouldBeRight` a
            -- without docs, last field without trailing comma
            parse' "record person (\n\
                   \    text name,\n\
                   \    date dob,\n\
                   \    # date of birth\n\
                   \    gender gender\n\
                   \);" `shouldBeRight` a
            -- with docs, last field with trailing comma
            parse' "record person (\n\
                   \    # person record type\n\n\
                   \    text name,\n\
                   \    date dob,\n\
                   \    # date of birth\n\
                   \    gender gender,\n\
                   \);" `shouldBeRight` b
            -- with docs, last field without trailing comma
            parse' "record person (\n\
                   \    # person record type\n\n\
                   \    text name,\n\
                   \    date dob,\n\
                   \    # date of birth\n\
                   \    gender gender\n\
                   \);" `shouldBeRight` b
        it "should have one or more fields" $ do
            expectError "record unit ();" 1 14
            expectError "record unit (\n# docs\n);" 3 1
        it "fails to parse if there are duplicated facial names" $
            expectError "record dup (\n\
                        \    text a/b,\n\
                        \    text b/c,\n\
                        \    text a/d,\n\
                        \);" 5 1
        it "fails to parse if there are duplicated behind names" $
            expectError "record dup (\n\
                        \    text a/b,\n\
                        \    text b/c,\n\
                        \    text c/b,\n\
                        \);" 5 1
        it "fails to parse if there's no space between field type and name" $ do
            expectError "record a (typename);" 1 11
            expectError "record a (typename\n#docs\n);" 1 11

    descTypeDecl "unionTypeDeclaration" P.unionTypeDeclaration $ \helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (UnionType ...)) if succeeded to parse" $ do
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
                a = TypeDeclaration "shape" union Nothing
                b = a { typeDocs = Just "shape type" }
            parse' "union shape\n\
                   \    = circle (point origin, \
                                 \offset radius,)\n\
                   \    | rectangle (point upper-left, \
                                    \point lower-right,)\n\
                   \    | none\n\
                   \    ;" `shouldBeRight` a
            parse' "union shape\n\
                   \    # shape type\n\
                   \    = circle (point origin, \
                                 \offset radius,)\n\
                   \    | rectangle (point upper-left, \
                                    \point lower-right,)\n\
                   \    | none\n\
                   \    ;" `shouldBeRight` b
        it "fails to parse if there are duplicated facial names" $ do
            expectError "union dup\n\
                        \    = a/b\n\
                        \    | b/c\n\
                        \    | a/d\n\
                        \    ;" 5 6
            expectError "union dup\n\
                        \    = a (text a/b, text b/c, text a/d)\n\
                        \    | b\n\
                        \    ;" 2 38
        it "fails to parse if there are duplicated behind names" $ do
            expectError "union dup\n\
                        \    = a/b\n\
                        \    | b/c\n\
                        \    | c/b\n\
                        \    ;" 5 6
            expectError "union dup\n\
                        \    = a (text a/b, text b/c, text c/b)\n\
                        \    | b\n\
                        \    ;" 2 38

    describe "method" $ do
        let (parse', expectError) = helperFuncs P.method
        it "emits Method if succeeded to parse" $ do
            parse' "text get-name()" `shouldBeRight`
                Method "get-name" [] "text" Nothing
            parse' "text get-name (person user)" `shouldBeRight`
                Method "get-name" [Parameter "user" "person" Nothing]
                       "text" Nothing
            parse' "text get-name  ( person user,text default )" `shouldBeRight`
                Method "get-name"
                       [ Parameter "user" "person" Nothing
                       , Parameter "default" "text" Nothing
                       ]
                       "text" Nothing
        it "can have docs" $ do
            parse' "text get-name (\n\
                   \  # Gets the name.\n\
                   \)" `shouldBeRight`
                Method "get-name" [] "text" (Just "Gets the name.")
            parse' "text get-name (\n\
                   \  # Gets the name of the user.\n\
                   \  person user,\n\
                   \)" `shouldBeRight`
                Method "get-name"
                       [Parameter "user" "person" Nothing]
                       "text"
                       (Just "Gets the name of the user.")
            parse' "text get-name (\n\
                   \  # Gets the name of the user.\n\
                   \  person user,\n\
                   \  # The person to find their name.\n\
                   \  text default\n\
                   \  # The default name used when the user has no name.\n\
                   \)" `shouldBeRight`
                Method "get-name"
                       [ Parameter "user" "person" $
                                   Just "The person to find their name."
                       , Parameter "default" "text" $
                                   Just "The default name used when \
                                        \the user has no name."
                       ]
                       "text"
                       (Just "Gets the name of the user.")
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
        it "emits ServiceDeclaration if succeeded to parse" $ do
            parse' "service null-service();" `shouldBeRight`
                ServiceDeclaration "null-service" (Service []) Nothing
            parse' "service null-service (\n\
                   \  # Service having no methods.\n\
                   \);" `shouldBeRight`
                ServiceDeclaration "null-service"
                                   (Service [])
                                   (Just "Service having no methods.")
            parse' "service one-method-service(\n\
                   \  user get-user(uuid user-id)\n\
                   \);" `shouldBeRight`
                ServiceDeclaration
                    "one-method-service"
                    (Service [ Method "get-user"
                                      [Parameter "user-id" "uuid" Nothing]
                                      "user"
                                      Nothing
                             ])
                    Nothing
            parse' "service one-method-service (\n\
                   \  # Service having only one method.\n\
                   \  user get-user (\n\
                   \    # Gets an user by its id.\n\
                   \    uuid user-id\n\
                   \  ),\n\
                   \);" `shouldBeRight`
                ServiceDeclaration
                    "one-method-service"
                    (Service [ Method "get-user"
                                      [Parameter "user-id" "uuid" Nothing]
                                      "user"
                                      (Just "Gets an user by its id.")
                             ])
                    (Just "Service having only one method.")
            parse' "service user-service (\n\
                   \  # Service having multiple methods.\n\
                   \  user create-user (\n\
                   \    # Creates a new user\n\
                   \    user user\n\
                   \  ),\n\
                   \  user get-user (\n\
                   \    # Gets an user by its id.\n\
                   \    uuid user-id\n\
                   \  ),\n\
                   \);" `shouldBeRight`
                ServiceDeclaration
                    "user-service"
                    (Service [ Method "create-user"
                                      [Parameter "user" "user" Nothing]
                                      "user"
                                      (Just "Creates a new user")
                             , Method "get-user"
                                      [Parameter "user-id" "uuid" Nothing]
                                      "user"
                                      (Just "Gets an user by its id.")
                             ])
                    (Just "Service having multiple methods.")
        it "fails to parse if there are methods of the same facial name" $ do
            expectError "service method-dups (\n\
                        \  bool same-name ()\n\
                        \  text same-name (uuid id)\n\
                        \);" 3 3
            expectError "service method-dups (\n\
                        \  bool same-name ()\n\
                        \  text same-name/different-behind-name (uuid id)\n\
                        \);" 3 3
            expectError "service method-dups (\n\
                        \  bool same-name/unique-behind-name ()\n\
                        \  text same-name/different-behind-name (uuid id)\n\
                        \);" 3 3
        it "fails to parse if there are methods of the same behind name" $ do
            expectError "service method-dups (\n\
                        \  bool same-name ()\n\
                        \  text same-name (uuid id)\n\
                        \);" 3 3
            expectError "service method-dups (\n\
                        \  bool same-name ()\n\
                        \  text unique-name/same-name (uuid id)\n\
                        \);" 3 3
            expectError "service method-dups (\n\
                        \  bool unique-name/same-name ()\n\
                        \  text different-facial-name/same-name (uuid id)\n\
                        \);" 3 3
    let moduleParsers = [ ("module'", P.module')
                        , ("file", P.file)
                        ] :: [(String, Parser Module)]
    forM_ moduleParsers $ \(label, parser') ->
        describe label $ do
            let (parse', expectError) = helperFuncs parser'
            it "emits Module if succeeded to parse" $ do
                let decls = [ TypeDeclaration "path" (Alias "text") Nothing
                            , TypeDeclaration "offset"
                                              (BoxedType "float64") Nothing
                            ]
                parse' "type path = text; boxed offset (float64);"
                    `shouldBeRight` Module decls Nothing
                parse' "#docs\n#...\ntype path = text; boxed offset (float64);"
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
                [ Import ["foo", "bar"] "a"
                , Import ["foo", "bar"] "b"
                ]
        it "errors if parentheses have nothing" $
            expectError "import foo.bar ();" 1 17

    specify "parse & parseFile" $ do
        files <- getDirectoryContents "examples"
        let examples = map ("examples/" ++) $ filter (isSuffixOf ".nrm") files
        forM_ examples $ \filePath -> do
            sourceCode <- readFile filePath
            let parseResult = P.parse (filePath ++ " (text)") sourceCode
            parseResult `shouldSatisfy` isRight
            let Right module' = parseResult
            P.parse (filePath ++ " (text inverse)") (toCode module')
                `shouldBeRight` module'
            parseFileResult <- P.parseFile filePath
            parseFileResult `shouldSatisfy` isRight
            parseFileResult `shouldBe` parseResult

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
