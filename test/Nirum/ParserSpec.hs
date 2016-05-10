{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.ParserSpec where

import Control.Monad (forM_)
import Data.Either (isLeft, isRight, lefts)
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import Prelude hiding (readFile)
import System.Directory (getDirectoryContents)

import qualified Data.Text as T
import Data.Text.IO (readFile)
import Test.Hspec.Meta
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (ParseError, errorPos)
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine)
import Text.Megaparsec.Text (Parser)

import qualified Nirum.Parser as P
import Nirum.Constructs (Construct(toCode))
import Nirum.Constructs.Declaration (Docs(Docs))
import Nirum.Constructs.DeclarationSet (DeclarationSet)
import Nirum.Constructs.Identifier (fromText)
import Nirum.Constructs.Module (Module(Module))
import Nirum.Constructs.Name (Name(..))
import Nirum.Constructs.TypeDeclaration ( EnumMember(EnumMember)
                                        , Field(Field)
                                        , Tag(Tag)
                                        , Type(..)
                                        , TypeDeclaration(..)
                                        )
import Nirum.Constructs.TypeExpression (TypeExpression(..))

erroredPos :: Either ParseError a -> (Int, Int)
erroredPos left =
    (sourceLine pos, sourceColumn pos)
  where
    error' = head $ lefts [left] :: ParseError
    pos = errorPos error' :: SourcePos

helperFuncs :: (Show a)
            => Parser a
            -> ( T.Text -> Either ParseError a
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
    expectError string line col = do
        parse' string `shouldSatisfy` isLeft
        erroredPos (parse' string) `shouldBe` (line, col)

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
            parse' "identifier" `shouldBe` Right (identifier' "identifier")
            parse' "valid-identifier" `shouldBe`
                Right (identifier' "valid-identifier")
            parse' "valid_identifier" `shouldBe`
                Right (identifier' "valid_identifier")
        it "can parse reserved keywords iff they are quoted" $
            forM_ keywords $ \kwd ->
                parse' ('`' `T.cons` kwd `T.snoc` '`') `shouldBe`
                    Right (identifier' kwd)

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
            parse' "name" `shouldBe` Right (Name "name" "name")
            parse' "`enum`" `shouldBe` Right (Name "enum" "enum")
            parse' "facial/behind" `shouldBe` Right (Name "facial" "behind")
            parse' "facial / behind" `shouldBe` Right (Name "facial" "behind")
            parse' "`enum`/`boxed`" `shouldBe` Right (Name "enum" "boxed")
            parse' "`enum` / `boxed`" `shouldBe` Right (Name "enum" "boxed")

    describe "typeIdentifier" $ do
        let (parse', expectError) = helperFuncs P.typeIdentifier
        it "fails to parse if the input is not a valid identifier" $
            expectError "-invalid-type-identifier" 1 1
        it "emits TypeIdentifier if succeeded to parse" $ do
            parse' "text" `shouldBe` Right (TypeIdentifier "text")
            parse' "uuid" `shouldBe` Right (TypeIdentifier "uuid")

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
                parse' "text?" `shouldBe`
                    Right (OptionModifier $ TypeIdentifier "text")
                parse' "uuid ?" `shouldBe`
                    Right (OptionModifier $ TypeIdentifier "uuid")
                parse' "{text}?" `shouldBe`
                    Right (OptionModifier $ SetModifier
                                          $ TypeIdentifier "text")
            it "can be appended to set modifier" $
                parse' "{text}?" `shouldBe`
                    Right (OptionModifier $ SetModifier
                                          $ TypeIdentifier "text")

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
                parse' "{text}" `shouldBe`
                    Right (SetModifier $ TypeIdentifier "text")
                parse' "{ uuid }" `shouldBe`
                    Right (SetModifier $ TypeIdentifier "uuid")
            it "can be nested to represent 2d set" $ do
                parse' "{{text}}" `shouldBe`
                    Right (SetModifier $ SetModifier $ TypeIdentifier "text")
                parse' "{[text]}" `shouldBe`
                    Right (SetModifier $ ListModifier $ TypeIdentifier "text")
            it "can consist of optional elements" $
                parse' "{uuid?}" `shouldBe`
                    Right (SetModifier $ OptionModifier
                                       $ TypeIdentifier "uuid")

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
                parse' "[text]" `shouldBe`
                    Right (ListModifier $ TypeIdentifier "text")
                parse' "[ uuid ]" `shouldBe`
                    Right (ListModifier $ TypeIdentifier "uuid")
            it "can be nested to represent 2d list" $ do
                parse' "[[text]]" `shouldBe`
                    Right (ListModifier $ ListModifier $ TypeIdentifier "text")
                parse' "[{text}]" `shouldBe`
                    Right (ListModifier $ SetModifier $ TypeIdentifier "text")
            it "can consist of optional elements" $
                parse' "[uuid?]" `shouldBe`
                    Right (ListModifier $ OptionModifier
                                        $ TypeIdentifier "uuid")

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
                parse' "{uuid: text}" `shouldBe`
                    Right (MapModifier (TypeIdentifier "uuid")
                                       (TypeIdentifier "text"))
                parse' "{ text : uuid }" `shouldBe`
                    Right (MapModifier (TypeIdentifier "text")
                                       (TypeIdentifier "uuid"))
            it "can be nested to represent 2d map" $ do
                parse' "{uuid: {uuid: text}}" `shouldBe`
                    Right (MapModifier (TypeIdentifier "uuid")
                                       (MapModifier (TypeIdentifier "uuid")
                                                    (TypeIdentifier "text")))
                parse' "{uuid: [text]}" `shouldBe`
                    Right (MapModifier (TypeIdentifier "uuid")
                                       (ListModifier $ TypeIdentifier "text"))

    describe "typeExpression" $ do
        let (_, expectError) = helperFuncs P.typeExpression
        it "cannot append two or more option modifiers" $ do
            expectError "text??" 1 6
            expectError "text???" 1 6
            expectError "text????" 1 6

    describe "docs" $ do
        let (parse', expectError) = helperFuncs P.docs
        it "emits Docs if succeeded to parse" $ do
            parse' "#docs" `shouldBe` Right (Docs "docs\n")
            parse' "#docs\n#docs..." `shouldBe` Right (Docs "docs\ndocs...\n")
        it "may ignore a leading space" $ do
            parse' "# docs" `shouldBe` Right (Docs "docs\n")
            parse' "# docs\n# docs..." `shouldBe` Right (Docs "docs\ndocs...\n")
            parse' "#  docs" `shouldBe` Right (Docs " docs\n")
            parse' "#  docs\n#  docs..." `shouldBe`
                Right(Docs " docs\n docs...\n")
        it "may be mixed with whitespaces" $ do
            parse' "# docs\n\n# docs..." `shouldBe`
                Right(Docs "docs\ndocs...\n")
            parse' "# docs\n    # docs..." `shouldBe`
                Right(Docs "docs\ndocs...\n")
        it "differs from comment" $
            expectError "// comment" 1 1

    let descTypeDecl label parser spec' =
            let parsers = [ (label, parser)
                          , (label ++ " (typeDescription)", P.typeDeclaration)
                          ] :: [(String, Parser TypeDeclaration)]
            in forM_ parsers $ \(label', parser') ->
                describe label' $
                    spec' $ helperFuncs parser'

    descTypeDecl "aliasTypeDeclaration" P.aliasTypeDeclaration $ \helpers -> do
        let (parse', expectError) = helpers
        it "emits (TypeDeclaration (Alias ...)) if succeeded to parse" $ do
            parse' "type path = text;" `shouldBe`
                Right (TypeDeclaration "path" (Alias "text") Nothing)
            parse' "type path = text;\n# docs" `shouldBe`
                Right (TypeDeclaration "path" (Alias "text") $
                                       Just $ Docs "docs\n")
            parse' "type path = text;\n# docs\n# docs..." `shouldBe`
                Right (TypeDeclaration "path" (Alias "text") $
                                       Just $ Docs "docs\ndocs...\n")
        specify "its name can't have behind name since \
                \its canonical type's behind name would be used instead" $
            expectError "type path/error = text;" 1 10

    descTypeDecl "boxedTypeDeclaration" P.boxedTypeDeclaration $ \helpers -> do
        let (parse', _) = helpers
        it "emits (TypeDeclaration (BoxedType ...)) if succeeded to parse" $ do
            parse' "boxed offset (float64);" `shouldBe`
                Right (TypeDeclaration "offset" (BoxedType "float64") Nothing)
            parse' "boxed offset (float64);\n# docs" `shouldBe`
                Right (TypeDeclaration "offset" (BoxedType "float64") $
                                       Just $ Docs "docs\n")
            parse' "boxed offset (float64);\n# docs\n# docs..." `shouldBe`
                Right (TypeDeclaration "offset" (BoxedType "float64") $
                                       Just $ Docs "docs\ndocs...\n")

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
            parse' "enum gender = male | female | unknown;" `shouldBe`
                Right expected
            parse' "enum gender=male|female|unknown;" `shouldBe` Right expected
            parse' "enum gender =\n# gender type\nmale | female | unknown;"
                `shouldBe` Right (expected { docs = Just "gender type\n" })
            parse' "enum gender = male # docs\n| female | unknown # docs2\n;"
                `shouldBe` Right (TypeDeclaration "gender"
                                                  (EnumType membersWithDocs)
                                                  Nothing)
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
                b = a { docs = Just "person record type" }
            -- without docs, last field with trailing comma
            parse' "record person (\n\
                   \    text name,\n\
                   \    date dob,\n\
                   \    # date of birth\n\
                   \    gender gender,\n\
                   \);" `shouldBe` Right a
            -- without docs, last field without trailing comma
            parse' "record person (\n\
                   \    text name,\n\
                   \    date dob,\n\
                   \    # date of birth\n\
                   \    gender gender\n\
                   \);" `shouldBe` Right a
            -- with docs, last field with trailing comma
            parse' "record person (\n\
                   \    # person record type\n\n\
                   \    text name,\n\
                   \    date dob,\n\
                   \    # date of birth\n\
                   \    gender gender,\n\
                   \);" `shouldBe` Right b
            -- with docs, last field without trailing comma
            parse' "record person (\n\
                   \    # person record type\n\n\
                   \    text name,\n\
                   \    date dob,\n\
                   \    # date of birth\n\
                   \    gender gender\n\
                   \);" `shouldBe` Right b
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
                b = a { docs = Just "shape type" }
            parse' "union shape\n\
                   \    = circle (point origin, \
                                 \offset radius,)\n\
                   \    | rectangle (point upper-left, \
                                    \point lower-right,)\n\
                   \    | none\n\
                   \    ;" `shouldBe` Right a
            parse' "union shape\n\
                   \    # shape type\n\
                   \    = circle (point origin, \
                                 \offset radius,)\n\
                   \    | rectangle (point upper-left, \
                                    \point lower-right,)\n\
                   \    | none\n\
                   \    ;" `shouldBe` Right b
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

    let moduleParsers = [ ("module'", P.module')
                        , ("file", P.file)
                        ] :: [(String, Parser Module)]
    forM_ moduleParsers $ \(label, parser') ->
        describe label $ do
            let (parse', _) = helperFuncs parser'
            it "emits Module if succeeded to parse" $ do
                let decls = [ TypeDeclaration "path" (Alias "text") Nothing
                            , TypeDeclaration "offset"
                                              (BoxedType "float64") Nothing
                            ]
                parse' "type path = text; boxed offset (float64);"
                    `shouldBe` Right (Module decls Nothing)
                parse' "#docs\n#...\ntype path = text; boxed offset (float64);"
                    `shouldBe` Right (Module decls $ Just "docs\n...")
            it "may have no type declarations" $ do
                parse' "" `shouldBe` Right (Module [] Nothing)
                parse' "# docs" `shouldBe` Right (Module [] $ Just "docs")

    specify "parse & parseFile" $ do
        files <- getDirectoryContents "examples"
        let examples = map ("examples/" ++) $ filter (isSuffixOf ".nrm") files
        forM_ examples $ \filePath -> do
            sourceCode <- readFile filePath
            let parseResult = P.parse (filePath ++ " (text)") sourceCode
            parseResult `shouldSatisfy` isRight
            let Right module' = parseResult
            P.parse (filePath ++ " (text inverse)") (toCode module')
                `shouldBe` Right module'
            parseFileResult <- P.parseFile filePath
            parseFileResult `shouldSatisfy` isRight
            parseFileResult `shouldBe` parseResult

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
