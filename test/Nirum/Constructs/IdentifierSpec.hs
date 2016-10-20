module Nirum.Constructs.IdentifierSpec where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Maybe (fromJust)

import Data.Text (Text)
import Test.Hspec.Meta

import Nirum.Constructs.Identifier ( Identifier
                                   , fromText
                                   , normalize
                                   , toCamelCaseText
                                   , toCode
                                   , toLispCaseText
                                   , toNormalizedString
                                   , toNormalizedText
                                   , toPascalCaseText
                                   , toSnakeCaseText
                                   , toString
                                   , toText
                                   , tokens
                                   )

fromText' :: Text -> Identifier
fromText' = fromJust . fromText

getIdentifier :: Text -> Text
getIdentifier = toText . fromText'

getCode :: Text -> Text
getCode = toCode . fromText'

getNormalizedIdentifier :: Text -> Text
getNormalizedIdentifier = toText . normalize . fromText'

spec :: Spec
spec = do
    describe "fromText" $ do
        it "returns Nothing if identifier text is invalid" $ do
            fromText "" `shouldBe` Nothing
            fromText "-" `shouldBe` Nothing
            fromText "1dentifier-cannot-start-with-digits" `shouldBe` Nothing
            fromText "-identifier-cannot-start-with-hyphen" `shouldBe` Nothing
            fromText "_identifier_cannot_start_with_underscore"
                `shouldBe` Nothing
            fromText "identifier-cannot-end-with-hyphen-" `shouldBe` Nothing
            fromText "identifier_cannot_end_with_underscore_"
                `shouldBe` Nothing
            fromText "identifier-cannot-have-two--or---more---continued-hyphens"
                `shouldBe` Nothing
            fromText "identifier_cannot_have_two__or_more_continued_underscores"
                `shouldBe` Nothing
            fromText "\xbb34\xd6a8\xd55c-\xc2dd\xbcc4\xc790" `shouldBe` Nothing
            fromText "invalid-\xc2dd\xbcc4\xc790" `shouldBe` Nothing
            fromText "identifier cannot contain whitepsaces" `shouldBe` Nothing
        it "returns Just Identifier if nothing goes wrong" $ do
            toText "datetime" `shouldBe` "datetime"
            toText "person-name" `shouldBe` "person-name"
    describe "tokens" $ do
        it "returns tokens behind" $ do
            tokens (fromText' "name") `shouldBe` ["name"]
            tokens (fromText' "a-name") `shouldBe` ["a", "name"]
        it "returns normalized tokens" $ do
            tokens (fromText' "Name") `shouldBe` ["name"]
            tokens (fromText' "A-Name") `shouldBe` ["a", "name"]
    describe "normalize" $
        it "returns normalized Identifier" $ do
            getNormalizedIdentifier "name" `shouldBe` "name"
            getNormalizedIdentifier "Name" `shouldBe` "name"
            getNormalizedIdentifier "normalize_test" `shouldBe` "normalize-test"
    describe "Identifier == Identifier" $ do
        it "returns True if words are all the same" $ do
            fromText "name" `shouldBe` fromText "name"
            fromText "valid-name" `shouldBe` fromText "valid-name"
        it "returns False if any word does not match" $ do
            fromText "name" `shouldNotBe` fromText "nam"
            fromText "a-name" `shouldNotBe` fromText "an-ame"
        it "is case-insensitive" $ do
            fromText "name" `shouldBe` fromText "Name"
            fromText "Valid-Name" `shouldBe` fromText "valid-name"
            fromText "valid_name" `shouldBe` fromText "valid-name"
    describe "compare Identifier Identifier" $ do
        it "returns Ordering in lexicographical order" $ do
            compare ("aaaab" :: Identifier) "aaaac" `shouldBe` LT
            compare ("aaaab" :: Identifier) "aaaaa" `shouldBe` GT
        it "is case-insensitive" $ do
            compare ("AAAAB" :: Identifier) "aaaac" `shouldBe` LT
            compare ("aaaab" :: Identifier) "AAAAC" `shouldBe` LT
            compare ("AAAAB" :: Identifier) "aaaaa" `shouldBe` GT
            compare ("aaaab" :: Identifier) "AAAAA" `shouldBe` GT
    describe "toCode Identifier" $ do
        it "returns its code representation" $ do
            getCode "name" `shouldBe` "name"
            getCode "person-name" `shouldBe` "person-name"
        it "quotes identifier if the identifier is a reserved keyword" $
            getCode "enum" `shouldBe` "`enum`"
    describe "fromString" $ do
        it "overrides strings if it's a valid identifier" $ do
            "name" `shouldBe` fromText' "name"
            "valid-name" `shouldBe` fromText' "valid-name"
        it "errors if it's a invalid identifier" $
            evaluate ("-" :: Identifier) `shouldThrow`
                errorCall "invalid identifier: -"
    describe "toString" $
        it "behaves same as toText except its return type is [Char]" $
            toString (fromText' "asdf") `shouldBe` ("asdf" :: String)
    describe "toNormalizedText Identifier" $ do
        let normalizers = [ toNormalizedText
                          , toLispCaseText
                          ] :: [Identifier -> Text]
        forM_ normalizers $ \ normalizer -> do
            it "returns lowercased identifier text" $ do
                normalizer "IDENTIFIER" `shouldBe` "identifier"
                normalizer "Identifier" `shouldBe` "identifier"
            it "replaces underscores with hyphens" $ do
                normalizer "valid_identifier" `shouldBe` "valid-identifier"
                normalizer "Valid_Identifier" `shouldBe` "valid-identifier"
    describe "toNormalizedString Identifier" $ do
        it "returns lowercased identifier string" $ do
            toNormalizedString "IDENTIFIER" `shouldBe` "identifier"
            toNormalizedString "Identifier" `shouldBe` "identifier"
        it "replaces underscores with hyphens" $ do
            toNormalizedString "valid_identifier" `shouldBe` "valid-identifier"
            toNormalizedString "Valid_Identifier" `shouldBe` "valid-identifier"
    describe "toPascalCaseText Identifier" $
        it "returns PascalCase-ed identifier string" $ do
            toPascalCaseText (fromText' "pascal-case") `shouldBe` "PascalCase"
            toPascalCaseText (fromText' "PASCAL-CASE") `shouldBe` "PascalCase"
            toPascalCaseText (fromText' "pascal_case") `shouldBe` "PascalCase"
            toPascalCaseText (fromText' "PASCAL_CASE") `shouldBe` "PascalCase"
    describe "toCamelCaseText Identifier" $
        it "returns camelCase-ed identifier string" $ do
            toCamelCaseText (fromText' "camel-case") `shouldBe` "camelCase"
            toCamelCaseText (fromText' "CAMEL-CASE") `shouldBe` "camelCase"
            toCamelCaseText (fromText' "camel_case") `shouldBe` "camelCase"
            toCamelCaseText (fromText' "CAMEL_CASE") `shouldBe` "camelCase"
    describe "toSnakeCaseText Identifier" $
        it "returns snake_case-ed identifier string" $ do
            toSnakeCaseText (fromText' "snake-case") `shouldBe` "snake_case"
            toSnakeCaseText (fromText' "SNAKE-CASE") `shouldBe` "snake_case"
            toSnakeCaseText (fromText' "snake_case") `shouldBe` "snake_case"
            toSnakeCaseText (fromText' "SNAKE_CASE") `shouldBe` "snake_case"
