{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module Nirum.Package.MetadataSpec where

import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.Either (isRight)
import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.SemVer as SV
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.FilePath ((</>))
import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (q)
import qualified Text.Parsec.Error as PE
import Text.Parsec.Pos (sourceColumn, sourceLine)
import Text.Toml (parseTomlDoc)

import Nirum.Package.Metadata ( Metadata (Metadata, version)
                              , MetadataError ( FieldError
                                              , FieldTypeError
                                              , FieldValueError
                                              , FormatError
                                              )
                              , Target ( CompileError
                                       , CompileResult
                                       , compilePackage
                                       , parseTarget
                                       , showCompileError
                                       , targetName
                                       , toByteString
                                       )
                              , fieldType
                              , metadataFilename
                              , metadataPath
                              , parseMetadata
                              , prependMetadataErrorField
                              , readFromPackage
                              , readMetadata
                              , stringField
                              , versionField
                              )

stripPrefix :: String -> String
stripPrefix = dropWhile isSpace
ignoreLines :: String -> String
ignoreLines = concatMap stripPrefix . lines

spec :: Spec
spec =
    describe "Metadata" $ do
        specify "prependMetadataErrorField" $ do
            prependMetadataErrorField "targets" (FieldError "python")
                `shouldBe` FieldError "targets.python"
            prependMetadataErrorField "targets"
                                      (FieldTypeError "python" "table" "string")
                `shouldBe` FieldTypeError "targets.python" "table" "string"
            prependMetadataErrorField "prefix" (FieldValueError "version" "1a")
                `shouldBe` FieldValueError "prefix.version" "1a"
        describe "parseMetadata" $ do
            it "returns Metadata if the package.toml is valid" $ do
                let parsed = parse [q|version = "1.2.3"
                                      [targets.dummy]|]
                parsed `shouldSatisfy` isRight
                let Right metadata = parsed
                    Metadata { version = v } = metadata
                v `shouldBe` SV.version 1 2 3 [] []
            it ("returns MetadataError (FormatError) if the package.toml is " ++
                "not a valid TOML file") $ do
                let Left (FormatError e) = parse "version = 0.3.0"
                sourceLine (PE.errorPos e) `shouldBe` 1
                sourceColumn (PE.errorPos e) `shouldBe` 14
            it ("returns MetadataError (FieldError) if the package.toml " ++
                "lacks any required fields") $ do
                let Left (FieldError field) = parse ""
                field `shouldBe` "version"
            it ("returns MetadataError (FieldTypeError) if some fields of " ++
                "the package.toml has a value of unexpected type") $
                forM_ [ ( "version = 123"
                        , "version"
                        , "string"
                        , "integer (123)"
                        )
                      , ( [q|version = "1.2.3"
                             [authors]
                             name = "John Doe"
                          |]
                        , "authors"
                        , "array of tables"
                        , "table of an item"
                        )
                      , ( [q|version = "1.2.3"
                             [[authors]]
                             name = 123
                          |]
                        , "name"
                        , "string"
                        , "integer (123)"
                        )
                      , ( [q|version = "1.2.3"
                             [[authors]]
                             name = "John Doe"
                             [[authors]]
                             name = 456
                          |]
                        , "name"
                        , "string"
                        , "integer (456)"
                        )
                      , ( [q|version = "1.2.3"
                             [[authors]]
                             name = "John Doe"
                             email = "john@example.com"
                             [[authors]]
                             name = "Hong Minhee"
                             email = []
                          |]
                        , "email"
                        , "string"
                        , "array of 0 values"
                        )
                      , ( [q|version = "1.2.3"
                             description = 123
                          |]
                        , "description"
                        , "string"
                        , "integer (123)"
                        )
                      ] $ \ (toml, field, expected, actual) -> do
                        let Left e = parse toml
                            FieldTypeError field' expected' actual' = e
                        field' `shouldBe` field
                        expected' `shouldBe` expected
                        actual' `shouldBe` actual
            it ("returns MetadataError (FieldValueError) if some fields of " ++
                "the package.toml has an invalid/malformed value") $
                forM_ [ ( [q|version = "0/3/0"|]
                        , "version"
                        , [q|expected a semver string (e.g. "1.2.3")
                             , not "0/3/0"|]
                        )
                      , ( [q|version = "1.2.3"
                             [[authors]]
                             name = "John Doe"
                             email = "invalid#email"
                          |]
                        , "email"
                        , [q|expected an email address, not invalid#email
                             ; @: not enough input|]
                        )
                      ] $ \ (toml, field, msg) -> do
                        let Left e = parse toml
                            FieldValueError field' msg' = e
                        field' `shouldBe` field
                        msg' `shouldBe` ignoreLines msg
        let examplePackagePath = "." </> "examples"
            samplePackagePath = "." </> "test" </> "metadata_error"
            readMetadata' = readMetadata
                :: FilePath -> IO (Either MetadataError (Metadata DummyTarget))
        describe "readMetadata" $ do
            it "returns Metadata if the package.toml is valid" $ do
                readResult <- readMetadata' $ metadataPath examplePackagePath
                readResult `shouldSatisfy` isRight
                let Right metadata = readResult
                    Metadata { version = v } = metadata
                v `shouldBe` SV.version 0 3 0 [] []
            it "returns MetadataError if the package.toml is invalid" $ do
                r <- readMetadata' $ metadataPath samplePackagePath
                let Left (FormatError e) = r
                sourceLine (PE.errorPos e) `shouldBe` 3
                sourceColumn (PE.errorPos e) `shouldBe` 14
        specify "metadataPath" $
            metadataPath "asdf" `shouldBe` "asdf" </> metadataFilename
        specify "readFromPackage" $
            forM_ [examplePackagePath, samplePackagePath] $ \ pkgPath -> do
                r <- readFromPackage pkgPath
                r' <- readMetadata' $ metadataPath pkgPath
                r `shouldBe` r'
        specify "stringField" $ do
            let Right table = parseTomlDoc "<string>" [q|foo = "success"
                                                         bar = 1|]
            stringField "foo" table `shouldBe` Right "success"
            stringField "bar" table `shouldBe`
                Left (FieldTypeError "bar" "string" "integer (1)")
            stringField "qux" table `shouldBe` Left (FieldError "qux")
        specify "versionField" $ do
            let Right table = parseTomlDoc "<string>"
                                           [q|a = "1.0.0"
                                              b = "1.2.3"
                                              c = "1.0"
                                              d = 1.0
                                              e = "1.2.3.4"|]
            versionField "a" table `shouldBe` Right (SV.version 1 0 0 [] [])
            versionField "b" table `shouldBe` Right (SV.version 1 2 3 [] [])
            versionField "c" table `shouldBe` Left (FieldValueError "c"
                "expected a semver string (e.g. \"1.2.3\"), not \"1.0\"")
            versionField "d" table `shouldBe`
                Left (FieldTypeError "d" "string" "float (1.0)")
            versionField "e" table `shouldBe` Left (FieldValueError "e"
                "expected a semver string (e.g. \"1.2.3\"), not \"1.2.3.4\"")
        specify "fieldType" $ do
            let Right table = parseTomlDoc "<string>"
                    [q|s = "foobar"
                       i = 123
                       f = 3.14
                       bt = true
                       bf = false
                       d = 2017-03-16T10:56:30Z
                       a0 = []
                       a1 = ["foobar"]
                       a3 = ["foo", "bar", "baz"]
                       t0 = {}
                       t1 = { a = 1 }
                       [t2]
                       a = 1
                       b = 2
                       [[ta]]
                       a = 1
                       b = 2
                       [[ta]]
                       c = 3
                       d = 4|]
                get = fromJust . (`HM.lookup` table)
            fieldType (get "s") `shouldBe` "string (foobar)"
            fieldType (get "i") `shouldBe` "integer (123)"
            fieldType (get "f") `shouldBe` "float (3.14)"
            fieldType (get "bt") `shouldBe` "boolean (true)"
            fieldType (get "bf") `shouldBe` "boolean (false)"
            fieldType (get "d") `shouldBe` "datetime (2017-03-16 10:56:30 UTC)"
            fieldType (get "a0") `shouldBe` "array of 0 values"
            fieldType (get "a1") `shouldBe` "array of a value"
            fieldType (get "a3") `shouldBe` "array of 3 values"
            fieldType (get "t0") `shouldBe` "table of 0 items"
            fieldType (get "t1") `shouldBe` "table of an item"
            fieldType (get "t2") `shouldBe` "table of 2 items"
            fieldType (get "ta") `shouldBe` "array of 2 tables"
  where
    parse :: Text -> Either MetadataError (Metadata DummyTarget)
    parse = parseMetadata "<string>"


data DummyTarget = DummyTarget deriving (Eq, Ord, Show)

instance Target DummyTarget where
    type CompileResult DummyTarget = Text
    type CompileError DummyTarget = Text
    targetName _ = "dummy"
    parseTarget _ = return DummyTarget
    compilePackage _ = M.empty
    showCompileError _ e = e
    toByteString _ = encodeUtf8
