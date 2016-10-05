{-# LANGUAGE QuasiQuotes #-}
module Nirum.Package.MetadataSpec where

import Control.Monad (forM_)

import qualified Data.SemVer as SV
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (q)
import qualified Text.Parsec.Error as PE
import Text.Parsec.Pos (sourceColumn, sourceLine)

import Nirum.Package.Metadata ( Metadata (Metadata, version)
                              , MetadataError ( FieldError
                                              , FieldTypeError
                                              , FieldValueError
                                              , FormatError
                                              )
                              , metadataFilename
                              , metadataPath
                              , parseMetadata
                              , readFromPackage
                              , readMetadata
                              )

spec :: Spec
spec =
    describe "Metadata" $ do
        describe "parseMetadata" $ do
            it "returns Metadata if the package.toml is valid" $
                let Right metadata = parse [q|version = "1.2.3"|]
                    Metadata { version = v } = metadata
                in
                    v `shouldBe` SV.version 1 2 3 [] []
            it "returns MetadataError (FormatError) if the package.toml is \
               \not a valid TOML file" $ do
                let Left (FormatError e) = parse "version = 0.3.0"
                sourceLine (PE.errorPos e) `shouldBe` 1
                sourceColumn (PE.errorPos e) `shouldBe` 14
            it "returns MetadataError (FieldError) if the package.toml lacks \
               \any required fields" $ do
                let Left (FieldError field) = parse ""
                field `shouldBe` "version"
            it "returns MetadataError (FieldTypeError) if some fields of \
               \the package.toml has a value of unexpected type" $ do
                let Left e = parse [q|version = 123|]
                    FieldTypeError field expected actual = e
                field `shouldBe` "version"
                expected `shouldBe` "string"
                actual `shouldBe` "integer (123)"
            it "returns MetadataError (FieldValueError) if some fields of \
               \the package.toml has an invalid/malformed value" $ do
                let Left e = parse [q|version = "0/3/0"|]
                    FieldValueError field msg = e
                field `shouldBe` "version"
                msg `shouldBe`
                    "expected a semver string (e.g. \"1.2.3\"), not \"0/3/0\""
        let examplePackagePath = "." </> "examples"
            samplePackagePath = "." </> "test" </> "metadata_error"
        describe "readMetadata" $ do
            it "returns Metadata if the package.toml is valid" $ do
                Right metadata <- readMetadata $ metadataPath examplePackagePath
                let Metadata { version = v } = metadata
                v `shouldBe` SV.version 0 3 0 [] []
            it "returns MetadataError if the package.toml is invalid" $ do
                r <- readMetadata $ metadataPath samplePackagePath
                let Left (FormatError e) = r
                sourceLine (PE.errorPos e) `shouldBe` 3
                sourceColumn (PE.errorPos e) `shouldBe` 14
        specify "metadataPath" $
            metadataPath "asdf" `shouldBe` "asdf" </> metadataFilename
        specify "readFromPackage" $
            forM_ [examplePackagePath, samplePackagePath] $ \pkgPath -> do
                r <- readFromPackage pkgPath
                r' <- readMetadata $ metadataPath pkgPath
                r `shouldBe` r'
  where
    parse :: Text -> Either MetadataError Metadata
    parse = parseMetadata "<string>"
