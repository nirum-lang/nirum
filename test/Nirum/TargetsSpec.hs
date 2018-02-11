{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Nirum.TargetsSpec where

import Data.Either (isLeft, isRight)
import System.IO.Error (isDoesNotExistError)

import qualified Data.Map.Strict as M
import System.FilePath ((</>))
import Test.Hspec.Meta
import qualified Text.Parsec.Error as PE
import Text.Parsec.Pos (sourceColumn, sourceLine)

import Nirum.Package (PackageError (ImportError, MetadataError, ScanError))
import Nirum.Package.Metadata (MetadataError (FormatError))
import Nirum.Package.ModuleSet (ImportError (MissingModulePathError))
import Nirum.Targets ( BuildError (PackageError, TargetNameError)
                     , buildPackage
                     )

spec :: Spec
spec =
    describe "Targets" $
        describe "buildPackage" $ do
            it "returns Left TargetNameError if there's no such target" $ do
                let path = "." </> "examples"
                result <- buildPackage "unregisteredtarget" path
                result `shouldBe` Left (TargetNameError "unregisteredtarget")
            it "returns Left PackageError if the given package is invalid" $ do
                let testDir = "." </> "test"
                -- ScanError
                result <- buildPackage "python" $ testDir </> "scan_error"
                result `shouldSatisfy` isLeft
                let Left (PackageError (ScanError filePath ioError')) = result
                filePath `shouldBe` testDir </> "scan_error" </> "package.toml"
                ioError' `shouldSatisfy` isDoesNotExistError
                -- MetadataError
                result2 <- buildPackage "python" $ testDir </> "metadata_error"
                result2 `shouldSatisfy` isLeft
                let Left (PackageError (MetadataError (FormatError e))) =
                        result2
                sourceLine (PE.errorPos e) `shouldBe` 3
                sourceColumn (PE.errorPos e) `shouldBe` 14
                -- ImportError
                result3 <- buildPackage "python" $ testDir </> "import_error"
                result3 `shouldSatisfy` isLeft
                let Left (PackageError (ImportError l)) = result3
                l `shouldBe` [MissingModulePathError ["import_error"] ["foo"]]
            it "returns Right BuildResult" $ do
                result <- buildPackage "python" $ "." </> "examples"
                result `shouldSatisfy` isRight
                let Right buildResult = result
                M.keysSet buildResult `shouldBe`
                    [ "MANIFEST.in"
                    , "setup.py"
                    , "src-py2" </> "address" </> "__init__.py"
                    , "src-py2" </> "blockchain" </> "__init__.py"
                    , "src-py2" </> "builtins" </> "__init__.py"
                    , "src-py2" </> "countries" </> "__init__.py"
                    , "src-py2" </> "pdf_service" </> "__init__.py"
                    , "src-py2" </> "product" </> "__init__.py"
                    , "src-py2" </> "shapes" </> "__init__.py"
                    , "src" </> "address" </> "__init__.py"
                    , "src" </> "blockchain" </> "__init__.py"
                    , "src" </> "builtins" </> "__init__.py"
                    , "src" </> "countries" </> "__init__.py"
                    , "src" </> "pdf_service" </> "__init__.py"
                    , "src" </> "product" </> "__init__.py"
                    , "src" </> "shapes" </> "__init__.py"
                    ]
