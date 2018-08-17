{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables #-}
module Nirum.PackageSpec where

import Data.Either (isLeft, isRight)
import Data.Proxy (Proxy (Proxy))
import Data.Text
import System.IO.Error (isDoesNotExistError)

import Data.Map.Strict (Map)
import qualified Data.SemVer as SV
import System.FilePath ((</>))
import Test.Hspec.Meta
import Text.InterpolatedString.Perl6 (qq)
import qualified Text.Parsec.Error as PE
import Text.Parsec.Pos (sourceColumn, sourceLine)

import Nirum.Constructs.Docs
import Nirum.Constructs.Module
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Package hiding (modules, target)
import Nirum.Package.Metadata ( Metadata ( Metadata
                                         , authors
                                         , target
                                         , version
                                         , description
                                         , license
                                         , keywords
                                         )
                              , MetadataError (FormatError)
                              , Target (targetName)
                              )
import Nirum.Package.MetadataSpec (DummyTarget (DummyTarget))
import Nirum.Package.ModuleSet ( ImportError (MissingModulePathError)
                               , fromList
                               )
import Nirum.Package.ModuleSetSpec (validModules)
import Nirum.Parser (parseFile)
import Nirum.Targets.Python (Python (Python))
import Nirum.Targets.Python.CodeGen (minimumRuntime)

createPackage :: Target t
              => Metadata t
              -> [(ModulePath, Module)]
              -> Map FilePath Docs
              -> Package t
createPackage metadata' modules' documents' =
    case fromList modules' of
        Right ms -> Package metadata' ms documents'
        Left e -> error $ "errored: " ++ show e

createValidPackage :: forall t . Target t => t -> Package t
createValidPackage t =
    createPackage metadata' validModules []
  where
    metadata' :: Metadata t
    metadata' = Metadata
        { version = SV.initial
        , authors = []
        , description = Nothing
        , license = Nothing
        , keywords = []
        , target = t
        }

spec :: Spec
spec = do
    testPackage (Python "nirum-examples" minimumRuntime [] classifiers')
    testPackage DummyTarget
  where
    classifiers' :: [Text]
    classifiers' =
        [ "Development Status :: 3 - Alpha"
        , append "License :: OSI Approved :: "
                 "GNU General Public License v3 or later (GPLv3+)"
        ]

testPackage :: forall t . Target t => t -> Spec
testPackage target' = do
    let targetName' = targetName (Proxy :: Proxy t)
        validPackage = createValidPackage target'
    describe [qq|Package (target: $targetName')|] $ do
        specify "resolveModule" $ do
            resolveModule ["foo"] validPackage `shouldBe`
                Just (Module [] $ Just "foo")
            resolveModule ["foo", "bar"] validPackage `shouldBe`
                Just (Module [] $ Just "foo.bar")
            resolveModule ["qux"] validPackage `shouldBe`
                Just (Module [] $ Just "qux")
            resolveModule ["baz"] validPackage `shouldBe` Nothing
        describe "scanPackage" $ do
            it "returns Package value when all is well" $ do
                let path = "." </> "examples"
                package' <- scanPackage' path
                package' `shouldSatisfy` isRight
                let Right package = package'
                Right blockchainM <- parseFile (path </> "blockchain.nrm")
                Right builtinsM <- parseFile (path </> "builtins.nrm")
                Right productM <- parseFile (path </> "product.nrm")
                Right shapesM <- parseFile (path </> "shapes.nrm")
                Right countriesM <- parseFile (path </> "countries.nrm")
                Right addressM <- parseFile (path </> "address.nrm")
                Right pdfServiceM <- parseFile (path </> "pdf-service.nrm")
                Right geoM <- parseFile (path </> "geo.nrm")
                let modules = [ (["blockchain"], blockchainM)
                              , (["builtins"], builtinsM)
                              , (["geo"], geoM)
                              , (["product"], productM)
                              , (["shapes"], shapesM)
                              , (["countries"], countriesM)
                              , (["address"], addressM)
                              , (["pdf-service"], pdfServiceM)
                              ] :: [(ModulePath, Module)]
                let metadata' = Metadata { version = SV.version 0 3 0 [] []
                                         , authors = []
                                         , description = Nothing
                                         , license = Nothing
                                         , keywords = []
                                         , target = target'
                                         }
                let documents' =
                        [ ( "README.md"
                          , Docs $ Data.Text.concat
                                [ "# Nirum examples\n\nThis directory "
                                , "contains a sample Nirum package.\n"
                                ]
                          )
                        ]
                metadata package `shouldBe` metadata'
                package `shouldBe` createPackage metadata' modules documents'
            let testDir = "." </> "test"
            it "returns ScanError if the directory lacks package.toml" $ do
                scanResult <- scanPackage' $ testDir </> "scan_error"
                scanResult `shouldSatisfy` isLeft
                let Left (ScanError filePath ioError') = scanResult
                filePath `shouldBe` testDir </> "scan_error" </> "package.toml"
                ioError' `shouldSatisfy` isDoesNotExistError
            it "returns MetadataError if the package.toml is invalid" $ do
                scanResult <- scanPackage' $ testDir </> "metadata_error"
                scanResult `shouldSatisfy` isLeft
                let Left (MetadataError (FormatError e)) = scanResult
                sourceLine (PE.errorPos e) `shouldBe` 3
                sourceColumn (PE.errorPos e) `shouldBe` 14
            it "returns ImportError if a module imports an absent module" $ do
                scanResult <- scanPackage' $ testDir </> "import_error"
                scanResult `shouldSatisfy` isLeft
                let Left (ImportError l) = scanResult
                l `shouldBe` [MissingModulePathError ["import_error"] ["foo"]]
        specify "scanModules" $ do
            let path = "." </> "examples"
            mods' <- scanModules path
            mods' `shouldBe` [ (["blockchain"], path </> "blockchain.nrm")
                             , (["builtins"], path </> "builtins.nrm")
                             , (["geo"], path </> "geo.nrm")
                             , (["product"], path </> "product.nrm")
                             , (["shapes"], path </> "shapes.nrm")
                             , (["countries"], path </> "countries.nrm")
                             , (["address"], path </> "address.nrm")
                             , (["pdf-service"], path </> "pdf-service.nrm")
                             ]
  where
    scanPackage' :: FilePath -> IO (Either PackageError (Package t))
    scanPackage' = scanPackage
